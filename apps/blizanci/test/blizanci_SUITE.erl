%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

%% Application-level Common Test suite: the real application is started
%% on a free port, with a throwaway docroot and a self-signed certificate
%% made on the fly, and spoken to over TLS like a Gemini client would.
%% Unit-level behaviour stays in the eunit tests.
%%
%% Run with:  rebar3 ct
%%
%% FUTURE CASE (deliberately not written yet, the supervision tree is
%% being restructured): kill blizanci_conf_serv and check what recovers,
%% i.e. that the listener keeps serving, that the live configuration
%% (blizanci_config:apply/1 since start) survives, and that the CGI and
%% Titan pools are not killed along with it. See issues.md, "TODO:
%% housekeeping and supervision audit", D.1.
-module(blizanci_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).
-export([static/1, cgi/1, titan/1, sni_live_apply/1, restart/1,
         no_leaks/1, tls_versions/1, client_cert_request/1,
         client_cert_live_apply/1, default_table/1,
         default_table_titan/1, mounts_table/1, mounts_table_empty_realm/1,
         default_table_titan_no_ca_certs/1]).
-export([log/2]). % logger handler callback, see cert_requested/4

-define(HOST, "first.test").
-define(SECOND, "second.test").
-define(TIMEOUT, 5000).
-define(LEAK_TIMEOUT, 5000).

all() ->
    [static, cgi, titan, sni_live_apply, restart, no_leaks,
     tls_versions, client_cert_request, client_cert_live_apply,
     default_table, default_table_titan, mounts_table,
     mounts_table_empty_realm,
     default_table_titan_no_ca_certs].

%%%===================================================================
%%% Setup
%%%===================================================================

init_per_suite(Config) ->
    Dir = ?config(priv_dir, Config),
    %% TLS 1.3 wants a P-256 key and a SHA-2 signature, not the
    %% library's defaults
    Opts = fun() ->
               [{key, public_key:generate_key({namedCurve, secp256r1})},
                {digest, sha256}]
           end,
    #{server_config := SC} =
        public_key:pkix_test_data(#{server_chain => #{root => Opts(),
                                                      peer => Opts()},
                                    client_chain => #{root => [],
                                                      peer => []}}),
    Der = proplists:get_value(cert, SC),
    {KeyType, KeyDer} = proplists:get_value(key, SC),
    Cert = filename:join(Dir, "cert.pem"),
    Key = filename:join(Dir, "key.pem"),
    ok = file:write_file(Cert, public_key:pem_encode(
                                 [{'Certificate', Der, not_encrypted}])),
    ok = file:write_file(Key, public_key:pem_encode(
                                [{KeyType, KeyDer, not_encrypted}])),
    ok = file:change_mode(Key, 8#600),
    {ok, _} = application:ensure_all_started(ssl),
    %% a client identity: an RSA certificate issued by a CA whose
    %% certificate is written out for the private routes to trust. (RSA
    %% because blizanci_x509:verify_signed_by/2 does not check EC-signed
    %% certificates, so a private route would refuse it.)
    Rsa = [{key, {rsa, 2048, 17}}, {digest, sha256}],
    #{server_config := CliSC, client_config := CliCC} =
        public_key:pkix_test_data(#{server_chain => #{root => Opts(),
                                                      peer => Opts()},
                                    client_chain => #{root => Rsa,
                                                      peer => Rsa}}),
    [CaDer | _] = proplists:get_value(cacerts, CliSC),
    CaFile = filename:join(Dir, "client-ca.pem"),
    ok = file:write_file(CaFile, public_key:pem_encode(
                                   [{'Certificate', CaDer,
                                     not_encrypted}])),
    [{certfile, Cert}, {keyfile, Key}, {client_ca, CaFile},
     {client_opts, [{cert, proplists:get_value(cert, CliCC)},
                    {key, proplists:get_value(key, CliCC)}]}
     | Config].

end_per_suite(_Config) ->
    _ = application:stop(erlexec),
    ok.

init_per_testcase(Case, Config) ->
    Dir = filename:join(?config(priv_dir, Config), atom_to_list(Case)),
    Docroot = filename:join(Dir, "docroot"),
    CgiRoot = filename:join(Dir, "cgi-bin"),
    Work = filename:join(Dir, "titan-temp"),
    [ ok = filelib:ensure_path(D) || D <- [Docroot, CgiRoot, Work] ],
    ok = file:write_file(filename:join(Docroot, "hello.gmi"),
                         <<"# Hello\n">>),
    ok = filelib:ensure_path(filename:join(Docroot, "sub")),
    Port = free_port(),
    Routing =
        [{gemini, "cgi-bin/(?<PATH>.*)", blizanci_cgi, public,
          #{cgiroot => CgiRoot}},
         {titan, "(?<PATH>.*)", blizanci_titan, public,
          #{docroot => Docroot, work_dir => Work}},
         {gemini, "(?<PATH>.*)", blizanci_static, public,
          #{docroot => Docroot}}],
    Env = [{port, Port},
           {hostname, ?HOST},
           {certfile, ?config(certfile, Config)},
           {keyfile, ?config(keyfile, Config)},
           {docroot, Docroot},
           {routing, Routing}],
    %% load first: loading after set_env would reset the env
    ok = load_app(),
    [ ok = application:set_env(blizanci, K, V) || {K, V} <- Env ],
    [{port, Port}, {docroot, Docroot}, {cgiroot, CgiRoot},
     {casedir, Dir} | Config].

end_per_testcase(_Case, _Config) ->
    _ = application:stop(blizanci),
    [ application:unset_env(blizanci, K)
      || K <- [port, hostname, certfile, keyfile, docroot, routing,
               cgiroot, ca_certs, titan_work_dir, realms] ],
    ok.

%%%===================================================================
%%% Cases
%%%===================================================================

static(Config) ->
    {ok, _} = application:ensure_all_started(blizanci),
    Port = ?config(port, Config),
    {ok, R1} = fetch(Port, ?HOST, ?HOST, "/hello.gmi"),
    <<"20 text/gemini", _/binary>> = R1,
    {_, <<"# Hello\n">>} = split_header(R1),
    {ok, R2} = fetch(Port, ?HOST, ?HOST, "/nothing.gmi"),
    <<"51 ", _/binary>> = R2,
    {ok, R3} = fetch(Port, ?HOST, ?HOST, "/sub"),
    Expected = iolist_to_binary(
                 ["31 gemini://", ?HOST, ":", integer_to_list(Port),
                  "/sub/index.gemini\r\n"]),
    Expected = R3,
    ok = file:write_file(filename:join([?config(docroot, Config),
                                        "sub", "index.gemini"]),
                         <<"# Sub\n">>),
    {ok, R4} = fetch(Port, ?HOST, ?HOST, "/sub/index.gemini"),
    {_, <<"# Sub\n">>} = split_header(R4),
    ok.

cgi(Config) ->
    case exec_works() of
        ok -> cgi_run(Config);
        {error, Why} ->
            {skip, {cannot_run_cgi, Why}}
    end.

cgi_run(Config) ->
    Script = filename:join(?config(cgiroot, Config), "hi.sh"),
    ok = file:write_file(Script,
                         <<"#!/bin/sh\nprintf '20 text/plain\\r\\n'\n"
                           "echo hello from cgi\n">>),
    ok = file:change_mode(Script, 8#755),
    {ok, _} = application:ensure_all_started(blizanci),
    Port = ?config(port, Config),
    {ok, R} = fetch(Port, ?HOST, ?HOST, "/cgi-bin/hi.sh"),
    <<"20 text/plain\r\n", Body/binary>> = R,
    <<"hello from cgi\n">> = Body,
    {ok, R2} = fetch(Port, ?HOST, ?HOST, "/cgi-bin/absent.sh"),
    <<"51 ", _/binary>> = R2,
    ok.

titan(Config) ->
    {ok, _} = application:ensure_all_started(blizanci),
    Port = ?config(port, Config),
    Body = <<"uploaded by titan\n">>,
    Size = integer_to_list(byte_size(Body)),
    Line = "titan://" ?HOST ":" ++ integer_to_list(Port)
        ++ "/up.txt;size=" ++ Size ++ ";mime=text/plain\r\n",
    {ok, Reply} = converse(Port, ?HOST, [Line, Body]),
    ct:log("titan reply: ~p", [Reply]),
    <<"2", _/binary>> = Reply,
    ok = wait_for_file(filename:join(?config(docroot, Config), "up.txt")),
    {ok, R} = fetch(Port, ?HOST, ?HOST, "/up.txt"),
    <<"20 text/plain", _/binary>> = R,
    {_, Body} = split_header(R),
    ok.

%% The default routing table (no routing key), from docroot, cgiroot,
%% ca_certs and titan_work_dir, served end to end: /cgi-bin runs the
%% script in cgiroot, the rest is public; a directory is redirected to
%% its index.
default_table(Config) ->
    use_default_table(Config, false),
    Docroot = ?config(docroot, Config),
    ok = filelib:ensure_path(filename:join(Docroot, "sub")),
    ok = file:write_file(filename:join(Docroot, "sub/index.gemini"),
                         <<"# sub">>),
    CgiOk = exec_works() =:= ok,
    CgiOk andalso write_cgi_script(Config),
    {ok, _} = application:ensure_all_started(blizanci),
    Port = ?config(port, Config),
    Get = fun(Extra, Path) ->
              fetch(Port, ?HOST, 'tlsv1.3', Extra, Path)
          end,
    {ok, <<"20 text/gemini", _/binary>> = R1} = Get([], "/hello.gmi"),
    {_, <<"# Hello\n">>} = split_header(R1),
    {ok, <<"51 ", _/binary>>} = Get([], "/nothere.gmi"),
    %% directories: the redirect goes to the index
    Redirect = fun(Rel) ->
                   iolist_to_binary(["31 gemini://", ?HOST, ":",
                                     integer_to_list(Port), Rel,
                                     "\r\n"])
               end,
    [ {ok, Want} = Get([], Path)
      || {Path, Want} <-
             [{"/sub", Redirect("/sub/index.gemini")},
              {"/sub/", Redirect("/sub/index.gemini")}] ],
    {ok, R5} = Get([], "/sub/index.gemini"),
    {_, <<"# sub">>} = split_header(R5),
    %% cgi-bin runs the script in cgiroot
    case CgiOk of
        true ->
            {ok, <<"20 text/plain\r\n", "hi\n">>} =
                Get([], "/cgi-bin/hi.sh");
        false -> ok
    end,
    %% with no titan_work_dir there is no Titan route
    {ok, <<"5", _/binary>>} =
        converse(Port, ?HOST,
                 ["titan://" ?HOST ":" ++ integer_to_list(Port)
                  ++ "/up.txt;size=1;mime=text/plain\r\n", "x"]),
    ok.

%% With titan_work_dir set the default table gets a Titan route that
%% needs the default realm's certificate and shares the docroot.
default_table_titan(Config) ->
    use_default_table(Config, true),
    {ok, _} = application:ensure_all_started(blizanci),
    Port = ?config(port, Config),
    Cert = ?config(client_opts, Config),
    Up = fun(Extra) ->
             Line = "titan://" ?HOST ":" ++ integer_to_list(Port)
                 ++ "/up.txt;size=2;mime=text/plain\r\n",
             {ok, S} = connect(Port, ?HOST, 'tlsv1.3', Extra),
             try
                 ok = ssl:send(S, [Line, "hi"]),
                 recv_all(S, <<>>)
             after
                 ssl:close(S)
             end
         end,
    {ok, <<"60 ", _/binary>>} = Up([]),
    {ok, <<"2", _/binary>>} = Up(Cert),
    ok = wait_for_file(filename:join(?config(docroot, Config), "up.txt")),
    {ok, R} = fetch(Port, ?HOST, ?HOST, "/up.txt"),
    {_, <<"hi">>} = split_header(R),
    ok.

%% An explicit routing table with /private (realm "default") and
%% /restricted mounts over subdirectories of the docroot, beside
%% /cgi-bin and /, served end to end: private and restricted need a
%% client certificate and serve their own directories, /cgi-bin runs the
%% script in cgiroot, the rest is public; a directory is redirected to
%% its index with the mount kept; "private" is matched as a whole
%% segment only.
mounts_table(Config) ->
    use_mounts_table(Config, [{<<"default">>,
                               [?config(client_ca, Config)]}]),
    Docroot = ?config(docroot, Config),
    [ ok = filelib:ensure_path(filename:join(Docroot, D))
      || D <- ["private/sub", "restricted", "privatefoo"] ],
    [ ok = file:write_file(filename:join(Docroot, F), [<<"# ">>, F])
      || F <- ["private/x.gmi", "restricted/x.gmi", "privatefoo/x.gmi",
               "private/sub/index.gemini"] ],
    CgiOk = exec_works() =:= ok,
    CgiOk andalso write_cgi_script(Config),
    {ok, _} = application:ensure_all_started(blizanci),
    Port = ?config(port, Config),
    Cert = ?config(client_opts, Config),
    Get = fun(Extra, Path) ->
              fetch(Port, ?HOST, 'tlsv1.3', Extra, Path)
          end,
    {ok, <<"20 text/gemini", _/binary>> = R1} = Get([], "/hello.gmi"),
    {_, <<"# Hello\n">>} = split_header(R1),
    %% a mount is whole segments: privatefoo is public
    {ok, R2} = Get([], "/privatefoo/x.gmi"),
    {_, <<"# privatefoo/x.gmi">>} = split_header(R2),
    %% private and restricted ask for a certificate
    [ {ok, <<"60 ", _/binary>>} = Get([], P)
      || P <- ["/private/x.gmi", "/private", "/private/",
               "/restricted/x.gmi"] ],
    {ok, R3} = Get(Cert, "/private/x.gmi"),
    {_, <<"# private/x.gmi">>} = split_header(R3),
    {ok, R4} = Get(Cert, "/restricted/x.gmi"),
    {_, <<"# restricted/x.gmi">>} = split_header(R4),
    {ok, <<"51 ", _/binary>>} = Get(Cert, "/private/nothere.gmi"),
    %% directories: the redirect keeps the mount
    Redirect = fun(Rel) ->
                   iolist_to_binary(["31 gemini://", ?HOST, ":",
                                     integer_to_list(Port), Rel,
                                     "\r\n"])
               end,
    [ {ok, Want} = Get(Cert, Path)
      || {Path, Want} <-
             [{"/private", Redirect("/private/index.gemini")},
              {"/private/", Redirect("/private/index.gemini")},
              {"/private/sub", Redirect("/private/sub/index.gemini")},
              {"/private/sub/", Redirect("/private/sub/index.gemini")},
              {"/restricted/", Redirect("/restricted/index.gemini")}] ],
    {ok, R5} = Get(Cert, "/private/sub/index.gemini"),
    {_, <<"# private/sub/index.gemini">>} = split_header(R5),
    %% cgi-bin runs the script in cgiroot
    case CgiOk of
        true ->
            {ok, <<"20 text/plain\r\n", "hi\n">>} =
                Get([], "/cgi-bin/hi.sh");
        false -> ok
    end,
    ok.

%% A private mount on a realm with no CA files starts and authorises
%% nobody, not even a certificate; nothing falls through to the public
%% mount.
mounts_table_empty_realm(Config) ->
    use_mounts_table(Config, [{<<"default">>, []}]),
    Docroot = ?config(docroot, Config),
    ok = filelib:ensure_path(filename:join(Docroot, "private")),
    ok = filelib:ensure_path(filename:join(Docroot, "restricted")),
    ok = file:write_file(filename:join([Docroot, "private", "x.gmi"]),
                         <<"# secret\n">>),
    {ok, _} = application:ensure_all_started(blizanci),
    Port = ?config(port, Config),
    {ok, <<"60 ", _/binary>>} =
        fetch(Port, ?HOST, 'tlsv1.3', [], "/private/x.gmi"),
    {ok, <<"61 ", _/binary>>} =
        fetch(Port, ?HOST, 'tlsv1.3', ?config(client_opts, Config),
              "/private/x.gmi"),
    {ok, <<"20 ", _/binary>>} = fetch(Port, ?HOST, ?HOST, "/hello.gmi"),
    ok.

%% Settings for an explicit routing table with private and restricted
%% mounts over subdirectories of the docroot; Realms is the global
%% realms key.
use_mounts_table(Config, Realms) ->
    Docroot = ?config(docroot, Config),
    ok = application:unset_env(blizanci, ca_certs),
    ok = application:set_env(blizanci, realms, Realms),
    ok = application:set_env(blizanci, cgiroot, ?config(cgiroot, Config)),
    ok = application:set_env(
           blizanci, routing,
           [{mount, gemini, "/cgi-bin", blizanci_cgi, public,
             #{ cgiroot => ?config(cgiroot, Config) }},
            {mount, gemini, "/private", blizanci_static,
             {private, {realm, <<"default">>}},
             #{ docroot => filename:join(Docroot, "private") }},
            {mount, gemini, "/restricted", blizanci_static, restricted,
             #{ docroot => filename:join(Docroot, "restricted") }},
            {mount, gemini, "/", blizanci_static, public,
             #{ docroot => Docroot }}]).

%% titan_work_dir without ca_certs starts: the realm "default" is empty,
%% so the Titan route authorises nobody (a certificate gets 61), while
%% the public routes are served as usual.
default_table_titan_no_ca_certs(Config) ->
    use_default_table(Config, true),
    ok = application:unset_env(blizanci, ca_certs),
    {ok, _} = application:ensure_all_started(blizanci),
    Port = ?config(port, Config),
    Up = fun(Extra) ->
             Line = "titan://" ?HOST ":" ++ integer_to_list(Port)
                 ++ "/up.txt;size=2;mime=text/plain\r\n",
             {ok, S} = connect(Port, ?HOST, 'tlsv1.3', Extra),
             try
                 ok = ssl:send(S, [Line, "hi"]),
                 recv_all(S, <<>>)
             after
                 ssl:close(S)
             end
         end,
    {ok, <<"60 ", _/binary>>} = Up([]),
    {ok, <<"61 ", _/binary>>} = Up(?config(client_opts, Config)),
    {ok, <<"20 ", _/binary>>} = fetch(Port, ?HOST, ?HOST, "/hello.gmi"),
    ok.

%% Settings for the default table: no routing key; ca_certs is the CA
%% that issued the client certificate.
use_default_table(Config, WithTitan) ->
    ok = application:unset_env(blizanci, routing),
    ok = application:set_env(blizanci, cgiroot, ?config(cgiroot, Config)),
    ok = application:set_env(blizanci, ca_certs,
                             [?config(client_ca, Config)]),
    case WithTitan of
        true ->
            Work = filename:join(?config(casedir, Config), "titan-temp"),
            ok = application:set_env(blizanci, titan_work_dir, Work);
        false -> ok
    end.

sni_live_apply(Config) ->
    {ok, _} = application:ensure_all_started(blizanci),
    Port = ?config(port, Config),
    Docroot = ?config(docroot, Config),
    %% a connection is open, with nothing sent, across the apply
    {ok, Open} = connect(Port, ?HOST),
    %% the second site is not known yet
    {error, _} = connect(Port, ?SECOND),
    Docroot2 = filename:join(filename:dirname(Docroot), "docroot2"),
    ok = filelib:ensure_path(Docroot2),
    ok = file:write_file(filename:join(Docroot2, "two.gmi"), <<"# Two\n">>),
    ok = blizanci_config:apply(
           two_site_config(Port, Config, Docroot, Docroot2)),
    {ok, R} = fetch(Port, ?SECOND, ?SECOND, "/two.gmi"),
    {_, <<"# Two\n">>} = split_header(R),
    %% the first site still works for new connections
    {ok, R1} = fetch(Port, ?HOST, ?HOST, "/hello.gmi"),
    {_, <<"# Hello\n">>} = split_header(R1),
    %% and the connection opened before the apply finishes
    ok = ssl:send(Open, request(?HOST, Port, "/hello.gmi")),
    {ok, R2} = recv_all(Open, <<>>),
    {_, <<"# Hello\n">>} = split_header(R2),
    ssl:close(Open),
    ok.

restart(Config) ->
    Port = ?config(port, Config),
    {ok, _} = application:ensure_all_started(blizanci),
    {ok, R} = fetch(Port, ?HOST, ?HOST, "/hello.gmi"),
    <<"20 ", _/binary>> = R,
    ok = application:stop(blizanci),
    undefined = whereis(blizanci_cgi),
    undefined = whereis(blizanci_titan),
    undefined = whereis(blizanci_conf_serv),
    undefined = whereis(blizanci_sup),
    {error, _} = connect(Port, ?HOST),
    {ok, _} = application:ensure_all_started(blizanci),
    true = is_pid(whereis(blizanci_conf_serv)),
    true = is_pid(whereis(blizanci_cgi)),
    true = is_pid(whereis(blizanci_titan)),
    {ok, R2} = fetch(Port, ?HOST, ?HOST, "/hello.gmi"),
    <<"20 ", _/binary>> = R2,
    ok.

%% Both TLS versions are served: a client limited to 1.2 and one limited
%% to 1.3 each complete a request, and each negotiated what it asked for.
tls_versions(Config) ->
    {ok, _} = application:ensure_all_started(blizanci),
    Port = ?config(port, Config),
    [ begin
          {ok, S} = connect(Port, ?HOST, V, []),
          {ok, [{protocol, V}]} = ssl:connection_information(S, [protocol]),
          ok = ssl:send(S, request(?HOST, Port, "/hello.gmi")),
          {ok, R} = recv_all(S, <<>>),
          ssl:close(S),
          <<"20 text/gemini", _/binary>> = R,
          {_, <<"# Hello\n">>} = split_header(R)
      end || V <- versions() ],
    ok.

%% A site with a restricted or private route asks for a client
%% certificate; a site with only public routes does not. Observed on the
%% client, by a debug-level handshake log (see cert_requested/4). The
%% client holds a valid certificate in some of the runs, so the
%% all-public site not asking is the server's choice, not the client's
%% lack of one.
client_cert_request(Config) ->
    {ok, _} = application:ensure_all_started(blizanci),
    Port = ?config(port, Config),
    ok = apply_sites(Config, with_private, Port),
    Cert = ?config(client_opts, Config),
    [ begin
          %% all-public site: nothing is requested, and a public page
          %% is served to a client with and without a certificate
          false = cert_requested(Port, ?HOST, V, Cert),
          false = cert_requested(Port, ?HOST, V, []),
          {ok, <<"20 ", _/binary>>} =
              fetch(Port, ?HOST, V, Cert, "/hello.gmi"),
          {ok, <<"20 ", _/binary>>} =
              fetch(Port, ?HOST, V, [], "/hello.gmi"),
          %% site with restricted and private routes: requested
          true = cert_requested(Port, ?SECOND, V, Cert),
          true = cert_requested(Port, ?SECOND, V, []),
          %% a public route there needs no certificate
          {ok, <<"20 ", _/binary>>} =
              fetch(Port, ?SECOND, V, [], "/hello.gmi"),
          [ begin
                {ok, <<"60 ", _/binary>>} =
                    fetch(Port, ?SECOND, V, [], Path),
                {ok, <<"20 ", _/binary>>} =
                    fetch(Port, ?SECOND, V, Cert, Path)
            end || Path <- ["/restricted/x.gmi", "/private/x.gmi"] ]
      end || V <- versions() ],
    ok.

%% A site that starts all-public and is then given a private route
%% begins asking for certificates on the next handshake, and stops
%% asking when the route is taken away again.
client_cert_live_apply(Config) ->
    {ok, _} = application:ensure_all_started(blizanci),
    Port = ?config(port, Config),
    Cert = ?config(client_opts, Config),
    ok = apply_sites(Config, public_only, Port),
    [ begin
          false = cert_requested(Port, ?SECOND, V, Cert),
          {ok, <<"20 ", _/binary>>} =
              fetch(Port, ?SECOND, V, [], "/private/x.gmi"),
          ok = apply_sites(Config, with_private, Port),
          true = cert_requested(Port, ?SECOND, V, Cert),
          {ok, <<"60 ", _/binary>>} =
              fetch(Port, ?SECOND, V, [], "/private/x.gmi"),
          {ok, <<"20 ", _/binary>>} =
              fetch(Port, ?SECOND, V, Cert, "/private/x.gmi"),
          ok = apply_sites(Config, public_only, Port),
          false = cert_requested(Port, ?SECOND, V, Cert)
      end || V <- versions() ],
    ok.

%% Nothing is left behind by a run of the application. After a first
%% cycle, to create whatever is created once and then kept (ssl's
%% session tables and the like), a baseline is taken; the application
%% is then run through a representative set of requests and stopped,
%% and the processes, ports, ETS tables, registered names and files
%% must be back to the baseline, within a timeout (shutdown is
%% asynchronous), polled for rather than slept for.
%%
%% The dependencies (ranch, ppool, ssl, erlexec, ...) are started before
%% the baseline and left running: they outlive the application, so
%% whatever they hold is not blizanci's leak and is not counted. Their
%% processes that blizanci's connections cause to appear must still go
%% away, and are counted.
no_leaks(Config) ->
    Port = ?config(port, Config),
    CgiOk = exec_works() =:= ok,
    ok = write_cgi_script(Config),
    [ {ok, _} = application:ensure_all_started(A)
      || A <- dependencies() ],
    ok = leak_cycle(Port, CgiOk),
    Base = snapshot(Config),
    ok = leak_cycle(Port, CgiOk),
    Deadline = erlang:monotonic_time(millisecond) + ?LEAK_TIMEOUT,
    case await_no_leaks(Base, Config, Deadline) of
        ok -> ok;
        {leaked, Report} ->
            ct:log("leaked: ~p", [Report]),
            ct:fail({leaked, Report})
    end.

dependencies() ->
    {ok, Apps} = application:get_key(blizanci, applications),
    Apps -- [kernel, stdlib].

%% Start, serve a representative set of requests, stop.
leak_cycle(Port, CgiOk) ->
    {ok, _} = application:ensure_all_started(blizanci),
    {ok, <<"20 ", _/binary>>} = fetch(Port, ?HOST, ?HOST, "/hello.gmi"),
    {ok, <<"51 ", _/binary>>} = fetch(Port, ?HOST, ?HOST, "/nope.gmi"),
    %% malformed: not a URL
    {ok, <<"59 ", _/binary>>} =
        converse(Port, ?HOST, ["not a url\r\n"]),
    %% hangs up without sending a request
    {ok, S} = connect(Port, ?HOST),
    ok = ssl:close(S),
    %% sends part of a request, then hangs up
    {ok, S2} = connect(Port, ?HOST),
    ok = ssl:send(S2, "gemini://first"),
    ok = ssl:close(S2),
    %% a Titan upload, which is staged in the work dir
    Body = <<"leak check\n">>,
    Size = integer_to_list(byte_size(Body)),
    Line = "titan://" ?HOST ":" ++ integer_to_list(Port)
        ++ "/leak.txt;size=" ++ Size ++ ";mime=text/plain\r\n",
    {ok, <<"2", _/binary>>} = converse(Port, ?HOST, [Line, Body]),
    %% a deferred (pooled) servlet, where exec works
    case CgiOk of
        true ->
            {ok, <<"20 ", _/binary>>} =
                fetch(Port, ?HOST, ?HOST, "/cgi-bin/hi.sh");
        false -> ok
    end,
    ok = application:stop(blizanci).

write_cgi_script(Config) ->
    Script = filename:join(?config(cgiroot, Config), "hi.sh"),
    ok = file:write_file(Script, <<"#!/bin/sh\nprintf '20 text/plain"
                                   "\\r\\n'\necho hi\n">>),
    ok = file:change_mode(Script, 8#755).

snapshot(Config) ->
    #{processes => maps:from_list(
                     [ {P, proc_info(P)} || P <- processes() ]),
      ports => erlang:ports(),
      tables => maps:from_list(
                  [ {T, {ets:info(T, name), ets:info(T, owner)}}
                    || T <- ets:all() ]),
      names => lists:sort(registered()),
      %% the case's whole directory: docroot, cgi-bin and Titan's
      %% work dir, the only places the application writes
      files => lists:sort(
                 filelib:wildcard("**", ?config(casedir, Config)))}.

proc_info(P) ->
    case process_info(P, [initial_call, current_function,
                          registered_name]) of
        undefined -> gone;
        I -> I
    end.

await_no_leaks(Base, Config, Deadline) ->
    case leaks(Base, snapshot(Config)) of
        [] -> ok;
        Report ->
            case erlang:monotonic_time(millisecond) >= Deadline of
                true -> {leaked, Report};
                false ->
                    timer:sleep(50),
                    await_no_leaks(Base, Config, Deadline)
            end
    end.

%% What is in Now and not in Base, as a list of {What, Items}.
leaks(Base, Now) ->
    #{processes := P0, ports := Po0, tables := T0, names := N0,
      files := F0} = Base,
    #{processes := P1, ports := Po1, tables := T1, names := N1,
      files := F1} = Now,
    Checks =
        [{extra_registered_names, N1 -- N0},
         {extra_processes,
          [ {P, proc_info(P)} || P <- maps:keys(P1),
                                 not maps:is_key(P, P0) ]},
         {extra_ports,
          [ {Po, erlang:port_info(Po, name)} || Po <- Po1 -- Po0 ]},
         {extra_tables, [ {T, maps:get(T, T1)}
                          || T <- maps:keys(T1),
                             not maps:is_key(T, T0) ]},
         {extra_files, F1 -- F0}],
    [ C || {_, V} = C <- Checks, V =/= [] ].

%%%===================================================================
%%% Helpers
%%%===================================================================

two_site_config(Port, Config, Docroot, Docroot2) ->
    Cert = ?config(certfile, Config),
    Key = ?config(keyfile, Config),
    Site = fun(Name, Root) ->
               {ok, S0} = blizanci_config:site(Name, Cert, Key),
               {ok, Route} = blizanci_config:route(
                               gemini, "(?<PATH>.*)", blizanci_static,
                               public, #{docroot => Root}),
               blizanci_config:add_route(S0, Route)
           end,
    Global = blizanci_config:global(Port, [blizanci_static,
                                           blizanci_cgi,
                                           blizanci_titan]),
    blizanci_config:new(Global, [Site(?HOST, Docroot),
                                 Site(?SECOND, Docroot2)]).

versions() -> ['tlsv1.2', 'tlsv1.3'].

%% Publish two sites on Port. ?HOST has only public routes, over the
%% docroot of the test case. ?SECOND has the same routes plus, if Kind is
%% with_private, restricted and private ones (the private one trusts the
%% CA that issued the client certificate), over a second docroot that
%% holds the same pages.
apply_sites(Config, Kind, Port) ->
    Cert = ?config(certfile, Config),
    Key = ?config(keyfile, Config),
    Docroot = ?config(docroot, Config),
    Docroot2 = filename:join(filename:dirname(Docroot), "docroot2"),
    [ begin
          ok = filelib:ensure_path(filename:join(Docroot2, D)),
          ok = file:write_file(filename:join([Docroot2, D, "x.gmi"]),
                               <<"# x\n">>)
      end || D <- ["restricted", "private"] ],
    ok = file:write_file(filename:join(Docroot2, "hello.gmi"),
                         <<"# Hello\n">>),
    Route = fun(Rx, Auth, Root) ->
                {ok, R} = blizanci_config:route(
                            gemini, Rx, blizanci_static, Auth,
                            #{docroot => Root}),
                R
            end,
    Site = fun(Name, Routes) ->
               {ok, S0} = blizanci_config:site(Name, Cert, Key),
               lists:foldl(fun(R, S) -> blizanci_config:add_route(S, R) end,
                           S0, Routes)
           end,
    Public = fun(Root) -> Route("(?<PATH>.*)", public, Root) end,
    Second = case Kind of
                 public_only -> [Public(Docroot2)];
                 with_private ->
                     [Route("(?<PATH>private.*)",
                            {private, [?config(client_ca, Config)]},
                            Docroot2),
                      Route("(?<PATH>restricted.*)", restricted, Docroot2),
                      Public(Docroot2)]
             end,
    Global = blizanci_config:global(Port, [blizanci_static,
                                           blizanci_cgi,
                                           blizanci_titan]),
    blizanci_config:apply(
      blizanci_config:new(Global, [Site(?HOST, [Public(Docroot)]),
                                   Site(?SECOND, Second)])).

load_app() ->
    case application:load(blizanci) of
        ok -> ok;
        {error, {already_loaded, blizanci}} -> ok
    end.

free_port() ->
    {ok, L} = gen_tcp:listen(0, []),
    {ok, Port} = inet:port(L),
    ok = gen_tcp:close(L),
    Port.

exec_works() ->
    try
        {ok, _} = application:ensure_all_started(erlexec),
        case exec:run("true", [sync]) of
            {ok, _} -> ok;
            Other -> {error, Other}
        end
    catch
        C:E -> {error, {C, E}}
    end.

connect(Port, Sni) ->
    connect(Port, Sni, 'tlsv1.3', []).

connect(Port, Sni, Version, Extra) ->
    ssl:connect({127,0,0,1}, Port,
                [binary, {active, false}, {verify, verify_none},
                 {versions, [Version]},
                 {server_name_indication, Sni} | Extra], ?TIMEOUT).

%% Did the server send a CertificateRequest in this handshake? The
%% client is run at ssl log level debug, which logs each handshake
%% message it receives, and a logger handler collects those: the
%% handshake messages of both TLS versions are logged by the client's
%% own process before connect/4 returns, so there is nothing to wait for.
%% (OTP 28 has no connection_information item for this.) The handler is
%% removed and the primary log level restored afterwards.
cert_requested(Port, Sni, Version, Extra) ->
    Id = cert_request_probe,
    Level = maps:get(level, logger:get_primary_config()),
    {ok, #{level := DefLevel}} = logger:get_handler_config(default),
    ok = logger:set_primary_config(level, all),
    ok = logger:set_handler_config(default, level, notice),
    ok = logger:add_handler(Id, ?MODULE, #{config => self(), level => all}),
    try
        {ok, S} = connect(Port, Sni, Version,
                          [{log_level, debug} | Extra]),
        ok = ssl:close(S),
        collect_cert_requests(false)
    after
        _ = logger:remove_handler(Id),
        ok = logger:set_handler_config(default, level, DefLevel),
        ok = logger:set_primary_config(level, Level)
    end.

collect_cert_requests(Seen) ->
    receive
        {handshake_logged, Event} ->
            Text = lists:flatten(io_lib:format("~p", [Event])),
            collect_cert_requests(
              Seen orelse string:find(Text, "certificate_request")
                  =/= nomatch)
    after 0 -> Seen
    end.

%% logger handler callback
log(Event, #{config := Pid}) ->
    Pid ! {handshake_logged, Event},
    ok.

fetch(Port, Host, Version, Extra, Path) ->
    {ok, S} = connect(Port, Host, Version, Extra),
    try
        ok = ssl:send(S, request(Host, Port, Path)),
        recv_all(S, <<>>)
    after
        ssl:close(S)
    end.

request(Host, Port, Path) ->
    ["gemini://", Host, ":", integer_to_list(Port), Path, "\r\n"].

fetch(Port, Sni, Host, Path) ->
    converse(Port, Sni, [request(Host, Port, Path)]).

converse(Port, Sni, Sends) ->
    case connect(Port, Sni) of
        {ok, S} ->
            try
                [ ok = ssl:send(S, D) || D <- Sends ],
                recv_all(S, <<>>)
            after
                ssl:close(S)
            end;
        {error, _} = Error -> Error
    end.

recv_all(S, Acc) ->
    case ssl:recv(S, 0, ?TIMEOUT) of
        {ok, Data} -> recv_all(S, <<Acc/binary, Data/binary>>);
        {error, closed} -> {ok, Acc};
        {error, Why} -> {error, {Why, Acc}}
    end.

split_header(Response) ->
    [Header, Body] = binary:split(Response, <<"\r\n">>),
    {Header, Body}.

wait_for_file(Path) ->
    wait_for_file(Path, 50).

wait_for_file(Path, 0) ->
    {error, {no_such_file, Path}};
wait_for_file(Path, N) ->
    case filelib:is_regular(Path) of
        true -> ok;
        false -> timer:sleep(100), wait_for_file(Path, N - 1)
    end.
