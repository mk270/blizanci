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
         no_leaks/1]).

-define(HOST, "first.test").
-define(SECOND, "second.test").
-define(TIMEOUT, 5000).
-define(LEAK_TIMEOUT, 5000).

all() ->
    [static, cgi, titan, sni_live_apply, restart, no_leaks].

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
    [{certfile, Cert}, {keyfile, Key} | Config].

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
      || K <- [port, hostname, certfile, keyfile, docroot, routing] ],
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
    ssl:connect({127,0,0,1}, Port,
                [binary, {active, false}, {verify, verify_none},
                 {versions, ['tlsv1.3']},
                 {server_name_indication, Sni}], ?TIMEOUT).

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
