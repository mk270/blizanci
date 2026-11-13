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
-export([static/1, cgi/1, titan/1, sni_live_apply/1, restart/1]).

-define(HOST, "first.test").
-define(SECOND, "second.test").
-define(TIMEOUT, 5000).

all() ->
    [static, cgi, titan, sni_live_apply, restart].

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
    [{port, Port}, {docroot, Docroot}, {cgiroot, CgiRoot} | Config].

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
