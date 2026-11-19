%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_connection_tests).

-include_lib("eunit/include/eunit.hrl").
-include("blizanci_types.hrl").

-import(blizanci_connection,
        [code_change/3, construct_url/4, handle_call/3, handle_cast/2,
         handle_info/2, handle_request/2, resolve_site/2, respond/2,
         servlet_result/2, shape/1, terminate/2]).

%% A redirect's location is the decoded path, encoded again.
construct_url_test_() ->
    U = fun(Path) -> construct_url(<<"gemini">>, <<"h">>, 1965, Path) end,
    [?_assertEqual(<<"gemini://h/dir1/index.gemini">>,
                   U(<<"dir1/index.gemini">>)),
     ?_assertEqual(<<"gemini://h/sp%20ace/index.gemini">>,
                   U(<<"sp ace/index.gemini">>)),
     ?_assertEqual(<<"gemini://h/100%25/index.gemini">>,
                   U(<<"100%/index.gemini">>)),
     ?_assertEqual(<<"gemini://h/x%3Fy%23z/index.gemini">>,
                   U(<<"x?y#z/index.gemini">>)),
     % the default port is left out; any other is kept
     ?_assertEqual(<<"gemini://h:1966/dir1/index.gemini">>,
                   construct_url(<<"gemini">>, <<"h">>, 1966,
                                 <<"dir1/index.gemini">>)),
     % what the client receives decodes back to the path
     ?_test(begin
                Path = <<"a b/100%/x?y#z;w/index.gemini">>,
                Url = U(Path),
                #{ path := Encoded } = uri_string:parse(Url),
                <<"/", Raw/binary>> = Encoded,
                ?assertEqual({ok, {decoded_path, Path}},
                             blizanci_urlpath:decode_path(Raw))
            end)].

%% A client that disconnects right after the handshake must make the
%% connection process exit normally, not crash.
resolve_site_client_gone_test_() ->
    {setup,
     fun() -> {ok, _} = application:ensure_all_started(ssl) end,
     fun(_) -> ok end,
     {timeout, 30, fun resolve_site_client_gone/0}}.

resolve_site_client_gone() ->
    Rsa = [{key, {rsa, 2048, 17}}],
    #{server_config := ServerConf, client_config := ClientConf} =
        public_key:pkix_test_data(#{server_chain => #{root => Rsa,
                                                      peer => Rsa},
                                    client_chain => #{root => Rsa,
                                                      peer => Rsa}}),
    {ok, Listen} = ssl:listen(
                     0, ServerConf ++ [{versions,
                                        blizanci_config:tls_versions()},
                                       {active, false}]),
    {ok, {_, Port}} = ssl:sockname(Listen),
    Self = self(),
    {Pid, Ref} = spawn_monitor(
                   fun() ->
                       {ok, T} = ssl:transport_accept(Listen),
                       {ok, S} = ssl:handshake(T, 5000),
                       Self ! {handshaken, self()},
                       {error, closed} = ssl:recv(S, 0, 5000),
                       resolve_site(S, ranch_ssl)
                   end),
    {ok, C} = ssl:connect("localhost", Port,
                          ClientConf ++ [{verify, verify_none},
                                         {versions,
                                          blizanci_config:tls_versions()},
                                         {server_name_indication,
                                          "localhost"}]),
    receive {handshaken, Pid} -> ok after 5000 -> error(no_handshake) end,
    ok = ssl:close(C),
    receive
        {'DOWN', Ref, process, Pid, Reason} ->
            ?assertEqual(normal, Reason)
    after 5000 ->
        error(server_did_not_exit)
    end,
    ssl:close(Listen).


%% What the client is sent when the servlet container reports that a
%% servlet refused the request before its worker started: the reason
%% reaches the client where the status table has a failure status for it
%% (a missing CGI script gives 51), and anything else is a 40. The exit
%% message fed to handle_info/2 is the container's real one, and the
%% client is a real TCP socket. Fixture, under a fresh temporary directory:
%%   Base/cgi/            (empty)
refused_request_test_() ->
    {setup, fun refused_setup/0, fun refused_teardown/1,
     fun refused_tests/1}.

refused_setup() ->
    Base = blizanci_test_support:tmp_path("connection_refused_test"),
    ok = filelib:ensure_path(filename:join(Base, "cgi")),
    Base.

refused_teardown(Base) ->
    file:del_dir_r(Base).

%% Feed handle_info/2 the exit message of a container whose servlet
%% module's serve/4 refused the request, and return the bytes the client
%% receives.
-spec client_sees(Module, Matches, RouteOpts) -> Result
              when Module    :: module(),
                   Matches   :: path_matches(),
                   RouteOpts :: map(),
                   Result    :: binary().
client_sees(Module, Matches, RouteOpts) ->
    process_flag(trap_exit, true),
    Cfg = #server_config{hostname = <<"h">>, port = 1965, routing = []},
    Req = #{ client_cert => {error, no_peercert}, query => <<>>,
             rest_of_input => <<>>, scheme => <<"gemini">>, host => <<"h">>,
             port => 1965, path => <<>> },
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}]),
    {ok, Port} = inet:port(Listen),
    {ok, Client} = gen_tcp:connect({127, 0, 0, 1}, Port,
                                   [binary, {active, false}]),
    {ok, Server} = gen_tcp:accept(Listen, 5000),
    {ok, Pid} = proc_lib:start_link(blizanci_servlet_container, init,
                                    [[self(), Module, Matches, Req, Cfg,
                                      RouteOpts]]),
    receive
        {'EXIT', Pid, _} = Exit ->
            State = #state{transport = ranch_tcp, socket = Server,
                           buffer = <<>>, config = Cfg, requested = true,
                           servlet_proc = {proc, Pid},
                           client_cert = {error, no_peercert}},
            {noreply, _} = handle_info(Exit, State)
    after 5000 ->
        error(servlet_did_not_exit)
    end,
    {ok, Data} = gen_tcp:recv(Client, 0, 5000),
    ok = gen_tcp:close(Client),
    ok = gen_tcp:close(Server),
    ok = gen_tcp:close(Listen),
    Data.

refused_tests(Base) ->
    Opts = #{ cgiroot => filename:join(Base, <<"cgi">>),
              cgiprefix => "/cgi-bin/" },
    Cgi = fun(Path) ->
              client_sees(blizanci_cgi, #{ <<"PATH">> => Path }, Opts)
          end,
    [?_assertEqual(<<"51 File not found\r\n">>, Cgi(<<"nosuch">>)),
     ?_assertEqual(<<"51 File not found\r\n">>, Cgi(<<".hidden">>)),
     ?_assertEqual(<<"51 File not found\r\n">>, Cgi(<<"a b">>)),
     ?_assertEqual(<<"40 Not implemented\r\n">>,
                   client_sees(blizanci_static, #{}, #{}))].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests: respond/2, handle_request/2, handle_info/2 and handle_call/3,
%% with a loopback TCP socket in place of the TLS one (ranch_tcp gives
%% the same transport interface), and the stub servlet in
%% test/blizanci_test_stub.erl behind a real route. Each test runs in a
%% process of its own, as deferring a request makes its caller trap exits.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(STUB, blizanci_test_stub).
-define(REQLINE, <<"gemini://h/x\r\n">>).

%% Hide a value's type from dialyzer, for tests that break a spec.
-spec any(term()) -> term().
any(X) -> erlang:binary_to_term(erlang:term_to_binary(X)).

-spec isolated(Fun) -> Result
              when Fun    :: fun(() -> term()),
                   Result :: term().
isolated(Fun) ->
    Self = self(),
    Ref = make_ref(),
    {Pid, MRef} = spawn_monitor(fun() -> Self ! {Ref, Fun()} end),
    receive
        {Ref, Result} ->
            erlang:demonitor(MRef, [flush]),
            Result;
        {'DOWN', MRef, process, Pid, Why} ->
            {test_process_died, Why}
    after 10000 ->
        exit(Pid, kill),
        error(test_timeout)
    end.

dead_pid() ->
    Pid = spawn(fun() -> ok end),
    Ref = erlang:monitor(process, Pid),
    receive {'DOWN', Ref, process, Pid, _} -> Pid end.

drain(Client, Acc) ->
    case gen_tcp:recv(Client, 0, 2000) of
        {ok, Data} -> drain(Client, <<Acc/binary, Data/binary>>);
        {error, _} -> Acc
    end.

%% Run Fun with the server end of a loopback connection, in a process of
%% its own, and return {FunResult, WhatTheClientReceived}.
-spec with_client(Fun) -> Result
              when Fun    :: fun((port()) -> term()),
                   Result :: {term(), binary()}.
with_client(Fun) ->
    isolated(fun() ->
        {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}]),
        {ok, Port} = inet:port(Listen),
        {ok, Client} = gen_tcp:connect({127, 0, 0, 1}, Port,
                                       [binary, {active, false}]),
        {ok, Server} = gen_tcp:accept(Listen, 5000),
        Result = Fun(Server),
        _ = gen_tcp:close(Server),
        Data = drain(Client, <<>>),
        ok = gen_tcp:close(Client),
        ok = gen_tcp:close(Listen),
        {Result, Data}
    end).

%% Routing for the stub servlet: every path goes to it.
stub_config(Script) ->
    Routes = [{gemini, <<"(?<PATH>.*)">>, ?STUB, public,
               #{ script => Script }}],
    {ok, Routing} = blizanci_router:prepare(Routes),
    #server_config{hostname = <<"h">>, port = 1965, routing = Routing}.

conn_state(Socket, Proc, Script) ->
    #state{transport = ranch_tcp, socket = Socket, buffer = <<>>,
           config = stub_config(Script), requested = false,
           servlet_proc = Proc, client_cert = {error, no_peercert}}.

conn_state(Socket) ->
    conn_state(Socket, no_proc, fun(request) -> {immediate, none} end).

blank_state() ->
    conn_state(none).

%% What the client is sent for a response.
sent(Response) ->
    {_, Data} = with_client(fun(S) -> respond(Response, conn_state(S)) end),
    Data.

%% Start a servlet through a real request, with a stub worker that
%% answers client data with Handler(Payload), and return its pid.
start_servlet(Socket, Handler) ->
    Worker = (any(?STUB)):worker(self(), Handler),
    Script = fun(request) -> defer;
                (serve) -> {gateway_started, Worker}
             end,
    State = conn_state(Socket, no_proc, Script),
    {<<>>, {init_servlet, Pid}} = handle_request(?REQLINE, State),
    {Pid, Worker, State#state{servlet_proc = {proc, Pid}}}.

respond_test_() ->
    [% nothing to say yet: stay connected, send nothing
     ?_assertEqual({continue, <<>>},
                   with_client(fun(S) ->
                                   respond(none, conn_state(S))
                               end)),
     ?_test(begin
                Pid = dead_pid(),
                ?assertEqual({{expect_servlet, Pid}, <<>>},
                             with_client(fun(S) ->
                                 respond({init_servlet, Pid},
                                         conn_state(S))
                             end))
            end),
     ?_assertEqual({finished, <<>>},
                   with_client(fun(S) ->
                                   respond(hangup, conn_state(S))
                               end)),
     ?_assertEqual({finished, <<"20 text/gemini\r\nhi">>},
                   with_client(fun(S) ->
                       respond({success, <<"text/gemini">>, <<"hi">>},
                               conn_state(S))
                   end)),
     ?_assertEqual({finished, <<"20 text/gemini\r\n">>},
                   with_client(fun(S) ->
                       respond({success, <<"text/gemini">>, <<>>},
                               conn_state(S))
                   end)),
     ?_assertEqual({finished, <<"51 File not found\r\n">>},
                   with_client(fun(S) ->
                       respond({error_code, file_not_found},
                               conn_state(S))
                   end)),
     ?_assertEqual({finished, <<"59 Request too long\r\n">>},
                   with_client(fun(S) ->
                       respond({error_code, request_too_long},
                               conn_state(S))
                   end)),
     ?_assertEqual({finished, <<"40 Gateway too busy\r\n">>},
                   with_client(fun(S) ->
                       respond({error_code, gateway_busy},
                               conn_state(S))
                   end)),
     % a redirect's path is turned into a URL of this server
     ?_assertEqual({finished,
                    <<"31 gemini://h/dir1/index.gemini\r\n">>},
                   with_client(fun(S) ->
                       respond({redirect, <<"dir1/index.gemini">>},
                               conn_state(S))
                   end)),
     % an error atom with no status is a 40
     ?_assertEqual({finished, <<"40 Internal server error\r\n">>},
                   with_client(fun(S) ->
                       respond({error_code, no_such_status},
                               conn_state(S))
                   end))].

%% Any reply respond/2 has no clause for is a 40 and finishes the
%% connection, rather than crashing it.
respond_unexpected_test_() ->
    [?_assertEqual({finished, <<"40 Internal server error\r\n">>},
                   with_client(fun(S) ->
                       respond(any(Reply), conn_state(S))
                   end)) ||
        Reply <- [ok, {no, such, reply}, {servlet_output}, "text",
                  <<"bytes">>, 42, undefined]].

%% The shape of a term, without its contents.
shape_test_() ->
    [?_assertEqual(Want, shape(Term)) ||
        {Term, Want} <- [{ok, ok},
                         {{a, <<"secret">>}, {tuple, 2, a}},
                         {{<<"x">>, y}, {tuple, 2, none}},
                         {{}, {tuple, 0, none}},
                         {<<"secret">>, binary},
                         {"secret", list},
                         {7, number},
                         {fun() -> ok end, other}]].

respond_file_test_() ->
    {timeout, 30, fun respond_file/0}.

respond_file() ->
    Dir = blizanci_test_support:tmp_path("connection_file_test"),
    ok = filelib:ensure_path(Dir),
    File = list_to_binary(filename:join(Dir, "f.gmi")),
    ok = file:write_file(File, <<"# hello\n">>),
    try
        ?assertEqual({finished, <<"20 text/gemini\r\n# hello\n">>},
                     with_client(fun(S) ->
                         respond({file, <<"text/gemini">>, File},
                                 conn_state(S))
                     end)),
        Missing = <<File/binary, ".missing">>,
        ?assertEqual({finished, <<"51 File not found\r\n">>},
                     with_client(fun(S) ->
                         respond({file, <<"text/gemini">>, Missing},
                                 conn_state(S))
                     end)),
        % a directory is not a file
        ?assertEqual({finished, <<"51 File not found\r\n">>},
                     with_client(fun(S) ->
                         respond({file, <<"text/gemini">>,
                                  list_to_binary(Dir)},
                                 conn_state(S))
                     end)),
        % no read permission (skipped where it stays readable, as root)
        case respond_file_unreadable(File) of
            skipped -> ok;
            Got -> ?assertEqual({finished, <<"40 Internal server error"
                                             "\r\n">>}, Got)
        end,
        % empty file: still a success
        Empty = list_to_binary(filename:join(Dir, "empty.gmi")),
        ok = file:write_file(Empty, <<>>),
        ?assertEqual({finished, <<"20 text/gemini\r\n">>},
                     with_client(fun(S) ->
                         respond({file, <<"text/gemini">>, Empty},
                                 conn_state(S))
                     end)),
        % several transport chunks
        Big = binary:copy(<<"0123456789abcdef">>, 300 * 1024 div 16),
        BigF = list_to_binary(filename:join(Dir, "big.bin")),
        ok = file:write_file(BigF, Big),
        ?assertEqual({finished, <<"20 text/plain\r\n", Big/binary>>},
                     with_client(fun(S) ->
                         respond({file, <<"text/plain">>, BigF},
                                 conn_state(S))
                     end))
    after
        file:del_dir_r(Dir)
    end.

respond_file_unreadable(File) ->
    ok = file:change_mode(File, 8#000),
    try file:open(File, [read, raw, binary]) of
        {ok, Fd} ->
            ok = file:close(Fd),
            skipped;
        {error, _} ->
            with_client(fun(S) ->
                respond({file, <<"text/gemini">>, File}, conn_state(S))
            end)
    after
        file:change_mode(File, 8#644)
    end.

%% A servlet's output is a complete response, parsed before being sent.
respond_servlet_output_test_() ->
    Out = fun(Bin) -> sent({servlet_output, Bin}) end,
    [?_assertEqual({finished, <<"20 text/gemini\r\nbody\r\nmore">>},
                   with_client(fun(S) ->
                       respond({servlet_output,
                                <<"20 text/gemini\r\nbody\r\nmore">>},
                               conn_state(S))
                   end)),
     ?_assertEqual(<<"31 /elsewhere\r\n">>, Out(<<"31 /elsewhere\r\n">>)),
     % no CRLF, no space, a non-number, or a status not in the protocol
     ?_assertEqual(<<"40 Gateway error\r\n">>, Out(<<"20 text/gemini">>)),
     ?_assertEqual(<<"40 Gateway error\r\n">>, Out(<<"\r\nbody">>)),
     ?_assertEqual(<<"40 Gateway error\r\n">>, Out(<<"ok text/x\r\nb">>)),
     ?_assertEqual(<<"40 Gateway error\r\n">>, Out(<<"99 text/x\r\nb">>)),
     ?_assertEqual(<<"40 Gateway error\r\n">>, Out(<<"20\r\nb">>)),
     ?_assertEqual(<<"40 Gateway error\r\n">>, Out(<<>>))].

%% A request that has not arrived whole is buffered, whatever comes next.
handle_request_buffering_test_() ->
    Req = fun(Buffer, Payload) ->
              {R, _} = with_client(fun(S) ->
                           handle_request(Payload,
                                          (conn_state(S))#state{
                                            buffer = Buffer})
                       end),
              R
          end,
    [?_assertEqual({<<"gemini://h/x">>, none},
                   Req(<<>>, <<"gemini://h/x">>)),
     ?_assertEqual({<<"gemini://h/x\r">>, none},
                   Req(<<"gemini://h/">>, <<"x\r">>)),
     % a CRLF split across two packets is still found
     ?_assertEqual({<<>>, none}, Req(<<"gemini://h/x\r">>, <<"\n">>)),
     ?_assertEqual({<<>>, {error_code, request_too_long}},
                   Req(<<>>, binary:copy(<<"a">>, 4001))),
     ?_assertEqual({binary:copy(<<"a">>, 4000), none},
                   Req(<<>>, binary:copy(<<"a">>, 4000))),
     % a long line that is complete is judged by the request parser
     ?_assertEqual({<<>>, {error_code, request_too_long}},
                   Req(<<>>, <<(binary:copy(<<"a">>, 1030))/binary,
                               "\r\n">>))].

%% A complete request line is routed, and what the servlet module says
%% comes back as the response.
handle_request_routing_test_() ->
    Req = fun(Line, Script) ->
              {R, _} = with_client(fun(S) ->
                           handle_request(Line,
                                          conn_state(S, no_proc, Script))
                       end),
              R
          end,
    Imm = fun(Resp) -> fun(request) -> {immediate, Resp} end end,
    [?_assertEqual({<<>>, {success, <<"text/gemini">>, <<"yo">>}},
                   Req(?REQLINE,
                       Imm({success, <<"text/gemini">>, <<"yo">>}))),
     ?_assertEqual({<<>>, {error_code, file_not_found}},
                   Req(?REQLINE, Imm({error_code, file_not_found}))),
     ?_assertEqual({<<>>, none}, Req(?REQLINE, Imm(none))),
     ?_assertMatch({<<>>, {init_servlet, _}},
                   Req(?REQLINE, fun(request) -> defer;
                                 (serve) -> {gateway_finished, none}
                              end)),
     % what comes after the CRLF is not part of the request
     ?_assertEqual({<<>>, none},
                   Req(<<"gemini://h/x\r\nmore">>, Imm(none))),
     ?_assertEqual({<<>>, {error_code, request_not_parsed}},
                   Req(<<"x:y z\r\n">>, Imm(none))),
     ?_assertEqual({<<>>, {error_code, host_unrecognised}},
                   Req(<<"gemini://other/x\r\n">>, Imm(none))),
     ?_assertEqual({<<>>, {error_code, port_unrecognised}},
                   Req(<<"gemini://h:1/x\r\n">>, Imm(none))),
     ?_assertEqual({<<>>, {error_code, unrecognised_protocol}},
                   Req(<<"ftp://h/x\r\n">>, Imm(none))),
     ?_assertEqual({<<>>, {error_code, proxy_refused}},
                   Req(<<"https://h/x\r\n">>, Imm(none))),
     ?_assertEqual({<<>>, {error_code, bad_filename}},
                   Req(<<"gemini://h/a..b\r\n">>, Imm(none)))].

%% Once a servlet has been started, further client data goes to it.
handle_request_servlet_test_() ->
    [?_assertEqual({<<>>, {success, <<"text/plain">>, <<"ab">>}},
                   with_servlet_data(
                     fun(P) -> {success, <<"text/plain">>, P} end,
                     <<"a">>, <<"b">>)),
     ?_assertEqual({<<>>, none},
                   with_servlet_data(fun(_) -> none end, <<>>, <<"b">>)),
     % a servlet that has already finished is an internal error
     ?_assertEqual({<<>>, {error_code, internal_server_error}},
                   isolated(fun() ->
                       {R, _} = with_client(fun(S) ->
                           St = conn_state(S),
                           any(handle_request(
                                 <<"x">>,
                                 St#state{servlet_proc =
                                              {proc, dead_pid()}}))
                       end),
                       R
                   end)),
     % a servlet that fails on client data is an internal error
     ?_assertEqual({<<>>, {error_code, internal_server_error}},
                   with_servlet_data(fun(_) -> {raise, boom} end,
                                     <<>>, <<"b">>))].

%% Start a servlet, then give the connection the buffer Buffer and the
%% payload Payload, as it arrives.
with_servlet_data(Handler, Buffer, Payload) ->
    {R, _} = with_client(fun(S) ->
        {_Pid, _Worker, State} = start_servlet(S, Handler),
        handle_request(Payload, State#state{buffer = Buffer})
    end),
    R.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_info/2

%% Run a message through handle_info/2 with no servlet; return the
%% callback's result with the state elided, and what the client got.
info(Msg) ->
    info(Msg, no_proc).

info(Msg, Proc) ->
    with_client(fun(S) ->
        State = (conn_state(S))#state{servlet_proc = Proc},
        case handle_info(Msg, State) of
            {stop, Why, _} -> {stop, Why};
            {noreply, _} -> noreply
        end
    end).

handle_info_simple_test_() ->
    [?_assertEqual({{stop, normal}, <<>>}, info({tcp_closed, sock})),
     ?_assertEqual({{stop, normal}, <<>>}, info({tcp_error, sock, why})),
     ?_assertEqual({{stop, normal}, <<>>}, info({ssl_closed, closed})),
     ?_assertEqual({{stop, normal}, <<>>}, info(finished)),
     ?_assertEqual({{stop, normal}, <<>>}, info(whatever))].

%% The 30 second timer: the client is told which of the two it was, and
%% a finished message is queued for the process.
handle_info_timeout_test_() ->
    [?_assertEqual({{stop, normal}, <<"40 Timeout\r\n">>},
                   info(connection_timeout)),
     ?_assertEqual({{stop, normal}, <<"59 Request timeout\r\n">>},
                   info(connection_timeout, {proc, self()})),
     ?_assertEqual({true, <<"40 Timeout\r\n">>},
                   with_client(fun(S) ->
         {stop, normal, _} = handle_info(connection_timeout,
                                  conn_state(S)),
         receive finished -> true after 1000 -> false end
     end))].

%% The container's exit, as the connection sees it: the servlet is the
%% process in servlet_proc, anything else is an abend.
handle_info_exit_test_() ->
    P = self(),
    Ex = fun(Pid, Reason) ->
             info({'EXIT', Pid, Reason}, {proc, P})
         end,
    [?_assertEqual({noreply, <<"40 Internal server error\r\n">>},
                   Ex(P, {shutdown, {gateway_init_error, P, why}})),
     ?_assertEqual({noreply, <<"51 File not found\r\n">>},
                   Ex(P, {shutdown, {gateway_complete, P,
                                     {error_code, file_not_found}}})),
     ?_assertEqual({noreply, <<"20 text/gemini\r\nhi">>},
                   Ex(P, {shutdown, {gateway_complete, P,
                                     {success, <<"text/gemini">>,
                                      <<"hi">>}}})),
     ?_assertEqual({noreply, <<>>},
                   Ex(P, {shutdown, {gateway_complete, P, none}})),
     % the servlet dying any other way: an internal error
     ?_assertEqual({noreply, <<"40 Internal server error\r\n">>},
                   Ex(self(), crashed)),
     % an exit from a process that is not the servlet is not an answer
     ?_assertEqual({noreply, <<>>}, Ex(dead_pid(), normal)),
     ?_assertEqual({noreply, <<>>}, Ex(dead_pid(), crashed)),
     ?_assertEqual({noreply, <<>>},
                   Ex(dead_pid(), {shutdown, {gateway_complete, P,
                                              {error_code,
                                               file_not_found}}})),
     ?_assertEqual({noreply, <<>>},
                   info({'EXIT', P, {shutdown, {gateway_init_error, P,
                                                why}}}, no_proc)),
     ?_assertEqual({noreply, <<>>}, info({'EXIT', P, crashed}, no_proc))].

%% The connection traps exits from init on (the test stops it in the
%% handshake, which never comes), and a shutdown from the parent still
%% ends it.
connection_traps_exits_test_() ->
    [?_test(begin
                Pid = proc_lib:spawn(blizanci_connection, init,
                                     [{no_such_ref, ranch_tcp, []}]),
                Ref = erlang:monitor(process, Pid),
                ok = wait_trapping(Pid, 100),
                exit(Pid, kill),
                receive {'DOWN', Ref, process, Pid, _} -> ok end
            end),
     ?_test(begin
                % under gen_server:enter_loop, as the connection runs,
                % the parent's shutdown stops it
                Self = self(),
                {Parent, PRef} = spawn_monitor(fun() ->
                    Child = proc_lib:spawn_link(fun() ->
                        process_flag(trap_exit, true),
                        gen_server:enter_loop(blizanci_connection, [], blank_state())
                    end),
                    Self ! {child, Child},
                    receive stop -> exit(Child, shutdown) end,
                    receive never -> ok end
                end),
                Child = receive {child, C} -> C after 2000 -> error(no) end,
                CRef = erlang:monitor(process, Child),
                Parent ! stop,
                receive {'DOWN', CRef, process, Child, Why} ->
                    ?assertEqual(shutdown, Why)
                after 2000 -> error(still_alive)
                end,
                exit(Parent, kill),
                receive {'DOWN', PRef, process, Parent, _} -> ok end
            end)].

wait_trapping(_Pid, 0) ->
    timeout;
wait_trapping(Pid, N) ->
    case erlang:process_info(Pid, trap_exit) of
        {trap_exit, true} -> ok;
        _ -> timer:sleep(10), wait_trapping(Pid, N - 1)
    end.

%% Data from the client (messages {ssl, Socket, Data} on the real thing).
handle_info_data_test_() ->
    Data = fun(Payload, Script) ->
               with_client(fun(S) ->
                   State = conn_state(S, no_proc, Script),
                   case handle_info({ssl, S, Payload}, State) of
                       {noreply, N} ->
                           {noreply, N#state.buffer, N#state.servlet_proc};
                       {stop, Why, _} -> {stop, Why}
                   end
               end)
           end,
    Imm = fun(Resp) -> fun(request) -> {immediate, Resp} end end,
    [% a whole request with an immediate answer: sent, and finished
     ?_assertEqual({{stop, normal}, <<"20 text/gemini\r\nhi">>},
                   Data(?REQLINE, Imm({success, <<"text/gemini">>,
                                    <<"hi">>}))),
     ?_assertEqual({{stop, normal}, <<"51 File not found\r\n">>},
                   Data(?REQLINE, Imm({error_code, file_not_found}))),
     ?_assertEqual({{stop, normal}, <<"59 Request not parsed\r\n">>},
                   Data(<<"x:y z\r\n">>, Imm(none))),
     % part of a request is buffered
     ?_assertEqual({{noreply, <<"gemini://h/">>, no_proc}, <<>>},
                   Data(<<"gemini://h/">>, Imm(none))),
     % an answer of 'none' to a complete request keeps the connection
     ?_assertEqual({{noreply, <<>>, no_proc}, <<>>},
                   Data(?REQLINE, Imm(none))),
     % a deferred request: the servlet is now expected
     ?_assertMatch({{noreply, <<>>, {proc, _}}, <<>>},
                   Data(?REQLINE, fun(request) -> defer;
                                  (serve) -> {gateway_finished, none}
                               end)),
     % so is a reply nobody has a clause for, such as 'ok'
     ?_assertEqual({{stop, normal}, <<"40 Internal server error\r\n">>},
                   Data(?REQLINE, Imm(any(ok)))),
     ?_assertEqual({{stop, normal}, <<"40 Internal server error\r\n">>},
                   Data(?REQLINE, Imm(any({no, such, reply})))),
     % an error thrown by a servlet is a 40, and finishes the connection
     ?_assertEqual({{stop, normal}, <<"40 Internal server error\r\n">>},
                   Data(?REQLINE, fun(request) -> throw(oops) end)),
     % a socket that has gone away: nothing is sent, the process stops
     ?_assertEqual({{stop, normal}, <<>>},
                   with_client(fun(S) ->
                       State = conn_state(S, no_proc,
                                          Imm({success, <<"t">>, <<"x">>})),
                       ok = gen_tcp:close(S),
                       {stop, Why, _} = handle_info({ssl, S, ?REQLINE},
                                                    State),
                       {stop, Why}
                   end)),
     % so is an error or an exit, whatever the servlet's request/4 does
     ?_assertEqual({{stop, normal}, <<"40 Internal server error\r\n">>},
                   Data(?REQLINE, fun(request) -> error(boom) end)),
     ?_assertEqual({{stop, normal}, <<"40 Internal server error\r\n">>},
                   Data(?REQLINE, fun(request) -> exit(boom) end)),
     ?_assertEqual({{stop, normal}, <<"40 Internal server error\r\n">>},
                   Data(?REQLINE, fun(request) ->
                                      erlang:error(function_clause)
                                  end))].

%% Client data that arrives while a servlet runs goes to the servlet.
handle_info_servlet_data_test_() ->
    [?_assertEqual({{noreply, <<>>}, <<>>},
                   with_client(fun(S) ->
                       {_, _, State} =
                           start_servlet(S, fun(_) -> none end),
                       {noreply, N} = handle_info({ssl, S, <<"x">>},
                                                  State),
                       {noreply, N#state.buffer}
                   end)),
     ?_assertEqual({{stop, normal}, <<"20 text/plain\r\nab">>},
                   with_client(fun(S) ->
                       {_, _, State} =
                           start_servlet(
                             S, fun(P) ->
                                    {success, <<"text/plain">>, P}
                                end),
                       {stop, Why, _} =
                           handle_info({ssl, S, <<"ab">>}, State),
                       {stop, Why}
                   end))].

%% Client data for a servlet whose container has already gone is answered
%% with a 40 (not a crash for want of a response clause).
dead_container_test() ->
    {R, Data} = with_client(fun(S) ->
        State = conn_state(S, {proc, dead_pid()},
                           fun(request) -> {immediate, none} end),
        case handle_info({ssl, S, <<"x">>}, State) of
            {stop, Why, _} -> {stop, Why};
            Other -> Other
        end
    end),
    ?assertEqual({stop, normal}, R),
    ?assertEqual(<<"40 Internal server error\r\n">>, Data).

%% The container reports to the connection while the connection is inside
%% a call to the container (client data sent as the script finishes).
%% The result must be delivered at once, not after a call timeout. The
%% container is held busy by a first client_data call from a helper, so
%% that the gateway_exit and then the connection's own client_data call
%% queue up behind it, in that order.
client_data_as_script_finishes_test() ->
    {Done, Data} = with_client(fun(S) ->
        Me = self(),
        Hold = fun(<<"hold">>) -> receive go -> none end;
                  (_) -> none
               end,
        P = proc_lib:spawn(fun() ->
            process_flag(trap_exit, true),
            {Cont, Worker, State} = start_servlet(S, Hold),
            Me ! {started, Cont, Worker},
            gen_server:enter_loop(blizanci_connection, [], State)
        end),
        PRef = erlang:monitor(process, P),
        {Cont, Worker} = receive {started, C, W} -> {C, W}
                         after 2000 -> error(no_start)
                         end,
        T0 = erlang:monotonic_time(millisecond),
        spawn(fun() ->
            blizanci_servlet_container:handle_client_data(Cont, <<"hold">>)
        end),
        timer:sleep(100),
        spawn(fun() ->
            blizanci_servlet_container:gateway_exit(
              Cont, {gateway_output, <<"20 text/gemini\r\nhi">>})
        end),
        timer:sleep(100),
        P ! {ssl, S, <<"more">>},
        timer:sleep(100),
        Worker ! go,
        Down = receive {'DOWN', PRef, process, P, Why} -> Why
               after 3000 -> still_waiting
               end,
        {Down, erlang:monotonic_time(millisecond) - T0}
    end),
    ?assertMatch({normal, Ms} when Ms < 2000, Done),
    ?assertEqual(<<"20 text/gemini\r\nhi">>, Data).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_cast/2 (the servlet's report), terminate/2, servlet_result/2

%% Report a servlet result as the container does; the trap_exit flag is
%% left alone, a finished message is queued, and the client is sent
%% the response.
report(Result) ->
    with_client(fun(S) ->
        {noreply, _} = handle_cast({servlet_result, Result},
                                   conn_state(S)),
        {trap_exit, TE} = process_info(self(), trap_exit),
        Finished = receive finished -> true after 1000 -> false end,
        {TE, Finished}
    end).

handle_call_test_() ->
    St = blank_state(),
    [?_assertEqual({{false, true}, <<"51 File not found\r\n">>},
                   report({servlet_failed, file_not_found})),
     ?_assertEqual({{false, true}, <<"40 Gateway error\r\n">>},
                   report({servlet_failed, cgi_exec_error})),
     ?_assertEqual({{false, true}, <<"20 text/gemini\r\nhi">>},
                   report({servlet_complete, <<"20 text/gemini\r\nhi">>})),
     ?_assertEqual({{false, true}, <<"40 Gateway error\r\n">>},
                   report({servlet_complete, <<"garbage">>})),
     % a gateway error with no status (the container passes any atom
     % on) is a 40
     ?_assertEqual({{false, true}, <<"40 Internal server error\r\n">>},
                   report({servlet_failed, no_such_status})),
     ?_assertEqual({{false, true}, <<"40 Not implemented\r\n">>},
                   report({servlet_failed, unimplemented})),
     ?_assertEqual({reply, ok, St}, handle_call(anything, from, St)),
     ?_assertEqual({noreply, St}, handle_cast(anything, St)),
     ?_assertEqual({ok, St}, code_change(old, St, extra))].

servlet_result_dead_connection_test() ->
    ?assertEqual(ok, servlet_result(dead_pid(),
                                    {servlet_failed, file_not_found})).

%% Closing the session cancels a running servlet, and is harmless without.
terminate_test_() ->
    [?_assertEqual(ok, terminate(normal, blank_state())),
     ?_assertEqual(ok, terminate(normal,
                                 (blank_state())#state{
                                   servlet_proc = {proc, dead_pid()}})),
     ?_assertEqual({ok, cancelled},
                   begin
                       {R, _} = with_client(fun(S) ->
                           {_Pid, Worker, State} =
                               start_servlet(S, fun(_) -> none end),
                           Ok = terminate(normal, State),
                           Got = receive
                                     {worker_cancelled, Worker} ->
                                         cancelled
                                 after 5000 -> not_cancelled
                                 end,
                           {Ok, Got}
                       end),
                       R
                   end)].
