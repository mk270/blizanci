%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_servlet_container_tests).

-include_lib("eunit/include/eunit.hrl").
-include("blizanci_types.hrl").
-include("blizanci_servlet_container.hrl").

-import(blizanci_servlet_container,
        [request/5, cancel/1, gateway_exit/2, handle_client_data/2,
         handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).


%% A servlet that refuses a request before its worker starts returns
%% {gateway_error, Reason} from serve/4. A Reason the status table maps to
%% a failure status becomes the client's answer ({error_code, Reason});
%% any other Reason stays an internal failure. Only serve/4 refusals that
%% need no worker pool are used here (CGI refuses before queueing, and
%% blizanci_static:serve/4 is a stub that returns 'unimplemented', which
%% the status table maps to 40).
%% Fixture, under a fresh temporary directory:
%%   Base/cgi/{script, .hidden}
init_refusal_test_() ->
    {setup, fun refusal_setup/0, fun refusal_teardown/1,
     fun refusal_tests/1}.

refusal_setup() ->
    Base = blizanci_test_support:tmp_path("container_refusal_test"),
    Dir = filename:join(Base, "cgi"),
    ok = filelib:ensure_path(Dir),
    [ ok = file:write_file(filename:join(Dir, F), <<"#!/bin/sh\n">>)
      || F <- ["script", ".hidden"] ],
    Base.

refusal_teardown(Base) ->
    file:del_dir_r(Base).

%% Run init/1 in a process of its own, as defer_request/5 does, and return
%% how it exited, with the exit's pid replaced by 'Pid'.
-spec init_exit(Module, Matches, Req, RouteOpts) -> Result
              when Module    :: module(),
                   Matches   :: path_matches(),
                   Req       :: request_details(),
                   RouteOpts :: map(),
                   Result    :: term().
init_exit(Module, Matches, Req, RouteOpts) ->
    process_flag(trap_exit, true),
    Cfg = #server_config{hostname = <<"h">>, port = 1965, routing = []},
    {ok, Pid} = proc_lib:start_link(blizanci_servlet_container, init,
                                    [[self(), Module, Matches, Req, Cfg,
                                      RouteOpts]]),
    receive
        {'EXIT', Pid, {shutdown, {Tag, Pid, Detail}}} -> {Tag, Detail}
    after 5000 ->
        error(servlet_did_not_exit)
    end.

refusal_request(Extra) ->
    maps:merge(#{ client_cert => {error, no_peercert},
                  query => <<>>,
                  rest_of_input => <<>>,
                  scheme => <<"gemini">>,
                  host => <<"h">>,
                  port => 1965,
                  path => <<>> }, Extra).

refusal_tests(Base) ->
    Dir = filename:join(Base, "cgi"),
    Opts = #{ cgiroot => list_to_binary(Dir), cgiprefix => "/cgi-bin/" },
    Cgi = fun(Path, Extra) ->
              init_exit(blizanci_cgi, #{ <<"PATH">> => Path },
                        refusal_request(Extra), Opts)
          end,
    [% a missing script, a hidden one and one outside the CGI alphabet
     % are file_not_found, the client's answer
     ?_assertEqual({gateway_complete, {error_code, file_not_found}},
                   Cgi(<<"nosuch">>, #{})),
     ?_assertEqual({gateway_complete, {error_code, file_not_found}},
                   Cgi(<<".hidden">>, #{})),
     ?_assertEqual({gateway_complete, {error_code, file_not_found}},
                   Cgi(<<"a b">>, #{})),
     % a client certificate that cannot be understood
     ?_assertEqual({gateway_complete, {error_code, cert_not_parsed}},
                   Cgi(<<"script">>,
                       #{ client_cert => {ok, <<"not a certificate">>} })),
     % the static stub's 'unimplemented' has a (40) status
     ?_assertEqual({gateway_complete, {error_code, unimplemented}},
                   init_exit(blizanci_static, #{}, refusal_request(#{}),
                             #{}))].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests: every result a servlet's serve/4 can return, and the rest of
%% the container's API, using the stub servlet in
%% test/blizanci_test_stub.erl. Each test runs in a process of its own,
%% as request/5 makes its caller trap exits and links the container to it.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(STUB, blizanci_test_stub).

%% Hide a value's type from dialyzer, for tests that break a spec.
-spec any(term()) -> term().
any(X) -> erlang:binary_to_term(erlang:term_to_binary(X)).

-spec isolated(Fun) -> Result
              when Fun    :: fun(() -> term()),
                   Result :: term().
isolated(Fun) ->
    Self = self(),
    Ref = make_ref(),
    % this process stands in for the connection, which traps exits from
    % its start, so that a container's exit reaches it as a message
    {Pid, MRef} = spawn_monitor(fun() ->
                                    process_flag(trap_exit, true),
                                    Self ! {Ref, Fun()}
                                end),
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

stub_request(Script) ->
    Cfg = #server_config{hostname = <<"h">>, port = 1965, routing = []},
    request(?STUB, #{ <<"PATH">> => <<"x">> }, refusal_request(#{}), Cfg,
            #{ script => Script }).

%% How a container whose serve/4 returned Result ended, with its pid
%% dropped from the exit reason.
serve_exit(Result) ->
    isolated(fun() ->
        {init_servlet, Pid} = stub_request(fun(request) -> defer;
                                              (serve) -> Result
                                           end),
        receive
            {'EXIT', Pid, {shutdown, {Tag, Pid, Detail}}} -> {Tag, Detail};
            {'EXIT', Pid, Other} -> {other_exit, Other}
        after 5000 -> container_did_not_exit
        end
    end).

%% Start a container with a live stub worker; Fun gets the container's
%% pid and the worker's pid, and is run in the (exit-trapping) test
%% process.
with_container(Handler, Fun) ->
    isolated(fun() ->
        Me = self(),
        Worker = (any(?STUB)):worker(Me, Handler),
        {init_servlet, Pid} =
            stub_request(fun(request) -> defer;
                            (serve) -> {gateway_started, Worker}
                         end),
        Fun(Pid, Worker)
    end).

dead_pid() ->
    Pid = spawn(fun() -> ok end),
    Ref = erlang:monitor(process, Pid),
    receive {'DOWN', Ref, process, Pid, _} -> Pid end.

request_immediate_test_() ->
    Imm = fun(Resp) ->
              isolated(fun() ->
                  stub_request(fun(request) -> {immediate, Resp} end)
              end)
          end,
    [?_assertEqual({success, <<"text/gemini">>, <<"hi">>},
                   Imm({success, <<"text/gemini">>, <<"hi">>})),
     ?_assertEqual({error_code, file_not_found},
                   Imm({error_code, file_not_found})),
     ?_assertEqual({redirect, <<"a/">>}, Imm({redirect, <<"a/">>})),
     ?_assertEqual(none, Imm(none))].

%% serve/4 results that end the container at once: the gemini_response
%% inside gateway_finished is passed to the connection untouched.
serve_finished_test_() ->
    [?_assertEqual({gateway_complete, {success, <<"text/gemini">>,
                                       <<"body">>}},
                   serve_exit({gateway_finished,
                               {success, <<"text/gemini">>, <<"body">>}})),
     ?_assertEqual({gateway_complete, {error_code, cgi_exec_error}},
                   serve_exit({gateway_finished,
                               {error_code, cgi_exec_error}})),
     ?_assertEqual({gateway_complete, {redirect, <<"d/">>}},
                   serve_exit({gateway_finished, {redirect, <<"d/">>}})),
     ?_assertEqual({gateway_complete, {file, <<"text/plain">>, <<"/f">>}},
                   serve_exit({gateway_finished,
                               {file, <<"text/plain">>, <<"/f">>}})),
     ?_assertEqual({gateway_complete, hangup},
                   serve_exit({gateway_finished, hangup})),
     ?_assertEqual({gateway_complete, none},
                   serve_exit({gateway_finished, none}))].

%% {gateway_error, R}: a reason the status table maps to a failure status
%% is the client's answer; any other is an internal failure.
serve_error_test_() ->
    [?_assertEqual({gateway_complete, {error_code, file_not_found}},
                   serve_exit({gateway_error, file_not_found})),
     ?_assertEqual({gateway_complete, {error_code, gateway_busy}},
                   serve_exit({gateway_error, gateway_busy})),
     ?_assertEqual({gateway_complete, {error_code, cert_not_parsed}},
                   serve_exit({gateway_error, cert_not_parsed})),
     ?_assertEqual({gateway_complete, {error_code, unimplemented}},
                   serve_exit({gateway_error, unimplemented})),
     ?_assertEqual({gateway_init_error, no_such_reason},
                   serve_exit(any({gateway_error, no_such_reason}))),
     % mapped, but not to a failure status
     ?_assertEqual({gateway_init_error, success},
                   serve_exit(any({gateway_error, success})))].

%% {gateway_started, Pid}: the container stays up and the caller is told
%% to expect a servlet.
serve_started_test_() ->
    [?_assert(with_container(fun(_) -> none end,
                             fun(Pid, _Worker) ->
                                 is_process_alive(Pid)
                             end))].

%% Results reported by the gateway after it started, via gateway_exit/2;
%% the test process plays the connection, receiving the container's
%% cast.
gateway_exit_result(Result) ->
    with_container(fun(_) -> none end,
        fun(Pid, _Worker) ->
            Me = self(),
            spawn(fun() -> Me ! {gw_reply, gateway_exit(Pid, Result)} end),
            Got = receive
                      {'$gen_cast', {servlet_result, SR}} -> SR
                  after 5000 -> no_servlet_result
                  end,
            Reply = receive {gw_reply, R} -> R after 5000 -> no_reply end,
            {Got, Reply, is_process_alive(Pid)}
        end).

gateway_exit_test_() ->
    [?_assertEqual({{servlet_complete, <<"20 text/gemini\r\nhi">>}, ok,
                    true},
                   gateway_exit_result({gateway_output,
                                        <<"20 text/gemini\r\nhi">>})),
     ?_assertEqual({{servlet_failed, file_not_found}, ok, true},
                   gateway_exit_result({gateway_error, file_not_found})),
     ?_assertEqual({{servlet_failed, cgi_output_too_large}, ok, true},
                   gateway_exit_result({gateway_error,
                                        cgi_output_too_large})),
     % no status mapping: still reported as a failure, as it is
     ?_assertEqual({{servlet_failed, no_such_status}, ok, true},
                   gateway_exit_result({gateway_error, no_such_status})),
     % a container that is already gone is not an error
     ?_assertEqual(ok, gateway_exit(dead_pid(), {gateway_output, <<>>})),
     % containers dying concurrently with the report never crash the
     % reporting worker
     ?_assert(lists:all(fun(_) -> gateway_exit_race() end,
                        lists:seq(1, 200)))].

gateway_exit_race() ->
    Pid = spawn(fun() -> receive stop -> ok end end),
    spawn(fun() -> exit(Pid, kill) end),
    ok =:= gateway_exit(Pid, {gateway_output, <<>>}).

handle_client_data_test_() ->
    Reply = fun(Handler, Payload) ->
                with_container(Handler,
                               fun(Pid, _W) ->
                                   handle_client_data(Pid, Payload)
                               end)
            end,
    [?_assertEqual({success, <<"text/plain">>, <<"abc">>},
                   Reply(fun(P) -> {success, <<"text/plain">>, P} end,
                         <<"abc">>)),
     ?_assertEqual(none, Reply(fun(_) -> none end, <<"abc">>)),
     % a servlet that fails while handling client data takes the container
     % down with an internal server error for the connection
     ?_assertEqual({gateway_complete,
                    {error_code, internal_server_error}},
                   with_container(
                     fun(_) -> {raise, boom} end,
                     fun(Pid, _W) ->
                         try handle_client_data(Pid, <<"x">>) of
                             R -> {returned, R}
                         catch
                             exit:_ ->
                                 receive
                                     {'EXIT', Pid,
                                      {shutdown, {Tag, Pid, D}}} ->
                                         {Tag, D}
                                 after 5000 -> no_exit
                                 end
                         end
                     end)),
     % a container that has already gone gives an error response
     ?_assertEqual({error_code, internal_server_error},
                   handle_client_data(dead_pid(), <<"x">>))].

cancel_test_() ->
    [?_assertEqual(ok, cancel(no_proc)),
     ?_assertEqual(ok, cancel({proc, dead_pid()})),
     % cancelling cancels the gateway and ends the container normally
     ?_assertEqual({cancelled, normal},
                   with_container(
                     fun(_) -> none end,
                     fun(Pid, Worker) ->
                         ok = cancel({proc, Pid}),
                         Cancelled = receive
                                         {worker_cancelled, Worker} ->
                                             cancelled
                                     after 5000 -> not_cancelled
                                     end,
                         Why = receive
                                   {'EXIT', Pid, W} -> W
                               after 5000 -> no_exit
                               end,
                         {Cancelled, Why}
                     end))].

%% The callbacks that do nothing much.
callbacks_test_() ->
    S = #servlet_state{},
    [?_assertEqual({reply, ok, S}, handle_call(anything, from, S)),
     ?_assertEqual({noreply, S}, handle_cast(anything, S)),
     ?_assertEqual({noreply, S}, handle_info(anything, S)),
     ?_assertEqual({ok, S}, code_change(old, S, extra)),
     ?_assertEqual(ok, terminate(normal, S)),
     ?_assertEqual(ok, terminate(some_reason, S))].
