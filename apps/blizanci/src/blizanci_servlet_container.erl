%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

%% @doc
% This module provides a mechanism for dynamic content generation for
% Gemini requests.
%
% One blizanci_servlet_container process is generated per deferred
% request. This is done by calling blizanci_servlet_container:request/5,
% which has the side-effect of caching the caller's Pid and passing it to
% the servlet process it creates.
%
% On completion of the request, which may well not be synchronous, results
% may be communicated back to the Gemini parent (the original caller) by
% sending a message in the following form:
%
%    {servlet_failed, ErrorCode :: atom()}
%  | {servlet_complete, Output :: binary()}
%
% It is likely unnecessary for a handler module to provide substantive
% implementations of both Module:serve/4 and Module:request/4. This is
% because a handler module will likely behave wholly synchronously or
% wholly asychronously.
%
% A synchrononous handler module should have a substantive implementation
% of Module:request/4 which returns a gemini response marked with the
% atom 'immediate'; this means its implementation of Module:serve/4 will
% never be called and may be left as a stub. Conversely, an asychronous
% handler module would have a trivial implementation of Module:request/4,
% returning the atom 'defer', and the substantive implementation would be
% afforded via its Module:serve/4 implementation instead.
%
%% @end

-module(blizanci_servlet_container).
-include_lib("eunit/include/eunit.hrl").
-include("blizanci_types.hrl").

-behaviour(gen_server).

%% API
-export([request/5, cancel/1, gateway_exit/2, handle_client_data/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(servlet_state, {parent,
                        matches,
                        request,
                        server_config,
                        route_options,
                        gateway_pid,
                        gateway_module}).

-type servlet_state() :: #servlet_state{}.

%%%===================================================================
%%% API
%%%===================================================================


% Called by the gemini protocol server during shutdown of a connection
-spec cancel(Proc) -> Result
              when Proc   :: servlet_proc(),
                   Result :: 'ok'.

cancel(no_proc) ->
    ok;
cancel({proc, Pid}) ->
    case is_process_alive(Pid) of
        true -> gen_server:cast(Pid, servlet_quit);
        _ -> ok
    end.


% Called by the CGI runner
%-spec gateway_exit(pid(), gateway_result()) -> 'ok'.
-spec gateway_exit(Pid, GWResult) -> Result
              when Pid      :: pid(),
                   GWResult :: gateway_result(),
                   Result   :: 'ok'.

gateway_exit(Pid, Result) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true -> gen_server:call(Pid, {gateway_result, Result});
        _ -> ok
    end.


% TODO: document what on earth is going on here
-spec handle_client_data(Pid, Payload) -> Result
              when Pid     :: pid(),
                   Payload :: binary(),
                   Result  :: 'ok' | gemini_response().

handle_client_data(Pid, Payload) when is_binary(Payload) ->
    case is_process_alive(Pid) of
        true -> actually_handle_client_data(Pid, Payload);
        _ -> ok % this seems unprincipled
    end.


-spec actually_handle_client_data(Pid, Payload) -> Result
              when Pid     :: pid(),
                   Payload :: binary(),
                   Result  :: gemini_response().

actually_handle_client_data(Pid, Payload) ->
    Result = gen_server:call(Pid, {client_data, Payload}),
    Result.


%% @doc Called by the router to dispatch a request to a specific handler.
%% @param Module the module implementing the handler
%% @param Matches a list of binary key-value pairs representing the manner in which the request matched the pattern associated with the route
%% @param Request the request
%% @param ServerConfig the configuration for the Gemini server
%% @param RouteOpts the configuration to be passed to the servlet module
%%
%% The function calls Module:request(Matches, Request, Config), which may
%% return either an immediate Gemini response or the atom 'defer'. Where it
%% is indicated that the response will be deferred, a new process is started
%% and Module:serve(Matches, Req, Config) is called. This sets things up
%% for the results of the request to be notified back to the caller
%% (of request/5), which is a ranch handler for the Gemini protocol.
%% @end
-spec request(Module, Matches, Request, ServerConfig, RouteOpts) -> Result
              when Module       :: module(),
                   Matches      :: path_matches(),
                   Request      :: request_details(),
                   ServerConfig :: server_config(),
                   RouteOpts    :: any(),
                   Result       :: gemini_response().

request(Module, Matches, Request, ServerConfig, RouteOpts) ->
    case Module:request(Matches, Request, ServerConfig, RouteOpts) of
        {immediate, Result} -> Result;
        defer -> defer_request(Module, Matches, Request,
                               ServerConfig, RouteOpts)
    end.


-spec defer_request(Module, Matches, Req, ServerConfig, RouteOpts) -> Result
              when Module       :: module(),
                   Matches      :: path_matches(),
                   Req          :: request_details(),
                   ServerConfig :: server_config(),
                   RouteOpts    :: any(),
                   Result       :: gemini_response().

defer_request(Module, Matches, Req, ServerConfig, RouteOpts) ->
    Args = [self(), Module, Matches, Req, ServerConfig, RouteOpts],
    process_flag(trap_exit, true),
    %% TBD: there should also be a timeout not on startup but on completion
    %%      though the protocol handler timeout might make this redundant
    Timeout = 2000, % i.e., two seconds
    case proc_lib:start_link(?MODULE, init, [Args], Timeout) of
        {ok, Pid} -> {init_servlet, Pid};
        {error, _} -> {error_code, internal_server_error}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @doc
%% @hidden
%% @end
init([Parent, Module, Matches, Req, ServerConfig, RouteOpts]) ->
    proc_lib:init_ack({ok, self()}),
    case Module:serve(Matches, Req, ServerConfig, RouteOpts) of
        {gateway_finished, Response} ->
            exit({shutdown, {gateway_complete, self(), Response}});
        {gateway_error, Error} ->
            %% A reason the status table maps to a failure status is the
            %% client's answer (file_not_found gives 51, gateway_busy 40);
            %% anything else is an internal failure, logged by the parent.
            %% TBD: this should be factored into a utility fn in the container
            case blizanci_status:is_error(Error) of
                true ->
                    exit({shutdown, {gateway_complete, self(),
                                     {error_code, Error}}});
                false ->
                    exit({shutdown, {gateway_init_error, self(), Error}})
            end;
        {gateway_started, Pid} ->
            State = #servlet_state{parent=Parent,
                           matches=Matches,
                           request=Req,
                           server_config=ServerConfig,
                           route_options=RouteOpts,
                           gateway_pid=Pid,
                           gateway_module=Module},
            gen_server:enter_loop(?MODULE, [], State)
    end.

%% @doc
%% @hidden
%% @end
handle_call({gateway_result, Result}, _From, State) ->
    do_handle_gateway_result(Result, State);

handle_call({client_data, Payload}, _From, State) ->
    do_handle_payload(Payload, State);

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% @doc
%% @hidden
%% @end
handle_cast(servlet_quit, State=#servlet_state{gateway_module=Module}) ->
    Pid = State#servlet_state.gateway_pid,
    Module:cancel(Pid),
    {stop, normal, State};

handle_cast(_Request, State) ->
    {noreply, State}.


%% @doc
%% @hidden
%% @end
handle_info(_Info, State) ->
    {noreply, State}.


%% @doc
%% @hidden
%% @end
terminate(normal, _State) ->
    ok;
terminate(Reason, _State) ->
    logger:info("servlet ~p terminating because: [[~p]]", [self(), Reason]),
    ok.


%% @doc
%% @hidden
%% @end
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal
%%%===================================================================

-spec do_handle_gateway_result(GWResult, State) -> Result
              when GWResult :: gateway_result(),
                   State    :: servlet_state(),
                   Result   :: any().

do_handle_gateway_result(GWResult, State=#servlet_state{parent=Parent}) ->
    ServletResult = case GWResult of
                        {gateway_output, Output} -> {servlet_complete, Output};
                        {gateway_error, Error} -> {servlet_failed, Error}
                    end,
    report_result(Parent, ServletResult),
    {reply, ok, State}.


-spec do_handle_payload(Payload, State) -> Result
              when Payload :: binary(),
                   State   :: servlet_state(),
                   Result  :: any().

do_handle_payload(Payload, State=#servlet_state{
                                   gateway_module=Module,
                                   gateway_pid=Pid
                                   }) ->
    try Module:handle_client_data(Pid, Payload) of
        Reply -> {reply, Reply, State}
    catch
        E1:E2 -> logger:info("unmatched error in servlet: ~p", [{E1, E2}]),
                 Response = {error_code, internal_server_error},
                 exit({shutdown, {gateway_complete, self(), Response}})
    end.


-spec report_result(ParentPid, ServletResult) -> Result
              when ParentPid     :: pid(),
                   ServletResult :: servlet_result(),
                   Result        :: 'ok'.

report_result(Parent, Result) ->
    ok = check_result(Result),
    ServletResult = Result,
    ok = blizanci_connection:servlet_result(Parent, ServletResult).


-spec check_result(ServResult) -> Result
              when ServResult :: servlet_result(),
                   Result     :: 'ok'.

check_result({servlet_failed, Atom}) when is_atom(Atom) -> ok;
check_result({servlet_complete, Output}) when is_binary(Output) -> ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% A servlet that refuses a request before its worker starts returns
%% {gateway_error, Reason} from serve/4. A Reason the status table maps to
%% a failure status becomes the client's answer ({error_code, Reason});
%% any other Reason stays an internal failure. Only serve/4 refusals that
%% need no worker pool are used here (CGI refuses before queueing, and
%% blizanci_static:serve/4 is a stub that returns 'unimplemented').
%% Fixture, under a fresh temporary directory:
%%   Base/cgi/{script, .hidden}
init_refusal_test_() ->
    {setup, fun refusal_setup/0, fun refusal_teardown/1,
     fun refusal_tests/1}.

refusal_setup() ->
    Unique = integer_to_list(erlang:unique_integer([positive])),
    Base = filename:absname("container_refusal_test_" ++ Unique),
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
    {ok, Pid} = proc_lib:start_link(?MODULE, init,
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
    Opts = #{ cgiroot => Dir, cgiprefix => "/cgi-bin/" },
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
     % a reason with no status mapping is still an internal failure
     ?_assertEqual({gateway_init_error, unimplemented},
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
     ?_assertEqual({gateway_init_error, unimplemented},
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
%% the test process plays the connection, answering the container's
%% gen_server call.
gateway_exit_result(Result) ->
    with_container(fun(_) -> none end,
        fun(Pid, _Worker) ->
            Me = self(),
            spawn(fun() -> Me ! {gw_reply, gateway_exit(Pid, Result)} end),
            Got = receive
                      {'$gen_call', From, {servlet_result, SR}} ->
                          gen_server:reply(From, ok),
                          SR
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
     ?_assertEqual({{servlet_failed, unimplemented}, ok, true},
                   gateway_exit_result({gateway_error, unimplemented})),
     % a container that is already gone is not an error
     ?_assertEqual(ok, gateway_exit(dead_pid(), {gateway_output, <<>>}))].

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
     % PINS CURRENT BEHAVIOUR (smell F5), not desired: a container that has
     % already gone yields 'ok', which the connection cannot respond to
     ?_assertEqual(ok, handle_client_data(dead_pid(), <<"x">>))].

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
