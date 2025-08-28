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
-include("blizanci_types.hrl").

-behaviour(gen_server).
-include("gen_server.hrl").

%% API
-export([request/5, cancel/1, gateway_exit/2, handle_client_data/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

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
-spec cancel(Proc) -> 'ok'
              when Proc :: servlet_proc().

cancel(no_proc) ->
    ok;
cancel({proc, Pid}) ->
    case is_process_alive(Pid) of
        true -> gen_server:cast(Pid, servlet_quit);
        _ -> ok
    end.


% Called by the CGI runner
%-spec gateway_exit(pid(), gateway_result()) -> 'ok'.
-spec gateway_exit(Pid, Result) -> 'ok'
              when Pid    :: pid(),
                   Result :: gateway_result().

gateway_exit(Pid, Result) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true -> gen_server:call(Pid, {gateway_result, Result});
        _ -> ok
    end.


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
        {gateway_error, gateway_busy} ->
            exit({shutdown, {gateway_complete, self(), {error_code,
                                                        gateway_busy}}});
        {gateway_error, Error} ->
            %% TBD: this should be factored into a utility fn in the container
            exit({shutdown, {gateway_init_error, self(), Error}});
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
terminate(_Reason, _State) ->
    %lager:info("servlet ~p terminating because: [[~p]]", [self(), Reason]),
    ok.


%% @doc
%% @hidden
%% @end
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().
%% @doc
%% @hidden
%% @end
format_status(_Opt, Status) ->
    Status.

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
        _E1:_E2 -> %lager:info("unmatched error in servlet: ~p", [{E1, E2}]),
                 Response = {error_code, internal_server_error},
                 exit({shutdown, {gateway_complete, self(), Response}})
    end.


-spec report_result(ParentPid, Result) -> 'ok'
              when ParentPid :: pid(),
                   Result    :: servlet_result().

report_result(Parent, Result) ->
    ok = check_result(Result),
    ServletResult = Result,
    ok = blizanci_connection:servlet_result(Parent, ServletResult).


-spec check_result(Result) -> 'ok'
              when Result :: servlet_result().

check_result({servlet_failed, Atom}) when is_atom(Atom) -> ok;
check_result({servlet_complete, Output}) when is_binary(Output) -> ok.
