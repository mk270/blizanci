%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_servlet_container).

-moduledoc """
The process side of the servlet contract (see blizanci_servlet): runs
a servlet's request/4 and, if it defers, a container process for its
serve/4.

See the manual chapter [Servlets](servlets.md).
""".
-include("blizanci_types.hrl").
-include("blizanci_servlet_container.hrl").

-behaviour(gen_server).

%% API
-export([request/5, cancel/1, gateway_exit/2, handle_client_data/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

% How long defer_request/5 waits for the servlet process to start and
% acknowledge, in milliseconds; failing that the request gets a 40.
-define(START_TIMEOUT_MS, 2000).


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


% Called by the servlet's worker (the CGI runner, say), with the pid of
% the container, to report how it ended. This is a cast: the container's
% reply was always 'ok' and no caller used it, and a cast cannot fail or
% block if the container has already gone (no check-then-act race, no
% timeout).
-spec gateway_exit(Pid, GWResult) -> Result
              when Pid      :: pid(),
                   GWResult :: gateway_result(),
                   Result   :: 'ok'.

gateway_exit(Pid, Result) when is_pid(Pid) ->
    gen_server:cast(Pid, {gateway_result, Result}).


% Called by the connection, in its own process, with the client input
% that follows the request line once a servlet process exists (e.g., a
% Titan upload body). It is a call to the container, in whose process
% Module:handle_client_data(WorkerPid, Payload) runs; the reply is that
% function's gemini_response(), where 'none' means more is awaited. If
% that function raises, the container exits with an internal error
% instead. A container that has already gone gives an
% internal-server-error response (status 40), not 'ok'.
-spec handle_client_data(Pid, Payload) -> Result
              when Pid     :: pid(),
                   Payload :: binary(),
                   Result  :: gemini_response().

handle_client_data(Pid, Payload) when is_binary(Payload) ->
    try gen_server:call(Pid, {client_data, Payload})
    catch
        exit:{noproc, _} -> {error_code, internal_server_error}
    end.


-doc """
Called by the router, in the connection process, to dispatch a
request to a specific handler.

- `Module`: the module implementing the handler
- `Matches`: a map of binary keys to binary values: what each named
  group of the route's pattern captured (see blizanci_servlet)
- `Request`: the request
- `ServerConfig`: the configuration for the Gemini server
- `RouteOpts`: the configuration to be passed to the servlet module

The function calls Module:request(Matches, Request, ServerConfig,
RouteOpts), which may return either `{immediate, Response}` or the atom
`defer`. Where it is indicated that the response will be deferred, a
new container process is started, and in it Module:serve(Matches, Req,
ServerConfig, RouteOpts) is called. This sets things up
for the results of the request to be notified back to the caller
(of request/5), which is a ranch handler for the Gemini protocol.
The result here is then `{init_servlet, Pid}` (or an
internal-server-error code if the process did not start).
""".
-spec request(Module, Matches, Request, ServerConfig, RouteOpts) -> Result
              when Module       :: module(),
                   Matches      :: path_matches(),
                   Request      :: request_details(),
                   ServerConfig :: server_config(),
                   RouteOpts    :: map(),
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
                   RouteOpts    :: map(),
                   Result       :: gemini_response().

defer_request(Module, Matches, Req, ServerConfig, RouteOpts) ->
    Args = [self(), Module, Matches, Req, ServerConfig, RouteOpts],
    %% No timeout on completion: the connection's fixed deadline covers it
    %% (see TIMEOUT_MS in blizanci_connection).
    case proc_lib:start_link(?MODULE, init, [Args], ?START_TIMEOUT_MS) of
        {ok, Pid} -> {init_servlet, Pid};
        {error, _} -> {error_code, internal_server_error}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-doc false.
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

-doc false.
handle_call({client_data, Payload}, _From, State) ->
    do_handle_payload(Payload, State);

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


-doc false.
handle_cast({gateway_result, Result}, State) ->
    do_handle_gateway_result(Result, State);

handle_cast(servlet_quit, State=#servlet_state{gateway_module=Module}) ->
    Pid = State#servlet_state.gateway_pid,
    Module:cancel(Pid),
    {stop, normal, State};

handle_cast(_Request, State) ->
    {noreply, State}.


-doc false.
handle_info(_Info, State) ->
    {noreply, State}.


-doc false.
terminate(normal, _State) ->
    ok;
terminate(Reason, _State) ->
    logger:info("servlet ~p terminating because: [[~p]]", [self(), Reason]),
    ok.


-doc false.
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
    {noreply, State}.


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
