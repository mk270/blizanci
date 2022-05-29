%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_servlet_container).

% This module provides a mechanism for dynamic content generation for
% Gemini requests. Currently it is somewhat hardwired to use CGI to
% fulfil these.
%
% One blizanci_servlet_container process is generated per
% request. This is done by calling blizanci_servlet_container:request/4,
% which has the side-effect of caching the caller's Pid and passing it to
% the servlet process it creates.
%
% On completion of the request, which may well not be synchronous, results
% may be communicated back to the Gemini parent (the original caller) by
% sending a message in the following form:
%
%    {servlet_failed, ErrorCode :: atom()}
%  | {servlet_complete, Output :: binary()}


-include("blizanci_types.hrl").

-behaviour(gen_server).
-include("gen_server.hrl").

%% API
-export([request/4, cancel/1, gateway_exit/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(servlet_state, {parent,
                        url,
                        request,
                        config,
                        gateway_pid,
                        gateway_module}).

%%%===================================================================
%%% API
%%%===================================================================


% Called by the gemini protocol server during shutdown of a connection
-spec cancel(servlet_proc()) -> 'ok'.
cancel(no_proc) ->
    ok;
cancel({proc, Pid}) ->
    case is_process_alive(Pid) of
        true -> gen_server:cast(Pid, servlet_quit);
        _ -> ok
    end.


% Called by the CGI runner
-spec gateway_exit(pid(), gateway_result()) -> 'ok'.
gateway_exit(Pid, Result) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true -> gen_server:call(Pid, {gateway_result, Result});
        _ -> ok
    end.


-spec request(module(), path_matches(), any(), any()) -> gemini_response().
request(Module, Matches, Req, Config) ->
    Parent = self(),
    case Module:request(Matches, Req, Config) of
        {immediate, Result} -> Result;
        defer ->
            case proc_lib:start_link(?MODULE, init, [[Parent, Module,
                                                      Matches, Req, Config]]) of
                {ok, Pid} -> {init_servlet, Pid};
                {error, _} -> {error_code, internal_server_error}
            end
    end.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @doc
%% @hidden
%% @end
init([Parent, Module, URL, Req, Config]) ->
    proc_lib:init_ack({ok, self()}),
    case Module:serve(URL, Req, Config) of
        {gateway_error, Error} ->
            report_result(Parent, {servlet_failed, Error}),
            {stop, normal};
        {gateway_started, Pid} ->
            State = #servlet_state{parent=Parent,
                           url=URL,
                           request=Req,
                           config=Config,
                           gateway_pid=Pid,
                           gateway_module=Module},
            gen_server:enter_loop(?MODULE, [], State)
    end.

%% @doc
%% @hidden
%% @end
handle_call({gateway_result, Result}, _From,
            State=#servlet_state{parent=Parent}) ->
    ServletResult = case Result of
                        {gateway_output, Output} -> {servlet_complete, Output};
                        {gateway_error, Error} -> {servlet_failed, Error}
                    end,
    report_result(Parent, ServletResult),
    {reply, ok, State};

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
handle_info(Info, State) ->
    lager:info("Servlet message: ~p", [Info]),
    {noreply, State}.


%% @doc
%% @hidden
%% @end
terminate(normal, _State) ->
    ok;
terminate(Reason, _State) ->
    lager:info("servlet ~p terminating because: [[~p]]", [self(), Reason]),
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

-spec report_result(pid(), servlet_result()) -> 'ok'.
report_result(Parent, Result) ->
    ServletResult = Result,
    ok = blizanci_gemini:servlet_result(Parent, ServletResult).
