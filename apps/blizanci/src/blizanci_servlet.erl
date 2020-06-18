%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_servlet).

% This module provides a mechanism for dynamic content generation for
% Gemini requests. Currently it is somewhat hardwired to use CGI to
% fulfil these.
%
% One blizanci_servlet process is generated per request. This is done by
% calling blizanci_servlet:start_link/3, which has the side-effect of
% caching the caller's Pid and passing it to the servlet process it
% creates.
%
% On completion of the request, which may well not be synchronous, results
% may be communicated back to the Gemini parent (the original caller) by
% sending a message in the following form:
%
%    {servlet_failed, ErrorCode :: atom()}
%  | {servlet_complete, Output :: binary()}


-include("blizanci_types.hrl").

-behaviour(gen_server).

%% API
-export([start_link/3, cancel/1, gateway_exit/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(servlet_state, {parent, url, request, config, cgi_pid}).

%%%===================================================================
%%% API
%%%===================================================================


% Called by the gemini protocol server during shutdown of a connection
-spec cancel(servlet_proc()) -> 'ok'.
cancel(no_proc) ->
    ok;
cancel({proc, Pid}) ->
    case is_process_alive(Pid) of
        true -> gen_server:call(Pid, quit);
        _ -> ok
    end.


% Called by the CGI runner
-spec gateway_exit(pid(), exec_result()) -> 'ok'.
gateway_exit(Pid, Result) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true -> gen_server:call(Pid, {gateway_result, Result});
        _ -> ok
    end.


-spec start_link(any(), any(), any()) -> {ok, Pid :: pid()} |
                      {error, Error :: {already_started, pid()}} |
                      {error, Error :: term()} |
                      ignore.
start_link(URL, Req, Config) ->
    Parent = self(),
    proc_lib:start_link(?MODULE, init, [[Parent, URL, Req, Config]]).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(Args :: term()) -> {ok, State :: term()} |
                              {ok, State :: term(), Timeout :: timeout()} |
                              {ok, State :: term(), hibernate} |
                              {stop, Reason :: term()} |
                              ignore.
init([Parent, URL, Req, Config]) ->
    process_flag(trap_exit, true),
    proc_lib:init_ack({ok, self()}),
    case blizanci_cgi:serve(URL, Req, Config) of
        {error_code, Error} ->
            report_result(Parent, {servlet_failed, Error}),
            {stop, normal};
        {init_cgi, Pid} ->
            State = #servlet_state{parent=Parent,
                           url=URL,
                           request=Req,
                           config=Config,
                           cgi_pid=Pid},
            gen_server:enter_loop(?MODULE, [], State)
    end.

handle_call(quit, _From, State) ->
    Pid = State#servlet_state.cgi_pid,
    blizanci_cgi:cancel(Pid),
    {stop, normal, ok, State};

handle_call({gateway_result, Result}, _From,
            State=#servlet_state{parent=Parent}) ->
    ServletResult = case Result of
                        {cgi_output, Output} -> {servlet_complete, Output};
                        {error_code, Error} -> {servlet_failed, Error}
                    end,
    report_result(Parent, ServletResult),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast(_Request, State) ->
    {noreply, State}.


-spec handle_info(Info :: timeout() | term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(Info, State) ->
    lager:info("Servlet message: ~p", [Info]),
    {noreply, State}.


-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

-spec code_change(OldVsn :: term() | {down, term()},
                  State :: term(),
                  Extra :: term()) -> {ok, NewState :: term()} |
                                      {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal
%%%===================================================================

-spec report_result(pid(), servlet_result()) -> 'ok'.
report_result(Parent, Result) ->
    ServletResult = Result,
    blizanci_gemini:servlet_result(Parent, ServletResult).
