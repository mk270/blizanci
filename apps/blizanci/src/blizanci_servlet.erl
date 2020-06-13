%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.
-module(blizanci_servlet).
-include("blizanci_types.hrl").

-behaviour(gen_server).

%% API
-export([start_link/3, cancel/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(servlet_state, {parent, url, request, config, cgi_status}).

%%%===================================================================
%%% API
%%%===================================================================


-spec cancel(cgi_proc()) -> 'ok'.
cancel(no_proc) ->
    ok;
cancel({proc, Pid}) ->
    Pid ! quit,
    ok.


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
        {error_code, Error} -> exit({cgi_exec_failed, Error});
        {init_cgi, Pid, OsPid} ->
            State = #servlet_state{parent=Parent,
                           url=URL,
                           request=Req,
                           config=Config,
                           cgi_status={Pid, OsPid, <<>>}},
            gen_server:enter_loop(?MODULE, [], State)
    end.

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

handle_info({'DOWN', OsPid, process, Pid, Status}, State) ->
    {ExpectedPid, ExpectedOsPid, Buffer} = State#servlet_state.cgi_status,
    ExpectedPid = Pid,
    ExpectedOsPid = OsPid,
    %ExitStatus = exec:status(Status),
    NewState = State#servlet_state{cgi_status=no_proc},
    case Status of
        normal ->
            myexit({cgi_output, Buffer}, NewState);
        {exit_status, St} ->
            RV = exec:status(St),
            lager:info("cgi process ended with non-zero status: ~p", [RV]),
            myexit({error_code, cgi_exec_error}, NewState);
        St ->
            lager:info("cgi process terminated anomalously: ~p", [St]),
            myexit({error_code, cgi_exec_error}, NewState)
    end;

handle_info({stdout, OsPid, Msg}, State) ->
    {ExpectedPid, ExpectedOsPid, Buffer} = State#servlet_state.cgi_status,
    ExpectedOsPid = OsPid,
    NewBuffer = erlang:iolist_to_binary([Buffer, Msg]),
    NewState = State#servlet_state{
                 cgi_status={ExpectedPid, ExpectedOsPid, NewBuffer}},
    {noreply, NewState};

handle_info(quit, State) ->
    {_Pid, OsPid, _} = State#servlet_state.cgi_status,
    exec:kill(OsPid, 9),
    {stop, normal, State};

handle_info(Info, State) ->
    lager:info("Servlet message: ~p", [Info]),
    {noreply, State}.


myexit(Reason, State=#servlet_state{parent=Parent}) ->
    Parent ! {cgi_exit, Reason},
    {stop, normal, State}.

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
