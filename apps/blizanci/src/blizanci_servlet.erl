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

-record(servlet_state, {parent, url, request, config, cgi_pid}).

%%%===================================================================
%%% API
%%%===================================================================


-spec cancel(cgi_proc()) -> 'ok'.
cancel(no_proc) ->
    ok;
cancel({proc, Pid}) ->
    case is_process_alive(Pid) of
        true -> gen_server:call(Pid, quit);
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
            Parent ! {cgi_exec_failed, Error},
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
handle_info({cgi_exit, Msg}, State=#servlet_state{parent=Parent}) ->
    Parent ! {cgi_exit, Msg},
    {noreply, State};

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
