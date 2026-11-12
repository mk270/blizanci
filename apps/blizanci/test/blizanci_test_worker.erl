%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2026  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

%% A stub pool worker for tests of blizanci_pool: a gen_server which stays
%% alive until it is cast 'quit' (or its pool stops it), and which tells
%% its "parent" (the process that enqueued the job) when it has started.
%% Started the way blizanci_pool:enqueue/2 starts a worker: start_link/1
%% is given {Parent, Job}.

-module(blizanci_test_worker).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-spec start_link(Args) -> Result
              when Args   :: {pid(), term()},
                   Result :: gen_server:start_ret().
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init({Parent, Job}) ->
    Parent ! {worker_started, self(), Job},
    {ok, Job}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(quit, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.
