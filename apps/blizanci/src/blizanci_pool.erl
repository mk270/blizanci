%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

%% @doc
%% Shared `ppool' pool lifecycle helpers for the servlets that queue
%% work onto a worker pool (`blizanci_cgi', `blizanci_titan').
%% @end

-module(blizanci_pool).
-include("blizanci_types.hrl").

-export([start/3, stop/1, cancel/2, enqueue/2]).

-spec start(Queue, Max, Module) -> Result
              when Queue  :: atom(),
                   Max    :: pos_integer(),
                   Module :: module(),
                   Result :: 'ok'.
start(Queue, Max, Module) ->
    case ppool:start_pool(Queue, Max, {Module, start_link, []}) of
        {error, {already_started, _Pid}} -> ok;
        {ok, _Pid} -> ok
    end.


-spec stop(Queue) -> Result
              when Queue  :: atom(),
                   Result :: 'ok'.
stop(Queue) ->
    case ppool:stop_pool(Queue) of
        ok -> ok;
        {error, _} -> ok
    end.


%% @doc
%% Cancel a queued/running worker, e.g. because the client connection
%% has gone away. QuitMsg is the gen_cast message the worker's own
%% handle_cast/2 expects, to shut itself down cleanly.
%% @end
-spec cancel(Pid, QuitMsg) -> Result
              when Pid     :: pid(),
                   QuitMsg :: atom(),
                   Result  :: 'ok'.
cancel(Pid, QuitMsg) ->
    case is_process_alive(Pid) of
        true -> gen_server:cast(Pid, QuitMsg);
        _ -> ok
    end.


-spec enqueue(Queue, Job) -> Result
              when Queue  :: atom(),
                   Job    :: term(),
                   Result :: gateway_result().
enqueue(Queue, Job) ->
    case ppool:run(Queue, [{self(), Job}]) of
        {ok, Pid} -> {gateway_started, Pid};
        noalloc -> {gateway_error, gateway_busy}
    end.
