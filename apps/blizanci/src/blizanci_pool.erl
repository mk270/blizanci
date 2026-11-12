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
-include_lib("eunit/include/eunit.hrl").
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Each test runs in a process of its own, with a pool of its own under
%% a throwaway name, which is stopped afterwards whether or not the test
%% passes. The CGI and Titan pools are never named.
with_pool_name(Fun) ->
    blizanci_test_support:isolated(
      fun() ->
          {ok, _} = application:ensure_all_started(ppool),
          Q = list_to_atom("blizanci_pool_test_"
                           ++ integer_to_list(
                                erlang:unique_integer([positive]))),
          Others = {whereis(blizanci_cgi), whereis(blizanci_titan)},
          try Fun(Q)
          after
              ok = stop(Q),
              % ours is gone, and the real ones were not disturbed
              {error, _} = ppool:stop_pool(Q),
              Others = {whereis(blizanci_cgi), whereis(blizanci_titan)}
          end
      end).

pool_running(Q) ->
    try ppool:status(Q) of
        _ -> true
    catch _:_ -> false
    end.

await_down(Pid) ->
    Ref = monitor(process, Pid),
    receive {'DOWN', Ref, process, Pid, _} -> ok
    after 2000 -> timeout
    end.

start_test() ->
    with_pool_name(
      fun(Q) ->
          ?assertEqual(false, pool_running(Q)),
          ?assertEqual(ok, start(Q, 2, blizanci_test_worker)),
          ?assertEqual(true, pool_running(Q))
      end).

%% Starting a pool that is already there is fine, and leaves it (and
%% its workers) alone, whatever Max or Module the second call gives.
start_twice_test() ->
    with_pool_name(
      fun(Q) ->
          ok = start(Q, 1, blizanci_test_worker),
          {gateway_started, W} = enqueue(Q, job),
          ?assertEqual(ok, start(Q, 1, blizanci_test_worker)),
          ?assertEqual(ok, start(Q, 9, no_such_module)),
          ?assert(is_process_alive(W)),
          % still the one-worker pool: the second job is not taken
          ?assertEqual({gateway_error, gateway_busy}, enqueue(Q, job2))
      end).

stop_test() ->
    with_pool_name(
      fun(Q) ->
          ok = start(Q, 2, blizanci_test_worker),
          {gateway_started, W} = enqueue(Q, job),
          ?assertEqual(ok, stop(Q)),
          ?assertEqual(false, pool_running(Q)),
          ?assertEqual(ok, await_down(W))
      end).

%% Stopping a pool that is not there (or has been stopped) is fine.
stop_missing_test() ->
    with_pool_name(
      fun(Q) ->
          ?assertEqual(ok, stop(Q)),
          ok = start(Q, 1, blizanci_test_worker),
          ?assertEqual(ok, stop(Q)),
          ?assertEqual(ok, stop(Q))
      end).

%% A pool can be started again after being stopped.
restart_test() ->
    with_pool_name(
      fun(Q) ->
          ok = start(Q, 1, blizanci_test_worker),
          ok = stop(Q),
          ?assertEqual(ok, start(Q, 1, blizanci_test_worker)),
          ?assertMatch({gateway_started, _}, enqueue(Q, job))
      end).

%% Cancelling a worker that is already dead does nothing.
cancel_dead_test() ->
    Pid = spawn(fun() -> ok end),
    ?assertEqual(ok, await_down(Pid)),
    ?assertEqual(ok, cancel(Pid, quit)).

%% Cancelling a live worker casts it the message: 'quit' ends it, and
%% frees its slot in the pool; anything else it ignores.
cancel_live_test() ->
    with_pool_name(
      fun(Q) ->
          ok = start(Q, 1, blizanci_test_worker),
          {gateway_started, W} = enqueue(Q, job),
          ?assertEqual({gateway_error, gateway_busy}, enqueue(Q, job2)),
          ?assertEqual(ok, cancel(W, ignored)),
          ?assertEqual(timeout, await_down_within(W, 100)),
          ?assertEqual(ok, cancel(W, quit)),
          ?assertEqual(ok, await_down(W)),
          ?assertEqual(ok, cancel(W, quit)),
          ?assertMatch({gateway_started, _}, enqueue_when_free(Q))
      end).

await_down_within(Pid, Ms) ->
    Ref = monitor(process, Pid),
    receive {'DOWN', Ref, process, Pid, _} -> ok
    after Ms -> demonitor(Ref, [flush]), timeout
    end.

%% The pool notices a worker's exit asynchronously.
enqueue_when_free(Q) ->
    enqueue_when_free(Q, 50).

enqueue_when_free(Q, 0) ->
    enqueue(Q, again);
enqueue_when_free(Q, N) ->
    case enqueue(Q, again) of
        {gateway_error, gateway_busy} ->
            timer:sleep(20),
            enqueue_when_free(Q, N - 1);
        Other -> Other
    end.

%% A job is run by a new worker, started with the caller as its parent
%% and the job; a full pool says it is busy rather than queueing.
enqueue_test() ->
    with_pool_name(
      fun(Q) ->
          ok = start(Q, 2, blizanci_test_worker),
          {gateway_started, W1} = enqueue(Q, {job, 1}),
          receive {worker_started, W1, {job, 1}} -> ok
          after 1000 -> error(no_worker_report)
          end,
          ?assert(is_process_alive(W1)),
          {gateway_started, W2} = enqueue(Q, {job, 2}),
          ?assertNotEqual(W1, W2),
          ?assertEqual({gateway_error, gateway_busy}, enqueue(Q, {job, 3})),
          ?assert(is_process_alive(W1)),
          ?assert(is_process_alive(W2))
      end).

%% Without such a pool, enqueueing crashes: PINS CURRENT BEHAVIOUR
%% (callers only enqueue after the pool has been started).
enqueue_no_pool_test() ->
    with_pool_name(
      fun(Q) ->
          ?assertExit(_, enqueue(Q, job))
      end).
