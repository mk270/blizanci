%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_pool).

-moduledoc """
Shared `ppool` pool lifecycle helpers for the servlets that queue
work onto a worker pool (`blizanci_cgi`, `blizanci_titan`).
""".
-include_lib("eunit/include/eunit.hrl").
-include("blizanci_types.hrl").

-export([start/3, stop/1, cancel/2, enqueue/2]).

-doc """
Start the named pool, of at most Max workers of Module. A pool that is
already running under the name is left alone and is `ok`.

The arguments are checked here, before ppool is called, in the order
name, limit, module: Queue must be an atom (other than `undefined`),
Max an integer of at least 1, and Module an atom. A bad one gives
`{error, {invalid_name, Queue}}`, `{error, {invalid_limit, Max}}` or
`{error, {invalid_module, Module}}` (ppool's own error shapes), and
nothing is started or touched. This is deliberate defence in depth:
the spec below is not enforced at run time, and the limits may one
day come from configuration rather than being constants.

Any other refusal by ppool (or a name held by some other registered
process) is returned as `{error, Reason}`, Reason being ppool's,
unchanged.
""".
-spec start(Queue, Max, Module) -> Result
              when Queue  :: atom(),
                   Max    :: pos_integer(),
                   Module :: module(),
                   Result :: 'ok' | {'error', term()}.
start(Queue, _Max, _Module)
  when not is_atom(Queue); Queue =:= undefined ->
    {error, {invalid_name, Queue}};
start(_Queue, Max, _Module)
  when not is_integer(Max); Max < 1 ->
    {error, {invalid_limit, Max}};
start(_Queue, _Max, Module)
  when not is_atom(Module) ->
    {error, {invalid_module, Module}};
start(Queue, Max, Module) ->
    % through apply/3: ppool's spec is narrower than what we tolerate
    case apply(ppool, start_pool, [Queue, Max, {Module, start_link, []}]) of
        {error, {already_started, _Pid}} -> ok;
        {ok, _Pid} -> ok;
        {error, _} = Error -> Error;
        Other -> {error, {unexpected, Other}}
    end.


-spec stop(Queue) -> Result
              when Queue  :: atom(),
                   Result :: 'ok'.
stop(Queue) ->
    case ppool:stop_pool(Queue) of
        ok -> ok;
        {error, _} -> ok
    end.


-doc """
Cancel a queued/running worker, e.g. because the client connection
has gone away. QuitMsg is the gen_cast message the worker's own
handle_cast/2 expects, to shut itself down cleanly.
""".
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

%% Bad arguments are refused by start/3 itself, before ppool is called,
%% in the order name, limit, module; no pool is left running.
start_invalid_limit_test() ->
    with_pool_name(
      fun(Q) ->
          F = fun(Max) -> apply(?MODULE, start,
                                [Q, Max, blizanci_test_worker]) end,
          lists:foreach(
            fun(Max) ->
                ?assertEqual({error, {invalid_limit, Max}}, F(Max)),
                ?assertEqual(false, pool_running(Q))
            end, [0, -1, 1.5, "5", undefined, many])
      end).

start_invalid_name_test() ->
    lists:foreach(
      fun(Name) ->
          ?assertEqual({error, {invalid_name, Name}},
                       apply(?MODULE, start,
                             [Name, 1, blizanci_test_worker]))
      end, ["x", 5, undefined]).

start_invalid_module_test() ->
    with_pool_name(
      fun(Q) ->
          lists:foreach(
            fun(Mod) ->
                ?assertEqual({error, {invalid_module, Mod}},
                             apply(?MODULE, start, [Q, 1, Mod])),
                ?assertEqual(false, pool_running(Q))
            end, ["m", 5])
      end).

%% The checks run in the order name, limit, module.
start_invalid_order_test() ->
    ?assertEqual({error, {invalid_name, "x"}},
                 apply(?MODULE, start, ["x", 0, "m"])),
    ?assertEqual({error, {invalid_limit, 0}},
                 apply(?MODULE, start, [some_pool, 0, "m"])),
    ?assertEqual(false, pool_running(some_pool)).

%% A bad argument is refused even when the pool is already running, and
%% the running pool is left alone.
start_invalid_when_running_test() ->
    with_pool_name(
      fun(Q) ->
          ok = start(Q, 1, blizanci_test_worker),
          ?assertEqual({error, {invalid_limit, 0}},
                       apply(?MODULE, start,
                             [Q, 0, blizanci_test_worker])),
          ?assertEqual({error, {invalid_module, "m"}},
                       apply(?MODULE, start, [Q, 1, "m"])),
          ?assertEqual(true, pool_running(Q))
      end).

%% A name held by some other registered process is not an existing
%% pool: ppool's (nested) error comes back, not a crash.
start_name_taken_test() ->
    with_pool_name(
      fun(Q) ->
          Holder = spawn_link(fun() -> receive stop -> ok end end),
          true = register(Q, Holder),
          ?assertMatch({error, _}, start(Q, 1, blizanci_test_worker)),
          ?assertEqual(Holder, whereis(Q)),
          unlink(Holder),
          Holder ! stop,
          ok = await_down(Holder)
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
