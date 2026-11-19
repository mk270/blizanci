%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_pool).

-moduledoc """
Shared `ppool` pool lifecycle helpers for the servlets that queue
work onto a worker pool (`blizanci_cgi`, `blizanci_titan`).
""".
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
