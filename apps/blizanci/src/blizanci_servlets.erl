%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

%% @hidden
%% Owns the lifecycle of the declared servlets' shared resources. A
%% servlet is a module implementing the blizanci_servlet behaviour; its
%% start/0 sets up whatever the module needs in order to serve requests
%% (for blizanci_cgi and blizanci_titan, a pool of worker processes) and
%% its stop/0 tears that down again.
%%
%% The pools themselves live under ppool's own supervisor, outside our
%% tree; this process is what represents them in it. On start it takes
%% the declared list from the configuration table (blizanci_conf_serv),
%% not from any start argument, so a restart sees whatever
%% blizanci_config:apply/1 last published, and calls start/0 on each,
%% which is harmless for a pool that is already running. On termination
%% it calls stop/0 on the servlets it believes are running. A stop/0
%% that raises or returns something but ok is logged with
%% logger:warning and ignored, here and on apply/1 and after a failed
%% start, so every servlet is stopped and this process never crashes
%% over one. The list of servlets running is
%% kept in its own state, not read from the table, because when the
%% table owner dies the table is gone before this process terminates.
%%
%% blizanci_config:apply/1 comes here (reconfigure/3), so the declared
%% list and the real pools change in one place, one request at a time.
%% It is supervised after the table owner and before the listener.

-module(blizanci_servlets).
-behaviour(gen_server).
-include("blizanci_types.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([start_link/0, reconfigure/3]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-define(CALL_TIMEOUT, 30000).


-spec start_link() -> Result
              when Result :: {'ok', pid()} | {'error', term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% Switch the running configuration to the given one: start the pools
%% of the servlets added, publish, then stop those removed, so a route
%% never names a servlet whose resources aren't set up. If a start
%% fails, those already started are stopped again and nothing is
%% published. The port can't change without a restart.
-spec reconfigure(Port, Servlets, Sites) -> Result
              when Port     :: integer(),
                   Servlets :: [module()],
                   Sites    :: [site()],
                   Result   :: 'ok' | {'error', term()}.
reconfigure(Port, Servlets, Sites) ->
    try gen_server:call(?MODULE, {reconfigure, Port, Servlets, Sites},
                        ?CALL_TIMEOUT)
    catch
        exit:{noproc, _} -> {error, not_running}
    end.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    case blizanci_conf_serv:current() of
        {ok, #{servlets := Servlets}} ->
            case start_servlets(Servlets, []) of
                ok -> {ok, Servlets};
                {error, Reason} -> {stop, Reason}
            end;
        not_running ->
            {stop, no_configuration}
    end.

handle_call({reconfigure, Port, Servlets, Sites}, _From, Old) ->
    case blizanci_conf_serv:current() of
        {ok, #{port := OldPort}} when OldPort =/= Port ->
            {reply, {error, {port_change_requires_restart, OldPort, Port}},
             Old};
        {ok, _} ->
            switch(Port, Servlets, Sites, Old);
        not_running ->
            {reply, {error, not_running}, Old}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, not_supported}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, Servlets) ->
    stop_servlets(Servlets).


%%%===================================================================
%%% Internal
%%%===================================================================

-spec switch(Port, Servlets, Sites, Old) -> Result
              when Port     :: integer(),
                   Servlets :: [module()],
                   Sites    :: [site()],
                   Old      :: [module()],
                   Result   :: {'reply', 'ok' | {'error', term()},
                                [module()]}.
switch(Port, Servlets, Sites, Old) ->
    Added   = [ S || S <- Servlets, not lists:member(S, Old) ],
    Removed = [ S || S <- Old, not lists:member(S, Servlets) ],
    case start_servlets(Added, []) of
        ok ->
            ok = blizanci_conf_serv:publish(Port, Servlets, Sites),
            ok = stop_servlets(Removed),
            {reply, ok, Servlets};
        {error, _} = Error ->
            {reply, Error, Old}
    end.


% Call start/0 on each servlet in turn; if one fails (returns
% {error, Reason}, or raises, or returns anything but ok), call stop/0
% on those already started so a failed start leaves nothing behind.
-spec start_servlets(Servlets, Started) -> Result
              when Servlets :: [module()],
                   Started  :: [module()],
                   Result   :: 'ok' | {'error', term()}.
start_servlets([], _Started) ->
    ok;
start_servlets([Servlet | Rest], Started) ->
    try Servlet:start() of
        ok -> start_servlets(Rest, [Servlet | Started]);
        {error, Reason} -> start_failed(Servlet, Reason, Started);
        Other -> start_failed(Servlet, {unexpected, Other}, Started)
    catch
        Class:Reason -> start_failed(Servlet, {Class, Reason}, Started)
    end.

-spec start_failed(Servlet, Reason, Started) -> Result
              when Servlet :: module(),
                   Reason  :: term(),
                   Started :: [module()],
                   Result  :: {'error', {'servlet_start_failed', module(),
                                         term()}}.
start_failed(Servlet, Reason, Started) ->
    ok = stop_servlets(Started),
    {error, {servlet_start_failed, Servlet, Reason}}.


% Call stop/0 on every servlet given. A servlet whose stop/0 raises or
% returns anything but ok is logged and otherwise ignored, so the rest
% are still stopped and this process does not crash.
-spec stop_servlets(Servlets) -> 'ok'
              when Servlets :: [module()].
stop_servlets(Servlets) ->
    lists:foreach(fun stop_servlet/1, Servlets).

-spec stop_servlet(Servlet) -> 'ok'
              when Servlet :: module().
stop_servlet(Servlet) ->
    try Servlet:stop() of
        ok ->
            ok;
        Other ->
            logger:warning("servlet ~p: stop/0 returned ~p, ignored",
                           [Servlet, Other])
    catch
        Class:Reason ->
            logger:warning("servlet ~p: stop/0 failed: ~p, ignored",
                           [Servlet, {Class, Reason}])
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% These tests use the registered processes, the named table and the
%% blizanci_cgi pool, so each runs in a process of its own and removes
%% them afterwards, whether or not it passes.
in_clean_state(Fun) ->
    blizanci_test_support:isolated(
      fun() ->
          {ok, _} = application:ensure_all_started(ppool),
          blizanci_test_support:live_down(),
          try Fun() after blizanci_test_support:live_down() end
      end).

%% The declared servlets are taken from the table when the process
%% starts, and stopped when it stops, those declared at that moment.
lifecycle_test() ->
    in_clean_state(fun lifecycle/0).

lifecycle() ->
    ?assertEqual({error, not_running}, reconfigure(1965, [], [])),
    ok = blizanci_test_support:live_up(1965, [blizanci_cgi], []),
    ?assertNotEqual(undefined, whereis(blizanci_cgi)),
    ok = gen_server:stop(blizanci_servlets),
    ?assertEqual(undefined, whereis(blizanci_cgi)).

%% A process that finds pools already running (it was killed, so did
%% not stop them) takes them over, and starts any that are missing.
restart_reconciles_test() ->
    in_clean_state(fun restart_reconciles/0).

restart_reconciles() ->
    ok = blizanci_test_support:live_up(1965, [blizanci_cgi], []),
    Pool = whereis(blizanci_cgi),
    unlink_and_kill(blizanci_servlets),
    ?assertEqual(Pool, whereis(blizanci_cgi)),
    {ok, _} = start_link(),
    ?assertEqual(Pool, whereis(blizanci_cgi)),
    unlink_and_kill(blizanci_servlets),
    ok = blizanci_cgi:stop(),
    {ok, _} = start_link(),
    ?assertNotEqual(undefined, whereis(blizanci_cgi)).

%% A servlet whose start/0 fails keeps the process from starting, with
%% the reason; those started before it in the list are stopped again.
init_start_error_test() ->
    in_clean_state(
      fun() ->
          process_flag(trap_exit, true),
          {ok, _} = blizanci_conf_serv:start_link(
                      #{port => 1965,
                        servlets => [blizanci_cgi, blizanci_test_failing],
                        sites => []}),
          ?assertEqual({error, {servlet_start_failed,
                                blizanci_test_failing, boom}},
                       start_link()),
          ?assertEqual(undefined, whereis(blizanci_servlets)),
          ?assertEqual(undefined, whereis(blizanci_cgi))
      end).

%% Without a configuration table there is nothing to start from.
no_table_test() ->
    in_clean_state(
      fun() ->
          process_flag(trap_exit, true),
          ?assertEqual({error, no_configuration}, start_link())
      end).

unlink_and_kill(Name) ->
    Pid = whereis(Name),
    Ref = monitor(process, Pid),
    unlink(Pid),
    exit(Pid, kill),
    receive {'DOWN', Ref, process, Pid, _} -> ok end.

%% A servlet whose stop/0 returns something but ok, or raises, does not
%% keep the others from being stopped, nor crash the process, on apply
%% or at termination.
bad_stop_test() ->
    in_clean_state(fun bad_stop/0).

bad_stop() ->
    Fakes = [blizanci_test_stop_bad, blizanci_test_stop_raise,
             blizanci_test_stop_ok],
    register(blizanci_stop_probe, self()),
    ok = blizanci_test_support:live_up(1965, Fakes, []),
    Pid = whereis(blizanci_servlets),
    ?assertEqual(ok, reconfigure(1965, [], [])),
    ?assertEqual(Fakes, stopped([])),
    ?assertEqual(Pid, whereis(blizanci_servlets)),
    ?assertEqual(ok, ?MODULE:terminate(normal, Fakes)),
    ?assertEqual(Fakes, stopped([])),
    ?assertMatch({error, {servlet_start_failed, blizanci_test_failing,
                          boom}},
                 start_servlets(Fakes ++ [blizanci_test_failing], [])),
    ?assertEqual(lists:reverse(Fakes), stopped([])).

stopped(Acc) ->
    receive {stopped, M} -> stopped(Acc ++ [M])
    after 0 -> Acc end.
