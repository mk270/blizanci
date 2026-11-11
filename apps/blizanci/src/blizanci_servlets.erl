%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2026  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

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
%% it calls stop/0 on the servlets it believes are running. That list is
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
            [ ok = Servlet:start() || Servlet <- Servlets ],
            {ok, Servlets};
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
    [ ok = Servlet:stop() || Servlet <- Servlets ],
    ok.


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
            [ ok = S:stop() || S <- Removed ],
            {reply, ok, Servlets};
        {error, _} = Error ->
            {reply, Error, Old}
    end.


% Call start/0 on each servlet in turn; if one fails, call stop/0 on
% those already started so a failed apply leaves nothing behind.
-spec start_servlets(Servlets, Started) -> Result
              when Servlets :: [module()],
                   Started  :: [module()],
                   Result   :: 'ok' | {'error', term()}.
start_servlets([], _Started) ->
    ok;
start_servlets([Servlet | Rest], Started) ->
    try Servlet:start() of
        ok -> start_servlets(Rest, [Servlet | Started])
    catch
        Class:Reason ->
            [ ok = S:stop() || S <- Started ],
            {error, {servlet_start_failed, Servlet, {Class, Reason}}}
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
