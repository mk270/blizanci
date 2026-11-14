%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

%% @hidden

%% The supervision tree, strategy rest_for_one, in start order:
%%
%%   blizanci_sup
%%     blizanci_conf_serv    owns the configuration table, nothing else
%%     blizanci_servlets     starts/stops the servlets' pools (which
%%                           live under ppool's supervisor) and
%%                           applies configuration changes to them
%%     blizanci_service_ssl  the ranch listener
%%
%% Each depends on those before it: the pools are set up from the
%% table, the listener's TLS and connections read the table and use the
%% pools. So a crash restarts the crashed child and those after it,
%% never those before: the table owner is not logic, and survives a
%% crash of the servlet child with every applied change intact.
%% Shutdown is the reverse of start: listener, then pools, then table.
%%
%% Intensity is 3 restarts in 10 seconds, not the 1 in 5 of a flat
%% one_for_one tree. One fault in the first child restarts all three,
%% and a single flaky servlet start should not take the application
%% down; but a child that keeps crashing still does.

-module(blizanci_sup).

-behaviour(supervisor).
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(LISTENER, blizanci_service_ssl).

% Restart intensity: more than this many restarts within the period
% below takes the supervisor, and so the application, down.
-define(RESTART_INTENSITY, 3).
% The restart period, in seconds.
-define(RESTART_PERIOD_S, 10).
% How long the table owner may take to stop, in milliseconds.
-define(TABLE_SHUTDOWN_MS, 5000).
% How long the servlets server may take to stop, in milliseconds.
-define(SERVLETS_SHUTDOWN_MS, 10000).

start_link(Config) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Config).

init(Config) ->
    #{
      ssl_opts   := SSL_Opts,
      proto_opts := Proto_Opts,
      servlets   := Servlets,
      port       := Port,
      sites      := Sites
     } = Config,

    SupFlags = #{strategy => rest_for_one,
                 intensity => ?RESTART_INTENSITY,
                 period => ?RESTART_PERIOD_S},

    RanchChildSpec = ranch:child_spec(?LISTENER, ranch_ssl, SSL_Opts,
                                      blizanci_connection, Proto_Opts),

    TableChildSpec = #{id => blizanci_conf_serv,
                       start => {blizanci_conf_serv, start_link,
                                 [#{port => Port,
                                    servlets => Servlets,
                                    sites => Sites}]},
                       restart => permanent,
                       shutdown => ?TABLE_SHUTDOWN_MS,
                       type => worker,
                       modules => [blizanci_conf_serv]},

    ServletsChildSpec = #{id => blizanci_servlets,
                          start => {blizanci_servlets, start_link, []},
                          restart => permanent,
                          shutdown => ?SERVLETS_SHUTDOWN_MS,
                          type => worker,
                          modules => [blizanci_servlets]},

    {ok, {SupFlags, [TableChildSpec, ServletsChildSpec, RanchChildSpec]}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(LISTENER_ID, {ranch_embedded_sup, ?LISTENER}).

%% Each test runs the real tree, with a listener on an ephemeral port,
%% in a process of its own, and takes it down afterwards whether or not
%% it passes.
with_tree(Servlets, Fun) ->
    blizanci_test_support:isolated(
      fun() ->
          {ok, _} = application:ensure_all_started(ranch),
          {ok, _} = application:ensure_all_started(ppool),
          blizanci_test_support:live_down(),
          A = blizanci_config:test_site("a.example", "a.pem", "a.key"),
          B = blizanci_config:test_site("b.example", "b.pem", "b.key"),
          process_flag(trap_exit, true),
          {ok, Sup} = start_link(blizanci_config:make(0, Servlets, [A])),
          try Fun(Sup, A, B)
          after
              exit(Sup, shutdown),
              receive {'EXIT', Sup, _} -> ok after 5000 -> ok end,
              blizanci_test_support:live_down()
          end
      end).

%% The pid of each child, by id.
pids(Sup) ->
    maps:from_list([ {Id, Pid}
                     || {Id, Pid, _, _} <- supervisor:which_children(Sup) ]).

%% Wait until each of Ids is running under a pid other than its Old one.
await_restart(Sup, Old, Ids) ->
    New = pids(Sup),
    Restarted = fun(Id) ->
                    Pid = maps:get(Id, New),
                    is_pid(Pid) andalso Pid =/= maps:get(Id, Old)
                end,
    case lists:all(Restarted, Ids) of
        true -> New;
        false -> timer:sleep(10), await_restart(Sup, Old, Ids)
    end.

kill(Pid) ->
    Ref = monitor(process, Pid),
    exit(Pid, kill),
    receive {'DOWN', Ref, process, Pid, _} -> ok end.

%% Children are listed latest-started first.
child_order_test() ->
    with_tree([], fun(Sup, _, _) ->
        ?assertEqual([?LISTENER_ID, blizanci_servlets,
                      blizanci_conf_serv],
                     [ Id || {Id, _, _, _}
                                 <- supervisor:which_children(Sup) ])
    end).

%% A crash of the servlets' process leaves the table owner, and so every
%% change applied since start, alone; the pools come back, those found
%% running adopted and any missing started.
servlets_crash_keeps_config_test() ->
    with_tree([blizanci_cgi], fun(Sup, _, B) ->
        ok = blizanci_conf_serv:publish(0, [blizanci_cgi], [B]),
        Old = pids(Sup),
        ok = blizanci_cgi:stop(),
        kill(maps:get(blizanci_servlets, Old)),
        New = await_restart(Sup, Old, [blizanci_servlets,
                                       ?LISTENER_ID]),
        ?assertEqual(maps:get(blizanci_conf_serv, Old),
                     maps:get(blizanci_conf_serv, New)),
        ?assertMatch({ok, #{sites := [B]}}, blizanci_conf_serv:current()),
        ?assertEqual(not_found, blizanci_conf_serv:find_site(
                                  <<"a.example">>)),
        ?assertNotEqual(undefined, whereis(blizanci_cgi))
    end).

%% A crash of the table owner restarts everything after it, in order:
%% the servlets' process can only start (and bring up its pool) from
%% the re-created table, and the listener after that.
table_crash_restarts_all_test() ->
    with_tree([blizanci_cgi], fun(Sup, A, B) ->
        ok = blizanci_conf_serv:publish(0, [blizanci_cgi], [B]),
        Old = pids(Sup),
        ok = blizanci_cgi:stop(),
        kill(maps:get(blizanci_conf_serv, Old)),
        await_restart(Sup, Old, [blizanci_conf_serv, blizanci_servlets,
                                 ?LISTENER_ID]),
        % the table is rebuilt from the original arguments
        ?assertMatch({ok, #{sites := [A]}}, blizanci_conf_serv:current()),
        ?assertNotEqual(undefined, whereis(blizanci_cgi))
    end).

%% Shutdown is the reverse of start: the listener, then the servlets'
%% pools, then the table.
shutdown_order_test() ->
    with_tree([blizanci_cgi], fun(Sup, _, _) ->
        Pids = pids(Sup),
        Order = [?LISTENER_ID, blizanci_servlets,
                 blizanci_conf_serv],
        [ erlang:trace(maps:get(Id, Pids), true, [procs])
          || Id <- Order ],
        exit(Sup, shutdown),
        Exited = collect_exits(maps:values(Pids), []),
        ?assertEqual([ maps:get(Id, Pids) || Id <- Order ], Exited),
        ?assertEqual(undefined, whereis(blizanci_cgi))
    end).

%% The pids in the order their exit was traced.
collect_exits([], Acc) ->
    lists:reverse(Acc);
collect_exits(Left, Acc) ->
    receive
        {trace, Pid, exit, _} ->
            case lists:member(Pid, Left) of
                true -> collect_exits(Left -- [Pid], [Pid | Acc]);
                false -> collect_exits(Left, Acc)
            end
    after 5000 -> erlang:error({exits_missing, Left, Acc})
    end.

%% A servlet that will not start stops the tree from starting, with a
%% reason that blizanci_app logs as one line.
servlet_start_error_test() ->
    blizanci_test_support:isolated(
      fun() ->
          {ok, _} = application:ensure_all_started(ranch),
          {ok, _} = application:ensure_all_started(ppool),
          blizanci_test_support:live_down(),
          A = blizanci_config:test_site("a.example", "a.pem", "a.key"),
          process_flag(trap_exit, true),
          Config = blizanci_config:make(
                     0, [blizanci_cgi, blizanci_test_failing], [A]),
          try start_link(Config) of
              {error, Reason} ->
                  ?assertEqual("servlet blizanci_test_failing could not "
                               "start: boom",
                               blizanci_config:describe_error(Reason)),
                  ?assertEqual(undefined, whereis(blizanci_cgi)),
                  ?assertEqual(undefined, whereis(?SERVER))
          after
              blizanci_test_support:live_down()
          end
      end).
