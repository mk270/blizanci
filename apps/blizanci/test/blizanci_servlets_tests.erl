%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_servlets_tests).

-include_lib("eunit/include/eunit.hrl").

-import(blizanci_servlets,
        [reconfigure/3, start_link/0, start_servlets/2]).

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
    ?assertEqual(ok, blizanci_servlets:terminate(normal, Fakes)),
    ?assertEqual(Fakes, stopped([])),
    ?assertMatch({error, {servlet_start_failed, blizanci_test_failing,
                          boom}},
                 start_servlets(Fakes ++ [blizanci_test_failing], [])),
    ?assertEqual(lists:reverse(Fakes), stopped([])).

stopped(Acc) ->
    receive {stopped, M} -> stopped(Acc ++ [M])
    after 0 -> Acc end.
