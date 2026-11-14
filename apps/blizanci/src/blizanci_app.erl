%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

%% @hidden

-module(blizanci_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    case blizanci_config:from_app_env() of
        {ok, Config} ->
            case blizanci_sup:start_link(Config) of
                {ok, Pid} ->
                    {ok, Pid};
                {error, Reason} ->
                    logger:error("blizanci: ~s",
                                 [blizanci_config:describe_error(Reason)]),
                    {error, Reason}
            end;
        {error, Reason} ->
            logger:error("blizanci: ~s",
                         [blizanci_config:describe_error(Reason)]),
            {error, Reason}
    end.

% The declared servlets' resources are set up and torn down by
% blizanci_servlets, which tracks the declared list as it changes
% (blizanci_config:apply/1).
stop(_State) ->
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("eunit/include/eunit.hrl").

-define(ENV_KEYS, [certfile, keyfile, hostname, port, servlets,
                   routing, docroot, cgiroot]).

%% start/2 reads the application environment of the application it runs
%% in, so these tests start blizanci itself (application:start/1, which
%% calls start/2) with an environment of the test's choosing, in a
%% process of their own. The environment, the loaded state of the
%% application, any applications started on the way, the logger handler
%% and the PEM fixture are all put back afterwards, pass or fail. Port
%% 0, and neither test gets as far as a listener: nothing binds a
%% fixed port.
with_app_env(Env, Fun) ->
    blizanci_test_support:isolated(
      fun() ->
          Dir = blizanci_x509:test_tmp_dir(),
          Pki = blizanci_x509:test_pki(),
          Cert = blizanci_x509:test_write_pem(
                   Dir, "cert.pem",
                   [{'Certificate', maps:get(ca, Pki), not_encrypted}]),
          Loaded = lists:keymember(blizanci, 1,
                                   application:loaded_applications()),
          ok = case Loaded of
                   true -> ok;
                   false -> application:load(blizanci)
               end,
          Saved = [ {K, application:get_env(blizanci, K)}
                    || K <- ?ENV_KEYS ],
          Before = running_apps(),
          Handler = list_to_atom(
                      "blizanci_app_test_log_"
                      ++ integer_to_list(
                           erlang:unique_integer([positive]))),
          ok = logger:add_handler(Handler, blizanci_test_log_handler,
                                  #{config => #{notify => self()}}),
          Base = [{certfile, Cert}, {keyfile, Cert},
                  {hostname, "test.example"}, {port, 0}],
          [ application:set_env(blizanci, K, V)
            || {K, V} <- Base ++ Env ],
          try Fun()
          after
              ok = logger:remove_handler(Handler),
              [ stop_app(A) || A <- running_apps() -- Before ],
              [ case V of
                    undefined -> application:unset_env(blizanci, K);
                    {ok, Old} -> application:set_env(blizanci, K, Old)
                end || {K, V} <- Saved ],
              case Loaded of
                  true -> ok;
                  false -> application:unload(blizanci)
              end,
              file:del_dir_r(Dir)
          end
      end).

running_apps() ->
    [ A || {A, _, _} <- application:which_applications() ].

stop_app(A) ->
    _ = application:stop(A),
    ok.

%% What the handler has been sent so far, in order. start/2 logs in the
%% application master's process before application:start/1 returns, so
%% by then everything it logged is already in the mailbox.
logged() ->
    receive {logged, L, T} -> [{L, T} | logged()]
    after 0 -> []
    end.

blizanci_lines(Logged) ->
    [ T || {error, "blizanci: " ++ _ = T} <- Logged ].

%% A servlet whose start/0 returns {error, boom}: the application does
%% not start, with that reason, and says so in one line, rather than
%% crashing on a badmatch.
servlet_start_failure_test() ->
    with_app_env(
      [{servlets, [blizanci_test_failing]}, {routing, []}],
      fun() ->
          Result = application:ensure_all_started(blizanci),
          ?assertMatch({error, {blizanci, {{shutdown,
                                 {failed_to_start_child,
                                  blizanci_servlets,
                                  {servlet_start_failed,
                                   blizanci_test_failing, boom}}},
                                {blizanci_app, start, _}}}},
                       Result),
          ?assertEqual(["blizanci: servlet blizanci_test_failing could "
                        "not start: boom"],
                       blizanci_lines(logged())),
          ?assertEqual(undefined, whereis(blizanci_sup)),
          ?assertEqual(undefined, whereis(blizanci_servlets)),
          ?assertEqual(undefined, whereis(blizanci_conf_serv))
      end).

%% A routing table that will not build is a configuration error, found
%% before the tree is started; it is logged once, and no process or
%% table is left behind.
invalid_routing_test() ->
    Bad = {gemini, "x", blizanci_test_stub, public, #{}},
    with_app_env(
      [{servlets, [blizanci_test_stub]}, {routing, [Bad]}],
      fun() ->
          Result = application:ensure_all_started(blizanci),
          ?assertMatch({error, {blizanci, {{invalid_routing,
                                 {bad_route, 1, tuple,
                                  {route_without_path_group, "x",
                                   blizanci_test_stub}}},
                                {blizanci_app, start, _}}}},
                       Result),
          ?assertMatch(["blizanci: invalid routing table: route 1 "
                        "(old-format route): route \"x\" " ++ _],
                       blizanci_lines(logged())),
          ?assertEqual(undefined, whereis(blizanci_sup)),
          ?assertEqual(undefined, whereis(blizanci_conf_serv))
      end).

routing_warnings(Logged) ->
    [ {L, T} || {L, "routing: " ++ _ = T} <- Logged ].

%% An operator's own old-format routes are translated, with one warning
%% at start-up giving their number.
old_format_routes_warning_test() ->
    Stub = #{ script => fun(_) -> {immediate, {error_code, no_such}} end },
    Old = fun(Re) -> {gemini, Re, blizanci_test_stub, public, Stub} end,
    with_app_env(
      [{servlets, [blizanci_test_stub]},
       {routing, [Old("(?<PATH>a)"), Old("(?<PATH>b)"),
                  {mount, gemini, "/m", blizanci_test_stub, public,
                   Stub}]}],
      fun() ->
          ?assertMatch({ok, _}, application:ensure_all_started(blizanci)),
          ?assertMatch([{warning, "routing: 2 routes use the old tuple "
                         "format {Proto, Regex, Module, Auth, Opts}; "
                         "translated, behaviour unchanged (see README, "
                         "Routing)"}],
                       routing_warnings(logged()))
      end).

%% New-format routes alone give no warning.
new_format_routes_quiet_test() ->
    Stub = #{ script => fun(_) -> {immediate, {error_code, no_such}} end },
    with_app_env(
      [{servlets, [blizanci_test_stub]},
       {routing, [{mount, gemini, "/m", blizanci_test_stub, public, Stub},
                  {regex, gemini, "(?<PATH>a)", blizanci_test_stub,
                   public, Stub}]}],
      fun() ->
          ?assertMatch({ok, _}, application:ensure_all_started(blizanci)),
          ?assertEqual([], routing_warnings(logged()))
      end).

%% The project's own default table is still made of old tuples, but it
%% is not the operator's doing, so it does not warn.
default_routing_quiet_test() ->
    with_app_env(
      [{servlets, [blizanci_static, blizanci_cgi]},
       {docroot, "."}, {cgiroot, "."}],
      fun() ->
          ?assertMatch({ok, _}, application:ensure_all_started(blizanci)),
          ?assertEqual([], routing_warnings(logged()))
      end).
