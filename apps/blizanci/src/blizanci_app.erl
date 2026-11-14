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
        {error, Errors} ->
            % every problem found, one line each, so that the operator
            % can fix them all before the next start
            [ logger:error("blizanci: ~s",
                           [blizanci_config:describe_error(E)])
              || E <- Errors ],
            {error, {invalid_configuration, Errors}}
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

-define(ENV_KEYS, [certfile, keyfile, hostname, port, servlets, realms,
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
          {Cert, Key} = blizanci_x509:test_pair_files(Dir, "site", rsa),
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
          Base = [{certfile, Cert}, {keyfile, Key},
                  {hostname, "test.example"}, {port, free_port()}],
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

%% A port that was free a moment ago: validation refuses port 0, so the
%% tests that get as far as a listener name a real one.
free_port() ->
    {ok, L} = gen_tcp:listen(0, []),
    {ok, Port} = inet:port(L),
    ok = gen_tcp:close(L),
    Port.

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
          ?assertMatch({error, {blizanci, {{invalid_configuration,
                                 [{site, "test.example",
                                   {invalid_routing,
                                    {bad_route, 1, tuple,
                                     {route_without_path_group, "x",
                                      blizanci_test_stub}}}}]},
                                {blizanci_app, start, _}}}},
                       Result),
          ?assertMatch(["blizanci: site test.example: invalid routing "
                        "table: route 1 (old-format route): route \"x\" "
                        ++ _],
                       blizanci_lines(logged())),
          ?assertEqual(undefined, whereis(blizanci_sup)),
          ?assertEqual(undefined, whereis(blizanci_conf_serv))
      end).

%% Start-up refuses with the list of everything that is wrong, one
%% logged line each, in a stable order: the realms, then the site's
%% routes in order, then the certificate and key, then the servlets.
%% The route naming the failed realm gets no second error, the route
%% naming an undefined one does.
all_problems_reported_test() ->
    Stub = blizanci_test_stub,
    with_app_env(
      [{certfile, "/no/such/cert.pem"}, {keyfile, "/no/such/key.pem"},
       {servlets, []},
       {realms, [{<<"r">>, ["/no/such/ca.pem"]}]},
       {routing, [{mount, gemini, "/a", Stub,
                   {private, {realm, <<"r">>}}, #{}},
                  {mount, gemini, "/b", Stub,
                   {private, {realm, <<"q">>}}, #{}},
                  {gemini, "x", Stub, public, #{}},
                  {mount, gemini, "/c", blizanci_no_such_servlet,
                   public, #{}},
                  {mount, gemini, "/d", Stub, public, #{}}]}],
      fun() ->
          Result = application:ensure_all_started(blizanci),
          ?assertMatch({error, {blizanci,
                                {{invalid_configuration, [_|_]},
                                 {blizanci_app, start, _}}}}, Result),
          ?assertEqual(
             ["blizanci: realm <<\"r\">>: CA file /no/such/ca.pem cannot "
              "be used (enoent); check that it exists and holds a PEM "
              "certificate",
              "blizanci: site test.example: invalid routing table: "
              "route 2 (mount route): route \"/b\" "
              "(servlet blizanci_test_stub) names realm <<\"q\">>, which "
              "is not defined",
              "blizanci: site test.example: invalid routing table: "
              "route 3 (old-format route): route \"x\" (servlet "
              "blizanci_test_stub) has no named group PATH in its "
              "regex; add one, e.g. (?<PATH>.*)",
              "blizanci: site test.example: invalid routing table: "
              "route 4 (mount route): servlet blizanci_no_such_servlet "
              "cannot be loaded; check the module name and that it is "
              "installed",
              "blizanci: site test.example: the certificate file is not "
              "a readable PEM file (couldnt_open_pem_file)",
              "blizanci: site test.example: the key file is not a "
              "readable PEM file (couldnt_open_pem_file)",
              "blizanci: site test.example: a route uses servlet "
              "blizanci_test_stub, which is not in the servlets list"],
             blizanci_lines(logged())),
          ?assertEqual(undefined, whereis(blizanci_sup)),
          ?assertEqual(undefined, whereis(blizanci_conf_serv))
      end).

%% An unreadable certificate alone, and an unreadable key alone, each
%% give one line naming the site.
unreadable_cert_and_key_test() ->
    with_app_env(
      [{certfile, "/no/such/cert.pem"}, {servlets, [blizanci_test_stub]},
       {routing, []}],
      fun() ->
          ?assertMatch({error, _},
                       application:ensure_all_started(blizanci)),
          ?assertEqual(["blizanci: site test.example: the certificate "
                        "file is not a readable PEM file "
                        "(couldnt_open_pem_file)"],
                       blizanci_lines(logged()))
      end),
    with_app_env(
      [{keyfile, "/no/such/key.pem"}, {servlets, [blizanci_test_stub]},
       {routing, []}],
      fun() ->
          ?assertMatch({error, _},
                       application:ensure_all_started(blizanci)),
          ?assertEqual(["blizanci: site test.example: the key file is "
                        "not a readable PEM file "
                        "(couldnt_open_pem_file)"],
                       blizanci_lines(logged()))
      end).

%% A hostname that is not a string cannot name a site: that is one
%% line, with no "no sites" or other follow-on, and the rest (here a bad
%% port) is still checked. An upper-case hostname is folded, as it
%% always was, and starts.
bad_hostname_test() ->
    with_app_env(
      [{hostname, 42}, {port, 99999}, {servlets, [blizanci_test_stub]},
       {routing, []}],
      fun() ->
          ?assertMatch({error, _},
                       application:ensure_all_started(blizanci)),
          ?assertEqual(["blizanci: hostname 42 is not a non-empty string",
                        "blizanci: port 99999 is not a TCP port number "
                        "(1 to 65535)"],
                       blizanci_lines(logged()))
      end),
    with_app_env(
      [{hostname, "Test.EXAMPLE"}, {servlets, [blizanci_test_stub]},
       {routing, []}],
      fun() ->
          ?assertMatch({ok, _}, application:ensure_all_started(blizanci)),
          ?assertEqual([], blizanci_lines(logged()))
      end).

%% Shadowed routes are a warning, not an error: it starts.
shadowed_routes_warning_test() ->
    Stub = blizanci_test_stub,
    with_app_env(
      [{servlets, [Stub]},
       {routing, [{mount, gemini, "/", Stub, public, #{}},
                  {mount, gemini, "/x", Stub, public, #{}}]}],
      fun() ->
          ?assertMatch({ok, _}, application:ensure_all_started(blizanci)),
          ?assertMatch([{warning, "routing: " ++ _}],
                       routing_warnings(logged())),
          ?assertEqual([], blizanci_lines(logged()))
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

%% The default table is in the new format, and it never warns about
%% the old format.
default_routing_quiet_test() ->
    with_app_env(
      [{servlets, [blizanci_static, blizanci_cgi]},
       {docroot, "."}, {cgiroot, "."}],
      fun() ->
          ?assertMatch({ok, _}, application:ensure_all_started(blizanci)),
          ?assertEqual([], routing_warnings(logged()))
      end).
