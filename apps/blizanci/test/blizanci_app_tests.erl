%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_app_tests).

-include_lib("eunit/include/eunit.hrl").
-include("blizanci_types.hrl").

-define(OLD_KEYS, [hostname, certfile, keyfile, docroot, cgiroot,
                   ca_certs, titan_work_dir, routing]).
-define(ENV_KEYS, [port, servlets, realms, sites | ?OLD_KEYS]).

%% start/2 reads the application environment of the application it runs
%% in, so these tests start blizanci itself (application:start/1, which
%% calls start/2) with an environment of the test's choosing, in a
%% process of their own. Env is a list of keys to set, or a fun of the
%% certificate and key file names of a throwaway pair returning one;
%% the old flat keys and sites are unset first. The environment, the
%% loaded state of the application, any applications started on the
%% way, the logger handler and the PEM fixture are all put back
%% afterwards, pass or fail. Port
%% 0, and neither test gets as far as a listener: nothing binds a
%% fixed port.
with_app_env(EnvArg, Fun) ->
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
          Env = case is_function(EnvArg, 2) of
                    true -> EnvArg(Cert, Key);
                    false -> EnvArg
                end,
          [ application:unset_env(blizanci, K) || K <- ?ENV_KEYS ],
          [ application:set_env(blizanci, K, V)
            || {K, V} <- [{port, free_port()} | Env] ],
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

%% The environment of a start-up test with one site, test.example, whose
%% certificate and key are those of the throwaway pair.
one_site(Routing, Extra) ->
    one_site("test.example", Routing, Extra).

one_site(Host, Routing, Extra) ->
    fun(Cert, Key) ->
        [{sites, [{Host, [{certfile, Cert}, {keyfile, Key},
                          {routing, Routing}]}]} | Extra]
    end.

blizanci_lines(Logged) ->
    [ T || {error, "blizanci: " ++ _ = T} <- Logged ].

%% A servlet whose start/0 returns {error, boom}: the application does
%% not start, with that reason, and says so in one line, rather than
%% crashing on a badmatch.
servlet_start_failure_test() ->
    with_app_env(
      one_site([], [{servlets, [blizanci_test_failing]}]),
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
      one_site([Bad], [{servlets, [blizanci_test_stub]}]),
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
      [{servlets, []},
       {realms, [{<<"r">>, ["/no/such/ca.pem"]}]},
       {sites, [{"test.example",
                 [{certfile, "/no/such/cert.pem"},
                  {keyfile, "/no/such/key.pem"},
                  {routing, [{mount, gemini, "/a", Stub,
                              {private, {realm, <<"r">>}}, #{}},
                             {mount, gemini, "/b", Stub,
                              {private, {realm, <<"q">>}}, #{}},
                             {gemini, "x", Stub, public, #{}},
                             {mount, gemini, "/c",
                              blizanci_no_such_servlet, public, #{}},
                             {mount, gemini, "/d", Stub, public,
                              #{}}]}]}]}],
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
      fun(_, Key) ->
          [{servlets, [blizanci_test_stub]},
           {sites, [{"test.example",
                     [{certfile, "/no/such/cert.pem"}, {keyfile, Key},
                      {routing, []}]}]}]
      end,
      fun() ->
          ?assertMatch({error, _},
                       application:ensure_all_started(blizanci)),
          ?assertEqual(["blizanci: site test.example: the certificate "
                        "file is not a readable PEM file "
                        "(couldnt_open_pem_file)"],
                       blizanci_lines(logged()))
      end),
    with_app_env(
      fun(Cert, _) ->
          [{servlets, [blizanci_test_stub]},
           {sites, [{"test.example",
                     [{certfile, Cert}, {keyfile, "/no/such/key.pem"},
                      {routing, []}]}]}]
      end,
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
      one_site(42, [], [{port, 99999},
                        {servlets, [blizanci_test_stub]}]),
      fun() ->
          ?assertMatch({error, _},
                       application:ensure_all_started(blizanci)),
          ?assertEqual(["blizanci: hostname 42 is not a non-empty string",
                        "blizanci: port 99999 is not a TCP port number "
                        "(1 to 65535)"],
                       blizanci_lines(logged()))
      end),
    with_app_env(
      one_site("Test.EXAMPLE", [], [{servlets, [blizanci_test_stub]}]),
      fun() ->
          ?assertMatch({ok, _}, application:ensure_all_started(blizanci)),
          ?assertEqual([], blizanci_lines(logged()))
      end).

%% Shadowed routes are a warning, not an error: it starts.
shadowed_routes_warning_test() ->
    Stub = blizanci_test_stub,
    with_app_env(
      one_site([{mount, gemini, "/", Stub, public, #{}},
                {mount, gemini, "/x", Stub, public, #{}}],
               [{servlets, [Stub]}]),
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
      one_site([Old("(?<PATH>a)"), Old("(?<PATH>b)"),
                {mount, gemini, "/m", blizanci_test_stub, public, Stub}],
               [{servlets, [blizanci_test_stub]}]),
      fun() ->
          ?assertMatch({ok, _}, application:ensure_all_started(blizanci)),
          ?assertMatch([{warning, "routing: site test.example: 2 routes "
                         "use the old tuple "
                         "format {Proto, Regex, Module, Auth, Opts}; "
                         "translated, behaviour unchanged (see manual, "
                         "Routing)"}],
                       routing_warnings(logged()))
      end).

%% New-format routes alone give no warning.
new_format_routes_quiet_test() ->
    Stub = #{ script => fun(_) -> {immediate, {error_code, no_such}} end },
    with_app_env(
      one_site([{mount, gemini, "/m", blizanci_test_stub, public, Stub},
                {regex, gemini, "(?<PATH>a)", blizanci_test_stub,
                 public, Stub}],
               [{servlets, [blizanci_test_stub]}]),
      fun() ->
          ?assertMatch({ok, _}, application:ensure_all_started(blizanci)),
          ?assertEqual([], routing_warnings(logged()))
      end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The sites key.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The environment of a sites test.
sites_env(Sites) ->
    [{servlets, [blizanci_test_stub]}, {sites, Sites}].

%% Two certificate and key pairs, in a directory of their own.
with_pairs(Fun) ->
    Dir = blizanci_x509:test_tmp_dir(),
    try Fun(blizanci_x509:test_pair_files(Dir, "a", rsa),
            blizanci_x509:test_pair_files(Dir, "b", rsa))
    after file:del_dir_r(Dir)
    end.

public_routes() ->
    [{mount, gemini, "/", blizanci_test_stub, public, #{}}].

site_opts({Cert, Key}) ->
    [{certfile, Cert}, {keyfile, Key}, {routing, public_routes()}].

%% Start the application and return what it logged as errors.
start_errors() ->
    ?assertMatch({error, _}, application:ensure_all_started(blizanci)),
    blizanci_lines(logged()).

%% Two sites are built, each from its own files and routes, and both
%% are served; a name with capitals is folded.
two_sites_start_test() ->
    with_pairs(
      fun(A, B) ->
          with_app_env(
            sites_env([{"a.example", site_opts(A)},
                       {"B.Example", site_opts(B)}]),
            fun() ->
                ?assertMatch({ok, _},
                             application:ensure_all_started(blizanci)),
                ?assertEqual([], blizanci_lines(logged())),
                [ ?assertMatch({ok, _}, blizanci_conf_serv:find_site(H))
                  || H <- [<<"a.example">>, <<"b.example">>] ],
                ?assertEqual(not_found,
                             blizanci_conf_serv:find_site(<<"c.example">>))
            end)
      end).

%% The path options of a site's routes are converted to binaries when
%% its routes are built; one that is not plain
%% ASCII is refused, naming the site, the route and the option.
sites_path_options_test_() ->
    {timeout, 120, fun sites_path_options_body/0}.

sites_path_options_body() ->
    with_pairs(
      fun(A, B) ->
          Route = fun(Opts) ->
                      [{mount, gemini, "/", blizanci_test_stub, public,
                        Opts}]
                  end,
          Site = fun(Pair, Opts) ->
                     [{certfile, element(1, Pair)},
                      {keyfile, element(2, Pair)},
                      {routing, Route(Opts)}]
                 end,
          with_app_env(
            sites_env([{"a.example",
                        Site(A, #{ docroot => "some/docs",
                                   cgiroot => <<"/srv/cgi">>,
                                   work_dir => "/srv/work" })}]),
            fun() ->
                ?assertMatch({ok, _},
                             application:ensure_all_started(blizanci)),
                {ok, S} = blizanci_conf_serv:find_site(<<"a.example">>),
                ?assertMatch([#route{options = #{
                                       docroot := <<"some/docs">>,
                                       cgiroot := <<"/srv/cgi">>,
                                       work_dir := <<"/srv/work">> }}],
                             blizanci_config:routes(S))
            end),
          with_app_env(
            sites_env([{"a.example", Site(A, #{ docroot => "ok" })},
                       {"b.example",
                        Site(B, #{ cgiroot => [$c, 233] })}]),
            fun() ->
                ?assertEqual(
                   ["blizanci: site b.example: invalid routing table: "
                    "route 1 (mount route): the cgiroot option must be a "
                    "plain ASCII file name (printable ASCII characters "
                    "only), not [99,233]"],
                   start_errors())
            end)
      end).

%% Each of the old flat keys alone is refused, with one line naming it
%% and nothing else reported, even though the rest is fine.
single_site_key_refused_test_() ->
    {timeout, 120, fun single_site_key_refused_body/0}.

single_site_key_refused_body() ->
    with_pairs(
      fun(A, _) ->
          Values = [{hostname, "x.example"}, {certfile, "c.pem"},
                    {keyfile, "k.pem"}, {docroot, "."}, {cgiroot, "."},
                    {ca_certs, []}, {titan_work_dir, "."},
                    {routing, []}],
          [ with_app_env(
              sites_env([{"a.example", site_opts(A)}]) ++ [KV],
              fun() ->
                  ?assertEqual([old_keys_line(atom_to_list(K))],
                               start_errors())
              end) || {K, _} = KV <- Values ],
          %% without sites too: the same line, not "no sites"
          [ with_app_env(
              [KV],
              fun() ->
                  ?assertEqual([old_keys_line(atom_to_list(K))],
                               start_errors())
              end) || {K, _} = KV <- Values ],
          %% several at once, all named, in a fixed order
          with_app_env(
            sites_env([{"a.example", site_opts(A)}])
            ++ [{routing, []}, {hostname, "x"}, {ca_certs, []}],
            fun() ->
                ?assertEqual([old_keys_line("hostname, ca_certs, "
                                            "routing")],
                             start_errors())
            end),
          with_app_env(
            [{certfile, "c.pem"}, {keyfile, "k.pem"}, {docroot, "."}],
            fun() ->
                ?assertEqual([old_keys_line("certfile, keyfile, "
                                            "docroot")],
                             start_errors())
            end)
      end).

old_keys_line(Keys) ->
    "blizanci: the single-site keys " ++ Keys ++ " are no longer "
    "supported; move them into an entry of the sites key".

%% No sites key and no old key: no sites.
no_sites_key_test() ->
    with_app_env(
      [{servlets, [blizanci_test_stub]}],
      fun() ->
          ?assertEqual(["blizanci: no sites are configured; the sites "
                        "key must list at least one"],
                       start_errors())
      end).

%% A valid configuration of one site starts.
one_site_starts_test() ->
    with_app_env(
      one_site([{mount, gemini, "/", blizanci_test_stub, public, #{}}],
               [{servlets, [blizanci_test_stub]}]),
      fun() ->
          ?assertMatch({ok, _}, application:ensure_all_started(blizanci)),
          ?assertEqual([], blizanci_lines(logged())),
          ?assertMatch({ok, _},
                       blizanci_conf_serv:find_site(<<"test.example">>))
      end).

%% Each way a site can be wrong, one line naming the site.
site_shape_errors_test_() ->
    {timeout, 120, fun site_shape_errors_body/0}.

site_shape_errors_body() ->
    with_pairs(
      fun(A, _) ->
          [Cert, Key, Routing] = site_opts(A),
          Case = fun(Sites, Lines) ->
                     with_app_env(
                       sites_env(Sites),
                       fun() ->
                           ?assertEqual(["blizanci: " ++ L || L <- Lines],
                                        start_errors())
                       end)
                 end,
          Case([{"a.example", [Key, Routing]}],
               ["site a.example: the required key certfile is missing"]),
          Case([{"a.example", [Cert, Routing]}],
               ["site a.example: the required key keyfile is missing"]),
          Case([{"a.example", [Cert, Key]}],
               ["site a.example: the required key routing is missing"]),
          Case([{"a.example", []}],
               ["site a.example: the required key certfile is missing",
                "site a.example: the required key keyfile is missing",
                "site a.example: the required key routing is missing"]),
          Case([{"a.example", [Cert, Key, Routing, {docroot, "."}]}],
               ["site a.example: docroot is not a key of a site (a site "
                "takes only certfile, keyfile and routing)"]),
          Case([{"a.example", [Cert, Key, Routing, {ca_certs, []},
                               {hostname, "z"}]}],
               ["site a.example: ca_certs is not a key of a site (a site "
                "takes only certfile, keyfile and routing)",
                "site a.example: hostname is not a key of a site (a site "
                "takes only certfile, keyfile and routing)"]),
          Case([{"a.example", [Cert, Key, Routing, Routing]}],
               ["site a.example: the key routing is given more than "
                "once"]),
          Case([{"a.example", [{certfile, ""}, Key, Routing]}],
               ["site a.example: certfile must be a non-empty file name "
                "string, not []"]),
          Case([{"a.example", not_a_list}],
               ["site a.example: the site's options must be a list, not "
                "not_a_list"]),
          Case([not_a_pair],
               ["sites entry not_a_pair is not a {Hostname, Options} "
                "pair"]),
          Case([{42, site_opts(A)}],
               ["hostname 42 is not a non-empty string"]),
          Case([{"", site_opts(A)}],
               ["hostname [] is not a non-empty string"]),
          Case([{<<"a.example">>, site_opts(A)}],
               ["hostname <<\"a.example\">> is not a non-empty string"])
      end),
    with_app_env(
      sites_env(not_a_list),
      fun() ->
          ?assertEqual(["blizanci: sites must be a list of {Hostname, "
                        "Options}, not not_a_list"],
                       start_errors())
      end),
    %% no sites at all is "no sites", as ever
    with_app_env(
      sites_env([]),
      fun() ->
          ?assertEqual(["blizanci: no sites are configured; the sites "
                        "key must list at least one"],
                       start_errors())
      end).

%% The same name twice, after folding to lower case.
duplicate_sites_test_() ->
    {timeout, 120, fun duplicate_sites_body/0}.

duplicate_sites_body() ->
    with_pairs(
      fun(A, B) ->
          with_app_env(
            sites_env([{"a.example", site_opts(A)},
                       {"A.EXAMPLE", site_opts(B)}]),
            fun() ->
                ?assertEqual(["blizanci: site a.example is configured "
                              "more than once"],
                             start_errors())
            end)
      end).

%% Every problem of every site is reported in one pass, the sites in
%% their order, a bad site not hiding the next, and the realms first.
several_sites_problems_test_() ->
    {timeout, 120, fun several_sites_problems_body/0}.

several_sites_problems_body() ->
    with_pairs(
      fun(A, B) ->
          Stub = blizanci_test_stub,
          [_, _, Routing] = site_opts(B),
          with_app_env(
            sites_env(
              [{"one.example",
                [{certfile, "/no/such/cert.pem"},
                 {keyfile, element(2, A)},
                 {routing, [{mount, gemini, "/a", Stub,
                             {private, {realm, <<"q">>}}, #{}},
                            {mount, gemini, "/b", blizanci_no_such, public,
                             #{}}]}]},
               {"two.example", [{keyfile, element(2, B)}, Routing]},
               {"three.example", site_opts(A) ++ [{extra, 1}]},
               {"four.example", site_opts(B)}])
            ++ [{port, 99999},
                {realms, [{<<"r">>, ["/no/such/ca.pem"]}]}],
            fun() ->
                ?assertEqual(
                   ["blizanci: realm <<\"r\">>: CA file /no/such/ca.pem "
                    "cannot be used (enoent); check that it exists and "
                    "holds a PEM certificate",
                    "blizanci: site one.example: invalid routing table: "
                    "route 1 (mount route): route \"/a\" (servlet "
                    "blizanci_test_stub) names realm <<\"q\">>, which is "
                    "not defined",
                    "blizanci: site one.example: invalid routing table: "
                    "route 2 (mount route): servlet blizanci_no_such "
                    "cannot be loaded; check the module name and that it "
                    "is installed",
                    "blizanci: site one.example: the certificate file is "
                    "not a readable PEM file (couldnt_open_pem_file)",
                    "blizanci: site two.example: the required key "
                    "certfile is missing",
                    "blizanci: site three.example: extra is not a key of "
                    "a site (a site takes only certfile, keyfile and "
                    "routing)",
                    "blizanci: port 99999 is not a TCP port number "
                    "(1 to 65535)"],
                   start_errors())
            end)
      end).

%% The sites are validated together: a site whose key does not match
%% its certificate is not built, and an undeclared servlet is reported
%% for the built site and for the unbuilt one.
sites_validated_together_test_() ->
    {timeout, 120, fun sites_validated_together_body/0}.

sites_validated_together_body() ->
    with_pairs(
      fun(A, B) ->
          with_app_env(
            sites_env(
              [{"a.example",
                [{certfile, element(1, A)}, {keyfile, element(2, B)},
                 {routing, public_routes()}]},
               {"b.example", site_opts(B)}])
            ++ [{servlets, []}],
            fun() ->
                ?assertEqual(
                   ["blizanci: site a.example: certificate "
                    ++ element(1, A) ++ " and private key "
                    ++ element(2, B) ++ " are not a matching pair",
                    %% built sites are validated first, then the
                    %% servlets used by the one that was not built
                    "blizanci: site b.example: a route uses servlet "
                    "blizanci_test_stub, which is not in the servlets "
                    "list",
                    "blizanci: site a.example: a route uses servlet "
                    "blizanci_test_stub, which is not in the servlets "
                    "list"],
                   start_errors())
            end)
      end),
    with_pairs(
      fun(A, B) ->
          with_app_env(
            sites_env([{"a.example", site_opts(A)},
                       {"b.example", site_opts(B)}])
            ++ [{servlets, []}],
            fun() ->
                ?assertEqual(
                   ["blizanci: site a.example: a route uses servlet "
                    "blizanci_test_stub, which is not in the servlets "
                    "list",
                    "blizanci: site b.example: a route uses servlet "
                    "blizanci_test_stub, which is not in the servlets "
                    "list"],
                   start_errors())
            end)
      end).

%% The warnings about a routing table say which site's it is.
sites_routing_warnings_test() ->
    with_pairs(
      fun(A, B) ->
          Stub = blizanci_test_stub,
          Old = {gemini, "(?<PATH>a)", Stub, public, #{}},
          with_app_env(
            sites_env(
              [{"a.example", site_opts(A)},
               {"B.Example",
                [{certfile, element(1, B)}, {keyfile, element(2, B)},
                 {routing, [Old,
                            {mount, gemini, "/", Stub, public, #{}},
                            {mount, gemini, "/x", Stub, public,
                             #{}}]}]}]),
            fun() ->
                ?assertMatch({ok, _},
                             application:ensure_all_started(blizanci)),
                ?assertMatch(
                   [{warning, "routing: site b.example: 1 routes use "
                     "the old tuple format " ++ _},
                    {warning, "routing: site b.example: route 3 " ++ _}],
                   routing_warnings(logged()))
            end)
      end).
