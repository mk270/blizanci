%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_config_tests).

-include_lib("eunit/include/eunit.hrl").
-include("blizanci_types.hrl").

-import(blizanci_config,
        [add_realm/3, add_route/2, apply/1, ascii_lowercase/1,
         describe_error/1, describe_route_error/1, find_site/2, global/2,
         insert_route/3, make/3, new/2, problems_site/1, realm/1,
         realms/0, realms/1, realms_from_env/1, route/2, route/5, route/6,
         routes/1, site/3, site_wants_client_cert/1, ssl_opts/1,
         test_site/3, tls_versions/0, validate/1, validate_all/1]).

% Build one site from explicit values.
make_site(Hostname, Cert, Key, Routes) ->
    make_site(Hostname, Cert, Key, Routes, #{}).

make_site(Hostname, Cert, Key, Routes, Realms) ->
    build_site(Hostname, Cert, Key, Routes, Realms, #{}).

% As make_site/5, with options for blizanci_router:prepare/3.
build_site(Hostname, Cert, Key, Routes, Realms, PrepareOpts) ->
    case blizanci_router:prepare(Routes, Realms, PrepareOpts) of
        {ok, Routing} ->
            case site(Hostname, Cert, Key) of
                {ok, Site} ->
                    {ok, Site#site{routing = Routing}};
                {error, _} = Error -> Error
            end;
        {error, Reason} -> {error, {invalid_routing, Reason}}
    end.

-define(TEST_DOCROOT, "test-docroot").
-define(RX, "(?<PATH>x)").

route_ok_test_() ->
    Opts = #{ docroot => ?TEST_DOCROOT },
    Static = fun(Regex, Auth) ->
                 route(gemini, Regex, blizanci_static, Auth, Opts)
             end,
    [?_assertMatch({ok, #route{proto = gemini,
                               module = blizanci_static,
                               auth_policy = public}},
                   Static("(?<PATH>.*)", public)),
     ?_assertMatch({ok, #route{}}, Static(<<"(?<PATH>.*)">>, public)),
     ?_assertMatch({ok, #route{auth_policy = restricted}},
                   Static(?RX, restricted)),
     ?_assertMatch({ok, #route{auth_policy = {private, [], []}}},
                   Static(?RX, {private, []}))].

% A private route's CA files are checked when it is built
route_private_ca_test_() ->
    {setup,
     fun() -> blizanci_x509:test_tmp_dir() end,
     fun(Dir) -> file:del_dir_r(Dir) end,
     fun(Dir) ->
         Opts = #{ docroot => ?TEST_DOCROOT },
         Static = fun(CAs) ->
                      route(gemini, ?RX, blizanci_static,
                            {private, CAs}, Opts)
                  end,
         CA = blizanci_x509:test_write_pem(
                Dir, "ca.pem",
                [{'Certificate',
                  maps:get(ca, blizanci_x509:test_pki()),
                  not_encrypted}]),
         Missing = filename:join(Dir, "missing.pem"),
         [?_assertMatch({ok, #route{auth_policy = {private, [CA], [_]}}},
                        Static([CA])),
          ?_assertEqual({error, {invalid_ca_file, Missing, enoent}},
                        Static([CA, Missing]))]
     end}.

% A route's realms are resolved when it is built, against the realm table.
route_realm_test_() ->
    {setup,
     fun() -> blizanci_x509:test_tmp_dir() end,
     fun(Dir) -> file:del_dir_r(Dir) end,
     fun(Dir) ->
         Opts = #{ docroot => ?TEST_DOCROOT },
         CA = blizanci_x509:test_write_pem(
                Dir, "ca.pem",
                [{'Certificate',
                  maps:get(ca, blizanci_x509:test_pki()),
                  not_encrypted}]),
         {ok, T0} = add_realm(realms(), <<"staff">>, [CA]),
         {ok, T} = add_realm(T0, <<"none">>, []),
         Staff = {realm, <<"staff">>},
         R = fun(Auth) ->
                 route(gemini, ?RX, blizanci_static, Auth, Opts, T)
             end,
         [?_assertMatch({ok, #route{auth_policy = {private, [Staff], [_]}}},
                        R({private, Staff})),
          ?_assertMatch({ok, #route{auth_policy = {private, [Staff], [_]}}},
                        R({private, [Staff]})),
          ?_assertEqual({error, {unknown_realm, {realm, <<"x">>}, ?RX,
                                 blizanci_static}},
                        R({private, {realm, <<"x">>}})),
          % a realm with no files is allowed
          ?_assertMatch({ok, #route{auth_policy = {private, [_], []}}},
                        R({private, {realm, <<"none">>}})),
          ?_assertEqual({error, {unknown_realm, Staff, ?RX,
                                 blizanci_static}},
                        route(gemini, ?RX, blizanci_static,
                              {private, Staff}, Opts)),
          ?_assertMatch({ok, #route{auth_policy = {private, [CA], [_]}}},
                        R({private, [CA]})),
          ?_assertEqual({error, {duplicate_realm, Staff}},
                        add_realm(T, <<"staff">>, [CA])),
          ?_assertEqual({error, {invalid_realm_name, <<>>}},
                        add_realm(T, <<>>, [CA])),
          ?_assertEqual({ok, Staff}, realm(<<"staff">>)),
          ?_assertEqual(
             "route \"(?<PATH>x)\" (servlet blizanci_static) names realm "
             "<<\"x\">>, which is not defined",
             describe_route_error({unknown_realm, {realm, <<"x">>}, ?RX,
                                   blizanci_static})),
          ?_assertMatch({ok, [#route{auth_policy = {private, [Staff], _}}]},
                        blizanci_router:prepare(
                          [{gemini, ?RX, blizanci_static,
                            {private, Staff}, Opts}], T))]
         ++ new_format_file_list_tests(CA, T, Opts)
     end}.

% New-format routes take realm policies only; old tuples keep the file
% list
new_format_file_list_tests(CA, T, Opts) ->
    Staff = {realm, <<"staff">>},
    Mount = fun(Auth) ->
                {mount, gemini, "/x", blizanci_static, Auth, Opts}
            end,
    Regex = fun(Auth) ->
                {regex, gemini, ?RX, blizanci_static, Auth, Opts}
            end,
    Old = fun(Auth) ->
              {gemini, ?RX, blizanci_static, Auth, Opts}
          end,
    Err = {error, file_list_in_new_route},
    Prep = fun(Routes) -> blizanci_router:prepare(Routes, T) end,
    [?_assertEqual(Err, route(Mount({private, [CA]}), T)),
     ?_assertEqual(Err, route(Mount({private, []}), T)),
     ?_assertEqual(Err, route(Regex({private, [CA]}), T)),
     ?_assertEqual(Err, route(Regex({private, []}), T)),
     ?_assertEqual({error, {bad_route, 2, regex, file_list_in_new_route}},
                   Prep([Mount(public), Regex({private, [CA]})])),
     ?_assertEqual({error, {bad_route, 1, mount, file_list_in_new_route}},
                   Prep([Mount({private, []})])),
     ?_assertEqual(
        "route 2 (regex route): a mount or regex route cannot take a list of "
        "CA files in {private, Files}; define a realm in the realms key "
        "and name it, e.g. {private, {realm, <<\"staff\">>}}",
        describe_route_error({bad_route, 2, regex,
                              file_list_in_new_route})),
     ?_assertEqual(
        "route 2 (mount route): mount path [47,233] is invalid (not plain "
        "ASCII); a mount must consist of printable ASCII characters only",
        describe_route_error({bad_route, 2, mount,
                              {invalid_mount, [$/, 233], non_ascii}})),
     % realm policies are still fine in new-format routes
     ?_assertMatch({ok, #route{auth_policy = {private, [Staff], [_]}}},
                   route(Mount({private, Staff}), T)),
     ?_assertMatch({ok, #route{auth_policy = {private, [Staff], [_]}}},
                   route(Regex({private, [Staff]}), T)),
     % old tuples are unchanged
     ?_assertMatch({ok, #route{auth_policy = {private, [CA], [_]}}},
                   route(Old({private, [CA]}), T)),
     ?_assertMatch({ok, #route{auth_policy = {private, [], []}}},
                   route(Old({private, []}), T)),
     ?_assertMatch({ok, [#route{auth_policy = {private, [CA], [_]}},
                         #route{auth_policy = {private, [], []}}]},
                   Prep([Old({private, [CA]}), Old({private, []})]))].

% realms_from_env/1 in the shape of blizanci_auth:realm_table/1: the
% table, or the first error.
realms_env(App) ->
    case realms_from_env(App) of
        {Table, _, []} -> {ok, Table};
        {_, _, [{_, _, Why} | _]} -> {error, Why};
        {_, _, [{_, Why} | _]} -> {error, Why}
    end.

% Every bad realm is reported, and the good ones still build.
realms_from_env_collects_test_() ->
    {setup,
     fun() -> blizanci_x509:test_tmp_dir() end,
     fun(Dir) ->
         application:unset_env(blizanci_realms_test, realms),
         file:del_dir_r(Dir)
     end,
     fun(Dir) ->
         CA = blizanci_x509:test_write_pem(
                Dir, "ca.pem",
                [{'Certificate',
                  maps:get(ca, blizanci_x509:test_pki()),
                  not_encrypted}]),
         App = blizanci_realms_test,
         Set = fun(Key, Val) -> application:set_env(App, Key, Val) end,
         Missing = filename:join(Dir, "missing.pem"),
         [?_test(begin
                     Set(realms, [{<<"a">>, [Missing]},
                                  {<<"b">>, [CA]},
                                  {<<"c">>, [Missing]}]),
                     {T, Failed, Errors} = realms_from_env(App),
                     ?assertEqual([{realm, <<"b">>}], maps:keys(T)),
                     ?assertEqual([<<"a">>, <<"c">>], Failed),
                     ?assertEqual(
                        [{bad_realm, <<"a">>,
                          {invalid_ca_file, Missing, enoent}},
                         {bad_realm, <<"c">>,
                          {invalid_ca_file, Missing, enoent}}],
                        Errors)
                 end),
          % a definition that is not a pair: no name can be blamed
          ?_test(begin
                     Set(realms, [{<<"b">>, [CA]}, oops]),
                     ?assertMatch(
                        {_, all, [{realms, {invalid_realm, oops}}]},
                        realms_from_env(App))
                 end),
          ?_test(begin
                     Set(realms, nonsense),
                     ?assertMatch(
                        {_, all, [{realms, {invalid_realms, nonsense}}]},
                        realms_from_env(App))
                 end)]
     end}.

% The sys.config key realms.
realms_from_env_test_() ->
    {setup,
     fun() -> blizanci_x509:test_tmp_dir() end,
     fun(Dir) ->
         application:unset_env(blizanci_realms_test, realms),
         file:del_dir_r(Dir)
     end,
     fun(Dir) ->
         CA = blizanci_x509:test_write_pem(
                Dir, "ca.pem",
                [{'Certificate',
                  maps:get(ca, blizanci_x509:test_pki()),
                  not_encrypted}]),
         App = blizanci_realms_test,
         Set = fun(Key, Val) -> application:set_env(App, Key, Val) end,
         Reset = fun() -> application:unset_env(App, realms) end,
         Staff = {realm, <<"staff">>},
         [?_test(begin
                     Reset(),
                     ?assertEqual({ok, #{}}, realms_env(App))
                 end),
          ?_test(begin
                     Reset(),
                     Set(realms, [{<<"staff">>, [CA]}]),
                     {ok, T} = realms_env(App),
                     ?assertEqual([Staff], maps:keys(T))
                 end),
          % a realm named "default" is an ordinary realm
          ?_test(begin
                     Reset(),
                     Set(realms, [{<<"default">>, [CA]}]),
                     {ok, T} = realms_env(App),
                     ?assertEqual([{realm, <<"default">>}], maps:keys(T))
                 end),
          ?_test(begin
                     Reset(),
                     Set(realms, [{<<"staff">>, ["/nonexistent.pem"]}]),
                     ?assertMatch({error, {invalid_ca_file, _, _}},
                                  realms_env(App))
                 end),
          ?_test(begin
                     Reset(),
                     Set(realms, [{staff, [CA]}]),
                     ?assertEqual({error, {invalid_realm_name, staff}},
                                  realms_env(App))
                 end)]
     end}.

route_merges_default_options_test() ->
    {ok, #route{options = Options}} =
        route(gemini, ?RX, blizanci_static, public,
              #{ docroot => ?TEST_DOCROOT }),
    Defaults = blizanci_static:default_options(),
    % the route builder converts the path option to a binary
    ?assertEqual(maps:merge(Defaults,
                            #{ docroot => list_to_binary(?TEST_DOCROOT) }),
                 Options).

route_errors_test_() ->
    Opts = #{ docroot => ?TEST_DOCROOT },
    % some of these arguments deliberately break the spec, so route/5 is
    % called through apply/3 to keep dialyzer from objecting
    BadRoute = fun(Proto, Regex, Module, Authz, RouteOpts) ->
                   apply(blizanci_config, route,
                         [Proto, Regex, Module, Authz, RouteOpts])
               end,
    [?_assertMatch({error, {invalid_proto, gopher}},
                   route(gopher, ?RX, blizanci_static, public, Opts)),
     ?_assertMatch({error, {invalid_regex, "(", _}},
                   route(gemini, "(", blizanci_static, public, Opts)),
     ?_assertMatch({error, {invalid_regex, 42, not_a_string}},
                   BadRoute(gemini, 42, blizanci_static, public, Opts)),
     ?_assertEqual({error, {unknown_servlet, no_such_servlet}},
                   route(gemini, ?RX, no_such_servlet, public, Opts)),
     ?_assertEqual({error, {servlet_missing_callbacks, lists,
                            [{default_options, 0}, {validate_options, 1},
                             {request, 4}, {serve, 4}, {start, 0},
                             {stop, 0}, {cancel, 1},
                             {handle_client_data, 2}]}},
                   route(gemini, ?RX, lists, public, Opts)),
     % a servlet needs request/4 and serve/4 as well as the options
     % callbacks
     ?_assertEqual({error, {servlet_missing_callbacks,
                            blizanci_test_partial, [{serve, 4}]}},
                   route(gemini, ?RX, blizanci_test_partial, public,
                         #{})),
     ?_assertEqual(ok, blizanci_router:check_servlet(blizanci_test_stub)),
     ?_assertEqual({error, {servlet_missing_callbacks,
                            blizanci_test_partial, [{serve, 4}]}},
                   blizanci_router:check_servlet(blizanci_test_partial)),
     ?_assertMatch({ok, #route{}},
                   route(gemini, ?RX, blizanci_test_stub, public, #{})),
     % each callback is checked: a servlet missing exactly one is refused
     ?_assertEqual({error, {servlet_missing_callbacks,
                            blizanci_test_no_start, [{start, 0}]}},
                   blizanci_router:check_servlet(blizanci_test_no_start)),
     ?_assertEqual({error, {servlet_missing_callbacks,
                            blizanci_test_no_stop, [{stop, 0}]}},
                   blizanci_router:check_servlet(blizanci_test_no_stop)),
     ?_assertEqual({error, {servlet_missing_callbacks,
                            blizanci_test_no_cancel, [{cancel, 1}]}},
                   blizanci_router:check_servlet(blizanci_test_no_cancel)),
     ?_assertEqual({error, {servlet_missing_callbacks,
                            blizanci_test_no_hcd,
                            [{handle_client_data, 2}]}},
                   blizanci_router:check_servlet(blizanci_test_no_hcd)),
     ?_assertEqual({error, {not_a_servlet, "blizanci_static"}},
                   BadRoute(gemini, ?RX, "blizanci_static", public, Opts)),
     ?_assertEqual({error, {invalid_authz_policy, bogus}},
                   BadRoute(gemini, ?RX, blizanci_static, bogus, Opts)),
     ?_assertEqual({error, {invalid_route_opts, not_a_map}},
                   BadRoute(gemini, ?RX, blizanci_static, public, not_a_map)),
     ?_assertEqual({error, {invalid_route_options, blizanci_static,
                            {docroot_not_a_directory, <<"/no/such/dir">>}}},
                   route(gemini, ?RX, blizanci_static, public,
                         #{ docroot => "/no/such/dir" }))].

% A directory holding a real certificate and key, for tests of sites
% built with site/3, which refuses a pair that does not match.
pair_fixture(Tests) ->
    {setup,
     fun() ->
         Dir = blizanci_x509:test_tmp_dir(),
         {Dir, blizanci_x509:test_pair_files(Dir, "a", rsa),
          blizanci_x509:test_pair_files(Dir, "b", rsa)}
     end,
     fun({Dir, _, _}) -> file:del_dir_r(Dir) end,
     fun({_, A, B}) -> Tests(A, B) end}.

make_site_test_() ->
    pair_fixture(fun make_site_tests/2).

make_site_tests({Cert, Key}, {_, OtherKey}) ->
    Opts = #{ docroot => ?TEST_DOCROOT },
    Good = {gemini, ?RX, blizanci_static, public, Opts},
    Bad = {gemini, "x", blizanci_static, public, Opts},
    Make = fun(Host, Routes) ->
               make_site(Host, Cert, Key, Routes)
           end,
    [?_assertMatch({ok, #site{routing = [#route{}]}},
                   Make("example.org", [Good])),
     % a route error is returned, not raised as a badmatch
     ?_assertEqual({error, {invalid_routing,
                            {bad_route, 2, tuple,
                             {route_without_path_group, "x",
                              blizanci_static}}}},
                   Make("example.org", [Good, Bad])),
     ?_assertMatch({error, {invalid_routing,
                            {bad_route, 1, tuple,
                             {invalid_proto, gopher}}}},
                   Make("example.org",
                        [{gopher, ?RX, blizanci_static, public, Opts}])),
     ?_assertEqual({error, hostname_not_lowercase},
                   Make("Example.org", [Good])),
     % a certificate that does not match the key is refused
     ?_assertEqual({error, {cert_key_mismatch, <<"example.org">>,
                            Cert, OtherKey}},
                   make_site("example.org", Cert, OtherKey, [Good]))].

describe_titan_work_dir_test() ->
    Work = filename:join(blizanci_x509:test_tmp_dir(), "no-such-dir"),
    {error, Reason} =
        blizanci_router:prepare([{titan, ?RX, blizanci_titan, public,
                                  #{ docroot => ?TEST_DOCROOT,
                                     work_dir => list_to_binary(Work) }}]),
    ?assertEqual({bad_route, 1, tuple,
                  {invalid_route_options, blizanci_titan,
                   {work_dir_not_a_directory, list_to_binary(Work)}}},
                 Reason),
    ?assertEqual("invalid routing table: route 1 (old-format route): "
                 "the Titan route's work_dir "
                 ++ Work ++ " is not a directory; create it, "
                 "or set work_dir in the Titan route's "
                 "options to an existing directory",
                 describe_error({invalid_routing, Reason})).

titan_work_dir_refusals_test_() ->
    Prep = fun(Opts) ->
               blizanci_router:prepare(
                 [{titan, ?RX, blizanci_titan, public,
                   maps:merge(#{ docroot => ?TEST_DOCROOT }, Opts)}])
           end,
    Bad = fun(Why) -> {error, {bad_route, 1, tuple,
                               {invalid_route_options, blizanci_titan,
                                Why}}} end,
    Msg = fun(Why) ->
              describe_error({invalid_routing,
                              {bad_route, 1, tuple,
                               {invalid_route_options, blizanci_titan,
                                Why}}})
          end,
    [?_assertEqual(Bad(work_dir_missing), Prep(#{})),
     ?_assertEqual(Bad({work_dir_not_absolute, <<"rel">>}),
                   Prep(#{ work_dir => "rel" })),
     ?_assertEqual(Bad({work_dir_not_absolute, <<"rel">>}),
                   Prep(#{ work_dir => <<"rel">> })),
     ?_assertEqual(Bad({work_dir_other_filesystem, <<"/proc">>}),
                   Prep(#{ work_dir => "/proc" })),
     ?_assertMatch({ok, _}, Prep(#{ work_dir => test_tmp() })),
     ?_assertEqual("invalid routing table: route 1 (old-format route): "
                   "the Titan route has no work_dir; set work_dir in "
                   "its options to an existing absolute directory on the "
                   "docroot's filesystem", Msg(work_dir_missing)),
     ?_assertEqual("invalid routing table: route 1 (old-format route): "
                   "the Titan route's work_dir rel is not an absolute "
                   "path; set it to an existing "
                   "absolute directory",
                   Msg({work_dir_not_absolute, "rel"})),
     ?_assertEqual("invalid routing table: route 1 (old-format route): "
                   "the Titan route's work_dir /proc is on a different "
                   "filesystem from its "
                   "docroot; uploads are installed by hard link and "
                   "rename, so use a directory on the docroot's "
                   "filesystem",
                   Msg({work_dir_other_filesystem, "/proc"}))].

% An existing directory on the same filesystem as the test docroot.
test_tmp() ->
    filename:absname(?TEST_DOCROOT).

% A docroot fixture (Base/docroot/{hello.gmi, sub/index.gemini,
% sub/x.gmi, private/x.gmi, private/sub/index.gemini, restricted/x.gmi,
% privatefoo/x.gmi}, Base/cgi).
table_teardown({Mime, Base}) ->
    file:del_dir_r(Base),
    blizanci_test_support:mime_down(Mime).

table_setup() ->
    Mime = blizanci_test_support:mime_up(),
    Base = blizanci_test_support:tmp_path("mounts_table_test"),
    Docroot = filename:join(Base, "docroot"),
    [ ok = filelib:ensure_path(filename:join(Docroot, D))
      || D <- ["sub", "private/sub", "restricted", "privatefoo"] ],
    ok = filelib:ensure_path(filename:join(Base, "cgi")),
    [ ok = file:write_file(filename:join(Docroot, F), <<"x">>)
      || F <- ["hello.gmi", "sub/index.gemini", "sub/x.gmi",
               "private/x.gmi", "private/sub/index.gemini",
               "restricted/x.gmi", "privatefoo/x.gmi"] ],
    {Mime, Base}.

% A routing table with /private and /restricted mounts over
% subdirectories of the docroot, beside /cgi-bin and /.
mounts_table_test_() ->
    {setup,
     fun table_setup/0,
     fun table_teardown/1,
     fun mounts_table_tests/1}.

mounts_table(Docroot, Cgi, Private) ->
    [{mount, gemini, "/cgi-bin", blizanci_cgi, public,
      #{ cgiroot => Cgi }},
     {mount, gemini, "/private", blizanci_static, Private,
      #{ docroot => filename:join(Docroot, "private") }},
     {mount, gemini, "/restricted", blizanci_static, restricted,
      #{ docroot => filename:join(Docroot, "restricted") }},
     {mount, gemini, "/", blizanci_static, public,
      #{ docroot => Docroot }}].

mounts_table_tests({_Mime, Base}) ->
    Docroot = filename:join(Base, "docroot"),
    Cgi = filename:join(Base, "cgi"),
    Full = fun(Rel) ->
               {file, <<"text/gemini">>,
                list_to_binary(filename:absname(filename:join(Docroot,
                                                               Rel)))}
           end,
    Realm = {private, {realm, <<"staff">>}},
    Table = mounts_table(Docroot, Cgi, Realm),
    [?_test(mounts_table_serves(Full, Table, Docroot)),
     ?_test(mounts_table_with_cert(Docroot, Full, Table)),
     ?_test(mounts_table_empty_realm(Docroot, Cgi))].

% What the table does, through the request handler, with no client
% certificate: private and restricted ask for one (status 60); the rest
% is served, and a directory is redirected to its index, with the mount
% in the path.
mounts_table_serves(Full, Table, Docroot) ->
    {ok, Routing} = blizanci_router:prepare(
                      Table, mounts_realms(Docroot,
                                           blizanci_x509:test_pki())),
    H = fun(Path) -> table_get(Routing, Path) end,
    NotFound = {error_code, file_not_found},
    ?assertEqual(Full("hello.gmi"), H(<<"hello.gmi">>)),
    % "private" is a whole segment
    ?assertEqual(Full("privatefoo/x.gmi"), H(<<"privatefoo/x.gmi">>)),
    ?assertEqual(NotFound, H(<<"privatefoo/nothere">>)),
    ?assertEqual({redirect, <<"privatefoo/index.gemini">>},
                 H(<<"privatefoo">>)),
    Need = {error_code, cert_required},
    [ ?assertEqual(Need, H(P))
      || P <- [<<"private/x.gmi">>, <<"restricted/x.gmi">>,
               <<"private">>, <<"private/">>, <<"private/nothere">>,
               <<"restricted">>] ],
    ?assertEqual(NotFound, H(<<"nothere.gmi">>)).

% With the client certificate of the CA behind realm "staff", private
% serves <docroot>/private, and a certificate from any CA suffices for
% restricted; redirects keep the mount.
mounts_table_with_cert(Docroot, Full, Table) ->
    Pki = blizanci_x509:test_pki(),
    {ok, Routing} = blizanci_router:prepare(
                      Table, mounts_realms(Docroot, Pki)),
    #{ peer := Peer } = Pki,
    H = fun(Path) -> table_get(Routing, {ok, Peer}, Path) end,
    ?assertEqual(Full("private/x.gmi"), H(<<"private/x.gmi">>)),
    ?assertEqual(Full("restricted/x.gmi"), H(<<"restricted/x.gmi">>)),
    ?assertEqual({redirect, <<"private/index.gemini">>}, H(<<"private">>)),
    ?assertEqual({redirect, <<"private/index.gemini">>},
                 H(<<"private/">>)),
    ?assertEqual({redirect, <<"private/sub/index.gemini">>},
                 H(<<"private/sub">>)),
    ?assertEqual({redirect, <<"private/sub/index.gemini">>},
                 H(<<"private/sub/">>)),
    ?assertEqual({redirect, <<"restricted/index.gemini">>},
                 H(<<"restricted/">>)),
    ?assertEqual({error_code, file_not_found}, H(<<"private/nothere">>)).

% A private mount on a realm with no CA files builds and prepares, and
% authorises nobody, even a certificate (rather than falling through to
% the public mount).
mounts_table_empty_realm(Docroot, Cgi) ->
    {ok, Realms} = realms([{<<"staff">>, []}]),
    {ok, Routing} = blizanci_router:prepare(
                      mounts_table(Docroot, Cgi,
                                   {private, {realm, <<"staff">>}}),
                      Realms),
    #{ peer := Peer } = blizanci_x509:test_pki(),
    NotFound = {error_code, file_not_found},
    ?assertEqual({error_code, cert_not_authorised},
                 table_get(Routing, {ok, Peer},
                                   <<"private/x.gmi">>)),
    ?assertEqual({error_code, cert_required},
                 table_get(Routing, {error, no_peercert},
                                   <<"private/x.gmi">>)),
    ?assertNotEqual(NotFound, table_get(
                                Routing, {ok, Peer}, <<"private">>)).

mounts_realms(Docroot, Pki) ->
    CA = blizanci_x509:test_write_pem(
           filename:dirname(Docroot), "default-ca.pem",
           [{'Certificate', maps:get(ca, Pki),
             not_encrypted}]),
    {ok, T} = realms([{<<"staff">>, [CA]}]),
    T.

table_get(Routing, Path) ->
    table_get(Routing, {error, no_peercert}, Path).

table_get(Routing, Cert, Path) ->
    Config = #server_config{hostname = <<"h">>, port = 1965,
                            routing = Routing},
    blizanci_request:handle_line(<<"gemini://h/", Path/binary>>, Config,
                                 Cert, <<>>).

describe_error_test_() ->
    [?_assertEqual("invalid routing table: route \"x\" (servlet "
                   "blizanci_static) has no named group PATH in its "
                   "regex; add one, e.g. (?<PATH>.*)",
                   describe_error(
                     {invalid_routing,
                      {route_without_path_group, "x", blizanci_static}})),
     ?_assertEqual("invalid routing table: a private route's CA file "
                   "/x/ca.pem cannot be used (enoent); check that it "
                   "exists and holds a PEM certificate",
                   describe_error(
                     {invalid_routing,
                      {invalid_ca_file, "/x/ca.pem", enoent}})),
     ?_assertEqual("invalid routing table: route for servlet "
                   "blizanci_static has invalid options: "
                   "{unknown_option,1}",
                   describe_error(
                     {invalid_routing,
                      {invalid_route_options, blizanci_static,
                       {unknown_option, 1}}})),
     ?_assertEqual("invalid routing table: the route's docroot /nope is "
                   "not a directory; create it, or set docroot to an "
                   "existing directory",
                   describe_error(
                     {invalid_routing,
                      {invalid_route_options, blizanci_static,
                       {docroot_not_a_directory, <<"/nope">>}}})),
     ?_assertEqual("invalid routing table: the route's cgiroot /nope is "
                   "not a directory; create it, or set cgiroot to an "
                   "existing directory",
                   describe_error(
                     {invalid_routing,
                      {invalid_route_options, blizanci_cgi,
                       {cgiroot_not_a_directory, <<"/nope">>}}})),
     ?_assertEqual("servlet blizanci_cgi could not start: "
                   "{invalid_limit,0}",
                   describe_error(
                     {shutdown,
                      {failed_to_start_child, blizanci_servlets,
                       {servlet_start_failed, blizanci_cgi,
                        {invalid_limit, 0}}}})),
     ?_assertEqual("servlet blizanci_titan could not start: "
                   "{invalid_limit,\"5\"}",
                   describe_error(
                     {servlet_start_failed, blizanci_titan,
                      {invalid_limit, "5"}})),
     ?_assertEqual("servlet blizanci_cgi could not start: "
                   "{invalid_module,5}",
                   describe_error(
                     {servlet_start_failed, blizanci_cgi,
                      {invalid_module, 5}}))].

global_test() ->
    ?assertEqual(#global{port = 1965, servlets = [blizanci_static]},
                 global(1965, [blizanci_static])).

site_test() ->
    ?assertEqual(#site{hostname = <<"example.org">>,
                       certfile = "c.pem",
                       keyfile  = "k.pem",
                       routing  = []},
                 test_site("example.org", "c.pem", "k.pem")).

site_rejects_uppercase_test_() ->
    pair_fixture(fun({Cert, Key}, _) ->
        [?_assertEqual({error, hostname_not_lowercase},
                       site("Example.org", Cert, Key)),
         ?_assertEqual({error, hostname_not_lowercase},
                       site("EXAMPLE.ORG", Cert, Key)),
         ?_assertMatch({ok, #site{hostname = <<"example.org">>}},
                       site("example.org", Cert, Key))]
    end).

% site/3 refuses a certificate and key that are not a pair, naming the
% site and both files.
site_cert_key_pair_test_() ->
    pair_fixture(fun({Cert, Key}, {OtherCert, OtherKey}) ->
        Missing = filename:join(filename:dirname(Cert), "missing.pem"),
        [?_assertMatch({ok, #site{certfile = Cert, keyfile = Key}},
                       site("a.example", Cert, Key)),
         ?_assertEqual({error, {cert_key_mismatch, <<"a.example">>,
                                Cert, OtherKey}},
                       site("a.example", Cert, OtherKey)),
         ?_assertEqual({error, {cert_key_mismatch, <<"b.example">>,
                                OtherCert, Key}},
                       site("b.example", OtherCert, Key)),
         ?_assertEqual({error, {bad_certfile, <<"a.example">>,
                                {unreadable, enoent}}},
                       site("a.example", Missing, Key)),
         ?_assertEqual({error, {bad_keyfile, <<"a.example">>,
                                no_private_key}},
                       site("a.example", Cert, Cert)),
         ?_assertEqual("site a.example: certificate " ++ Cert
                       ++ " and private key " ++ OtherKey
                       ++ " are not a matching pair",
                       describe_error(
                         {cert_key_mismatch, <<"a.example">>,
                          Cert, OtherKey}))]
    end).

% A pair the check cannot compare (DSA, a curve it does not know) is
% accepted with exactly one warning, from site/3; validating the built
% site and applying an unchanged configuration say nothing more.
site_unverified_pair_test_() ->
    {setup,
     fun() ->
         Dir = blizanci_x509:test_tmp_dir(),
         {Dir, [ {K, blizanci_x509:test_pair_files(Dir, atom_to_list(K),
                                                   K)}
                 || K <- [dsa, p224, rsa] ]}
     end,
     fun({Dir, _}) -> file:del_dir_r(Dir) end,
     fun({_, Pairs}) ->
         {DsaCert, DsaKey} = proplists:get_value(dsa, Pairs),
         {EcCert, EcKey} = proplists:get_value(p224, Pairs),
         {RsaCert, _} = proplists:get_value(rsa, Pairs),
         Warnings =
             fun(Fun) ->
                 Id = unverified_pair_log_handler,
                 ok = logger:add_handler(Id, blizanci_test_log_handler,
                                         #{config =>
                                           #{notify => self()}}),
                 try Fun(), drain_warnings()
                 after ok = logger:remove_handler(Id)
                 end
             end,
         Text = fun(Cert, Key, Type) ->
                    "site a.example: cannot verify that certificate "
                    ++ Cert ++ " and private key " ++ Key
                    ++ " are a pair (unsupported key type " ++ Type ++ ")"
                end,
         [?_assertEqual([Text(DsaCert, DsaKey, "DSA")],
                        Warnings(fun() ->
                            {ok, #site{}} = site("a.example", DsaCert,
                                                 DsaKey)
                        end)),
          ?_assertEqual([Text(EcCert, EcKey, "EC secp224r1")],
                        Warnings(fun() ->
                            {ok, #site{}} = site("a.example", EcCert,
                                                 EcKey)
                        end)),
          % an RSA certificate with a DSA key is a mismatch, not a warning
          ?_assertEqual({[], {error, {cert_key_mismatch, <<"a.example">>,
                                      RsaCert, DsaKey}}},
                        begin
                            Ref = make_ref(),
                            W = Warnings(fun() ->
                                put(Ref, site("a.example", RsaCert,
                                              DsaKey))
                            end),
                            {W, erase(Ref)}
                        end),
          % the start-up path builds the site once, so warns once
          ?_assertEqual([Text(DsaCert, DsaKey, "DSA")],
                        Warnings(fun() ->
                            {ok, _} = make_site("a.example", DsaCert,
                                                DsaKey, [])
                        end)),
          % validating a site built without site/3, silent
          ?_assertEqual({[], []},
                        begin
                            Ref = make_ref(),
                            W = Warnings(fun() ->
                                put(Ref, problems_site(
                                           test_site("a.example",
                                                     DsaCert, DsaKey)))
                            end),
                            {W, erase(Ref)}
                        end)]
     end}.

drain_warnings() ->
    receive {logged, warning, T} -> [T | drain_warnings()];
            {logged, _, _} -> drain_warnings()
    after 0 -> []
    end.

ascii_lowercase_test_() ->
    [?_assertEqual("example.org", ascii_lowercase("Example.ORG")),
     ?_assertEqual("a-1.b", ascii_lowercase("A-1.B")),
     % only A-Z: the Kelvin sign is left alone, not folded to "k"
     ?_assertEqual([16#212A], ascii_lowercase([16#212A])),
     ?_assertEqual(<<"example.org">>, ascii_lowercase(<<"Example.ORG">>)),
     ?_assertEqual(<<"a-1.b">>, ascii_lowercase(<<"A-1.B">>)),
     ?_assertEqual(<<>>, ascii_lowercase(<<>>)),
     ?_assertEqual(<<226,132,170>>, ascii_lowercase(<<226,132,170>>)),
     ?_assertEqual(<<"a", 226,132,170, "b">>,
                   ascii_lowercase(<<"A", 226,132,170, "B">>))].

add_route_appends_test() ->
    Opts = #{ docroot => ?TEST_DOCROOT },
    {ok, R1} = route(gemini, "(?<PATH>a)", blizanci_static, public, Opts),
    {ok, R2} = route(gemini, "(?<PATH>b)", blizanci_static, restricted, Opts),
    Site0 = test_site("example.org", "c.pem", "k.pem"),
    Site1 = add_route(Site0, R1),
    Site2 = add_route(Site1, R2),
    ?assertEqual([R1], Site1#site.routing),
    ?assertEqual([R1, R2], Site2#site.routing),
    ?assertEqual([], Site0#site.routing).

add_route_rejects_non_route_test() ->
    Site = test_site("example.org", "c.pem", "k.pem"),
    % (through apply/3, as the argument deliberately breaks the spec)
    ?assertError(function_clause,
                 apply(blizanci_config, add_route, [Site, {not_a_route}])).

find_site_test_() ->
    A = test_site("a.example", "a.pem", "a.key"),
    B = test_site("b.example", "b.pem", "b.key"),
    [?_assertEqual({ok, A}, find_site(<<"a.example">>, [A, B])),
     ?_assertEqual({ok, B}, find_site(<<"b.example">>, [A, B])),
     ?_assertEqual(not_found, find_site(<<"c.example">>, [A, B])),
     ?_assertEqual({ok, A}, find_site(<<"A.EXAMPLE">>, [A, B])),
     ?_assertEqual({ok, B}, find_site(<<"B.Example">>, [A, B])),
     ?_assertEqual(not_found, find_site(<<"a.example.">>, [A, B])),
     ?_assertEqual(not_found, find_site(<<"a.example">>, []))].

make_test() ->
    Site = test_site("a.example", "a.pem", "a.key"),
    Config = make(1966, [blizanci_static], [Site]),
    ?assertEqual([blizanci_static], maps:get(servlets, Config)),
    SSL = maps:get(ssl_opts, Config),
    Proto = maps:get(proto_opts, Config),
    ?assertEqual(1966, proplists:get_value(port, SSL)),
    ?assertEqual([{port, 1966}], Proto),
    ?assertEqual(1966, maps:get(port, Config)),
    ?assertEqual([Site], maps:get(sites, Config)).

% SNI is the only source of TLS credentials: no top-level cert/key, and
% the sni_fun serves exactly the configured sites.
sni_fun_test_() ->
    A = test_site("a.example", "a.pem", "a.key"),
    B = test_site("b.example", "b.pem", "b.key"),
    SSL = ssl_opts(1965),
    SniFun = proplists:get_value(sni_fun, SSL),
    {setup,
     fun() -> live_up(1965, [], [A, B]) end,
     fun(_) -> blizanci_test_support:live_down() end,
     [?_assertEqual([{certfile, "a.pem"}, {keyfile, "a.key"},
                     {verify, verify_none}],
                    SniFun("a.example")),
      ?_assertEqual([{certfile, "b.pem"}, {keyfile, "b.key"},
                     {verify, verify_none}],
                    SniFun("b.example")),
      ?_assertEqual(unrecognized, SniFun("evil.example")),
      % a name that isn't ours is refused whatever its case
      ?_assertEqual(unrecognized, SniFun("EVIL.EXAMPLE")),
      % and a name with a trailing dot (not valid in an SNI) is refused
      ?_assertEqual(unrecognized, SniFun("a.example.")),
      ?_assertEqual(unrecognized, SniFun("A.EXAMPLE.")),
      ?_assertEqual(undefined, proplists:get_value(certfile, SSL)),
      ?_assertEqual(undefined, proplists:get_value(keyfile, SSL)),
      ?_assertEqual(['tlsv1.3', 'tlsv1.2'],
                    proplists:get_value(versions, SSL)),
      ?_assertEqual(['tlsv1.3', 'tlsv1.2'], tls_versions()),
      % the same sni_fun sees a replaced site list: this is what lets
      % apply/1 change certificates on a running listener
      ?_test(begin
                 ok = blizanci_conf_serv:publish(1965, [], [B]),
                 ?assertEqual(unrecognized, SniFun("a.example")),
                 ?assertEqual([{certfile, "b.pem"}, {keyfile, "b.key"},
                     {verify, verify_none}],
                              SniFun("b.example"))
             end)]}.

%% Whether a site asks for a client certificate follows from its routes.
site_wants_client_cert_test_() ->
    {setup,
     fun() -> blizanci_x509:test_tmp_dir() end,
     fun(Dir) -> file:del_dir_r(Dir) end,
     fun(Dir) ->
         #{ca := CA} = blizanci_x509:test_pki(),
         CAFile = blizanci_x509:test_write_pem(
                    Dir, "ca.pem", [{'Certificate', CA, not_encrypted}]),
         Opts = #{ docroot => ?TEST_DOCROOT,
                   work_dir => filename:absname(?TEST_DOCROOT) },
         R = fun(Proto, Mod, Auth) ->
                 {ok, Route} = route(Proto, ?RX, Mod, Auth, Opts),
                 Route
             end,
         Pub = R(gemini, blizanci_static, public),
         Res = R(gemini, blizanci_static, restricted),
         Priv = R(gemini, blizanci_static, {private, [CAFile]}),
         TitanPub = R(titan, blizanci_titan, public),
         TitanPriv = R(titan, blizanci_titan, {private, [CAFile]}),
         TitanRes = R(titan, blizanci_titan, restricted),
         [?_assertMatch(#route{auth_policy = {private, [_], [_]}}, Priv),
          ?_assertNot(site_wants_client_cert([])),
          ?_assertNot(site_wants_client_cert([Pub])),
          ?_assertNot(site_wants_client_cert([Pub, TitanPub, Pub])),
          ?_assert(site_wants_client_cert([Res])),
          ?_assert(site_wants_client_cert([Priv])),
          ?_assert(site_wants_client_cert([Pub, Res, Pub])),
          ?_assert(site_wants_client_cert([Pub, Priv])),
          ?_assert(site_wants_client_cert([Priv, Res])),
          ?_assert(site_wants_client_cert([Pub, TitanPriv])),
          ?_assert(site_wants_client_cert([TitanRes])),
          ?_assert(site_wants_client_cert([TitanPub, Pub, TitanPriv]))]
     end}.

%% The sni_fun result follows the live site, and so follows apply/1.
sni_fun_client_cert_test_() ->
    {setup,
     fun() -> {write_test_pem(), blizanci_x509:test_tmp_dir()} end,
     fun({Pem, Dir}) ->
         blizanci_test_support:live_down(),
         file:delete(Pem),
         file:del_dir_r(Dir)
     end,
     fun({Pem, Dir}) ->
         ?_test(begin
             #{ca := CA} = blizanci_x509:test_pki(),
             CAFile = blizanci_x509:test_write_pem(
                        Dir, "ca.pem", [{'Certificate', CA, not_encrypted}]),
             Opts = #{ docroot => ?TEST_DOCROOT },
             Site = fun(Auth) ->
                 {ok, Route} = route(gemini, ?RX, blizanci_static,
                                     Auth, Opts),
                 add_route(test_site("a.example", Pem, Pem), Route)
             end,
             Conf = fun(Auth) ->
                 new(global(1965, [blizanci_static]), [Site(Auth)])
             end,
             SniFun = proplists:get_value(sni_fun, ssl_opts(1965)),
             Creds = [{certfile, Pem}, {keyfile, Pem}],
             live_up(1965, [blizanci_static], [Site(public)]),
             ?assertEqual(Creds ++ [{verify, verify_none}],
                          SniFun("a.example")),
             ?assertEqual(ok, apply(Conf({private, [CAFile]}))),
             ?assertEqual(Creds, SniFun("a.example")),
             ?assertEqual(ok, apply(Conf(restricted))),
             ?assertEqual(Creds, SniFun("a.example")),
             ?assertEqual(ok, apply(Conf(public))),
             ?assertEqual(Creds ++ [{verify, verify_none}],
                          SniFun("a.example"))
         end)
     end}.

%% A host name is case-insensitive, so the same site is found whatever
%% the case of the name the client sent in its SNI.
sni_fun_case_insensitive_test_() ->
    A = test_site("a.example", "a.pem", "a.key"),
    B = test_site("b.example", "b.pem", "b.key"),
    SniFun = proplists:get_value(sni_fun, ssl_opts(1965)),
    {setup,
     fun() -> live_up(1965, [], [A, B]) end,
     fun(_) -> blizanci_test_support:live_down() end,
     [?_assertEqual([{certfile, "a.pem"}, {keyfile, "a.key"},
                     {verify, verify_none}],
                    SniFun("A.EXAMPLE")),
      ?_assertEqual([{certfile, "a.pem"}, {keyfile, "a.key"},
                     {verify, verify_none}],
                    SniFun("A.Example")),
      ?_assertEqual([{certfile, "b.pem"}, {keyfile, "b.key"},
                     {verify, verify_none}],
                    SniFun("B.example"))]}.

%% Builder: routes and configuration.

insert_route_test_() ->
    Opts = #{ docroot => ?TEST_DOCROOT },
    {ok, R1} = route(gemini, "(?<PATH>a)", blizanci_static, public, Opts),
    {ok, R2} = route(gemini, "(?<PATH>b)", blizanci_static, public, Opts),
    {ok, R3} = route(gemini, "(?<PATH>c)", blizanci_static, public, Opts),
    Site = add_route(add_route(test_site("h", "c.pem", "k.pem"), R1), R2),
    [?_assertEqual({ok, [R3, R1, R2]},
                   rmap(insert_route(Site, 1, R3))),
     ?_assertEqual({ok, [R1, R3, R2]},
                   rmap(insert_route(Site, 2, R3))),
     % past the end is an error, not an append
     ?_assertEqual({error, {index_out_of_range, 3, 2}},
                   insert_route(Site, 3, R3)),
     ?_assertEqual({error, {index_out_of_range, 0, 2}},
                   apply(blizanci_config, insert_route, [Site, 0, R3])),
     ?_assertEqual({error, {index_out_of_range, 1, 0}},
                   insert_route(test_site("h", "c.pem", "k.pem"), 1, R3)),
     ?_assertEqual([R1, R2], routes(Site))].

rmap({ok, Site}) -> {ok, routes(Site)};
rmap(Error) -> Error.

new_test() ->
    G = global(1965, [blizanci_static]),
    S = test_site("a.example", "c.pem", "k.pem"),
    ?assertEqual(#config{global = G, sites = [S]}, new(G, [S])).

%% validate/1. Uses a real PEM file so the cert/key checks can pass.
validate_test_() ->
    {setup, fun write_test_pem/0, fun file:delete/1, fun validate_tests/1}.

validate_tests(Pem) ->
    {ok, R} = route(gemini, "(?<PATH>.*)", blizanci_static, public,
                    #{ docroot => ?TEST_DOCROOT }),
    Good = fun(Host) -> add_route(test_site(Host, Pem, Pem), R) end,
    G = global(1965, [blizanci_static]),
    V = fun(Gl, Sites) -> validate(new(Gl, Sites)) end,
    [?_assertEqual(ok, V(G, [Good("a"), Good("b")])),
     ?_assertEqual({error, {invalid_port, 0}},
                   V(global(0, [blizanci_static]), [Good("a")])),
     ?_assertEqual({error, {invalid_port, <<"x">>}},
                   V(global(<<"x">>, [blizanci_static]), [Good("a")])),
     ?_assertEqual({error, {unknown_servlet, nope}},
                   V(global(1965, [nope]), [Good("a")])),
     ?_assertEqual({error, no_sites}, V(G, [])),
     ?_assertEqual({error, {duplicate_site, <<"a">>}},
                   V(G, [Good("a"), Good("a")])),
     % a site with no routes is valid
     ?_assertEqual(ok, V(G, [test_site("a", Pem, Pem)])),
     ?_assertEqual({error, {bad_certfile, <<"a">>, couldnt_open_pem_file}},
                   V(G, [add_route(test_site("a", "/no/such.pem", Pem), R)])),
     ?_assertEqual({error, {bad_keyfile, <<"a">>, couldnt_open_pem_file}},
                   V(G, [add_route(test_site("a", Pem, "/no/such.pem"), R)])),
     ?_assertEqual({error, {undeclared_servlet, <<"a">>, blizanci_static}},
                   V(global(1965, []), [Good("a")]))].

% validate_all/1 reports every problem, in a stable order, and
% validate/1 the first of them.
validate_all_test_() ->
    {setup, fun write_test_pem/0, fun file:delete/1,
     fun validate_all_tests/1}.

validate_all_tests(Pem) ->
    {ok, R} = route(gemini, "(?<PATH>.*)", blizanci_static, public,
                    #{ docroot => ?TEST_DOCROOT }),
    Bad = add_route(test_site("a", "/no/such.pem", "/no/such.key"), R),
    Good = add_route(test_site("b", Pem, Pem), R),
    G = global(0, [nope]),
    Config = new(G, [Bad, Good]),
    All = [{invalid_port, 0},
           {unknown_servlet, nope},
           {bad_certfile, <<"a">>, couldnt_open_pem_file},
           {bad_keyfile, <<"a">>, couldnt_open_pem_file},
           {undeclared_servlet, <<"a">>, blizanci_static},
           {undeclared_servlet, <<"b">>, blizanci_static}],
    [?_assertEqual({error, All}, validate_all(Config)),
     ?_assertEqual({error, hd(All)}, validate(Config)),
     ?_assertEqual(ok, validate_all(new(global(1965, [blizanci_static]),
                                        [Good]))),
     ?_assertEqual({error, [no_sites]},
                   validate_all(new(global(1965, []), []))),
     ?_assertEqual({error, [{duplicate_site, <<"b">>}]},
                   validate_all(new(global(1965, [blizanci_static]),
                                    [Good, Good]))),
     ?_assertEqual(
        ["site a: the certificate file is not a readable PEM file "
         "(couldnt_open_pem_file)",
         "site a: a route uses servlet blizanci_static, which is not in "
         "the servlets list",
         "port 0 is not a TCP port number (1 to 65535)"],
        [ describe_error(E)
          || E <- [lists:nth(3, All), lists:nth(5, All), hd(All)] ]),
     ?_assertEqual(
        "site a.example: hostname is not lower case; realm <<\"r\">>: "
        "CA file /x cannot be used (enoent); check that it exists and "
        "holds a PEM certificate",
        describe_error(
          {invalid_configuration,
           [{site, "a.example", hostname_not_lowercase},
            {bad_realm, <<"r">>, {invalid_ca_file, "/x", enoent}}]}))].

write_test_pem() ->
    Path = blizanci_test_support:tmp_path("validate_test") ++ ".pem",
    % A real certificate and its key in one file, so that the pair
    % check passes when the file is used as both.
    Dir = blizanci_x509:test_tmp_dir(),
    {Cert, Key} = blizanci_x509:test_pair_files(Dir, "p", rsa),
    {ok, CertPem} = file:read_file(Cert),
    {ok, KeyPem} = file:read_file(Key),
    ok = file:write_file(Path, [CertPem, KeyPem]),
    ok = file:del_dir_r(Dir),
    Path.

%% apply/1.

%% Stand up the live configuration, as blizanci_sup would.
live_up(Port, Servlets, Sites) ->
    blizanci_test_support:live_up(Port, Servlets, Sites).

apply_test_() ->
    {setup, fun write_test_pem/0,
     fun(Pem) -> file:delete(Pem), blizanci_test_support:live_down() end,
     fun apply_tests/1}.

apply_tests(Pem) ->
    {ok, R} = route(gemini, "(?<PATH>.*)", blizanci_static, public,
                    #{ docroot => ?TEST_DOCROOT }),
    Site = fun(Host) -> add_route(test_site(Host, Pem, Pem), R) end,
    Conf = fun(Port, Servlets, Sites) ->
               new(global(Port, Servlets), Sites)
           end,
    Reset = fun() -> blizanci_test_support:live_down() end,
    [?_test(begin
                Reset(),
                ?assertEqual({error, not_running},
                             apply(Conf(1965, [blizanci_static],
                                        [Site("a")])))
            end),
     ?_test(begin
                Reset(),
                live_up(1965, [blizanci_static], [Site("a")]),
                ?assertEqual({error, no_sites},
                             apply(Conf(1965, [blizanci_static], []))),
                % an invalid configuration changes nothing
                ?assertEqual([Site("a")], blizanci_conf_serv:sites())
            end),
     ?_test(begin
                Reset(),
                live_up(1965, [blizanci_static], [Site("a")]),
                ?assertEqual(ok, apply(Conf(1965, [blizanci_static],
                                            [Site("b"), Site("c")]))),
                Find = fun(Host) -> blizanci_conf_serv:find_site(Host) end,
                ?assertEqual(not_found, Find(<<"a">>)),
                ?assertMatch({ok, _}, Find(<<"b">>)),
                ?assertMatch({ok, _}, Find(<<"c">>))
            end),
     ?_test(begin
                Reset(),
                live_up(1965, [blizanci_static], [Site("a")]),
                ?assertEqual({error, {port_change_requires_restart,
                                      1965, 1966}},
                             apply(Conf(1966, [blizanci_static],
                                        [Site("b")]))),
                ?assertEqual({ok, #{port => 1965,
                                    servlets => [blizanci_static],
                                    sites => [Site("a")]}},
                             blizanci_conf_serv:current())
            end)].

%% An apply/1 whose site has a certificate and key that are not a pair
%% is refused, and the configuration that was serving keeps serving. The
%% bad site is one rebuilt from a key file replaced since site/3 passed
%% it, and one built without the check, as a record.
apply_cert_key_mismatch_test_() ->
    pair_fixture(
      fun({Cert, Key}, {_, OtherKey}) ->
          {ok, R} = route(gemini, "(?<PATH>.*)", blizanci_static, public,
                          #{ docroot => ?TEST_DOCROOT }),
          Conf = fun(Sites) ->
                     new(global(1965, [blizanci_static]), Sites)
                 end,
          {setup,
           fun() ->
               {ok, Good} = site("a.example", Cert, Key),
               Old = add_route(Good, R),
               live_up(1965, [blizanci_static], [Old]),
               Old
           end,
           fun(_) -> blizanci_test_support:live_down() end,
           fun(Old) ->
               Bad = add_route(test_site("a.example", Cert, OtherKey), R),
               Expected = {error, {cert_key_mismatch, <<"a.example">>,
                                   Cert, OtherKey}},
               Unchanged = fun() ->
                   ?assertEqual({ok, #{port => 1965,
                                       servlets => [blizanci_static],
                                       sites => [Old]}},
                                blizanci_conf_serv:current()),
                   ?assertEqual({ok, Old},
                                blizanci_conf_serv:find_site(
                                  <<"a.example">>))
               end,
               [?_test(begin
                   ?assertEqual(Expected, apply(Conf([Bad]))),
                   Unchanged()
                end),
                % one bad site among good ones refuses the whole lot
                ?_test(begin
                    Other = add_route(test_site("b.example", Cert, Key), R),
                    ?assertEqual(Expected, apply(Conf([Other, Bad]))),
                    Unchanged()
                end),
                ?_test(begin
                    ?assertEqual(ok, apply(Conf([Old]))),
                    Unchanged()
                end)]
           end}
      end).

%% A private route holds the CA certificates decoded when it was built:
%% a CA file changed afterwards takes effect only when a rebuilt route is
%% applied.
apply_ca_file_test_() ->
    {setup,
     fun() -> {write_test_pem(), blizanci_x509:test_tmp_dir()} end,
     fun({Pem, Dir}) ->
         blizanci_test_support:live_down(),
         file:delete(Pem),
         file:del_dir_r(Dir)
     end,
     fun({Pem, Dir}) ->
         ?_test(begin
             #{ca := CA, ca2 := CA2, peer := Peer, peer_ca2 := PeerCA2} =
                 blizanci_x509:test_pki(),
             CAFile = blizanci_x509:test_write_pem(
                        Dir, "ca.pem", [{'Certificate', CA, not_encrypted}]),
             Build = fun() ->
                 {ok, R} = route(gemini, "(?<PATH>.*)", blizanci_static,
                                 {private, [CAFile]},
                                 #{ docroot => ?TEST_DOCROOT }),
                 add_route(test_site("a", Pem, Pem), R)
             end,
             Live = fun(Cert) ->
                 {ok, #site{routing = [#route{auth_policy = P}]}} =
                     blizanci_conf_serv:find_site(<<"a">>),
                 blizanci_auth:authorised(
                   P, blizanci_test_support:test_request(
                        #{ client_cert => {ok, Cert} }))
             end,
             live_up(1965, [blizanci_static], [Build()]),
             ?assertEqual(authorised, Live(Peer)),
             ?assertEqual({error_code, cert_not_authorised}, Live(PeerCA2)),
             % rewrite the file: nothing changes until it is applied
             blizanci_x509:test_write_pem(
               Dir, "ca.pem", [{'Certificate', CA2, not_encrypted}]),
             ?assertEqual(authorised, Live(Peer)),
             ?assertEqual({error_code, cert_not_authorised}, Live(PeerCA2)),
             ?assertEqual(ok, apply(new(global(1965, [blizanci_static]),
                                        [Build()]))),
             ?assertEqual({error_code, cert_not_authorised}, Live(Peer)),
             ?assertEqual(authorised, Live(PeerCA2)),
             % a file that has gone bad refuses the new route, and the
             % running one is left as it was
             ok = file:write_file(CAFile, <<"garbage\n">>),
             ?assertMatch({error, {invalid_ca_file, CAFile,
                                   no_certificate_in_file}},
                          route(gemini, "(?<PATH>.*)", blizanci_static,
                                {private, [CAFile]},
                                #{ docroot => ?TEST_DOCROOT })),
             ?assertEqual(authorised, Live(PeerCA2))
         end)
     end}.

%% Servlet pools start and stop as the declared list changes.
apply_servlets_test_() ->
    {setup,
     fun() ->
         {ok, _} = application:ensure_all_started(ppool),
         write_test_pem()
     end,
     fun(Pem) ->
         blizanci_test_support:live_down(),
         file:delete(Pem)
     end,
     fun(Pem) ->
         ?_test(begin
             {ok, R} = route(gemini, "(?<PATH>.*)", blizanci_static,
                             public, #{ docroot => ?TEST_DOCROOT }),
             Site = add_route(test_site("a", Pem, Pem), R),
             Conf = fun(Servlets) ->
                        new(global(1965, Servlets), [Site])
                    end,
             live_up(1965, [blizanci_static], [Site]),
             ?assertEqual(undefined, whereis(blizanci_cgi)),
             ?assertEqual(ok, apply(Conf([blizanci_static,
                                          blizanci_cgi]))),
             ?assertNotEqual(undefined, whereis(blizanci_cgi)),
             ?assertEqual(ok, apply(Conf([blizanci_static]))),
             ?assertEqual(undefined, whereis(blizanci_cgi)),
             ?assertMatch({ok, #{servlets := [blizanci_static]}},
                          blizanci_conf_serv:current()),
             blizanci_test_support:live_down()
         end)
     end}.

%% A servlet whose start/0 returns {error, boom}: apply/1 refuses, with
%% the servlets it had already started in that call stopped again,
%% nothing published, and the running configuration and pools as before.
apply_servlet_start_error_test_() ->
    {setup,
     fun() ->
         {ok, _} = application:ensure_all_started(ppool),
         write_test_pem()
     end,
     fun(Pem) ->
         blizanci_test_support:live_down(),
         file:delete(Pem)
     end,
     fun(Pem) ->
         ?_test(begin
             {ok, R} = route(gemini, "(?<PATH>.*)", blizanci_static,
                             public, #{ docroot => ?TEST_DOCROOT }),
             Site = add_route(test_site("a", Pem, Pem), R),
             Conf = fun(Servlets) ->
                        new(global(1965, Servlets), [Site])
                    end,
             live_up(1965, [blizanci_static], [Site]),
             Before = blizanci_conf_serv:current(),
             ?assertEqual({error, {servlet_start_failed,
                                   blizanci_test_failing, boom}},
                          apply(Conf([blizanci_static, blizanci_cgi,
                                      blizanci_test_failing]))),
             ?assertEqual(undefined, whereis(blizanci_cgi)),
             ?assertEqual(Before, blizanci_conf_serv:current()),
             % the process is still there, and can still reconfigure
             ?assertEqual(ok, apply(Conf([blizanci_static,
                                          blizanci_cgi]))),
             ?assertNotEqual(undefined, whereis(blizanci_cgi)),
             ?assertEqual({error, {servlet_start_failed,
                                   blizanci_test_failing, boom}},
                          apply(Conf([blizanci_static,
                                      blizanci_test_failing]))),
             ?assertNotEqual(undefined, whereis(blizanci_cgi)),
             ?assertMatch({ok, #{servlets := [blizanci_static,
                                              blizanci_cgi]}},
                          blizanci_conf_serv:current()),
             blizanci_test_support:live_down()
         end)
     end}.

%% validate/1 refuses a site whose routes let a Titan upload land in a
%% CGI directory.
validate_upload_in_cgi_dir_test_() ->
    {setup,
     fun() -> {write_test_pem(), make_test_dir()} end,
     fun({Pem, Dir}) -> file:delete(Pem), file:del_dir_r(Dir) end,
     fun({Pem, Dir}) ->
         {ok, C} = route(gemini, "(?<PATH>c)", blizanci_cgi, public,
                         #{ cgiroot => Dir }),
         {ok, T} = route(titan, "(?<PATH>t)", blizanci_titan, public,
                         #{ docroot => Dir, work_dir => Dir }),
         Site = add_route(add_route(test_site("a", Pem, Pem), C), T),
         G = global(1965, [blizanci_cgi, blizanci_titan]),
         ?_assertMatch({error, {bad_routes, <<"a">>,
                                {upload_dir_overlaps_cgiroot, _, _, _}}},
                       validate(new(G, [Site])))
     end}.

make_test_dir() ->
    Dir = blizanci_test_support:tmp_path("validate_dir_test"),
    ok = filelib:ensure_path(Dir),
    Dir.
