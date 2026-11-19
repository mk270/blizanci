%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_router_tests).

-include_lib("eunit/include/eunit.hrl").
-include("blizanci_types.hrl").

-import(blizanci_router,
        [build_route/2, build_route/5, check_upload_dirs/1, prepare/1,
         prepare/3, route/4, route_match/2]).

%% Fixture, under a fresh temporary directory:
%%   Base/cgi  Base/cgi-up  Base/up  Base/work  Base/cgi/inner
%%   Base/alias -> Base/cgi
upload_dirs_test_() ->
    {setup, fun upload_setup/0, fun upload_teardown/1,
     fun upload_tests/1}.

upload_setup() ->
    Base = blizanci_test_support:tmp_path("router_test"),
    [ ok = filelib:ensure_path(filename:join(Base, D))
      || D <- ["cgi", "cgi-up", "up", "work", "cgi/inner"] ],
    ok = file:make_symlink(filename:join(Base, "cgi"),
                           filename:join(Base, "alias")),
    Base.

upload_teardown(Base) ->
    file:del_dir_r(Base).

-define(RX, "(?<PATH>x)").

path_group_test_() ->
    Opts = #{ docroot => "test-docroot" },
    Build = fun(Regex) ->
                build_route(gemini, Regex, blizanci_static, public, Opts)
            end,
    [?_assertMatch({ok, _}, Build(?RX)),
     % other groups are allowed, as long as PATH is there too
     ?_assertMatch({ok, _}, Build("(?<A>a)(?<PATH>.*)")),
     ?_assertEqual({error, {route_without_path_group, "x",
                            blizanci_static}},
                   Build("x")),
     ?_assertEqual({error, {route_without_path_group, "(?<FOO>.*)",
                            blizanci_static}},
                   Build("(?<FOO>.*)")),
     ?_assertEqual({error, {route_without_path_group, <<"(.*)">>,
                            blizanci_static}},
                   Build(<<"(.*)">>)),
     % an unnamed group is not enough; names are case-sensitive
     ?_assertMatch({error, {route_without_path_group, "(?<path>.*)", _}},
                   Build("(?<path>.*)")),
     % the whole table is refused if any one route is bad
     ?_assertMatch({error, {bad_route, 2, tuple,
                            {route_without_path_group, "bad", _}}},
                   prepare([{gemini, ?RX, blizanci_static, public, Opts},
                            {gemini, "bad", blizanci_static, public,
                             Opts}]))].

upload_tests(Base) ->
    D = fun(Name) -> filename:join(Base, Name) end,
    Cgi = fun(Dir) ->
              {ok, R} = build_route(gemini, ?RX, blizanci_cgi, public,
                                    #{ cgiroot => Dir }),
              R
          end,
    Titan = fun(Docroot, Work) ->
                {ok, R} = build_route(titan, ?RX, blizanci_titan, public,
                                      #{ docroot => Docroot,
                                         work_dir => Work }),
                R
            end,
    Check = fun(Routes) -> check_upload_dirs(Routes) end,
    Overlaps = fun(Kind) ->
                   fun({error, {upload_dir_overlaps_cgiroot, K, _, _}}) ->
                           K =:= Kind;
                      (_) -> false
                   end
               end,
    [?_assertEqual(ok, Check([Cgi(D("cgi")), Titan(D("up"), D("work"))])),
     % a sibling whose name merely starts with the cgiroot's is fine
     ?_assertEqual(ok, Check([Cgi(D("cgi")),
                              Titan(D("cgi-up"), D("work"))])),
     ?_assertEqual(ok, Check([Cgi(D("cgi"))])),
     ?_assertEqual(ok, Check([Titan(D("up"), D("work"))])),
     ?_assertEqual(ok, Check([])),
     ?_assert((Overlaps(docroot))(
                Check([Cgi(D("cgi")), Titan(D("cgi"), D("work"))]))),
     % uploads inside the cgiroot
     ?_assert((Overlaps(docroot))(
                Check([Cgi(D("cgi")), Titan(D("cgi/inner"), D("work"))]))),
     % the cgiroot inside the upload directory
     ?_assert((Overlaps(docroot))(
                Check([Cgi(D("cgi")), Titan(Base, D("work"))]))),
     ?_assert((Overlaps(work_dir))(
                Check([Cgi(D("cgi")), Titan(D("up"), D("cgi"))]))),
     % a symbolic link to the cgiroot is the cgiroot
     ?_assert((Overlaps(docroot))(
                Check([Cgi(D("cgi")), Titan(D("alias"), D("work"))]))),
     % the order of the routes doesn't matter
     ?_assert((Overlaps(docroot))(
                Check([Titan(D("cgi"), D("work")), Cgi(D("cgi"))]))),
     % binary directory names, as a Titan work_dir may be
     ?_assert((Overlaps(work_dir))(
                Check([Cgi(D("cgi")),
                       Titan(D("up"), list_to_binary(D("cgi")))]))),
     % and the check is part of preparing a routing table
     ?_assertMatch({error, {upload_dir_overlaps_cgiroot, docroot, _, _}},
                   prepare([{gemini, <<"(?<PATH>c)">>, blizanci_cgi, public,
                             #{ cgiroot => D("cgi") }},
                            {titan, <<"(?<PATH>t)">>, blizanci_titan, public,
                             #{ docroot => D("cgi"),
                                work_dir => D("work") }}]))].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests: route formats, matching, translation.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(SOPTS, #{ docroot => "test-docroot" }).

mount(Path) -> {mount, gemini, Path, blizanci_static, public, ?SOPTS}.
regex(Re) -> {regex, gemini, Re, blizanci_static, public, ?SOPTS}.
old(Re) -> {gemini, Re, blizanci_static, public, ?SOPTS}.

% Build one route, which must be valid
built(Spec) ->
    {ok, R} = build_route(Spec, #{}),
    R.

% What a route says of a path: the Matches, or nomatch
matches(Spec, Path) ->
    case route_match(Path, built(Spec)) of
        {match, M, _, _, _} -> M;
        nomatch -> nomatch
    end.

mount_matching_test_() ->
    M = fun(Mount, Path) -> matches(mount(Mount), Path) end,
    Hit = fun(Path, Rest) ->
              #{ <<"PATH">> => Rest, <<"FULL_PATH">> => Path }
          end,
    [% whole segments only
     ?_assertEqual(Hit(<<"docs/a.gmi">>, <<"a.gmi">>),
                   M("/docs", <<"docs/a.gmi">>)),
     ?_assertEqual(Hit(<<"docs">>, <<>>), M("/docs", <<"docs">>)),
     ?_assertEqual(Hit(<<"docs/">>, <<>>), M("/docs", <<"docs/">>)),
     ?_assertEqual(nomatch, M("/docs", <<"docsx/a.gmi">>)),
     ?_assertEqual(nomatch, M("/docs", <<"docsx">>)),
     ?_assertEqual(nomatch, M("/docs", <<"doc">>)),
     ?_assertEqual(nomatch, M("/docs", <<"x/docs/a.gmi">>)),
     ?_assertEqual(nomatch, M("/docs", <<"Docs/a.gmi">>)),
     ?_assertEqual(nomatch, M("/docs", <<>>)),
     % a trailing slash on the mount is optional and normalised
     ?_assertEqual(Hit(<<"docs/a/b.gmi">>, <<"a/b.gmi">>),
                   M("/docs/", <<"docs/a/b.gmi">>)),
     ?_assertEqual(Hit(<<"docs">>, <<>>), M("/docs/", <<"docs">>)),
     ?_assertEqual(nomatch, M("/docs/", <<"docsx/a">>)),
     ?_assertEqual(matches(mount("/docs"), <<"docs/q">>),
                   matches(mount("/docs/"), <<"docs/q">>)),
     % binaries as well as strings
     ?_assertEqual(Hit(<<"docs/a">>, <<"a">>),
                   M(<<"/docs">>, <<"docs/a">>)),
     % nested mounts
     ?_assertEqual(Hit(<<"a/b/c">>, <<"c">>), M("/a/b", <<"a/b/c">>)),
     ?_assertEqual(nomatch, M("/a/b", <<"a/bc">>)),
     ?_assertEqual(nomatch, M("/a/b", <<"a">>)),
     % the root mount takes everything, and PATH is the whole path
     ?_assertEqual(Hit(<<"x/y.gmi">>, <<"x/y.gmi">>),
                   M("/", <<"x/y.gmi">>)),
     ?_assertEqual(Hit(<<>>, <<>>), M("/", <<>>)),
     % a mount with a space matches the decoded path
     ?_assertEqual(Hit(<<"a b/c">>, <<"c">>), M("/a b", <<"a b/c">>))].

regex_anchoring_test_() ->
    R = fun(Re, Path) -> matches(regex(Re), Path) end,
    Old = fun(Re, Path) -> matches(old(Re), Path) end,
    [?_assertMatch(#{ <<"PATH">> := <<"private/x">> },
                   R("(?<PATH>private.*)", <<"private/x">>)),
     % the whole path, not a search
     ?_assertEqual(nomatch, R("(?<PATH>private.*)", <<"docs/private/x">>)),
     ?_assertEqual(nomatch, R("(?<PATH>private.*)", <<"x-private">>)),
     ?_assertEqual(nomatch, R("cgi-bin/(?<PATH>.*)", <<"a/cgi-bin/x">>)),
     ?_assertEqual(nomatch, R("(?<PATH>a)", <<"ab">>)),
     ?_assertMatch(#{ <<"PATH">> := <<"ab">> },
                   R("(?<PATH>a|ab)", <<"ab">>)),
     % alternation is anchored as a whole
     ?_assertMatch(#{ <<"PATH">> := <<"a">> }, R("(?<PATH>a)|b", <<"a">>)),
     ?_assertEqual(nomatch, R("(?<PATH>a)|b", <<"ba">>)),
     ?_assertMatch(#{ <<"PATH">> := <<"ab">> },
                   R(<<"(?<PATH>a.)">>, <<"ab">>)),
     % no trailing newline slips through
     ?_assertEqual(nomatch, R("(?<PATH>a)", <<"a\n">>)),
     % FULL_PATH, and other groups still pass through
     ?_assertEqual(#{ <<"PATH">> => <<"x.cgi">>,
                      <<"PATH_INFO">> => <<"/i">>,
                      <<"FULL_PATH">> => <<"cgi-bin/x.cgi/i">> },
                   R("cgi-bin/(?<PATH>[^/]*)(?<PATH_INFO>/.*)?",
                     <<"cgi-bin/x.cgi/i">>)),
     % ... whereas the old format searches, as it always did
     ?_assertMatch(#{ <<"PATH">> := <<"private/x">> },
                   Old("(?<PATH>private.*)", <<"docs/private/x">>)),
     ?_assertMatch(#{ <<"PATH">> := <<"cgi-bin/hello.sh">> },
                   Old("(?<PATH>cgi-bin/.*)", <<"docs/cgi-bin/hello.sh">>))].

full_path_test_() ->
    [?_assertMatch(#{ <<"FULL_PATH">> := <<"a/b">> },
                   matches(regex("(?<PATH>.*)"), <<"a/b">>)),
     ?_assertMatch(#{ <<"FULL_PATH">> := <<"a/b">> },
                   matches(mount("/a"), <<"a/b">>)),
     ?_assertMatch(#{ <<"FULL_PATH">> := <<"a/b">> },
                   matches(old("(?<PATH>b)"), <<"a/b">>)),
     % an old route's own FULL_PATH group is not overridden
     ?_assertEqual(#{ <<"PATH">> => <<"b">>, <<"FULL_PATH">> => <<"a">> },
                   matches(old("(?<FULL_PATH>a)/(?<PATH>b)"), <<"a/b">>)),
     % a new regex route may not use the name
     ?_assertMatch({error, {reserved_group, _, <<"FULL_PATH">>}},
                   build_route(regex("(?<PATH>a)(?<FULL_PATH>b)"), #{}))].

% Dispatch to a stub servlet that says which route it is
labelled(Label, Proto, Kind, Pattern) ->
    Opts = #{ script => fun(request) ->
                            {immediate, {success, <<"text/gemini">>, Label}}
                        end },
    {Kind, Proto, Pattern, blizanci_test_stub, public, Opts}.

routed(Routes, Proto, Path) ->
    {ok, Prepared} = prepare(Routes),
    Req = #{ client_cert => {error, no_peer_cert}, query => <<>>,
             rest_of_input => <<>>, scheme => <<"gemini">>,
             host => <<"h">>, port => 1, path => Path },
    route(Proto, Path, Req, #server_config{hostname = <<"h">>, port = 1,
                                           routing = Prepared}).

route_order_test_() ->
    A = labelled(<<"A">>, gemini, mount, "/docs/private"),
    B = labelled(<<"B">>, gemini, mount, "/docs"),
    C = labelled(<<"C">>, gemini, regex, "(?<PATH>.*\\.gmi)"),
    D = labelled(<<"D">>, gemini, mount, "/"),
    T = labelled(<<"T">>, titan, mount, "/docs"),
    Ok = fun(L) -> {success, <<"text/gemini">>, L} end,
    [% first match wins, whatever the kind
     ?_assertEqual(Ok(<<"A">>),
                   routed([A, B, C, D], gemini, <<"docs/private/x.gmi">>)),
     ?_assertEqual(Ok(<<"B">>),
                   routed([A, B, C, D], gemini, <<"docs/x.gmi">>)),
     ?_assertEqual(Ok(<<"C">>),
                   routed([A, C, B, D], gemini, <<"docs/x.gmi">>)),
     ?_assertEqual(Ok(<<"D">>),
                   routed([A, B, C, D], gemini, <<"other.txt">>)),
     % an earlier, wider route shadows a later one
     ?_assertEqual(Ok(<<"D">>),
                   routed([D, A], gemini, <<"docs/private/x">>)),
     % the boundary is respected in the table too
     ?_assertEqual(Ok(<<"D">>),
                   routed([A, B, D], gemini, <<"docsx/a">>)),
     ?_assertEqual({error_code, file_not_found},
                   routed([A, B], gemini, <<"docsx/a">>)),
     % each route serves one protocol
     ?_assertEqual(Ok(<<"T">>), routed([B, T], titan, <<"docs/x">>)),
     ?_assertEqual(Ok(<<"B">>), routed([T, B], gemini, <<"docs/x">>)),
     ?_assertEqual({error_code, file_not_found},
                   routed([B], titan, <<"docs/x">>))].

translation_test_() ->
    % The old format selects as it did: the old route's regex compiled
    % bare, and searched for
    Reference = fun(Re, Path) ->
                    {ok, MP} = re:compile(Re),
                    {namelist, Ns} = re:inspect(MP, namelist),
                    case re:run(Path, MP, [{capture, all_names, binary}]) of
                        {match, M} -> maps:from_list(lists:zip(Ns, M));
                        _ -> nomatch
                    end
                end,
    Strip = fun(nomatch) -> nomatch;
               (M) -> maps:remove(<<"FULL_PATH">>, M)
            end,
    Paths = [<<>>, <<"private">>, <<"private/x.gmi">>, <<"privatefoo">>,
             <<"docs/private/x.gmi">>, <<"public/x-private.txt">>,
             <<"cgi-bin/hello.sh">>, <<"docs/cgi-bin/hello.sh">>,
             <<"restricted/a">>, <<"a/b/c.gmi">>, <<"x">>],
    Res = ["(?<PATH>private.*)", "cgi-bin/(?<PATH>.*)",
           "(?<PATH>restricted.*)", "(?<PATH>.*)", "(?<PATH>x)",
           "(?<D>[a-z]+)/(?<PATH>.*\\.gmi)"],
    [ ?_assertEqual(Reference(Re, P), Strip(matches(old(Re), P)))
      || Re <- Res, P <- Paths ]
    ++
    [?_assertMatch({ok, #route{selector = {legacy, _}}},
                   build_route(old("(?<PATH>x)"), #{})),
     ?_assertMatch({ok, #route{selector = {mount, <<"a/b">>}}},
                   build_route(mount("/a/b/"), #{})),
     ?_assertMatch({ok, #route{selector = {mount, <<>>}}},
                   build_route(mount("/"), #{})),
     ?_assertMatch({ok, #route{selector = {regex, _}}},
                   build_route(regex("(?<PATH>x)"), #{}))].

mount_validation_test_() ->
    Bad = fun(Path) -> build_route(mount(Path), #{}) end,
    [?_assertMatch({ok, _}, Bad("/")),
     ?_assertMatch({ok, _}, Bad("/a")),
     ?_assertMatch({ok, _}, Bad("/a/")),
     ?_assertMatch({ok, _}, Bad("/a b/c.d")),
     ?_assertEqual({error, {invalid_mount, "", empty}}, Bad("")),
     ?_assertEqual({error, {invalid_mount, <<>>, empty}}, Bad(<<>>)),
     ?_assertEqual({error, {invalid_mount, "docs", not_absolute}},
                   Bad("docs")),
     ?_assertEqual({error, {invalid_mount, "//", empty_segment}},
                   Bad("//")),
     ?_assertEqual({error, {invalid_mount, "/a//b", empty_segment}},
                   Bad("/a//b")),
     ?_assertEqual({error, {invalid_mount, "/a//", empty_segment}},
                   Bad("/a//")),
     ?_assertEqual({error, {invalid_mount, "/a/../b", dot_segment}},
                   Bad("/a/../b")),
     ?_assertEqual({error, {invalid_mount, "/.", dot_segment}}, Bad("/.")),
     ?_assertEqual({error, {invalid_mount, "/a\n", control_character}},
                   Bad("/a\n")),
     ?_assertEqual({error, {invalid_mount, 42, not_a_string}}, Bad(42)),
     ?_assertEqual({error, {invalid_mount, docs, not_a_string}},
                   Bad(docs)),
     ?_assertEqual({error, {invalid_mount, <<"/a", 255>>, not_utf8}},
                   Bad(<<"/a", 255>>)),
     ?_assertMatch({ok, _}, Bad("/a-b_c~d!$&'()*+,;=:@")),
     ?_assertMatch({ok, _}, Bad(<<"/a b">>)),
     ?_assertEqual({error, {invalid_mount, [$/, 233], non_ascii}},
                   Bad([$/, 233])),
     ?_assertEqual({error, {invalid_mount, [$/, 8364], non_ascii}},
                   Bad([$/, 8364])),
     ?_assertEqual({error, {invalid_mount, <<"/", 195, 169>>,
                            non_ascii}},
                   Bad(<<"/", 195, 169>>)),
     ?_assertEqual({error, {invalid_mount, "/a\t", control_character}},
                   Bad("/a\t")),
     ?_assertEqual({error, {invalid_mount, [$/, 127], control_character}},
                   Bad([$/, 127]))].

route_error_position_test_() ->
    Good = mount("/ok"),
    P = fun(Routes) -> apply(blizanci_router, prepare, [Routes]) end,
    [?_assertMatch({ok, [_, _, _]},
                   P([Good, regex("(?<PATH>x)"), old("(?<PATH>y)")])),
     ?_assertMatch({error, {bad_route, 1, mount,
                            {invalid_mount, "nope", not_absolute}}},
                   P([mount("nope"), Good])),
     ?_assertMatch({error, {bad_route, 3, mount,
                            {invalid_mount, "/a//b", empty_segment}}},
                   P([Good, Good, mount("/a//b")])),
     ?_assertMatch({error, {bad_route, 2, mount,
                            {invalid_mount, [$/, 233], non_ascii}}},
                   P([Good, mount([$/, 233])])),
     ?_assertMatch({error, {bad_route, 2, regex,
                            {route_without_path_group, "x", _}}},
                   P([Good, regex("x")])),
     ?_assertMatch({error, {bad_route, 2, regex,
                            {invalid_regex, "(", _}}},
                   P([Good, regex("(")])),
     ?_assertMatch({error, {bad_route, 2, regex,
                            {reserved_group, _, <<"FULL_PATH">>}}},
                   P([Good, regex("(?<PATH>a)(?<FULL_PATH>b)")])),
     ?_assertMatch({error, {bad_route, 1, tuple, {invalid_regex, "(", _}}},
                   P([old("(")])),
     ?_assertMatch({error, {bad_route, 2, mount, {invalid_proto, gopher}}},
                   P([Good, {mount, gopher, "/x", blizanci_static, public,
                             ?SOPTS}])),
     ?_assertMatch({error, {bad_route, 2, mount,
                            {unknown_servlet, no_such_servlet}}},
                   P([Good, {mount, gemini, "/x", no_such_servlet, public,
                             ?SOPTS}])),
     ?_assertMatch({error, {bad_route, 2, regex, {invalid_route_opts, x}}},
                   P([Good, {regex, gemini, "(?<PATH>x)", blizanci_static,
                             public, x}])),
     % neither kind nor arity is right
     ?_assertMatch({error, {bad_route, 2, unknown, invalid_route}},
                   P([Good, {mountt, gemini, "/x", blizanci_static, public,
                             ?SOPTS}])),
     ?_assertMatch({error, {bad_route, 1, unknown, invalid_route}},
                   P([{mount, gemini, "/x"}])),
     ?_assertMatch({error, {bad_route, 1, unknown, invalid_route}},
                   P([not_a_tuple])),
     ?_assertMatch({error, {bad_route, 1, unknown, invalid_route}},
                   P([{mount, gemini, "/x", blizanci_static, public,
                       ?SOPTS, extra}])),
     % realms are resolved for the new kinds too, and the error names
     % the mount
     ?_assertMatch({error, {bad_route, 1, mount,
                            {unknown_realm, {realm, <<"r">>}, "/p",
                             blizanci_static}}},
                   P([{mount, gemini, "/p", blizanci_static,
                       {private, {realm, <<"r">>}}, ?SOPTS}]))].

% Run Fun with a logger handler that sends us what is logged; return
% what it logged about routing
with_log(Fun) ->
    Id = list_to_atom("router_test_log_" ++
                      integer_to_list(erlang:unique_integer([positive]))),
    ok = logger:add_handler(Id, blizanci_test_log_handler,
                            #{config => #{notify => self()}}),
    try Fun(), logged_routing()
    after ok = logger:remove_handler(Id)
    end.

logged_routing() ->
    receive {logged, L, "routing: " ++ _ = T} -> [{L, T} | logged_routing()];
            {logged, _, _} -> logged_routing()
    after 0 -> []
    end.

titan_mount(Path) ->
    {mount, titan, Path, blizanci_static, public, ?SOPTS}.
titan_regex(Re) ->
    {regex, titan, Re, blizanci_static, public, ?SOPTS}.

% The warnings logged for preparing a table
shadow_logs(Routes) ->
    [ T || {warning, T} <- with_log(
                             fun() -> {ok, _} = prepare(Routes) end),
           string:find(T, "old tuple format") =:= nomatch ].

shadow_mounts_test_() ->
    S = fun shadow_logs/1,
    [% equal mounts, and a prefix of whole segments
     ?_assertEqual(["routing: route 2 (gemini mount \"/docs/\") is "
                    "shadowed by route 1 (gemini mount \"/docs\") and "
                    "is never used"],
                   S([mount("/docs"), mount("/docs/")])),
     ?_assertEqual(["routing: route 2 (gemini mount \"/docs/a/b\") is "
                    "shadowed by route 1 (gemini mount \"/docs\") and "
                    "is never used"],
                   S([mount("/docs"), mount("/docs/a/b")])),
     % "/" first shadows everything after it, one line each
     ?_assertEqual(["routing: route 2 (gemini mount \"/a\") is shadowed "
                    "by route 1 (gemini mount \"/\") and is never used",
                    "routing: route 3 (gemini mount \"/b/c\") is "
                    "shadowed by route 1 (gemini mount \"/\") and is "
                    "never used"],
                   S([mount("/"), mount("/a"), mount("/b/c")])),
     ?_assertMatch([_], S([mount("/"), mount("/")])),
     % the first route that shadows is the one named
     ?_assertMatch(["routing: route 3 (gemini mount \"/a/b\") is "
                    "shadowed by route 1 (gemini mount \"/a\")" ++ _],
                   S([mount("/a"), mount("/x"), mount("/a/b")])),
     % not shadowed: siblings, a later prefix, a longer name
     ?_assertEqual([], S([mount("/a"), mount("/b")])),
     ?_assertEqual([], S([mount("/docs"), mount("/docsx")])),
     ?_assertEqual([], S([mount("/docsx"), mount("/docs")])),
     ?_assertEqual([], S([mount("/docs/a"), mount("/docs")])),
     ?_assertEqual([], S([mount("/docs/a"), mount("/docs/b"),
                          mount("/")])),
     ?_assertEqual([], S([]))].

shadow_protocol_test_() ->
    S = fun shadow_logs/1,
    [% routes of different protocols never shadow each other
     ?_assertEqual([], S([mount("/"), titan_mount("/up")])),
     ?_assertEqual([], S([titan_mount("/"), mount("/up"),
                          regex("(?<PATH>.*)")])),
     % within a protocol they do, and positions count the whole list
     ?_assertEqual(["routing: route 4 (titan mount \"/up\") is shadowed "
                    "by route 2 (titan mount \"/\") and is never used"],
                   S([mount("/x"), titan_mount("/"), mount("/y"),
                      titan_mount("/up")])),
     ?_assertEqual(["routing: route 3 (titan regex \"(?<PATH>.*)\") is "
                    "shadowed by route 1 (titan mount \"/\") and is "
                    "never used"],
                   S([titan_mount("/"), mount("/"),
                      titan_regex("(?<PATH>.*)")]))].

shadow_regex_test_() ->
    S = fun shadow_logs/1,
    All = "(?<PATH>.*)",
    [% an earlier catch-all, of any kind, shadows later regexes
     ?_assertEqual(["routing: route 2 (gemini regex "
                    "\"cgi-bin/(?<PATH>.*)\") is shadowed by route 1 "
                    "(gemini regex \"(?<PATH>.*)\") and is never used"],
                   S([regex(All), regex("cgi-bin/(?<PATH>.*)")])),
     ?_assertEqual(["routing: route 2 (gemini tuple "
                    "\"cgi-bin/(?<PATH>.*)\") is shadowed by route 1 "
                    "(gemini tuple \"(?<PATH>.*)\") and is never used"],
                   S([old(All), old("cgi-bin/(?<PATH>.*)")])),
     ?_assertMatch([_], S([old(All), regex("docs/(?<PATH>.*)")])),
     ?_assertMatch([_], S([mount("/"), regex("docs/(?<PATH>.*)")])),
     ?_assertMatch([_], S([mount("/docs"), regex("docs/(?<PATH>.*)")])),
     ?_assertMatch([_], S([mount("/"), regex(All)])),
     ?_assertMatch([_], S([regex(All), mount("/docs")])),
     ?_assertMatch([_], S([old("(?<PATH>docs.*)"), mount("/docs")])),
     ?_assertMatch([_, _], S([regex(All), mount("/a"), old(All)])),
     % partial overlaps are not reported
     ?_assertEqual([], S([mount("/docs"), regex(All)])),
     ?_assertEqual([], S([regex("docs/(?<PATH>.*)"), mount("/docs")])),
     ?_assertEqual([], S([regex("(?<PATH>.*\\.gmi)"), regex(All)])),
     ?_assertEqual([], S([regex("(?<PATH>x)"), regex("(?<PATH>.)")])),
     ?_assertEqual([], S([old("(?<PATH>a.*)"), old("(?<PATH>.*a)")])),
     % no samples can be derived: not reported, even if it is shadowed
     ?_assertEqual([], S([regex(All), regex("(?<PATH>a|b)")])),
     ?_assertEqual([], S([regex(All), regex("(?<PATH>x)")])),
     ?_assertMatch([_], S([regex(All), regex("(a)?(?<PATH>.*)")]))].

shadow_option_test_() ->
    Routes = [mount("/"), mount("/a")],
    [?_assertMatch([{warning, _}],
                   with_log(fun() -> {ok, _} = prepare(Routes) end)),
     ?_assertEqual([], with_log(fun() ->
                                    {ok, _} = prepare(Routes, #{},
                                              #{ warn_shadowed => false })
                                end)),
     % start-up is not refused, and the routes are all there
     ?_assertMatch({ok, [_, _]}, prepare(Routes))].

old_format_warning_test_() ->
    Mounts = [mount("/a"), regex("(?<PATH>x)")],
    Olds = [old("(?<PATH>a)"), old("(?<PATH>b)"), old("(?<PATH>c)")],
    Warn = fun(Routes, Opts) ->
               with_log(fun() -> {ok, _} = prepare(Routes, #{}, Opts) end)
           end,
    [?_assertEqual([{warning,
                     "routing: 3 routes use the old tuple format "
                     "{Proto, Regex, Module, Auth, Opts}; translated, "
                     "behaviour unchanged (see manual, Routing)"}],
                   Warn(Mounts ++ Olds, #{})),
     ?_assertMatch([{warning, "routing: 1 routes use" ++ _}],
                   Warn(Mounts ++ [hd(Olds)], #{})),
     % nothing to say if there are none
     ?_assertEqual([], Warn(Mounts, #{})),
     ?_assertEqual([], Warn([], #{})),
     ?_assertMatch([{warning, _}], Warn(Olds, #{})),
     % not logged for a table that is refused, nor by building one route
     ?_assertEqual([], with_log(fun() ->
                                    {error, _} = prepare(Olds ++ [old("(")])
                                end)),
     ?_assertEqual([], with_log(fun() ->
                                    {ok, _} = build_route(hd(Olds), #{}),
                                    {ok, _} = build_route(
                                                gemini, "(?<PATH>x)",
                                                blizanci_static, public,
                                                ?SOPTS)
                                end))].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests: the path-valued options are binaries once a route is built.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(PATH_KEYS, [docroot, cgiroot, work_dir]).

% A stub servlet's route in each of the three formats, with Opts.
formats(Opts) ->
    S = blizanci_test_stub,
    [{mount, {mount, gemini, "/m", S, public, Opts}},
     {regex, {regex, gemini, "(?<PATH>m.*)", S, public, Opts}},
     {tuple, {gemini, "(?<PATH>m.*)", S, public, Opts}}].

converted(Info) ->
    {ok, [#route{options = O}]} = prepare([Info]),
    O.

path_options_converted_test_() ->
    Opts = fun(V) -> #{ docroot => V, cgiroot => V, work_dir => V,
                        cgiprefix => "/cgi-bin/" } end,
    Expect = #{ docroot => <<"/srv/docs">>, cgiroot => <<"/srv/docs">>,
                work_dir => <<"/srv/docs">>, cgiprefix => "/cgi-bin/" },
    Relative = #{ docroot => "rel/dir", cgiroot => <<"./c d">>,
                  work_dir => "" },
    [ ?_assertEqual(Expect, converted(Info))
      || V <- ["/srv/docs", <<"/srv/docs">>],
         {_, Info} <- formats(Opts(V)) ]
    ++
    [% relative paths are kept as given (a space is printable ASCII)
     ?_assertEqual(#{ docroot => <<"rel/dir">>, cgiroot => <<"./c d">>,
                      work_dir => <<>> },
                   converted(Info))
     || {_, Info} <- formats(Relative) ]
    ++
    [% build_route/2 and build_route/5 convert as well
     ?_assertMatch({ok, #route{options = #{ docroot := <<"d">> }}},
                   build_route({mount, gemini, "/", blizanci_test_stub,
                                public, #{ docroot => "d" }}, #{})),
     ?_assertMatch({ok, #route{options = #{ docroot := <<"d">> }}},
                   build_route(gemini, "(?<PATH>x)", blizanci_test_stub,
                               public, #{ docroot => "d" })),
     % the options a servlet's validate_options/1 sees are converted
     ?_test(begin
                {ok, _} = prepare([{mount, gemini, "/", blizanci_test_stub,
                                    public,
                                    #{ docroot => "d", cgiroot => "c",
                                       report_to => self() }}]),
                receive
                    {validate_options, O} ->
                        ?assertMatch(#{ docroot := <<"d">>,
                                        cgiroot := <<"c">> }, O)
                after 1000 -> error(not_validated)
                end
            end),
     ?_assert(is_binary(maps:get(docroot,
                                 blizanci_titan:default_options())))].

path_options_refused_test_() ->
    % strings with a code point above 126, above 255, a control
    % character, DEL; binaries with a byte above 126, NUL, a newline;
    % and things that are not file names at all
    Values = [[$a, 233], [$a, 8364], "a\nb", "a\tb", [127], [0],
              <<"a", 233>>, <<"caf", 195, 169>>, <<"a", 0>>, <<"a\nb">>,
              <<127>>, not_a_string, 42, [a, b], ["a", "b"], {x}, #{},
              improper()],
    [ ?_assertEqual({error, {bad_route, 2, Kind,
                             {invalid_path_option, Key, V}}},
                    prepare([mount("/ok"),
                             hd([ I || {K, I} <- formats(#{ Key => V }),
                                       K =:= Kind ])]))
      || Key <- ?PATH_KEYS, V <- Values, Kind <- [mount, regex, tuple] ]
    ++
    [% the message is one line naming the route's position and the option
     ?_assertEqual(
        "invalid routing table: route 2 (mount route): the docroot "
        "option must be a plain ASCII file name (printable ASCII "
        "characters only), not [100,233]",
        blizanci_config:describe_error(
          {invalid_routing,
           element(2, prepare([mount("/ok"),
                               element(2, hd(formats(
                                               #{ docroot =>
                                                  [$d, 233] })))])) })),
     % build_route reports it without a position
     ?_assertEqual({error, {invalid_path_option, cgiroot, [$a, 233]}},
                   build_route({mount, gemini, "/", blizanci_test_stub,
                                public, #{ cgiroot => [$a, 233] }}, #{}))].

% An improper list (built so that dialyzer does not object).
improper() ->
    lists:foldl(fun(X, Acc) -> [X | Acc] end, tail, [$a]).
