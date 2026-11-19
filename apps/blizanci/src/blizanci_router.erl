%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_router).

-export([prepare/1, prepare/2, prepare/3, prepare_all/3, build_route/2,
         build_route/5, build_route/6, check_servlet/1,
         check_upload_dirs/1]).
-export([route/4]).

-ifdef(TEST).
%% Exported for the tests in test/blizanci_router_tests.erl.
-export([route_match/2]).
-endif.

-include("blizanci_types.hrl").


%% A route as configured.  The old format, {Proto, Regex, Module, Auth,
%% Opts}, is translated; the new one is a tagged tuple, {mount, Proto,
%% Path, Module, Auth, Opts} or {regex, Proto, Re, Module, Auth, Opts}.
%% The rules for mount paths, regexes and old-format routes are in the
%% manual chapter 'Routing' (doc/manual/routing.gmi).
-type route_info() :: {'mount', Proto :: atom(), Path :: string() | binary(),
                       Module :: module(), Auth :: authorisation_config(),
                       Opts :: map()}
                    | {'regex', Proto :: atom(), Re :: string() | binary(),
                       Module :: module(), Auth :: authorisation_config(),
                       Opts :: map()}
                    | {Proto      :: atom(),
                       Regex      :: string() | binary(),
                       Module     :: module(),
                       AuthPolicy :: authorisation_config(),
                       Opts       :: map()
                      }.

-define(IS_LIT(C), ((C >= $a andalso C =< $z) orelse
                    (C >= $A andalso C =< $Z) orelse
                    (C >= $0 andalso C =< $9) orelse
                    C =:= $/ orelse C =:= $- orelse C =:= $_)).
-define(IS_QUANT(Q), (Q =:= $* orelse Q =:= $+ orelse Q =:= $? orelse
                      Q =:= ${)).

-type prepare_opts() :: #{ warn_shadowed => boolean(),
                          site => string() }.

-doc """
Check that a routing table entry is valid.
""".
-spec prepare(RouteInfo) -> Result
              when RouteInfo :: [route_info()],
                   Result    :: {'ok', [route()]} |
                                {'error', term()}.

prepare(RouteInfo) ->
    prepare(RouteInfo, #{}).


-doc """
As prepare/1, resolving realms against a realm table.
""".
-spec prepare(RouteInfo, Realms) -> Result
              when RouteInfo :: [route_info()],
                   Realms    :: realm_table(),
                   Result    :: {'ok', [route()]} |
                                {'error', term()}.
prepare(RouteInfo, Realms) ->
    prepare(RouteInfo, Realms, #{}).


-doc """
As prepare/2, with options.  Routes are one ordered list, first match
wins.  An error names the route: {bad_route, Index, Kind, Reason},
where Index is the 1-based position in the list, Kind is mount, regex,
tuple (the old format) or unknown, and Reason says what is wrong.

Old-format routes are translated, not refused.  If there are any,
one warning is logged saying how many.  New-format routes never
trigger it.

A table that validated is also checked for shadowed routes, which can
never be reached because an earlier route of the same protocol
matches every request they could.  One warning is logged for each.
This only warns, and can be turned off with the option warn_shadowed
(default true).  The rules are in the manual chapter 'Routing',
section 'Shadowing'.
""".
-spec prepare(RouteInfo, Realms, Opts) -> Result
              when RouteInfo :: [route_info()],
                   Realms    :: realm_table(),
                   Opts      :: prepare_opts(),
                   Result    :: {'ok', [route()]} |
                                {'error', term()}.
prepare(RouteInfo, Realms, Opts) ->
    try make_routes(RouteInfo, Realms) of
        Routes ->
            case check_upload_dirs(Routes) of
                ok ->
                    warn_old_format(RouteInfo, Opts),
                    warn_shadowed(RouteInfo, Routes, Opts),
                    {ok, Routes};
                {error, _} = Error -> Error
            end
    catch
        Err -> {error, Err}
    end.


-doc """
As prepare/3, but for start-up validation, which reports every
problem at once: every route is built, and the routes that built are
returned along with an error, {bad_route, Index, Kind, Reason}, for
each one that did not, in order. Warnings are logged only when every
route built. The upload directory check of prepare/3 is not made;
see check_upload_dirs/1, which the caller runs on the routes.
""".
-spec prepare_all(RouteInfo, Realms, Opts) -> Result
              when RouteInfo :: [route_info()],
                   Realms    :: realm_table(),
                   Opts      :: prepare_opts(),
                   Result    :: {[route()], [term()]}.
prepare_all(RouteInfo, Realms, Opts) ->
    Results = [ {I, R, build_route(R, Realms)}
                || {I, R} <- lists:enumerate(RouteInfo) ],
    Routes = [ Route || {_, _, {ok, Route}} <- Results ],
    Errors = [ {bad_route, I, route_kind(R), Why}
               || {I, R, {error, Why}} <- Results ],
    case Errors of
        [] ->
            warn_old_format(RouteInfo, Opts),
            warn_shadowed(RouteInfo, Routes, Opts);
        _ -> ok
    end,
    {Routes, Errors}.


-spec warn_old_format(RouteInfo, Opts) -> ok
              when RouteInfo :: [route_info()],
                   Opts      :: prepare_opts().
warn_old_format(RouteInfo, Opts) ->
    case length([ R || R <- RouteInfo, route_kind(R) =:= tuple ]) of
        0 -> ok;
        N -> logger:warning(
               "routing: ~s~b routes use the old tuple format "
               "{Proto, Regex, Module, Auth, Opts}; translated, "
               "behaviour unchanged (see manual, Routing)",
               [site_prefix(Opts), N])
    end.


% Log a warning for each shadowed route; see prepare/3.
-spec warn_shadowed(RouteInfo, Routes, Opts) -> ok
              when RouteInfo :: [route_info()],
                   Routes    :: [route()],
                   Opts      :: prepare_opts().
warn_shadowed(RouteInfo, Routes, Opts) ->
    case maps:get(warn_shadowed, Opts, true) of
        false -> ok;
        true ->
            Items = lists:zip3(lists:seq(1, length(Routes)), RouteInfo,
                               Routes),
            lists:foreach(fun(Pair) -> log_shadowed(Pair, Opts) end,
                          shadowed(Items, []))
    end.


-type item() :: {pos_integer(), route_info(), route()}.

% The shadowed routes, each with the first earlier route of its
% protocol that shadows it.  Earlier is the routes seen so far, latest
% first.
-spec shadowed(Items, Earlier) -> Found
              when Items   :: [item()],
                   Earlier :: [item()],
                   Found   :: [{item(), item()}].
shadowed([], _) ->
    [];
shadowed([{_, _, #route{proto = Proto}} = Item | Rest], Earlier) ->
    Same = [ E || {_, _, #route{proto = P}} = E <- lists:reverse(Earlier),
                  P =:= Proto ],
    case [ E || E <- Same, shadows(E, Item) ] of
        [First | _] -> [{Item, First} | shadowed(Rest, [Item | Earlier])];
        [] -> shadowed(Rest, [Item | Earlier])
    end.


% Does the earlier route match every request the later one could?
-spec shadows(Earlier, Later) -> boolean()
              when Earlier :: item(),
                   Later   :: item().
shadows({_, _, #route{selector = {mount, EP}}},
        {_, _, #route{selector = {mount, LP}}}) ->
    segments_prefix(segments(EP), segments(LP));
shadows({_, _, #route{selector = ESel}},
        {_, Info, #route{selector = LSel}}) ->
    case probes(LSel, pattern_of(Info)) of
        [] -> false;
        Samples -> lists:all(fun(S) -> select(ESel, S) =/= nomatch end,
                             Samples)
    end.


-spec segments(Prefix) -> Segments
              when Prefix   :: binary(),
                   Segments :: [binary()].
segments(<<>>) -> [];
segments(Prefix) -> binary:split(Prefix, <<"/">>, [global]).


-spec segments_prefix(A, B) -> boolean()
              when A :: [binary()],
                   B :: [binary()].
segments_prefix([], _) -> true;
segments_prefix([S | A], [S | B]) -> segments_prefix(A, B);
segments_prefix(_, _) -> false.


% Sample request paths that the route matches (see prepare/3); none if
% fewer than two can be had.
-spec probes(Selector, Pattern) -> Samples
              when Selector :: selector(),
                   Pattern  :: string() | binary(),
                   Samples  :: [binary()].
probes({mount, <<>>}, _) ->
    [<<>>, <<"x">>, <<"a/b">>];
probes({mount, Prefix}, _) ->
    [Prefix, <<Prefix/binary, "/x">>, <<Prefix/binary, "/a/b">>];
probes(Selector, Pattern) ->
    case literal_prefix(Pattern) of
        none -> [];
        Lit ->
            Cands = [ <<Lit/binary, T/binary>>
                      || T <- [<<>>, <<"x">>, <<"a/b">>, <<"x.gmi">>] ],
            Samples = lists:usort([ S || S <- Cands,
                                         select(Selector, S) =/= nomatch ]),
            case Samples of
                [_, _ | _] -> Samples;
                _ -> []
            end
    end.


% The literal text a regex source starts with, or 'none' if it has an
% alternation or is not text.
-spec literal_prefix(Pattern) -> Result
              when Pattern :: string() | binary(),
                   Result  :: binary() | 'none'.
literal_prefix(Pattern) ->
    Bin = try unicode:characters_to_binary(Pattern)
          catch error:badarg -> none end,
    case is_binary(Bin) andalso binary:match(Bin, <<"|">>) =:= nomatch of
        false -> none;
        true ->
            case Bin of
                <<"^", R/binary>> -> take_literal(R, <<>>);
                _ -> take_literal(Bin, <<>>)
            end
    end.


-spec take_literal(Src, Acc) -> Literal
              when Src     :: binary(),
                   Acc     :: binary(),
                   Literal :: binary().
take_literal(<<C, Q, _/binary>>, Acc) when ?IS_LIT(C), ?IS_QUANT(Q) ->
    Acc;
take_literal(<<C, Rest/binary>>, Acc) when ?IS_LIT(C) ->
    take_literal(Rest, <<Acc/binary, C>>);
take_literal(_, Acc) ->
    Acc.


-spec log_shadowed(Pair, Opts) -> ok
              when Pair :: {item(), item()},
                   Opts :: prepare_opts().
log_shadowed({{I, Info, #route{proto = P}},
              {J, EInfo, #route{proto = EP}}}, Opts) ->
    logger:warning("routing: ~sroute ~b (~s ~s ~ts) is shadowed by "
                   "route ~b (~s ~s ~ts) and is never used",
                   [site_prefix(Opts), I, P, route_kind(Info),
                    pattern_text(Info),
                    J, EP, route_kind(EInfo), pattern_text(EInfo)]).


% The site a table belongs to, if the caller says (the sites key), as
% the start of a warning's text.
-spec site_prefix(Opts) -> Result
              when Opts   :: prepare_opts(),
                   Result :: string().
site_prefix(#{site := Site}) -> "site " ++ Site ++ ": ";
site_prefix(_) -> "".


% The mount path or regex a configured route is selected by.
-spec pattern_of(Info) -> Pattern
              when Info    :: route_info(),
                   Pattern :: string() | binary().
pattern_of(Info) when tuple_size(Info) =:= 6 -> element(3, Info);
pattern_of(Info) -> element(2, Info).


-spec pattern_text(Info) -> Text
              when Info :: route_info(),
                   Text :: string().
pattern_text(Info) ->
    Pattern = pattern_of(Info),
    Q = try unicode:characters_to_list(Pattern) of
            L when is_list(L) -> L;
            _ -> io_lib:format("~w", [Pattern])
        catch error:badarg -> io_lib:format("~w", [Pattern]) end,
    [$" | Q] ++ "\"".


% What kind of route a configured entry is, going by its shape.
-spec route_kind(RouteInfo) -> Kind
              when RouteInfo :: term(),
                   Kind      :: 'mount' | 'regex' | 'tuple' | 'unknown'.
route_kind(R) when tuple_size(R) =:= 6, element(1, R) =:= mount -> mount;
route_kind(R) when tuple_size(R) =:= 6, element(1, R) =:= regex -> regex;
route_kind(R) when tuple_size(R) =:= 5 -> tuple;
route_kind(_) -> unknown.


-doc """
Check that no Titan upload can land in a directory CGI scripts are
run from: neither a Titan route's docroot (where uploads end up) nor
its work_dir (where they are staged) may be, contain, or lie inside
the cgiroot of any CGI route. Directories are compared after
resolving symbolic links, so an alias doesn't get round it.
""".
-spec check_upload_dirs(Routes) -> Result
              when Routes :: [route()],
                   Result :: 'ok' | {'error', term()}.
check_upload_dirs(Routes) ->
    CGIRoots = [ Root || #route{module = blizanci_cgi,
                                options = #{ cgiroot := Root }} <- Routes ],
    UploadDirs = [ {Kind, Dir}
                   || #route{module = blizanci_titan,
                             options = Opts} <- Routes,
                      Kind <- [docroot, work_dir],
                      Dir <- [maps:get(Kind, Opts, undefined)],
                      Dir =/= undefined ],
    first_overlap([ {Kind, Dir, Root} || {Kind, Dir} <- UploadDirs,
                                         Root <- CGIRoots ]).


-spec first_overlap(Pairs) -> Result
              when Pairs  :: [{atom(), term(), term()}],
                   Result :: 'ok' | {'error', term()}.
first_overlap([]) ->
    ok;
first_overlap([{Kind, Dir, Root} | Rest]) ->
    case {canonical_dir(Dir), canonical_dir(Root)} of
        {{ok, CanonDir}, {ok, CanonRoot}} ->
            case blizanci_path:path_under_root(CanonDir, CanonRoot)
                 orelse blizanci_path:path_under_root(CanonRoot, CanonDir) of
                true -> {error, {upload_dir_overlaps_cgiroot,
                                 Kind, Dir, Root}};
                false -> first_overlap(Rest)
            end;
        {{error, Reason}, _} ->
            {error, {cannot_resolve_dir, Dir, Reason}};
        {_, {error, Reason}} ->
            {error, {cannot_resolve_dir, Root, Reason}}
    end.


-spec canonical_dir(Dir) -> Result
              when Dir    :: binary(),
                   Result :: {'ok', binary()} | {'error', atom()}.
canonical_dir(Dir) when is_binary(Dir) ->
    blizanci_path:fix_path(filename:absname(Dir)).


-spec make_routes(RouteInfo, Realms) -> Routes
              when RouteInfo :: [route_info()],
                   Realms    :: realm_table(),
                   Routes    :: [route()].
make_routes(RouteInfo, Realms) ->
    [ try make_route(R, Realms)
      catch throw:Why -> throw({bad_route, I, route_kind(R), Why}) end
      || {I, R} <- lists:enumerate(RouteInfo) ].


-doc """
Build and validate a single route of either format, given as a tuple
(see prepare/3), resolving realms against a realm table.  An old-format
tuple is translated silently.
""".
-spec build_route(RouteInfo, Realms) -> Result
              when RouteInfo :: route_info(),
                   Realms    :: realm_table(),
                   Result    :: {'ok', route()} | {'error', term()}.
build_route(RouteInfo, Realms) ->
    try make_route(RouteInfo, Realms) of
        Route -> {ok, Route}
    catch
        throw:Reason -> {error, Reason}
    end.


-doc """
Build and validate a single route, reporting a specific reason if it
is invalid rather than throwing or crashing.
""".
-spec build_route(Proto, Regex, Module, AuthPolicy, Opts) -> Result
              when Proto      :: atom(),
                   Regex      :: string() | binary(),
                   Module     :: module(),
                   AuthPolicy :: authorisation_config(),
                   Opts       :: map(),
                   Result     :: {'ok', route()} | {'error', term()}.
build_route(Proto, Regex, Module, AuthPolicy, Opts) ->
    build_route(Proto, Regex, Module, AuthPolicy, Opts, #{}).


-doc """
As build_route/5, resolving realms against a realm table.
""".
-spec build_route(Proto, Regex, Module, AuthPolicy, Opts, Realms) ->
              Result
              when Proto      :: atom(),
                   Regex      :: string() | binary(),
                   Module     :: module(),
                   AuthPolicy :: authorisation_config(),
                   Opts       :: map(),
                   Realms     :: realm_table(),
                   Result     :: {'ok', route()} | {'error', term()}.
build_route(Proto, Regex, Module, AuthPolicy, Opts, Realms) ->
    try make_route({Proto, Regex, Module, AuthPolicy, Opts}, Realms) of
        Route -> {ok, Route}
    catch
        throw:Reason -> {error, Reason}
    end.


-spec make_route(RouteInfo, Realms) -> Route
              when RouteInfo :: route_info(),
                   Realms    :: realm_table(),
                   Route     :: route().

make_route({mount, Proto, Path, Module, AuthPolicy, Opts}, Realms) ->
    ok = valid_proto(Proto),
    Prefix = parse_mount(Path),
    finish_route(Proto, {mount, Prefix}, Path, Module, AuthPolicy, Opts,
                 Realms);
make_route({regex, Proto, Re, Module, AuthPolicy, Opts}, Realms) ->
    ok = valid_proto(Proto),
    RE = compile_regex(anchor(Re), Re),
    ok = no_full_path_group(Re, RE),
    finish_route(Proto, {regex, RE}, Re, Module, AuthPolicy, Opts, Realms);
make_route({Proto, Regex, Module, AuthPolicy, Opts}, Realms) ->
    % the old format: translated, behaving as it always did
    ok = valid_proto(Proto),
    RE = compile_regex(Regex, Regex),
    finish_route(Proto, {legacy, RE}, Regex, Module, AuthPolicy, Opts,
                 Realms);
make_route(_, _) ->
    throw(invalid_route).


% The checks every kind of route shares, and the route itself.  What
% names the route in errors (Pattern) is its mount path or regex.
-spec finish_route(Proto, Selector, Pattern, Module, AuthPolicy, Opts,
                   Realms) -> Route
              when Proto      :: atom(),
                   Selector   :: selector(),
                   Pattern    :: string() | binary(),
                   Module     :: term(),
                   AuthPolicy :: term(),
                   Opts       :: term(),
                   Realms     :: realm_table(),
                   Route      :: route().
finish_route(Proto, Selector, Pattern, Module, AuthPolicy, Opts, Realms) ->
    ok = valid_servlet(Module),
    ok = valid_route_opts(Opts),
    % The route's own options are merged with the module's defaults
    % here, once, so every caller (production config, tests, any
    % future third-party caller) gets the same guarantee -- callers
    % are not responsible for remembering to merge before calling in.
    FullOpts = path_options(maps:merge(Module:default_options(), Opts)),
    ok = valid_module_opts(Module, FullOpts),
    ok = valid_new_format_authz(Selector, AuthPolicy),
    {ok, Authz} = valid_authz(AuthPolicy, Realms, Pattern, Module),
    case Selector of
        {mount, _} -> ok;
        {_, RE} -> ok = valid_path_group(Pattern, RE, Module)
    end,
    #route{proto=Proto,
           selector=Selector,
           module=Module,
           auth_policy=Authz,
           options=FullOpts}.


% The options that name directories are file names, and file names are
% bytes end to end: here, once, when the route is built, each is
% converted to a binary (the operator may write a string or a binary), and
% from here on nothing downstream sees a string.  Only printable ASCII is
% accepted, so that no UTF-8, look-alike or invisible character can get
% into a root.  (cgiprefix is a URL prefix, not a file name, and is left
% alone.)  A relative path is kept as given.
-define(PATH_OPTIONS, [docroot, cgiroot, work_dir]).

-spec path_options(Opts) -> Opts
              when Opts :: map().
path_options(Opts) ->
    lists:foldl(fun path_option/2, Opts, ?PATH_OPTIONS).


-spec path_option(Key, Opts) -> Opts
              when Key  :: atom(),
                   Opts :: map().
path_option(Key, Opts) ->
    case maps:find(Key, Opts) of
        error -> Opts;
        {ok, Value} -> Opts#{ Key := plain_ascii(Key, Value) }
    end.


-spec plain_ascii(Key, Value) -> Binary
              when Key    :: atom(),
                   Value  :: term(),
                   Binary :: binary().
plain_ascii(Key, Value) ->
    case plain_bytes(Value) of
        {ok, Bytes} -> Bytes;
        error -> throw({invalid_path_option, Key, Value})
    end.


% The bytes of a binary or string, if all are printable ASCII.
-spec plain_bytes(Value) -> Result
              when Value  :: term(),
                   Result :: {'ok', binary()} | 'error'.
plain_bytes(Value) when is_binary(Value) ->
    case [ B || <<B>> <= Value, B < 32 orelse B > 126 ] of
        [] -> {ok, Value};
        _ -> error
    end;
plain_bytes(Value) when is_list(Value) ->
    try lists:all(fun (C) -> is_integer(C) andalso C >= 32
                                 andalso C =< 126 end, Value) of
        true -> {ok, list_to_binary(Value)};
        false -> error
    catch error:_ -> error % an improper list
    end;
plain_bytes(_) ->
    error.


% Check a mount path (see route_info()) and return it as a prefix of
% whole segments: no leading or trailing slash, <<>> for "/".  The
% path must be printable ASCII.  Checks run in this order: not a
% string, not UTF-8 (so invalid UTF-8 keeps not_utf8), empty, not
% absolute, then per segment empty, dot, control_character (below 32
% or DEL) and last non_ascii (anything above 127).
-spec parse_mount(Path) -> Prefix
              when Path   :: term(),
                   Prefix :: binary().
parse_mount(Path) ->
    Bin = try unicode:characters_to_binary(Path) of
              B when is_binary(B) -> B;
              _ -> throw({invalid_mount, Path, not_utf8})
          catch
              error:badarg -> throw({invalid_mount, Path, not_a_string})
          end,
    case Bin of
        <<>> -> throw({invalid_mount, Path, empty});
        <<"/", _/binary>> -> ok;
        _ -> throw({invalid_mount, Path, not_absolute})
    end,
    [<<>> | Segs0] = binary:split(Bin, <<"/">>, [global]),
    Segs = case lists:reverse(Segs0) of
               [<<>> | Rest] -> lists:reverse(Rest);
               _ -> Segs0
           end,
    lists:foreach(fun(S) -> valid_segment(Path, S) end, Segs),
    iolist_to_binary(lists:join(<<"/">>, Segs)).


-spec valid_segment(Path, Segment) -> ok
              when Path    :: term(),
                   Segment :: binary().
valid_segment(Path, <<>>) ->
    throw({invalid_mount, Path, empty_segment});
valid_segment(Path, S) when S =:= <<".">>; S =:= <<"..">> ->
    throw({invalid_mount, Path, dot_segment});
valid_segment(Path, S) ->
    case [ C || <<C>> <= S, C < 32 orelse C =:= 127 ] of
        [] -> ok;
        _ -> throw({invalid_mount, Path, control_character})
    end,
    case [ C || <<C>> <= S, C > 127 ] of
        [] -> ok;
        _ -> throw({invalid_mount, Path, non_ascii})
    end.


% Make a regex match only the whole path.  Group numbering is
% unchanged, as the wrapper's group does not capture.
-spec anchor(Re) -> Anchored
              when Re       :: term(),
                   Anchored :: term().
anchor(Re) when is_binary(Re) -> <<"\\A(?:", Re/binary, ")\\z">>;
anchor(Re) when is_list(Re) -> "\\A(?:" ++ Re ++ ")\\z";
anchor(Re) -> Re.


-spec no_full_path_group(Re, RE) -> Result
              when Re     :: string() | binary(),
                   RE     :: mp(),
                   Result :: 'ok'.
no_full_path_group(Re, RE) ->
    {namelist, Names} = re:inspect(RE, namelist),
    case lists:member(<<"FULL_PATH">>, Names) of
        false -> ok;
        true -> throw({reserved_group, Re, <<"FULL_PATH">>})
    end.


% New-format routes (mount, regex) take realm policies only: a file
% list, {private, Files} or {private, []}, is for old tuples alone.
-spec valid_new_format_authz(Selector, AuthPolicy) -> Result
              when Selector   :: selector(),
                   AuthPolicy :: term(),
                   Result     :: 'ok'.
valid_new_format_authz({legacy, _}, _) -> ok;
valid_new_format_authz(_, {private, [{realm, _} | _]}) -> ok;
valid_new_format_authz(_, {private, Files}) when is_list(Files) ->
    throw(file_list_in_new_route);
valid_new_format_authz(_, _) -> ok.


-spec valid_authz(AuthPolicy, Realms, Regex, Module) -> Result
              when AuthPolicy :: term(),
                   Realms     :: realm_table(),
                   Regex      :: string() | binary(),
                   Module     :: module(),
                   Result     :: {'ok', authorisation()}.
valid_authz(AuthPolicy, Realms, Regex, Module) ->
    case blizanci_auth:valid_authz_policy(AuthPolicy, Realms) of
        {ok, _} = Ok -> Ok;
        {error, invalid_authz_policy} ->
            throw({invalid_authz_policy, AuthPolicy});
        {error, {unknown_realm, Realm}} ->
            throw({unknown_realm, Realm, Regex, Module});
        {error, Reason} -> throw(Reason)
    end.


% Every servlet takes the path to serve from the named group PATH of
% the route's regex (see blizanci_path:resolve_request/3), so a regex
% without one is a configuration bug: refuse it here, at start-up,
% rather than let the first matching request crash. (A regex with no
% named groups at all never even matches, see route_match/2.)
-spec valid_path_group(Regex, RE, Module) -> Result
              when Regex  :: string() | binary(),
                   RE     :: mp(),
                   Module :: term(),
                   Result :: 'ok'.
valid_path_group(Regex, RE, Module) ->
    {namelist, Names} = re:inspect(RE, namelist),
    case lists:member(<<"PATH">>, Names) of
        true  -> ok;
        false -> throw({route_without_path_group, Regex, Module})
    end.


% Compile what to compile; Regex is what the operator wrote, for errors.
-spec compile_regex(ToCompile, Regex) -> Result
              when ToCompile :: term(),
                   Regex     :: term(),
                   Result    :: mp().
compile_regex(ToCompile, Regex) ->
    try re:compile(ToCompile) of
        {ok, RE} -> RE;
        {error, Why} -> throw({invalid_regex, Regex, Why})
    catch
        error:badarg -> throw({invalid_regex, Regex, not_a_string})
    end.


-doc """
Check that Module is loadable and exports the callbacks a servlet
needs: all the blizanci_servlet callbacks, that is default_options/0,
validate_options/1, request/4, serve/4, start/0, stop/0, cancel/1 and
handle_client_data/2. A missing one is reported as
{servlet_missing_callbacks, Module, [{Name, Arity}]}.
""".
-spec check_servlet(Module) -> Result
              when Module :: term(),
                   Result :: 'ok' | {'error', term()}.
check_servlet(Module) ->
    try valid_servlet(Module) of
        ok -> ok
    catch
        throw:Reason -> {error, Reason}
    end.


% What a route's servlet must export: every callback of the
% blizanci_servlet behaviour, so that a missing one is refused at
% configuration time rather than failing at run time.
-define(SERVLET_CALLBACKS, [{default_options, 0}, {validate_options, 1},
                            {request, 4}, {serve, 4}, {start, 0},
                            {stop, 0}, {cancel, 1},
                            {handle_client_data, 2}]).

-spec valid_servlet(Module) -> Result
              when Module :: term(),
                   Result :: 'ok'.
valid_servlet(Module) when is_atom(Module) ->
    case code:ensure_loaded(Module) of
        {module, Module} ->
            case [ FA || {F, A} = FA <- ?SERVLET_CALLBACKS,
                         not erlang:function_exported(Module, F, A) ] of
                [] -> ok;
                Missing -> throw({servlet_missing_callbacks, Module,
                                  Missing})
            end;
        {error, _} -> throw({unknown_servlet, Module})
    end;
valid_servlet(Module) ->
    throw({not_a_servlet, Module}).


-spec valid_proto(Proto) -> Result
              when Proto  :: atom(),
                   Result :: 'ok'.
valid_proto(gemini) -> ok;
valid_proto(titan) -> ok;
valid_proto(Proto) -> throw({invalid_proto, Proto}).


-spec valid_route_opts(Opts) -> Result
              when Opts   :: term(),
                   Result :: 'ok'.
valid_route_opts(Opts) when is_map(Opts) ->
    ok; % obviously we could do better than this!
valid_route_opts(Opts) ->
    throw({invalid_route_opts, Opts}).


% Let the servlet itself reject a bad configuration (e.g. a
% non-existent root directory) at startup, rather than have it
% surface as a per-request failure.
-spec valid_module_opts(Module, Opts) -> Result
              when Module :: module(),
                   Opts   :: map(),
                   Result :: 'ok'.
valid_module_opts(Module, Opts) ->
    case Module:validate_options(Opts) of
        ok -> ok;
        {error, Reason} -> throw({invalid_route_options, Module, Reason})
    end.


-doc """
Route a Gemini request to a handler.

The Config provides a routing table, which is an ordered list of regular
expressions and associated handler modules. route/3 searches for whether
the Request matches any of these routes, and if so dispatches it to the
appropriate handler module.

- `Proto`: the atom `gemini` or similar
- `Path`: the Path in the Gemini request
- `Request`: the Gemini request
- `Config`: the server configuration, including the routing table
""".
-spec route(Proto, Path, Request, Config) -> Response
              when Proto    :: atom(),
                   Path     :: binary(),
                   Request  :: request_details(),
                   Config   :: server_config(),
                   Response :: gemini_response().

route(Proto, Path, Request, Config=#server_config{routing=Routes}) ->
    Relevant_Routes =
        lists:filter(fun (Route) -> Route#route.proto =:= Proto end,
                     Routes),
    try_route(Path, Request, Config, Relevant_Routes).


% Iterate through the routes in the routing table in order, and dispatch
% the request if it matches one of them.
%
% Otherwise return a "file not found" code
-spec try_route(Path, Request, Config, Routes) -> Result
              when Path    :: binary(),
                   Request :: request_details(),
                   Config  :: server_config(),
                   Routes  :: [route()],
                   Result  :: gemini_response().

try_route(_Path, _Request, _Config, []) ->
    {error_code, file_not_found};

try_route(Path, Request, Config, [Route|Tail]) ->
    case route_match(Path, Route) of
        {match, Matches, Module, AuthPolicy, RouteOpts} ->
            dispatch(Matches, Module, Request, AuthPolicy, Config, RouteOpts);
        _ -> try_route(Path, Request, Config, Tail)
    end.


-type match() :: {'match', map(), module(), authorisation(),
                map()}.

% Does the Path match the candidate route? If so return details of how it
% matches. Otherwise return 'nomatch'.
%
% The data returned in the 'match' case includes variables captured by
% the regular expression, such as "PATH"
%
% see the documentation for re:inspect/2 for what Matches represents and
% its format
-spec route_match(Path, Route) -> Result
              when Path   :: binary(),
                   Route  :: route(),
                   Result :: match() | 'nomatch'.

route_match(Path, #route{selector=Selector,
                         module=Module,
                         auth_policy=AuthPolicy,
                         options=RouteOpts}) ->
    case select(Selector, Path) of
        {match, Matches} ->
            {match, Matches, Module, AuthPolicy, RouteOpts};
        nomatch -> nomatch
    end.


% Match the path against the route's selector, and give the servlet's
% Matches.  Every route gets FULL_PATH, the path as it was given to the
% router; an old-format route's own group of that name takes precedence,
% so it sees nothing different from before.
-spec select(Selector, Path) -> Result
              when Selector :: selector(),
                   Path     :: binary(),
                   Result   :: {'match', path_matches()} | 'nomatch'.
select({mount, <<>>}, Path) ->
    {match, #{ <<"PATH">> => Path, <<"FULL_PATH">> => Path }};
select({mount, Prefix}, Path) ->
    N = byte_size(Prefix),
    case Path of
        Prefix ->
            {match, #{ <<"PATH">> => <<>>, <<"FULL_PATH">> => Path }};
        <<Prefix:N/binary, "/", Rest/binary>> ->
            {match, #{ <<"PATH">> => Rest, <<"FULL_PATH">> => Path }};
        _ -> nomatch
    end;
select({regex, Regex}, Path) ->
    case run_regex(Regex, Path) of
        {match, Matches} ->
            {match, Matches#{ <<"FULL_PATH">> => Path }};
        nomatch -> nomatch
    end;
select({legacy, Regex}, Path) ->
    case run_regex(Regex, Path) of
        {match, Matches} ->
            {match, maps:merge(#{ <<"FULL_PATH">> => Path }, Matches)};
        nomatch -> nomatch
    end.


% See the documentation for re:inspect/2 for what Matches represents and
% its format.
-spec run_regex(Regex, Path) -> Result
              when Regex  :: mp(),
                   Path   :: binary(),
                   Result :: {'match', path_matches()} | 'nomatch'.
run_regex(Regex, Path) ->
    {namelist, Names} = re:inspect(Regex, namelist),
    case re:run(Path, Regex, [{capture, all_names, binary}]) of
        {match, M} -> {match, maps:from_list(lists:zip(Names, M))};
        _ -> nomatch
    end.


-spec dispatch(Matches, Module, Request, AuthPolicy,
               ServerConfig, RouteOpts) -> Result
              when Matches      :: path_matches(),
                   Module       :: module(),
                   Request      :: request_details(),
                   AuthPolicy   :: authorisation(),
                   ServerConfig :: server_config(),
                   RouteOpts    :: map(),
                   Result       :: gemini_response().

dispatch(Matches, Module, Request, AuthPolicy, ServerConfig, RouteOpts) ->
    % io:format("dispatch req: ~p~n", [Request]),
    case blizanci_auth:authorised(AuthPolicy, Request) of
        authorised ->
            blizanci_servlet_container:request(Module, Matches, Request,
                                               ServerConfig, RouteOpts);
        Error -> Error
    end.
