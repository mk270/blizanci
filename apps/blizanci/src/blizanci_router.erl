%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_router).

-export([prepare/1, prepare/2, prepare/3, prepare_all/3, build_route/2,
         build_route/5, build_route/6, check_servlet/1,
         check_upload_dirs/1]).
-export([route/4]).

-include("blizanci_types.hrl").
-include_lib("eunit/include/eunit.hrl").


%% A route as configured.  The old format, {Proto, Regex, Module, Auth,
%% Opts}, is translated (see below); the new one is a tagged tuple,
%% {mount, Proto, Path, Module, Auth, Opts} or {regex, Proto, Re, Module,
%% Auth, Opts}.
%%
%% A mount Path must be a non-empty string or binary (UTF-8) that starts
%% with "/".  Split on "/", it may have no empty segment (so no "//"),
%% except that one trailing "/" is allowed and ignored, no "." or ".."
%% segment and no control character.  "/" is the whole site.  A mount
%% matches whole path segments only: "/docs" and "/docs/" are the same
%% and match "/docs", "/docs/" and "/docs/a.gmi", but not "/docsx".  The
%% servlet's PATH is what follows the mount, without a leading slash
%% (empty for "/docs" itself); FULL_PATH is the whole request path.
%% Request paths here are the decoded ones, without the leading slash.
%%
%% A regex route's Re must match the WHOLE request path (it is anchored
%% by wrapping it in \A(?:...)\z, so a trailing comment in (?x) mode
%% will not compile), and must have a named group PATH and none called
%% FULL_PATH.  Other named groups pass through to the servlet.
%%
%% An old-format tuple behaves exactly as before: its regex is searched
%% for, not anchored, in the path, and must have a PATH group.  FULL_PATH
%% is added to its Matches unless its regex has a group of that name.
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

A table that validated is also checked for shadowed routes, which
can never be reached because an earlier route of the same protocol
(gemini routes are compared only with gemini routes, titan with
titan) matches every request they could.  One warning is logged for
each shadowed route, naming it and the first route shadowing it by
position and kind, and by mount path or regex.  This only warns: it
never refuses a table or changes routing, and it can be turned off
with the option warn_shadowed (default true).  Auth is not
considered.  The rules are:

Mount against mount is exact: the earlier mount shadows the later
if its segments are a prefix of the later one's segments, so "/"
shadows every later mount, and so does an equal mount, but "/docs"
does not shadow "/docsx".

Whenever a regex is involved (as either route, old-format or not)
overlap cannot be decided, so it is probed, and a false negative is
preferred to a false positive.  Sample paths are derived from the
later route: for a mount M, M itself, M/x and M/a/b; for a regex,
the literal text its source starts with (a leading ^ skipped, the
text cut short before a quantifier, and none at all if the source
has a vertical bar), L, plus L followed by x, a/b and x.gmi.  Only
samples that the later route itself matches are kept, and with
fewer than two of them nothing is reported.  The earlier route is
then run on every sample with the router's own selector, and the
later route is reported only if the earlier one matches all of them.
A partial overlap is never reported.
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
               "behaviour unchanged (see README, Routing)",
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    P = fun(Routes) -> apply(?MODULE, prepare, [Routes]) end,
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
                     "behaviour unchanged (see README, Routing)"}],
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
