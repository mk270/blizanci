%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_router).

-export([prepare/1, build_route/5, check_servlet/1,
         check_upload_dirs/1]).
-export([route/4]).

-include("blizanci_types.hrl").
-include_lib("eunit/include/eunit.hrl").


-type route_info() :: {Proto      :: atom(),
                       Regex      :: binary(),
                       Module     :: module(),
                       AuthPolicy :: authorisation(),
                       Opts       :: map()
                      }.

% TBD
%% @doc
%% Check that a routing table entry is valid.
%% @end
-spec prepare(RouteInfo) -> Result
              when RouteInfo :: [route_info()],
                   Result    :: {'ok', [route()]} |
                                {'error', term()}.

prepare(RouteInfo) ->
    try make_routes(RouteInfo) of
        Routes ->
            case check_upload_dirs(Routes) of
                ok -> {ok, Routes};
                {error, _} = Error -> Error
            end
    catch
        Err -> {error, Err}
    end.


%% @doc
%% Check that no Titan upload can land in a directory CGI scripts are
%% run from: neither a Titan route's docroot (where uploads end up) nor
%% its work_dir (where they are staged) may be, contain, or lie inside
%% the cgiroot of any CGI route. Directories are compared after
%% resolving symbolic links, so an alias doesn't get round it.
%% @end
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
              when Dir    :: binary() | string(),
                   Result :: {'ok', string()} | {'error', atom()}.
canonical_dir(Dir) when is_binary(Dir) ->
    canonical_dir(unicode:characters_to_list(Dir));
canonical_dir(Dir) when is_list(Dir) ->
    blizanci_path:fix_path(filename:absname(Dir)).


-spec make_routes(RouteInfo) -> Routes
              when RouteInfo :: [route_info()],
                   Routes    :: [route()].
make_routes(RouteInfo) ->
    [ make_route(R) || R <- RouteInfo ].


%% @doc
%% Build and validate a single route, reporting a specific reason if it
%% is invalid rather than throwing or crashing.
%% @end
-spec build_route(Proto, Regex, Module, AuthPolicy, Opts) -> Result
              when Proto      :: atom(),
                   Regex      :: string() | binary(),
                   Module     :: module(),
                   AuthPolicy :: authorisation(),
                   Opts       :: map(),
                   Result     :: {'ok', route()} | {'error', term()}.
build_route(Proto, Regex, Module, AuthPolicy, Opts) ->
    try make_route({Proto, Regex, Module, AuthPolicy, Opts}) of
        Route -> {ok, Route}
    catch
        throw:Reason -> {error, Reason}
    end.


-spec make_route(RouteInfo) -> Route
              when RouteInfo :: route_info(),
                   Route     :: route().

make_route({Proto, Regex, Module, AuthPolicy, Opts}) ->
    ok = valid_proto(Proto),
    RE = compile_regex(Regex),
    ok = valid_servlet(Module),
    ok = valid_route_opts(Opts),
    % The route's own options are merged with the module's defaults
    % here, once, so every caller (production config, tests, any
    % future third-party caller) gets the same guarantee -- callers
    % are not responsible for remembering to merge before calling in.
    FullOpts = maps:merge(Module:default_options(), Opts),
    ok = valid_module_opts(Module, FullOpts),
    case blizanci_auth:valid_authz_policy(AuthPolicy) of
        {ok, _} -> #route{proto=Proto,
                          pattern=RE,
                          module=Module,
                          auth_policy=AuthPolicy,
                          options=FullOpts};
        {error, Reason} -> throw({Reason, AuthPolicy})
    end;
make_route(_) ->
    throw(invalid_route).


-spec compile_regex(Regex) -> Result
              when Regex  :: term(),
                   Result :: mp().
compile_regex(Regex) ->
    try re:compile(Regex) of
        {ok, RE} -> RE;
        {error, Why} -> throw({invalid_regex, Regex, Why})
    catch
        error:badarg -> throw({invalid_regex, Regex, not_a_string})
    end.


%% @doc
%% Check that Module is loadable and implements the servlet callbacks
%% used for route validation.
%% @end
-spec check_servlet(Module) -> Result
              when Module :: term(),
                   Result :: 'ok' | {'error', term()}.
check_servlet(Module) ->
    try valid_servlet(Module) of
        ok -> ok
    catch
        throw:Reason -> {error, Reason}
    end.


-spec valid_servlet(Module) -> Result
              when Module :: term(),
                   Result :: 'ok'.
valid_servlet(Module) when is_atom(Module) ->
    case code:ensure_loaded(Module) of
        {module, Module} ->
            case erlang:function_exported(Module, default_options, 0)
                 andalso erlang:function_exported(Module,
                                                  validate_options, 1) of
                true -> ok;
                false -> throw({not_a_servlet, Module})
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


%% @doc
%% Route a Gemini request to a handler.
%% The Config provides a routing table, which is an ordered list of regular
%% expressions and associated handler modules. route/3 searches for whether
%% the Request matches any of these routes, and if so dispatches it to the
%% appropriate handler module.
%%
%% @param The atom 'gemini' or similar
%% @param Path the Path in the Gemini request
%% @param Request the Gemini request
%% @param Config the server configuration, including the routing table
%% @end
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


-type match() :: {'match', map(), module(), authorisation(), any()}.

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

route_match(Path, #route{pattern=Regex,
                         module=Module,
                         auth_policy=AuthPolicy,
                         options=RouteOpts}) ->
    {namelist, Names} = re:inspect(Regex, namelist),
    case re:run(Path, Regex, [{capture, all_names, binary}]) of
        {match, M} ->
            Matches = maps:from_list(lists:zip(Names, M)),
            {match, Matches, Module, AuthPolicy, RouteOpts};
        _ -> nomatch
    end.


-spec dispatch(Matches, Module, Request, AuthPolicy,
               ServerConfig, RouteOpts) -> Result
              when Matches      :: path_matches(),
                   Module       :: module(),
                   Request      :: request_details(),
                   AuthPolicy   :: authorisation(),
                   ServerConfig :: server_config(),
                   RouteOpts    :: any(),
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

upload_tests(Base) ->
    D = fun(Name) -> filename:join(Base, Name) end,
    Cgi = fun(Dir) ->
              {ok, R} = build_route(gemini, "x", blizanci_cgi, public,
                                    #{ cgiroot => Dir }),
              R
          end,
    Titan = fun(Docroot, Work) ->
                {ok, R} = build_route(titan, "x", blizanci_titan, public,
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
     % binary directory names, as the default Titan work_dir is
     ?_assert((Overlaps(work_dir))(
                Check([Cgi(D("cgi")),
                       Titan(D("up"), list_to_binary(D("cgi")))]))),
     % and the check is part of preparing a routing table
     ?_assertMatch({error, {upload_dir_overlaps_cgiroot, docroot, _, _}},
                   prepare([{gemini, <<"c">>, blizanci_cgi, public,
                             #{ cgiroot => D("cgi") }},
                            {titan, <<"t">>, blizanci_titan, public,
                             #{ docroot => D("cgi"),
                                work_dir => D("work") }}]))].
