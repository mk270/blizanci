%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_router).

-export([prepare/1]).
-export([route/4]).

-include("blizanci_types.hrl").


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
        Routes -> {ok, Routes}
    catch
        Err -> {error, Err}
    end.


-spec make_routes(RouteInfo) -> Routes
              when RouteInfo :: [route_info()],
                   Routes    :: [route()].
make_routes(RouteInfo) ->
    [ make_route(R) || R <- RouteInfo ].


-spec make_route(RouteInfo) -> Route
              when RouteInfo :: route_info(),
                   Route     :: route().

make_route({Proto, Regex, Module, AuthPolicy, Opts}) ->
    {ok, RE} = re:compile(Regex),
    ok = valid_proto(Proto),
    ok = valid_route_opts(Opts),
    % OptsList = maps:to_list(Opts),
    case blizanci_auth:valid_authz_policy(AuthPolicy) of
        {ok, _} -> #route{proto=Proto,
                          pattern=RE,
                          module=Module,
                          auth_policy=AuthPolicy,
                          options=Opts};
        {error, _} -> throw(invalid_route)
    end;
make_route(_) ->
    throw(invalid_route).


-spec valid_proto(Proto :: atom()) -> 'ok'.
valid_proto(gemini) -> ok;
valid_proto(titan) -> ok;
valid_proto(_) -> throw(invalid_route).


-spec valid_route_opts(term()) -> 'ok'.
valid_route_opts(Opts) when is_map(Opts) ->
    ok; % obviously we could do better than this!
valid_route_opts(_) ->
    throw(invalid_route).


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
                   Request  :: map(),
                   Config   :: server_config(),
                   Response :: gemini_response().

route(Proto, Path, Request, Config=#server_config{routing=Routes}) ->
    Relevant_Routes =
        lists:filter(fun (Route) -> Route#route.proto =:= Proto end,
                     Routes),
    try_route(Path, Request, Config, Relevant_Routes).


-spec try_route(Path, Request, Config, Routes) -> Result
        when Path    :: binary(),
             Request :: map(),
             Config  :: server_config(),
             Routes  :: [route()],
             Result  :: any(). % FIXME

try_route(_Path, _Request, _Config, []) ->
    {error_code, file_not_found};

try_route(Path, Request, Config, [Route|Tail]) ->
    case route_match(Path, Route) of
        {match, Matches, Module, AuthPolicy, RouteOpts} ->
            dispatch(Matches, Module, Request, AuthPolicy, Config, RouteOpts);
        _ -> try_route(Path, Request, Config, Tail)
    end.


-type match() :: {'match', map(), module(), authorisation(), any()}.

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
                   Request      :: map(),
                   AuthPolicy   :: authorisation(),
                   ServerConfig :: server_config(),
                   RouteOpts    :: any(),
                   Result       :: gemini_response().

dispatch(Matches, Module, Request, AuthPolicy, ServerConfig, RouteOpts) ->
    case blizanci_auth:authorised(AuthPolicy, Request) of
        authorised ->
            blizanci_servlet_container:request(Module, Matches, Request,
                                               ServerConfig, RouteOpts);
        Error -> Error
    end.
