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

-spec prepare([{string(), module(), authorisation(), [route_option()]}]) ->
    {'ok', [route()]} |
    {'error', atom()}.
% TBD
%% @doc
%% Check that a routing table entry is valid.
%% @end
prepare(RouteInfo) ->
    try make_routes(RouteInfo) of
        Routes -> {ok, Routes}
    catch
        Err -> {error, Err}
    end.

% TBD: spec
make_routes(RouteInfo) ->
    [ make_route(R) || R <- RouteInfo ].

-spec make_route({atom(), binary(), module(), authorisation(), [route_option()]})
                -> route().
make_route({Proto, Regex, Module, AuthPolicy, Opts}) ->
    {ok, RE} = re:compile(Regex),
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


-spec route(atom(), binary(), map(), server_config()) -> gemini_response().
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
route(Proto, Path, Request, Config=#server_config{routing=Routes}) ->
    Relevant_Routes =
        lists:filter(fun (Route) -> Route#route.proto =:= Proto end,
                     Routes),
    try_route(Path, Request, Config, Relevant_Routes).

-spec try_route(binary(), map(), server_config(), [route()]) -> any().
try_route(_Path, _Request, _Config, []) ->
    {error_code, file_not_found};
try_route(Path, Request, Config, [Route|Tail]) ->
    case route_match(Path, Route) of
        {match, Matches, Module, AuthPolicy, RouteOpts} ->
            dispatch(Matches, Module, Request, AuthPolicy, Config, RouteOpts);
        _ -> try_route(Path, Request, Config, Tail)
    end.

-spec route_match(binary(), route()) ->
                         'nomatch' |
                         {'match', map(), module(), authorisation(), any()}.
% see the documentation for re:inspect/2 for what Matches represents and
% its format
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

% TBD: typing could be improved
-spec dispatch(path_matches(), module(), map(), authorisation(),
               server_config(), any()) ->
          gemini_response().
dispatch(Matches, Module, Request, AuthPolicy, ServerConfig, RouteOpts) ->
    case blizanci_auth:authorised(AuthPolicy, Request) of
        authorised ->
            blizanci_servlet_container:request(Module, Matches, Request,
                                               ServerConfig, RouteOpts);
        Error -> Error
    end.
