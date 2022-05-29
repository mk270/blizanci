%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_router).

-export([prepare/1]).
-export([route/3]).

-include("blizanci_types.hrl").

-spec prepare([{string(), module(), [route_option()]}]) ->
    {'ok', [route()]} |
    {'error', atom()}.
prepare(RouteInfo) ->
    try make_routes(RouteInfo) of
        Routes -> {ok, Routes}
    catch
        Err -> {error, Err}
    end.

% TBD: spec
make_routes(RouteInfo) ->
    [ make_route(R) || R <- RouteInfo ].

-spec make_route({binary(), module(), [route_option()]}) -> route().
make_route({Regex, Module, Opts}) ->
    {ok, RE} = re:compile(Regex),
    #route{pattern=RE, module=Module, options=Opts};
make_route(_) ->
    throw(invalid_route).


-spec route(binary(), map(), server_config()) -> gemini_response().
route(Path, Request, Config=#server_config{routing=Routes}) ->
    try_route(Path, Request, Config, Routes).

-spec try_route(binary(), map(), server_config(), [route()]) -> any().
try_route(_Path, _Request, _Config, []) ->
    {error_code, file_not_found};
try_route(Path, Request, Config, [Route|Tail]) ->
    case route_match(Path, Route) of
        {match, Matches, Module, Options} ->
            dispatch(Matches, Module, Request, Options);
        _ -> try_route(Path, Request, Config, Tail)
    end.

-spec route_match(binary(), route()) ->
                         'nomatch' |
                         {'match', map(), module(), any()}.
% see the documentation for re:inspect/2 for what Matches represents and
% its format
route_match(Path, #route{pattern=Regex, module=Module, options=Options}) ->
    {namelist, Names} = re:inspect(Regex, namelist),
    case re:run(Path, Regex, [{capture, all_names, binary}]) of
        {match, M} ->
            Matches = maps:from_list(lists:zip(Names, M)),
            {match, Matches, Module, Options};
        _ -> nomatch
    end.

% TBD: typing could be improved
-spec dispatch(map(), module(), map(), any()) -> any().
dispatch(Matches, Module, Request, Options) ->
    blizanci_servlet_container:request(Module, Matches, Request, Options).
