%%%-------------------------------------------------------------------

%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

% TBD: module-level docs
-module(blizanci_titan).

-behaviour(blizanci_servlet).

-include("blizanci_types.hrl").

-export([serve/4, cancel/1, request/4, default_options/0,
         handle_client_data/1]).

-type options() :: #{ 
                      docroot := string()
                    }.

%%%===================================================================
%%% API
%%%===================================================================

-spec default_options() -> map().
default_options() ->
    #{ }.

-spec cancel(pid()) -> ok.
cancel(_) ->
    ok.

% Called by the servlet
%
-spec request(path_matches(), request_details(), server_config(), options()) ->
                         {'immediate', gemini_response()} |
                         'defer'.
request(Matches, Req, ServerConfig, RouteOpts) ->
    lager:info("servlet request received: ~p ~p ~p ~p",
               [Matches, Req, ServerConfig, RouteOpts]),
    {immediate, {error_code, unrecognised_protocol}}.


-spec serve(path_matches(), request_details(), server_config(), options()) ->
                   gateway_result().
serve(_, _, _, _) ->
    {gateway_error, unimplemented}.


handle_client_data(_) -> none.


%%%===================================================================
%%% Internal functions
%%%===================================================================
