%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

%% @hidden
%% The configuration of the running instance: its port, declared
%% servlets and sites. This is the single source of truth for site
%% lookup, read both by the TLS layer (the sni_fun, which ranch fixes
%% at listen time) and by each connection, so replacing it takes
%% effect for every handshake after the replacement without touching
%% the listener.

-module(blizanci_live).
-include("blizanci_types.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([publish/3, current/0, sites/0, find_site/1, clear/0]).

-define(KEY, {?MODULE, running}).

-type running() :: #{port     := integer(),
                     servlets := [module()],
                     sites    := [site()]}.
-export_type([running/0]).


-spec publish(Port, Servlets, Sites) -> Result
              when Port     :: integer(),
                   Servlets :: [module()],
                   Sites    :: [site()],
                   Result   :: 'ok'.
publish(Port, Servlets, Sites) ->
    persistent_term:put(?KEY, #{port => Port,
                                servlets => Servlets,
                                sites => Sites}).


-spec current() -> Result
              when Result :: {'ok', running()} | 'not_running'.
current() ->
    case persistent_term:get(?KEY, not_running) of
        not_running -> not_running;
        Running -> {ok, Running}
    end.


-spec sites() -> Result
              when Result :: [site()].
sites() ->
    case current() of
        {ok, #{sites := Sites}} -> Sites;
        not_running -> []
    end.


-spec find_site(Hostname) -> Result
              when Hostname :: binary(),
                   Result   :: {'ok', site()} | 'not_found'.
find_site(Hostname) ->
    blizanci_config:find_site(Hostname, sites()).


-spec clear() -> Result
              when Result :: 'ok'.
clear() ->
    _ = persistent_term:erase(?KEY),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

live_test_() ->
    A = blizanci_config:site("a.example", "a.pem", "a.key"),
    B = blizanci_config:site("b.example", "b.pem", "b.key"),
    {setup, fun clear/0, fun(_) -> clear() end,
     fun(_) ->
         [?_assertEqual(not_running, current()),
          ?_assertEqual([], sites()),
          ?_assertEqual(not_found, find_site(<<"a.example">>)),
          ?_test(begin
                     ok = publish(1965, [blizanci_static], [A, B]),
                     ?assertEqual({ok, #{port => 1965,
                                         servlets => [blizanci_static],
                                         sites => [A, B]}}, current()),
                     ?assertEqual({ok, B}, find_site(<<"b.example">>)),
                     ok = publish(1965, [], [A]),
                     ?assertEqual(not_found, find_site(<<"b.example">>)),
                     ok = clear(),
                     ?assertEqual(not_running, current())
                 end)]
     end}.
