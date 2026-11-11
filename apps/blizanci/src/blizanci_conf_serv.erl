%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

%% @hidden
%% Owns the configuration table of the running instance: its port,
%% the servlet modules it declares, and its sites.
%%
%% The configuration is held in a public ETS table owned by this
%% process, and is the single source of truth for site lookup. It is
%% read by the TLS layer (the sni_fun, which ranch fixes at listen
%% time) and by each connection, and replaced by
%% blizanci_config:apply/1, so a replacement takes effect for every
%% handshake after it without touching the listener. Readers see either
%% the old value or the new one, never a mixture.
%%
%% This process does nothing else, deliberately: it creates the table
%% from the initial configuration and then only waits, so that nothing
%% in it can crash and take the table (and every applied change) with
%% it. Readers and writers use the table directly. The servlets'
%% lifecycle is blizanci_servlets, supervised after this process, so a
%% crash there leaves the table alone.
%%
%% It is supervised first (see blizanci_sup), so the table exists
%% before anything else starts, and is stopped last.

-module(blizanci_conf_serv).
-behaviour(gen_server).
-include("blizanci_types.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([start_link/1]).
-export([publish/3, current/0, sites/0, find_site/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-define(TABLE, ?MODULE).
-define(ROW, running).

-type running() :: #{port     := integer(),
                     servlets := [module()],
                     sites    := [site()]}.
-export_type([running/0]).


-spec start_link(Running) -> Result
              when Running :: running(),
                   Result  :: {'ok', pid()} | {'error', term()}.
start_link(Running) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Running, []).


%% Replace the running configuration. Readers see either the old value
%% or the new one, never a mixture.
-spec publish(Port, Servlets, Sites) -> Result
              when Port     :: integer(),
                   Servlets :: [module()],
                   Sites    :: [site()],
                   Result   :: 'ok'.
publish(Port, Servlets, Sites) ->
    true = ets:insert(?TABLE, {?ROW, #{port => Port,
                                       servlets => Servlets,
                                       sites => Sites}}),
    ok.


-spec current() -> Result
              when Result :: {'ok', running()} | 'not_running'.
current() ->
    try ets:lookup(?TABLE, ?ROW) of
        [{?ROW, Running}] -> {ok, Running};
        [] -> not_running
    catch
        error:badarg -> not_running
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


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Running = #{port := Port, servlets := Servlets, sites := Sites}) ->
    ?TABLE = ets:new(?TABLE, [named_table, public, {read_concurrency, true}]),
    ok = publish(Port, Servlets, Sites),
    {ok, Running}.

handle_call(_Request, _From, State) ->
    {reply, {error, not_supported}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% These tests use the registered process and the named table, so each
%% runs in a process of its own and removes both afterwards, whether or
%% not it passes.
in_clean_state(Fun) ->
    blizanci_test_support:isolated(
      fun() ->
          blizanci_test_support:live_down(),
          try Fun() after blizanci_test_support:live_down() end
      end).

live_test() ->
    in_clean_state(fun live/0).

live() ->
    {ok, A} = blizanci_config:site("a.example", "a.pem", "a.key"),
    {ok, B} = blizanci_config:site("b.example", "b.pem", "b.key"),
    ?assertEqual(not_running, current()),
    ?assertEqual([], sites()),
    ?assertEqual(not_found, find_site(<<"a.example">>)),
    {ok, Pid} = start_link(#{port => 1965, servlets => [blizanci_static],
                             sites => [A, B]}),
    ?assertEqual({ok, #{port => 1965, servlets => [blizanci_static],
                        sites => [A, B]}}, current()),
    ?assertEqual({ok, B}, find_site(<<"b.example">>)),
    ok = publish(1965, [], [A]),
    ?assertEqual(not_found, find_site(<<"b.example">>)),
    ok = gen_server:stop(Pid),
    ?assertEqual(not_running, current()).
