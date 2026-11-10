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
%%
%% It is held in a public ETS table owned by this process, which is
%% supervised ahead of the listener: it starts the declared servlets
%% when it starts, and stops whatever servlets are declared at that
%% moment when it is terminated, after the listener has gone.

-module(blizanci_live).
-behaviour(gen_server).
-include("blizanci_types.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([start_link/1]).
-export([publish/3, current/0, sites/0, find_site/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

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
    process_flag(trap_exit, true),
    ?TABLE = ets:new(?TABLE, [named_table, public, {read_concurrency, true}]),
    [ ok = Servlet:start() || Servlet <- Servlets ],
    ok = publish(Port, Servlets, Sites),
    {ok, Running}.

handle_call(_Request, _From, State) ->
    {reply, {error, not_supported}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    case current() of
        {ok, #{servlets := Servlets}} ->
            [ ok = Servlet:stop() || Servlet <- Servlets ],
            ok;
        not_running ->
            ok
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

live_test() ->
    A = blizanci_config:site("a.example", "a.pem", "a.key"),
    B = blizanci_config:site("b.example", "b.pem", "b.key"),
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

%% Stopping the process stops the servlets declared at that moment, not
%% those it was started with.
terminate_stops_current_servlets_test() ->
    {ok, _} = application:ensure_all_started(ppool),
    {ok, Pid} = start_link(#{port => 1965, servlets => [],
                             sites => []}),
    ok = publish(1965, [], []),
    ok = blizanci_cgi:start(),
    ok = publish(1965, [blizanci_cgi], []),
    ?assertNotEqual(undefined, whereis(blizanci_cgi)),
    ok = gen_server:stop(Pid),
    ?assertEqual(undefined, whereis(blizanci_cgi)).
