%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

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
%% Layout: one row per site, and one index row.
%%
%%   {running, Gen, Port, Servlets, Hostnames}   the index row
%%   {{site, Hostname}, Gen, Site}               one per site
%%
%% Hostnames lists the sites' hostnames in published order; Gen is a
%% fresh, increasing integer for each publish, written into the index
%% and into every site row of that publish. find_site/1 therefore
%% copies out only the one site row (and the index row's Gen, an
%% integer), not every site with its compiled routes, which matters
%% because it runs on every handshake and again in every connection.
%% current/0 and sites/0 assemble the whole value from the rows.
%%
%% Atomicity: publish/3 writes the index and all the new site rows in
%% one ets:insert/2 of a list, which is atomic and isolated, so a
%% reader sees all of it or none of it. A replaced site keeps its key,
%% so is overwritten with no gap. A site the new configuration drops
%% has its row deleted only afterwards; until then the row is stale,
%% and a reader ignores it, because its Gen is older than the index's
%% (find_site/1 reads the index Gen first, then the row). So a removed
%% site stops being found at the instant the new index is written, and
%% its row is merely garbage for a moment. A reader that read the old
%% index just before a publish and looks the row up just after the
%% publish sees either a newer row (the site is in the new
%% configuration, and the answer is the new one: the lookup is simply
%% linearised after the publish) or, if the site was removed and its
%% row already deleted, no row, which is not_found, correct for the
%% new configuration; it never crashes and never returns a site that
%% the configuration it consulted lacks. current/0 reads the index and
%% then the rows, and starts again if any row is missing or newer than
%% the index it read, so it too returns one publish's value.
%%
%% Writers are not serialised by the table: publish/3 assumes one
%% publisher at a time, which blizanci_servlets provides. Hostnames
%% are unique in a validated configuration; if not, the first of a
%% name wins, as lookup always did.
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
-define(GEN, 2).
-define(HOSTS, 5).

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
%% or the new one, never a mixture (see the module doc).
-spec publish(Port, Servlets, Sites) -> Result
              when Port     :: integer(),
                   Servlets :: [module()],
                   Sites    :: [site()],
                   Result   :: 'ok'.
publish(Port, Servlets, Sites) ->
    Gen = erlang:unique_integer([monotonic]),
    Hostnames = [ H || #site{hostname = H} <- Sites ],
    OldHostnames = try ets:lookup_element(?TABLE, ?ROW, ?HOSTS)
                   catch error:badarg -> []
                   end,
    % Reversed, so that for a repeated name the first site wins.
    Rows = [ {{site, H}, Gen, S}
             || S = #site{hostname = H} <- lists:reverse(Sites) ],
    true = ets:insert(?TABLE, [{?ROW, Gen, Port, Servlets, Hostnames}
                               | Rows]),
    [ true = ets:delete(?TABLE, {site, H})
      || H <- lists:usort(OldHostnames -- Hostnames) ],
    ok.


-spec current() -> Result
              when Result :: {'ok', running()} | 'not_running'.
current() ->
    try ets:lookup(?TABLE, ?ROW) of
        [{?ROW, Gen, Port, Servlets, Hostnames}] ->
            case rows(Hostnames, Gen, []) of
                {ok, Sites} ->
                    {ok, #{port => Port, servlets => Servlets,
                           sites => Sites}};
                retry ->
                    current()
            end;
        [] -> not_running
    catch
        error:badarg -> not_running
    end.

% The site rows of one publish: retry if one is missing or newer.
rows([], _Gen, Acc) ->
    {ok, lists:reverse(Acc)};
rows([H | T], Gen, Acc) ->
    case ets:lookup(?TABLE, {site, H}) of
        [{_, Gen, Site}] -> rows(T, Gen, [Site | Acc]);
        _ -> retry
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
    Folded = blizanci_config:ascii_lowercase(Hostname),
    try ets:lookup_element(?TABLE, ?ROW, ?GEN) of
        Gen ->
            case ets:lookup(?TABLE, {site, Folded}) of
                [{_, RowGen, Site}] when RowGen >= Gen -> {ok, Site};
                _ -> not_found
            end
    catch
        error:badarg -> not_found
    end.


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

test_site(Name, Cert) ->
    {ok, S} = blizanci_config:site(Name, Cert, "k.key"),
    S.

layout_test() ->
    in_clean_state(fun layout/0).

layout() ->
    A = test_site("a.example", "a.pem"),
    B = test_site("b.example", "b.pem"),
    C = test_site("c.example", "c.pem"),
    {ok, Pid} = start_link(#{port => 7, servlets => [], sites => [A, B]}),
    % One row per site, plus the index.
    ?assertEqual(3, ets:info(?TABLE, size)),
    ?assertMatch([{?ROW, _, 7, [], [<<"a.example">>, <<"b.example">>]}],
                 ets:lookup(?TABLE, ?ROW)),
    ?assertMatch([{{site, <<"a.example">>}, _, A}],
                 ets:lookup(?TABLE, {site, <<"a.example">>})),
    ?assertEqual({ok, A}, find_site(<<"a.example">>)),
    ?assertEqual({ok, B}, find_site(<<"B.Example">>)),
    ?assertEqual(not_found, find_site(<<"c.example">>)),
    % Replace one, add one, remove one; order is as published.
    B2 = test_site("b.example", "b2.pem"),
    ok = publish(8, [x], [C, B2]),
    ?assertEqual(3, ets:info(?TABLE, size)),
    ?assertEqual(not_found, find_site(<<"a.example">>)),
    ?assertEqual({ok, B2}, find_site(<<"b.example">>)),
    ?assertEqual({ok, #{port => 8, servlets => [x], sites => [C, B2]}},
                 current()),
    ok = publish(8, [], []),
    ?assertEqual(1, ets:info(?TABLE, size)),
    ?assertEqual([], sites()),
    ?assertEqual(not_found, find_site(<<"c.example">>)),
    ok = gen_server:stop(Pid).

empty_table_test() ->
    in_clean_state(fun() ->
        ?TABLE = ets:new(?TABLE, [named_table, public]),
        ?assertEqual(not_running, current()),
        ?assertEqual([], sites()),
        ?assertEqual(not_found, find_site(<<"a.example">>))
    end).

%% Readers loop on find_site/1, sites/0 and current/0 while a series
%% of publishes alternates between two configurations that replace,
%% add and remove sites; every observation must be that of exactly one
%% of the two.
atomic_publish_test_() ->
    {timeout, 60, fun() -> in_clean_state(fun atomic_publish/0) end}.

atomic_publish() ->
    A  = test_site("a.example", "a.pem"),
    B1 = test_site("b.example", "b1.pem"),
    B2 = test_site("b.example", "b2.pem"),
    C  = test_site("c.example", "c.pem"),
    D  = test_site("d.example", "d.pem"),
    One = #{port => 1, servlets => [s1], sites => [A, B1, C]},
    Two = #{port => 2, servlets => [s2], sites => [D, B2, A]},
    {ok, Pid} = start_link(One),
    Names = [<<"a.example">>, <<"b.example">>, <<"c.example">>,
             <<"d.example">>],
    Answer = fun(Config, N) ->
                 blizanci_config:find_site(N, maps:get(sites, Config))
             end,
    Check = fun() ->
        {ok, Cur} = current(),
        true = lists:member(Cur, [One, Two]),
        true = lists:member(sites(), [maps:get(sites, One),
                                      maps:get(sites, Two)]),
        [ true = lists:member(find_site(N), [Answer(One, N),
                                             Answer(Two, N)])
          || N <- Names ],
        ok
    end,
    Parent = self(),
    Readers = [ spawn_monitor(
                  fun() ->
                      Result = try reader_loop(Check, Parent, false)
                               catch Cl:Re:St -> {bad, Cl, Re, St}
                               end,
                      exit({reader, Result})
                  end) || _ <- lists:seq(1, 4) ],
    % Each reader has observed at least once before publishing starts.
    [ receive {started, P} -> ok end || {P, _} <- Readers ],
    [ ok = publish(maps:get(port, Cfg), maps:get(servlets, Cfg),
                   maps:get(sites, Cfg))
      || _ <- lists:seq(1, 1000), Cfg <- [Two, One] ],
    [ P ! stop || {P, _} <- Readers ],
    [ receive
          {'DOWN', Ref, process, P, {reader, Result}} ->
              ?assertEqual(ok, Result)
      end || {P, Ref} <- Readers ],
    ok = gen_server:stop(Pid).

reader_loop(Check, Parent, Announced) ->
    ok = Check(),
    case Announced of
        false -> Parent ! {started, self()};
        true -> ok
    end,
    receive stop -> ok
    after 0 -> reader_loop(Check, Parent, true)
    end.
