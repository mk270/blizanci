%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

%% Owns the configuration table of the running instance: its port,
%% the servlet modules it declares, and its sites.  The table layout,
%% its atomic replacement and why this process does nothing else are
%% described in the manual chapter 'Internals'
%% (doc/manual/internals.gmi).

-module(blizanci_conf_serv).

-moduledoc false.
-behaviour(gen_server).
-include("blizanci_types.hrl").
-include("blizanci_conf_serv.hrl").

-export([start_link/1]).
-export([publish/3, current/0, sites/0, find_site/1]).
-export([init/1, handle_call/3, handle_cast/2]).

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
