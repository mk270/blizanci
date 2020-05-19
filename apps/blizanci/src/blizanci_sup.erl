%%%-------------------------------------------------------------------
%%% @author Martin Keegan <mk270@savile>
%%% @copyright (C) 2020, Martin Keegan
%%% @doc
%%%
%%% @end
%%% Created : 19 May 2020 by Martin Keegan <mk270@savile>
%%%-------------------------------------------------------------------
-module(blizanci_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, Pid :: pid()} |
                      {error, {already_started, Pid :: pid()}} |
                      {error, {shutdown, term()}} |
                      {error, term()} |
                      ignore.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
-spec init(Args :: term()) ->
                  {ok, {SupFlags :: supervisor:sup_flags(),
                        [ChildSpec :: supervisor:child_spec()]}} |
                  ignore.
init([]) ->

    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    Child = #{id => blizanci_mimetypes,
              start => {blizanci_mimetypes, start_link, []},
              restart => permanent,
              shutdown => 5000,
              type => worker,
              modules => [blizanci_mimetypes]},

    {ok, {SupFlags, [Child]}}.
