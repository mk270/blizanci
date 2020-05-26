%% Blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

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
    Chil2 = #{id => blizanci_cgi,
              start => {blizanci_cgi, start_link, []},
              restart => permanent,
              shutdown => 5000,
              type => worker,
              modules => [blizanci_cgi]},

    {ok, {SupFlags, [Child, Chil2]}}.
