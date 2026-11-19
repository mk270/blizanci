%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

%% The supervision tree, strategy rest_for_one: blizanci_conf_serv,
%% then blizanci_servlets, then the ranch listener.  See the manual
%% chapter 'Internals' (doc/manual/internals.gmi).

-module(blizanci_sup).

-moduledoc false.

-behaviour(supervisor).
-include("blizanci_sup.hrl").

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).


% Restart intensity: more than this many restarts within the period
% below takes the supervisor, and so the application, down.
-define(RESTART_INTENSITY, 3).
% The restart period, in seconds.
-define(RESTART_PERIOD_S, 10).
% How long the table owner may take to stop, in milliseconds.
-define(TABLE_SHUTDOWN_MS, 5000).
% How long the servlets server may take to stop, in milliseconds.
-define(SERVLETS_SHUTDOWN_MS, 10000).

start_link(Config) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Config).

init(Config) ->
    #{
      ssl_opts   := SSL_Opts,
      proto_opts := Proto_Opts,
      servlets   := Servlets,
      port       := Port,
      sites      := Sites
     } = Config,

    SupFlags = #{strategy => rest_for_one,
                 intensity => ?RESTART_INTENSITY,
                 period => ?RESTART_PERIOD_S},

    RanchChildSpec = ranch:child_spec(?LISTENER, ranch_ssl, SSL_Opts,
                                      blizanci_connection, Proto_Opts),

    TableChildSpec = #{id => blizanci_conf_serv,
                       start => {blizanci_conf_serv, start_link,
                                 [#{port => Port,
                                    servlets => Servlets,
                                    sites => Sites}]},
                       restart => permanent,
                       shutdown => ?TABLE_SHUTDOWN_MS,
                       type => worker,
                       modules => [blizanci_conf_serv]},

    ServletsChildSpec = #{id => blizanci_servlets,
                          start => {blizanci_servlets, start_link, []},
                          restart => permanent,
                          shutdown => ?SERVLETS_SHUTDOWN_MS,
                          type => worker,
                          modules => [blizanci_servlets]},

    {ok, {SupFlags, [TableChildSpec, ServletsChildSpec, RanchChildSpec]}}.
