%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

%% @hidden

-module(blizanci_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(LISTENER, blizanci_service_ssl).

start_link(Config) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Config).

init(Config) ->
    #{
      ssl_opts        := SSL_Opts,
      proto_opts      := Proto_Opts,
      active_servlets := Servlets
     } = Config,

    [ ok = Servlet:start() || Servlet <- Servlets ],

    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    RanchChildSpec = ranch:child_spec(?LISTENER, ranch_ssl, SSL_Opts,
                                      blizanci_connection, Proto_Opts),

    {ok, {SupFlags, [RanchChildSpec]}}.
