%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(LISTENER, blizanci_service_ssl).

start(_StartType, _StartArgs) ->
    ok = application:ensure_started(ranch),

    SSL_Opts   = blizanci_config:ssl_opts(),
    Proto_Opts = blizanci_config:proto_opts(),

    {ok, Listener} = ranch:start_listener(?LISTENER,
                                          ranch_ssl, SSL_Opts,
                                          blizanci_gemini, Proto_Opts),
    {ok, Pid} = blizanci_sup:start_link(),
    {ok, Pid, Listener}.

stop(_State) ->
    ok = ranch:stop_listener(?LISTENER).
