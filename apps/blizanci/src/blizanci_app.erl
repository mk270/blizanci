%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

%% @hidden

-module(blizanci_app).

-behaviour(application).

-export([start/2, stop/1, prep_stop/1]).

-define(LISTENER, blizanci_service_ssl).

listen() ->
    ok = application:ensure_started(mime_lookup),
    ok = application:ensure_started(ranch),

    Config = blizanci_config:make(),
    #{
      ssl_opts        := SSL_Opts,
      proto_opts      := Proto_Opts,
      active_servlets := Servlets
     } = Config,

    [ ok = Servlet:start() || Servlet <- Servlets ],

    {ok, Listener} = ranch:start_listener(?LISTENER,
                                          ranch_ssl, SSL_Opts,
                                          blizanci_connection, Proto_Opts),
    {ok, Listener}.

start(_StartType, _StartArgs) ->
    {ok, _Listener} = listen(), % the _Listener is bound to the ranch pid
    {ok, _Pid} = blizanci_sup:start_link().

prep_stop(_State) ->
    ok = ranch:stop_listener(?LISTENER).

stop(_State) ->
    ok.
