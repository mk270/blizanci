%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ok = application:ensure_started(ranch),

    {ok, Cert} = application:get_env(certfile),
    {ok, Key} = application:get_env(keyfile),
    {ok, Hostname} = application:get_env(hostname),
    {ok, Docroot} = application:get_env(docroot),
    SSL_Opts =
        [{port, 1965},
         {certfile, Cert},
         {keyfile, Key}
        ],
    Proto_Opts =
        [{hostname, Hostname},
         {docroot, Docroot},
         {mimetypes, []}],
    {ok, Listener} = ranch:start_listener(blizanci_service_clear,
                                          ranch_ssl, SSL_Opts,
                                          blizanci_proto, Proto_Opts),
    {ok, Pid} = blizanci_sup:start_link(),
    {ok, Pid, Listener}.

stop(State) ->
    ranch:stop_listener(State),
    ok.
