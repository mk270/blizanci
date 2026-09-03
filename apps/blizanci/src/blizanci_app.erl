%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

%% @hidden

-module(blizanci_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ok = application:ensure_started(mime_lookup),
    ok = application:ensure_started(ranch),
    Config = blizanci_config:make(),
    #{ active_servlets := Servlets } = Config,
    {ok, Pid} = blizanci_sup:start_link(Config),
    {ok, Pid, Servlets}.

stop(Servlets) ->
    [ ok = Servlet:stop() || Servlet <- Servlets ],
    ok.
