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
    Config = blizanci_config:from_app_env(),
    {ok, Pid} = blizanci_sup:start_link(Config),
    {ok, Pid}.

% The servlet list can change while running (blizanci_config:apply/1),
% so stop whatever is running now, not what was started.
stop(_State) ->
    case blizanci_live:current() of
        {ok, #{servlets := Servlets}} ->
            [ ok = Servlet:stop() || Servlet <- Servlets ];
        not_running ->
            ok
    end,
    ok = blizanci_live:clear().
