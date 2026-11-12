%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2026  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

%% A servlet module for tests whose start/0 fails: it returns
%% {error, boom}. stop/0 is harmless.

-module(blizanci_test_failing).
-behaviour(blizanci_servlet).

-export([request/4, serve/4, handle_client_data/2, cancel/1,
         default_options/0, validate_options/1, start/0, stop/0]).

default_options() -> #{}.

validate_options(_Opts) -> ok.

request(_Matches, _Req, _Config, _Opts) -> {gateway_error, not_found}.

serve(_Matches, _Req, _Config, _Opts) -> {gateway_error, not_found}.

handle_client_data(_Pid, _Payload) -> none.

cancel(_Pid) -> ok.

start() -> {error, boom}.

stop() -> ok.
