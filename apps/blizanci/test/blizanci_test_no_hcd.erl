%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2026  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

% A servlet lacking exactly one callback, handle_client_data/2, for tests of
% servlet validation.

-module(blizanci_test_no_hcd).

-export([request/4,
         serve/4,
         default_options/0,
         validate_options/1,
         start/0,
         stop/0,
         cancel/1]).

default_options() -> #{}.

validate_options(_Opts) -> ok.

request(_M, _R, _C, _O) -> defer.

serve(_M, _R, _C, _O) -> {gateway_error, not_found}.

start() -> ok.

stop() -> ok.

cancel(_Pid) -> ok.
