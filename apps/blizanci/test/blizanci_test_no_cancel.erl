%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

% A servlet lacking exactly one callback, cancel/1, for tests of
% servlet validation.

-module(blizanci_test_no_cancel).

-export([request/4,
         serve/4,
         default_options/0,
         validate_options/1,
         start/0,
         stop/0,
         handle_client_data/2]).

default_options() -> #{}.

validate_options(_Opts) -> ok.

request(_M, _R, _C, _O) -> defer.

serve(_M, _R, _C, _O) -> {gateway_error, not_found}.

start() -> ok.

stop() -> ok.

handle_client_data(_Pid, _Payload) -> none.
