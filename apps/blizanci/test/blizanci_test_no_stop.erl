%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

% A servlet lacking exactly one callback, stop/0, for tests of
% servlet validation.

-module(blizanci_test_no_stop).

-export([request/4,
         serve/4,
         default_options/0,
         validate_options/1,
         start/0,
         cancel/1,
         handle_client_data/2]).

default_options() -> #{}.

validate_options(_Opts) -> ok.

request(_M, _R, _C, _O) -> defer.

serve(_M, _R, _C, _O) -> {gateway_error, not_found}.

start() -> ok.

cancel(_Pid) -> ok.

handle_client_data(_Pid, _Payload) -> none.
