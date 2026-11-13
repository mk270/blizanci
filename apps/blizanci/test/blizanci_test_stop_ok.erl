%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

%% Fake servlet for tests: stop/0 misbehaves in the way the name says.
-module(blizanci_test_stop_ok).
-export([start/0, stop/0]).

start() -> ok.

stop() ->
    blizanci_stop_probe ! {stopped, ?MODULE},
    ok.
