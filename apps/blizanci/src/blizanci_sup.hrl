%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

%% The supervisor's registered name and the ranch listener's id, shared
%% by the module and its tests.
-define(SERVER, blizanci_sup).
-define(LISTENER, blizanci_service_ssl).
