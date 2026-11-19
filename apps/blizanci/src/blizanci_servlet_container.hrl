%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

%% The state of a container process, shared by the module and its
%% tests.
-record(servlet_state, {parent,
                        matches,
                        request,
                        server_config,
                        route_options,
                        gateway_pid,
                        gateway_module}).
