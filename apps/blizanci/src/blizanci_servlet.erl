%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_servlet).
-include("blizanci_types.hrl").

% Called by blizanci_servlet_container *before* creating a servlet process;
%
% A module implementing the blizanci_servlet behaviour may give an
% immediate response, which will have been constructed by code
% executed in the context of the caller (which is a
% blizanci_servlet_container process). If it opts instead to defer
% processing, the caller will start and link a new process which will call
% Module:serve/3 below.
-callback request(binary(), map(), server_config()) ->
    {'immediate', gemini_response()} |
    'defer'.

% TBD: gateway_error | gateway_started
% TBD: gateway_exit

-callback serve(binary(), map(), server_config()) -> gateway_result().

% Called for asynchronous shutdown, e.g., when the client has hung up the
% TCP connection. The callee should validate that that Pid provided still
% relates to a running process, and promptly terminate.
-callback cancel(pid()) -> 'ok'.
