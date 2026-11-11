%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

% @doc
% A module implementing the blizanci_servlet behaviour may give an
% immediate response, which will have been constructed by code
% executed in the context of the caller (which is a
% blizanci_servlet_container process). If it opts instead to defer
% processing, the caller will start and link a new process which will call
% Module:serve/4.
% @end

-module(blizanci_servlet).
-include("blizanci_types.hrl").

% Called by blizanci_servlet_container *before* creating a servlet process;
-callback request(path_matches(), request_details(), server_config(), map()) ->
    {'immediate', gemini_response()} |
    'defer'.

%% Serve the request asynchronously, in the process started for it. The
%% result is one of
%%   {gateway_finished, Response}: the servlet is done; Response goes to
%%       the client as it stands;
%%   {gateway_error, Reason}: the request failed; a Reason that
%%       blizanci_status knows (e.g., file_not_found, gateway_busy) is
%%       sent to the client as that status, any other is an internal
%%       error;
%%   {gateway_started, Pid}: the work continues in Pid, which will
%%       report its outcome later with
%%       blizanci_servlet_container:gateway_exit/2, passing
%%       {gateway_output, Binary} or {gateway_error, Reason}.
-callback serve(path_matches(), request_details(), server_config(), map()) ->
    gateway_result().

%% Called for asynchronous shutdown, e.g., when the client has hung up the
%% TCP connection. The callee should validate that that Pid provided still
%% relates to a running process, and promptly terminate.
-callback cancel(pid()) -> 'ok'.

%% Called during configuration; allows the servlet to provide a map of
%% default options for any route with which it is associated.
-callback default_options() -> map().

%% Called during configuration, once per route, with that route's
%% options already merged with default_options/0 -- lets the servlet
%% reject a bad configuration (e.g. a non-existent root directory)
%% at startup, rather than have it surface as a per-request failure.
-callback validate_options(map()) -> 'ok' | {'error', term()}.

%% Called with further data sent by the client after its request line
%% (e.g., a Titan upload body), for a servlet that started a process
%% via serve/4. Returns the response to send to the client, or 'none'
%% if more data is awaited.
-callback handle_client_data(Pid::pid(), Payload::binary()) ->
    gemini_response().

%% Called at application start, to set up any pool/resource the servlet
%% needs; see stop/0.
-callback start() -> 'ok'.

%% Called at application shutdown; the counterpart to start/0. Allows
%% the servlet to tear down any pool/resource it set up there, so
%% nothing is left running (e.g. leaked ppool pools) across a
%% stop/start cycle in the same VM.
-callback stop() -> 'ok'.
