%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_servlet).

-moduledoc """
The servlet contract: what a module must provide to serve routes.

A servlet is a module with -behaviour(blizanci_servlet), named in a
route and in the configured list of servlets. The contract has three
layers, declared below in this order: the module contract, the servlet
process contract, and the gateway contract.

See the manual chapter [Servlets](servlets.md).
""".
-include("blizanci_types.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Part 1: the module contract.

%% Called during configuration; allows the servlet to provide a map of
%% default options for any route with which it is associated.
-callback default_options() -> map().

%% Called during configuration, once per route, with that route's
%% options already merged with default_options/0 -- lets the servlet
%% reject a bad configuration (e.g. a non-existent root directory)
%% at startup, rather than have it surface as a per-request failure.
-callback validate_options(map()) -> 'ok' | {'error', term()}.

%% Called at application start, to set up any pool/resource the servlet
%% needs; see stop/0. Returns {error, Reason} if it cannot (e.g. a pool
%% that will not start); the servlet is then not started, and neither
%% are any of the others in the same start-up or configuration change.
%% (An exception is treated alike, with Reason {Class, Term}.)
-callback start() -> 'ok' | {'error', term()}.

%% Called at application shutdown; the counterpart to start/0. Allows
%% the servlet to tear down any pool/resource it set up there, so
%% nothing is left running (e.g. leaked ppool pools) across a
%% stop/start cycle in the same VM. Should return ok; any other return
%% value, or an exception, is logged as a warning and ignored, and the
%% remaining servlets are still stopped.
-callback stop() -> 'ok'.

%% Called by blizanci_servlet_container:request/5, in the connection
%% process and *before* any servlet process is created, once per
%% matched and authorised request. Answers the client at once, or
%% defers to serve/4.
-callback request(path_matches(), request_details(), server_config(),
                  map()) ->
    {'immediate', gemini_response()} |
    'defer'.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Part 2: the servlet process contract (after request/4 has answered
%% 'defer'), run in the container process.

%% Serve the request asynchronously, in the process started for it (the
%% container). The result is one of
%%   {gateway_finished, Response}: the servlet is done; Response goes to
%%       the client as it stands;
%%   {gateway_error, Reason}: the request failed; a Reason that
%%       blizanci_status knows (e.g., file_not_found, gateway_busy) is
%%       sent to the client as that status, any other is an internal
%%       error;
%%   {gateway_started, Pid}: the work continues in Pid, which will
%%       report its outcome later with
%%       blizanci_servlet_container:gateway_exit/2, passing
%%       {gateway_output, Binary} or {gateway_error, Reason}. (That
%%       report is an obligation, but not a callback; a worker that
%%       answers via handle_client_data/2 instead does not make it.)
-callback serve(path_matches(), request_details(), server_config(),
                map()) ->
    gateway_result().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Part 3: the gateway contract (after serve/4 has returned
%% {gateway_started, Pid}); the worker's own reports to the container,
%% blizanci_servlet_container:gateway_exit/2, are not callbacks.

%% Called with further data sent by the client after its request line
%% (e.g., a Titan upload body), for a servlet that started a process
%% via serve/4. Returns the response to send to the client, or 'none'
%% if more data is awaited.
-callback handle_client_data(Pid::pid(), Payload::binary()) ->
    gemini_response().

%% Called for asynchronous shutdown, e.g., when the client has hung up the
%% TCP connection. The callee should validate that that Pid provided still
%% relates to a running process, and promptly terminate.
-callback cancel(pid()) -> 'ok'.
