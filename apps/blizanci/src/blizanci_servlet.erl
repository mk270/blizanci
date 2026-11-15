%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

%% @doc
%% The servlet contract: what a module must provide to serve routes.
%%
%% A servlet is a module with <code>-behaviour(blizanci_servlet)</code>,
%% named in a route (see blizanci_router) and in the configured list of
%% servlets. The three servlets shipped are blizanci_static (files, with
%% no worker), blizanci_cgi and blizanci_titan (both with a worker in a
%% ppool queue). The contract has three layers, which this behaviour
%% declares in this order: the module contract, the servlet process
%% contract, and the gateway contract.
%%
%% == 1. The module contract ==
%%
%% What the servlet MODULE provides. These callbacks are called in the
%% context of whoever calls them, or at configuration time; no servlet
%% process exists yet.
%%
%% <ul>
%% <li><code>default_options/0</code> and
%% <code>validate_options/1</code> are called when a route is built
%% (blizanci_router:build_route/5, from the configuration check and
%% apply, in the process doing that). The route's own options are
%% merged over <code>default_options/0</code> (the route's win), the
%% result is passed to <code>validate_options/1</code>, and that merged
%% map is what the route keeps and passes to every later call as
%% RouteOpts. A <code>{error, Reason}</code> from
%% <code>validate_options/1</code> refuses the whole routing table. It
%% is the place to reject a bad configuration (say, a docroot that is
%% not a directory) at start-up, not per request.</li>
%% <li><code>start/0</code> and <code>stop/0</code> are called by the
%% blizanci_servlets process: <code>start/0</code> at application start
%% and, on blizanci_config:apply/1, for servlets newly listed;
%% <code>stop/0</code> at application stop and for servlets no longer
%% listed. They set up and tear down shared resources, such as the
%% ppool queue of blizanci_cgi and blizanci_titan. <code>start/0</code>
%% may return <code>{error, Reason}</code> (or raise); the servlet is
%% then not started, nor are any others in the same start-up or
%% apply, and those already started are stopped again.
%% <code>start/0</code> must be harmless if the resource is already
%% running; <code>stop/0</code> should return <code>ok</code>. If it
%% returns anything else, or raises, a warning is logged and it is
%% otherwise ignored: the servlet is treated as stopped, the other
%% servlets are still stopped, and nothing crashes.</li>
%% <li><code>request/4</code> is called once per request that the
%% router has matched to a route of the servlet and authorised. It runs
%% in the process of the connection (blizanci_connection), via
%% blizanci_servlet_container:request/5, so it should not block for
%% long. It answers <code>{immediate, Response}</code>, a
%% gemini_response() sent to the client as it stands, or
%% <code>defer</code>, which hands the request to the process contract
%% below.</li>
%% </ul>
%%
%% A servlet with no worker, such as blizanci_static, does all its work
%% in <code>request/4</code> and always answers <code>immediate</code>.
%% It need only implement the callbacks of parts 2 and 3 trivially:
%% <code>serve/4</code> is never called (it may return
%% <code>{gateway_error, unimplemented}</code>),
%% <code>handle_client_data/2</code> may return <code>none</code>, and
%% <code>cancel/1</code> may return <code>ok</code>. Conversely a servlet
%% that always defers, as blizanci_cgi and blizanci_titan do, has a
%% trivial <code>request/4</code> returning <code>defer</code>.
%%
%% === The inputs ===
%%
%% <code>request/4</code> and <code>serve/4</code> take the same four
%% arguments: Matches, Request, ServerConfig and RouteOpts.
%%
%% <ul>
%% <li>Matches, a path_matches() map from binary to binary. The router
%% matches the route's regular expression (anchored, except for an
%% old-format route) against the request path, or a mount against its
%% start; Matches holds what each NAMED group of the regular
%% expression captured, keyed by the group's name, and
%% <code>FULL_PATH</code>, the whole request path (for a mount, PATH is
%% what follows the mount); a group that took
%% no part in the match gives <code>&lt;&lt;&gt;&gt;</code>. The path
%% matched is the decoded one: leading slash removed, percent-escapes
%% decoded, normalised, and known to contain no
%% <code>..</code>. The router refuses at route build time any
%% regular expression with no group named <code>PATH</code>, so
%% <code>PATH</code> is always present, whatever the servlet; it is the
%% path the servlet resolves under its own root (see
%% blizanci_path:resolve_request/3). Other named groups pass
%% through unchanged (CGI reads PATH_INFO this way).</li>
%% <li>Request, a request_details() map with keys
%% <code>client_cert</code> (the raw peer certificate, or an error),
%% <code>query</code> (the query string),
%% <code>rest_of_input</code> (whatever the client sent after the
%% request line in the same read, <code>&lt;&lt;&gt;&gt;</code> if
%% nothing; Titan takes the start of the upload body from it),
%% <code>scheme</code> and <code>host</code> (lower-cased),
%% <code>port</code> and <code>path</code> (the request path as sent,
%% before the decoding described above).</li>
%% <li>ServerConfig, the server_config() record of the site serving the
%% request: <code>hostname</code>, <code>port</code> and
%% <code>routing</code> (its list of routes). blizanci_cgi takes the
%% server name and port from it; blizanci_static and blizanci_titan
%% ignore it.</li>
%% <li>RouteOpts, the route's options map, merged with the defaults and
%% validated as described above. The contents are the servlet's own;
%% for example docroot, index and mimetypes for blizanci_static,
%% cgiroot and cgiprefix for blizanci_cgi, docroot and work_dir for
%% blizanci_titan. The router does not interpret them.</li>
%% </ul>
%%
%% == 2. The servlet process contract ==
%%
%% What the servlet does in the CONTAINER process, once
%% <code>request/4</code> has answered <code>defer</code>.
%% blizanci_servlet_container:request/5 then starts and links a new
%% process, a blizanci_servlet_container (the "container"), and the
%% connection traps exits to hear of its end. The servlet's worker, if
%% any, is a further process, started by <code>serve/4</code>
%% (blizanci_cgi and blizanci_titan use a ppool queue); how it and the
%% container talk is the gateway contract, part 3.
%%
%% <ul>
%% <li><code>serve/4</code> is called once, in the container process,
%% straight after the container has told the connection it has started
%% (so the connection is not held up by it, but the container is
%% busy until it returns). It returns one of:
%% <ul>
%% <li><code>{gateway_finished, Response}</code>: done; the container
%% exits and Response, a gemini_response(), goes to the client as it
%% stands.</li>
%% <li><code>{gateway_error, Reason}</code>: the request failed. A
%% Reason which blizanci_status maps to a failure status (for example
%% <code>file_not_found</code> gives 51, <code>gateway_busy</code> 40)
%% is sent to the client as that status; any other Reason is logged
%% and the client gets an internal error (40).</li>
%% <li><code>{gateway_started, Pid}</code>: the work continues in Pid,
%% the worker, and the container stays alive, waiting for it. Pid is
%% what the container later gives to <code>handle_client_data/2</code>
%% and <code>cancel/1</code>.</li>
%% </ul>
%% (<code>gateway_output</code>, also a gateway_result(), is not a
%% valid return from <code>serve/4</code>: the container would crash
%% and the client get an internal error. This is documented as invalid,
%% not endorsed.)</li>
%% </ul>
%%
%% == 3. The gateway contract ==
%%
%% The protocol between the container and the worker (the "gateway")
%% which <code>serve/4</code> started, once it has returned
%% <code>{gateway_started, Pid}</code>. The container is then a
%% gen_server (blizanci_servlet_container) which the connection and the
%% worker talk to, and it remembers Pid and the servlet module. The
%% worker's own messages are hidden behind the servlet module, as a
%% gen_server module hides its protocol: the container never sends the
%% worker anything but through the callbacks below.
%%
%% Container to worker, through the servlet module:
%%
%% <ul>
%% <li><code>handle_client_data/2</code> is called in the container
%% process, once the worker has started, with the client's further
%% input (for example a Titan upload body) as the connection receives
%% it, and the worker's Pid. It returns the gemini_response() to send
%% to the client, or <code>none</code> if more is awaited. If it
%% raises, the container is stopped and the client gets an internal
%% error. A servlet which takes no further input returns
%% <code>none</code>.</li>
%% <li><code>cancel/1</code> is called in the container process when
%% the connection is shutting down before the work is over (for
%% example the client hung up), with the worker's Pid. It should check
%% that Pid is still a live process, and promptly stop it. The
%% container then stops. It is not called if <code>serve/4</code> did
%% not return <code>gateway_started</code>.</li>
%% </ul>
%%
%% Worker to container, which is NOT a callback: the cast
%% blizanci_servlet_container:gateway_exit(ContainerPid, Result). A
%% worker which does not answer through
%% <code>handle_client_data/2</code> must itself report
%% how it ended, exactly once, by calling
%% blizanci_servlet_container:gateway_exit/2 with the container's pid
%% (the pid of the process that called <code>serve/4</code>, which the
%% worker has to be given) and one of
%% <code>{gateway_output, Binary}</code>, the finished response as the
%% script produced it, or <code>{gateway_error, Reason}</code>, treated
%% as for <code>serve/4</code>. gateway_exit/2 is a cast, harmless if
%% the container has gone. The container turns the report into a
%% message to the connection, which answers the client and closes the
%% connection. A worker that never reports leaves the client waiting
%% for the connection's own timeout.
%%
%% === Who runs what ===
%%
%% <pre>
%% callback              layer    process                  when
%% default_options/0     1 module route builder            config, apply
%% validate_options/1    1 module route builder            config, apply
%% start/0               1 module blizanci_servlets        start, apply
%% stop/0                1 module blizanci_servlets        stop, apply
%% request/4             1 module connection               each request
%% serve/4               2 proc   container (new, linked)  after defer
%% handle_client_data/2  3 gway   container                later input
%% cancel/1              3 gway   container                conn closing
%% </pre>
%%
%% == Known limits ==
%%
%% This section describes the current behaviour, which may change.
%%
%% There are thus two reply paths, as the code stands. CGI-style
%% workers (blizanci_cgi) report with gateway_exit/2, and
%% <code>handle_client_data/2</code> returns <code>none</code>.
%% blizanci_titan never calls gateway_exit/2: it answers its client
%% through the return value of <code>handle_client_data/2</code>, which
%% is <code>none</code> while more data is awaited and the response once
%% the upload is over.
%%
%% Not every gateway_result() is valid on every path. The container has
%% no case clause for some, and would crash (the client getting an
%% internal error). Invalid on the <code>serve/4</code> path (part 2):
%% <code>{gateway_output, Binary}</code>. Invalid as the argument of
%% gateway_exit/2: <code>{gateway_finished, Response}</code> and
%% <code>{gateway_started, Pid}</code>; only
%% <code>{gateway_output, Binary}</code> and
%% <code>{gateway_error, Reason}</code> are valid there. These are
%% documented as invalid, not endorsed: the gateway_result() type is
%% wider than either use.
%%
%% @end

-module(blizanci_servlet).
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
