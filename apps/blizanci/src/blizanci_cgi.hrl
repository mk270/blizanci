%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

%% Records and macros shared by blizanci_cgi and its tests. Include
%% blizanci_types.hrl first.

-record(cgi_proc, {pid, os_pid, buffer, script}).

% The per-request inputs from which the CGI environment is built.
-record(cgi_req, {cmd       :: binary() | string(),
                  path      :: binary(),
                  matches   :: path_matches(),
                  req       :: request_details(),
                  host_port :: {binary(), integer()},
                  prefix    :: string()}).
-type cgi_req() :: #cgi_req{}.
-record(worker_state, {parent, parent_ref, cgi_status}).
-type worker_state() :: #worker_state{}.

% The most bytes of a script's stderr put in a single log entry.
-define(MAX_STDERR_LOG_BYTES, 512).

% A note re environment variables.
%
% The CGI child's environment is built from scratch on each request
% (erlexec's {env, [clear | Env]} option -- see enqueue_cgi/2), not
% inherited from the Erlang VM's own OS environment.  The variables
% that are preserved (?ALLOWED_ENV, below) and those set per request
% (see make_environment/1) are listed in the manual chapter 'CGI'
% (doc/manual/cgi.gmi), as are the variables the Gemini App Guide
% suggests.

-define(ALLOWED_ENV,
        ["HOME",
         "USER",
         "PATH",
         "LOGNAME",
         "SHELL"]).
