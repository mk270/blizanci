%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

%% Types, records and macros shared by blizanci_titan and its tests.

-type io_device() :: pid() | {'file_descriptor', atom(), any()}.
-type filepath() :: binary().

% The largest upload accepted, in bytes: a request declaring more is
% refused before any of its data is read.
-define(MAX_UPLOAD_BYTES, (10 * 1024 * 1024)). % 10 MiB

% Everything finish_file/1 needs to put a completed upload in place;
% shared by the all-in-one path and the queued worker.
-record(titan_upload, {stream   :: io_device(),
                       path     :: binary(),
                       root_dir :: filepath(),
                       tmp_file :: filepath(),
                       size     :: integer()}).
-type titan_upload() :: #titan_upload{}.

-record(titan_state, {parent,
                      mime_type,
                      bytes_recv,
                      work_dir,
                      upload :: titan_upload()}).
-type titan_state() :: #titan_state{}.

-record(titan_request, {path :: binary(),
                        size :: integer(),
                        mime :: binary()}).
-type titan_request() :: #titan_request{}.

-record(titan_job, {request     :: titan_request(),
                    rest        :: binary(),
                    work_dir    :: filepath(),
                    root_dir    :: filepath()}).
-type titan_job() :: #titan_job{}.
