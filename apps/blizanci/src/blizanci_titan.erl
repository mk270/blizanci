%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_titan).

-moduledoc """
A servlet accepting Titan uploads into a route's docroot, staged in
the route's work_dir.

See the manual chapter [Titan and static files](titan-and-static.md).
""".
-behaviour(blizanci_servlet).

-include("blizanci_types.hrl").
-include_lib("kernel/include/file.hrl").
-include("blizanci_titan.hrl").

-behaviour(gen_server).

-export([serve/4, start/0, stop/0, cancel/1, request/4, default_options/0,
         validate_options/1, handle_client_data/2]).
-export([start_link/1]).

-ifdef(TEST).
%% Exported for the tests in test/blizanci_titan_tests.erl.
-export([create_tmp_file/2, delete/1, discard/1, finish_file/1,
         handle_upload_status/1, parse_titan_request/1, recv_data/2]).
-endif.

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-type options() :: #{
                     docroot := binary(),
                     work_dir := binary()
                    }.

-define(QUEUE, ?MODULE).

% The most Titan uploads being processed at once (the worker pool size).
-define(MAX_TITAN, 10).

% How old a leftover temporary file must be before it is expired, in
% seconds (one hour).
-define(MAX_AGE, 3600).
-define(DEFAULT_MIME, <<"text/gemini">>). % per the Titan specification

% Hidden files are refused, as everywhere, except that a ".well-known"
% directory at the top of the upload root may be written to.
-define(CONFINE_OPTS, #{ well_known => true }).

-type upload_status() :: titan_finished
                       | titan_enotdir
                       | titan_not_found
                       | titan_too_much
                       | titan_failed
                       | {titan_updated, integer()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec default_options() -> Result
              when Result :: map().
default_options() ->
    #{
      docroot => <<"docroot">>
     }.


-spec validate_options(Opts) -> Result
              when Opts   :: map(),
                   Result :: 'ok' | {'error', term()}.
validate_options(#{ docroot := Docroot, work_dir := WorkDir })
  when is_binary(Docroot), is_binary(WorkDir) ->
    case {filelib:is_dir(filename:absname(Docroot)),
          filename:pathtype(WorkDir)} of
        {false, _} -> {error, {docroot_not_a_directory, Docroot}};
        {true, absolute} -> check_work_dir(Docroot, WorkDir);
        {true, _} -> {error, {work_dir_not_absolute, WorkDir}}
    end;
validate_options(#{ docroot := _ } = Opts)
  when not is_map_key(work_dir, Opts) ->
    {error, work_dir_missing};
validate_options(Opts) ->
    {error, {invalid_options, Opts}}.

% The work directory must exist and be on the docroot's filesystem,
% as uploads are installed by hard link and rename.
-spec check_work_dir(Docroot, WorkDir) -> Result
              when Docroot :: filepath(),
                   WorkDir :: filepath(),
                   Result  :: 'ok' | {'error', term()}.
check_work_dir(Docroot, WorkDir) ->
    case {filelib:is_dir(WorkDir), device(WorkDir), device(Docroot)} of
        {false, _, _} -> {error, {work_dir_not_a_directory, WorkDir}};
        {true, Dev, Dev} -> ok;
        {true, _, _} -> {error, {work_dir_other_filesystem, WorkDir}}
    end.

-spec device(Path) -> Result
              when Path   :: filepath(),
                   Result :: term().
device(Path) ->
    case file:read_file_info(filename:absname(Path)) of
        {ok, #file_info{major_device = Maj, minor_device = Min}} ->
            {Maj, Min};
        {error, Reason} -> {unreadable, Reason}
    end.

-spec start() -> Result
              when Result :: 'ok' | {'error', term()}.
start() ->
    blizanci_pool:start(?QUEUE, ?MAX_TITAN, ?MODULE).


% The counterpart to start/0; called at application shutdown so the
% Titan ppool doesn't leak across a stop/start cycle in the same VM.
-spec stop() -> Result
              when Result :: ok.
stop() ->
    blizanci_pool:stop(?QUEUE).

-spec start_link(Args) -> Result
              when Args   :: any(),
                   Result :: 'ignore'
                           | {'error', _}
                           | {'ok', pid() | {pid(), reference()}}.
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).


-spec cancel(Pid) -> Result
              when Pid    :: pid(),
                   Result :: ok.
cancel(Pid) ->
    blizanci_pool:cancel(Pid, titan_quit).

% Called by blizanci_servlet_container, in the connection process
%
-spec request(Matches, Req, ServerConfig, RouteOpts) -> Result
              when Matches      :: path_matches(),
                   Req          :: request_details(),
                   ServerConfig :: server_config(),
                   RouteOpts    :: options(),
                   Result       :: {'immediate', gemini_response()}
                                 | 'defer'.
request(_, _, _, _) ->
    defer.


-spec serve(Matches, Req, ServerConfig, RouteOpts) -> Result
              when Matches      :: path_matches(),
                   Req          :: request_details(),
                   ServerConfig :: server_config(),
                   RouteOpts    :: options(),
                   Result       :: gateway_result().

serve(Matches, Req, _ServerConfig, RouteOpts) ->
    #{ <<"PATH">> := Fragment } = Matches,
    #{ rest_of_input := Rest } = Req,
    #{ work_dir := WorkDirBase,
       docroot := RootDirBase } = RouteOpts,

    WorkDir = filename:absname(WorkDirBase),
    RootDir = filename:absname(RootDirBase),
    serve_titan_request(Fragment, Rest, WorkDir, RootDir).


-spec serve_titan_request(Fragment, Rest, WorkDir, RootDir) -> Result
              when Fragment :: binary(),
                   Rest     :: binary(),
                   WorkDir  :: filepath(),
                   RootDir  :: filepath(),
                   Result   :: gateway_result().

serve_titan_request(Fragment, Rest, WorkDir, RootDir)
  when is_binary(Rest), is_binary(WorkDir), is_binary(RootDir)->
    case parse_and_confine(Fragment, RootDir) of
        {ok, TitanReq, TargetPath} ->
            dispatch_titan_request(TitanReq, TargetPath, Rest,
                                   WorkDir, RootDir);
        {error, Err} ->
            {gateway_finished, {error_code, Err}}
    end.


% Parse the request (the parameters are split off the raw path before it
% is decoded: see parse_titan_request/1), then canonicalise-and-confine
% the upload target under RootDir up front, before any worker (sync or
% queued) is started, the same mechanism CGI and static-file serving
% use. This also catches a symlink inside RootDir that would otherwise
% let an upload land outside it. It is only an early refusal: the target
% is resolved again, just before it is written (see finish_file/1),
% because a long upload leaves time for the directory tree to change.
-spec parse_and_confine(Fragment, RootDir) -> Result
              when Fragment :: binary(),
                   RootDir  :: filepath(),
                   Result   :: {ok, TitanReq, TargetPath}
                             | {error, atom()},
                   TitanReq   :: titan_request(),
                   TargetPath :: binary().

parse_and_confine(Fragment, RootDir) ->
    case parse_titan_request(Fragment) of
        {ok, #titan_request{path=Path} = TitanReq} ->
            case blizanci_path:resolve_request(#{ <<"PATH">> => Path },
                                               RootDir, ?CONFINE_OPTS) of
                {ok, TargetPath} -> {ok, TitanReq, TargetPath};
                {error, Err} -> {error, Err}
            end;
        {error, Err} ->
            {error, Err}
    end.


-spec dispatch_titan_request(TitanReq, TargetPath, Rest, WorkDir,
                             RootDir) -> Result
              when TitanReq   :: titan_request(),
                   TargetPath :: binary(),
                   Rest       :: binary(),
                   WorkDir    :: filepath(),
                   RootDir    :: filepath(),
                   Result     :: gateway_result().

dispatch_titan_request(#titan_request{size=Size}, _TargetPath,
                       Rest, _WorkDir, _RootDir)
  when byte_size(Rest) > Size ->
    % more data than the request declared: refused before anything is
    % written
    {gateway_finished, {error_code, request_too_long}};
dispatch_titan_request(#titan_request{size=0}, TargetPath,
                       _Rest, _WorkDir, _RootDir) ->
    {gateway_finished, delete_resource(TargetPath)};
dispatch_titan_request(#titan_request{size=Size} = TitanReq,
                       _TargetPath, Rest, WorkDir, RootDir) ->
    Job = #titan_job{request=TitanReq,
                     rest=Rest,
                     work_dir=WorkDir,
                     root_dir=RootDir},
    case byte_size(Rest) >= Size of
        true ->
            handle_all_in_one_request(Job);
        false ->
            enqueue_titan_job(Job)
    end.


% A request of zero bytes is how the Titan specification says to delete
% a resource. Only a regular file is deleted: a resource is a file, so
% a directory (or anything else) is reported as not found. As an upload
% does, the target is the file the path resolves to inside the root.
% (A zero-size request that carries data has already been refused, like
% any request that sends more than it declared.)
-spec delete_resource(TargetPath) -> Result
              when TargetPath :: filepath(),
                   Result     :: gemini_response().

delete_resource(TargetPath) ->
    case file:read_link_info(TargetPath) of
        {ok, #file_info{type = regular}} ->
            case file:delete(TargetPath) of
                ok -> {success, <<"text/plain">>, <<"Deleted.\r\n">>};
                {error, enoent} -> {error_code, file_not_found};
                {error, Reason} ->
                    logger:warning("titan: cannot delete ~p: ~p",
                                   [TargetPath, Reason]),
                    {error_code, internal_server_error}
            end;
        {ok, #file_info{}} ->
            {error_code, file_not_found};
        {error, Reason} when Reason =:= enoent; Reason =:= enotdir ->
            {error_code, file_not_found};
        {error, Reason} ->
            logger:warning("titan: cannot stat ~p: ~p",
                           [TargetPath, Reason]),
            {error_code, internal_server_error}
    end.


% deal with the following case:
%  a valid Titan request has been submitted, but the network data arrives
%  concurrently with the payload; that is, the Titan request specifies
%  that a certain number of bytes of payload will follow, and sufficient
%  bytes are already available.
-spec handle_all_in_one_request(Job) -> Result
              when Job    :: titan_job(),
                   Result :: gateway_result().

handle_all_in_one_request(#titan_job{request=TitanReq,
                                     rest=Rest,
                                     work_dir=WorkDir,
                                     root_dir=RootDir}) ->
    #titan_request{path=Path, size=Size} = TitanReq,
    {ok, Stream, TmpPath} = create_tmp_file(WorkDir, Rest),
    Upload = #titan_upload{stream=Stream,
                           path=Path,
                           root_dir=RootDir,
                           tmp_file=TmpPath,
                           size=Size},

    % No Titan worker is involved here, so nothing else would remove the
    % temporary file if finishing the upload went wrong.
    UploadStatus =
        try finish_file(Upload)
        catch
            Class:Reason:Stacktrace ->
                _ = file:close(Stream),
                _ = delete(TmpPath),
                erlang:raise(Class, Reason, Stacktrace)
        end,
    handle_upload_status(UploadStatus).


% the alternative case (to the previous function) is that a valid Titan
% request has been received, but there is either no concurrent payload
% data, or insufficient such data to fulfil the request; this entails
% waiting for more data, therefore we put a Titan worker into the worker
% pool to handle this
-spec enqueue_titan_job(Job) -> Result
              when Job    :: titan_job(),
                   Result :: gateway_result().

enqueue_titan_job(Job) ->
    blizanci_pool:enqueue(?QUEUE, Job).


-spec handle_client_data(Pid, Data) -> Result
              when Pid    :: pid(),
                   Data   :: binary(),
                   Result :: gemini_response().

handle_client_data(Pid, Data) when is_pid(Pid) and is_binary(Data) ->
    case gen_server:call(Pid, {client_data, Data}) of
        {ok, in_progress, _NewSize} -> none;
        {gateway_finished, Response} -> Response
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec parse_titan_request(Fragment) -> Result
              when Fragment :: binary(),
                   Result   :: {ok, titan_request()}
                             | {error, atom()}.

parse_titan_request(Fragment) ->
    case binary:split(Fragment, <<";">>) of
        [_Single] -> {error, bad_query_string};
        [Path, Query] -> parse_titan_qs(Path, Query);
        _ -> {error, internal_server_error}
    end.


-spec parse_titan_qs(Path, Query) -> Result
              when Path   :: binary(),
                   Query  :: binary(),
                   Result :: {ok, titan_request()}
                             | {error, atom()}.

parse_titan_qs(Path, Query)
  when is_binary(Path) and is_binary(Query)
 ->
    Q2 = binary:replace(Query, <<";">>, <<"&">>, [global]),
    AllParams = uri_string:dissect_query(Q2),
    BinParams = [{K,V} || {K,V} <- AllParams, is_binary(V)],
    KVPs = maps:from_list(BinParams),
    case parse_size(maps:get(<<"size">>, KVPs, undefined)) of
        {ok, Size} ->
            MimeType = maps:get(<<"mime">>, KVPs, ?DEFAULT_MIME),
            {ok, #titan_request{path=Path, size=Size, mime=MimeType}};
        {error, _} = Error ->
            Error
    end.


% The size parameter is mandatory, and a number of bytes: so a missing,
% non-numeric or negative size is a bad request. Zero is valid (the
% Titan specification uses it to mean "delete"). A size over the limit
% is refused outright.
-spec parse_size(SizeS) -> Result
              when SizeS  :: binary() | 'undefined',
                   Result :: {ok, non_neg_integer()}
                             | {error, atom()}.
parse_size(undefined) ->
    {error, bad_query_string};
parse_size(SizeS) when is_binary(SizeS) ->
    try erlang:binary_to_integer(SizeS) of
        Size when Size > ?MAX_UPLOAD_BYTES -> {error, request_too_long};
        Size when Size >= 0 -> {ok, Size};
        _Negative -> {error, bad_query_string}
    catch
        error:badarg -> {error, bad_query_string}
    end.

init({Parent, Job}) ->
    process_flag(trap_exit, true),
    #titan_job{request=TitanReq,
               rest=Rest,
               work_dir=WorkDir,
               root_dir=RootDir} = Job,
    #titan_request{path=Path, size=Size, mime=MimeType} = TitanReq,
    BytesRecv = byte_size(Rest),
    {ok, Stream, TmpPath} = create_tmp_file(WorkDir, Rest),
    Upload = #titan_upload{stream=Stream,
                           path=Path,
                           root_dir=RootDir,
                           tmp_file=TmpPath,
                           size=Size},
    State = #titan_state{
               parent=Parent,
               mime_type=MimeType,
               bytes_recv=BytesRecv,
               work_dir=WorkDir,
               upload=Upload
              },
    {ok, State}.


handle_call({client_data, Data}, _From, State) ->
    do_handle_client_data(Data, State);
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(titan_quit, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(Info, State) ->
    logger:info("OOB msg: ~p", [Info]),
    {noreply, State}.


terminate(normal, #titan_state{upload=#titan_upload{tmp_file=TmpPath},
                               work_dir=WorkDir}) ->
    ok = purge(TmpPath, WorkDir),
    ok;
terminate(Reason, #titan_state{upload=#titan_upload{tmp_file=TmpPath},
                               work_dir=WorkDir}) ->
    logger:info("Titan queue worker ~p terminating: [[~p]]", [self(), Reason]),
    ok = purge(TmpPath, WorkDir),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec do_handle_client_data(Data, State) -> Result
              when Data   :: binary(),
                   State  :: titan_state(),
                   Result :: any().

do_handle_client_data(Data, State) ->
    UploadStatus = recv_data(Data, State),
    Reply = handle_upload_status(UploadStatus),
    case Reply of
        {gateway_finished, _GatewayStatus} ->
            {stop, normal, Reply, State};
        {ok, in_progress, NewSize} ->
            {reply, Reply, State#titan_state{bytes_recv=NewSize}}
    end.


-spec handle_upload_status(Status) -> Result
              when Status :: upload_status(),
                   Result :: gateway_result()
                           | {ok, in_progress, integer() }.

handle_upload_status(titan_finished) ->
    {gateway_finished, {success, <<"text/plain">>, <<"Uploaded.\r\n">>}};
handle_upload_status(titan_enotdir) ->
    {gateway_finished, {error_code, cannot_overwrite}};
handle_upload_status(titan_not_found) ->
    {gateway_finished, {error_code, file_not_found}};
handle_upload_status(titan_too_much) ->
    {gateway_finished, {error_code, request_too_long}};
handle_upload_status(titan_failed) ->
    {gateway_finished, {error_code, internal_server_error}};
handle_upload_status({titan_updated, NewSize}) ->
    {ok, in_progress, NewSize}.


-spec recv_data(Data, State) -> Result
              when Data   :: binary(),
                   State  :: titan_state(),
                   Result :: upload_status().

recv_data(Data, #titan_state{upload=Upload,
                             bytes_recv=OldBytesRecv}) ->
    #titan_upload{stream=Stream, tmp_file=TmpPath, size=Size} = Upload,
    NewSize = OldBytesRecv + byte_size(Data),
    if
        NewSize > Size ->
            % The client sent more than it declared: nothing is
            % installed, and what was received is thrown away.
            ok = file:close(Stream),
            discard(TmpPath),
            titan_too_much;
        NewSize =:= Size ->
            ok = file:write(Stream, Data),
            finish_file(Upload);
        true ->
            ok = file:write(Stream, Data),
            {titan_updated, NewSize}
    end.


% Put the uploaded data in place. The target is resolved here, under
% RootDir, immediately before it is replaced, and not taken from the
% check made when the request arrived: an upload can take minutes, and
% in that time a directory on the path could have been replaced by a
% symbolic link pointing out of RootDir. This narrows the gap between
% checking the path and using it to the few calls below; it can't close
% it, since Erlang has no way to open a file without following links.
-spec finish_file(Upload) -> Result
              when Upload :: titan_upload(),
                   Result :: upload_status().

finish_file(#titan_upload{stream=Stream,
                          path=Path,
                          root_dir=RootDir,
                          tmp_file=TmpPath,
                          size=Size}) ->
    truncate(Stream, Size),
    ok = file:close(Stream),
    case blizanci_path:confine(Path, RootDir, ?CONFINE_OPTS) of
        {ok, TargetPath} ->
            replace_target(TargetPath, TmpPath);
        {error, file_not_found} ->
            discard(TmpPath),
            titan_not_found
    end.


% Install the finished upload as the target. The old resource is never
% removed first: the new content is linked into the target's own
% directory under a hidden, unique name (so a failure to link, such as
% exdev when the work dir is on another filesystem, changes nothing),
% and only then renamed over the target, which replaces it in one step.
% Whatever happens, the temporary file is gone afterwards.
-spec replace_target(TargetPath, TmpPath) -> Result
              when TargetPath :: filepath(),
                   TmpPath    :: filepath(),
                   Result     :: upload_status().

replace_target(TargetPath, TmpPath) ->
    Staging = staging_path(TargetPath),
    Status = case file:make_link(TmpPath, Staging) of
                 ok -> install(Staging, TargetPath);
                 {error, Reason} -> link_failed(Reason, TmpPath, Staging)
             end,
    case delete(TmpPath) of
        ok -> ok;
        Error -> logger:warning("titan: cannot remove ~p: ~p",
                                [TmpPath, Error])
    end,
    Status.


-spec install(Staging, TargetPath) -> Result
              when Staging    :: filepath(),
                   TargetPath :: filepath(),
                   Result     :: upload_status().

install(Staging, TargetPath) ->
    case file:rename(Staging, TargetPath) of
        ok ->
            titan_finished;
        {error, Reason} ->
            _ = delete(Staging),
            case Reason of
                % the target is a directory: it is not replaced
                eisdir -> titan_enotdir;
                enotdir -> titan_enotdir;
                _ -> upload_failed(TargetPath, Reason)
            end
    end.


-spec link_failed(Reason, TmpPath, Staging) -> Result
              when Reason  :: atom(),
                   TmpPath :: filepath(),
                   Staging :: filepath(),
                   Result  :: upload_status().

% a file where a directory is needed: the same refusal as before
link_failed(enotdir, _TmpPath, _Staging) -> titan_enotdir;
link_failed(enoent, _TmpPath, _Staging) -> titan_not_found;
link_failed(Reason, TmpPath, Staging) ->
    upload_failed({TmpPath, Staging}, Reason).


-spec upload_failed(What, Reason) -> Result
              when What   :: term(),
                   Reason :: term(),
                   Result :: upload_status().

upload_failed(What, Reason) ->
    logger:warning("titan: cannot install upload ~p: ~p", [What, Reason]),
    titan_failed.


% A name in the target's own directory, so that renaming it over the
% target stays on one filesystem. Dot-named, so it is never served.
-spec staging_path(TargetPath) -> Result
              when TargetPath :: filepath(),
                   Result     :: filepath().

staging_path(TargetPath) ->
    Name = lists:flatten(io_lib:format(".blizanci-new-~s-~B-~B",
                                       [os:getpid(),
                                        erlang:system_time(microsecond),
                                        erlang:unique_integer([positive])])),
    filename:join(filename:dirname(TargetPath), Name).


-spec delete(Path) -> Result
              when Path   :: filepath(),
                   Result :: ok | {error, atom()}.

delete(Path) ->
    case file:delete(Path) of
        ok -> ok;
        {error, enoent} -> ok;
        {error, _} = Error -> Error
    end.


% Remove a temporary file that is no longer wanted. If it can't be
% removed the upload's outcome is unchanged: this only logs it.
-spec discard(Path) -> Result
              when Path   :: filepath(),
                   Result :: ok.

discard(Path) ->
    case delete(Path) of
        ok -> ok;
        {error, Reason} ->
            logger:warning("titan: cannot remove ~p: ~p", [Path, Reason]),
            ok
    end.


-spec truncate(Stream, Position) -> Result
              when Stream   :: io_device(),
                   Position :: integer(),
                   Result   :: 'ok'.

truncate(Stream, Position) when is_integer(Position) ->
    {ok, _Pos} = file:position(Stream, Position),
    ok = file:truncate(Stream).


%% purge work dir of obviously left-over old files
-spec purge(Path, WorkDir) -> Result
              when Path    :: filepath(),
                   WorkDir :: filepath(),
                   Result  :: 'ok'.

purge(Path, WorkDir) when is_binary(Path) and is_binary(WorkDir) ->
    delete(Path),
    logger:debug("Purging: ~p, ~p", [Path, WorkDir]),
    [ delete(F) || F <- blizanci_tmpdir:stale(WorkDir, ?MAX_AGE) ],
    ok.


% The target path is resolved by the caller (serve_titan_request/4),
% via blizanci_path:confine/2, before a worker is ever started; this
% function only needs a place to stream the incoming bytes to.
-spec create_tmp_file(WorkDir, Rest) -> Result
              when WorkDir :: filepath(),
                   Rest    :: binary(),
                   Result  :: {ok, io_device(), filepath()}.

create_tmp_file(WorkDir, Rest)
  when is_binary(WorkDir) and is_binary(Rest)
 ->
    {ok, Stream, TmpPath} = blizanci_tmpdir:create(WorkDir),
    logger:debug("titan tmp: ~p", [TmpPath]),
    ok = file:write(Stream, Rest),
    {ok, Stream, TmpPath}.
