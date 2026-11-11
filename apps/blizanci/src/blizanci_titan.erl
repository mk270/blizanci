%%%-------------------------------------------------------------------

%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

% TBD: module-level docs
%
% The Titan protocol, for uploading to Gemini servers, is specified at
%   gemini://transjovian.org/titan/The%20Titan%20Specification
% In it the "size" parameter is mandatory, "mime" is optional (default
% text/gemini) and "token" is optional. A request with size=0 deletes
% the resource.
-module(blizanci_titan).
-behaviour(blizanci_servlet).

-include("blizanci_types.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").
-import(blizanci_test_support, [test_request/1]).

-behaviour(gen_server).

-export([serve/4, start/0, stop/0, cancel/1, request/4, default_options/0,
         validate_options/1, handle_client_data/2]).
-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-type options() :: #{
                     docroot := string(),
                     work_dir := binary()
                    }.

-type io_device() :: pid() | {'file_descriptor', atom(), any()}.
-type filepath() :: binary() | string().

-define(QUEUE, ?MODULE).
-define(MAX_TITAN, 10).

-define(MAX_AGE, 3600). % for expiring files; measured in seconds
-define(DEFAULT_MIME, <<"text/gemini">>). % per the Titan specification

% Hidden files are refused, as everywhere, except that a ".well-known"
% directory at the top of the upload root may be written to.
-define(CONFINE_OPTS, #{ well_known => true }).

% The largest upload accepted, in bytes: a request declaring more is
% refused before any of its data is read.
% TODO: plumb this through to the config system one day, as a route
% option so each Titan route can set its own limit, instead of being
% fixed here at compile time.
-define(MAX_UPLOAD_BYTES, (10 * 1024 * 1024)). % 10 MiB

-record(titan_state, {parent,
                      size,
                      mime_type,
                      bytes_recv,
                      tmp_file,
                      path,
                      root_dir,
                      work_dir,
                      stream}).
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
      work_dir => <<"titan-temp">>,
      docroot => "docroot"
     }.


-spec validate_options(Opts) -> Result
              when Opts   :: map(),
                   Result :: 'ok' | {'error', term()}.
validate_options(#{ docroot := Docroot, work_dir := WorkDir })
  when is_binary(Docroot) orelse is_list(Docroot),
       is_binary(WorkDir) orelse is_list(WorkDir) ->
    case {filelib:is_dir(filename:absname(Docroot)),
          filelib:is_dir(filename:absname(WorkDir))} of
        {true, true} -> ok;
        {false, _} -> {error, {docroot_not_a_directory, Docroot}};
        {_, false} -> {error, {work_dir_not_a_directory, WorkDir}}
    end;
validate_options(Opts) ->
    {error, {invalid_options, Opts}}.

-spec start() -> Result
              when Result :: ok.
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

% Called by the servlet
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

    WorkDir = filename:absname(ensure_binary(WorkDirBase)),
    RootDir = filename:absname(ensure_binary(RootDirBase)),
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
% is resolved again, just before it is written (see finish_file/5),
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
dispatch_titan_request(#titan_request{path=Path, size=Size} = TitanReq,
                       _TargetPath, Rest, WorkDir, RootDir) ->
    case byte_size(Rest) >= Size of
        true ->
            handle_all_in_one_request(WorkDir, RootDir, Path, Rest, Size);
        false ->
            enqueue_titan_job(#titan_job{request=TitanReq,
                                         rest=Rest,
                                         work_dir=WorkDir,
                                         root_dir=RootDir})
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
-spec handle_all_in_one_request(WorkDir, RootDir, Path, Rest, Size) -> Result
              when WorkDir :: filepath(),
                   RootDir :: filepath(),
                   Path    :: binary(),
                   Rest    :: binary(),
                   Size    :: integer(),
                   Result  :: gateway_result().

handle_all_in_one_request(WorkDir, RootDir, Path, Rest, Size) ->
    {ok, Stream, TmpPath} = create_tmp_file(WorkDir, Rest),

    % No Titan worker is involved here, so nothing else would remove the
    % temporary file if finishing the upload went wrong.
    UploadStatus =
        try finish_file(Stream, Path, RootDir, TmpPath, Size)
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


-spec ensure_binary(Input) -> Result
              when Input  :: list()
                           | binary(),
                   Result :: binary().
ensure_binary(X) when is_list(X) -> binary:list_to_bin(X);
ensure_binary(X) when is_binary(X) -> X.


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
    State = #titan_state{
               parent=Parent,
               size=Size,
               mime_type=MimeType,
               bytes_recv=BytesRecv,
               tmp_file=TmpPath,
               path=Path,
               root_dir=RootDir,
               work_dir=WorkDir,
               stream=Stream
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


terminate(normal, #titan_state{tmp_file=TmpPath, work_dir=WorkDir}) ->
    ok = purge(TmpPath, WorkDir),
    ok;
terminate(Reason, #titan_state{tmp_file=TmpPath, work_dir=WorkDir}) ->
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

recv_data(Data, #titan_state{stream=Stream,
                             size=Size,
                             path=Path,
                             root_dir=RootDir,
                             tmp_file=TmpPath,
                             bytes_recv=OldBytesRecv}) ->
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
            finish_file(Stream, Path, RootDir, TmpPath, Size);
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
-spec finish_file(Stream, Path, RootDir, TmpPath, Size) -> Result
              when Stream  :: io_device(),
                   Path    :: binary(),
                   RootDir :: filepath(),
                   TmpPath :: filepath(),
                   Size    :: integer(),
                   Result  :: upload_status().

finish_file(Stream, Path, RootDir, TmpPath, Size) ->
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_titan_request_test_() ->
    P = fun(Params) ->
            parse_titan_request(<<"dir/page;", Params/binary>>)
        end,
    [?_assertEqual({ok, #titan_request{path= <<"dir/page">>, size=123,
                            mime= <<"text/plain">>}},
                   P(<<"size=123;mime=text/plain">>)),
     ?_assertEqual({ok, #titan_request{path= <<"dir/page">>, size=0,
                            mime= <<"text/plain">>}},
                   P(<<"size=0;mime=text/plain">>)),
     % mime is optional and defaults to text/gemini
     ?_assertEqual({ok, #titan_request{path= <<"dir/page">>, size=5,
                            mime= <<"text/gemini">>}},
                   P(<<"size=5">>)),
     ?_assertEqual({error, bad_query_string},
                   P(<<"size=-1;mime=text/plain">>)),
     ?_assertEqual({error, bad_query_string},
                   P(<<"size=-0123456789;mime=text/plain">>)),
     % the limit itself is allowed; one byte more is not
     ?_assertMatch({ok, #titan_request{size=?MAX_UPLOAD_BYTES}},
                   P(list_to_binary(
                       "size=" ++ integer_to_list(?MAX_UPLOAD_BYTES)))),
     ?_assertEqual({error, request_too_long},
                   P(list_to_binary(
                       "size=" ++ integer_to_list(?MAX_UPLOAD_BYTES + 1)))),
     ?_assertEqual({error, request_too_long},
                   P(<<"size=1000000000000000000000000000000">>)),
     ?_assertEqual({error, bad_query_string},
                   P(<<"size=abc;mime=text/plain">>)),
     ?_assertEqual({error, bad_query_string},
                   P(<<"size=;mime=text/plain">>)),
     ?_assertEqual({error, bad_query_string},
                   P(<<"mime=text/plain">>)),
     ?_assertEqual({error, bad_query_string},
                   parse_titan_request(<<"dir/page">>))].


%% finish_file/5 resolves the target afresh, so a path that has become
%% unsafe since the request was checked is refused, not written.
%% Fixture, under a fresh temporary directory:
%%   Base/root/existing.txt  Base/root/escape -> ../outside  Base/outside
%%   Base/work
finish_file_test_() ->
    {setup, fun finish_setup/0, fun finish_teardown/1, fun finish_tests/1}.

finish_setup() ->
    Base = blizanci_test_support:tmp_path("titan_test"),
    [ ok = filelib:ensure_path(filename:join(Base, D))
      || D <- ["root", "outside", "work"] ],
    ok = file:write_file(filename:join([Base, "root", "existing.txt"]),
                         <<"old">>),
    ok = file:make_symlink("../outside",
                           filename:join([Base, "root", "escape"])),
    Base.

finish_teardown(Base) ->
    file:del_dir_r(Base).

finish_tests(Base) ->
    Root = list_to_binary(filename:join(Base, "root")),
    Work = list_to_binary(filename:join(Base, "work")),
    Upload = fun(Path, Data) ->
                 {ok, Stream, Tmp} = create_tmp_file(Work, Data),
                 {finish_file(Stream, Path, Root, Tmp, byte_size(Data)),
                  Tmp}
             end,
    InRoot = fun(Name) -> filename:join([Base, "root", Name]) end,
    [?_test(begin
                {Status, Tmp} = Upload(<<"new.txt">>, <<"hello">>),
                ?assertEqual(titan_finished, Status),
                ?assertEqual({ok, <<"hello">>},
                             file:read_file(InRoot("new.txt"))),
                ?assertNot(filelib:is_file(Tmp))
            end),
     ?_test(begin
                {Status, _} = Upload(<<"existing.txt">>, <<"new">>),
                ?assertEqual(titan_finished, Status),
                ?assertEqual({ok, <<"new">>},
                             file:read_file(InRoot("existing.txt")))
            end),
     % a directory on the path now leads out of the root
     ?_test(begin
                {Status, Tmp} = Upload(<<"escape/pwned.txt">>, <<"x">>),
                ?assertEqual(titan_not_found, Status),
                ?assertNot(filelib:is_file(
                             filename:join([Base, "outside", "pwned.txt"]))),
                ?assertNot(filelib:is_file(Tmp))
            end),
     % the race itself: the request-time check passes while "swap" is a
     % real directory, then it is replaced by a link out of the root
     % before the upload finishes
     ?_test(begin
                Swap = InRoot("swap"),
                ok = filelib:ensure_path(Swap),
                ?assertMatch({ok, _},
                             blizanci_path:confine(<<"swap/f.txt">>, Root)),
                ok = file:del_dir(Swap),
                ok = file:make_symlink("../outside", Swap),
                {Status, _} = Upload(<<"swap/f.txt">>, <<"x">>),
                ?assertEqual(titan_not_found, Status),
                ?assertNot(filelib:is_file(
                             filename:join([Base, "outside", "f.txt"])))
            end),
     ?_assertEqual({gateway_finished, {error_code, file_not_found}},
                   handle_upload_status(titan_not_found))].


%% A failed install leaves the old page alone and the temporary file
%% removed, whichever way the upload arrived. Fixture, under a fresh
%% temporary directory:
%%   Base/root/{old.txt,dir/inner.txt}  Base/work
%% and, where /dev/shm is a different filesystem, Shm/work (the
%% work_dir of the cross-device case, which is how a link from the work
%% dir into the docroot fails with exdev).
failed_install_test_() ->
    {setup, fun failed_setup/0, fun failed_teardown/1, fun failed_tests/1}.

failed_setup() ->
    Base = blizanci_test_support:tmp_path("titan_failed_test"),
    [ ok = filelib:ensure_path(filename:join(Base, D))
      || D <- ["root/dir", "work"] ],
    ok = file:write_file(filename:join([Base, "root", "old.txt"]), <<"old">>),
    ok = file:write_file(filename:join([Base, "root", "dir", "inner.txt"]),
                         <<"inner">>),
    Shm = filename:join("/dev/shm", filename:basename(Base)),
    {Base, other_filesystem(Base, Shm)}.

% {ok, Dir} when a directory can be made on a filesystem that files in
% Base cannot be hard-linked from, else unavailable.
other_filesystem(Base, Shm) ->
    Link = filename:join(Base, "probe"),
    Result = case filelib:ensure_path(Shm) of
                 ok ->
                     ShmProbe = filename:join(Shm, "probe"),
                     ok = file:write_file(ShmProbe, <<>>),
                     case file:make_link(ShmProbe, Link) of
                         {error, exdev} -> {ok, Shm};
                         _ -> file:del_dir_r(Shm), unavailable
                     end;
                 _ -> unavailable
             end,
    file:delete(Link),
    Result.

failed_teardown({Base, Other}) ->
    file:del_dir_r(Base),
    case Other of
        {ok, Shm} -> file:del_dir_r(Shm);
        unavailable -> ok
    end.

failed_tests({Base, Other}) ->
    Root = filename:join(Base, "root"),
    Cfg = #server_config{hostname = <<"h">>, port = 1965,
                         routing = [], docroot = Root},
    Serve = fun(Work, Fragment, Rest) ->
                Opts = #{ docroot => Root, work_dir => list_to_binary(Work) },
                serve(#{ <<"PATH">> => Fragment },
                      test_request(#{ rest_of_input => Rest }), Cfg, Opts)
            end,
    Work = filename:join(Base, "work"),
    Read = fun(Name) -> file:read_file(filename:join(Root, Name)) end,
    % nothing but the files the fixture made: no temporary or staging
    % file in the work dir, and none beside the target
    Clean = fun(WorkDir) ->
                ?assertEqual({ok, []}, file:list_dir(WorkDir)),
                {ok, Names} = file:list_dir(Root),
                ?assertEqual(["dir", "old.txt"], lists:sort(Names))
            end,
    Upload = fun(WorkDir, Path, Data) ->
                 {ok, Stream, Tmp} =
                     create_tmp_file(list_to_binary(WorkDir), Data),
                 finish_file(Stream, Path, list_to_binary(Root), Tmp,
                             byte_size(Data))
             end,
    [% the target is a directory: it is not overwritten, and the upload
     % is refused with the status for "cannot overwrite"
     ?_test(begin
                ?assertEqual(titan_enotdir, Upload(Work, <<"dir">>, <<"x">>)),
                ?assertEqual({ok, <<"inner">>}, Read("dir/inner.txt")),
                Clean(Work)
            end),
     ?_test(begin
                ?assertEqual({gateway_finished,
                              {error_code, cannot_overwrite}},
                             Serve(Work, <<"dir;size=3">>, <<"new">>)),
                ?assertEqual({ok, <<"inner">>}, Read("dir/inner.txt")),
                Clean(Work)
            end),
     % a file where a directory is needed: refused, temporary file removed
     ?_test(begin
                Status = Upload(Work, <<"old.txt/sub">>, <<"x">>),
                ?assert(lists:member(Status,
                                     [titan_enotdir, titan_not_found])),
                ?assertEqual({ok, <<"old">>}, Read("old.txt")),
                Clean(Work)
            end),
     % a failure part-way through is cleaned up, then re-raised
     ?_test(begin
                ?assertError(_, apply(?MODULE, handle_all_in_one_request,
                                      [list_to_binary(Work), not_a_path,
                                       <<"new.txt">>, <<"x">>, 1])),
                Clean(Work)
            end)]
    ++ exdev_tests(Other, Serve, Upload, Read, Clean).

% The work dir is on another filesystem from the docroot, so the link
% from one to the other fails (exdev): the old page must survive, the
% temporary file must go, and the client sees a server error.
exdev_tests(unavailable, _Serve, _Upload, _Read, _Clean) ->
    [];
exdev_tests({ok, Shm}, Serve, Upload, Read, Clean) ->
    Work = filename:join(Shm, "work"),
    [?_test(begin
                ok = filelib:ensure_path(Work),
                ?assertEqual(titan_failed,
                             Upload(Work, <<"old.txt">>, <<"new">>)),
                ?assertEqual({ok, <<"old">>}, Read("old.txt")),
                Clean(Work)
            end),
     ?_test(begin
                ok = filelib:ensure_path(Work),
                ?assertEqual({gateway_finished,
                              {error_code, internal_server_error}},
                             Serve(Work, <<"old.txt;size=3">>, <<"new">>)),
                ?assertEqual({ok, <<"old">>}, Read("old.txt")),
                Clean(Work)
            end),
     % and the same for a page that does not exist yet
     ?_test(begin
                ok = filelib:ensure_path(Work),
                ?assertEqual(titan_failed,
                             Upload(Work, <<"fresh.txt">>, <<"new">>)),
                ?assertEqual({error, enoent}, Read("fresh.txt")),
                Clean(Work)
            end),
     ?_assertEqual({gateway_finished, {error_code, internal_server_error}},
                   handle_upload_status(titan_failed))].


%% size=0 deletes the resource (see delete_resource/2). Driven through
%% serve/4, so path confinement and parsing are exercised too.
%% Fixture, under a fresh temporary directory:
%%   Base/root/{a.txt,b.txt,c.txt,dir/inner.txt,escape -> ../outside}
%%   Base/root/{.env,.git/config,.well-known/old}
%%   Base/outside/secret.txt  Base/work
delete_test_() ->
    {setup, fun delete_setup/0, fun delete_teardown/1, fun delete_tests/1}.

delete_setup() ->
    Base = blizanci_test_support:tmp_path("titan_delete_test"),
    [ ok = filelib:ensure_path(filename:join(Base, D))
      || D <- ["root/dir", "root/.git", "root/.well-known", "outside",
               "work"] ],
    [ ok = file:write_file(filename:join([Base, "root", F]), <<"x">>)
      || F <- ["a.txt", "b.txt", "c.txt", "dir/inner.txt", ".env",
               ".git/config", ".well-known/old"] ],
    ok = file:write_file(filename:join([Base, "outside", "secret.txt"]),
                         <<"secret">>),
    ok = file:make_symlink("../outside",
                           filename:join([Base, "root", "escape"])),
    Base.

delete_teardown(Base) ->
    file:del_dir_r(Base).

delete_tests(Base) ->
    Root = filename:join(Base, "root"),
    Opts = #{ docroot => Root, work_dir => list_to_binary(Base ++ "/work") },
    Cfg = #server_config{hostname = <<"h">>, port = 1965,
                         routing = [], docroot = Root},
    Serve = fun(Fragment, Rest) ->
                serve(#{ <<"PATH">> => Fragment },
                      test_request(#{ rest_of_input => Rest }), Cfg, Opts)
            end,
    Exists = fun(Name) -> filelib:is_file(filename:join(Root, Name)) end,
    [?_test(begin
                ?assert(Exists("a.txt")),
                ?assertEqual({gateway_finished,
                              {success, <<"text/plain">>,
                               <<"Deleted.\r\n">>}},
                             Serve(<<"a.txt;size=0;mime=text/plain">>, <<>>)),
                ?assertNot(Exists("a.txt"))
            end),
     % mime is optional here as elsewhere
     ?_test(begin
                ?assertMatch({gateway_finished, {success, _, _}},
                             Serve(<<"b.txt;size=0">>, <<>>)),
                ?assertNot(Exists("b.txt"))
            end),
     % already gone
     ?_assertEqual({gateway_finished, {error_code, file_not_found}},
                   Serve(<<"a.txt;size=0">>, <<>>)),
     ?_assertEqual({gateway_finished, {error_code, file_not_found}},
                   Serve(<<"no/such/file;size=0">>, <<>>)),
     % never a directory, nor the root itself: not a file, so not found
     ?_test(begin
                ?assertEqual({gateway_finished,
                              {error_code, file_not_found}},
                             Serve(<<"dir;size=0">>, <<>>)),
                ?assert(Exists("dir/inner.txt"))
            end),
     ?_assertEqual({gateway_finished, {error_code, file_not_found}},
                   Serve(<<";size=0">>, <<>>)),
     % a zero-size request that carries data sends more than it
     % declared: refused, and nothing is deleted
     ?_test(begin
                ?assertEqual({gateway_finished,
                              {error_code, request_too_long}},
                             Serve(<<"c.txt;size=0">>, <<"junk">>)),
                ?assert(Exists("c.txt"))
            end),
     % confined like an upload: can't reach outside the root
     ?_test(begin
                ?assertEqual({gateway_finished,
                              {error_code, file_not_found}},
                             Serve(<<"escape/secret.txt;size=0">>, <<>>)),
                ?assert(filelib:is_file(
                          filename:join([Base, "outside", "secret.txt"])))
            end),
     ?_assertEqual({gateway_finished, {error_code, file_not_found}},
                   Serve(<<"../outside/secret.txt;size=0">>, <<>>))].


%% An over-large upload is refused before anything is read or created.
size_cap_test_() ->
    {setup, fun delete_setup/0, fun delete_teardown/1,
     fun(Base) ->
         Root = filename:join(Base, "root"),
         Work = filename:join(Base, "work"),
         Opts = #{ docroot => Root, work_dir => list_to_binary(Work) },
         Cfg = #server_config{hostname = <<"h">>, port = 1965,
                              routing = [], docroot = Root},
         Big = list_to_binary("big.txt;size=" ++
                              integer_to_list(?MAX_UPLOAD_BYTES + 1)),
         ?_test(begin
                    ?assertEqual({gateway_finished,
                                  {error_code, request_too_long}},
                                 serve(#{ <<"PATH">> => Big },
                                       test_request(
                                         #{ rest_of_input =>
                                                <<"some data">> }),
                                       Cfg, Opts)),
                    ?assertEqual({ok, []}, file:list_dir(Work)),
                    ?assertNot(filelib:is_file(
                                 filename:join(Root, "big.txt")))
                end)
     end}.


%% An upload that sends more than it declared is refused with a bad
%% request, nothing is installed (an existing file is left as it was),
%% and the data received is deleted.
excess_data_test_() ->
    {setup, fun delete_setup/0, fun delete_teardown/1, fun excess_tests/1}.

excess_tests(Base) ->
    Root = filename:join(Base, "root"),
    Work = list_to_binary(filename:join(Base, "work")),
    Opts = #{ docroot => Root, work_dir => Work },
    Cfg = #server_config{hostname = <<"h">>, port = 1965,
                         routing = [], docroot = Root},
    Serve = fun(Fragment, Rest) ->
                serve(#{ <<"PATH">> => Fragment },
                      test_request(#{ rest_of_input => Rest }), Cfg, Opts)
            end,
    TooLong = {gateway_finished, {error_code, request_too_long}},
    Inner = filename:join([Root, "dir", "inner.txt"]),
    % a state as the queued worker would hold it part-way through
    State = fun(Path, Size, Received) ->
                {ok, Stream, Tmp} = blizanci_tmpdir:create(Work),
                ok = file:write(Stream, binary:copy(<<"y">>, Received)),
                {#titan_state{size = Size, bytes_recv = Received,
                              tmp_file = Tmp, path = Path,
                              root_dir = list_to_binary(Root),
                              stream = Stream}, Tmp}
            end,
    [% data sent with the request line
     ?_test(begin
                ?assertEqual(TooLong,
                             Serve(<<"new.txt;size=3">>, <<"abcdef">>)),
                ?assertNot(filelib:is_file(filename:join(Root, "new.txt"))),
                ?assertEqual({ok, []}, file:list_dir(Work))
            end),
     ?_test(begin
                ?assertEqual({ok, <<"x">>}, file:read_file(Inner)),
                ?assertEqual(TooLong,
                             Serve(<<"dir/inner.txt;size=3">>, <<"abcdef">>)),
                ?assertEqual({ok, <<"x">>}, file:read_file(Inner))
            end),
     % exactly the declared size is fine
     ?_assertMatch({gateway_finished, {success, _, _}},
                   Serve(<<"exact.txt;size=3">>, <<"abc">>)),
     % excess arriving later, to a queued upload
     ?_test(begin
                {St, Tmp} = State(<<"late.txt">>, 3, 0),
                ?assertEqual(titan_too_much, recv_data(<<"abcdef">>, St)),
                ?assertNot(filelib:is_file(Tmp)),
                ?assertNot(filelib:is_file(filename:join(Root, "late.txt")))
            end),
     ?_test(begin
                {St, Tmp} = State(<<"dir/inner.txt">>, 3, 2),
                ?assertEqual(titan_too_much, recv_data(<<"cd">>, St)),
                ?assertNot(filelib:is_file(Tmp)),
                ?assertEqual({ok, <<"x">>}, file:read_file(Inner))
            end),
     % chunks that add up exactly to the declared size are fine
     ?_test(begin
                {St, _Tmp} = State(<<"chunks.txt">>, 4, 2),
                ?assertEqual(titan_finished, recv_data(<<"zz">>, St)),
                ?assertEqual({ok, <<"yyzz">>},
                             file:read_file(filename:join(Root,
                                                          "chunks.txt")))
            end),
     ?_test(begin
                {St, _Tmp} = State(<<"part.txt">>, 4, 1),
                ?assertEqual({titan_updated, 3}, recv_data(<<"zz">>, St))
            end),
     ?_assertEqual(TooLong, handle_upload_status(titan_too_much))].


%% Hidden files can't be created, overwritten or deleted, whether the
%% data comes with the request or later; the one exception is real files
%% directly under a ".well-known" directory at the top of the root.
hidden_files_test_() ->
    {setup, fun delete_setup/0, fun delete_teardown/1, fun hidden_tests/1}.

hidden_tests(Base) ->
    Root = filename:join(Base, "root"),
    Work = list_to_binary(filename:join(Base, "work")),
    Opts = #{ docroot => Root, work_dir => Work },
    Cfg = #server_config{hostname = <<"h">>, port = 1965,
                         routing = [], docroot = Root},
    Serve = fun(Fragment, Rest) ->
                serve(#{ <<"PATH">> => Fragment },
                      test_request(#{ rest_of_input => Rest }), Cfg, Opts)
            end,
    NotFound = {gateway_finished, {error_code, file_not_found}},
    Read = fun(Name) -> file:read_file(filename:join(Root, Name)) end,
    Exists = fun(Name) -> filelib:is_file(filename:join(Root, Name)) end,
    [% creating
     ?_test(begin
                ?assertEqual(NotFound, Serve(<<".evil;size=3">>, <<"abc">>)),
                ?assertNot(Exists(".evil"))
            end),
     ?_test(begin
                ?assertEqual(NotFound,
                             Serve(<<".git/pwn;size=3">>, <<"abc">>)),
                ?assertNot(Exists(".git/pwn"))
            end),
     % overwriting
     ?_test(begin
                ?assertEqual(NotFound, Serve(<<".env;size=3">>, <<"new">>)),
                ?assertEqual({ok, <<"x">>}, Read(".env"))
            end),
     % deleting
     ?_test(begin
                ?assertEqual(NotFound, Serve(<<".git/config;size=0">>, <<>>)),
                ?assert(Exists(".git/config"))
            end),
     % a dotfile inside .well-known is still hidden
     ?_assertEqual(NotFound, Serve(<<".well-known/.x;size=3">>, <<"abc">>)),
     % but real files there can be written and deleted
     ?_test(begin
                ?assertMatch({gateway_finished, {success, _, _}},
                             Serve(<<".well-known/security.txt;size=3">>,
                                   <<"abc">>)),
                ?assertEqual({ok, <<"abc">>},
                             Read(".well-known/security.txt"))
            end),
     ?_test(begin
                ?assertMatch({gateway_finished, {success, _, _}},
                             Serve(<<".well-known/old;size=0">>, <<>>)),
                ?assertNot(Exists(".well-known/old"))
            end),
     % the same rule applies when the upload is finished later (the
     % target is resolved again then)
     ?_test(begin
                {ok, Stream, Tmp} = create_tmp_file(Work, <<"x">>),
                ?assertEqual(titan_not_found,
                             finish_file(Stream, <<".late">>,
                                         list_to_binary(Root), Tmp, 1)),
                ?assertNot(Exists(".late")),
                ?assertNot(filelib:is_file(Tmp))
            end)].


%% A temporary file that can't be removed must not crash the upload: the
%% outcome is the same and the failure is only logged.
delete_failure_test_() ->
    {setup, fun delete_setup/0, fun delete_failure_teardown/1,
     fun delete_failure_tests/1}.

delete_failure_teardown(Base) ->
    _ = file:write_file_info(filename:join(Base, "lock"),
                             #file_info{mode = 8#755}),
    delete_teardown(Base).

delete_failure_tests(Base) ->
    Root = filename:join(Base, "root"),
    Lock = filename:join(Base, "lock"),
    ok = filelib:ensure_path(Lock),
    IsRoot = string:trim(os:cmd("id -u")) =:= "0",
    Open = fun(Name) ->
               Tmp = filename:join(Lock, Name),
               {ok, Stream} = file:open(Tmp, [write, raw, binary]),
               {Stream, Tmp}
           end,
    [?_assertEqual(ok, delete(filename:join(Base, "no/such/file"))),
     ?_assertEqual({error, enotdir},
                   delete(filename:join([Root, "a.txt", "x"]))),
     % a directory can't be deleted as a file
     ?_assertMatch({error, E} when E =:= eisdir; E =:= eperm,
                   delete(filename:join(Root, "dir"))),
     ?_assertEqual(ok, discard(filename:join(Root, "dir"))),
     % eacces: nothing in a read-only directory can be removed
     ?_test(case IsRoot of
                true -> ok;
                false ->
                    {S1, T1} = Open("t1"),
                    {S2, T2} = Open("t2"),
                    ok = file:write_file_info(Lock,
                                              #file_info{mode = 8#555}),
                    ?assertEqual({error, eacces}, delete(T1)),
                    ?assertEqual(ok, discard(T1)),
                    % target not found: the upload is refused cleanly
                    ?assertEqual(titan_not_found,
                                 finish_file(S1, <<".nope">>,
                                             list_to_binary(Root), T1, 0)),
                    % more data than declared: refused cleanly
                    ?assertEqual(titan_too_much,
                                 recv_data(<<"abc">>,
                                           #titan_state{stream = S2,
                                                        size = 1,
                                                        path = <<"x">>,
                                                        root_dir = Root,
                                                        tmp_file = T2,
                                                        bytes_recv = 0})),
                    ok = file:write_file_info(Lock,
                                              #file_info{mode = 8#755}),
                    ?assert(filelib:is_file(T1))
            end)].
