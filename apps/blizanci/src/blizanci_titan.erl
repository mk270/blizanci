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
% text/gemini) and "token" is optional.
-module(blizanci_titan).
-behaviour(blizanci_servlet).

-include("blizanci_types.hrl").
-include_lib("eunit/include/eunit.hrl").

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

-type titan_request() :: {titan_request, binary(), integer(), binary()}.

-record(titan_job, {request     :: titan_request(),
                    rest        :: binary(),
                    work_dir    :: filepath(),
                    root_dir    :: filepath()}).
-type titan_job() :: #titan_job{}.

-type upload_status() :: titan_finished
                       | titan_enotdir
                       | titan_not_found
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
    case parse_titan_request(Fragment) of
        {ok, TitanReq} ->
            {titan_request, Path, Size, _MimeType} = TitanReq,

            % Canonicalise-and-confine the upload target under RootDir
            % up front, before any worker (sync or queued) is started,
            % the same mechanism CGI and static-file serving use. This
            % also catches a symlink inside RootDir that would
            % otherwise let an upload land outside it. It is only an
            % early refusal: the target is resolved again, just before
            % it is written (see finish_file/5), because a long upload
            % leaves time for the directory tree to change.
            case blizanci_path:confine(binary_to_list(Path),
                                       binary_to_list(RootDir)) of
                {ok, _TargetPath} ->
                    Job = #titan_job{request=TitanReq,
                                     rest=Rest,
                                     work_dir=WorkDir,
                                     root_dir=RootDir},
                    RestSize = byte_size(Rest),
                    case RestSize >= Size of
                        true -> handle_all_in_one_request(WorkDir, RootDir,
                                                          Path, Rest, Size);
                        false -> enqueue_titan_job(Job)
                    end;
                {error, file_not_found} ->
                    {gateway_finished, {error_code, file_not_found}}
            end;
        {error, Err} ->
            {gateway_finished, {error_code, Err}}
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

    UploadStatus = finish_file(Stream, Path, RootDir, TmpPath, Size),
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
            {ok, {titan_request, Path, Size, MimeType}};
        {error, _} = Error ->
            Error
    end.


% The size parameter is mandatory, and a number of bytes: so a missing,
% non-numeric or negative size is a bad request. Zero is valid (the
% Titan specification uses it to mean "delete").
-spec parse_size(SizeS) -> Result
              when SizeS  :: binary() | 'undefined',
                   Result :: {ok, non_neg_integer()}
                             | {error, atom()}.
parse_size(undefined) ->
    {error, bad_query_string};
parse_size(SizeS) when is_binary(SizeS) ->
    try erlang:binary_to_integer(SizeS) of
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
    {titan_request, Path, Size, MimeType} = TitanReq,
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
    Len = byte_size(Data),
    file:write(Stream, Data),
    NewSize = OldBytesRecv + Len,
    case NewSize >= Size of
        true ->
            finish_file(Stream, Path, RootDir, TmpPath, Size);
        _ ->
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
    case blizanci_path:confine(binary_to_list(Path),
                               binary_to_list(RootDir)) of
        {ok, TargetPath} ->
            replace_target(TargetPath, TmpPath);
        {error, file_not_found} ->
            ok = delete(TmpPath),
            titan_not_found
    end.


-spec replace_target(TargetPath, TmpPath) -> Result
              when TargetPath :: filepath(),
                   TmpPath    :: filepath(),
                   Result     :: upload_status().

replace_target(TargetPath, TmpPath) ->
    case delete(TargetPath) of
        {fail, enotdir} -> titan_enotdir;
        ok ->
            ok = file:make_link(TmpPath, TargetPath),
            ok = delete(TmpPath),
            titan_finished
    end.


-spec delete(Path) -> Result
              when Path   :: filepath(),
                   Result :: ok
                           | {fail, atom()}
                           | term().

delete(Path) ->
    case file:delete(Path) of
        ok -> ok;
        {error, enoent} -> ok;
        {error, enotdir} -> {fail, enotdir};
        Error -> Error
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
    TmpFile = blizanci_tmpdir:tmp_file_name(),
    TmpPath = filename:join(WorkDir, TmpFile),
    logger:debug("titan tmp: ~p", [TmpPath]),
    {ok, Stream} = file:open(TmpPath, [write]),
    ok = file:write(Stream, Rest),
    {ok, Stream, TmpPath}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_titan_request_test_() ->
    P = fun(Params) ->
            parse_titan_request(<<"dir/page;", Params/binary>>)
        end,
    [?_assertEqual({ok, {titan_request, <<"dir/page">>, 123,
                         <<"text/plain">>}},
                   P(<<"size=123;mime=text/plain">>)),
     ?_assertEqual({ok, {titan_request, <<"dir/page">>, 0,
                         <<"text/plain">>}},
                   P(<<"size=0;mime=text/plain">>)),
     % mime is optional and defaults to text/gemini
     ?_assertEqual({ok, {titan_request, <<"dir/page">>, 5,
                         <<"text/gemini">>}},
                   P(<<"size=5">>)),
     ?_assertEqual({error, bad_query_string},
                   P(<<"size=-1;mime=text/plain">>)),
     ?_assertEqual({error, bad_query_string},
                   P(<<"size=-0123456789;mime=text/plain">>)),
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
    Unique = integer_to_list(erlang:unique_integer([positive])),
    Base = filename:absname("titan_test_" ++ Unique),
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
                             blizanci_path:confine("swap/f.txt",
                                                   binary_to_list(Root))),
                ok = file:del_dir(Swap),
                ok = file:make_symlink("../outside", Swap),
                {Status, _} = Upload(<<"swap/f.txt">>, <<"x">>),
                ?assertEqual(titan_not_found, Status),
                ?assertNot(filelib:is_file(
                             filename:join([Base, "outside", "f.txt"])))
            end),
     ?_assertEqual({gateway_finished, {error_code, file_not_found}},
                   handle_upload_status(titan_not_found))].
