%%%-------------------------------------------------------------------

%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

% TBD: module-level docs
-module(blizanci_titan).
-behaviour(blizanci_servlet).

-include("blizanci_types.hrl").

-behaviour(gen_server).
-include("gen_server.hrl").

-export([serve/4, start/0, cancel/1, request/4, default_options/0,
         handle_client_data/2]).
-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-type options() :: #{
                     docroot := string(),
                     work_dir := binary()
                    }.

-type io_device() :: pid() | {'file_descriptor', atom(), any()}.
-type filepath() :: binary() | string().

-define(QUEUE, ?MODULE).
-define(MAX_TITAN, 10).

-define(MAX_AGE, 3600). % for expiring files; measured in seconds

-record(titan_state, {parent,
                      size,
                      mime_type,
                      bytes_recv,
                      tmp_file,
                      target_path,
                      work_dir,
                      stream}).
-type titan_state() :: #titan_state{}.

-type titan_request() :: {titan_request, binary(), integer(), binary()}.

-type upload_status() :: titan_finished
                       | titan_enotdir
                       | {titan_updated, integer()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec default_options() -> map().
default_options() ->
    #{
      work_dir => <<"titan-temp">>,
      docroot => "docroot"
     }.

-spec start() -> ok.
start() ->
    case ppool:start_pool(?QUEUE, ?MAX_TITAN,
                          {?MODULE, start_link, []}) of
        {error, {already_started, _Pid}} -> ok;
        {ok, _Pid} -> ok
    end.

-spec start_link(any()) ->
          'ignore' | {'error',_} | {'ok',pid() | {pid(),reference()}}.
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).


-spec cancel(pid()) -> ok.
cancel(Pid) ->
    case is_process_alive(Pid) of
        true -> gen_server:cast(Pid, titan_quit);
        _ -> ok
    end.

% Called by the servlet
%
-spec request(path_matches(), request_details(), server_config(), options()) ->
                         {'immediate', gemini_response()} |
                         'defer'.
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
            Config = {TitanReq, Rest, RootDir, WorkDir},
            {titan_request, Path, Size, _MimeType} = TitanReq,
            RestSize = byte_size(Rest),
            case RestSize >= Size of
                true -> handle_all_in_one_request(WorkDir, RootDir, Path,
                                                  Rest, Size);
                false -> enqueue_titan_job(Config)
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
    {ok, Stream, TmpPath, TargetPath} =
        create_tmp_file(WorkDir, RootDir, Path, Rest),

    UploadStatus = finish_file(Stream, TargetPath, TmpPath, Size),
    handle_upload_status(UploadStatus).


% the alternative case (to the previous function) is that a valid Titan
% request has been received, but there is either no concurrent payload
% data, or insufficient such data to fulfil the request; this entails
% waiting for more data, therefore we put a Titan worker into the worker
% pool to handle this
-spec enqueue_titan_job(Config) -> Result
              when Config :: {titan_request(),
                              binary(),
                              filepath(),
                              filepath()},
                   Result :: gateway_result().

enqueue_titan_job(Config) ->
    case ppool:run(?QUEUE, [{self(), Config}]) of
        {ok, Pid} -> {gateway_started, Pid};
        noalloc -> {gateway_error, gateway_busy}
    end.


-spec ensure_binary(list()) -> binary();
                   (binary()) -> binary().
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
                   Result   :: {ok, titan_request()} |
                               {error, atom()}.

parse_titan_request(Fragment) ->
    case binary:split(Fragment, <<";">>) of
        [_Single] -> {error, bad_query_string};
        [Path, Query] -> parse_titan_qs(Path, Query);
        _ -> {error, internal_server_error}
    end.


-spec parse_titan_qs(Path, Query) -> Result
              when Path   ::  binary(),
                   Query  :: binary(),
                   Result :: {ok, titan_request()}.

parse_titan_qs(Path, Query)
  when is_binary(Path) and is_binary(Query)
 ->
    Q2 = binary:replace(Query, <<";">>, <<"&">>, [global]),
    AllParams = uri_string:dissect_query(Q2),
    BinParams = [{K,V} || {K,V} <- AllParams, is_binary(V)],
    KVPs = maps:from_list(BinParams),
    SizeS = maps:get(<<"size">>, KVPs),
    MimeType = maps:get(<<"mime">>, KVPs),
    Size = erlang:binary_to_integer(SizeS),
    TitanRequest = {titan_request, Path, Size, MimeType},
    {ok, TitanRequest}.

% TODO: factor out config type
init({Parent, Config}) ->
    process_flag(trap_exit, true),
    {TitanReq, Rest, RootDir, WorkDir} = Config,
    {titan_request, Path, Size, MimeType} = TitanReq,
    BytesRecv = byte_size(Rest),
    {ok, Stream, TmpPath, TargetPath} = create_tmp_file(
                                          WorkDir, RootDir, Path, Rest),
    State = #titan_state{
               parent=Parent,
               size=Size,
               mime_type=MimeType,
               bytes_recv=BytesRecv,
               tmp_file=TmpPath,
               target_path=TargetPath,
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


handle_info(_Info, State) ->
    %lager:info("OOB msg:~p", [Info]),
    {noreply, State}.


terminate(normal, #titan_state{tmp_file=TmpPath, work_dir=WorkDir}) ->
    ok = purge(TmpPath, WorkDir),
    ok;
terminate(_Reason, #titan_state{tmp_file=TmpPath, work_dir=WorkDir}) ->
    %lager:info("Titan queue worker ~p terminating: [[~p]]", [self(), Reason]),
    ok = purge(TmpPath, WorkDir),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.

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
                   Result :: gateway_result() | {ok, in_progress, integer() }.

handle_upload_status(titan_finished) ->
    {gateway_finished, {success, <<"text/plain">>, <<"Uploaded.\r\n">>}};
handle_upload_status(titan_enotdir) ->
    {gateway_finished, {error_code, cannot_overwrite}};
handle_upload_status({titan_updated, NewSize}) ->
    {ok, in_progress, NewSize}.


-spec recv_data(Data, State) -> Result
              when Data   :: binary(),
                   State  :: titan_state(),
                   Result :: upload_status().

recv_data(Data, #titan_state{stream=Stream,
                             size=Size,
                             target_path=TargetPath,
                             tmp_file=TmpPath,
                             bytes_recv=OldBytesRecv}) ->
    Len = byte_size(Data),
    file:write(Stream, Data),
    NewSize = OldBytesRecv + Len,
    case NewSize >= Size of
        true ->
            finish_file(Stream, TargetPath, TmpPath, Size);
        _ ->
            {titan_updated, NewSize}
    end.


-spec finish_file(Stream, TargetPath, TmpPath, Size) -> Result
              when Stream     :: io_device(),
                   TargetPath :: filepath(),
                   TmpPath    :: filepath(),
                   Size       :: integer(),
                   Result     :: upload_status().

finish_file(Stream, TargetPath, TmpPath, Size) ->
    truncate(Stream, Size),
    ok = file:close(Stream),
    case delete(TargetPath) of
        {fail, enotdir} -> titan_enotdir;
        ok ->
            ok = file:make_link(TmpPath, TargetPath),
            ok = delete(TmpPath),
            titan_finished
    end.


-spec delete(Path) -> Result
              when Path   :: filepath(),
                   Result :: ok | {fail, atom()} | term().

delete(Path) ->
    case file:delete(Path) of
        ok -> ok;
        {error, enoent} -> ok;
        {error, enotdir} -> {fail, enotdir};
        Error -> Error
    end.


-spec truncate(Stream, Position) -> 'ok'
              when Stream   :: io_device(),
                   Position :: integer().

truncate(Stream, Position) when is_integer(Position) ->
    {ok, _Pos} = file:position(Stream, Position),
    ok = file:truncate(Stream).


%% purge work dir of obviously left-over old files
-spec purge(Path, WorkDir) -> 'ok'
              when Path    :: filepath(),
                   WorkDir :: filepath().

purge(Path, WorkDir) when is_binary(Path) and is_binary(WorkDir) ->
    delete(Path),
    %lager:debug("Purging: ~p, ~p", [Path, WorkDir]),
    [ delete(F) || F <- blizanci_tmpdir:stale(WorkDir, ?MAX_AGE) ],
    ok.


-spec create_tmp_file(WorkDir, RootDir, Path, Rest) -> Result
              when WorkDir :: filepath(),
                   RootDir :: filepath(),
                   Path    :: filepath(),
                   Rest    :: binary(),
                   Result  :: {ok, io_device(), filepath(), filepath()}.

create_tmp_file(WorkDir, RootDir, Path, Rest)
  when is_binary(WorkDir) and is_binary(RootDir) and
       is_binary(Path) and is_binary(Rest)
 ->
    TmpFile = blizanci_tmpdir:tmp_file_name(),
    TmpPath = filename:join(WorkDir, TmpFile),
    TargetPath = filename:join(RootDir, Path),
    %lager:debug("titan path: ~p", [Path]),
    %lager:debug("titan tmp: ~p, target: ~p", [TmpPath, TargetPath]),
    {ok, Stream} = file:open(TmpPath, [write]),
    ok = file:write(Stream, Rest),
    {ok, Stream, TmpPath, TargetPath}.
