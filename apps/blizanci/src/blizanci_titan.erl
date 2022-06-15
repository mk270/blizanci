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

-define(QUEUE, ?MODULE).
-define(MAX_TITAN, 10).

-define(MAX_AGE, 3600). % for expiring files

-record(titan_state, {parent,
                      size,
                      mime_type,
                      bytes_recv,
                      tmp_file,
                      target_path,
                      work_dir,
                      stream}).
%-type titan_state() :: #titan_state{}.

-type titan_request() :: {titan_request, binary(), integer(), binary()}.

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

%% TBD: break up this excessively long/complicated fn
-spec serve(path_matches(), request_details(), server_config(), options()) ->
                   gateway_result().
serve(Matches, Req, _ServerConfig, RouteOpts) ->
    #{ <<"PATH">> := Fragment } = Matches,
    #{ rest_of_input := Rest } = Req,
    #{ work_dir := WorkDir,
       docroot := RootDir } = RouteOpts,
    case parse_titan_request(Fragment) of
        {ok, TitanReq} ->
            Config = {TitanReq, Rest, RootDir, WorkDir},
            {titan_request, Path, Size, _MimeType} = TitanReq,
            RestSize = byte_size(Rest),
            case RestSize >= Size of
                true ->
                    {ok, Stream, TmpPath, TargetPath} =
                        create_tmp_file(WorkDir, RootDir, Path, Rest),
                    finish_file(Stream, TargetPath, TmpPath, Size),
                    {gateway_finished, {success, <<"text/plain">>,
                                       <<"# Uploaded.\r\n">>}};
                false ->
                    case ppool:run(?QUEUE, [{self(), Config}]) of
                        {ok, Pid} -> {gateway_started, Pid};
                        noalloc -> {gateway_error, gateway_busy}
                    end
            end;
        {error, Err} ->
            {gateway_finished, {error_code, Err}}
    end.

-spec handle_client_data(pid(), binary()) -> gemini_response().
handle_client_data(Pid, Data) ->
    case gen_server:call(Pid, {client_data, Data}) of
        {ok, in_progress} -> none;
        {gateway_finished, {error_code, Err}} -> {error_code, Err}
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

parse_titan_request(Fragment) ->
    case binary:split(Fragment, <<";">>) of
        [_Single] -> {error, bad_query_string};
        [Path, Query] -> parse_titan_qs(Path, Query);
        _ -> {error, internal_server_error}
    end.

-spec parse_titan_qs(binary(), binary()) ->
          {ok, titan_request()}.
parse_titan_qs(Path, Query) ->
    Q2 = binary:replace(Query, <<";">>, <<"&">>, [global]),
    KVPs = maps:from_list(uri_string:dissect_query(Q2)),
    SizeS = maps:get(<<"size">>, KVPs),
    MimeType = maps:get(<<"mime">>, KVPs),
    Size = erlang:binary_to_integer(SizeS),
    Params = {titan_request, Path, Size, MimeType},
    {ok, Params}.

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

handle_call({client_data, Data}, _From,
            State=#titan_state{stream=Stream,
                               size=Size,
                               target_path=TargetPath,
                               tmp_file=TmpPath,
                               bytes_recv=OldBytesRecv}) ->
    case recv_data(Stream, Data, Size, TargetPath, TmpPath, OldBytesRecv) of
        titan_finished ->
            % TBD: factor together
            Reply = {gateway_finished, {success, <<"text/plain">>,
                                       <<"Uploaded.\r\n">>}},
            {stop, normal, Reply, State};
        {titan_updated, NewSize} ->
            Reply = {ok, in_progress},
            {reply, Reply, State#titan_state{bytes_recv=NewSize}}
    end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(titan_quit, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(Info, State) ->
    lager:info("OOB msg:~p", [Info]),
    {noreply, State}.


terminate(normal, _State) ->
    ok;
terminate(Reason, #titan_state{tmp_file=TmpPath, work_dir=WorkDir}) ->
    lager:info("Titan queue worker ~p terminating: [[~p]]", [self(), Reason]),
    purge(TmpPath, WorkDir),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.

tmp_file_name() ->
    Hash = erlang:phash2(make_ref()),
    integer_to_list(Hash).

recv_data(Stream, Data, Size, TargetPath, TmpPath, OldBytesRecv) ->
    Len = byte_size(Data),
    file:write(Stream, Data),
    NewSize = OldBytesRecv + Len,
    case NewSize >= Size of
        true ->
            finish_file(Stream, TargetPath, TmpPath, Size);
        _ ->
            {titan_updated, NewSize}
    end.

finish_file(Stream, TargetPath, TmpPath, Size) ->
    truncate(Stream, Size),
    ok = file:close(Stream),
    ok = delete(TargetPath),
    ok = file:make_link(TmpPath, TargetPath),
    ok = delete(TmpPath),
    titan_finished.

delete(Path) ->
    case file:delete(Path) of
        ok -> ok;
        {error, enoent} -> ok;
        Error -> Error
    end.

truncate(Stream, Position) ->
    {ok, _Pos} = file:position(Stream, Position),
    ok = file:truncate(Stream).

%% TBD: purge work dir of obviously left-over old files
purge(Path, WorkDir) ->
    delete(Path),
    [ delete(F) || F <- stale(WorkDir) ].

-spec create_tmp_file(binary(), binary(), binary(), binary()) ->
          {ok, term(), binary(), binary()}.
create_tmp_file(WorkDir, RootDir, Path, Rest) ->
    TmpFile = tmp_file_name(),
    TmpPath = filename:join(WorkDir, TmpFile),
    TargetPath = filename:join(RootDir, Path),
    {ok, Stream} = file:open(TmpPath, [write]),
    ok = file:write(Stream, Rest),
    {ok, Stream, TmpPath, TargetPath}.


stale(Dir) when is_list(Dir) ->
    %{ok, Cwd} = file:get_cwd(),
    %FullDir = filename:join(Cwd, Dir),
    FullDir = Dir,
    {ok, Files} = file:list_dir_all(FullDir),
    FullFilenames = [ filename:join(FullDir, F) || F <- Files ],
    [ F || F <- FullFilenames, older_than(F, ?MAX_AGE) ].

older_than(Filename, MaxAge) ->
    {ok, Stat} = file:read_file_info(Filename),
    Stamp = erlang:element(5, Stat),

    Now = calendar:now_to_local_time(erlang:timestamp()),
    {Days, Time} = calendar:time_difference(Stamp, Now),
    Secs = calendar:time_to_seconds(Time),

    case {Days >= 0, Secs >= MaxAge} of
        {true, false} -> false;
        {false, _} -> true; % just write it off - filestamp in future?
        {true, true} -> true
    end.
