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
                      docroot := string()
                    }.

-define(QUEUE, ?MODULE).
-define(MAX_TITAN, 10).

-record(titan_state, {parent,
                      size,
                      mime_type,
                      bytes_recv,
                      tmp_file,
                      target_path,
                      stream}).
%-type titan_state() :: #titan_state{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec default_options() -> map().
default_options() ->
    #{
      work_dir => <<"titan-temp">>
     }.

start() ->
    case ppool:start_pool(?QUEUE, ?MAX_TITAN,
                          {?MODULE, start_link, []}) of
        {error, {already_started, _Pid}} -> ok;
        {ok, _Pid} -> ok
    end.

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


-spec serve(path_matches(), request_details(), server_config(), options()) ->
                   gateway_result().
serve(Matches, Req, _ServerConfig, _RouteOpts) ->
    #{ <<"PATH">> := Fragment } = Matches,
    #{ rest_of_input := Rest } = Req,
    case parse_titan_request(Fragment) of
        {ok, TitanReq} ->
            Config = {TitanReq, Rest},
            lager:info("titan config: ~p", [Config]),
            case ppool:run(?QUEUE, [{self(), Config}]) of
                {ok, Pid} -> {gateway_started, Pid};
                noalloc -> {gateway_error, gateway_busy}
            end;
        {error, Err} ->
            {gateway_finished, {error_code, Err}}
    end.


handle_client_data(Pid, Data) ->
    ok = gen_server:cast(Pid, {client_data, Data}),
    none.


%%%===================================================================
%%% Internal functions
%%%===================================================================

parse_titan_request(Fragment) ->
    case binary:split(Fragment, <<";">>) of
        [_Single] -> {error, bad_query_string};
        [Path, Query] -> parse_titan_qs(Path, Query);
        _ -> {error, internal_server_error}
    end.

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
    lager:info("in titan gs: ~p ~p", [Parent, Config]),
    {TitanReq, Rest} = Config,
    {titan_request, Path, Size, MimeType} = TitanReq,
    BytesRecv = byte_size(Rest),
    TmpFile = tmp_file_name(),
    WorkDir = <<"titan-temp">>, %% get from cfg
    RootDir = <<"docroot">>, %% get from cfg
    TmpPath = filename:join(WorkDir, TmpFile),
    TargetPath = filename:join(RootDir, Path),
    {ok, Stream} = file:open(TmpPath, [write]),
    ok = file:write(Stream, Rest),
    State = #titan_state{
               parent=Parent,
               size=Size,
               mime_type=MimeType,
               bytes_recv=BytesRecv,
               tmp_file=TmpPath,
               target_path=TargetPath,
               stream=Stream
              },
    {ok, State}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({client_data, Data}, State) ->
    lager:info("Titan proc recv: ~p @ ~p", [Data, State]),
    file:write(State#titan_state.stream, Data),
    Len = byte_size(Data),
    NewSize = State#titan_state.bytes_recv + Len,
    {noreply, State#titan_state{bytes_recv=NewSize}};
handle_cast(titan_quit, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(Info, State) ->
    lager:info("OOB msg:~p", [Info]),
    {noreply, State}.


terminate(normal, _State) ->
    ok;
terminate(Reason, _State) ->
    lager:info("Titan queue worker ~p terminating: [[~p]]", [self(), Reason]),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.

tmp_file_name() ->
    Hash = erlang:phash2(make_ref()),
    integer_to_list(Hash).
