%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_mimetypes).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% API
-export([lookup/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
lookup(MimeType) ->
    gen_server:call(?SERVER, {lookup, MimeType}).

-spec start_link() -> {ok, Pid :: pid()} |
                      {error, Error :: {already_started, pid()}} |
                      {error, Error :: term()} |
                      ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(Args :: term()) -> {ok, State :: term()} |
                              {ok, State :: term(), Timeout :: timeout()} |
                              {ok, State :: term(), hibernate} |
                              {stop, Reason :: term()} |
                              ignore.
init([]) ->
    process_flag(trap_exit, true),
    self() ! load,
    {ok, #state{}}.

-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
                         {reply, Reply :: term(), NewState :: term()} |
                         {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
                         {reply, Reply :: term(), NewState :: term(), hibernate} |
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_call({lookup, MimeType}, _From, State) ->
    Reply = case ets:lookup(?MODULE, MimeType) of
                [] -> notfound;
                [{_, MT}] -> {ok, MT};
                [{_, MT}|_] -> {ok, MT} %oops
            end,
    {reply, Reply, State}.

-spec handle_cast(Request :: term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(Info :: timeout() | term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(load, State) ->
    ok = load_data(),
    {noreply, State};
handle_info(Info, State) ->
    {noreply, State}.

-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

-spec code_change(OldVsn :: term() | {down, term()},
                  State :: term(),
                  Extra :: term()) -> {ok, NewState :: term()} |
                                      {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

load_data() ->
    {ok, ConfigFile} = application:get_env(mime_types_path),
    {ok, Data} = file:read_file(ConfigFile),
    Lines = string:split(Data, <<"\n">>, all),
    ets:new(?MODULE, [bag, named_table]),
    Stripped = [ strip_comment(S) || S <- Lines ],
    [ ok = parse_line(S2) || S2 <- Stripped ],
    {ok, Additional} = application:get_env(mime_types),
    [ store_mapping([V, K]) || {K, V} <- Additional],
    ok.

strip_comment(S) ->
    Result = string:split(S, <<"#">>, leading),
    [Stripped|_] = Result,
    Stripped.

parse_line(<<>>) -> ok;

parse_line(Line) ->
    Words = string:lexemes(Line, " " ++ [$\t]),
    store_mapping(Words),
    ok.

store_mapping([_Key]) -> ok; %ignored
store_mapping([Key|Values]) -> 
    [ ets:insert(?MODULE, [{V, Key}]) || V <- Values],
    ok.
    
