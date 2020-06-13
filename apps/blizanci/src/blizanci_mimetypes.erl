%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_mimetypes).
-include_lib("eunit/include/eunit.hrl").
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
lookup(MimeType) when is_binary(MimeType) ->
    gen_server:call(?SERVER, {lookup, MimeType}).

-spec start_link() -> {ok, Pid :: pid()} |
                      {error, Error :: {already_started, pid()}} |
                      {error, Error :: term()} |
                      ignore.
start_link() ->
    proc_lib:start_link(?MODULE, init, [[]])

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(Args :: term()) -> {ok, State :: term()} |
                              {ok, State :: term(), Timeout :: timeout()} |
                              {ok, State :: term(), hibernate} |
                              {stop, Reason :: term()} |
                              ignore.
init([]) ->
    proc_lib:init_ack({ok, self()}),
    ok = load_data(),
    register(?SERVER, self()),
    gen_server:enter_loop(?MODULE, [], #state{}).


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
handle_info(_Info, State) ->
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
    ok = load_data(Data).

load_data(Data) ->
    Lines = string:split(Data, <<"\n">>, all),
    ets:new(?MODULE, [bag, named_table]),
    Stripped = [ strip_comment(S) || S <- Lines ],
    Mappings = [ line_to_mapping(S2) || S2 <- Stripped ],
    [ ok = store_mapping(M) || M <- Mappings ],
    {ok, Additional} = application:get_env(mime_types),
    [ ok = store_mapping([V, K]) || {K, V} <- Additional],
    ok.

strip_comment_test_data() ->
    [
     {<<>>, <<"# a comment line">>},
     {<<"hello">>, <<"hello#there">>},
     {<<"text/html  html htm">>, <<"text/html  html htm#optional">>}
    ].

strip_comment(S) when is_binary(S) ->
    Result = string:split(S, <<"#">>, leading),
    [Stripped|_] = Result,
    Stripped.

line_to_mapping_test_data() ->
    [
     {[], <<"">>},
     {[<<"text/html">>, <<"html">>, <<"htm">>], <<"text/html html htm">>},
     {[<<"application/batch-SMTP">>], <<"application/batch-SMTP     ">>}
    ].

line_to_mapping(Line) when is_binary(Line) ->
    string:lexemes(Line, " " ++ [$\t]).

store_mapping([]) -> ok; %ignored
store_mapping([_Key]) -> ok; %ignored
store_mapping([Key|Values]) ->
    [ ets:insert(?MODULE, [{V, Key}]) || V <- Values],
    ok.

strip_comment_test_() ->
    [ ?_assertEqual(Expected, strip_comment(Observed)) ||
        {Expected, Observed} <- strip_comment_test_data() ].

line_to_mapping_test_() ->
    [ ?_assertEqual(Expected, line_to_mapping(Observed)) ||
        {Expected, Observed} <- line_to_mapping_test_data() ].
