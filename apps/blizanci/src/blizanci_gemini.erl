%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_gemini).
-include_lib("eunit/include/eunit.hrl").
-behaviour(gen_server).
-behaviour(ranch_protocol).

%% API.
-export([start_link/4]).

%% gen_server.
-export([init/1]).
-export([init/4]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(EMPTY_BUF, <<>>).
-define(PROTO, <<"gemini">>).

-record(state,
        {socket :: inet:socket(),
         transport :: atom(),
         buffer :: binary(),
         hostname :: binary(),
         docroot :: string()}).

-type state() :: #state{}.
-type gemini_response() :: {'file', iolist(), binary()} | {'ok', iolist()}.

%%% FIXME: This function is never called. We only define it so that
%% we can use the -behaviour(gen_server) attribute.
-spec init([]) -> {ok, undefined}.
init([]) -> {ok, undefined}.

%% API.
-spec start_link(pid(), any(), any(), [any()]) -> {ok, pid()}.
start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

%% gen_server.

-spec init(pid(), any(), any(), [any()]) -> {ok, pid()}.
init(Ref, Socket, Transport, Opts) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    Hostname = erlang:list_to_binary(proplists:get_value(hostname, Opts)),
    State = #state{
               socket=Socket,
               transport=Transport,
               buffer=?EMPTY_BUF,
               hostname=Hostname,
               docroot=proplists:get_value(docroot, Opts)},
    gen_server:enter_loop(?MODULE, [], State).


handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
    {stop, Reason, State};

handle_info(timeout, State) ->
    {stop, normal, State};

handle_info({ssl, Socket, Payload}, State) ->
    {Buffer, Response} = handle_request(Payload, State),
    NewState = State#state{buffer=Buffer},
    Transport = State#state.transport,

    case Transport:setopts(Socket, [{active, once}]) of
        {error, closed} ->
            lager:info("socket closed somewhat unexpectedly"),
            {stop, normal, State};
        ok -> ok = case Response of
                       none -> ok;
                       hangup ->
                           Transport:close(Socket),
                           ok;
                       {ok, Msg} ->
                           Transport:send(Socket, Msg),
                           Transport:close(Socket),
                           ok;
                       {file, Header, Filename} ->
                           Transport:send(Socket, Header),
                           Transport:sendfile(Socket, Filename),
                           Transport:close(Socket),
                           ok
                   end,
              {noreply, NewState}
    end;

handle_info({ssl_closed, _SocketInfo}, State) ->
    {stop, normal, State};

handle_info(Msg, State) ->
    lager:warning("got unrecognised msg: ~p~n", [Msg]),
    {stop, normal, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal.

-spec handle_request(binary(), state()) -> {binary(), any()}.
handle_request(Payload, #state{buffer=Buffer,
                               hostname=Hostname,
                               docroot=Docroot}) ->
    AllInput = erlang:iolist_to_binary([Buffer, Payload]),
    case binary:split(AllInput, <<"\r\n">>) of
        [_] ->
            {AllInput, none};
        [Line, Rest] ->
            R = handle_line(Line, Hostname, Docroot),
            {Rest, R};
        _ ->
            lager:warning("Shouldn't get here"),
            {<<>>, hangup}
    end.

-spec handle_line(binary(), binary(), string()) -> gemini_response().
handle_line(Cmd, Host, Docroot) when is_binary(Cmd) ->
    {ok, Re} = re:compile("^\([a-z0-9]+\)://\([^/]*\)/\(.*\)$"),
    Match = re:run(Cmd, Re, [{capture, all, binary}]),
    Proto = ?PROTO,

    lager:info("req: ~p", [Match]),
    case Match of
        {match, [_All, Proto, Host, Path]} ->
            handle_file(Path, Docroot);
        {match, [_All, <<"gopher">>, _Host, _Path]} ->
            format_response(53, <<"text/plain">>, "Proxy request refused");
        {match, [_All, <<"https">>, _Host, _Path]} ->
            format_response(53, <<"text/plain">>, "Proxy request refused");
        {match, [_All, <<"http">>, _Host, _Path]} ->
            format_response(53, <<"text/plain">>, "Proxy request refused");
        {match, [_All, Proto, _Host, _Path]} ->
            format_response(53, <<"text/plain">>, "Host not recognised");
        {match, [_All, _Proto, _Host, _Path]} ->
            invalid_request(<<"Protocol not recognised">>);
        _ ->
            invalid_request(<<"Request not understood">>)
    end.

-spec handle_file(binary(), string()) -> gemini_response().
handle_file(Path, Docroot) when is_binary(Path), is_list(Docroot) ->
    case string:split(Path, "..") of
        [_] -> serve_file(Path, Docroot);
        [_, _] -> invalid_request(<<"Illegal filename">>)
    end.

-spec serve_file(binary(), string()) -> gemini_response().
serve_file(Path, Docroot) ->
    Full = filename:join(Docroot, Path),
    case filelib:is_regular(Full) of
        true ->
            Headers = format_headers(20, mime_type(Full)),
            {file, Headers, Full};
        false ->
            format_response(51, <<"text/plain">>, <<"File not found">>)
    end.

-spec mime_type(binary()) -> binary().
mime_type(Path) when is_binary(Path) ->
    case binary_to_list(filename:extension(Path)) of
        [] -> <<"text/gemini">>;
        [_Dot|Rest] -> Key = erlang:list_to_binary(Rest),
                      case blizanci_mimetypes:lookup(Key) of
                          notfound -> <<"application/octet-stream">>;
                          {ok, Result} -> Result
                      end
    end.                             

-spec format_headers(integer(), binary()) -> iolist().
format_headers(Code, MimeType) when is_integer(Code), is_binary(MimeType) ->
    Status = list_to_binary(integer_to_list(Code)),
    [Status, <<" ">>, MimeType, <<"\r\n">>].

-spec format_response(integer(), binary(), binary()) -> {ok, iolist()}.
format_response(Code, MimeType, Data) ->
    Headers = format_headers(Code, MimeType),
    {ok, [Headers, Data]}.

-spec invalid_request(binary()) -> {ok, iolist()}.
invalid_request(Msg) when is_binary(Msg) ->
    format_response(59, <<"text/plain">>, Msg).

invalid_request_test() ->
    Result = invalid_request(<<"Garbled request">>),
    {ok, Data} = Result,
    ?assertEqual(<<"59 text/plain\r\nGarbled request">>,
                 iolist_to_binary(Data)).
