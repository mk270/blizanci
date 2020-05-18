%%%
%%% Not yet implemented:
%%%
%%%   * MIME type inference
%%%   * correct handling of abbreviated requests
%%%

-module(blizanci_proto).
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
        {socket,
         transport,
         buffer,
         hostname,
         docroot,
         mimetypes}).

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
               docroot=proplists:get_value(docroot, Opts),
               mimetypes=proplists:get_value(mimetypes, Opts)},
    gen_server:enter_loop(?MODULE, [], State).


handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
    {stop, Reason, State};

handle_info(timeout, State) ->
    {stop, normal, State};

handle_info({notification, Msg}, State) ->
    Transport = State#state.transport,
    Transport:send(State#state.socket, Msg),
    {noreply, State};

handle_info(quit, State) ->
    {stop, normal, State};

handle_info({ssl, Socket, Payload}, State) ->
    {Buffer, Response} = handle_request(Payload, State),
    NewState = State#state{buffer=Buffer},
    Transport = State#state.transport,
    ok = case Response of
             none -> ok;
             hangup ->
                 Transport:close(Socket);
             {ok, Msg} ->
                 Transport:send(Socket, Msg),
                 Transport:close(Socket),
                 ok;
             {file, Header, Filename} ->
                 Transport:send(Socket, Header),
                 Transport:sendfile(Socket, Filename),
                 Transport:close(Socket),
                 ok;
             _ -> lager:warning("match fallthrough: ~p", [Response]),
                  ok
         end,
    {noreply, NewState};

handle_info({ssl_closed, _SocketInfo}, State) ->
    {stop, normal, State};

handle_info(Msg, State) ->
    io:format("got unrecognised msg: ~p~n", [Msg]),
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal.

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

handle_line(<<"quit">>, _Hostname, _Docroot) -> hangup;
handle_line(Cmd, Host, Docroot) ->
    {ok, Re} = re:compile("^\([a-z0-9]+\)://\([^/]*\)/\(.*\)$"),
    Match = re:run(Cmd, Re, [{capture, all, binary}]),
    Proto = ?PROTO,

    case Match of
        {match, [_All, Proto, Host, Path]} ->
            handle_file(Path, Docroot);
        {match, [_All, Proto, _Host, _Path]} ->
            invalid_request(<<"Host not recognised">>);
        {match, [_All, _Proto, _Host, _Path]} ->
            invalid_request(<<"Protocol not recognised">>);
        _ ->
            invalid_request(<<"Request not understood">>)
    end.

handle_file(Path, Docroot) ->
    case string:split(Path, "..") of
        [_] -> serve_file(Path, Docroot);
        [_, _] -> invalid_request(<<"Illegal filename">>)
    end.

serve_file(Path, Docroot) ->
    Full = filename:join(Docroot, Path),
    case filelib:is_regular(Full) of
        false -> invalid_request(<<"Illegal file">>);
        true -> {file, format_headers(20, mime_type(Full)), Full}
    end.

mime_type(_Path) ->
    <<"text/gemini">>. % for the time being

format_headers(Code, MimeType) ->
    Status = list_to_binary(integer_to_list(Code)),
    [Status, <<" ">>, MimeType, <<"\r\n">>].

format_response(Code, MimeType, Data) ->
    Headers = format_headers(Code, MimeType),
    {ok, [Headers, Data]}.

invalid_request(Msg) ->
    format_response(59, <<"text/plain">>, Msg).
