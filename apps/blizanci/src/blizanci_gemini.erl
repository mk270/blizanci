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
-export([handle_line/4]). % tmp

-define(EMPTY_BUF, <<>>).
-define(PROTO, <<"gemini:">>).
-define(INDEX, "index.gemini").
-define(TIMEOUT_MS, 10000).

-record(state,
        {socket    :: inet:socket(),
         transport :: atom(),
         buffer    :: binary(),
         hostname  :: binary(),
         port      :: binary(),
         docroot   :: string()}).

-type state() :: #state{}.
-type gemini_response() :: {'file', binary(), binary()}
                         | {'error_code', atom()}
                         | {'redirect', binary()}.

-spec error_detail(atom()) -> {integer(), binary()}.
error_detail(request_too_long)       -> {59, <<"Request too long">>};
error_detail(request_not_parsed)     -> {59, <<"Request not parsed">>};
error_detail(proxy_refused)          -> {53, <<"Proxy request refused">>};
error_detail(host_unrecognised)      -> {53, <<"Host unrecognised">>};
error_detail(port_unrecognised)      -> {53, <<"Port unrecognised">>};
error_detail(unrecognised_protocol)  -> {59, <<"Protocol not recognised">>};
error_detail(request_not_understood) -> {59, <<"Request not understood">>};
error_detail(bad_unicode)            -> {59, <<"Bad unicode in request">>};
error_detail(bad_filename)           -> {59, <<"Illegal filename">>};
error_detail(internal_server_error)  -> {40, <<"Internal server error">>};
error_detail(file_not_found)         -> {51, <<"File not found">>}.


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
    Port = integer_to_binary(proplists:get_value(port, Opts)),
    State = #state{
               socket=Socket,
               transport=Transport,
               buffer=?EMPTY_BUF,
               hostname=Hostname,
               port=Port,
               docroot=proplists:get_value(docroot, Opts)},
    erlang:send_after(?TIMEOUT_MS, self(), timeout),
    gen_server:enter_loop(?MODULE, [], State).


handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
    {stop, Reason, State};

handle_info(timeout, State) ->
    {stop, normal, State};

handle_info({ssl, Socket, Payload}, State) ->
    {Buffer, Response} =
        try handle_request(Payload, State) of
            Result -> Result
        catch
            _ -> {<<"">>, {error_code, internal_server_error}}
        end,
    NewState = State#state{buffer=Buffer},
    Transport = State#state.transport,

    case Transport:setopts(Socket, [{active, once}]) of
        {error, closed} ->
            {stop, normal, State};
        ok -> case respond(Transport, Socket, State, Response) of
                  continue -> {noreply, NewState};
                  finished -> Transport:close(Socket),
                              {noreply, NewState}
              end
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


-spec respond(atom(), inet:socket(), state(), gemini_response())
             -> 'continue' | 'finished'.
respond(_Transport, _Socket, _State, none) -> continue;

respond(_Transport, _Socket, _State, hangup) -> finished;

respond(Transport, Socket, _State, {file, MimeType, Filename}) ->
    Header = format_headers(20, MimeType),
    Transport:send(Socket, Header),
    Transport:sendfile(Socket, Filename),
    finished;

respond(Transport, Socket, _State, {error_code, Code}) ->
    {ok, Msg} = format_error(Code),
    Transport:send(Socket, Msg),
    finished;

respond(Transport, Socket, State, {redirect, Path}) ->
    Host = State#state.hostname,
    Port = State#state.port,
    Msg = <<"31 gemini://", Host/binary, ":",
            Port/binary, Path/binary, "\r\n">>,
    Transport:send(Socket, Msg),
    finished.


-spec handle_request(binary(), state()) -> {binary(), any()}.
handle_request(Payload, #state{buffer=Buffer,
                               hostname=Hostname,
                               port=Port,
                               docroot=Docroot}) ->
    AllInput = erlang:iolist_to_binary([Buffer, Payload]),
    case binary:split(AllInput, <<"\r\n">>) of
        [_] ->
            {AllInput, none};
        [Line, Rest] ->
            R = handle_line(Line, Hostname, Port, Docroot),
            {Rest, R};
        _ ->
            lager:warning("Shouldn't get here"),
            {<<>>, hangup}
    end.


-spec handle_line(binary(), binary(), binary(), string())
                 -> gemini_response().
handle_line(Cmd, _Host, _Port, _Docroot) when is_binary(Cmd),
                                              size(Cmd) > 1024 ->
    {error_code, request_too_long};

handle_line(Cmd, Host, Port, Docroot) when is_binary(Cmd) ->
    {ok, Re} = re:compile("^"
                          ++ "\([a-z0-9]+:\)?//"
                          ++ "\([^/:]+\)"
                          ++ "\(:[0-9]+\)?"
                          ++ "/\(.*\)?"
                          ++ "$"
                         ),
    Match = re:run(Cmd, Re, [{capture, all, binary}]),

    case Match of
        {match, [_All|Matches]} ->
            %lager:info("Matches: ~p", [Matches]),
            handle_url(Matches, Host, Port, Docroot);
        nomatch ->
            {ok, Re2} = re:compile("^"
                                   ++ "\([a-z0-9]+:\)?//"
                                   ++ "\([^/:]+\)"
                                   ++ "\(:[0-9]+\)?"
                                   ++ "$"
                                  ),
            Match2 = re:run(Cmd, Re2, [{capture, all, binary}]),
            case Match2 of
                {match, [_All|[Scheme, ReqHost, ReqPort]]} ->
                    handle_url([Scheme, ReqHost, ReqPort, <<"/">>],
                               Host, Port, Docroot);
                {match, [_All|[Scheme, ReqHost]]} ->
                    handle_url([Scheme, ReqHost, <<":", Port/binary>>, <<"">>],
                               Host, Port, Docroot);
                nomatch -> {error_code, request_not_parsed}
            end
    end.


-spec handle_url([any()], binary(), binary(), string()) -> gemini_response().
handle_url([<<"gopher:">>, _ReqHost, _ReqPort, _Path], _Host, _Port, _Docroot)
->
    {error_code, proxy_refused};

handle_url([<<"https:">>, _ReqHost, _ReqPort, _Path], _Host, _Port, _Docroot)
->
    {error_code, proxy_refused};

handle_url([<<"http:">>, _ReqHost, _ReqPort, _Path], _Host, _Port, _Docroot)
->
    {error_code, proxy_refused};

handle_url([?PROTO, ReqHost, ReqPort, Path], Host, Port, Docroot) ->
    handle_gemini_url(ReqHost, ReqPort, Path, Host, Port, Docroot);

handle_url([?EMPTY_BUF, ReqHost, ReqPort, Path], Host, Port, Docroot) ->
    handle_gemini_url(ReqHost, ReqPort, Path, Host, Port, Docroot);

handle_url([Proto, ReqHost, ReqPort, Path], _Host, _Port, _Docroot) ->
    lager:info("unrec: ~p", [{Proto, ReqHost, ReqPort, Path}]),
    {error_code, unrecognised_protocol};

handle_url(_, _Host, _Port, _Docroot) ->
    {error_code, request_not_understood}.

-spec handle_gemini_url(binary(), binary(), binary(), binary(), bitstring(),
                        string()) -> gemini_response().
handle_gemini_url(ReqHost, ReqPort, Path, Host, Port, Docroot) ->
    MatchPort = <<":", Port/binary>>,
    case {ReqHost, ReqPort} of
        {Host, <<>>}      -> handle_file(Path, Docroot);
        {Host, MatchPort} -> handle_file(Path, Docroot);
        {_,    MatchPort} -> {error_code, host_unrecognised};
        {Host, _}         -> {error_code, port_unrecognised};
        _                 -> {error_code, host_unrecognised}
    end.


-spec handle_file(binary(), string()) -> gemini_response().
handle_file(Path, Docroot) when is_binary(Path), is_list(Docroot) ->
    Recoded = unicode:characters_to_binary(<<Path/binary>>, utf8),
    case Recoded of
        {error, _, _}      -> {error_code, bad_unicode};
        {incomplete, _, _} -> {error_code, bad_unicode};
        _ ->
            case string:split(Path, "..") of
                [_] -> serve_file(Path, Docroot);
                [_, _] -> {error_code, bad_filename}
            end
    end.


-spec serve_file(binary(), string()) -> gemini_response().
serve_file(Path, Docroot) ->
    Full = filename:join(Docroot, Path),
    % lager:info("Path: ~p", [{Docroot, Path, Full}]),
    case {filelib:is_dir(Full), filelib:is_regular(Full)} of
        {true, _} ->
            Redirect = filename:join(Path, ?INDEX),
            {redirect, Redirect};
        {false, true} ->
            MimeType = mime_type(Full),
            {file, MimeType, Full};
        _ ->
            {error_code, file_not_found}
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
format_headers(Code, Meta) when is_integer(Code), is_binary(Meta) ->
    Status = list_to_binary(integer_to_list(Code)),
    [Status, <<" ">>, Meta, <<"\r\n">>].


-spec format_error(atom()) -> {'ok', iolist()}.
format_error(Code) when is_atom(Code) ->
    {GeminiStatus, Explanation} = error_detail(Code),
    Headers = format_headers(GeminiStatus, Explanation),
    {ok, Headers}.
    

%% tests

handle_line_test_data() ->
    [
     {{file, <<"text/gemini">>, <<"/bin/which">>},
      <<"gemini://this.host.dev/which">>},

     {{file, <<"text/gemini">>, <<"/bin/which">>},
      <<"gemini://this.host.dev:1965/which">>}
    ].

handle_line_test_() ->
    [ ?_assertEqual(Expected, handle_line(TestInput,
                                          <<"this.host.dev">>,
                                          <<"1965">>,
                                          "/bin")) ||
        {Expected, TestInput} <- handle_line_test_data() ].
