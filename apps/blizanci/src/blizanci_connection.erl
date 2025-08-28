%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

% The main code path consists in transforming data and handing it off
% down the call chain (always towards the bottom of the file) as follows:
%
%   handle_info({ssl, ...}):
%     new data from TLS, add it to buffer
%
%   handle_request(...):
%     extract line from buffer if one is complete
%
%   handle_line(...):
%     decode URL from line (i.e., Unicode, RFC-3986 percent escaping)
%     (now in a separate module)

-module(blizanci_connection).
-behaviour(gen_server).
-include("gen_server.hrl").
-behaviour(ranch_protocol).

-include("blizanci_types.hrl").

%% API to be called by other blizanci modules
-export([start_link/3]).
-export([servlet_result/2]).

%% API expected by gen_server callback behaviour
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).


%% types / constants / enums / config
-define(EMPTY_BUF, <<>>).
-define(PROTO, <<"gemini">>).
-define(CRLF, <<"\r\n">>).
-define(TIMEOUT_MS, 30000).
-define(MAX_REQUEST_BYTES, 4000).
-define(CGI_MODULE, blizanci_cgi).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-spec start_link(Ref, Transport, Opts) -> Result
              when Ref       :: pid(),
                   Transport :: any(),
                   Opts      :: [any()],
                   Result    :: {'ok', pid()}.
start_link(Ref, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [{Ref, Transport, Opts}]).


-spec servlet_result(Pid, Result) -> 'ok'
              when Pid    :: pid(),
                   Result :: servlet_result().
servlet_result(Pid, Result) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true ->
            ok = gen_server:call(Pid, {servlet_result, Result});
        _ ->
            ok
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc
%% @hidden
%% @end
init({Ref, Transport, Opts}) ->
    ok = proc_lib:init_ack({ok, self()}),
    {ok, Socket} = ranch:handshake(Ref),
    ok = activate(Transport, Socket),
    PeerCert = ssl:peercert(Socket),
    Hostname = erlang:list_to_binary(proplists:get_value(hostname, Opts)),
    Port = proplists:get_value(port, Opts),
    Routing = proplists:get_value(routing, Opts),
    Docroot = proplists:get_value(docroot, Opts),
    Config = #server_config{
                hostname=Hostname,
                port=Port,
                routing=Routing,
                docroot=Docroot},
    State = #state{
               transport=Transport,
               socket=Socket,
               buffer=?EMPTY_BUF,
               config=Config,
               requested=false,
               servlet_proc=no_proc,
               client_cert=PeerCert},
    erlang:send_after(?TIMEOUT_MS, self(), timeout),
    gen_server:enter_loop(?MODULE, [], State).

%% @doc
%% @hidden
%% @end
handle_info({ssl, Socket, Payload}, State) ->
    handle_ssl(Socket, Payload, State);
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};

handle_info({tcp_error, _, _Reason}, State) ->
    {stop, normal, State};

handle_info(timeout, State=#state{servlet_proc={proc, _ServletProc}}) ->
    %lager:debug("Proc Timeout ~p", [ServletProc]),
    respond({error_code, request_timeout}, State),
    self() ! finished,
    {stop, normal, State};

handle_info(timeout, State) ->
    %lager:debug("NoProc Timeout: ~p", [State]),
    respond({error_code, response_timeout}, State),
    self() ! finished,
    {stop, normal, State};

handle_info(finished, State) ->
    %% TBD cleanly close SSL?
    {stop, normal, State};

handle_info({ssl_closed, _SocketInfo}, State) ->
    {stop, normal, State};

handle_info({'EXIT', Pid, {shutdown, {gateway_init_error, RPid, _Reason}}},
            State=#state{servlet_proc={proc, ProcPid}})
  when Pid =:= RPid, Pid =:= ProcPid ->
    %lager:info("Gateway init failure: ~p", [Reason]),
    respond({error_code, internal_server_error}, State),
    self() ! finished,
    {noreply, State};

%% TBD: factor together with previous function definition
handle_info({'EXIT', Pid, {shutdown, {gateway_complete, RPid, Response}}},
            State=#state{servlet_proc={proc, ProcPid}})
  when Pid =:= RPid, Pid =:= ProcPid ->
    respond(Response, State),
    self() ! finished,
    {noreply, State};


handle_info({'EXIT', _Pid, _Reason}, State) ->
    %lager:info("Abend of process ~p ~p", [Pid, Reason]),
    respond({error_code, internal_server_error}, State),
    self() ! finished,
    {noreply, State};

handle_info(_Msg, State) ->
    %lager:info("Received unrecognised message: ~p~n", [Msg]),
    {stop, normal, State}.

%% TBD: respond + error_code + self ! finis.. etc ought to be factored into
%%      a library
%% @doc
%% @hidden
%% @end
handle_call({servlet_result, {servlet_failed, Result}}, _From, State) ->
    process_flag(trap_exit, false),
    respond({error_code, Result}, State),
    self() ! finished,
    {reply, ok, State};

handle_call({servlet_result, {servlet_complete, Output}}, _From, State) ->
    process_flag(trap_exit, false),
    respond({servlet_output, Output}, State),
    self() ! finished,
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @doc
%% @hidden
%% @end
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc
%% @hidden
%% @end
terminate(_Reason, State) ->
    close_session(State),
    ok.

%% @doc
%% @hidden
%% @end
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec handle_ssl(Socket, Payload, State) -> Result
              when Socket  :: ssl:sslsocket(),
                   Payload :: binary(),
                   State   :: state(),
                   Result  :: any(). % usual OTP return style

handle_ssl(Socket, Payload, State) ->
    {Buffer, Response} =
        try handle_request(Payload, State) of
            Result -> Result
        catch
            _ -> {<<"">>, {error_code, internal_server_error}}
        end,
    NewState = State#state{buffer=Buffer},
    Transport = State#state.transport,

    case activate(Transport, Socket) of
        ok -> case respond(Response, State) of
                  continue -> {noreply, NewState};
                  finished -> Transport:close(Socket),
                              {stop, normal, NewState};
                  {expect_servlet, Pid} ->
                      NewerState = NewState#state{servlet_proc={proc, Pid}},
                      {noreply, NewerState}
              end;
        {error, closed} ->
            {stop, normal, State};
        _ ->
            {stop, normal, State}
    end.

% This ceremony is necessary for telling the Erlang VM to monitor
% the socket for incoming data, and must be called each time the
% socket is to be read from.
-spec activate(Transport, Socket) -> Result
              when Transport :: atom(),
                   Socket    :: any(),
                   Result    :: 'ok' | {'error', _}.
activate(Transport, Socket) ->
    Transport:setopts(Socket, [{active, once}]).


close_session(State) ->
    ServletProc = State#state.servlet_proc,
    blizanci_servlet_container:cancel(ServletProc).


% Send a response back to the client, generally closing the connection
% if it is finished. If the client hasn't managed to send a whole request,
% do nothing.
-spec respond(Response, State) -> Session
              when Response :: gemini_response(),
                   State    :: state(),
                   Session  :: gemini_session().

respond(none, _State) ->
    % don't hang up where only part of the URL + CRLF has been received
    continue;

respond({init_servlet, Pid}, _State) ->
    {expect_servlet, Pid};

respond({servlet_output, Msg}, #state{transport=Transport, socket=Socket}) ->
    Header = format_headers(20, <<"text/plain">>),
    Transport:send(Socket, [Header, Msg]),
    Transport:close(Socket),
    finished;

respond(hangup, _State) ->
    finished;

respond({file, MimeType, Filename},
        #state{transport=Transport, socket=Socket}) ->
    Header = format_headers(20, MimeType),
    Transport:send(Socket, Header),
    Transport:sendfile(Socket, Filename),
    finished;

respond({error_code, Code}, #state{transport=Transport, socket=Socket}) ->
    {ok, Msg} = format_error(Code),
    Transport:send(Socket, Msg),
    finished;

respond({redirect, Path}, State=#state{transport=Transport, socket=Socket}) ->
    Config = State#state.config,
    Meta = construct_local_url(Config, Path),
    {Code, _} = blizanci_status:gemini_status(permanent_redirect),
    Msg = format_headers(Code, Meta),
    Transport:send(Socket, Msg),
    finished;

respond({success, MimeType, Data}, #state{transport=Transport,
                                          socket=Socket}) ->
    Header = format_headers(20, MimeType),
    Transport:send(Socket, Header),
    Transport:send(Socket, Data),
    finished.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Request handling.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Theoretically, a client could send its request very slowly, with
% parts of the URL arriving piecemeal; it could also send a massive
% blob of data all at once. We buffer data received from the client
% in State.buffer, and wait until a CRLF appears. The canonical case
% is of course that the CRLF appears the first time we ever receive
% data, rendering the buffer redundant.
-spec handle_request(Payload, State) -> Result
              when Payload :: binary(),
                   State   :: state(),
                   Result  :: {binary(), gemini_response()}.

handle_request(Payload, #state{buffer=Buffer,
                               config=Config,
                               client_cert=Cert,
                               servlet_proc=no_proc
                              }) ->
    AllInput = erlang:iolist_to_binary([Buffer, Payload]),
    case binary:split(AllInput, ?CRLF) of
        [S] when size(S) > ?MAX_REQUEST_BYTES ->
            {<<>>, {error_code, request_too_long}};
        [_] ->
            {AllInput, none};
        [Line, Rest] ->
            R = blizanci_request:handle_line(Line, Config, Cert, Rest),
            {<<"">>, R};
        _ ->
            %lager:warning("Shouldn't get here"),
            {<<>>, hangup}
    end;
handle_request(Payload, #state{buffer=Buffer,
                               servlet_proc={proc, Proc}
                              }) when is_pid(Proc) ->
    AllInput = erlang:iolist_to_binary([Buffer, Payload]),
    %lager:debug("~p about to send client data ~p", [self(), Payload]),

    try blizanci_servlet_container:handle_client_data(Proc, AllInput) of
        Result ->
            %lager:debug("client data sent"),
            {<<>>, Result}
    catch
        _Err1:_Err2 ->
            %lager:info("error sending client data: ~p ~p", [Err1, Err2]),
            {<<>>, {error_code, internal_server_error}}
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utilities.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-spec format_headers(Code, Meta) -> Result
              when Code   :: integer(),
                   Meta   :: binary(),
                   Result :: iolist().

format_headers(Code, Meta) when is_integer(Code), is_binary(Meta) ->
    Status = list_to_binary(integer_to_list(Code)),
    [Status, <<" ">>, Meta, ?CRLF].


-spec format_error(Code) -> Result
              when Code   :: atom(),
                   Result :: {'ok', iolist()}.

format_error(Code) when is_atom(Code) ->
    {GeminiStatus, Explanation} = blizanci_status:gemini_status(Code),
    Headers = format_headers(GeminiStatus, Explanation),
    {ok, Headers}.


-spec construct_local_url(Config, Path) -> URI
              when Config :: server_config(),
                   Path   :: binary(),
                   URI    :: binary().

construct_local_url(Config, Path) ->
    construct_url(?PROTO,
                  Config#server_config.hostname,
                  Config#server_config.port,
                  Path).


-spec construct_url(Scheme, Hostname, Port, Path) -> URI
              when Scheme   :: binary(),
                   Hostname :: binary(),
                   Port     :: integer(),
                   Path     :: binary(),
                   URI      :: binary().

construct_url(Scheme, Hostname, Port, Path) ->
    uri_string:recompose(#{ scheme => Scheme,
                            host => Hostname,
                            port => Port,
                            path => Path }).
