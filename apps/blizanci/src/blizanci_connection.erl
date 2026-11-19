%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_connection).

-moduledoc """
The first half of the request path: from TLS data arriving at the
connection process (start_link/3, the ranch entry point) to the call
of blizanci_request:handle_line/4.

See the manual chapter [Request handling](request-handling.md).
""".
-behaviour(gen_server).
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

-ifdef(TEST).
%% Exported for the tests in test/blizanci_connection_tests.erl.
-export([construct_url/4, handle_request/2, resolve_site/2, respond/2,
         shape/1]).
-endif.


%% types / constants / enums / config
-define(EMPTY_BUF, <<>>).
-define(PROTO, <<"gemini">>).

% The port a Gemini URL has when none is written; a URL we build leaves
% it out, as the specification makes the two forms equivalent.
-define(DEFAULT_PORT, 1965).
-define(CRLF, <<"\r\n">>).

% How long a client has to complete the whole exchange, in milliseconds,
% before connection_timeout fires and the connection is answered and
% closed.  The timer is set once, in init/1, and never reset: it is a
% fixed deadline for the connection's whole life, servlet work included,
% not a rolling idle timeout, which a client defeats by trickling just
% enough data to keep resetting it.  So a slow client can only spend its
% own share of the budget, and a separate timeout for a servlet's
% completion would only give it a second budget.  terminate/2 always
% cancels the servlet, however the connection ends; for CGI that kills
% the script.
-define(TIMEOUT_MS, 30000).

% The most bytes buffered while waiting for the CRLF that ends the
% request line. It is a guard on the connection layer's buffer, so a
% client that never sends CRLF cannot make it grow without bound; past
% it the answer is 59. It is deliberately looser than the 1024-byte URL
% limit in blizanci_request (?MAX_URL_BYTES), which is checked once the
% line is complete and also answers 59.
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


-spec servlet_result(Pid, ServletResult) -> Result
              when Pid           :: pid(),
                   ServletResult :: servlet_result(),
                   Result        :: 'ok'.
servlet_result(Pid, Result) when is_pid(Pid) ->
    % A cast: the caller (the servlet container) is never blocked on the
    % connection, which may itself be inside a call to the container. A
    % connection that has gone away simply drops the message.
    gen_server:cast(Pid, {servlet_result, Result}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-doc false.
init({Ref, Transport, Opts}) ->
    ok = proc_lib:init_ack({ok, self()}),
    %% This process owns the flag: it is linked to its servlet and must
    %% turn the servlet's exit into a response.  gen_server also needs it
    %% to see the parent's shutdown and run terminate/2.
    process_flag(trap_exit, true),
    {ok, Socket} = ranch:handshake(Ref),
    ok = activate(Transport, Socket),
    PeerCert = ssl:peercert(Socket),
    Port = proplists:get_value(port, Opts),
    {ok, Site} = resolve_site(Socket, Transport),
    Config = #server_config{
                hostname=Site#site.hostname,
                port=Port,
                routing=Site#site.routing},
    State = #state{
               transport=Transport,
               socket=Socket,
               buffer=?EMPTY_BUF,
               config=Config,
               requested=false,
               servlet_proc=no_proc,
               client_cert=PeerCert},
    erlang:send_after(?TIMEOUT_MS, self(),
                       connection_timeout),
    gen_server:enter_loop(?MODULE, [], State).

-doc false.
handle_info({ssl, Socket, Payload}, State) ->
    handle_ssl(Socket, Payload, State);
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};

handle_info({tcp_error, _, _Reason}, State) ->
    {stop, normal, State};

handle_info(connection_timeout,
            State=#state{servlet_proc={proc, ServletProc}}) ->
    logger:debug("Proc Timeout ~p", [ServletProc]),
    respond({error_code, request_timeout}, State),
    self() ! finished,
    {stop, normal, State};

handle_info(connection_timeout, State) ->
    logger:debug("NoProc Timeout: ~p", [State]),
    respond({error_code, response_timeout}, State),
    self() ! finished,
    {stop, normal, State};

handle_info(finished, State) ->
    %% TBD cleanly close SSL?
    {stop, normal, State};

handle_info({ssl_closed, _SocketInfo}, State) ->
    {stop, normal, State};

handle_info({'EXIT', Pid, {shutdown, {gateway_init_error, RPid, Reason}}},
            State=#state{servlet_proc={proc, ProcPid}})
  when Pid =:= RPid, Pid =:= ProcPid ->
    logger:info("Gateway init failure: ~p", [Reason]),
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


handle_info({'EXIT', Pid, Reason},
            State=#state{servlet_proc={proc, ServletPid}})
  when Pid =:= ServletPid ->
    logger:info("Abend of process ~p ~p", [Pid, Reason]),
    respond({error_code, internal_server_error}, State),
    self() ! finished,
    {noreply, State};

%% Some other linked process (the parent's own shutdown never gets here:
%% gen_server handles it first).
handle_info({'EXIT', Pid, Reason}, State) ->
    logger:info("Ignoring exit of unrelated process ~p ~p", [Pid, Reason]),
    {noreply, State};

handle_info(Msg, State) ->
    logger:info("Received unrecognised message: ~p", [Msg]),
    {stop, normal, State}.

%% TBD: respond + error_code + self ! finis.. etc ought to be factored into
%%      a library
-doc false.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-doc false.
handle_cast({servlet_result, {servlet_failed, Result}}, State) ->
    respond({error_code, Result}, State),
    self() ! finished,
    {noreply, State};

handle_cast({servlet_result, {servlet_complete, Output}}, State) ->
    respond({servlet_output, Output}, State),
    self() ! finished,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

-doc false.
terminate(_Reason, State) ->
    close_session(State),
    ok.

-doc false.
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
            % anything raised by the request pipeline (router, auth, a
            % servlet's request/4) is a 40, whatever its class: a bare
            % `catch _` would only have caught throws
            Class:Reason:Stack ->
                logger:warning("request handling failed: ~p:~p~n~p",
                               [Class, Reason, Stack]),
                {<<"">>, {error_code, internal_server_error}}
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


% Resolve the site the TLS layer negotiated via SNI. Should always
% succeed -- blizanci_config:ssl_opts/1's sni_fun already refuses any
% hostname that isn't a known site (or no SNI at all) before the
% handshake can even complete. Both look sites up in blizanci_conf_serv, so
% the only way to miss here is a reconfiguration landing between the
% handshake and this lookup, which removed the site. A client that has
% already disconnected can't be asked its SNI hostname at all; that is
% an ordinary event, not an error.
-spec resolve_site(Socket, Transport) -> Result
              when Socket    :: ssl:sslsocket(),
                   Transport :: atom(),
                   Result    :: {'ok', site()}.

resolve_site(Socket, Transport) ->
    case ssl:connection_information(Socket, [sni_hostname]) of
        {ok, [{sni_hostname, SniHostname}]} ->
            find_negotiated_site(SniHostname, Socket, Transport);
        {error, _Reason} ->
            Transport:close(Socket),
            exit(normal)
    end.


-spec find_negotiated_site(SniHostname, Socket, Transport) -> Result
              when SniHostname :: string() | 'undefined',
                   Socket      :: ssl:sslsocket(),
                   Transport   :: atom(),
                   Result      :: {'ok', site()}.
find_negotiated_site(SniHostname, Socket, Transport) ->
    Found = is_list(SniHostname) andalso
            blizanci_conf_serv:find_site(erlang:list_to_binary(SniHostname)),
    case Found of
        {ok, Site} -> {ok, Site};
        _ ->
            logger:warning("SNI hostname ~p accepted at TLS layer but no "
                           "matching site found -- site removed by a "
                           "reconfiguration mid-handshake?",
                           [SniHostname]),
            Transport:close(Socket),
            exit(normal)
    end.


close_session(State) ->
    ServletProc = State#state.servlet_proc,
    blizanci_servlet_container:cancel(ServletProc).


% Send a response back to the client, generally closing the connection
% if it is finished. If the client hasn't managed to send a whole request,
% do nothing.
-spec respond(Response, State) -> Session
              % term(): the final clause answers 40 to anything else
              when Response :: gemini_response() | term(),
                   State    :: state(),
                   Session  :: gemini_session().

respond(none, _State) ->
    % don't hang up where only part of the URL + CRLF has been received
    continue;

respond({init_servlet, Pid}, _State) ->
    {expect_servlet, Pid};

respond({servlet_output, Msg},
        State=#state{transport=Transport, socket=Socket}) ->
    case parse_cgi_output(Msg) of
        {ok, Code, Meta, Body} ->
            Transport:send(Socket, [format_headers(Code, Meta), Body]),
            Transport:close(Socket),
            finished;
        error ->
            respond({error_code, cgi_exec_error}, State)
    end;

respond(hangup, _State) ->
    finished;

% The file is opened once, before the header: a file that cannot be
% opened is answered with an error status instead of an empty `20', and
% what is sent is the handle that was checked.  The transport's sendfile
% takes the open file (and leaves it open), so it is closed here.
respond({file, MimeType, Filename}, State) ->
    case file:open(Filename, [read, raw, binary]) of
        {ok, Fd} ->
            try
                send_open_file(MimeType, Fd, State)
            after
                _ = file:close(Fd)
            end,
            finished;
        {error, Reason} when Reason =:= enoent; Reason =:= enotdir;
                             Reason =:= eisdir ->
            respond({error_code, file_not_found}, State);
        {error, Reason} ->
            logger:warning("cannot open file to send: ~p", [Reason]),
            respond({error_code, internal_server_error}, State)
    end;

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
    Header = format_headers(success_status(), MimeType),
    Transport:send(Socket, Header),
    Transport:send(Socket, Data),
    finished;

% Anything else is a bug in whoever produced it (the spec says it cannot
% happen), but must not crash the connection outside the request's try:
% log its shape only, as it may hold client data, and answer 40.
respond(Other, State) ->
    logger:error("unexpected response ~p; answering 40", [shape(Other)]),
    respond({error_code, internal_server_error}, State).


% The numeric status the table gives for success.
-spec success_status() -> Status
              when Status :: integer().

success_status() ->
    {Status, _Meta} = blizanci_status:gemini_status(success),
    Status.


% What kind of term this is, without its contents.
-spec shape(Term) -> Shape
              when Term  :: term(),
                   Shape :: atom() | {'tuple', non_neg_integer(), atom()}.

shape(Term) when is_atom(Term)  -> Term;
shape(Term) when is_tuple(Term) ->
    Tag = case tuple_size(Term) > 0 andalso element(1, Term) of
              A when is_atom(A), A =/= false -> A;
              _ -> none
          end,
    {tuple, tuple_size(Term), Tag};
shape(Term) when is_binary(Term)  -> binary;
shape(Term) when is_list(Term)    -> list;
shape(Term) when is_number(Term)  -> number;
shape(_)                          -> other.


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
    AllInput = <<Buffer/binary, Payload/binary>>,
    case binary:split(AllInput, ?CRLF) of
        [S] when size(S) > ?MAX_REQUEST_BYTES ->
            {<<>>, {error_code, request_too_long}};
        [_] ->
            {AllInput, none};
        [Line, Rest] ->
            R = blizanci_request:handle_line(Line, Config, Cert, Rest),
            {<<"">>, R};
        _ ->
            logger:warning("Shouldn't get here"),
            {<<>>, hangup}
    end;
handle_request(Payload, #state{buffer=Buffer,
                               servlet_proc={proc, Proc}
                              }) when is_pid(Proc) ->
    AllInput = <<Buffer/binary, Payload/binary>>,
    logger:debug("~p about to send client data ~p", [self(), Payload]),

    try blizanci_servlet_container:handle_client_data(Proc, AllInput) of
        Result ->
            logger:debug("client data sent"),
            {<<>>, Result}
    catch
        Err1:Err2 ->
            logger:info("error sending client data: ~p ~p", [Err1, Err2]),
            {<<>>, {error_code, internal_server_error}}
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utilities.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-spec parse_cgi_output(Buffer) -> Result
              when Buffer :: binary(),
                   Result :: {'ok', integer(), binary(), binary()} | 'error'.

parse_cgi_output(Buffer) ->
    case binary:split(Buffer, <<"\r\n">>) of
        [FirstLine, Body] ->
            case parse_gemini_header(FirstLine) of
                {ok, Code, Meta} -> {ok, Code, Meta, Body};
                error            -> error
            end;
        _ ->
            error
    end.


-spec parse_gemini_header(Line) -> Result
              when Line   :: binary(),
                   Result :: {'ok', integer(), binary()} | 'error'.

parse_gemini_header(Line) ->
    case binary:split(Line, <<" ">>) of
        [CodeBin, Meta] ->
            try binary_to_integer(CodeBin) of
                Code ->
                    case blizanci_status:valid_code(Code) of
                        true  -> {ok, Code, Meta};
                        false -> error
                    end
            catch
                _:_ -> error
            end;
        _ ->
            error
    end.



-spec format_headers(Code, Meta) -> Result
              when Code   :: integer(),
                   Meta   :: binary(),
                   Result :: iolist().

format_headers(Code, Meta) when is_integer(Code), is_binary(Meta) ->
    Status = list_to_binary(integer_to_list(Code)),
    [Status, <<" ">>, Meta, ?CRLF].


% Once the header has gone the status cannot change: a failed send is
% only logged, and the connection finished by the caller.
-spec send_open_file(MimeType, Fd, State) -> ok
              when MimeType :: binary(),
                   Fd       :: file:fd(),
                   State    :: #state{}.

send_open_file(MimeType, Fd, #state{transport=Transport, socket=Socket}) ->
    Header = format_headers(success_status(), MimeType),
    case Transport:send(Socket, Header) of
        ok ->
            case Transport:sendfile(Socket, Fd) of
                {ok, _Sent} ->
                    ok;
                {error, Reason} ->
                    log_send_failure(Reason)
            end;
        {error, Reason} ->
            log_send_failure(Reason)
    end.

-spec log_send_failure(Reason) -> ok when Reason :: term().

log_send_failure(Reason) when Reason =:= closed; Reason =:= timeout;
                              Reason =:= econnreset; Reason =:= epipe ->
    logger:debug("file send stopped: ~p", [Reason]);
log_send_failure(Reason) ->
    logger:warning("file send failed: ~p", [Reason]).


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

% Path is a decoded path (as served), so it is percent-encoded here, a
% segment at a time; otherwise a name containing a space, "%", "?" or "#"
% would make a malformed or misleading URL.
construct_url(Scheme, Hostname, Port, Path) ->
    Parts = #{ scheme => Scheme,
               host => Hostname,
               path => blizanci_urlpath:encode_path(Path) },
    uri_string:recompose(case Port of
                             ?DEFAULT_PORT -> Parts;
                             _ -> Parts#{ port => Port }
                         end).
