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
-behaviour(ranch_protocol).

-include("blizanci_types.hrl").
-include_lib("eunit/include/eunit.hrl").

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


-spec servlet_result(Pid, ServletResult) -> Result
              when Pid           :: pid(),
                   ServletResult :: servlet_result(),
                   Result        :: 'ok'.
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
    Port = proplists:get_value(port, Opts),
    {ok, Site} = resolve_site(Socket, Transport),
    Config = #server_config{
                hostname=Site#site.hostname,
                port=Port,
                routing=Site#site.routing,
                docroot=Site#site.docroot},
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

handle_info(timeout, State=#state{servlet_proc={proc, ServletProc}}) ->
    logger:debug("Proc Timeout ~p", [ServletProc]),
    respond({error_code, request_timeout}, State),
    self() ! finished,
    {stop, normal, State};

handle_info(timeout, State) ->
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


handle_info({'EXIT', Pid, Reason}, State) ->
    logger:info("Abend of process ~p ~p", [Pid, Reason]),
    respond({error_code, internal_server_error}, State),
    self() ! finished,
    {noreply, State};

handle_info(Msg, State) ->
    logger:info("Received unrecognised message: ~p", [Msg]),
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
              when Response :: gemini_response(),
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
            logger:warning("Shouldn't get here"),
            {<<>>, hangup}
    end;
handle_request(Payload, #state{buffer=Buffer,
                               servlet_proc={proc, Proc}
                              }) when is_pid(Proc) ->
    AllInput = erlang:iolist_to_binary([Buffer, Payload]),
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
    uri_string:recompose(#{ scheme => Scheme,
                            host => Hostname,
                            port => Port,
                            path => blizanci_urlpath:encode_path(Path) }).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% A redirect's location is the decoded path, encoded again.
construct_url_test_() ->
    U = fun(Path) -> construct_url(<<"gemini">>, <<"h">>, 1965, Path) end,
    [?_assertEqual(<<"gemini://h:1965/dir1/index.gemini">>,
                   U(<<"dir1/index.gemini">>)),
     ?_assertEqual(<<"gemini://h:1965/sp%20ace/index.gemini">>,
                   U(<<"sp ace/index.gemini">>)),
     ?_assertEqual(<<"gemini://h:1965/100%25/index.gemini">>,
                   U(<<"100%/index.gemini">>)),
     ?_assertEqual(<<"gemini://h:1965/x%3Fy%23z/index.gemini">>,
                   U(<<"x?y#z/index.gemini">>)),
     % what the client receives decodes back to the path
     ?_test(begin
                Path = <<"a b/100%/x?y#z;w/index.gemini">>,
                Url = U(Path),
                #{ path := Encoded } = uri_string:parse(Url),
                <<"/", Raw/binary>> = Encoded,
                ?assertEqual({ok, {decoded_path, Path}},
                             blizanci_urlpath:decode_path(Raw))
            end)].

%% A client that disconnects right after the handshake must make the
%% connection process exit normally, not crash.
resolve_site_client_gone_test_() ->
    {setup,
     fun() -> {ok, _} = application:ensure_all_started(ssl) end,
     fun(_) -> ok end,
     {timeout, 30, fun resolve_site_client_gone/0}}.

resolve_site_client_gone() ->
    Rsa = [{key, {rsa, 2048, 17}}],
    #{server_config := ServerConf, client_config := ClientConf} =
        public_key:pkix_test_data(#{server_chain => #{root => Rsa,
                                                      peer => Rsa},
                                    client_chain => #{root => Rsa,
                                                      peer => Rsa}}),
    {ok, Listen} = ssl:listen(0, ServerConf ++ [{versions, ['tlsv1.3']},
                                                {active, false}]),
    {ok, {_, Port}} = ssl:sockname(Listen),
    Self = self(),
    {Pid, Ref} = spawn_monitor(
                   fun() ->
                       {ok, T} = ssl:transport_accept(Listen),
                       {ok, S} = ssl:handshake(T, 5000),
                       Self ! {handshaken, self()},
                       {error, closed} = ssl:recv(S, 0, 5000),
                       resolve_site(S, ranch_ssl)
                   end),
    {ok, C} = ssl:connect("localhost", Port,
                          ClientConf ++ [{verify, verify_none},
                                         {versions, ['tlsv1.3']},
                                         {server_name_indication,
                                          "localhost"}]),
    receive {handshaken, Pid} -> ok after 5000 -> error(no_handshake) end,
    ok = ssl:close(C),
    receive
        {'DOWN', Ref, process, Pid, Reason} ->
            ?assertEqual(normal, Reason)
    after 5000 ->
        error(server_did_not_exit)
    end,
    ssl:close(Listen).


%% What the client is sent when the servlet container reports that a
%% servlet refused the request before its worker started: the reason
%% reaches the client where the status table has a failure status for it
%% (a missing CGI script gives 51), and anything else is a 40. The exit
%% message fed to handle_info/2 is the container's real one, and the
%% client is a real TCP socket. Fixture, under a fresh temporary directory:
%%   Base/cgi/            (empty)
refused_request_test_() ->
    {setup, fun refused_setup/0, fun refused_teardown/1,
     fun refused_tests/1}.

refused_setup() ->
    Unique = integer_to_list(erlang:unique_integer([positive])),
    Base = filename:absname("connection_refused_test_" ++ Unique),
    ok = filelib:ensure_path(filename:join(Base, "cgi")),
    Base.

refused_teardown(Base) ->
    file:del_dir_r(Base).

%% Feed handle_info/2 the exit message of a container whose servlet
%% module's serve/4 refused the request, and return the bytes the client
%% receives.
-spec client_sees(Module, Matches, RouteOpts) -> Result
              when Module    :: module(),
                   Matches   :: path_matches(),
                   RouteOpts :: map(),
                   Result    :: binary().
client_sees(Module, Matches, RouteOpts) ->
    process_flag(trap_exit, true),
    Cfg = #server_config{hostname = <<"h">>, port = 1965, routing = []},
    Req = #{ client_cert => {error, no_peercert}, query => <<>>,
             rest_of_input => <<>>, scheme => <<"gemini">>, host => <<"h">>,
             port => 1965, path => <<>> },
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}]),
    {ok, Port} = inet:port(Listen),
    {ok, Client} = gen_tcp:connect({127, 0, 0, 1}, Port,
                                   [binary, {active, false}]),
    {ok, Server} = gen_tcp:accept(Listen, 5000),
    {ok, Pid} = proc_lib:start_link(blizanci_servlet_container, init,
                                    [[self(), Module, Matches, Req, Cfg,
                                      RouteOpts]]),
    receive
        {'EXIT', Pid, _} = Exit ->
            State = #state{transport = ranch_tcp, socket = Server,
                           buffer = <<>>, config = Cfg, requested = true,
                           servlet_proc = {proc, Pid},
                           client_cert = {error, no_peercert}},
            {noreply, _} = handle_info(Exit, State)
    after 5000 ->
        error(servlet_did_not_exit)
    end,
    {ok, Data} = gen_tcp:recv(Client, 0, 5000),
    ok = gen_tcp:close(Client),
    ok = gen_tcp:close(Server),
    ok = gen_tcp:close(Listen),
    Data.

refused_tests(Base) ->
    Opts = #{ cgiroot => filename:join(Base, "cgi"),
              cgiprefix => "/cgi-bin/" },
    Cgi = fun(Path) ->
              client_sees(blizanci_cgi, #{ <<"PATH">> => Path }, Opts)
          end,
    [?_assertEqual(<<"51 File not found\r\n">>, Cgi(<<"nosuch">>)),
     ?_assertEqual(<<"51 File not found\r\n">>, Cgi(<<".hidden">>)),
     ?_assertEqual(<<"51 File not found\r\n">>, Cgi(<<"a b">>)),
     ?_assertEqual(<<"40 Internal server error\r\n">>,
                   client_sees(blizanci_static, #{}, #{}))].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests: respond/2, handle_request/2, handle_info/2 and handle_call/3,
%% with a loopback TCP socket in place of the TLS one (ranch_tcp gives
%% the same transport interface), and the stub servlet in
%% test/blizanci_test_stub.erl behind a real route. Each test runs in a
%% process of its own, as deferring a request makes its caller trap exits.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(STUB, blizanci_test_stub).
-define(REQLINE, <<"gemini://h/x\r\n">>).

%% Hide a value's type from dialyzer, for tests that break a spec.
-spec any(term()) -> term().
any(X) -> erlang:binary_to_term(erlang:term_to_binary(X)).

-spec isolated(Fun) -> Result
              when Fun    :: fun(() -> term()),
                   Result :: term().
isolated(Fun) ->
    Self = self(),
    Ref = make_ref(),
    {Pid, MRef} = spawn_monitor(fun() -> Self ! {Ref, Fun()} end),
    receive
        {Ref, Result} ->
            erlang:demonitor(MRef, [flush]),
            Result;
        {'DOWN', MRef, process, Pid, Why} ->
            {test_process_died, Why}
    after 10000 ->
        exit(Pid, kill),
        error(test_timeout)
    end.

dead_pid() ->
    Pid = spawn(fun() -> ok end),
    Ref = erlang:monitor(process, Pid),
    receive {'DOWN', Ref, process, Pid, _} -> Pid end.

drain(Client, Acc) ->
    case gen_tcp:recv(Client, 0, 2000) of
        {ok, Data} -> drain(Client, <<Acc/binary, Data/binary>>);
        {error, _} -> Acc
    end.

%% Run Fun with the server end of a loopback connection, in a process of
%% its own, and return {FunResult, WhatTheClientReceived}.
-spec with_client(Fun) -> Result
              when Fun    :: fun((port()) -> term()),
                   Result :: {term(), binary()}.
with_client(Fun) ->
    isolated(fun() ->
        {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}]),
        {ok, Port} = inet:port(Listen),
        {ok, Client} = gen_tcp:connect({127, 0, 0, 1}, Port,
                                       [binary, {active, false}]),
        {ok, Server} = gen_tcp:accept(Listen, 5000),
        Result = Fun(Server),
        _ = gen_tcp:close(Server),
        Data = drain(Client, <<>>),
        ok = gen_tcp:close(Client),
        ok = gen_tcp:close(Listen),
        {Result, Data}
    end).

%% Routing for the stub servlet: every path goes to it.
stub_config(Script) ->
    Routes = [{gemini, <<"(?<PATH>.*)">>, ?STUB, public,
               #{ script => Script }}],
    {ok, Routing} = blizanci_router:prepare(Routes),
    #server_config{hostname = <<"h">>, port = 1965, routing = Routing}.

conn_state(Socket, Proc, Script) ->
    #state{transport = ranch_tcp, socket = Socket, buffer = <<>>,
           config = stub_config(Script), requested = false,
           servlet_proc = Proc, client_cert = {error, no_peercert}}.

conn_state(Socket) ->
    conn_state(Socket, no_proc, fun(request) -> {immediate, none} end).

blank_state() ->
    conn_state(none).

%% What the client is sent for a response.
sent(Response) ->
    {_, Data} = with_client(fun(S) -> respond(Response, conn_state(S)) end),
    Data.

%% Start a servlet through a real request, with a stub worker that
%% answers client data with Handler(Payload), and return its pid.
start_servlet(Socket, Handler) ->
    Worker = (any(?STUB)):worker(self(), Handler),
    Script = fun(request) -> defer;
                (serve) -> {gateway_started, Worker}
             end,
    State = conn_state(Socket, no_proc, Script),
    {<<>>, {init_servlet, Pid}} = handle_request(?REQLINE, State),
    {Pid, Worker, State#state{servlet_proc = {proc, Pid}}}.

respond_test_() ->
    [% nothing to say yet: stay connected, send nothing
     ?_assertEqual({continue, <<>>},
                   with_client(fun(S) ->
                                   respond(none, conn_state(S))
                               end)),
     ?_test(begin
                Pid = dead_pid(),
                ?assertEqual({{expect_servlet, Pid}, <<>>},
                             with_client(fun(S) ->
                                 respond({init_servlet, Pid},
                                         conn_state(S))
                             end))
            end),
     ?_assertEqual({finished, <<>>},
                   with_client(fun(S) ->
                                   respond(hangup, conn_state(S))
                               end)),
     ?_assertEqual({finished, <<"20 text/gemini\r\nhi">>},
                   with_client(fun(S) ->
                       respond({success, <<"text/gemini">>, <<"hi">>},
                               conn_state(S))
                   end)),
     ?_assertEqual({finished, <<"20 text/gemini\r\n">>},
                   with_client(fun(S) ->
                       respond({success, <<"text/gemini">>, <<>>},
                               conn_state(S))
                   end)),
     ?_assertEqual({finished, <<"51 File not found\r\n">>},
                   with_client(fun(S) ->
                       respond({error_code, file_not_found},
                               conn_state(S))
                   end)),
     ?_assertEqual({finished, <<"59 Request too long\r\n">>},
                   with_client(fun(S) ->
                       respond({error_code, request_too_long},
                               conn_state(S))
                   end)),
     ?_assertEqual({finished, <<"40 Gateway too busy\r\n">>},
                   with_client(fun(S) ->
                       respond({error_code, gateway_busy},
                               conn_state(S))
                   end)),
     % a redirect's path is turned into a URL of this server
     ?_assertEqual({finished,
                    <<"31 gemini://h:1965/dir1/index.gemini\r\n">>},
                   with_client(fun(S) ->
                       respond({redirect, <<"dir1/index.gemini">>},
                               conn_state(S))
                   end)),
     % PINS CURRENT BEHAVIOUR (smell F5), not desired: the reply of a
     % container that has gone, or an atom with no status, is a crash
     ?_assertError(function_clause, respond(any(ok), conn_state(none))),
     ?_assertError(function_clause,
                   respond({error_code, no_such_status},
                           conn_state(none)))].

respond_file_test_() ->
    {timeout, 30, fun respond_file/0}.

respond_file() ->
    Dir = filename:absname("connection_file_test_"
                           ++ integer_to_list(erlang:unique_integer([
                                                              positive]))),
    ok = filelib:ensure_path(Dir),
    File = list_to_binary(filename:join(Dir, "f.gmi")),
    ok = file:write_file(File, <<"# hello\n">>),
    try
        ?assertEqual({finished, <<"20 text/gemini\r\n# hello\n">>},
                     with_client(fun(S) ->
                         respond({file, <<"text/gemini">>, File},
                                 conn_state(S))
                     end)),
        % PINS CURRENT BEHAVIOUR, not desired: a file that cannot be read
        % is answered with a bare success header and an empty body, as the
        % result of the transport's sendfile/2 is ignored
        Missing = <<File/binary, ".missing">>,
        ?assertEqual({finished, <<"20 text/gemini\r\n">>},
                     with_client(fun(S) ->
                         respond({file, <<"text/gemini">>, Missing},
                                 conn_state(S))
                     end))
    after
        file:del_dir_r(Dir)
    end.

%% A servlet's output is a complete response, parsed before being sent.
respond_servlet_output_test_() ->
    Out = fun(Bin) -> sent({servlet_output, Bin}) end,
    [?_assertEqual({finished, <<"20 text/gemini\r\nbody\r\nmore">>},
                   with_client(fun(S) ->
                       respond({servlet_output,
                                <<"20 text/gemini\r\nbody\r\nmore">>},
                               conn_state(S))
                   end)),
     ?_assertEqual(<<"31 /elsewhere\r\n">>, Out(<<"31 /elsewhere\r\n">>)),
     % no CRLF, no space, a non-number, or a status not in the protocol
     ?_assertEqual(<<"40 Gateway error\r\n">>, Out(<<"20 text/gemini">>)),
     ?_assertEqual(<<"40 Gateway error\r\n">>, Out(<<"\r\nbody">>)),
     ?_assertEqual(<<"40 Gateway error\r\n">>, Out(<<"ok text/x\r\nb">>)),
     ?_assertEqual(<<"40 Gateway error\r\n">>, Out(<<"99 text/x\r\nb">>)),
     ?_assertEqual(<<"40 Gateway error\r\n">>, Out(<<"20\r\nb">>)),
     ?_assertEqual(<<"40 Gateway error\r\n">>, Out(<<>>))].

%% A request that has not arrived whole is buffered, whatever comes next.
handle_request_buffering_test_() ->
    Req = fun(Buffer, Payload) ->
              {R, _} = with_client(fun(S) ->
                           handle_request(Payload,
                                          (conn_state(S))#state{
                                            buffer = Buffer})
                       end),
              R
          end,
    [?_assertEqual({<<"gemini://h/x">>, none},
                   Req(<<>>, <<"gemini://h/x">>)),
     ?_assertEqual({<<"gemini://h/x\r">>, none},
                   Req(<<"gemini://h/">>, <<"x\r">>)),
     % a CRLF split across two packets is still found
     ?_assertEqual({<<>>, none}, Req(<<"gemini://h/x\r">>, <<"\n">>)),
     ?_assertEqual({<<>>, {error_code, request_too_long}},
                   Req(<<>>, binary:copy(<<"a">>, 4001))),
     ?_assertEqual({binary:copy(<<"a">>, 4000), none},
                   Req(<<>>, binary:copy(<<"a">>, 4000))),
     % a long line that is complete is judged by the request parser
     ?_assertEqual({<<>>, {error_code, request_too_long}},
                   Req(<<>>, <<(binary:copy(<<"a">>, 1030))/binary,
                               "\r\n">>))].

%% A complete request line is routed, and what the servlet module says
%% comes back as the response.
handle_request_routing_test_() ->
    Req = fun(Line, Script) ->
              {R, _} = with_client(fun(S) ->
                           handle_request(Line,
                                          conn_state(S, no_proc, Script))
                       end),
              R
          end,
    Imm = fun(Resp) -> fun(request) -> {immediate, Resp} end end,
    [?_assertEqual({<<>>, {success, <<"text/gemini">>, <<"yo">>}},
                   Req(?REQLINE,
                       Imm({success, <<"text/gemini">>, <<"yo">>}))),
     ?_assertEqual({<<>>, {error_code, file_not_found}},
                   Req(?REQLINE, Imm({error_code, file_not_found}))),
     ?_assertEqual({<<>>, none}, Req(?REQLINE, Imm(none))),
     ?_assertMatch({<<>>, {init_servlet, _}},
                   Req(?REQLINE, fun(request) -> defer;
                                 (serve) -> {gateway_finished, none}
                              end)),
     % what comes after the CRLF is not part of the request
     ?_assertEqual({<<>>, none},
                   Req(<<"gemini://h/x\r\nmore">>, Imm(none))),
     ?_assertEqual({<<>>, {error_code, request_not_parsed}},
                   Req(<<"x:y z\r\n">>, Imm(none))),
     ?_assertEqual({<<>>, {error_code, host_unrecognised}},
                   Req(<<"gemini://other/x\r\n">>, Imm(none))),
     ?_assertEqual({<<>>, {error_code, port_unrecognised}},
                   Req(<<"gemini://h:1/x\r\n">>, Imm(none))),
     ?_assertEqual({<<>>, {error_code, unrecognised_protocol}},
                   Req(<<"ftp://h/x\r\n">>, Imm(none))),
     ?_assertEqual({<<>>, {error_code, proxy_refused}},
                   Req(<<"https://h/x\r\n">>, Imm(none))),
     ?_assertEqual({<<>>, {error_code, bad_filename}},
                   Req(<<"gemini://h/a..b\r\n">>, Imm(none)))].

%% Once a servlet has been started, further client data goes to it.
handle_request_servlet_test_() ->
    [?_assertEqual({<<>>, {success, <<"text/plain">>, <<"ab">>}},
                   with_servlet_data(
                     fun(P) -> {success, <<"text/plain">>, P} end,
                     <<"a">>, <<"b">>)),
     ?_assertEqual({<<>>, none},
                   with_servlet_data(fun(_) -> none end, <<>>, <<"b">>)),
     % PINS CURRENT BEHAVIOUR (smell F5), not desired: a servlet that has
     % already finished makes the "response" the bare atom 'ok'
     ?_assertEqual({<<>>, ok},
                   isolated(fun() ->
                       {R, _} = with_client(fun(S) ->
                           St = conn_state(S),
                           any(handle_request(
                                 <<"x">>,
                                 St#state{servlet_proc =
                                              {proc, dead_pid()}}))
                       end),
                       R
                   end)),
     % a servlet that fails on client data is an internal error
     ?_assertEqual({<<>>, {error_code, internal_server_error}},
                   with_servlet_data(fun(_) -> {raise, boom} end,
                                     <<>>, <<"b">>))].

%% Start a servlet, then give the connection the buffer Buffer and the
%% payload Payload, as it arrives.
with_servlet_data(Handler, Buffer, Payload) ->
    {R, _} = with_client(fun(S) ->
        {_Pid, _Worker, State} = start_servlet(S, Handler),
        handle_request(Payload, State#state{buffer = Buffer})
    end),
    R.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_info/2

%% Run a message through handle_info/2 with no servlet; return the
%% callback's result with the state elided, and what the client got.
info(Msg) ->
    info(Msg, no_proc).

info(Msg, Proc) ->
    with_client(fun(S) ->
        State = (conn_state(S))#state{servlet_proc = Proc},
        case handle_info(Msg, State) of
            {stop, Why, _} -> {stop, Why};
            {noreply, _} -> noreply
        end
    end).

handle_info_simple_test_() ->
    [?_assertEqual({{stop, normal}, <<>>}, info({tcp_closed, sock})),
     ?_assertEqual({{stop, normal}, <<>>}, info({tcp_error, sock, why})),
     ?_assertEqual({{stop, normal}, <<>>}, info({ssl_closed, closed})),
     ?_assertEqual({{stop, normal}, <<>>}, info(finished)),
     ?_assertEqual({{stop, normal}, <<>>}, info(whatever))].

%% The 30 second timer: the client is told which of the two it was, and
%% a finished message is queued for the process.
handle_info_timeout_test_() ->
    [?_assertEqual({{stop, normal}, <<"40 Timeout\r\n">>}, info(timeout)),
     ?_assertEqual({{stop, normal}, <<"59 Request timeout\r\n">>},
                   info(timeout, {proc, self()})),
     ?_assertEqual({true, <<"40 Timeout\r\n">>},
                   with_client(fun(S) ->
         {stop, normal, _} = handle_info(timeout, conn_state(S)),
         receive finished -> true after 1000 -> false end
     end))].

%% The container's exit, as the connection sees it: the servlet is the
%% process in servlet_proc, anything else is an abend.
handle_info_exit_test_() ->
    P = self(),
    Ex = fun(Pid, Reason) ->
             info({'EXIT', Pid, Reason}, {proc, P})
         end,
    [?_assertEqual({noreply, <<"40 Internal server error\r\n">>},
                   Ex(P, {shutdown, {gateway_init_error, P, why}})),
     ?_assertEqual({noreply, <<"51 File not found\r\n">>},
                   Ex(P, {shutdown, {gateway_complete, P,
                                     {error_code, file_not_found}}})),
     ?_assertEqual({noreply, <<"20 text/gemini\r\nhi">>},
                   Ex(P, {shutdown, {gateway_complete, P,
                                     {success, <<"text/gemini">>,
                                      <<"hi">>}}})),
     ?_assertEqual({noreply, <<>>},
                   Ex(P, {shutdown, {gateway_complete, P, none}})),
     % from a process other than the servlet, or for a different one, or
     % any other reason: an internal error
     ?_assertEqual({noreply, <<"40 Internal server error\r\n">>},
                   Ex(self(), crashed)),
     ?_assertEqual({noreply, <<"40 Internal server error\r\n">>},
                   Ex(dead_pid(), normal)),
     ?_assertEqual({noreply, <<"40 Internal server error\r\n">>},
                   Ex(dead_pid(), {shutdown, {gateway_complete, P,
                                              {error_code,
                                               file_not_found}}})),
     ?_assertEqual({noreply, <<"40 Internal server error\r\n">>},
                   info({'EXIT', P, {shutdown, {gateway_init_error, P,
                                                why}}}, no_proc))].

%% Data from the client (messages {ssl, Socket, Data} on the real thing).
handle_info_data_test_() ->
    Data = fun(Payload, Script) ->
               with_client(fun(S) ->
                   State = conn_state(S, no_proc, Script),
                   case handle_info({ssl, S, Payload}, State) of
                       {noreply, N} ->
                           {noreply, N#state.buffer, N#state.servlet_proc};
                       {stop, Why, _} -> {stop, Why}
                   end
               end)
           end,
    Imm = fun(Resp) -> fun(request) -> {immediate, Resp} end end,
    [% a whole request with an immediate answer: sent, and finished
     ?_assertEqual({{stop, normal}, <<"20 text/gemini\r\nhi">>},
                   Data(?REQLINE, Imm({success, <<"text/gemini">>,
                                    <<"hi">>}))),
     ?_assertEqual({{stop, normal}, <<"51 File not found\r\n">>},
                   Data(?REQLINE, Imm({error_code, file_not_found}))),
     ?_assertEqual({{stop, normal}, <<"59 Request not parsed\r\n">>},
                   Data(<<"x:y z\r\n">>, Imm(none))),
     % part of a request is buffered
     ?_assertEqual({{noreply, <<"gemini://h/">>, no_proc}, <<>>},
                   Data(<<"gemini://h/">>, Imm(none))),
     % an answer of 'none' to a complete request keeps the connection
     ?_assertEqual({{noreply, <<>>, no_proc}, <<>>},
                   Data(?REQLINE, Imm(none))),
     % a deferred request: the servlet is now expected
     ?_assertMatch({{noreply, <<>>, {proc, _}}, <<>>},
                   Data(?REQLINE, fun(request) -> defer;
                                  (serve) -> {gateway_finished, none}
                               end)),
     % an error thrown by a servlet is a 40, and finishes the connection
     ?_assertEqual({{stop, normal}, <<"40 Internal server error\r\n">>},
                   Data(?REQLINE, fun(request) -> throw(oops) end)),
     % a socket that has gone away: nothing is sent, the process stops
     ?_assertEqual({{stop, normal}, <<>>},
                   with_client(fun(S) ->
                       State = conn_state(S, no_proc,
                                          Imm({success, <<"t">>, <<"x">>})),
                       ok = gen_tcp:close(S),
                       {stop, Why, _} = handle_info({ssl, S, ?REQLINE},
                                                    State),
                       {stop, Why}
                   end)),
     % PINS CURRENT BEHAVIOUR (smell F3), not desired: only throws are
     % caught around the request pipeline, so an error raised by a
     % servlet's request/4 escapes handle_info/2 (and kills the process)
     ?_assertEqual({error, boom},
                   begin
                       {R, _} = with_client(fun(S) ->
                           State = conn_state(S, no_proc,
                                    fun(request) -> error(boom) end),
                           try handle_info({ssl, S, ?REQLINE}, State)
                           catch error:Reason -> {error, Reason}
                           end
                       end),
                       R
                   end)].

%% Client data that arrives while a servlet runs goes to the servlet.
handle_info_servlet_data_test_() ->
    [?_assertEqual({{noreply, <<>>}, <<>>},
                   with_client(fun(S) ->
                       {_, _, State} =
                           start_servlet(S, fun(_) -> none end),
                       {noreply, N} = handle_info({ssl, S, <<"x">>},
                                                  State),
                       {noreply, N#state.buffer}
                   end)),
     ?_assertEqual({{stop, normal}, <<"20 text/plain\r\nab">>},
                   with_client(fun(S) ->
                       {_, _, State} =
                           start_servlet(
                             S, fun(P) ->
                                    {success, <<"text/plain">>, P}
                                end),
                       {stop, Why, _} =
                           handle_info({ssl, S, <<"ab">>}, State),
                       {stop, Why}
                   end))].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call/3 (the servlet's report), terminate/2, servlet_result/2

%% Report a servlet result as the container does; the process stops
%% trapping exits, a finished message is queued, and the client is sent
%% the response.
report(Result) ->
    with_client(fun(S) ->
        process_flag(trap_exit, true),
        {reply, ok, _} = handle_call({servlet_result, Result}, from,
                                     conn_state(S)),
        {trap_exit, TE} = process_info(self(), trap_exit),
        Finished = receive finished -> true after 1000 -> false end,
        {TE, Finished}
    end).

handle_call_test_() ->
    St = blank_state(),
    [?_assertEqual({{false, true}, <<"51 File not found\r\n">>},
                   report({servlet_failed, file_not_found})),
     ?_assertEqual({{false, true}, <<"40 Gateway error\r\n">>},
                   report({servlet_failed, cgi_exec_error})),
     ?_assertEqual({{false, true}, <<"20 text/gemini\r\nhi">>},
                   report({servlet_complete, <<"20 text/gemini\r\nhi">>})),
     ?_assertEqual({{false, true}, <<"40 Gateway error\r\n">>},
                   report({servlet_complete, <<"garbage">>})),
     % PINS CURRENT BEHAVIOUR (smell F5), not desired: a gateway error
     % with no status (the container passes any atom on) makes the
     % connection process crash rather than answer
     ?_assertEqual({error, function_clause},
                   begin
                       {R, _} = with_client(fun(S) ->
                           try handle_call({servlet_result,
                                            {servlet_failed, unimplemented}},
                                           from, conn_state(S))
                           catch error:Reason -> {error, Reason}
                           end
                       end),
                       R
                   end),
     ?_assertEqual({reply, ok, St}, handle_call(anything, from, St)),
     ?_assertEqual({noreply, St}, handle_cast(anything, St)),
     ?_assertEqual({ok, St}, code_change(old, St, extra))].

servlet_result_dead_connection_test() ->
    ?assertEqual(ok, servlet_result(dead_pid(),
                                    {servlet_failed, file_not_found})).

%% Closing the session cancels a running servlet, and is harmless without.
terminate_test_() ->
    [?_assertEqual(ok, terminate(normal, blank_state())),
     ?_assertEqual(ok, terminate(normal,
                                 (blank_state())#state{
                                   servlet_proc = {proc, dead_pid()}})),
     ?_assertEqual({ok, cancelled},
                   begin
                       {R, _} = with_client(fun(S) ->
                           {_Pid, Worker, State} =
                               start_servlet(S, fun(_) -> none end),
                           Ok = terminate(normal, State),
                           Got = receive
                                     {worker_cancelled, Worker} ->
                                         cancelled
                                 after 5000 -> not_cancelled
                                 end,
                           {Ok, Got}
                       end),
                       R
                   end)].
