%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_cgi).

% This module is called by the servlet to run CGI scripts. It communicates
% with three other processes:
%
% 1) the servlet. The servlet's Pid is stored in the process state, as
%    "parent", though strictly-speaking the parent is the queue-manager
%    referred to in 2). Each blizanci_cgi process is related to a different
%    servlet process. The servlet uses code in this module to attempt to
%    submit a new job to the ppool queue, which may fail. If it succeeds,
%    then the queue-manager creates a process which calls blizanci_cgi:init/1.
%
% 2) the ppool queue-manager. Each blizanci_cgi process is managed by the
%    *same* ppool queue-manager process
%
% 3) the erlexec process. Each blizanci_cgi process tries to start a
%    external UNIX process via the erlexec application. The Pid of the
%    the erlexec manager process, and the Pid of the created UNIX process
%    are both stored in the blizanci_cgi process's state. There is one
%    erlexec manager process for each blizanci_cgi process, and the latter
%    should expect to receive a set of out-of-band info messages from the
%    former advising of subprocess outputs and termination.
%
% Once the external UNIX process has terminated, a message is sent to the
% servlet, in the form {cgi_exit, Result}, where Result may be:
%
%   {gateway_output, binary()}
% | {gateway_error, cgi_error()}
%
% This message is sent by calling blizanci_servlet:gateway_result/2.


-include("blizanci_types.hrl").

-behaviour(gen_server).

%% API
-export([serve/3, start/0, cancel/1, request/3]).
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-record(worker_state, {parent, cgi_status}).
-type worker_state() :: #worker_state{}.


-define(MAX_CGI, 5).
-define(QUEUE, ?MODULE).
-define(ALLOWED_ENV,
        ["HOME",
         "USER",
         "PATH",
         "LOGNAME",
         "SHELL"]).

%%%===================================================================
%%% API
%%%===================================================================

% Called by the ppool queue manager; passes the arguments straight through
-spec start_link(term()) -> {ok, Pid :: pid()} |
                      {error, Error :: {already_started, pid()}} |
                      {error, Error :: term()} |
                      ignore.
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).


% Must be called by the application on initialisation; establishes the
% ppool queue for which most of this module is a set of callbacks.
start() ->
    blizanci_osenv:unset_os_env_except(?ALLOWED_ENV),
    case ppool:start_pool(?QUEUE, ?MAX_CGI, {?MODULE, start_link, []}) of
        {error, {already_started, _Pid}} -> ok;
        {ok, _Pid} -> ok
    end.


% Called by the servlet to cancel a job, e.g., if the TCP connection has
% been closed by the remote end.
cancel(Pid) ->
    case is_process_alive(Pid) of
        true -> gen_server:cast(Pid, cgi_quit);
        _ -> ok
    end.


% Called by the servlet
%
-spec request(any(), any(), any()) ->
                     {'immediate', gemini_response()} |
                     'defer'.
request(_Path, _Req, _Config) ->
    defer.

% Called by the servlet
%
% Validate that a proper CGI request has been received, and if so, submit
% a job to the queue
-spec serve(binary(), map(), server_config()) -> gateway_result().
serve(Path, Req, #server_config{
                    hostname=Hostname,
                    port=Port,
                    cgiroot=CGIRoot}) ->
    CGIPrefix     = "/cgi-bin/",
    PathElements  = [CGIRoot, binary_to_list(Path)],
    {ok, Cmd}     = blizanci_path:fix_path(filename:join(PathElements)),

    % this is a belt-and-braces check; URLs with ".." in them are current
    % forbidden anyway
    true = blizanci_path:path_under_root(Cmd, CGIRoot),

    case filelib:is_file(Cmd) of
        false -> {gateway_error, file_not_found};
        true ->
            % Args represents a UNIX commandline comprising the path of
            % the executable with zero arguments.
            Args = [Cmd],
            Env = cgi_environment(CGIPrefix, Path, Cmd, Hostname, Req, Port),

            case run_cgi(Args, Env) of
                {ok, Pid} -> {gateway_started, Pid};
                noalloc -> {gateway_error, gateway_busy}
            end
    end.

-spec run_cgi([string()], env_list()) -> {'ok', pid()} | noalloc.
run_cgi(Args, Env) ->
    Options = [monitor, {env, Env}, stdout, stderr],
    ppool:run(?QUEUE, [{self(), {Args, Options}}]).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

% Called by the queue runner. The second argument to ppool:run is
% passed in as the first argument to this function.
-spec init(Args :: term()) -> {ok, State :: term()} |
                              {ok, State :: term(), Timeout :: timeout()} |
                              {ok, State :: term(), hibernate} |
                              {stop, Reason :: term()} |
                              ignore.
init({Parent, {CmdLine, Options}}) ->
    process_flag(trap_exit, true),
    Result = exec:run(CmdLine, Options),
    {ok, Pid, OsPid} = Result,
    {ok, #worker_state{parent=Parent, cgi_status={Pid, OsPid, <<>>}}}.


-spec handle_call(
        Request :: term(), From :: {pid(), term()}, State :: term()) ->
                         {reply, Reply :: term(), NewState :: term()} |
                         {reply, Reply :: term(), NewState :: term(),
                          Timeout :: timeout()} |
                         {reply, Reply :: term(), NewState :: term(),
                          hibernate} |
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), Reply :: term(),
                          NewState :: term()} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

-spec handle_cast(Request :: term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_cast(cgi_quit, State=#worker_state{cgi_status=CGI_Status}) ->
    {_Pid, OsPid, _Buffer} = CGI_Status,
    exec:kill(OsPid, 9),
    {stop, normal, State};

handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(Info :: timeout() | term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: normal | term(), NewState :: term()}.
handle_info({'DOWN', OsPid, process, Pid, Status}, State) ->
    {ExpectedPid, ExpectedOsPid, Buffer} = State#worker_state.cgi_status,
    ExpectedPid = Pid,
    ExpectedOsPid = OsPid,
    %ExitStatus = exec:status(Status),
    NewState = State#worker_state{cgi_status=no_proc},
    case Status of
        normal ->
            cgi_finished({gateway_output, Buffer}, NewState);
        {exit_status, St} ->
            RV = exec:status(St),
            lager:info("cgi process ended with non-zero status: ~p", [RV]),
            cgi_finished({gateway_error, cgi_exec_error}, NewState);
        St ->
            lager:info("cgi process terminated anomalously: ~p", [St]),
            cgi_finished({gateway_error, cgi_exec_error}, NewState)
    end;

handle_info({stdout, OsPid, Msg}, State) ->
    {ExpectedPid, ExpectedOsPid, Buffer} = State#worker_state.cgi_status,
    ExpectedOsPid = OsPid,
    NewBuffer = erlang:iolist_to_binary([Buffer, Msg]),
    NewState = State#worker_state{
                 cgi_status={ExpectedPid, ExpectedOsPid, NewBuffer}},
    {noreply, NewState};

handle_info(Info, State) ->
    lager:info("OOB msg:~p", [Info]),
    {noreply, State}.

-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(normal, _State) ->
    ok;
terminate(Reason, _State) ->
    lager:info("CGI queue worker ~p terminating: [[~p]]", [self(), Reason]),
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

-spec cgi_finished(gateway_result(), worker_state()) -> term().
cgi_finished(Reason, State=#worker_state{parent=Parent}) ->
    blizanci_servlet:gateway_exit(Parent, Reason),
    {stop, normal, State}.


cgi_environment(CGIPrefix, Path, Bin, Hostname, Req, Port) ->
    Env0 = make_environment(CGIPrefix, Path, Bin, Hostname, Req, Port),
    blizanci_osenv:sanitise(Env0).

make_environment(CGIPrefix, Path, Bin, Hostname, Req, Port) ->
    ScriptName = CGIPrefix ++ binary_to_list(Path),
    #{ query := QueryString,
       client_cert := Cert } = Req,

    KVPs = [
     {"PATH_TRANSLATED", Bin},
     {"QUERY_STRING", QueryString},
     {"SCRIPT_NAME", ScriptName},
     {"SERVER_NAME", Hostname},
     {"SERVER_PORT", Port},
     {"SERVER_PROTOCOL", "GEMINI"}
           ],

    case blizanci_x509:peercert_cn(Cert) of
        {ok, #{ common_name := CN }} ->
            KVPs ++ [{"REMOTE_USER", CN}];
        error ->
            KVPs
    end.

