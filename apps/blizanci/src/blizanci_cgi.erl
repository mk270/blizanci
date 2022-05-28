%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_cgi).
-behaviour(blizanci_servlet).

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
% This message is sent by calling blizanci_servlet_container:gateway_exit/2.


-include("blizanci_types.hrl").

-behaviour(gen_server).
-include("gen_server.hrl").

%% API
-export([serve/3, start/0, cancel/1, request/3]).
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-record(worker_state, {parent, cgi_status}).
-type worker_state() :: #worker_state{}.
-type options() :: #{
                     hostname := binary(),
                     port := integer(),
                     cgiroot := string(),
                     cgiprefix := string()
                    }.

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
%% @doc
%% @hidden
%% @end
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).


% Must be called by the application on initialisation; establishes the
% ppool queue for which most of this module is a set of callbacks.
-spec start() -> ok.
%% @doc
%% @hidden
%% @end
start() ->
    blizanci_osenv:unset_os_env_except(?ALLOWED_ENV),
    case ppool:start_pool(?QUEUE, ?MAX_CGI, {?MODULE, start_link, []}) of
        {error, {already_started, _Pid}} -> ok;
        {ok, _Pid} -> ok
    end.


% Called by the servlet to cancel a job, e.g., if the TCP connection has
% been closed by the remote end.
-spec cancel(pid()) -> ok.
cancel(Pid) ->
    case is_process_alive(Pid) of
        true -> gen_server:cast(Pid, cgi_quit);
        _ -> ok
    end.


% Called by the servlet
%
-spec request(any(), request_details(), any()) ->
                     {'immediate', gemini_response()} |
                     'defer'.
request(_Path, _Req, _Config) ->
    defer.

% Called by the servlet
%
% Validate that a proper CGI request has been received, and if so, submit
% a job to the queue
-spec serve(path_matches(), request_details(), options()) -> gateway_result().
serve(Matches, Req, Options) ->
    #{ <<"PATH">> := Path } = Matches,
    #{ cgiprefix := CGIPrefix,
       cgiroot   := CGIRoot } = Options,
    PathElements  = [CGIRoot, binary_to_list(Path)],
    {ok, Cmd}     = blizanci_path:fix_path(filename:join(PathElements)),

    % this is a belt-and-braces check; URLs with ".." in them are currently
    % forbidden anyway
    true = blizanci_path:path_under_root(Cmd, CGIRoot),

    case filelib:is_file(Cmd) of
        false -> {gateway_error, file_not_found};
        true ->
            #{ query := QueryString, client_cert := Cert } = Req,

            % Args represents a UNIX commandline comprising the path of
            % the executable with zero arguments.
            Args = [Cmd],
            Env = cgi_environment(CGIPrefix, Path, Cmd, Options,
                                  QueryString, Cert),

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
%% @doc
%% @hidden
%% @end
init({Parent, {CmdLine, Options}}) ->
    process_flag(trap_exit, true),
    Result = exec:run(CmdLine, Options),
    {ok, Pid, OsPid} = Result,
    {ok, #worker_state{parent=Parent, cgi_status={Pid, OsPid, <<>>}}}.


%% @doc
%% @hidden
%% @end
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @doc
%% @hidden
%% @end
handle_cast(cgi_quit, State=#worker_state{cgi_status=CGI_Status}) ->
    {_Pid, OsPid, _Buffer} = CGI_Status,
    exec:kill(OsPid, 9),
    {stop, normal, State};

handle_cast(_Request, State) ->
    {noreply, State}.

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

%% @doc
%% @hidden
%% @end
terminate(normal, _State) ->
    ok;
terminate(Reason, _State) ->
    lager:info("CGI queue worker ~p terminating: [[~p]]", [self(), Reason]),
    ok.

%% @doc
%% @hidden
%% @end
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().
%% @doc
%% @hidden
%% @end
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec cgi_finished(gateway_result(), worker_state()) -> term().
cgi_finished(Reason, State=#worker_state{parent=Parent}) ->
    blizanci_servlet_container:gateway_exit(Parent, Reason),
    {stop, normal, State}.


cgi_environment(CGIPrefix, Path, Bin, Options, QueryString, Cert) ->
    Env0 = make_environment(CGIPrefix, Path, Bin, Options, QueryString, Cert),
    blizanci_osenv:sanitise(Env0).

make_environment(CGIPrefix, Path, Bin, Options, QueryString, Cert) ->
    ScriptName = CGIPrefix ++ binary_to_list(Path),
    #{ hostname := Hostname,
       port     := Port } = Options,

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

