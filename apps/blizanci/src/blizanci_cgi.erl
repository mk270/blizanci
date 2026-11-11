%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

%% @doc
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
%
% The servlet has an analogous protocol for communicating back to the
% connection handler.
%
%% @end

-module(blizanci_cgi).
-behaviour(blizanci_servlet).

-include("blizanci_types.hrl").
-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).

%% API
-export([serve/4, start/0, stop/0, cancel/1, request/4, default_options/0,
        validate_options/1, handle_client_data/2]).
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(cgi_proc, {pid, os_pid, buffer}).
-record(worker_state, {parent, cgi_status}).
-type worker_state() :: #worker_state{}.
-type options() :: #{
                     cgiroot := string(),
                     cgiprefix := string()
                    }.

-define(MAX_CGI, 5).
-define(QUEUE, ?MODULE).
-define(MAX_CGI_OUTPUT_BYTES, 10 * 1024 * 1024). % 10 MiB

% A note re environment variables.
%
% The CGI child's environment is built from scratch on each request
% (erlexec's {env, [clear | Env]} option -- see enqueue_cgi/2), rather
% than inherited from the Erlang VM's own OS environment, so nothing
% other than what's listed below ever reaches the CGI script.
%
% The preserved variables, constant across all requests (copied from
% the Erlang VM's own environment at request time, if set -- see
% preserved_env/0), are:
%
%   $HOME
%   $USER
%   $PATH
%   $LOGNAME
%   $SHELL
%
% On a per-request basis, the following variables are set:
%
%   $PATH_TRANSLATED
%   $PATH_INFO
%   $QUERY_STRING
%   $SCRIPT_NAME
%   $SERVER_NAME
%   $SERVER_PORT
%   $SERVER_PROTOCOL
%   $REMOTE_USER  (this latter one is only set if a client cert is proferred)
%
% No other environment variables get passed through to the CGI runner.
% In particular, variables inherited at startup from the UNIX environment
% are not passed through unless on the "preserved" list above.
%
%
% In the Gemini App Guide, it is suggested that the following variables
% be made available:
%
%   $SCRIPT_NAME
%   $PATH_INFO
%   $QUERY_STRING
%   $REMOTE_ADDR
%   $AUTH_TYPE
%   $TLS_CLIENT_HASH
%   $TLS_CLIENT_SUBJECT
%   $TLS_CLIENT_ISSUER
%
% Note that these above are not exactly the variables currently supported
%
% See gemini://geminiprotocol.net/docs/app-guide.gmi

-define(ALLOWED_ENV,
        ["HOME",
         "USER",
         "PATH",
         "LOGNAME",
         "SHELL"]).

%%%===================================================================
%%% API
%%%===================================================================

-spec default_options() -> Result
              when Result :: map().
default_options() ->
    #{ cgiprefix => "/cgi-bin/" }.


-spec validate_options(Opts) -> Result
              when Opts   :: map(),
                   Result :: 'ok' | {'error', term()}.
validate_options(#{ cgiroot := CGIRoot, cgiprefix := CGIPrefix })
  when is_list(CGIRoot), is_list(CGIPrefix) ->
    case filelib:is_dir(filename:absname(CGIRoot)) of
        true ->
            case valid_prefix(CGIPrefix) of
                true -> ok;
                false -> {error, {invalid_cgiprefix, CGIPrefix}}
            end;
        false -> {error, {cgiroot_not_a_directory, CGIRoot}}
    end;
validate_options(Opts) ->
    {error, {invalid_options, Opts}}.


-spec valid_prefix(Prefix) -> Result
              when Prefix :: string(),
                   Result :: boolean().
valid_prefix(Prefix) ->
    lists:prefix("/", Prefix) andalso lists:suffix("/", Prefix).

% Called by the ppool queue manager; passes the arguments straight through
%% @doc
%% @hidden
%% @end
-spec start_link(Args) -> Result
              when Args   :: term(),
                   Result :: {ok, Pid :: pid()} |
                             {error, Error :: {already_started, pid()}} |
                             {error, Error :: term()} |
                             ignore.

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).


% Must be called by the application on initialisation; establishes the
% ppool queue for which most of this module is a set of callbacks.
%% @doc
%% @hidden
%% @end
-spec start() -> Result
              when Result :: ok.
start() ->
    blizanci_pool:start(?QUEUE, ?MAX_CGI, ?MODULE).


% The counterpart to start/0; called at application shutdown so the
% CGI ppool doesn't leak across a stop/start cycle in the same VM.
-spec stop() -> Result
              when Result :: ok.
stop() ->
    blizanci_pool:stop(?QUEUE).


% Called by the servlet to cancel a job, e.g., if the TCP connection has
% been closed by the remote end.
-spec cancel(Pid) -> Result
              when Pid    :: pid(),
                   Result :: ok.
cancel(Pid) ->
    blizanci_pool:cancel(Pid, cgi_quit).


% Called by the servlet
%
-spec request(Path, Req, ServerConfig, RouteOpts) -> Result
              when Path         :: any(),
                   Req          :: request_details(),
                   ServerConfig :: server_config(),
                   RouteOpts    :: any(),
                   Result       :: {'immediate', gemini_response()} | 'defer'.

request(_Path, _Req, _ServerConfig, _RouteOpts) ->
    defer.


%% @doc
%% Validate that a proper CGI request has been received, and if so, submit
%% a job to the queue. Called by the servlet.
%% @end
-spec serve(Matches, Req, ServerConfig, RouteOpts) -> Result
              when Matches      :: path_matches(),
                   Req          :: request_details(),
                   ServerConfig :: server_config(),
                   RouteOpts    :: options(),
                   Result       :: gateway_result().

serve(Matches, Req, #server_config{hostname=Hostname, port=Port}, RouteOpts) ->
    #{ <<"PATH">> := Path } = Matches,
    #{ cgiprefix := CGIPrefix,
       cgiroot   := CGIRoot } = RouteOpts,

    % this is a belt-and-braces check; URLs with ".." in them are
    % currently forbidden anyway. blizanci_path:confine/2 fails closed
    % (returning file_not_found rather than crashing) if the resolved
    % path can't be canonicalised, or resolves outside CGIRoot.
    %
    % Until the CGI boundary (erlexec) has been shown to handle names as
    % bytes, the script's name must be printable ASCII, and it is handed
    % over as a string, as before.
    case {printable_ascii(Path), blizanci_path:confine(Path, CGIRoot)} of
        {true, {ok, Cmd}} -> serve_cgi_bytes(Cmd, Path, Matches, Req,
                                              {Hostname, Port}, CGIPrefix);
        _ -> {gateway_error, file_not_found}
    end.


% The confined script name is bytes; it becomes the string the VM would
% have given for that name (so a configured root that is not ASCII still
% works), or the request is refused.
serve_cgi_bytes(Cmd, Path, Matches, Req, HostPort, CGIPrefix) ->
    case unicode:characters_to_list(Cmd, file:native_name_encoding()) of
        CmdString when is_list(CmdString) ->
            serve_cgi(CmdString, Path, Matches, Req, HostPort, CGIPrefix);
        _ ->
            {gateway_error, file_not_found}
    end.


-spec printable_ascii(Path) -> Result
              when Path   :: binary(),
                   Result :: boolean().
printable_ascii(Path) ->
    re:run(Path, <<"^[\\x20-\\x7e]*$">>, [{capture, none}]) =:= match.


-spec serve_cgi(Cmd, Path, Matches, Req, HostPort, CGIPrefix) -> Result
              when Cmd       :: string(),
                   Path      :: binary(),
                   Matches   :: path_matches(),
                   Req       :: request_details(),
                   HostPort  :: {binary(), integer()},
                   CGIPrefix :: string(),
                   Result    :: gateway_result().

serve_cgi(Cmd, Path, Matches, Req, HostPort, CGIPrefix) ->
    case filelib:is_file(Cmd) of
        false -> {gateway_error, file_not_found};
        true ->
            #{ query := QueryString, client_cert := Cert } = Req,
            PathInfo = maps:get(<<"PATH_INFO">>, Matches, <<>>),

            % Args represents a UNIX commandline comprising the path of
            % the executable with zero arguments.
            Args = [Cmd],

            % A client cert, if offered, need not have been required by
            % this route's auth policy (which may be 'public'); it's
            % opportunistically decoded here just to populate REMOTE_USER.
            % If that decoding fails, fail the whole request rather than
            % silently launching the CGI script without REMOTE_USER: a
            % certificate we can't understand is exactly the kind of
            % input we don't want to guess about.
            case cgi_environment(CGIPrefix, Path, Cmd,
                                 HostPort, QueryString,
                                 Cert, PathInfo) of
                {ok, Env} -> enqueue_cgi(Args, Env);
                {error, Reason} -> {gateway_error, Reason}
            end
    end.


% Actually place the CGI job on the queue
%
% The creation of the CGI subprocess is handled in init/1, not here.
-spec enqueue_cgi(Args, Env) -> Result
              when Args   :: [string()],
                   Env    :: env_list(),
                   Result :: gateway_result().

enqueue_cgi(Args, Env) ->
    Options = [monitor, {group, 0}, kill_group,
               {env, [clear | Env]}, stdout, stderr],
    blizanci_pool:enqueue(?QUEUE, {Args, Options}).


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
    CGI_Status = #cgi_proc{pid=Pid,
                           os_pid=OsPid,
                           buffer= <<>>},
    State = #worker_state{parent=Parent,
                          cgi_status=CGI_Status},
    {ok, State}.


%% @doc
%% @hidden
%% @end
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% @doc
%% @hidden
%% @end
handle_cast(cgi_quit, State=#worker_state{cgi_status=CGI}) ->
    exec:kill(CGI#cgi_proc.os_pid, 9),
    {stop, normal, State};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'DOWN', OsPid, process, Pid, Reason}, State) ->
    handle_down(OsPid, Pid, Reason, State);

handle_info({stdout, OsPid, Msg}, State) ->
    handle_stdout(OsPid, Msg, State);
handle_info(_Info, State) ->
    {noreply, State}.


%% @doc
%% @hidden
%% @end
terminate(normal, _State) ->
    ok;
terminate(Reason, _State) ->
    logger:info("CGI queue worker ~p terminating: [[~p]]", [self(), Reason]),
    ok.


%% @doc
%% @hidden
%% @end
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

% Deal with the asynchronous stdout from the CGI process.
-spec handle_stdout(OsPid, Msg, State) -> Result
              when OsPid  :: integer(),
                   Msg    :: binary(),
                   State  :: worker_state(),
                   Result :: {noreply, worker_state()}
                           | {stop, normal, worker_state()}.

handle_stdout(OsPid, Msg, State) ->
    CGI = State#worker_state.cgi_status,
    case CGI#cgi_proc.os_pid of
        OsPid ->
            handle_stdout2(Msg, State);
        Expected ->
            % Stale message from a prior process; ignore rather than crash.
            logger:warning("handle_stdout: unexpected ospid ~p (expected ~p)",
                           [OsPid, Expected]),
            {noreply, State}
    end.

-spec handle_stdout2(Msg, State) -> Result
              when Msg    :: binary(),
                   State  :: worker_state(),
                   Result :: {noreply, worker_state()}
                           | {stop, normal, worker_state()}.

handle_stdout2(Msg, State) ->
    CGI = State#worker_state.cgi_status,
    Buffer = CGI#cgi_proc.buffer,
    NewBuffer = erlang:iolist_to_binary([Buffer, Msg]),
    case byte_size(NewBuffer) > ?MAX_CGI_OUTPUT_BYTES of
        true ->
            exec:kill(CGI#cgi_proc.os_pid, 9),
            cgi_finished({gateway_error, cgi_output_too_large}, State);
        false ->
            NewState = State#worker_state{cgi_status=CGI#cgi_proc{buffer=NewBuffer}},
            {noreply, NewState}
    end.


% Despite the name, this covers the usual case when the CGI subprocess
% terminates (i.e., both the normal and error termination possibilities).
-spec handle_down(OsPid, Pid, Reason, State) -> Result
              when OsPid  :: integer(),
                   Pid    :: pid(),
                   Reason :: term(),
                   State  :: worker_state(),
                   Result :: {noreply, worker_state()}
                           | {stop, normal, worker_state()}.

handle_down(OsPid, Pid, Reason, State) ->
    CGI = State#worker_state.cgi_status,
    case {CGI#cgi_proc.pid, CGI#cgi_proc.os_pid} of
        {Pid, OsPid} ->
            handle_down2(Reason, CGI, State);
        _ ->
            % Stale DOWN from a prior process; ignore rather than crash.
            logger:warning("handle_down: unexpected pid/ospid ~p/~p",
                           [Pid, OsPid]),
            {noreply, State}
    end.

-spec handle_down2(Reason, CGI, State) -> Result
              when Reason :: term(),
                   CGI    :: #cgi_proc{},
                   State  :: worker_state(),
                   Result :: {noreply, worker_state()}
                           | {stop, normal, worker_state()}.

handle_down2(Reason, CGI, State) ->
    Buffer = CGI#cgi_proc.buffer,
    %ExitStatus = exec:status(Reason),
    NewState = State#worker_state{cgi_status=no_proc},
    case Reason of
        normal ->
            cgi_finished({gateway_output, Buffer}, NewState);
        {exit_status, St} ->
            RV = exec:status(St),
            logger:info("cgi process ended with non-zero status: ~p", [RV]),
            cgi_finished({gateway_error, cgi_exec_error}, NewState);
        St ->
            logger:info("cgi process terminated anomalously: ~p", [St]),
            cgi_finished({gateway_error, cgi_exec_error}, NewState)
    end.


% Notify the *rest* of the system that we're finished
-spec cgi_finished(Reason, State) -> Result
              when Reason :: gateway_result(),
                   State  :: worker_state(),
                   Result :: term().

cgi_finished(Reason, State=#worker_state{parent=Parent}) ->
    blizanci_servlet_container:gateway_exit(Parent, Reason),
    {stop, normal, State}.


% Returns the "sanitised" version of the per-request environment variables
%
% This basically ensures that both the key and value are strings, and
% values like 'undefined' are handled. The actual details of the variables
% are determined by make_environment/6
-spec cgi_environment(CGIPrefix, Path, Bin, HostPort,
                      QueryString, Cert, PathInfo) -> Result
              when CGIPrefix   :: string(),
                   Path        :: binary(),
                   Bin         :: string(),
                   HostPort    :: {binary(), integer()},
                   QueryString :: binary(),
                   Cert        :: peer_cert(),
                   PathInfo    :: binary(),
                   Result      :: {ok, env_list()}
                                | {error, atom()}.

cgi_environment(CGIPrefix, Path, Bin, HostPort, QueryString, Cert, PathInfo) ->
    Env = make_environment(CGIPrefix, Path, Bin, HostPort,
                           QueryString, Cert, PathInfo),
    case Env of
        {ok, Env0} ->
            {ok, blizanci_osenv:sanitise(preserved_env() ++ Env0)};
        {error, Reason} -> {error, Reason}
    end.


% The subset of the Erlang VM's own OS environment (per ?ALLOWED_ENV)
% that gets passed through to every CGI child, alongside the
% per-request variables built by make_environment/7. Only variables
% actually set are included -- os:getenv/1 returns 'false' for one
% that isn't, and that's not a value to hand to the CGI child.
-spec preserved_env() -> Result
              when Result :: [{string(), string()}].
preserved_env() ->
    [ {Name, Value} || Name <- ?ALLOWED_ENV,
                        Value <- [os:getenv(Name)],
                        Value =/= false ].


% Construct the process environment for the CGI program, based on the
% incoming Gemini request.
%
% QUERY_STRING and PATH_INFO are taken verbatim from client-controlled
% input, and the request line is only required to be valid UTF-8 -- which
% permits an embedded NUL byte (U+0000 is one valid UTF-8 octet). That
% NUL therefore reaches erlexec's {env, Env} list unmodified. This was
% checked against erlexec's env handling (CmdOptions::init_cenv,
% exec_impl.cpp): each entry is a length-tracked std::string, converted
% to a NUL-terminated C string only at the point of building execve's
% envp array, so an embedded NUL truncates that one variable's value --
% it cannot smuggle in an additional, independent environment variable.
-spec make_environment(CGIPrefix, Path, Bin, HostPort,
                       QueryString, Cert, PathInfo) -> Result
              when CGIPrefix   :: string(),
                   Path        :: binary(),
                   Bin         :: string(),
                   HostPort    :: {binary(), integer()},
                   QueryString :: binary(),
                   Cert        :: peer_cert(),
                   PathInfo    :: binary(),
                   Result      :: {ok, [{string(), term()}]}
                                | {error, atom()}.

make_environment(CGIPrefix, Path, Bin, HostPort, QueryString, Cert, PathInfo) ->
    ScriptName = CGIPrefix ++ binary_to_list(Path),
    {Hostname, Port} = HostPort,

    KVPs = [
     {"PATH_TRANSLATED", Bin},
     {"PATH_INFO", PathInfo},
     {"QUERY_STRING", QueryString},
     {"SCRIPT_NAME", ScriptName},
     {"SERVER_NAME", Hostname},
     {"SERVER_PORT", Port},
     {"SERVER_PROTOCOL", "GEMINI"}
    ],

    % A client cert here may not have been required by this route's auth
    % policy (which may be 'public'); it's decoded opportunistically just
    % to populate REMOTE_USER. See blizanci_x509:peercert_cn/1 for why a
    % cert we can't understand is reported rather than silently dropped.
    case blizanci_x509:peercert_cn(Cert) of
        {ok, #{ common_name := CN }} ->
            {ok, KVPs ++ [{"REMOTE_USER", CN}]};
        error ->
            {ok, KVPs};
        {error, Reason} ->
            {error, Reason}
    end.


% No-op for this module
-spec handle_client_data(Pid, Binary) -> Result
              when Pid    :: pid(),
                   Binary :: binary(),
                   Result :: gemini_response().
handle_client_data(_, _) -> none.


%% Hidden files are never run as CGI scripts: any path segment beginning
%% with a dot is refused, ".well-known" included (nothing is served from
%% there by CGI). The refusal comes before anything is queued, so these
%% tests need no worker pool.
%% Fixture, under a fresh temporary directory:
%%   Base/cgi/{.hidden,.well-known/x,sub/.y}
hidden_scripts_test_() ->
    {setup, fun hidden_setup/0, fun hidden_teardown/1, fun hidden_tests/1}.

hidden_setup() ->
    Unique = integer_to_list(erlang:unique_integer([positive])),
    Base = filename:absname("cgi_hidden_test_" ++ Unique),
    Dir = filename:join(Base, "cgi"),
    [ begin
          Path = filename:join(Dir, Rel),
          ok = filelib:ensure_path(filename:dirname(Path)),
          ok = file:write_file(Path, <<"#!/bin/sh\n">>)
      end || Rel <- [".hidden", ".well-known/x", "sub/.y"] ],
    Base.

hidden_teardown(Base) ->
    file:del_dir_r(Base).

hidden_tests(Base) ->
    Dir = filename:join(Base, "cgi"),
    Cfg = #server_config{hostname = <<"h">>, port = 1965,
                         routing = [], docroot = Dir},
    Opts = #{ cgiroot => Dir, cgiprefix => "/cgi-bin/" },
    Serve = fun(Path) ->
                serve(#{ <<"PATH">> => Path }, #{ query => <<>> }, Cfg, Opts)
            end,
    [% CGI script names stay ASCII-only for now (see serve/4)
     ?_assertEqual({gateway_error, file_not_found}, Serve(<<195, 169>>)),
     ?_assertEqual({gateway_error, file_not_found}, Serve(<<"ok", 233>>)),
     ?_assertEqual({gateway_error, file_not_found}, Serve(<<".hidden">>)),
     ?_assertEqual({gateway_error, file_not_found}, Serve(<<"sub/.y">>)),
     ?_assertEqual({gateway_error, file_not_found},
                   Serve(<<".well-known/x">>))].
