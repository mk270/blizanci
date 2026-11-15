%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_cgi).

-moduledoc """
A servlet that runs CGI scripts, in a ppool queue, via erlexec.

See the manual chapter [CGI](cgi.md).
""".
-behaviour(blizanci_servlet).

-include("blizanci_types.hrl").
-include_lib("eunit/include/eunit.hrl").
-import(blizanci_test_support, [test_request/1]).

-behaviour(gen_server).

%% API
-export([serve/4, start/0, stop/0, cancel/1, request/4, default_options/0,
        validate_options/1, handle_client_data/2]).
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(cgi_proc, {pid, os_pid, buffer, script}).
% The per-request inputs from which the CGI environment is built.
-record(cgi_req, {cmd       :: binary() | string(),
                  path      :: binary(),
                  matches   :: path_matches(),
                  req       :: request_details(),
                  host_port :: {binary(), integer()},
                  prefix    :: string()}).
-type cgi_req() :: #cgi_req{}.
-record(worker_state, {parent, parent_ref, cgi_status}).
-type worker_state() :: #worker_state{}.
-type options() :: #{
                     cgiroot := binary(),
                     cgiprefix := string()
                    }.

% The most CGI scripts running at once (the worker pool size); further
% requests are refused as too busy (gateway_busy), not queued.
-define(MAX_CGI, 5).
-define(QUEUE, ?MODULE).

% The most output accepted from one script, in bytes: past this the
% script is killed and the request fails.
-define(MAX_CGI_OUTPUT_BYTES, (10 * 1024 * 1024)). % 10 MiB

% The most bytes of a script's stderr put in a single log entry.
-define(MAX_STDERR_LOG_BYTES, 512).

% The signal number used to kill a script that must stop.
-define(SIGKILL, 9).

% A note re environment variables.
%
% The CGI child's environment is built from scratch on each request
% (erlexec's {env, [clear | Env]} option -- see enqueue_cgi/2), not
% inherited from the Erlang VM's own OS environment.  The variables
% that are preserved (?ALLOWED_ENV, below) and those set per request
% (see make_environment/1) are listed in the manual chapter 'CGI'
% (doc/manual/cgi.gmi), as are the variables the Gemini App Guide
% suggests.

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
  when is_binary(CGIRoot), is_list(CGIPrefix) ->
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
-doc false.
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
-doc false.
-spec start() -> Result
              when Result :: 'ok' | {'error', term()}.
start() ->
    blizanci_pool:start(?QUEUE, ?MAX_CGI, ?MODULE).


% The counterpart to start/0; called at application shutdown so the
% CGI ppool doesn't leak across a stop/start cycle in the same VM.
-spec stop() -> Result
              when Result :: ok.
stop() ->
    blizanci_pool:stop(?QUEUE).


% Called by the container to cancel a job, e.g., if the TCP connection has
% been closed by the remote end.
-spec cancel(Pid) -> Result
              when Pid    :: pid(),
                   Result :: ok.
cancel(Pid) ->
    blizanci_pool:cancel(Pid, cgi_quit).


% Called by blizanci_servlet_container, in the connection process
%
-spec request(Path, Req, ServerConfig, RouteOpts) -> Result
              when Path         :: any(),
                   Req          :: request_details(),
                   ServerConfig :: server_config(),
                   RouteOpts    :: any(),
                   Result       :: {'immediate', gemini_response()} | 'defer'.

request(_Path, _Req, _ServerConfig, _RouteOpts) ->
    defer.


-doc """
Validate that a proper CGI request has been received, and if so, submit
a job to the queue. Called by the servlet.
""".
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
    % The request path arrives percent-decoded (so it may hold a space or
    % shell metacharacters). How erlexec treats such characters has not
    % been audited, so until then the script's path may contain only
    % letters, digits and ". _ - /" (see issues.md), and it is handed over
    % as a string, as before.
    Confined = blizanci_path:resolve_request(Matches, CGIRoot, #{}),
    case {cgi_alphabet(Path), Confined} of
        {true, {ok, Cmd}} ->
            serve_cgi_bytes(#cgi_req{cmd=Cmd,
                                     path=Path,
                                     matches=Matches,
                                     req=Req,
                                     host_port={Hostname, Port},
                                     prefix=CGIPrefix});
        _ -> {gateway_error, file_not_found}
    end.


% The confined script name is bytes; it becomes the string the VM would
% have given for that name (so a configured root that is not ASCII still
% works), or the request is refused.
-spec serve_cgi_bytes(CgiReq) -> Result
              when CgiReq :: cgi_req(),
                   Result :: gateway_result().

serve_cgi_bytes(#cgi_req{cmd=Cmd} = CgiReq) ->
    case unicode:characters_to_list(Cmd, file:native_name_encoding()) of
        CmdString when is_list(CmdString) ->
            serve_cgi(CgiReq#cgi_req{cmd=CmdString});
        _ ->
            {gateway_error, file_not_found}
    end.


-spec cgi_alphabet(Path) -> Result
              when Path   :: binary(),
                   Result :: boolean().
cgi_alphabet(Path) ->
    re:run(Path, <<"^[A-Za-z0-9._/-]*$">>, [{capture, none}]) =:= match.


-spec serve_cgi(CgiReq) -> Result
              when CgiReq :: cgi_req(),
                   Result :: gateway_result().

serve_cgi(#cgi_req{cmd=Cmd} = CgiReq) ->
    case filelib:is_file(Cmd) of
        false -> {gateway_error, file_not_found};
        true ->
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
            case cgi_environment(CgiReq) of
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
-doc false.
init({Parent, {CmdLine, Options}}) ->
    process_flag(trap_exit, true),
    % The parent (the servlet container) is not linked to us, so watch it:
    % if it dies without sending cgi_quit, the script is stopped (see
    % handle_info/2).
    ParentRef = erlang:monitor(process, Parent),
    Result = exec:run(CmdLine, Options),
    {ok, Pid, OsPid} = Result,
    CGI_Status = #cgi_proc{pid=Pid,
                           os_pid=OsPid,
                           buffer= <<>>,
                           script=CmdLine},
    State = #worker_state{parent=Parent,
                          parent_ref=ParentRef,
                          cgi_status=CGI_Status},
    {ok, State}.


-doc false.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


-doc false.
handle_cast(cgi_quit, State) ->
    stop_script(State),
    {stop, normal, State};

handle_cast(_Request, State) ->
    {noreply, State}.

% The parent has died: nobody is left to receive the result, so stop the
% script and ourselves, as for cgi_quit. This clause must come before the
% next, which matches erlexec's 'DOWN' messages (whose second element is
% the OS pid, not a monitor reference).
handle_info({'DOWN', Ref, process, _Pid, _Reason},
            State=#worker_state{parent_ref=Ref}) ->
    stop_script(State),
    {stop, normal, State};

handle_info({'DOWN', OsPid, process, Pid, Reason}, State) ->
    handle_down(OsPid, Pid, Reason, State);

handle_info({stdout, OsPid, Msg}, State) ->
    handle_stdout(OsPid, Msg, State);

handle_info({stderr, OsPid, Msg}, State) ->
    handle_stderr(OsPid, Msg, State);
handle_info(_Info, State) ->
    {noreply, State}.


-doc false.
terminate(normal, _State) ->
    ok;
terminate(Reason, _State) ->
    logger:info("CGI queue worker ~p terminating: [[~p]]", [self(), Reason]),
    ok.


-doc false.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

% Kill the script, if one is still running.
-spec stop_script(State) -> Result
              when State  :: worker_state(),
                   Result :: ok.
stop_script(#worker_state{cgi_status=#cgi_proc{os_pid=OsPid}}) ->
    _ = exec:kill(OsPid, ?SIGKILL),
    ok;
stop_script(_State) ->
    ok.


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

% The script's stderr is only logged (at debug level); it never reaches
% the client. Long chunks are truncated, and ~p escapes control bytes.
-spec handle_stderr(OsPid, Msg, State) -> Result
              when OsPid  :: integer(),
                   Msg    :: binary(),
                   State  :: worker_state(),
                   Result :: {noreply, worker_state()}.

handle_stderr(OsPid, Msg, State=#worker_state{cgi_status=#cgi_proc{
                                                  os_pid=OsPid,
                                                  script=Script}}) ->
    logger:debug("cgi ~p (os pid ~p) stderr: ~p",
                 [Script, OsPid, truncate_log(Msg)]),
    {noreply, State};
handle_stderr(_OsPid, _Msg, State) ->
    {noreply, State}.


-spec truncate_log(Msg) -> Result
              when Msg    :: binary(),
                   Result :: binary() | {binary(), {truncated, integer()}}.
truncate_log(Msg) when byte_size(Msg) > ?MAX_STDERR_LOG_BYTES ->
    <<Head:?MAX_STDERR_LOG_BYTES/binary, Rest/binary>> = Msg,
    {Head, {truncated, byte_size(Rest)}};
truncate_log(Msg) ->
    Msg.


-spec handle_stdout2(Msg, State) -> Result
              when Msg    :: binary(),
                   State  :: worker_state(),
                   Result :: {noreply, worker_state()}
                           | {stop, normal, worker_state()}.

handle_stdout2(Msg, State) ->
    CGI = State#worker_state.cgi_status,
    Buffer = CGI#cgi_proc.buffer,
    NewBuffer = <<Buffer/binary, Msg/binary>>,
    case byte_size(NewBuffer) > ?MAX_CGI_OUTPUT_BYTES of
        true ->
            stop_script(State),
            cgi_finished({gateway_error, cgi_output_too_large}, State);
        false ->
            NewCGI = CGI#cgi_proc{buffer=NewBuffer},
            NewState = State#worker_state{cgi_status=NewCGI},
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
% are determined by make_environment/1
-spec cgi_environment(CgiReq) -> Result
              when CgiReq :: cgi_req(),
                   Result :: {ok, env_list()}
                           | {error, atom()}.

cgi_environment(CgiReq) ->
    Env = make_environment(CgiReq),
    case Env of
        {ok, Env0} ->
            {ok, blizanci_osenv:sanitise(preserved_env() ++ Env0)};
        {error, Reason} -> {error, Reason}
    end.


% The subset of the Erlang VM's own OS environment (per ?ALLOWED_ENV)
% that gets passed through to every CGI child, alongside the
% per-request variables built by make_environment/1. Only variables
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
-spec make_environment(CgiReq) -> Result
              when CgiReq :: cgi_req(),
                   Result :: {ok, [{string(), term()}]}
                           | {error, atom()}.

make_environment(#cgi_req{cmd=Bin, path=Path, matches=Matches, req=Req,
                          host_port={Hostname, Port},
                          prefix=CGIPrefix}) ->
    #{ query := QueryString, client_cert := Cert } = Req,
    PathInfo = maps:get(<<"PATH_INFO">>, Matches, <<>>),
    ScriptName = CGIPrefix ++ binary_to_list(Path),

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
        {ok, #{ common_name := undefined }} ->
            % a certificate with no Common Name: REMOTE_USER stays unset
            {ok, KVPs};
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


stderr_test_() ->
    CGI = #cgi_proc{pid=self(), os_pid=42, buffer= <<"x">>,
                    script=["/s"]},
    S = #worker_state{parent=self(), cgi_status=CGI},
    Big = binary:copy(<<"e">>, ?MAX_STDERR_LOG_BYTES + 5),
    [?_assertEqual({noreply, S}, handle_info({stderr, 42, <<"oops\n">>}, S)),
     ?_assertEqual({noreply, S}, handle_info({stderr, 7, <<"stale">>}, S)),
     ?_assertEqual({noreply, S}, handle_info({stderr, 42, Big}, S)),
     ?_assertEqual(<<"abc">>, truncate_log(<<"abc">>)),
     ?_assertEqual({binary:copy(<<"e">>, ?MAX_STDERR_LOG_BYTES),
                    {truncated, 5}}, truncate_log(Big))].


%% The parent's monitor message stops the worker; erlexec's own 'DOWN'
%% message (second element an OS pid) is not mistaken for it.
parent_down_test_() ->
    Ref = make_ref(),
    CGI = #cgi_proc{pid=self(), os_pid=0, buffer= <<>>, script=["/s"]},
    S = #worker_state{parent=self(), parent_ref=Ref, cgi_status=CGI},
    % (with a live script, stop_script/1 calls erlexec: see orphan_test_)
    Gone = S#worker_state{cgi_status=no_proc},
    [?_assertMatch({stop, normal, Gone},
                   handle_info({'DOWN', Ref, process, self(), killed},
                               Gone)),
     ?_assertMatch({noreply, S},
                   handle_info({'DOWN', 99, process, self(), normal}, S))].


%% End to end: killing the container brutally while the script runs must
%% not leave the script running. Skipped if erlexec cannot run here.
%% Fixture: Base/cgi/sleeper writes its pid to Base/pid, then sleeps.
orphan_test_() ->
    {setup, fun orphan_setup/0, fun orphan_teardown/1, fun orphan_tests/1}.

orphan_setup() ->
    Unique = integer_to_list(erlang:unique_integer([positive])),
    Base = filename:absname("cgi_orphan_test_" ++ Unique),
    Dir = filename:join(Base, "cgi"),
    ok = filelib:ensure_path(Dir),
    Script = filename:join(Dir, "sleeper"),
    ok = file:write_file(Script,
                         ["#!/bin/sh\necho $$ > ",
                          filename:join(Base, "pid"),
                          "\nexec sleep 30\n"]),
    ok = file:change_mode(Script, 8#755),
    Up = case application:ensure_all_started([erlexec, ppool]) of
             {ok, _} ->
                 case catch blizanci_cgi:start() of
                     ok -> true;
                     _ -> false
                 end;
             _ -> false
         end,
    {Base, Up}.

orphan_teardown({Base, Up}) ->
    case Up of
        true -> blizanci_cgi:stop();
        false -> ok
    end,
    file:del_dir_r(Base).

orphan_tests({Base, Up}) ->
    {timeout, 30,
     fun() ->
         case Up of
             false -> ok;
             true -> orphan_run(Base)
         end
     end}.

orphan_run(Base) ->
    Dir = filename:join(Base, "cgi"),
    Cfg = #server_config{hostname = <<"h">>, port = 1965,
                         routing = []},
    Opts = #{ cgiroot => list_to_binary(Dir), cgiprefix => "/cgi-bin/" },
    Me = self(),
    Container = spawn(fun() ->
        Me ! {started, serve(#{ <<"PATH">> => <<"sleeper">> },
                             test_request(#{}), Cfg, Opts)},
        receive stop -> ok end
    end),
    case receive {started, R} -> R after 5000 -> timeout end of
        {gateway_started, Worker} ->
            OsPid = wait_for_pid(filename:join(Base, "pid"), 50),
            true = os_alive(OsPid),
            exit(Container, kill),
            ?assert(wait_until(fun() -> not os_alive(OsPid) end, 50)),
            ?assert(wait_until(fun() -> not is_process_alive(Worker) end,
                               50));
        _ ->
            % erlexec cannot run here (or the pool is busy): skip
            exit(Container, kill)
    end.

wait_for_pid(_File, 0) -> error(no_pid_file);
wait_for_pid(File, N) ->
    case file:read_file(File) of
        {ok, Bin} ->
            case string:to_integer(string:trim(binary_to_list(Bin))) of
                {Int, _} when is_integer(Int) -> Int;
                _ -> timer:sleep(100), wait_for_pid(File, N - 1)
            end;
        _ -> timer:sleep(100), wait_for_pid(File, N - 1)
    end.

os_alive(OsPid) ->
    os:cmd("kill -0 " ++ integer_to_list(OsPid) ++ " 2>/dev/null; echo $?")
        =:= "0\n".

wait_until(_Fun, 0) -> false;
wait_until(Fun, N) ->
    case Fun() of
        true -> true;
        false -> timer:sleep(100), wait_until(Fun, N - 1)
    end.


%% Hidden files are never run as CGI scripts: any path segment beginning
%% with a dot is refused, ".well-known" included (nothing is served from
%% there by CGI). The refusal comes before anything is queued, so these
%% tests need no worker pool.
%% Fixture, under a fresh temporary directory:
%%   Base/cgi/{.hidden,.well-known/x,sub/.y}
%%   Base/cgi/<scripts with awkward names: "a b", "a;b", "a&b", "a|b",
%%             "a$b", "a`b", "a'b", "a\"b", "a\\b", "a%b">>
hidden_scripts_test_() ->
    {setup, fun hidden_setup/0, fun hidden_teardown/1, fun hidden_tests/1}.

hidden_setup() ->
    Base = blizanci_test_support:tmp_path("cgi_hidden_test"),
    Dir = filename:join(Base, "cgi"),
    [ begin
          Path = filename:join(Dir, Rel),
          ok = filelib:ensure_path(filename:dirname(Path)),
          ok = file:write_file(Path, <<"#!/bin/sh\n">>)
      end || Rel <- [".hidden", ".well-known/x", "sub/.y", "a b", "a;b",
                     "a&b", "a|b", "a$b", "a`b", "a'b", "a\"b", "a\\b",
                     "a%b"] ],
    Base.

hidden_teardown(Base) ->
    file:del_dir_r(Base).

hidden_tests(Base) ->
    Dir = filename:join(Base, "cgi"),
    Cfg = #server_config{hostname = <<"h">>, port = 1965,
                         routing = []},
    Opts = #{ cgiroot => list_to_binary(Dir), cgiprefix => "/cgi-bin/" },
    Serve = fun(Path) ->
                serve(#{ <<"PATH">> => Path }, test_request(#{}), Cfg, Opts)
            end,
    [% CGI script paths are restricted to letters, digits and "._-/" for
     % now (see serve/4): spaces and shell metacharacters, from decoded
     % escapes, are refused, as is anything non-ASCII
     ?_assertEqual({gateway_error, file_not_found}, Serve(<<"a b">>)),
     ?_assertEqual({gateway_error, file_not_found}, Serve(<<"a;b">>)),
     ?_assertEqual({gateway_error, file_not_found}, Serve(<<"a&b">>)),
     ?_assertEqual({gateway_error, file_not_found}, Serve(<<"a|b">>)),
     ?_assertEqual({gateway_error, file_not_found}, Serve(<<"a$b">>)),
     ?_assertEqual({gateway_error, file_not_found}, Serve(<<"a`b">>)),
     ?_assertEqual({gateway_error, file_not_found}, Serve(<<"a'b">>)),
     ?_assertEqual({gateway_error, file_not_found}, Serve(<<"a\"b">>)),
     ?_assertEqual({gateway_error, file_not_found}, Serve(<<"a\\b">>)),
     ?_assertEqual({gateway_error, file_not_found}, Serve(<<"a%b">>)),
     ?_assertEqual({gateway_error, file_not_found}, Serve(<<195, 169>>)),
     ?_assertEqual({gateway_error, file_not_found}, Serve(<<"ok", 233>>)),
     ?_assertEqual({gateway_error, file_not_found}, Serve(<<".hidden">>)),
     ?_assertEqual({gateway_error, file_not_found}, Serve(<<"sub/.y">>)),
     ?_assertEqual({gateway_error, file_not_found},
                   Serve(<<".well-known/x">>))].


%% The environment built for a request: the per-request variables, then
%% the whole lot passed through blizanci_osenv:sanitise/1. No script is
%% run.
test_cgi_req(ReqExtra) ->
    #cgi_req{cmd = <<"/srv/cgi/hello">>, path = <<"hello">>,
             matches = #{ <<"PATH_INFO">> => <<"/extra">> },
             req = test_request(maps:merge(#{ query => <<"a=1">> },
                                           ReqExtra)),
             host_port = {<<"example.org">>, 1965},
             prefix = "/cgi-bin/"}.

make_environment_test_() ->
    #{peer := Peer, no_cn := NoCN, no_cn_both := NoCNBoth} =
        blizanci_x509:test_pki(),
    {ok, Env} = make_environment(test_cgi_req(#{})),
    [?_assertEqual([{"PATH_TRANSLATED", <<"/srv/cgi/hello">>},
                    {"PATH_INFO", <<"/extra">>},
                    {"QUERY_STRING", <<"a=1">>},
                    {"SCRIPT_NAME", "/cgi-bin/hello"},
                    {"SERVER_NAME", <<"example.org">>},
                    {"SERVER_PORT", 1965},
                    {"SERVER_PROTOCOL", "GEMINI"}], Env),
     % no client certificate: no REMOTE_USER
     ?_assertEqual(false, lists:keymember("REMOTE_USER", 1, Env)),
     % a certificate with a Common Name sets REMOTE_USER; one without
     % leaves it unset
     ?_assertEqual({"REMOTE_USER", <<"alice">>},
                   lists:keyfind("REMOTE_USER", 1,
                                 element(2, make_environment(
                                   test_cgi_req(#{ client_cert =>
                                                   {ok, Peer} }))))),
     ?_assertEqual(false,
                   lists:keymember("REMOTE_USER", 1,
                                   element(2, make_environment(
                                     test_cgi_req(#{ client_cert =>
                                                     {ok, NoCN} }))))),
     ?_assertEqual(false,
                   lists:keymember("REMOTE_USER", 1,
                                   element(2, make_environment(
                                     test_cgi_req(#{ client_cert =>
                                                     {ok, NoCNBoth} }))))),
     % an undecodable certificate is reported, not dropped
     ?_assertEqual({error, cert_not_parsed},
                   make_environment(
                     test_cgi_req(#{ client_cert => {ok, <<"junk">>} }))),
     % no PATH_INFO match: empty
     ?_assertMatch({ok, [_, {"PATH_INFO", <<>>} | _]},
                   make_environment(
                     (test_cgi_req(#{}))#cgi_req{matches = #{}}))].

cgi_environment_test_() ->
    {ok, Env} = cgi_environment(test_cgi_req(#{})),
    Names = [ K || {K, _} <- Env ],
    Preserved = [ N || {N, _} <- preserved_env() ],
    Request = ["PATH_TRANSLATED", "PATH_INFO", "QUERY_STRING",
               "SCRIPT_NAME", "SERVER_NAME", "SERVER_PORT",
               "SERVER_PROTOCOL"],
    [% every name and value is a string, apart from the empty binary
     ?_assert(lists:all(fun({K, V}) ->
                            io_lib:printable_unicode_list(K)
                            andalso (is_list(V) orelse V =:= <<>>)
                        end, Env)),
     % binary and integer values have been converted
     ?_assertEqual({"PATH_TRANSLATED", "/srv/cgi/hello"},
                   lists:keyfind("PATH_TRANSLATED", 1, Env)),
     ?_assertEqual({"SERVER_PORT", "1965"},
                   lists:keyfind("SERVER_PORT", 1, Env)),
     ?_assertEqual({"SERVER_NAME", "example.org"},
                   lists:keyfind("SERVER_NAME", 1, Env)),
     % the inherited variables come first, are only allowed ones, and
     % nothing else is added
     ?_assertEqual(Preserved ++ Request, Names),
     ?_assertEqual([], Preserved -- ?ALLOWED_ENV),
     % an error from make_environment is passed on
     ?_assertEqual({error, cert_not_parsed},
                   cgi_environment(
                     test_cgi_req(#{ client_cert => {ok, <<"junk">>} }))),
     % an empty query comes through as the empty binary
     ?_assertEqual({"QUERY_STRING", <<>>},
                   lists:keyfind("QUERY_STRING", 1,
                     element(2, cgi_environment(
                                  test_cgi_req(#{ query => <<>> })))))].


%% End to end, through the real servlet code and its worker pool: real
%% scripts are run and the outcome the worker reports to its container
%% (here the test process) is checked. Skipped if erlexec cannot run
%% here. Fixture, under a fresh temporary directory:
%%   Base/cgi/{hello,env,nostatus,fail,flood}
run_setup() ->
    Base = blizanci_test_support:tmp_path("cgi_run_test"),
    Dir = filename:join(Base, "cgi"),
    ok = filelib:ensure_path(Dir),
    Scripts =
        [{"hello",
          "printf '20 text/gemini\\r\\nhello, world\\n'\n"},
         {"env",
          "printf '20 text/plain\\r\\n%s|%s|%s|%s|%s|%s\\n' "
          "\"$SERVER_NAME\" \"$SERVER_PORT\" \"$PATH_INFO\" "
          "\"$QUERY_STRING\" \"$SCRIPT_NAME\" \"$REMOTE_USER\"\n"},
         {"nostatus", "printf 'just a body\\n'\n"},
         {"fail", "printf '20 text/gemini\\r\\npartial\\n'\nexit 3\n"},
         {"flood", "exec dd if=/dev/zero bs=1048576 count=11 "
                   "2>/dev/null\n"}],
    [ begin
          Path = filename:join(Dir, Name),
          ok = file:write_file(Path, ["#!/bin/sh\n", Body]),
          ok = file:change_mode(Path, 8#755)
      end || {Name, Body} <- Scripts ],
    Started = case application:ensure_all_started([erlexec, ppool]) of
                  {ok, Apps} -> Apps;
                  _ -> failed
              end,
    Up = Started =/= failed andalso catch blizanci_cgi:start() =:= ok,
    {Base, Started, Up}.

run_teardown({Base, Started, Up}) ->
    case Up of
        true -> blizanci_cgi:stop();
        false -> ok
    end,
    undefined = whereis(blizanci_cgi),
    ok = file:del_dir_r(Base),
    false = filelib:is_file(Base),
    case Started of
        failed -> ok;
        Apps -> [ ok = application:stop(A) || A <- lists:reverse(Apps) ]
    end,
    ok.

run_cgi_test_() ->
    {setup, fun run_setup/0, fun run_teardown/1, fun run_cgi_tests/1}.

run_cgi_tests({_Base, _Started, false}) ->
    [];
run_cgi_tests({Base, _Started, true}) ->
    T = fun(Fun) -> {timeout, 60, fun() -> Fun(Base) end} end,
    [T(fun run_hello/1), T(fun run_env/1), T(fun run_remote_user/1),
     T(fun run_nostatus/1), T(fun run_fail/1), T(fun run_flood/1)].

% Run the script Name as the container (this process) would, and return
% what the worker reports; then wait for the worker to be gone.
run_script(Base, Name, Matches, ReqExtra) ->
    Dir = filename:join(Base, "cgi"),
    Cfg = #server_config{hostname = <<"example.org">>, port = 1965,
                         routing = []},
    Opts = #{ cgiroot => list_to_binary(Dir), cgiprefix => "/cgi-bin/" },
    M = maps:merge(#{ <<"PATH">> => list_to_binary(Name) }, Matches),
    {gateway_started, Worker} =
        serve(M, test_request(ReqExtra), Cfg, Opts),
    Ref = erlang:monitor(process, Worker),
    Got = receive
              {'$gen_cast', {gateway_result, R}} -> R
          after 20000 -> no_result
          end,
    receive
        {'DOWN', Ref, process, Worker, _} -> ok
    after 20000 -> error(worker_still_running)
    end,
    receive {'$gen_cast', Extra} -> error({second_report, Extra})
    after 0 -> ok
    end,
    Got.

run_hello(Base) ->
    ?assertEqual({gateway_output, <<"20 text/gemini\r\nhello, world\n">>},
                 run_script(Base, "hello", #{}, #{})).

run_env(Base) ->
    Got = run_script(Base, "env", #{ <<"PATH_INFO">> => <<"/x/y">> },
                     #{ query => <<"q=1">> }),
    ?assertEqual({gateway_output,
                  <<"20 text/plain\r\n"
                    "example.org|1965|/x/y|q=1|/cgi-bin/env|\n">>}, Got).

run_remote_user(Base) ->
    #{peer := Peer} = blizanci_x509:test_pki(),
    Got = run_script(Base, "env", #{}, #{ client_cert => {ok, Peer} }),
    ?assertEqual({gateway_output,
                  <<"20 text/plain\r\n"
                    "example.org|1965||"
                    "|/cgi-bin/env|alice\n">>}, Got).

% The worker does not validate the output: a body without a status line
% is reported as the output, verbatim.
run_nostatus(Base) ->
    ?assertEqual({gateway_output, <<"just a body\n">>},
                 run_script(Base, "nostatus", #{}, #{})).

% Non-zero exit: the partial output is discarded.
run_fail(Base) ->
    ?assertEqual({gateway_error, cgi_exec_error},
                 run_script(Base, "fail", #{}, #{})).

% 11 MiB of output is over the 10 MiB cap.
run_flood(Base) ->
    ?assertEqual({gateway_error, cgi_output_too_large},
                 run_script(Base, "flood", #{}, #{})).
