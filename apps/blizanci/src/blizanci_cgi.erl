%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_cgi).
-include("blizanci_types.hrl").

-behaviour(gen_server).

%% API
-export([serve/3, start/0, cancel/1]).
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(MAX_CGI, 5).
-define(QUEUE, ?MODULE).
-define(ALLOWED_ENV,
        ["HOME",
         "USER",
         "PATH",
         "LOGNAME",
         "SHELL"]).

-record(worker_state, {parent, cgi_status}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(term()) -> {ok, Pid :: pid()} |
                      {error, Error :: {already_started, pid()}} |
                      {error, Error :: term()} |
                      ignore.
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).


start() ->
    unset_os_env_except(?ALLOWED_ENV),
    case ppool:start_pool(?QUEUE, ?MAX_CGI, {?MODULE, start_link, []}) of
        {error, {already_started, _Pid}} -> ok;
        {ok, _Pid} -> ok
    end.


submit(Args) ->
    ppool:run(?QUEUE, [Args]).


cancel(Pid) ->
    case is_process_alive(Pid) of
        true -> gen_server:call(Pid, quit);
        _ -> ok
    end.


-spec serve(binary(), map(), server_config()) ->
                   {'error_code', atom()} |
                   {'init_cgi', pid()}.
serve(Path, Req, #server_config{
                    hostname=Hostname,
                    port=Port,
                    cgiroot=CGIRoot}) ->
    PathElements  = [CGIRoot, binary_to_list(Path)],
    {ok, Cmd}     = fix_path(filename:join(PathElements)),

    % this is a belt-and-braces check; URLs with ".." in them are current
    % forbidden anyway
    true = path_under_root(Cmd, CGIRoot),

    case filelib:is_file(Cmd) of
        false -> {error_code, file_not_found};
        true ->
            Args = [Cmd],
            Env = cgi_environment(Path, Cmd, Hostname, Req, Port),

            case run_cgi(Args, Env) of
                {ok, Pid} -> {init_cgi, Pid};
                noalloc -> {error_code, too_many_cgi}
            end
    end.

run_cgi(Args, Env) ->
    Options = [monitor, {env, Env}, stdout, stderr],
    submit({self(), {Args, Options}}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(Args :: term()) -> {ok, State :: term()} |
                              {ok, State :: term(), Timeout :: timeout()} |
                              {ok, State :: term(), hibernate} |
                              {stop, Reason :: term()} |
                              ignore.
init({Parent, {Args, Options}}) ->
    process_flag(trap_exit, true),
    Result = exec:run(Args, Options),
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
handle_call(quit, _From, State=#worker_state{cgi_status=CGI_Status}) ->
    {_Pid, OsPid, _Buffer} = CGI_Status,
    exec:kill(OsPid, 9),
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

-spec handle_cast(Request :: term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), NewState :: term()}.
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
            cgi_finished({cgi_output, Buffer}, NewState);
        {exit_status, St} ->
            RV = exec:status(St),
            lager:info("cgi process ended with non-zero status: ~p", [RV]),
            cgi_finished({error_code, cgi_exec_error}, NewState);
        St ->
            lager:info("cgi process terminated anomalously: ~p", [St]),
            cgi_finished({error_code, cgi_exec_error}, NewState)
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
terminate(_Reason, _State) ->
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


fix_path(S) ->
    {ok, S2} = realpath:normalise(S),
    {ok, S3} = realpath:canonicalise(S2),
    {ok, S3}.

path_under_root(S, CGIRoot) ->
    L = string:len(CGIRoot),
    Sl = string:slice(S, 0, L),
    Sl =:= CGIRoot.

cgi_environment(Path, Bin, Hostname, Req, Port) ->
    Env0 = make_environment(Path, Bin, Hostname, Req, Port),
    sanitise(Env0).

make_environment(Path, Bin, Hostname, Req, Port) ->
    ScriptName = "/cgi-bin/" ++ binary_to_list(Path),
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

sanitise(Env) ->
    [ sanitise_kv(K, V) || {K, V} <- Env ].


sanitise_kv(Key, <<"">>) ->
    {Key, <<"">>}; % exec:run apparently objects to null-strings as lists

sanitise_kv(Key, Value) when is_binary(Value) ->
    {Key, binary_to_list(Value)};

sanitise_kv(Key, Value) when is_integer(Value) ->
    {Key, integer_to_list(Value)};

sanitise_kv(Key, undefined) ->
    {Key, "undefined"};

sanitise_kv(Key, Value) ->
    {Key, Value}.


defined_os_env_vars() ->
    [ Head || [Head|_] <- [ string:split(Env, "=") || Env <- os:getenv() ] ].

unset_os_env_except(Exceptions) ->
    Keys = defined_os_env_vars(),
    [ os:unsetenv(Key) ||
        Key <- Keys,
        not lists:member(Key, Exceptions) ].


cgi_finished(Reason, State=#worker_state{parent=Parent}) ->
    Parent ! {cgi_exit, Reason},
    {stop, normal, State}.
