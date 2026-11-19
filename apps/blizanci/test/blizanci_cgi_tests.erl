%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_cgi_tests).

-include_lib("eunit/include/eunit.hrl").
-include("blizanci_types.hrl").
-include("blizanci_cgi.hrl").

%% The records and types of the header are not all used here.
-compile(nowarn_unused_type).

-import(blizanci_cgi,
        [cgi_environment/1, handle_info/2, make_environment/1,
         preserved_env/0, serve/4, truncate_log/1]).
-import(blizanci_test_support, [test_request/1]).

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
