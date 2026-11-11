%% Helpers shared by the tests that live in the source modules.
%%
%% It is in src/, not test/, because the test code that calls it is
%% compiled into the source modules, and dialyzer analyses only src/:
%% from test/ every call would be reported as an unknown function.

-module(blizanci_test_support).

-export([tmp_path/1, test_request/1, isolated/1]).
-export([live_down/0]).
-export([mime_up/0, mime_down/1]).

%% A fresh, absolute path (a string) for a fixture, in the current
%% directory. Nothing is created.
-spec tmp_path(Prefix :: string()) -> string().
tmp_path(Prefix) ->
    Unique = integer_to_list(erlang:unique_integer([positive])),
    filename:absname(Prefix ++ "_" ++ Unique).


%% A complete request, for tests that care about only some of its parts.
-spec test_request(Extra :: map()) -> map().
test_request(Extra) ->
    maps:merge(#{ client_cert => {error, no_peercert},
                  query => <<>>,
                  rest_of_input => <<>>,
                  scheme => <<"gemini">>,
                  host => <<"h">>,
                  port => 1965,
                  path => <<>> }, Extra).


%% Run Fun in a process of its own and return its result (or re-raise
%% its failure here), so that what it does to the process (setting
%% trap_exit, linking) does not outlive it.
-spec isolated(fun(() -> T)) -> T.
isolated(Fun) ->
    {Pid, Ref} = spawn_monitor(
                   fun() ->
                       exit({done, try {ok, Fun()}
                                   catch C:R:S -> {caught, C, R, S}
                                   end})
                   end),
    receive
        {'DOWN', Ref, process, Pid, {done, {ok, V}}} -> V;
        {'DOWN', Ref, process, Pid, {done, {caught, C, R, S}}} ->
            erlang:raise(C, R, S);
        {'DOWN', Ref, process, Pid, Why} -> erlang:error({isolated, Why})
    end.


%% Make sure mime_lookup is running; the result tells mime_down/1
%% whether this call started it.
-spec mime_up() -> already_running | started.
mime_up() ->
    case lists:keymember(mime_lookup, 1, application:which_applications()) of
        true -> already_running;
        false -> {ok, _} = application:ensure_all_started(mime_lookup),
                 started
    end.

-spec mime_down(already_running | started) -> ok.
mime_down(already_running) -> ok;
mime_down(started) -> ok = application:stop(mime_lookup).


%% Stop the live configuration process and the servlet pools it may have
%% started, if they are running: the process (and with it the named
%% table), and the blizanci_cgi pool.
-spec live_down() -> ok.
live_down() ->
    case whereis(blizanci_conf_serv) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end,
    case whereis(blizanci_cgi) of
        undefined -> ok;
        _ -> blizanci_cgi:stop()
    end.
