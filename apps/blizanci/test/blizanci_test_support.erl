%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

%% Helpers shared by the tests in this directory.

-module(blizanci_test_support).

-moduledoc false.

-include("blizanci_types.hrl").

-export([tmp_path/1, test_request/1, isolated/1]).
-export([live_up/3, live_down/0]).
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


%% Start the live configuration as blizanci_sup would, linked to the
%% caller, first stopping any that is running: the table owner, then the
%% servlets' process (which starts the declared servlets' pools).
-spec live_up(Port, Servlets, Sites) -> Result
              when Port     :: integer(),
                   Servlets :: [module()],
                   Sites    :: [site()],
                   Result   :: 'ok'.
live_up(Port, Servlets, Sites) ->
    live_down(),
    {ok, _} = blizanci_conf_serv:start_link(#{port => Port,
                                              servlets => Servlets,
                                              sites => Sites}),
    {ok, _} = blizanci_servlets:start_link(),
    ok.


%% Stop the live configuration processes and the servlet pools they may
%% have started, if they are running: the servlets' process (stopping
%% the pools it knows of), the table owner (and with it the named
%% table), and any blizanci_cgi pool left over.
-spec live_down() -> ok.
live_down() ->
    [ stop_registered(Name) || Name <- [blizanci_servlets,
                                        blizanci_conf_serv] ],
    case whereis(blizanci_cgi) of
        undefined -> ok;
        _ -> blizanci_cgi:stop()
    end.

stop_registered(Name) ->
    case whereis(Name) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end.
