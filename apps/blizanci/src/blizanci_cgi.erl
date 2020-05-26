%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_cgi).
-behaviour(gen_server).
-export([serve/2]).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).

-define(TIMEOUT_MS, 5000).
-define(SERVER, ?MODULE).

serve(Path, Docroot) ->
    lager:info("CGI request: ~p ~p", [Path, Docroot]),
    Result = gen_server:call(?SERVER, {cgi, Path, Docroot}),
    lager:info("Res: ~p", [Result]),
    {error_code, not_implemented}.


start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(State) ->
    process_flag(trap_exit, true),
    erlang:send_after(?TIMEOUT_MS, self(), timeout),
    {ok, State}.

handle_call({cgi, Path, Docroot}, _From, State) ->
    {ok, _Status} = run(Path, Docroot),
    {reply, ok, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    lager:info("Timeout: should kill launched process"),
    {stop, normal, State};
handle_info(Unrecognised, State) ->
    lager:info("Unrecognised message: ~p @~p", [Unrecognised, State]),
    {stop, normal, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) -> ok.


run(Path, Env) ->
    lager:info("Should have run: ~p ~p", [Path, Env]),
    Cmd = ["/usr/bin/env"],
    ErrorLog = "/tmp/errors",
    {ok, Status} = exec:run(Cmd, [sync,
                                      {stdout, ErrorLog, [append]},
                                      {stderr, ErrorLog, [append]}]),
    {ok, Status}.
