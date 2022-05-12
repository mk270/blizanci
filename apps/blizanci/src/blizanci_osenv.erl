%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_osenv).
-include("blizanci_types.hrl").

% This module deals with the OS environment, that is, conventional UNIX
% environment variables as opposed to Erlang's per-application environments.
% OS environment variables are implicated in two ways:
%
% 1) we want to sanitise a proplist of environment variables such that
%    it's appropriate for creating a new UNIX subprocess via the erlexec
%    port.
%
% 2) the Erlang VM is likely to be running in an OS process with its
%    own environment variables, and we want to unset most of those to
%    avoid polluting the environment of the subprocesses created in 1)

-export([sanitise/1, unset_os_env_except/1]).


% erlexec expects environment variables to be strings, not binaries; we try
% to massage the various reasonable input values (such as integers and
% binaries) into acceptable strings.
-spec sanitise([{string(), term()}]) -> env_list().
sanitise(Env) ->
    [ sanitise_kv(K, V) || {K, V} <- Env ].


-spec sanitise_kv(string(), term()) -> {string(), string()}.
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


% Attempt to unset all environment variables except those in a whitelist.
-spec unset_os_env_except([string()]) -> ok.
unset_os_env_except(Exceptions) ->
    Keys = defined_os_env_vars(),
    [ os:unsetenv(Key) ||
        Key <- Keys,
        not lists:member(Key, Exceptions) ],
    ok.

-spec defined_os_env_vars() -> [string()].
defined_os_env_vars() ->
    [ Head || [Head|_] <- [ string:split(Env, "=") || Env <- os:getenv() ] ].
