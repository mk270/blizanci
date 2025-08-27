%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

%% @doc
%% This module deals with the OS environment.
%% That is, it deals with conventional UNIX
%% environment variables as opposed to Erlang's per-application environments.
%% OS environment variables are implicated in two ways:
%%
%% 1) we want to sanitise a proplist of environment variables such that
%%    it's appropriate for creating a new UNIX subprocess via the erlexec
%%    port.
%%
%% 2) the Erlang VM is likely to be running in an OS process with its
%%    own environment variables, and we want to unset most of those to
%%    avoid polluting the environment of the subprocesses created in 1)
%% @end

-module(blizanci_osenv).
-include("blizanci_types.hrl").

-export([sanitise/1, unset_os_env_except/1]).


%% @doc Sanitise the system environment variables.
%%
%% erlexec expects environment variables to be strings, not binaries;
%% the function tries to massage the various reasonable input values
%% (such as integers and binaries) into acceptable strings. It crashes
%% on unreasonable input.
%% @end
-spec sanitise(Env) -> Result
              when Env    :: [{string(), term()}],
                   Result :: env_list().
sanitise(Env) ->
    [ sanitise_kv(K, V) || {K, V} <- Env, is_list(K) ].


-spec sanitise_kv(Key, Value) -> Result
              when Key    :: string(),
                   Value  :: term(),
                   Result :: {string(), string()}.

sanitise_kv(Key, <<"">>) when is_list(Key) ->
    {Key, <<"">>}; % exec:run apparently objects to null-strings as lists

sanitise_kv(Key, Value) when is_binary(Value) and is_list(Key) ->
    {Key, binary_to_list(Value)};

sanitise_kv(Key, Value) when is_integer(Value) and is_list(Key) ->
    {Key, integer_to_list(Value)};

sanitise_kv(Key, undefined) when is_list(Key) ->
    {Key, "undefined"};

sanitise_kv(Key, Value) when is_list(Key) and is_list(Value) ->
    {Key, Value}.


%% @doc Unset all environment variables but those in the Exceptions whitelist.
%% @end
-spec unset_os_env_except(Exceptions) -> Result
              when Exceptions :: [string()],
                   Result     :: ok.

unset_os_env_except(Exceptions) ->
    Keys = defined_os_env_vars(),
    [ os:unsetenv(Key) ||
        Key <- Keys,
        not lists:member(Key, Exceptions) ],
    ok.


-spec defined_os_env_vars() -> Result
              when Result :: [string()].
defined_os_env_vars() ->
    [ Head || [Head|_] <- [ string:split(Env, "=") || Env <- os:getenv() ] ].
