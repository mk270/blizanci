%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

%% @doc
%% This module deals with sanitising a proplist of OS environment
%% variables such that it's appropriate for creating a new UNIX
%% subprocess via the erlexec port.
%% @end

-module(blizanci_osenv).
-include("blizanci_types.hrl").

-export([sanitise/1]).


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


% Sanitise an individual {Key, Value} pair
%
% The only caller pretty much guarantees that 'Key' will be a string
%
% We convert:
%   binaries to strings: <<"foo">> -> "foo"
%   integers to strings: 1234 -> "1234"
%   the atom 'undefined' -> "undefined"
%
% We leave strings as strings
%
% Other inputs for 'Value' will lead to a nomatch
-spec sanitise_kv(Key, Value) -> Result
              when Key    :: string(),
                   Value  :: term(),
                   Result :: {string(), string() | binary()}.

sanitise_kv(Key, <<"">>) when is_list(Key) ->
    {Key, <<"">>}; % workaround exec:run apparently objecting
                   % to null-strings as lists

sanitise_kv(Key, "") when is_list(Key) ->
    {Key, <<"">>}; % ditto, for a plain empty string value

sanitise_kv(Key, Value) when is_binary(Value) and is_list(Key) ->
    {Key, binary_to_list(Value)};

sanitise_kv(Key, Value) when is_integer(Value) and is_list(Key) ->
    {Key, integer_to_list(Value)};

sanitise_kv(Key, undefined) when is_list(Key) ->
    {Key, "undefined"};

sanitise_kv(Key, Value) when is_list(Key) and is_list(Value) ->
    {Key, Value}.
