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
-include_lib("eunit/include/eunit.hrl").
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Values: converted to strings, except that an empty value (of either
%% kind) becomes the empty binary, which erlexec accepts where it
%% objects to an empty list.
value_test_() ->
    [?_assertEqual([{"K", "v"}], sanitise([{"K", "v"}])),
     ?_assertEqual([{"K", "v"}], sanitise([{"K", <<"v">>}])),
     ?_assertEqual([{"K", "1234"}], sanitise([{"K", 1234}])),
     ?_assertEqual([{"K", "0"}], sanitise([{"K", 0}])),
     ?_assertEqual([{"K", "-7"}], sanitise([{"K", -7}])),
     ?_assertEqual([{"K", "undefined"}], sanitise([{"K", undefined}])),
     ?_assertEqual([{"K", <<>>}], sanitise([{"K", ""}])),
     ?_assertEqual([{"K", <<>>}], sanitise([{"K", <<>>}])),
     % UTF-8 bytes are passed through as bytes, not decoded
     ?_assertEqual([{"K", [195, 169]}],
                   sanitise([{"K", <<195, 169>>}])),
     % a NUL byte is not removed here (see blizanci_cgi:make_environment)
     ?_assertEqual([{"K", [0, $a]}], sanitise([{"K", <<0, $a>>}])),
     % a string value is not checked, only passed on
     ?_assertEqual([{"K", [a, b]}], loose([{"K", [a, b]}]))].

%% Calls that deliberately break the spec, so go through apply.
loose(Env) ->
    apply(?MODULE, sanitise, [Env]).

%% Keys: only lists are kept; anything else is silently dropped, as is
%% any element that is not a two-tuple.
key_test_() ->
    [?_assertEqual([{"", "v"}], loose([{"", "v"}])),
     ?_assertEqual([], loose([{<<"K">>, "v"}])),
     ?_assertEqual([], loose([{'K', "v"}])),
     ?_assertEqual([], loose([{1, "v"}])),
     % a dropped key's value is never looked at
     ?_assertEqual([], loose([{<<"K">>, 1.5}])),
     ?_assertEqual([], loose(["K=v"])),
     ?_assertEqual([], loose([foo, {a, b, c}, {}])),
     ?_assertEqual([{"K", "v"}], loose([foo, {"K", "v"}, {a, b, c}]))].

%% Order and duplicates are preserved; nothing is merged.
list_test_() ->
    [?_assertEqual([], loose([])),
     ?_assertEqual([{"B", "2"}, {"A", "1"}, {"C", "c"}],
                   loose([{"B", 2}, {"A", <<"1">>}, {"C", "c"}])),
     ?_assertEqual([{"K", "1"}, {"K", "2"}, {"K", "1"}],
                   loose([{"K", 1}, {"K", "2"}, {"K", <<"1">>}])),
     ?_assertEqual([{"A", "x"}, {"B", "y"}],
                   loose([{1, "no"}, {"A", "x"}, {<<"Z">>, "no"},
                          {"B", "y"}]))].

%% A value of a kind not listed in the doc crashes the call, as the
%% doc says, even when other entries are fine.
unreasonable_value_test_() ->
    [?_assertError(function_clause, loose([{"K", 1.5}])),
     ?_assertError(function_clause, loose([{"K", foo}])),
     ?_assertError(function_clause, loose([{"K", {a}}])),
     ?_assertError(function_clause, loose([{"K", #{}}])),
     ?_assertError(function_clause, loose([{"K", true}])),
     ?_assertError(function_clause, loose([{"A", "ok"}, {"K", 1.5}]))].

%% Not a list of pairs at all.
bad_env_test_() ->
    [?_assertError({bad_generator, foo},
                   apply(?MODULE, sanitise, [foo])),
     ?_assertError({bad_generator, #{}},
                   apply(?MODULE, sanitise, [#{}])),
     ?_assertError({bad_generator, <<"K=v">>},
                   apply(?MODULE, sanitise, [<<"K=v">>]))].
