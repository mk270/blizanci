%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_osenv_tests).

-include_lib("eunit/include/eunit.hrl").

-import(blizanci_osenv, [sanitise/1]).

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
    apply(blizanci_osenv, sanitise, [Env]).

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
                   apply(blizanci_osenv, sanitise, [foo])),
     ?_assertError({bad_generator, #{}},
                   apply(blizanci_osenv, sanitise, [#{}])),
     ?_assertError({bad_generator, <<"K=v">>},
                   apply(blizanci_osenv, sanitise, [<<"K=v">>]))].
