%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_urlpath_tests).

-include_lib("eunit/include/eunit.hrl").

-import(blizanci_urlpath, [decode_path/1, split_titan/1, encode_path/1]).

decode_ok_test_() ->
    D = fun(P) -> decode_path(P) end,
    Ok = fun(B) -> {ok, {decoded_path, B}} end,
    [?_assertEqual(Ok(<<>>), D(<<>>)),
     ?_assertEqual(Ok(<<"a/b/c.txt">>), D(<<"a/b/c.txt">>)),
     ?_assertEqual(Ok(<<"a b">>), D(<<"a%20b">>)),
     ?_assertEqual(Ok(<<"100%">>), D(<<"100%25">>)),
     ?_assertEqual(Ok(<<"a;b">>), D(<<"a%3Bb">>)),
     ?_assertEqual(Ok(<<"a;b">>), D(<<"a%3bb">>)),
     ?_assertEqual(Ok(<<"a b/c d">>), D(<<"a%20b/c%20d">>)),
     % once only
     ?_assertEqual(Ok(<<"%2e">>), D(<<"%252e">>)),
     ?_assertEqual(Ok(<<"%20">>), D(<<"%2520">>)),
     % structure is kept: empty segments and a trailing slash
     ?_assertEqual(Ok(<<"a//b/">>), D(<<"a//b/">>)),
     % the whole printable ASCII range is allowed
     ?_assertEqual(Ok(<<" ~">>), D(<<"%20%7E">>))].

decode_refused_test_() ->
    D = fun(P) -> decode_path(P) end,
    Bad = {error, bad_filename},
    Malformed = {error, request_not_parsed},
    [?_assertEqual(Bad, D(<<"a%2Fb">>)),
     ?_assertEqual(Bad, D(<<"a%2fb">>)),
     ?_assertEqual(Bad, D(<<"%2F">>)),
     ?_assertEqual(Bad, D(<<"a%00b">>)),
     ?_assertEqual(Bad, D(<<"a%1Fb">>)),
     ?_assertEqual(Bad, D(<<"a%7Fb">>)),
     ?_assertEqual(Bad, D(<<"%80">>)),
     ?_assertEqual(Bad, D(<<"%C3%A9">>)),
     ?_assertEqual(Bad, D(<<"%FF">>)),
     % raw bytes are held to the same rule
     ?_assertEqual(Bad, D(<<195, 169>>)),
     ?_assertEqual(Bad, D(<<"a", 0, "b">>)),
     ?_assertEqual(Bad, D(<<"a", 127>>)),
     % a dot segment, however written (a raw one is normalised away
     % before this point)
     ?_assertEqual(Bad, D(<<"..">>)),
     ?_assertEqual(Bad, D(<<"a/../b">>)),
     ?_assertEqual(Bad, D(<<"a/%2e%2e/b">>)),
     ?_assertEqual(Bad, D(<<".">>)),
     ?_assertEqual(Malformed, D(<<"%">>)),
     ?_assertEqual(Malformed, D(<<"a%">>)),
     ?_assertEqual(Malformed, D(<<"%2">>)),
     ?_assertEqual(Malformed, D(<<"a%4">>)),
     ?_assertEqual(Malformed, D(<<"%zz">>)),
     ?_assertEqual(Malformed, D(<<"%G0">>)),
     ?_assertEqual(Malformed, D(<<"%0G">>)),
     % a malformed escape is reported even if something else is refused
     ?_assertEqual(Malformed, D(<<"%2F/%zz">>))].

split_titan_test_() ->
    [?_assertEqual({<<"a">>, <<"size=1">>}, split_titan(<<"a;size=1">>)),
     ?_assertEqual({<<"a">>, <<"size=1;mime=x">>},
                   split_titan(<<"a;size=1;mime=x">>)),
     ?_assertEqual({<<"a%3Bb">>, <<"size=1">>},
                   split_titan(<<"a%3Bb;size=1">>)),
     ?_assertEqual({<<"a">>, none}, split_titan(<<"a">>)),
     ?_assertEqual({<<>>, <<>>}, split_titan(<<";">>))].

encode_test_() ->
    E = fun(P) -> encode_path(P) end,
    [?_assertEqual(<<"a/b.txt">>, E(<<"a/b.txt">>)),
     ?_assertEqual(<<"a%20b/c">>, E(<<"a b/c">>)),
     ?_assertEqual(<<"100%25">>, E(<<"100%">>)),
     ?_assertEqual(<<"x%3Fy%23z">>, E(<<"x?y#z">>)),
     ?_assertEqual(<<"sp%20ace/index.gemini">>,
                   E(<<"sp ace/index.gemini">>)),
     ?_assertEqual(<<"a/">>, E(<<"a/">>)),
     % decoding gives the path back, for every printable ASCII name
     ?_test(begin
                All = list_to_binary(lists:seq(16#20, 16#7E)),
                Names = [ <<"x", C, "y">> || <<C>> <= All, C =/= $/ ],
                [ ?assertEqual({ok, {decoded_path, N}},
                               decode_path(E(N))) || N <- Names ]
            end)].
