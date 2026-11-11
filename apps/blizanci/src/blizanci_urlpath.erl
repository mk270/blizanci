%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

%% @doc
%% Percent-decoding of the path of a request URL, and the reverse for
%% redirects.
%%
%% This is the one place where a request path is decoded. The rules:
%%
%% <ul>
%% <li>The path is split on "/" first, and each segment is decoded on its
%%     own, so an escaped "/" cannot create a new separator (it is
%%     refused).</li>
%% <li>Every "%" must be followed by two hex digits, in either case;
%%     otherwise the path is malformed.</li>
%% <li>Decoding happens exactly once: "%2520" gives the text "%20".</li>
%% <li>The result may contain only printable ASCII (16#20 to 16#7E). NUL,
%%     other control characters, DEL and every non-ASCII byte are refused,
%%     whether they were written raw or as escapes, valid UTF-8 or not.
%%     (A decoded name is therefore also a valid name in any file name
%%     encoding.) This is a deliberate policy: names stay in a closed
%%     character set, so invisible, bidirectional and look-alike
%%     characters cannot appear.</li>
%% <li>A decoded segment may not be "." or "..".</li>
%% </ul>
%%
%% A decoded path is returned as `{decoded_path, Bin}', so that it is
%% distinguishable from a raw one. Callers outside this module may unwrap
%% it (and the request layer does, for now).
%% @end

-module(blizanci_urlpath).

-export([decode_path/1, split_titan/1, encode_path/1]).

-include_lib("eunit/include/eunit.hrl").

-type decoded() :: {decoded_path, binary()}.
-export_type([decoded/0]).


%% @doc
%% Decode the (already normalised, leading-slash-free) path of a request.
%% A malformed escape is `request_not_parsed'; a path that decodes to
%% something not allowed is `bad_filename'. These are the names of the
%% Gemini status codes (see blizanci_status).
%% @end
-spec decode_path(Path) -> Result
              when Path   :: binary(),
                   Result :: {ok, decoded()}
                           | {error, request_not_parsed | bad_filename}.
decode_path(Path) when is_binary(Path) ->
    case well_formed(Path) of
        true ->
            Segments = binary:split(Path, <<"/">>, [global]),
            decode_segments(Segments, []);
        false ->
            {error, request_not_parsed}
    end.


% Every "%" is followed by two hex digits.
-spec well_formed(Bin) -> Result
              when Bin    :: binary(),
                   Result :: boolean().
well_formed(<<>>) -> true;
well_formed(<<$%, H, L, Rest/binary>>) ->
    is_hex(H) andalso is_hex(L) andalso well_formed(Rest);
well_formed(<<$%, _/binary>>) -> false;
well_formed(<<_, Rest/binary>>) -> well_formed(Rest).


-spec is_hex(Char) -> Result
              when Char   :: byte(),
                   Result :: boolean().
is_hex(C) when C >= $0, C =< $9 -> true;
is_hex(C) when C >= $a, C =< $f -> true;
is_hex(C) when C >= $A, C =< $F -> true;
is_hex(_) -> false.


-spec hex_value(Char) -> Result
              when Char   :: byte(),
                   Result :: 0..15.
hex_value(C) when C >= $0, C =< $9 -> C - $0;
hex_value(C) when C >= $a, C =< $f -> C - $a + 10;
hex_value(C) when C >= $A, C =< $F -> C - $A + 10.


-spec decode_segments(Segments, Acc) -> Result
              when Segments :: [binary()],
                   Acc      :: [binary()],
                   Result   :: {ok, decoded()} | {error, bad_filename}.
decode_segments([], Acc) ->
    {ok, {decoded_path, join(lists:reverse(Acc))}};
decode_segments([Segment | Rest], Acc) ->
    case decode_segment(Segment, <<>>) of
        {ok, <<".">>}  -> {error, bad_filename};
        {ok, <<"..">>} -> {error, bad_filename};
        {ok, Decoded}  -> decode_segments(Rest, [Decoded | Acc]);
        {error, _} = Error -> Error
    end.


-spec join(Segments) -> Result
              when Segments :: [binary()],
                   Result   :: binary().
join(Segments) ->
    iolist_to_binary(lists:join(<<"/">>, Segments)).


% A segment contains no "/" before decoding, so one after decoding came
% from an escape.
-spec decode_segment(Segment, Acc) -> Result
              when Segment :: binary(),
                   Acc     :: binary(),
                   Result  :: {ok, binary()} | {error, bad_filename}.
decode_segment(<<>>, Acc) ->
    {ok, Acc};
decode_segment(<<$%, H, L, Rest/binary>>, Acc) ->
    add_byte(hex_value(H) * 16 + hex_value(L), Rest, Acc);
decode_segment(<<C, Rest/binary>>, Acc) ->
    add_byte(C, Rest, Acc).


-spec add_byte(Byte, Rest, Acc) -> Result
              when Byte   :: byte(),
                   Rest   :: binary(),
                   Acc    :: binary(),
                   Result :: {ok, binary()} | {error, bad_filename}.
add_byte(Byte, _Rest, _Acc) when Byte < 16#20; Byte > 16#7E ->
    {error, bad_filename};
add_byte($/, _Rest, _Acc) ->
    {error, bad_filename};
add_byte(Byte, Rest, Acc) ->
    decode_segment(Rest, <<Acc/binary, Byte>>).


%% @doc
%% A Titan path carries its parameters after the first ";" of the raw
%% path ("name;size=3;mime=..."). Split the raw path there, before any
%% decoding, so that an escaped ";" in the name can never be mistaken for
%% the start of the parameters. Returns the file part and the parameters
%% (everything after the ";", still raw), or `none' if there is no ";".
%% @end
-spec split_titan(Fragment) -> Result
              when Fragment :: binary(),
                   Result   :: {binary(), binary() | none}.
split_titan(Fragment) when is_binary(Fragment) ->
    case binary:split(Fragment, <<";">>) of
        [File]         -> {File, none};
        [File, Params] -> {File, Params}
    end.


%% @doc
%% The reverse of decode_path/1, for a path that is going into a URL (as
%% in a redirect): each segment is percent-encoded, so that spaces, "%",
%% "?", "#" and so on in a name survive, and the segments are joined with
%% "/". Decoding the result gives the path back.
%% @end
-spec encode_path(Path) -> Result
              when Path   :: binary(),
                   Result :: binary().
encode_path(Path) when is_binary(Path) ->
    Segments = binary:split(Path, <<"/">>, [global]),
    join([ uri_string:quote(S) || S <- Segments ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
