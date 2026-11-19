%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_urlpath).

-moduledoc """
Percent-decoding of the path of a request URL, and the reverse for
redirects. This is the one place where a request path is decoded.

See the manual chapter [Request handling](request-handling.md).
""".

-export([decode_path/1, split_titan/1, encode_path/1]).

-type decoded() :: {decoded_path, binary()}.
-export_type([decoded/0]).

% The printable ASCII range: a decoded byte outside it is refused.
-define(MIN_PRINTABLE, 16#20). % space
-define(MAX_PRINTABLE, 16#7E). % tilde


-doc """
Decode the (already normalised, leading-slash-free) path of a request.
A malformed escape is `request_not_parsed`; a path that decodes to
something not allowed is `bad_filename`. These are the names of the
Gemini status codes (see blizanci_status).
""".
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
add_byte(Byte, _Rest, _Acc)
  when Byte < ?MIN_PRINTABLE; Byte > ?MAX_PRINTABLE ->
    {error, bad_filename};
add_byte($/, _Rest, _Acc) ->
    {error, bad_filename};
add_byte(Byte, Rest, Acc) ->
    decode_segment(Rest, <<Acc/binary, Byte>>).


-doc """
A Titan path carries its parameters after the first ";" of the raw
path ("name;size=3;mime=..."). Split the raw path there, before any
decoding, so that an escaped ";" in the name can never be mistaken for
the start of the parameters. Returns the file part and the parameters
(everything after the ";", still raw), or `none` if there is no ";".
""".
-spec split_titan(Fragment) -> Result
              when Fragment :: binary(),
                   Result   :: {binary(), binary() | none}.
split_titan(Fragment) when is_binary(Fragment) ->
    case binary:split(Fragment, <<";">>) of
        [File]         -> {File, none};
        [File, Params] -> {File, Params}
    end.


-doc """
The reverse of decode_path/1, for a path that is going into a URL (as
in a redirect): each segment is percent-encoded, so that spaces, "%",
"?", "#" and so on in a name survive, and the segments are joined with
"/". Decoding the result gives the path back.
""".
-spec encode_path(Path) -> Result
              when Path   :: binary(),
                   Result :: binary().
encode_path(Path) when is_binary(Path) ->
    Segments = binary:split(Path, <<"/">>, [global]),
    join([ uri_string:quote(S) || S <- Segments ]).
