%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_osenv).

-moduledoc """
This module deals with sanitising a proplist of OS environment
variables such that it's appropriate for creating a new UNIX
subprocess via the erlexec port.
""".
-include("blizanci_types.hrl").

-export([sanitise/1]).


-doc """
Sanitise the system environment variables.

erlexec expects environment variables to be strings, not binaries;
the function tries to massage the various reasonable input values
(such as integers and binaries) into acceptable strings. It crashes
on unreasonable input.
""".
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
%
% An empty value is handed to erlexec as the empty BINARY <<"">>, never as
% an empty string.  erlexec's port program (c_src/ei++.hpp, decodeString;
% present in 2.4.0 and, by reading the source, in 2.6.0) mishandles an
% empty string: "" is the same term as [] (NIL on the wire), and
% decodeString takes its etNil branch, which reports success without
% consuming the NIL byte it peeked at.  The next decode then takes that
% leftover byte for the env list's real terminator, so everything after it
% is read one byte out of place and exec:run/2 fails with an error that
% looks unrelated:
%
%   exec:run("env", [stdout, {env, [{"X", ""}]}])
%   => {error, "badarg: option must be {Cmd, Opt} or {Cmd, Opt, Args} or
%               atom (got tp=j, arity=0)"}      % j is the NIL tag
%
% A binary has its own wire tag even at zero length, so {"X", <<"">>} is
% fine.  This matters here because the CGI environment always has
% genuinely empty values (PATH_INFO, QUERY_STRING).  By the source the same
% decoder reads other string options (cd, executable, kill command), so an
% empty string there would probably misbehave the same way.  Not reported
% upstream (saleyn/erlexec) as of 2026-11-19; the fix there looks like
% consuming the NIL byte in the etNil branch.
-spec sanitise_kv(Key, Value) -> Result
              when Key    :: string(),
                   Value  :: term(),
                   Result :: {string(), string() | binary()}.

sanitise_kv(Key, <<"">>) when is_list(Key) ->
    {Key, <<"">>}; % an empty value stays a binary (see above)

sanitise_kv(Key, "") when is_list(Key) ->
    {Key, <<"">>}; % an empty string becomes one

sanitise_kv(Key, Value) when is_binary(Value) and is_list(Key) ->
    {Key, binary_to_list(Value)};

sanitise_kv(Key, Value) when is_integer(Value) and is_list(Key) ->
    {Key, integer_to_list(Value)};

sanitise_kv(Key, undefined) when is_list(Key) ->
    {Key, "undefined"};

sanitise_kv(Key, Value) when is_list(Key) and is_list(Value) ->
    {Key, Value}.
