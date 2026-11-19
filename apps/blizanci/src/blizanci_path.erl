%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_path).

-export([fix_path/1, path_under_root/2, confine/2, confine/3,
         resolve_request/3]).

-include("blizanci_types.hrl").


%% TODO: does not support relative paths
-doc """
Canonicalise and normalise (q.v.) the path S, which may be a
string or a binary; the result has the same type.
""".
-spec fix_path(S) -> Result
              when S      :: string(),
                   Result :: {ok, string()} | {error, atom()};
                  (S) -> Result
              when S      :: binary(),
                   Result :: {ok, binary()} | {error, atom()}.
fix_path(S) ->
    fix_path2(realpath:normalise(S)).

fix_path2({ok, S}) ->
    fix_path3(realpath:canonicalise(S));
fix_path2(Error) -> Error.

fix_path3({ok, S}) ->
    realpath:normalise(S);
fix_path3(Error) -> Error.



-doc """
Is the path S underneath Root (or Root itself)? S must equal Root, or
begin with Root followed by a path separator: a bare string-prefix
test would wrongly accept a sibling such as /srv/docs-secret for a
Root of /srv/docs. S and Root are both strings or both binaries.

It is (currently) the responsibility of the caller to canonicalise/normalise
the paths before calling this function.

- `S`: the path to check
- `Root`: the directory under which S is supposed to be located
""".
-spec path_under_root(S, Root) -> Result
              when S      :: string() | binary(),
                   Root   :: string() | binary(),
                   Result :: boolean().
path_under_root(S, Root) when is_binary(S), is_binary(Root) ->
    Prefix = case binary:last(Root) of
                 $/ -> Root;
                 _  -> <<Root/binary, "/">>
             end,
    N = byte_size(Prefix),
    S =:= Root orelse (byte_size(S) >= N andalso
                       binary:part(S, 0, N) =:= Prefix);
path_under_root(S, Root) when is_list(S), is_list(Root) ->
    Prefix = case lists:suffix("/", Root) of
                 true  -> Root;
                 false -> Root ++ "/"
             end,
    S =:= Root orelse lists:prefix(Prefix, S).


-doc """
Join SubPath onto Root, canonicalise the result, and confine it to
lie under Root -- the pattern shared by every servlet that resolves
a request path against a configured root directory (CGI, static
files, Titan uploads).

File names are bytes. SubPath, Root and the result are all binaries,
so the name that is checked is exactly the name that is opened.
(Checking a string and opening it under another encoding once let a
symlink named with a non-ASCII character lead out of the docroot.)
The router converts the configured roots to binaries when a route is
built (see blizanci_router); anything else as Root or SubPath is
refused.

Root is resolved to an absolute path first (via filename:absname/1),
since fix_path/1 requires an absolute path and configured roots may
be relative.

Root itself is canonicalised too, not just absname'd, before being
used as the confinement prefix: if Root sits behind a symlink (e.g.
a blue/green deploy pattern such as docroot -> releases/v3), every
legitimate resolved path would otherwise fail the prefix check,
since Cmd is fully symlink-resolved but a bare absname'd Root would
not be.

A canonicalisation failure (e.g. a symlink loop tripping
realpath's TTL guard) and a path which resolves outside Root are
deliberately reported identically, as `file_not_found`: a remote
client should not be able to use the response to distinguish "this
doesn't exist" from "you tried to escape the root". The real reason
is still logged.

SubPath may contain any bytes except control characters: NUL, the
other bytes below 16#20, and 16#7F (DEL) are refused. Bytes 16#80 and
above are allowed, whether or not they form valid UTF-8. This is
deliberately NOT a policy on which names are acceptable: invisible,
bidirectional and look-alike characters (zero-width characters, bidi
overrides and so on) pass through here, and are for the caller, in the
URL layer, to refuse. Root is not restricted this way (it is the
operator's), but may not contain NUL, which operating systems treat as
the end of a name.
Anything other than a binary as SubPath is refused.

Hidden files are refused: any path segment beginning with a dot is
denied (as file_not_found, so the response does not reveal whether
the file exists), whatever the servlet. It is judged twice: on
SubPath as given, so a symlink named ".env" is refused although it
points at an ordinary file, and on the resolved path relative to Root,
so a link to a dot-directory (say "pub" pointing at ".git") is refused
although its own name is innocent. Only segments below Root count: a
Root that itself lies under a dot-directory is fine. The one exception
is a ".well-known" directory as the FIRST segment under Root, as RFC
8615 places it, and only when the caller asks for it with the option
`well_known` (confine/3); a ".well-known" further down is an ordinary
hidden directory, and a dotfile inside ".well-known" is still hidden.
confine/2 allows no exception.
""".
-spec confine(SubPath, Root) -> Result
              when SubPath :: binary(),
                   Root    :: binary(),
                   Result  :: {ok, binary()}
                            | {error, file_not_found}.

confine(SubPath, Root) ->
    confine(SubPath, Root, #{}).


-spec confine(SubPath, Root, Opts) -> Result
              when SubPath :: binary(),
                   Root    :: binary(),
                   Opts    :: #{ well_known => boolean() },
                   Result  :: {ok, binary()}
                            | {error, file_not_found}.

confine(SubPath, Root, Opts) when is_binary(SubPath), is_binary(Root) ->
    confine0(SubPath, Root, Opts);
confine(SubPath, Root, _Opts) ->
    logger:warning("path ~w under root ~p is not a binary", [SubPath, Root]),
    {error, file_not_found}.


-doc """
The step every servlet starts with: take the request path from the
route's Matches (key `<<"PATH">>`, which must be present) and confine
it under Root with confine/3. Each servlet chooses the Root and
options it needs, and wraps the error atom in its own result shape.
""".
-spec resolve_request(Matches, Root, Opts) -> Result
              when Matches :: path_matches(),
                   Root    :: binary(),
                   Opts    :: #{ well_known => boolean() },
                   Result  :: {ok, binary()}
                            | {error, file_not_found}.

resolve_request(#{ <<"PATH">> := Path }, Root, Opts) ->
    confine(Path, Root, Opts).


confine0(SubPath, Root, Opts) ->
    case {has_control(SubPath), has_nul(Root)} of
        {false, false} ->
            confine1(SubPath, Root, Opts);
        {true, _} ->
            logger:debug("path ~w under root ~p has a control character",
                         [SubPath, Root]),
            {error, file_not_found};
        {false, true} ->
            % Operating systems end a name at a NUL, so a name containing
            % one means something other than what was checked.
            logger:warning("root ~w contains a NUL", [Root]),
            {error, file_not_found}
    end.


% Bytes below 16#20 (NUL included) and DEL.
-spec has_control(Path) -> Result
              when Path   :: binary(),
                   Result :: boolean().
has_control(Path) ->
    re:run(Path, <<"[\\0-\\x1f\\x7f]">>, [{capture, none}]) =:= match.


-spec has_nul(Path) -> Result
              when Path   :: binary(),
                   Result :: boolean().
has_nul(Path) ->
    binary:match(Path, <<0>>) =/= nomatch.


-spec confine1(SubPath, Root, Opts) -> Result
              when SubPath :: binary(),
                   Root    :: binary(),
                   Opts    :: map(),
                   Result  :: {ok, binary()}
                            | {error, file_not_found}.

confine1(SubPath, Root, Opts) ->
    AbsRoot = filename:absname(Root),
    case fix_path(AbsRoot) of
        {ok, CanonRoot} -> confine2(SubPath, AbsRoot, CanonRoot, Opts);
        {error, Reason} ->
            logger:warning("root path resolution failed for ~p: ~p",
                           [AbsRoot, Reason]),
            {error, file_not_found}
    end.


% The hidden-file rule (see confine/3): no segment may begin with a dot,
% in the path as asked for or in the path it resolves to, except
% ".well-known" as the first segment where the caller allows it. "." and
% ".." are not hidden names; they are dealt with by resolving the path.
-spec visible(SubPath, Cmd, CanonRoot, Opts) -> Result
              when SubPath   :: binary(),
                   Cmd       :: binary(),
                   CanonRoot :: binary(),
                   Opts      :: map(),
                   Result    :: boolean().
visible(SubPath, Cmd, CanonRoot, Opts) ->
    WellKnown = maps:get(well_known, Opts, false),
    N = byte_size(CanonRoot),
    Resolved = binary:part(Cmd, N, byte_size(Cmd) - N),
    none_hidden(segments(SubPath), WellKnown)
        andalso none_hidden(segments(Resolved), WellKnown).


-spec segments(Path) -> Result
              when Path   :: binary(),
                   Result :: [binary()].
segments(Path) ->
    [ S || S <- binary:split(Path, <<"/">>, [global]),
           S =/= <<>>, S =/= <<".">>, S =/= <<"..">> ].


-spec none_hidden(Segments, WellKnown) -> Result
              when Segments  :: [binary()],
                   WellKnown :: boolean(),
                   Result    :: boolean().
none_hidden([<<".well-known">> | Rest], true) ->
    none_hidden(Rest, false);
none_hidden(Segments, _WellKnown) ->
    not lists:any(fun (<<$., _/binary>>) -> true;
                      (_) -> false
                  end, Segments).


-spec confine2(SubPath, AbsRoot, CanonRoot, Opts) -> Result
              when SubPath   :: binary(),
                   AbsRoot   :: binary(),
                   CanonRoot :: binary(),
                   Opts      :: map(),
                   Result    :: {ok, binary()}
                              | {error, file_not_found}.

confine2(SubPath, AbsRoot, CanonRoot, Opts) ->
    case fix_path(filename:join(AbsRoot, SubPath)) of
        {ok, Cmd} ->
            case path_under_root(Cmd, CanonRoot) of
                true ->
                    case visible(SubPath, Cmd, CanonRoot, Opts) of
                        true ->
                            {ok, Cmd};
                        false ->
                            logger:debug("path ~p under ~p has a hidden "
                                         "segment", [SubPath, CanonRoot]),
                            {error, file_not_found}
                    end;
                false ->
                    logger:warning("path ~p resolved outside root ~p",
                                   [Cmd, CanonRoot]),
                    {error, file_not_found}
            end;
        {error, Reason} ->
            logger:debug("path resolution failed for ~p under ~p: ~p",
                         [SubPath, AbsRoot, Reason]),
            {error, file_not_found}
    end.
