%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_path).

-export([fix_path/1, path_under_root/2, confine/2, confine/3]).

-include_lib("eunit/include/eunit.hrl").

%% @doc Canonicalise and normalise (q.v.) the path S
%% @todo does not support relative paths
%% @end
-spec fix_path(S) -> Result
              when S      :: list(),
                   Result :: {ok, string()} | {error, atom()}.
fix_path(S) ->
    fix_path2(realpath:normalise(S)).

fix_path2({ok, S}) ->
    fix_path3(realpath:canonicalise(S));
fix_path2(Error) -> Error.

fix_path3({ok, S}) ->
    realpath:normalise(S);
fix_path3(Error) -> Error.



%% @doc
%% Is the path S underneath Root (or Root itself)? S must equal Root, or
%% begin with Root followed by a path separator: a bare string-prefix
%% test would wrongly accept a sibling such as /srv/docs-secret for a
%% Root of /srv/docs.
%%
%% It is (currently) the responsibility of the caller to canonicalise/normalise
%% the paths before calling this function.
%% @param S the path to check
%% @param Root the directory under which S is supposed to be located
%% @end
-spec path_under_root(S, Root) -> Result
              when S      :: string(),
                   Root   :: string(),
                   Result :: boolean().
path_under_root(S, Root) ->
    Prefix = case lists:suffix("/", Root) of
                 true  -> Root;
                 false -> Root ++ "/"
             end,
    S =:= Root orelse lists:prefix(Prefix, S).


%% @doc
%% Join SubPath onto Root, canonicalise the result, and confine it to
%% lie under Root -- the pattern shared by every servlet that resolves
%% a request path against a configured root directory (CGI, static
%% files, Titan uploads).
%%
%% Root is resolved to an absolute path first (via filename:absname/1,
%% as blizanci_titan already does for its own root), since fix_path/1
%% requires an absolute path and most of the configured roots default
%% to relative ones. This is a minimal, local fix to keep those
%% defaults working; it does not address the broader FIXME in
%% blizanci_config:proto_opts/1 about validating such config up front.
%%
%% Root itself is canonicalised too, not just absname'd, before being
%% used as the confinement prefix: if Root sits behind a symlink (e.g.
%% a blue/green deploy pattern such as docroot -> releases/v3), every
%% legitimate resolved path would otherwise fail the prefix check,
%% since Cmd is fully symlink-resolved but a bare absname'd Root would
%% not be.
%%
%% A canonicalisation failure (e.g. a symlink loop tripping
%% realpath's TTL guard) and a path which resolves outside Root are
%% deliberately reported identically, as `file_not_found`: a remote
%% client should not be able to use the response to distinguish "this
%% doesn't exist" from "you tried to escape the root". The real reason
%% is still logged.
%%
%% SubPath may contain only printable ASCII (space to tilde), whatever
%% the caller has decoded or not: control characters (NUL included) and
%% anything non-ASCII are refused. A name is checked in one
%% representation and later opened in another, and how Erlang maps a
%% list of integers to the bytes of a file name depends on the VM's
%% file-name encoding (UTF-8 or Latin-1, chosen from the locale), so for
%% non-ASCII text the name checked and the name opened can differ.
%% ASCII is the same in every encoding. This is deliberately the
%% narrowest rule that is safe; names that aren't ASCII can be served
%% once confine/2 handles names as bytes end to end. Root is not
%% restricted this way (it is the operator's), but may not contain NUL.
%%
%% Hidden files are refused: any path segment beginning with a dot is
%% denied (as file_not_found, so the response does not reveal whether
%% the file exists), whatever the servlet. It is judged twice: on
%% SubPath as given, so a symlink named ".env" is refused although it
%% points at an ordinary file, and on the resolved path relative to Root,
%% so a link to a dot-directory (say "pub" pointing at ".git") is refused
%% although its own name is innocent. Only segments below Root count: a
%% Root that itself lies under a dot-directory is fine. The one exception
%% is a ".well-known" directory as the FIRST segment under Root, as RFC
%% 8615 places it, and only when the caller asks for it with the option
%% `well_known` (confine/3); a ".well-known" further down is an ordinary
%% hidden directory, and a dotfile inside ".well-known" is still hidden.
%% confine/2 allows no exception.
%% @end
-spec confine(SubPath, Root) -> Result
              when SubPath :: string(),
                   Root    :: string(),
                   Result  :: {ok, string()}
                            | {error, file_not_found}.

confine(SubPath, Root) ->
    confine(SubPath, Root, #{}).


-spec confine(SubPath, Root, Opts) -> Result
              when SubPath :: string(),
                   Root    :: string(),
                   Opts    :: #{ well_known => boolean() },
                   Result  :: {ok, string()}
                            | {error, file_not_found}.

confine(SubPath, Root, Opts) ->
    case {printable_ascii(SubPath), has_nul(Root)} of
        {true, false} ->
            confine1(SubPath, Root, Opts);
        {false, _} ->
            logger:warning("path ~w under root ~p has characters other "
                           "than printable ASCII", [SubPath, Root]),
            {error, file_not_found};
        {true, true} ->
            % Operating systems end a name at a NUL, so a name containing
            % one means something other than what was checked.
            logger:warning("root ~w contains a NUL", [Root]),
            {error, file_not_found}
    end.


-spec printable_ascii(Path) -> Result
              when Path   :: string() | binary(),
                   Result :: boolean().
printable_ascii(Path) when is_binary(Path) ->
    printable_ascii(binary_to_list(Path));
printable_ascii(Path) when is_list(Path) ->
    lists:all(fun (C) -> is_integer(C) andalso C >= 16#20 andalso C =< 16#7E
              end, lists:flatten(Path)).


-spec has_nul(Path) -> Result
              when Path   :: string() | binary(),
                   Result :: boolean().
has_nul(Path) when is_binary(Path) ->
    binary:match(Path, <<0>>) =/= nomatch;
has_nul(Path) when is_list(Path) ->
    lists:member(0, lists:flatten(Path)).


-spec confine1(SubPath, Root, Opts) -> Result
              when SubPath :: string(),
                   Root    :: string(),
                   Opts    :: map(),
                   Result  :: {ok, string()}
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
              when SubPath   :: string(),
                   Cmd       :: string(),
                   CanonRoot :: string(),
                   Opts      :: map(),
                   Result    :: boolean().
visible(SubPath, Cmd, CanonRoot, Opts) ->
    WellKnown = maps:get(well_known, Opts, false),
    Resolved = lists:nthtail(length(CanonRoot), Cmd),
    none_hidden(segments(SubPath), WellKnown)
        andalso none_hidden(segments(Resolved), WellKnown).


-spec segments(Path) -> Result
              when Path   :: string(),
                   Result :: [string()].
segments(Path) ->
    [ S || S <- string:split(Path, "/", all),
           S =/= "", S =/= ".", S =/= ".." ].


-spec none_hidden(Segments, WellKnown) -> Result
              when Segments  :: [string()],
                   WellKnown :: boolean(),
                   Result    :: boolean().
none_hidden([".well-known" | Rest], true) ->
    none_hidden(Rest, false);
none_hidden(Segments, _WellKnown) ->
    not lists:any(fun ([$. | _]) -> true;
                      (_) -> false
                  end, Segments).


-spec confine2(SubPath, AbsRoot, CanonRoot, Opts) -> Result
              when SubPath   :: string(),
                   AbsRoot   :: string(),
                   CanonRoot :: string(),
                   Opts      :: map(),
                   Result    :: {ok, string()}
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
                            logger:warning("path ~p under ~p has a hidden "
                                           "segment", [SubPath, CanonRoot]),
                            {error, file_not_found}
                    end;
                false ->
                    logger:warning("path ~p resolved outside root ~p",
                                   [Cmd, CanonRoot]),
                    {error, file_not_found}
            end;
        {error, Reason} ->
            logger:warning("path resolution failed for ~p under ~p: ~p",
                           [SubPath, AbsRoot, Reason]),
            {error, file_not_found}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

path_under_root_test_() ->
    [?_assert(path_under_root("/srv/docs/a/b", "/srv/docs")),
     ?_assert(path_under_root("/srv/docs", "/srv/docs")),
     ?_assert(path_under_root("/srv/docs/a", "/srv/docs/")),
     ?_assert(path_under_root("/anything", "/")),
     ?_assertNot(path_under_root("/srv/other/a", "/srv/docs")),
     % a sibling sharing a name prefix is not under the root
     ?_assertNot(path_under_root("/srv/docs-secret/a", "/srv/docs")),
     ?_assertNot(path_under_root("/srv/docs2", "/srv/docs")),
     ?_assertNot(path_under_root("/srv", "/srv/docs"))].

%% Fixture, built under a fresh temporary directory:
%%   Base/<d, o-acute, c, s>   (a root whose name is not ASCII)
%%   Base/.dotroot/f.txt        (a root that is itself under a dot name)
%%   Base/docs/.git/config  Base/docs/.well-known/{security,.secret}
%%   Base/docs/nested/.well-known/x
%%   Base/docs/viewgit -> .git   Base/docs/.dotlink -> inside.txt
%%   Base/docs/acme -> .well-known
%%   Base/docs/inside.txt
%%   Base/docs/escape -> ../outside        (symlink out of the root)
%%   Base/docs-secret/secret.txt           (sibling sharing a name prefix)
%%   Base/outside/secret.txt
confine_test_() ->
    {setup,
     fun confine_setup/0,
     fun confine_teardown/1,
     fun confine_tests/1}.

confine_setup() ->
    Unique = integer_to_list(erlang:unique_integer([positive])),
    Base = filename:absname("confine_test_" ++ Unique),
    Docs = filename:join(Base, "docs"),
    ok = filelib:ensure_path(Docs),
    ok = filelib:ensure_path(filename:join(Base, "docs-secret")),
    ok = filelib:ensure_path(filename:join(Base, "outside")),
    ok = file:write_file(filename:join(Docs, "inside.txt"), <<"x">>),
    ok = file:write_file(filename:join([Base, "docs-secret", "secret.txt"]),
                         <<"x">>),
    ok = file:write_file(filename:join([Base, "outside", "secret.txt"]),
                         <<"x">>),
    ok = file:make_symlink("../outside", filename:join(Docs, "escape")),
    ok = filelib:ensure_path(filename:join(Base, [$d, 243, $c, $s])),
    Mk = fun (Rel) ->
             Path = filename:join(Docs, Rel),
             ok = filelib:ensure_path(filename:dirname(Path)),
             ok = file:write_file(Path, <<"x">>)
         end,
    [ Mk(F) || F <- [".git/config", ".well-known/security",
                     ".well-known/.secret", "nested/.well-known/x"] ],
    ok = file:make_symlink(".git", filename:join(Docs, "viewgit")),
    ok = file:make_symlink("inside.txt", filename:join(Docs, ".dotlink")),
    ok = file:make_symlink(".well-known", filename:join(Docs, "acme")),
    DotRoot = filename:join(Base, ".dotroot"),
    ok = filelib:ensure_path(DotRoot),
    ok = file:write_file(filename:join(DotRoot, "f.txt"), <<"x">>),
    Base.

confine_teardown(Base) ->
    file:del_dir_r(Base).

confine_tests(Base) ->
    Docs = filename:join(Base, "docs"),
    {ok, CanonDocs} = fix_path(Docs),
    Odd = filename:join(Base, [$d, 243, $c, $s]),
    {ok, CanonOdd} = fix_path(Odd),
    [?_assertEqual({ok, filename:join(CanonDocs, "inside.txt")},
                   confine("inside.txt", Docs)),
     ?_assertEqual({error, file_not_found},
                   confine("../outside/secret.txt", Docs)),
     % regression: sibling directory sharing the root's name as a prefix
     ?_assertEqual({error, file_not_found},
                   confine("../docs-secret/secret.txt", Docs)),
     ?_assertEqual({error, file_not_found},
                   confine("escape/secret.txt", Docs)),
     % a NUL anywhere in the path is refused, even where the name before
     % it is fine
     ?_assertEqual({error, file_not_found},
                   confine("inside.txt\0.png", Docs)),
     ?_assertEqual({error, file_not_found}, confine("inside.txt\0", Docs)),
     ?_assertEqual({error, file_not_found}, confine("\0inside.txt", Docs)),
     ?_assertEqual({error, file_not_found}, confine([0], Docs)),
     ?_assertEqual({error, file_not_found},
                   confine("sub\0/../../outside/secret.txt", Docs)),
     ?_assertEqual({error, file_not_found},
                   confine(<<"inside.txt", 0, ".png">>, Docs)),
     ?_assertEqual({error, file_not_found},
                   confine("inside.txt", Docs ++ "\0")),
     % control characters other than NUL are refused too
     ?_assertEqual({error, file_not_found}, confine("inside\t.txt", Docs)),
     ?_assertEqual({error, file_not_found}, confine("inside\n.txt", Docs)),
     ?_assertEqual({error, file_not_found}, confine("inside\r.txt", Docs)),
     ?_assertEqual({error, file_not_found}, confine("inside\e.txt", Docs)),
     ?_assertEqual({error, file_not_found},
                   confine([$a, 127, $b], Docs)),
     % non-ASCII is refused, in any representation: a code point, the
     % UTF-8 bytes of it as a list (what binary_to_list/1 of a request
     % path gives), and as a binary
     ?_assertEqual({error, file_not_found},
                   confine([233, $., $t, $x, $t], Docs)),
     ?_assertEqual({error, file_not_found},
                   confine([195, 169, $., $t, $x, $t], Docs)),
     ?_assertEqual({error, file_not_found},
                   confine(<<195, 169, ".txt">>, Docs)),
     ?_assertEqual({error, file_not_found},
                   confine([16#4E2D, 16#6587], Docs)),
     ?_assertEqual({error, file_not_found}, confine("sub/\x{e9}", Docs)),
     % non-integers are refused, not crashed on
     ?_assertEqual({error, file_not_found}, confine([$a, b, $c], Docs)),
     % printable ASCII, space included, is fine whether or not the file
     % exists
     ?_assertEqual({ok, filename:join(CanonDocs, "inside.txt")},
                   confine("inside.txt", Docs)),
     ?_assertEqual({ok, filename:join(CanonDocs, "a b-c_d~e!.txt")},
                   confine("a b-c_d~e!.txt", Docs)),
     ?_assertEqual({ok, filename:join(CanonDocs, "sub/new file.txt")},
                   confine("sub/new file.txt", Docs)),
     % the root is the operator's and may be non-ASCII (but not hold a NUL)
     ?_assertEqual({ok, filename:join(CanonOdd, "f.txt")},
                   confine("f.txt", Odd))]
    ++ hidden_tests(Base, Docs, CanonDocs).

%% Hidden files: a path segment beginning with a dot is refused, except
%% ".well-known" as the first segment where the caller allows it.
hidden_tests(Base, Docs, CanonDocs) ->
    Strict = fun(P) -> confine(P, Docs) end,
    WK = fun(P) -> confine(P, Docs, #{ well_known => true }) end,
    Refused = {error, file_not_found},
    In = fun(P) -> {ok, filename:join(CanonDocs, P)} end,
    DotRoot = filename:join(Base, ".dotroot"),
    {ok, CanonDotRoot} = fix_path(DotRoot),
    [% any segment starting with a dot, at any depth, whether or not the
     % file exists
     ?_assertEqual(Refused, Strict(".env")),
     ?_assertEqual(Refused, Strict(".git/config")),
     ?_assertEqual(Refused, Strict("sub/.hidden")),
     ?_assertEqual(Refused, Strict(".hidden/sub/x")),
     ?_assertEqual(Refused, Strict("a/.b/c")),
     ?_assertEqual(Refused, Strict("...")),
     ?_assertEqual(Refused, WK(".env")),
     ?_assertEqual(Refused, WK(".git/config")),
     ?_assertEqual(Refused, WK("sub/.hidden")),
     % dots that are not at the start of a name are ordinary
     ?_assertEqual(In("file.with.dots"), Strict("file.with.dots")),
     ?_assertEqual(In("a.b/c.d"), Strict("a.b/c.d")),
     ?_assertEqual(In("trailing."), Strict("trailing.")),
     % "." and ".." are not hidden names; they are resolved
     ?_assertMatch({ok, _}, Strict(".")),
     ?_assertMatch({ok, _}, Strict("")),
     ?_assertEqual(In("inside.txt"), Strict("./inside.txt")),
     ?_assertEqual(In("inside.txt"), Strict("sub/../inside.txt")),
     % a link NAMED like a hidden file is refused although it points at an
     % ordinary one, and a link to a hidden directory is refused although
     % its own name is innocent
     ?_assertEqual(Refused, Strict(".dotlink")),
     ?_assertEqual(Refused, WK(".dotlink")),
     ?_assertEqual(Refused, Strict("viewgit/config")),
     ?_assertEqual(Refused, WK("viewgit/config")),
     ?_assertEqual(Refused, WK("viewgit")),
     % .well-known: no exception from confine/2 ...
     ?_assertEqual(Refused, Strict(".well-known/security")),
     ?_assertEqual(Refused, Strict(".well-known")),
     % ... but allowed as the first segment when asked for
     ?_assertEqual(In(".well-known/security"), WK(".well-known/security")),
     ?_assertEqual(In(".well-known"), WK(".well-known")),
     ?_assertEqual(In(".well-known/new/file"), WK(".well-known/new/file")),
     % a link to it resolves to it, and counts as it
     ?_assertEqual(In(".well-known/security"), WK("acme/security")),
     ?_assertEqual(Refused, Strict("acme/security")),
     % not as a deeper segment, and not for a dotfile inside it
     ?_assertEqual(Refused, WK("nested/.well-known/x")),
     ?_assertEqual(Refused, WK("nested/.well-known")),
     ?_assertEqual(Refused, WK(".well-known/.secret")),
     ?_assertEqual(Refused, WK(".well-known/.new")),
     ?_assertEqual(Refused, WK(".well-known/sub/.x")),
     % getting out of .well-known into a hidden place is still refused
     ?_assertEqual(Refused, WK(".well-known/../.git/config")),
     ?_assertEqual(In("inside.txt"), WK(".well-known/../inside.txt")),
     % only segments below the root count: a root under a dot name is fine
     ?_assertEqual({ok, filename:join(CanonDotRoot, "f.txt")},
                   confine("f.txt", DotRoot)),
     ?_assertEqual(Refused, confine(".x", DotRoot))].
