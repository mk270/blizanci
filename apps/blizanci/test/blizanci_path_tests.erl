%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_path_tests).

-include_lib("eunit/include/eunit.hrl").

-import(blizanci_path, [confine/2, confine/3, fix_path/1,
                        path_under_root/2, resolve_request/3]).

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
%%   Base/outside_<0xE9>/secret.txt        (a directory name that is not
%%                                          valid UTF-8)
%%   Base/docs/badlink -> Base/outside_<0xE9>/secret.txt  (an undecodable
%%   Base/docs/badrel -> ../outside_<0xE9>/secret.txt      target, twice)
confine_test_() ->
    {setup,
     fun confine_setup/0,
     fun confine_teardown/1,
     fun confine_tests/1}.

confine_setup() ->
    Base = blizanci_test_support:tmp_path("confine_test"),
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
    % names that are not valid UTF-8 can only be written as binaries
    BaseBin = list_to_binary(Base),
    Bad = <<BaseBin/binary, "/outside_", 233>>,
    ok = file:make_dir(Bad),
    ok = file:write_file(<<Bad/binary, "/secret.txt">>, <<"x">>),
    ok = file:make_symlink(<<Bad/binary, "/secret.txt">>,
                           <<BaseBin/binary, "/docs/badlink">>),
    ok = file:make_symlink(<<"../outside_", 233, "/secret.txt">>,
                           <<BaseBin/binary, "/docs/badrel">>),
    ok = file:write_file(<<BaseBin/binary, "/docs/", 195, 169, ".txt">>,
                         <<"x">>),
    ok = file:write_file(<<BaseBin/binary, "/docs/bad", 233>>, <<"x">>),
    ok = filelib:ensure_path(<<BaseBin/binary, "/docs/sub/", 233>>),
    ok = file:write_file(<<BaseBin/binary, "/docs/sub/", 233, "/f">>,
                         <<"x">>),
    ok = file:make_symlink(<<BaseBin/binary, "/outside/secret.txt">>,
                           <<BaseBin/binary, "/docs/", 195, 169>>),
    ok = file:make_symlink(<<BaseBin/binary, "/outside/secret.txt">>,
                           <<BaseBin/binary, "/docs/", 233>>),
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
    BaseBin = list_to_binary(Base),
    Docs = filename:join(BaseBin, <<"docs">>),
    {ok, CanonDocs} = fix_path(Docs),
    % a root with a character that is not ASCII, as the bytes the VM
    % makes of it
    Odd = filename:join(Base, [$d, 243, $c, $s]),
    % (called through apply/3 so that dialyzer does not object)
    NotBinary = fun(P) -> apply(blizanci_path, confine, [P, Docs]) end,
    StringRoot = fun(R) -> apply(blizanci_path, confine, [<<"f.txt">>, R]) end,
    OddBytes = unicode:characters_to_binary(Odd, unicode,
                                            file:native_name_encoding()),
    {ok, CanonOdd} = fix_path(OddBytes),
    [?_assertEqual({ok, filename:join(CanonDocs, "inside.txt")},
                   confine(<<"inside.txt">>, Docs)),
     % a symbolic link whose target is not valid UTF-8 is not skipped
     % over: the operating system follows it, out of the root, so it is
     % refused, whatever the VM's file-name encoding (this needs realpath
     % 1.4.0 or later, which resolves such links instead of ignoring them)
     ?_assertEqual({ok, <<"x">>},
                   file:read_file(<<BaseBin/binary, "/docs/badlink">>)),
     ?_assertEqual({ok, <<"x">>},
                   file:read_file(<<BaseBin/binary, "/docs/badrel">>)),
     ?_assertEqual({error, file_not_found}, confine(<<"badlink">>, Docs)),
     ?_assertEqual({error, file_not_found}, confine(<<"badrel">>, Docs)),
     ?_assertEqual({error, file_not_found},
                   confine(<<"../outside/secret.txt">>, Docs)),
     % regression: sibling directory sharing the root's name as a prefix
     ?_assertEqual({error, file_not_found},
                   confine(<<"../docs-secret/secret.txt">>, Docs)),
     ?_assertEqual({error, file_not_found},
                   confine(<<"escape/secret.txt">>, Docs)),
     % a NUL anywhere in the path is refused, even where the name before
     % it is fine
     ?_assertEqual({error, file_not_found},
                   confine(<<"inside.txt\0.png">>, Docs)),
     ?_assertEqual({error, file_not_found}, confine(<<"inside.txt\0">>, Docs)),
     ?_assertEqual({error, file_not_found}, confine(<<"\0inside.txt">>, Docs)),
     ?_assertEqual({error, file_not_found}, confine(<<0>>, Docs)),
     ?_assertEqual({error, file_not_found},
                   confine(<<"sub\0/../../outside/secret.txt">>, Docs)),
     ?_assertEqual({error, file_not_found},
                   confine(<<"inside.txt", 0, ".png">>, Docs)),
     ?_assertEqual({error, file_not_found},
                   confine(<<"inside.txt">>, <<Docs/binary, 0>>)),
     % control characters other than NUL are refused too
     ?_assertEqual({error, file_not_found}, confine(<<"inside\t.txt">>, Docs)),
     ?_assertEqual({error, file_not_found}, confine(<<"inside\n.txt">>, Docs)),
     ?_assertEqual({error, file_not_found}, confine(<<"inside\r.txt">>, Docs)),
     ?_assertEqual({error, file_not_found}, confine(<<"inside\e.txt">>, Docs)),
     ?_assertEqual({error, file_not_found},
                   confine(<<$a, 127, $b>>, Docs)),
     % names are bytes: confine/2 accepts anything but control characters,
     % valid UTF-8 or not, and resolves the file whatever the VM's
     % encoding. (Whether such a name can arrive in a request is up to
     % the URL layer, which does not decode percent-escapes yet.)
     ?_assertEqual({ok, filename:join(CanonDocs, <<195, 169, ".txt">>)},
                   confine(<<195, 169, ".txt">>, Docs)),
     ?_assertEqual({ok, <<"x">>},
                   file:read_file(filename:join(CanonDocs,
                                                <<195, 169, ".txt">>))),
     ?_assertEqual({ok, filename:join(CanonDocs, <<"bad", 233>>)},
                   confine(<<"bad", 233>>, Docs)),
     ?_assertEqual({ok, filename:join(CanonDocs, <<"sub/", 233, "/f">>)},
                   confine(<<"sub/", 233, "/f">>, Docs)),
     % but a link with such a name is judged by where it leads: outside
     ?_assertEqual({error, file_not_found},
                   confine(<<195, 169>>, Docs)),
     ?_assertEqual({error, file_not_found}, confine(<<233>>, Docs)),
     % only a binary is a path
     ?_assertEqual({error, file_not_found}, NotBinary("inside.txt")),
     ?_assertEqual({error, file_not_found}, NotBinary([$a, b, $c])),
     ?_assertEqual({error, file_not_found}, NotBinary(inside)),
     % printable ASCII, space included, is fine whether or not the file
     % exists
     ?_assertEqual({ok, filename:join(CanonDocs, "inside.txt")},
                   confine(<<"inside.txt">>, Docs)),
     ?_assertEqual({ok, filename:join(CanonDocs, "a b-c_d~e!.txt")},
                   confine(<<"a b-c_d~e!.txt">>, Docs)),
     ?_assertEqual({ok, filename:join(CanonDocs, "sub/new file.txt")},
                   confine(<<"sub/new file.txt">>, Docs)),
     % the root is the operator's and may be non-ASCII (but not hold a NUL)
     % a root that is a string is refused: the router makes binaries
     ?_assertEqual({error, file_not_found}, StringRoot(Odd)),
     ?_assertEqual({error, file_not_found}, StringRoot("/tmp")),
     ?_assertEqual({ok, filename:join(CanonOdd, "f.txt")},
                   confine(<<"f.txt">>, OddBytes))]
    ++ resolve_request_tests(Docs, CanonDocs)
    ++ hidden_tests(Base, Docs, CanonDocs).

%% resolve_request/3: PATH out of Matches, then confine/3.
resolve_request_tests(Docs, CanonDocs) ->
    R = fun(M, O) -> resolve_request(M, Docs, O) end,
    P = fun(Path) -> #{ <<"PATH">> => Path } end,
    Refused = {error, file_not_found},
    Inside = {ok, filename:join(CanonDocs, <<"inside.txt">>)},
    WKFile = {ok, filename:join([CanonDocs, <<".well-known">>,
                                 <<"security">>])},
    [?_assertEqual(Inside, R(P(<<"inside.txt">>), #{})),
     ?_assertEqual(Refused, R(P(<<"../outside/secret.txt">>), #{})),
     ?_assertEqual(Refused, R(P(<<"escape/secret.txt">>), #{})),
     ?_assertEqual(Refused, R(P(<<".git/config">>), #{})),
     ?_assertEqual(Refused, R(P(<<".well-known/security">>), #{})),
     ?_assertEqual(WKFile, R(P(<<".well-known/security">>),
                             #{ well_known => true })),
     % other matches are ignored; a missing PATH is a crash, as before
     ?_assertEqual(Inside, R(#{ <<"PATH">> => <<"inside.txt">>,
                                <<"PATH_INFO">> => <<"x">> }, #{})),
     ?_assertError(function_clause, R(#{}, #{}))].

%% Hidden files: a path segment beginning with a dot is refused, except
%% ".well-known" as the first segment where the caller allows it.
hidden_tests(Base, Docs, CanonDocs) ->
    Strict = fun(P) -> confine(list_to_binary(P), Docs) end,
    WK = fun(P) -> confine(list_to_binary(P), Docs,
                           #{ well_known => true }) end,
    Refused = {error, file_not_found},
    In = fun(P) -> {ok, filename:join(CanonDocs, P)} end,
    DotRoot = list_to_binary(filename:join(Base, ".dotroot")),
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
                   confine(<<"f.txt">>, DotRoot)),
     ?_assertEqual(Refused, confine(<<".x">>, DotRoot))].
