%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_path).

-export([fix_path/1, path_under_root/2, confine/2]).

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
%% @end
-spec confine(SubPath, Root) -> Result
              when SubPath :: string(),
                   Root    :: string(),
                   Result  :: {ok, string()}
                            | {error, file_not_found}.

confine(SubPath, Root) ->
    AbsRoot = filename:absname(Root),
    case fix_path(AbsRoot) of
        {ok, CanonRoot} -> confine2(SubPath, AbsRoot, CanonRoot);
        {error, Reason} ->
            logger:warning("root path resolution failed for ~p: ~p",
                           [AbsRoot, Reason]),
            {error, file_not_found}
    end.


-spec confine2(SubPath, AbsRoot, CanonRoot) -> Result
              when SubPath   :: string(),
                   AbsRoot   :: string(),
                   CanonRoot :: string(),
                   Result    :: {ok, string()}
                              | {error, file_not_found}.

confine2(SubPath, AbsRoot, CanonRoot) ->
    case fix_path(filename:join(AbsRoot, SubPath)) of
        {ok, Cmd} ->
            case path_under_root(Cmd, CanonRoot) of
                true ->
                    {ok, Cmd};
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
    Base.

confine_teardown(Base) ->
    file:del_dir_r(Base).

confine_tests(Base) ->
    Docs = filename:join(Base, "docs"),
    {ok, CanonDocs} = fix_path(Docs),
    [?_assertEqual({ok, filename:join(CanonDocs, "inside.txt")},
                   confine("inside.txt", Docs)),
     ?_assertEqual({error, file_not_found},
                   confine("../outside/secret.txt", Docs)),
     % regression: sibling directory sharing the root's name as a prefix
     ?_assertEqual({error, file_not_found},
                   confine("../docs-secret/secret.txt", Docs)),
     ?_assertEqual({error, file_not_found},
                   confine("escape/secret.txt", Docs))].
