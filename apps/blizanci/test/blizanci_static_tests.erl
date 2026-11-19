%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_static_tests).

-include_lib("eunit/include/eunit.hrl").
-include("blizanci_types.hrl").

-import(blizanci_static, [default_options/0, request/4, validate_options/1]).
-import(blizanci_test_support, [test_request/1]).

%% A server configuration, for tests that do not use it.
-spec test_config() -> Result
              when Result :: server_config().
test_config() ->
    #server_config{hostname = <<"h">>, port = 1965, routing = []}.


%% A request path is a binary of UTF-8 bytes. A symbolic link out of the
%% docroot whose name is not ASCII must not be followable, however the
%% name is spelled: the check and the file opened must be the same name.
%% Fixture, under a fresh temporary directory (names given as binaries,
%% so they are bytes whatever the VM's file-name encoding):
%%   Base/docs/<e-acute> -> Base/outside/secret.txt   (symlink)
%%   Base/docs/caf<e-acute> -> Base/outside/secret.txt (symlink)
%%   Base/docs/readme  Base/outside/secret.txt
non_ascii_symlink_test_() ->
    {setup, fun nonascii_setup/0, fun nonascii_teardown/1,
     fun nonascii_tests/1}.

nonascii_setup() ->
    Base = list_to_binary(blizanci_test_support:tmp_path("static_test")),
    Docs = filename:join(Base, <<"docs">>),
    ok = filelib:ensure_path(Docs),
    ok = filelib:ensure_path(filename:join(Base, <<"outside">>)),
    Secret = filename:join([Base, <<"outside">>, <<"secret.txt">>]),
    ok = file:write_file(Secret, <<"SECRET">>),
    ok = file:write_file(filename:join(Docs, <<"readme">>), <<"x">>),
    ok = file:make_symlink(Secret, filename:join(Docs, <<195, 169>>)),
    ok = file:make_symlink(Secret, filename:join(Docs, <<"caf", 195, 169>>)),
    Base.

nonascii_teardown(Base) ->
    file:del_dir_r(Base).

nonascii_tests(Base) ->
    Docs = filename:join(Base, <<"docs">>),
    Opts = maps:merge(default_options(), #{ docroot => Docs }),
    Request = fun(Path) ->
                  request(#{ <<"PATH">> => Path }, test_request(#{}),
                          test_config(), Opts)
              end,
    NotFound = {immediate, {error_code, file_not_found}},
    [?_assertEqual(NotFound, Request(<<195, 169>>)),
     ?_assertEqual(NotFound, Request(<<"caf", 195, 169>>)),
     ?_assertEqual(NotFound, Request(<<"readme", 0, ".png">>)),
     % an ordinary file in the same directory is still found (it has no
     % extension, so no MIME lookup is involved)
     ?_assertMatch({immediate, {file, _, _}}, Request(<<"readme">>))].

%% Hidden files are never served, whatever their path: any segment that
%% begins with a dot is refused, except under a ".well-known" directory
%% at the top of the docroot. Files here have no extension, so no MIME
%% lookup is involved.
%% Fixture, under a fresh temporary directory:
%%   Base/docs/{readme,.env,.git/config,.well-known/{security,.secret},
%%              nested/.well-known/x}
%%   Base/docs/viewgit -> .git   (an innocent name for a hidden directory)
hidden_files_test_() ->
    {setup, fun hidden_setup/0, fun hidden_teardown/1, fun hidden_tests/1}.

hidden_setup() ->
    Base = blizanci_test_support:tmp_path("static_hidden_test"),
    Docs = filename:join(Base, <<"docs">>),
    [ begin
          Path = filename:join(Docs, Rel),
          ok = filelib:ensure_path(filename:dirname(Path)),
          ok = file:write_file(Path, <<"x">>)
      end || Rel <- ["readme", ".env", ".git/config",
                     ".well-known/security", ".well-known/.secret",
                     "nested/.well-known/x"] ],
    ok = file:make_symlink(".git", filename:join(Docs, "viewgit")),
    Base.

hidden_teardown(Base) ->
    file:del_dir_r(Base).

hidden_tests(Base) ->
    Docs = filename:join(Base, <<"docs">>),
    Opts = maps:merge(default_options(), #{ docroot => Docs }),
    Request = fun(Path) ->
                  request(#{ <<"PATH">> => Path }, test_request(#{}),
                          test_config(), Opts)
              end,
    NotFound = {immediate, {error_code, file_not_found}},
    [?_assertMatch({immediate, {file, _, _}}, Request(<<"readme">>)),
     ?_assertEqual(NotFound, Request(<<".env">>)),
     ?_assertEqual(NotFound, Request(<<".git/config">>)),
     ?_assertEqual(NotFound, Request(<<"viewgit/config">>)),
     ?_assertEqual(NotFound, Request(<<"nested/.well-known/x">>)),
     ?_assertEqual(NotFound, Request(<<".well-known/.secret">>)),
     % the one exception, at the top of the docroot
     ?_assertMatch({immediate, {file, _, _}},
                   Request(<<".well-known/security">>))].

%% A redirect to a directory's index is to the whole requested path, so
%% under a mount it keeps the mount: PATH is only what follows it.
redirect_under_mount_test_() ->
    {setup, fun hidden_setup/0, fun hidden_teardown/1,
     fun(Base) ->
         Docs = filename:join(Base, <<"docs">>),
         Opts = maps:merge(default_options(), #{ docroot => Docs }),
         Request = fun(Matches) ->
                       request(Matches, test_request(#{}),
                               test_config(), Opts)
                   end,
         [?_assertEqual({immediate, {redirect, <<"nested/index.gemini">>}},
                        Request(#{ <<"PATH">> => <<"nested">> })),
          ?_assertEqual({immediate,
                         {redirect, <<"m/nested/index.gemini">>}},
                        Request(#{ <<"PATH">> => <<"nested">>,
                                   <<"FULL_PATH">> => <<"m/nested">> })),
          ?_assertEqual({immediate, {redirect, <<"m/index.gemini">>}},
                        Request(#{ <<"PATH">> => <<>>,
                                   <<"FULL_PATH">> => <<"m">> })),
          ?_assertEqual({immediate, {redirect, <<"m/index.gemini">>}},
                        Request(#{ <<"PATH">> => <<>>,
                                   <<"FULL_PATH">> => <<"m/">> }))]
     end}.


%% default_options/0 plus a docroot is a complete, valid map.
default_options_valid_test() ->
    Opts = maps:merge(default_options(), #{ docroot => <<".">> }),
    ?assertEqual(ok, validate_options(Opts)).

%% A map lacking any one of the four required keys is refused.
missing_option_refused_test_() ->
    Full = maps:merge(default_options(), #{ docroot => <<".">> }),
    [ ?_assertMatch({error, {invalid_options, _}},
                    validate_options(maps:remove(Key, Full)))
      || Key <- [docroot, index, bare_mimetype, unknown_mimetype] ].
