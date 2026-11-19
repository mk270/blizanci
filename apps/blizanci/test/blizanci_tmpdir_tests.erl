%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_tmpdir_tests).

-include_lib("eunit/include/eunit.hrl").
-include("blizanci_tmpdir.hrl").

-import(blizanci_tmpdir,
        [create/1, create/2, is_temp_name/1, older_than/2, stale/2,
         tmp_file_name/0]).


tmp_file_name_test() ->
    Name = tmp_file_name(),
    ?assert(is_temp_name(Name)),
    ?assertMatch({match, _},
                 re:run(Name, "^blizanci-tmp-[0-9]+-[0-9]+-[0-9]+$")),
    ?assertNotEqual(Name, tmp_file_name()),
    ?assertEqual(1000, length(lists:usort(
                               [ tmp_file_name()
                                 || _ <- lists:seq(1, 1000) ]))).

%% Fixture: a directory containing a file and a symbolic link that
%% sit at names create/2 will be offered first.
create_test_() ->
    {setup, fun create_setup/0, fun create_teardown/1, fun create_tests/1}.

create_setup() ->
    Dir = blizanci_test_support:tmp_path("tmpdir_create_test"),
    ok = filelib:ensure_path(Dir),
    ok = file:write_file(filename:join(Dir, "taken"), <<"precious">>),
    ok = file:write_file(filename:join(Dir, "victim"), <<"victim">>),
    ok = file:make_symlink(filename:join(Dir, "victim"),
                           filename:join(Dir, "planted")),
    Dir.

create_teardown(Dir) ->
    file:del_dir_r(Dir).

% A name function that offers the given names in turn, then repeats the
% last.
names(Names) ->
    Ref = make_ref(),
    put(Ref, Names),
    fun() ->
        case get(Ref) of
            [Last] -> Last;
            [Next | Rest] -> put(Ref, Rest), Next
        end
    end.

create_tests(Dir) ->
    Read = fun(Name) -> file:read_file(filename:join(Dir, Name)) end,
    [?_test(begin
                {ok, S, P} = create(Dir),
                ok = file:write(S, <<"data">>),
                ok = file:close(S),
                ?assertEqual({ok, <<"data">>}, file:read_file(P)),
                ?assert(is_temp_name(filename:basename(P)))
            end),
     % a taken name is skipped, and the existing file left alone
     ?_test(begin
                {ok, S, P} = create(Dir, names(["taken", "fresh1"])),
                ok = file:close(S),
                ?assertEqual(filename:join(Dir, "fresh1"), P),
                ?assertEqual({ok, <<"precious">>}, Read("taken"))
            end),
     % a symbolic link planted at the name is not followed
     ?_test(begin
                {ok, S, P} = create(Dir, names(["planted", "fresh2"])),
                ok = file:write(S, <<"redirected?">>),
                ok = file:close(S),
                ?assertEqual(filename:join(Dir, "fresh2"), P),
                ?assertEqual({ok, <<"victim">>}, Read("victim"))
            end),
     % binary directory names, as the Titan work_dir is
     ?_test(begin
                {ok, S, P} = create(list_to_binary(Dir)),
                ok = file:close(S),
                ?assert(is_binary(P))
            end),
     % give up rather than loop forever
     ?_assertEqual({error, eexist}, create(Dir, names(["taken"]))),
     ?_assertMatch({error, enoent},
                   create(filename:join(Dir, "no/such/dir")))].

%% Only our own old regular files are stale: not recent ones, not other
%% people's files (including numeric names, as blizanci used to make),
%% not directories or symlinks that happen to carry the prefix.
stale_test_() ->
    {setup, fun stale_setup/0, fun stale_teardown/1, fun stale_tests/1}.

stale_setup() ->
    Dir = blizanci_test_support:tmp_path("tmpdir_test"),
    ok = filelib:ensure_path(Dir),
    Old = {{2000, 1, 1}, {0, 0, 0}},
    Make = fun(Name) ->
               Path = filename:join(Dir, Name),
               ok = file:write_file(Path, <<"x">>),
               ok = file:change_time(Path, Old),
               Path
           end,
    Mine = Make(?PREFIX ++ "1"),
    Recent = filename:join(Dir, ?PREFIX ++ "2"),
    ok = file:write_file(Recent, <<"x">>),
    Make("notes.txt"),
    Make("12345"),
    ok = filelib:ensure_path(filename:join(Dir, ?PREFIX ++ "dir")),
    ok = file:change_time(filename:join(Dir, ?PREFIX ++ "dir"), Old),
    ok = file:make_symlink(Mine, filename:join(Dir, ?PREFIX ++ "link")),
    {Dir, Mine}.

stale_teardown({Dir, _Mine}) ->
    file:del_dir_r(Dir).

stale_tests({Dir, Mine}) ->
    [?_assertEqual([Mine], stale(Dir, 3600)),
     % the work directory is a binary in practice
     ?_assertEqual([list_to_binary(Mine)],
                   stale(list_to_binary(Dir), 3600)),
     ?_assertEqual([], stale(Dir, 3600 * 24 * 365 * 100))].

older_than_test_() ->
    Now = calendar:local_time(),
    Ago = fun(Secs) ->
              calendar:gregorian_seconds_to_datetime(
                calendar:datetime_to_gregorian_seconds(Now) - Secs)
          end,
    [?_assert(older_than(Ago(7200), 3600)),
     ?_assertNot(older_than(Ago(60), 3600)),
     % more than a day old with a small time-of-day remainder: old
     ?_assert(older_than(Ago(86400 * 2 + 60), 3600)),
     % a time in the future is not stale, however far ahead
     ?_assertNot(older_than(Ago(-1), 3600)),
     ?_assertNot(older_than(Ago(-3600), 3600)),
     ?_assertNot(older_than(Ago(-86400 * 365), 3600))].
