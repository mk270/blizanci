%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_tmpdir).

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([tmp_file_name/0, create/1, create/2, stale/2]).
-export_type([name_fun/0]).

-type filepath() :: binary() | string().
-type name_fun() :: fun(() -> string()).

% How many times create/2 will try a fresh name before giving up.
-define(ATTEMPTS, 10).

% Every temporary file blizanci creates starts with this, so that
% stale/2 can tell them apart from anything else in the directory.
-define(PREFIX, "blizanci-tmp-").

% A fresh name for a temporary file: the operating system's process id
% (so two instances sharing a directory differ), the time (so one run
% differs from the next) and a number unique within this run.
-spec tmp_file_name() -> Name
              when Name :: string().
tmp_file_name() ->
    lists:flatten(io_lib:format("~s~s-~B-~B",
                                [?PREFIX, os:getpid(),
                                 erlang:system_time(microsecond),
                                 erlang:unique_integer([positive])])).


% Create a new, empty temporary file in 'Dir', open for writing, and
% return it with its path. The file is created exclusively: if the name
% is already taken (by anything, including a symbolic link) it is left
% alone and a fresh name is tried, so one upload can never truncate
% another's file, nor be redirected by a link planted at the name.
-spec create(Dir) -> Result
              when Dir    :: filepath(),
                   Result :: {ok, file:io_device(), filepath()}
                             | {error, term()}.
create(Dir) ->
    create(Dir, fun tmp_file_name/0).


-spec create(Dir, NameFun) -> Result
              when Dir     :: filepath(),
                   NameFun :: name_fun(),
                   Result  :: {ok, file:io_device(), filepath()}
                              | {error, term()}.
create(Dir, NameFun) ->
    create(Dir, NameFun, ?ATTEMPTS).


-spec create(Dir, NameFun, Attempts) -> Result
              when Dir      :: filepath(),
                   NameFun  :: name_fun(),
                   Attempts :: non_neg_integer(),
                   Result   :: {ok, file:io_device(), filepath()}
                               | {error, term()}.
create(_Dir, _NameFun, 0) ->
    {error, eexist};
create(Dir, NameFun, Attempts) ->
    Path = filename:join(Dir, NameFun()),
    case file:open(Path, [write, exclusive]) of
        {ok, Stream} -> {ok, Stream, Path};
        {error, eexist} -> create(Dir, NameFun, Attempts - 1);
        {error, _} = Error -> Error
    end.


% Return the temporary files blizanci made in a directory 'Dir' that were
% last modified more than 'MaxAge' seconds ago. Only regular files whose
% names carry our prefix are ever returned, so pointing the work
% directory at a directory with other things in it can't get those
% deleted.
-spec stale(Dir, MaxAge) -> Result
              when Dir    :: filepath(),
                   MaxAge :: integer(),
                   Result :: [filepath()].
stale(Dir, MaxAge) ->
    {ok, Files} = file:list_dir_all(Dir),
    Candidates = [ filename:join(Dir, F) || F <- Files, is_temp_name(F) ],
    [ F || F <- Candidates, is_old_regular_file(F, MaxAge) ].


-spec is_temp_name(Name) -> Result
              when Name   :: filepath(),
                   Result :: boolean().
is_temp_name(Name) ->
    string:prefix(Name, ?PREFIX) =/= nomatch.


% Doesn't follow symbolic links: a link is never one of our temp files.
-spec is_old_regular_file(Filename, MaxAge) -> Result
              when Filename :: filepath(),
                   MaxAge   :: integer(),
                   Result   :: boolean().
is_old_regular_file(Filename, MaxAge) ->
    case file:read_link_info(Filename) of
        {ok, #file_info{type = regular, mtime = Mtime}} ->
            older_than(Mtime, MaxAge);
        _ ->
            false
    end.


% Modification time is used, not access time, so an upload that is still
% being written to isn't mistaken for a left-over. A time in the future
% is not stale: file times come from the operating system's clock (or a
% file server's) and the current time from Erlang's, which can differ
% by a little, and a file that has only just been written must not be
% taken for a left-over.
-spec older_than(Mtime, MaxAge) -> Result
              when Mtime   :: calendar:datetime(),
                   MaxAge  :: integer(),
                   Result  :: boolean().
older_than(Mtime, MaxAge) ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    Then = calendar:datetime_to_gregorian_seconds(Mtime),
    Now - Then >= MaxAge.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
