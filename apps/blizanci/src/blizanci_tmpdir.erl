%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_tmpdir).

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([tmp_file_name/0, stale/2]).

-type filepath() :: binary() | string().

% Every temporary file blizanci creates starts with this, so that
% stale/2 can tell them apart from anything else in the directory.
-define(PREFIX, "blizanci-tmp-").

% A fresh name for a temporary file.
-spec tmp_file_name() -> Name
              when Name :: string().
tmp_file_name() ->
    Hash = erlang:phash2(make_ref()),
    ?PREFIX ++ integer_to_list(Hash).


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
% is written off as stale.
-spec older_than(Mtime, MaxAge) -> Result
              when Mtime   :: calendar:datetime(),
                   MaxAge  :: integer(),
                   Result  :: boolean().
older_than(Mtime, MaxAge) ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    Then = calendar:datetime_to_gregorian_seconds(Mtime),
    Age = Now - Then,
    Age < 0 orelse Age >= MaxAge.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tmp_file_name_test() ->
    Name = tmp_file_name(),
    ?assert(is_temp_name(Name)),
    ?assertNotEqual(Name, tmp_file_name()).

%% Only our own old regular files are stale: not recent ones, not other
%% people's files (including numeric names, as blizanci used to make),
%% not directories or symlinks that happen to carry the prefix.
stale_test_() ->
    {setup, fun stale_setup/0, fun stale_teardown/1, fun stale_tests/1}.

stale_setup() ->
    Unique = integer_to_list(erlang:unique_integer([positive])),
    Dir = filename:absname("tmpdir_test_" ++ Unique),
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
     % a time in the future is written off as stale
     ?_assert(older_than(Ago(-3600), 3600))].
