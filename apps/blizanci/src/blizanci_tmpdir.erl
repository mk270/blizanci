%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_tmpdir).

-moduledoc """
Temporary files for Titan uploads: fresh, exclusively created files in a
work directory, and the means of finding those that have gone stale.
""".

-include_lib("kernel/include/file.hrl").
-include("blizanci_tmpdir.hrl").

-export([create/1, stale/2]).
-export_type([name_fun/0]).

-ifdef(TEST).
%% Exported for the tests in test/blizanci_tmpdir_tests.erl.
-export([is_temp_name/1, older_than/2, tmp_file_name/0, create/2]).
-endif.

-type filepath() :: binary() | string().
-type name_fun() :: fun(() -> string()).

% How many times create/2 will try a fresh name before giving up.
-define(ATTEMPTS, 10).

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
