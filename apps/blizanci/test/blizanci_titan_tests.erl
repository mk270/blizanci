%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_titan_tests).

-include_lib("eunit/include/eunit.hrl").
-include("blizanci_types.hrl").
-include_lib("kernel/include/file.hrl").
-include("blizanci_titan.hrl").

%% The records and types of the header are not all used here.
-compile(nowarn_unused_type).

-import(blizanci_titan,
        [create_tmp_file/2, delete/1, discard/1, finish_file/1,
         handle_upload_status/1, parse_titan_request/1, recv_data/2,
         serve/4]).
-import(blizanci_test_support, [test_request/1]).

parse_titan_request_test_() ->
    P = fun(Params) ->
            parse_titan_request(<<"dir/page;", Params/binary>>)
        end,
    [?_assertEqual({ok, #titan_request{path= <<"dir/page">>, size=123,
                            mime= <<"text/plain">>}},
                   P(<<"size=123;mime=text/plain">>)),
     ?_assertEqual({ok, #titan_request{path= <<"dir/page">>, size=0,
                            mime= <<"text/plain">>}},
                   P(<<"size=0;mime=text/plain">>)),
     % mime is optional and defaults to text/gemini
     ?_assertEqual({ok, #titan_request{path= <<"dir/page">>, size=5,
                            mime= <<"text/gemini">>}},
                   P(<<"size=5">>)),
     ?_assertEqual({error, bad_query_string},
                   P(<<"size=-1;mime=text/plain">>)),
     ?_assertEqual({error, bad_query_string},
                   P(<<"size=-0123456789;mime=text/plain">>)),
     % the limit itself is allowed; one byte more is not
     ?_assertMatch({ok, #titan_request{size=?MAX_UPLOAD_BYTES}},
                   P(list_to_binary(
                       "size=" ++ integer_to_list(?MAX_UPLOAD_BYTES)))),
     ?_assertEqual({error, request_too_long},
                   P(list_to_binary(
                       "size=" ++ integer_to_list(?MAX_UPLOAD_BYTES + 1)))),
     ?_assertEqual({error, request_too_long},
                   P(<<"size=1000000000000000000000000000000">>)),
     ?_assertEqual({error, bad_query_string},
                   P(<<"size=abc;mime=text/plain">>)),
     ?_assertEqual({error, bad_query_string},
                   P(<<"size=;mime=text/plain">>)),
     ?_assertEqual({error, bad_query_string},
                   P(<<"mime=text/plain">>)),
     ?_assertEqual({error, bad_query_string},
                   parse_titan_request(<<"dir/page">>))].


%% finish_file/1 resolves the target afresh, so a path that has become
%% unsafe since the request was checked is refused, not written.
%% Fixture, under a fresh temporary directory:
%%   Base/root/existing.txt  Base/root/escape -> ../outside  Base/outside
%%   Base/work
finish_file_test_() ->
    {setup, fun finish_setup/0, fun finish_teardown/1, fun finish_tests/1}.

finish_setup() ->
    Base = blizanci_test_support:tmp_path("titan_test"),
    [ ok = filelib:ensure_path(filename:join(Base, D))
      || D <- ["root", "outside", "work"] ],
    ok = file:write_file(filename:join([Base, "root", "existing.txt"]),
                         <<"old">>),
    ok = file:make_symlink("../outside",
                           filename:join([Base, "root", "escape"])),
    Base.

finish_teardown(Base) ->
    file:del_dir_r(Base).

finish_tests(Base) ->
    Root = list_to_binary(filename:join(Base, "root")),
    Work = list_to_binary(filename:join(Base, "work")),
    Upload = fun(Path, Data) ->
                 {ok, Stream, Tmp} = create_tmp_file(Work, Data),
                 {finish_file(#titan_upload{stream = Stream, path = Path,
                                              root_dir = Root, tmp_file = Tmp,
                                              size = byte_size(Data)}),
                  Tmp}
             end,
    InRoot = fun(Name) -> filename:join([Base, "root", Name]) end,
    [?_test(begin
                {Status, Tmp} = Upload(<<"new.txt">>, <<"hello">>),
                ?assertEqual(titan_finished, Status),
                ?assertEqual({ok, <<"hello">>},
                             file:read_file(InRoot("new.txt"))),
                ?assertNot(filelib:is_file(Tmp))
            end),
     ?_test(begin
                {Status, _} = Upload(<<"existing.txt">>, <<"new">>),
                ?assertEqual(titan_finished, Status),
                ?assertEqual({ok, <<"new">>},
                             file:read_file(InRoot("existing.txt")))
            end),
     % a directory on the path now leads out of the root
     ?_test(begin
                {Status, Tmp} = Upload(<<"escape/pwned.txt">>, <<"x">>),
                ?assertEqual(titan_not_found, Status),
                ?assertNot(filelib:is_file(
                             filename:join([Base, "outside", "pwned.txt"]))),
                ?assertNot(filelib:is_file(Tmp))
            end),
     % the race itself: the request-time check passes while "swap" is a
     % real directory, then it is replaced by a link out of the root
     % before the upload finishes
     ?_test(begin
                Swap = InRoot("swap"),
                ok = filelib:ensure_path(Swap),
                ?assertMatch({ok, _},
                             blizanci_path:confine(<<"swap/f.txt">>, Root)),
                ok = file:del_dir(Swap),
                ok = file:make_symlink("../outside", Swap),
                {Status, _} = Upload(<<"swap/f.txt">>, <<"x">>),
                ?assertEqual(titan_not_found, Status),
                ?assertNot(filelib:is_file(
                             filename:join([Base, "outside", "f.txt"])))
            end),
     ?_assertEqual({gateway_finished, {error_code, file_not_found}},
                   handle_upload_status(titan_not_found))].


%% A failed install leaves the old page alone and the temporary file
%% removed, whichever way the upload arrived. Fixture, under a fresh
%% temporary directory:
%%   Base/root/{old.txt,dir/inner.txt}  Base/work
%% and, where /dev/shm is a different filesystem, Shm/work (the
%% work_dir of the cross-device case, which is how a link from the work
%% dir into the docroot fails with exdev).
failed_install_test_() ->
    {setup, fun failed_setup/0, fun failed_teardown/1, fun failed_tests/1}.

failed_setup() ->
    Base = blizanci_test_support:tmp_path("titan_failed_test"),
    [ ok = filelib:ensure_path(filename:join(Base, D))
      || D <- ["root/dir", "work"] ],
    ok = file:write_file(filename:join([Base, "root", "old.txt"]), <<"old">>),
    ok = file:write_file(filename:join([Base, "root", "dir", "inner.txt"]),
                         <<"inner">>),
    Shm = filename:join("/dev/shm", filename:basename(Base)),
    {Base, other_filesystem(Base, Shm)}.

% {ok, Dir} when a directory can be made on a filesystem that files in
% Base cannot be hard-linked from, else unavailable.
other_filesystem(Base, Shm) ->
    Link = filename:join(Base, "probe"),
    Result = case filelib:ensure_path(Shm) of
                 ok ->
                     ShmProbe = filename:join(Shm, "probe"),
                     ok = file:write_file(ShmProbe, <<>>),
                     case file:make_link(ShmProbe, Link) of
                         {error, exdev} -> {ok, Shm};
                         _ -> file:del_dir_r(Shm), unavailable
                     end;
                 _ -> unavailable
             end,
    file:delete(Link),
    Result.

failed_teardown({Base, Other}) ->
    file:del_dir_r(Base),
    case Other of
        {ok, Shm} -> file:del_dir_r(Shm);
        unavailable -> ok
    end.

failed_tests({Base, Other}) ->
    Root = filename:join(Base, "root"),
    Cfg = #server_config{hostname = <<"h">>, port = 1965,
                         routing = []},
    Serve = fun(Work, Fragment, Rest) ->
                Opts = #{ docroot => list_to_binary(Root),
                          work_dir => list_to_binary(Work) },
                serve(#{ <<"PATH">> => Fragment },
                      test_request(#{ rest_of_input => Rest }), Cfg, Opts)
            end,
    Work = filename:join(Base, "work"),
    Read = fun(Name) -> file:read_file(filename:join(Root, Name)) end,
    % nothing but the files the fixture made: no temporary or staging
    % file in the work dir, and none beside the target
    Clean = fun(WorkDir) ->
                ?assertEqual({ok, []}, file:list_dir(WorkDir)),
                {ok, Names} = file:list_dir(Root),
                ?assertEqual(["dir", "old.txt"], lists:sort(Names))
            end,
    Upload = fun(WorkDir, Path, Data) ->
                 {ok, Stream, Tmp} =
                     create_tmp_file(list_to_binary(WorkDir), Data),
                 finish_file(#titan_upload{stream = Stream, path = Path,
                                           root_dir = list_to_binary(Root),
                                           tmp_file = Tmp,
                                           size = byte_size(Data)})
             end,
    [% the target is a directory: it is not overwritten, and the upload
     % is refused with the status for "cannot overwrite"
     ?_test(begin
                ?assertEqual(titan_enotdir, Upload(Work, <<"dir">>, <<"x">>)),
                ?assertEqual({ok, <<"inner">>}, Read("dir/inner.txt")),
                Clean(Work)
            end),
     ?_test(begin
                ?assertEqual({gateway_finished,
                              {error_code, cannot_overwrite}},
                             Serve(Work, <<"dir;size=3">>, <<"new">>)),
                ?assertEqual({ok, <<"inner">>}, Read("dir/inner.txt")),
                Clean(Work)
            end),
     % a file where a directory is needed: refused, temporary file removed
     ?_test(begin
                Status = Upload(Work, <<"old.txt/sub">>, <<"x">>),
                ?assert(lists:member(Status,
                                     [titan_enotdir, titan_not_found])),
                ?assertEqual({ok, <<"old">>}, Read("old.txt")),
                Clean(Work)
            end),
     % a failure part-way through is cleaned up, then re-raised
     ?_test(begin
                Job = #titan_job{request = #titan_request{
                                              path = <<"new.txt">>,
                                              size = 1,
                                              mime = <<"text/plain">>},
                                 rest = <<"x">>,
                                 work_dir = list_to_binary(Work),
                                 root_dir = <<>>},
                % a root_dir that is not a path, made past the type
                Bad = setelement(#titan_job.root_dir, Job, not_a_path),
                ?assertError(_, apply(blizanci_titan,
                                      handle_all_in_one_request,
                                      [Bad])),
                Clean(Work)
            end)]
    ++ exdev_tests(Other, Serve, Upload, Read, Clean).

% The work dir is on another filesystem from the docroot, so the link
% from one to the other fails (exdev): the old page must survive, the
% temporary file must go, and the client sees a server error.
exdev_tests(unavailable, _Serve, _Upload, _Read, _Clean) ->
    [];
exdev_tests({ok, Shm}, Serve, Upload, Read, Clean) ->
    Work = filename:join(Shm, "work"),
    [?_test(begin
                ok = filelib:ensure_path(Work),
                ?assertEqual(titan_failed,
                             Upload(Work, <<"old.txt">>, <<"new">>)),
                ?assertEqual({ok, <<"old">>}, Read("old.txt")),
                Clean(Work)
            end),
     ?_test(begin
                ok = filelib:ensure_path(Work),
                ?assertEqual({gateway_finished,
                              {error_code, internal_server_error}},
                             Serve(Work, <<"old.txt;size=3">>, <<"new">>)),
                ?assertEqual({ok, <<"old">>}, Read("old.txt")),
                Clean(Work)
            end),
     % and the same for a page that does not exist yet
     ?_test(begin
                ok = filelib:ensure_path(Work),
                ?assertEqual(titan_failed,
                             Upload(Work, <<"fresh.txt">>, <<"new">>)),
                ?assertEqual({error, enoent}, Read("fresh.txt")),
                Clean(Work)
            end),
     ?_assertEqual({gateway_finished, {error_code, internal_server_error}},
                   handle_upload_status(titan_failed))].


%% size=0 deletes the resource (see delete_resource/2). Driven through
%% serve/4, so path confinement and parsing are exercised too.
%% Fixture, under a fresh temporary directory:
%%   Base/root/{a.txt,b.txt,c.txt,dir/inner.txt,escape -> ../outside}
%%   Base/root/{.env,.git/config,.well-known/old}
%%   Base/outside/secret.txt  Base/work
delete_test_() ->
    {setup, fun delete_setup/0, fun delete_teardown/1, fun delete_tests/1}.

delete_setup() ->
    Base = blizanci_test_support:tmp_path("titan_delete_test"),
    [ ok = filelib:ensure_path(filename:join(Base, D))
      || D <- ["root/dir", "root/.git", "root/.well-known", "outside",
               "work"] ],
    [ ok = file:write_file(filename:join([Base, "root", F]), <<"x">>)
      || F <- ["a.txt", "b.txt", "c.txt", "dir/inner.txt", ".env",
               ".git/config", ".well-known/old"] ],
    ok = file:write_file(filename:join([Base, "outside", "secret.txt"]),
                         <<"secret">>),
    ok = file:make_symlink("../outside",
                           filename:join([Base, "root", "escape"])),
    Base.

delete_teardown(Base) ->
    file:del_dir_r(Base).

delete_tests(Base) ->
    Root = filename:join(Base, "root"),
    Opts = #{ docroot => list_to_binary(Root),
              work_dir => list_to_binary(Base ++ "/work") },
    Cfg = #server_config{hostname = <<"h">>, port = 1965,
                         routing = []},
    Serve = fun(Fragment, Rest) ->
                serve(#{ <<"PATH">> => Fragment },
                      test_request(#{ rest_of_input => Rest }), Cfg, Opts)
            end,
    Exists = fun(Name) -> filelib:is_file(filename:join(Root, Name)) end,
    [?_test(begin
                ?assert(Exists("a.txt")),
                ?assertEqual({gateway_finished,
                              {success, <<"text/plain">>,
                               <<"Deleted.\r\n">>}},
                             Serve(<<"a.txt;size=0;mime=text/plain">>, <<>>)),
                ?assertNot(Exists("a.txt"))
            end),
     % mime is optional here as elsewhere
     ?_test(begin
                ?assertMatch({gateway_finished, {success, _, _}},
                             Serve(<<"b.txt;size=0">>, <<>>)),
                ?assertNot(Exists("b.txt"))
            end),
     % already gone
     ?_assertEqual({gateway_finished, {error_code, file_not_found}},
                   Serve(<<"a.txt;size=0">>, <<>>)),
     ?_assertEqual({gateway_finished, {error_code, file_not_found}},
                   Serve(<<"no/such/file;size=0">>, <<>>)),
     % never a directory, nor the root itself: not a file, so not found
     ?_test(begin
                ?assertEqual({gateway_finished,
                              {error_code, file_not_found}},
                             Serve(<<"dir;size=0">>, <<>>)),
                ?assert(Exists("dir/inner.txt"))
            end),
     ?_assertEqual({gateway_finished, {error_code, file_not_found}},
                   Serve(<<";size=0">>, <<>>)),
     % a zero-size request that carries data sends more than it
     % declared: refused, and nothing is deleted
     ?_test(begin
                ?assertEqual({gateway_finished,
                              {error_code, request_too_long}},
                             Serve(<<"c.txt;size=0">>, <<"junk">>)),
                ?assert(Exists("c.txt"))
            end),
     % confined like an upload: can't reach outside the root
     ?_test(begin
                ?assertEqual({gateway_finished,
                              {error_code, file_not_found}},
                             Serve(<<"escape/secret.txt;size=0">>, <<>>)),
                ?assert(filelib:is_file(
                          filename:join([Base, "outside", "secret.txt"])))
            end),
     ?_assertEqual({gateway_finished, {error_code, file_not_found}},
                   Serve(<<"../outside/secret.txt;size=0">>, <<>>))].


%% An over-large upload is refused before anything is read or created.
size_cap_test_() ->
    {setup, fun delete_setup/0, fun delete_teardown/1,
     fun(Base) ->
         Root = filename:join(Base, "root"),
         Work = filename:join(Base, "work"),
         Opts = #{ docroot => list_to_binary(Root),
                   work_dir => list_to_binary(Work) },
         Cfg = #server_config{hostname = <<"h">>, port = 1965,
                              routing = []},
         Big = list_to_binary("big.txt;size=" ++
                              integer_to_list(?MAX_UPLOAD_BYTES + 1)),
         ?_test(begin
                    ?assertEqual({gateway_finished,
                                  {error_code, request_too_long}},
                                 serve(#{ <<"PATH">> => Big },
                                       test_request(
                                         #{ rest_of_input =>
                                                <<"some data">> }),
                                       Cfg, Opts)),
                    ?assertEqual({ok, []}, file:list_dir(Work)),
                    ?assertNot(filelib:is_file(
                                 filename:join(Root, "big.txt")))
                end)
     end}.


%% An upload that sends more than it declared is refused with a bad
%% request, nothing is installed (an existing file is left as it was),
%% and the data received is deleted.
excess_data_test_() ->
    {setup, fun delete_setup/0, fun delete_teardown/1, fun excess_tests/1}.

excess_tests(Base) ->
    Root = filename:join(Base, "root"),
    Work = list_to_binary(filename:join(Base, "work")),
    Opts = #{ docroot => list_to_binary(Root), work_dir => Work },
    Cfg = #server_config{hostname = <<"h">>, port = 1965,
                         routing = []},
    Serve = fun(Fragment, Rest) ->
                serve(#{ <<"PATH">> => Fragment },
                      test_request(#{ rest_of_input => Rest }), Cfg, Opts)
            end,
    TooLong = {gateway_finished, {error_code, request_too_long}},
    Inner = filename:join([Root, "dir", "inner.txt"]),
    % a state as the queued worker would hold it part-way through
    State = fun(Path, Size, Received) ->
                {ok, Stream, Tmp} = blizanci_tmpdir:create(Work),
                ok = file:write(Stream, binary:copy(<<"y">>, Received)),
                {#titan_state{bytes_recv = Received,
                              upload = #titan_upload{
                                          size = Size, tmp_file = Tmp,
                                          path = Path,
                                          root_dir = list_to_binary(Root),
                                          stream = Stream}}, Tmp}
            end,
    [% data sent with the request line
     ?_test(begin
                ?assertEqual(TooLong,
                             Serve(<<"new.txt;size=3">>, <<"abcdef">>)),
                ?assertNot(filelib:is_file(filename:join(Root, "new.txt"))),
                ?assertEqual({ok, []}, file:list_dir(Work))
            end),
     ?_test(begin
                ?assertEqual({ok, <<"x">>}, file:read_file(Inner)),
                ?assertEqual(TooLong,
                             Serve(<<"dir/inner.txt;size=3">>, <<"abcdef">>)),
                ?assertEqual({ok, <<"x">>}, file:read_file(Inner))
            end),
     % exactly the declared size is fine
     ?_assertMatch({gateway_finished, {success, _, _}},
                   Serve(<<"exact.txt;size=3">>, <<"abc">>)),
     % excess arriving later, to a queued upload
     ?_test(begin
                {St, Tmp} = State(<<"late.txt">>, 3, 0),
                ?assertEqual(titan_too_much, recv_data(<<"abcdef">>, St)),
                ?assertNot(filelib:is_file(Tmp)),
                ?assertNot(filelib:is_file(filename:join(Root, "late.txt")))
            end),
     ?_test(begin
                {St, Tmp} = State(<<"dir/inner.txt">>, 3, 2),
                ?assertEqual(titan_too_much, recv_data(<<"cd">>, St)),
                ?assertNot(filelib:is_file(Tmp)),
                ?assertEqual({ok, <<"x">>}, file:read_file(Inner))
            end),
     % chunks that add up exactly to the declared size are fine
     ?_test(begin
                {St, _Tmp} = State(<<"chunks.txt">>, 4, 2),
                ?assertEqual(titan_finished, recv_data(<<"zz">>, St)),
                ?assertEqual({ok, <<"yyzz">>},
                             file:read_file(filename:join(Root,
                                                          "chunks.txt")))
            end),
     ?_test(begin
                {St, _Tmp} = State(<<"part.txt">>, 4, 1),
                ?assertEqual({titan_updated, 3}, recv_data(<<"zz">>, St))
            end),
     ?_assertEqual(TooLong, handle_upload_status(titan_too_much))].


%% Hidden files can't be created, overwritten or deleted, whether the
%% data comes with the request or later; the one exception is real files
%% directly under a ".well-known" directory at the top of the root.
hidden_files_test_() ->
    {setup, fun delete_setup/0, fun delete_teardown/1, fun hidden_tests/1}.

hidden_tests(Base) ->
    Root = filename:join(Base, "root"),
    Work = list_to_binary(filename:join(Base, "work")),
    Opts = #{ docroot => list_to_binary(Root), work_dir => Work },
    Cfg = #server_config{hostname = <<"h">>, port = 1965,
                         routing = []},
    Serve = fun(Fragment, Rest) ->
                serve(#{ <<"PATH">> => Fragment },
                      test_request(#{ rest_of_input => Rest }), Cfg, Opts)
            end,
    NotFound = {gateway_finished, {error_code, file_not_found}},
    Read = fun(Name) -> file:read_file(filename:join(Root, Name)) end,
    Exists = fun(Name) -> filelib:is_file(filename:join(Root, Name)) end,
    [% creating
     ?_test(begin
                ?assertEqual(NotFound, Serve(<<".evil;size=3">>, <<"abc">>)),
                ?assertNot(Exists(".evil"))
            end),
     ?_test(begin
                ?assertEqual(NotFound,
                             Serve(<<".git/pwn;size=3">>, <<"abc">>)),
                ?assertNot(Exists(".git/pwn"))
            end),
     % overwriting
     ?_test(begin
                ?assertEqual(NotFound, Serve(<<".env;size=3">>, <<"new">>)),
                ?assertEqual({ok, <<"x">>}, Read(".env"))
            end),
     % deleting
     ?_test(begin
                ?assertEqual(NotFound, Serve(<<".git/config;size=0">>, <<>>)),
                ?assert(Exists(".git/config"))
            end),
     % a dotfile inside .well-known is still hidden
     ?_assertEqual(NotFound, Serve(<<".well-known/.x;size=3">>, <<"abc">>)),
     % but real files there can be written and deleted
     ?_test(begin
                ?assertMatch({gateway_finished, {success, _, _}},
                             Serve(<<".well-known/security.txt;size=3">>,
                                   <<"abc">>)),
                ?assertEqual({ok, <<"abc">>},
                             Read(".well-known/security.txt"))
            end),
     ?_test(begin
                ?assertMatch({gateway_finished, {success, _, _}},
                             Serve(<<".well-known/old;size=0">>, <<>>)),
                ?assertNot(Exists(".well-known/old"))
            end),
     % the same rule applies when the upload is finished later (the
     % target is resolved again then)
     ?_test(begin
                {ok, Stream, Tmp} = create_tmp_file(Work, <<"x">>),
                ?assertEqual(titan_not_found,
                             finish_file(#titan_upload{
                                           stream = Stream,
                                           path = <<".late">>,
                                           root_dir = list_to_binary(Root),
                                           tmp_file = Tmp, size = 1})),
                ?assertNot(Exists(".late")),
                ?assertNot(filelib:is_file(Tmp))
            end)].


%% A temporary file that can't be removed must not crash the upload: the
%% outcome is the same and the failure is only logged.
delete_failure_test_() ->
    {setup, fun delete_setup/0, fun delete_failure_teardown/1,
     fun delete_failure_tests/1}.

delete_failure_teardown(Base) ->
    _ = file:write_file_info(filename:join(Base, "lock"),
                             #file_info{mode = 8#755}),
    delete_teardown(Base).

delete_failure_tests(Base) ->
    Root = filename:join(Base, "root"),
    Lock = filename:join(Base, "lock"),
    ok = filelib:ensure_path(Lock),
    IsRoot = string:trim(os:cmd("id -u")) =:= "0",
    Open = fun(Name) ->
               Tmp = filename:join(Lock, Name),
               {ok, Stream} = file:open(Tmp, [write, raw, binary]),
               {Stream, Tmp}
           end,
    [?_assertEqual(ok, delete(filename:join(Base, "no/such/file"))),
     ?_assertEqual({error, enotdir},
                   delete(filename:join([Root, "a.txt", "x"]))),
     % a directory can't be deleted as a file
     ?_assertMatch({error, E} when E =:= eisdir; E =:= eperm,
                   delete(filename:join(Root, "dir"))),
     ?_assertEqual(ok, discard(filename:join(Root, "dir"))),
     % eacces: nothing in a read-only directory can be removed
     ?_test(case IsRoot of
                true -> ok;
                false ->
                    {S1, T1} = Open("t1"),
                    {S2, T2} = Open("t2"),
                    ok = file:write_file_info(Lock,
                                              #file_info{mode = 8#555}),
                    ?assertEqual({error, eacces}, delete(T1)),
                    ?assertEqual(ok, discard(T1)),
                    % target not found: the upload is refused cleanly
                    ?assertEqual(titan_not_found,
                                 finish_file(#titan_upload{
                                               stream = S1,
                                               path = <<".nope">>,
                                               root_dir =
                                                   list_to_binary(Root),
                                               tmp_file = T1, size = 0})),
                    % more data than declared: refused cleanly
                    ?assertEqual(titan_too_much,
                                 recv_data(<<"abc">>,
                                           #titan_state{
                                              bytes_recv = 0,
                                              upload = #titan_upload{
                                                          stream = S2,
                                                          size = 1,
                                                          path = <<"x">>,
                                                          root_dir = Root,
                                                          tmp_file = T2}})),
                    ok = file:write_file_info(Lock,
                                              #file_info{mode = 8#755}),
                    ?assert(filelib:is_file(T1))
            end)].
