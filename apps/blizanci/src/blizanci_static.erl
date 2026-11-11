%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

% TBD: module-level docs

-module(blizanci_static).
-behaviour(blizanci_servlet).

-include("blizanci_types.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([serve/4, cancel/1, request/4, default_options/0,
         validate_options/1, handle_client_data/2, start/0, stop/0]).

-type options() :: #{ bare_mimetype := binary(),
                      unknown_mimetype := binary(),
                      docroot := string(),
                      index := string()
                    }.


-define(INDEX,              "index.gemini").
-define(BARE_MIMETYPE,    <<"text/gemini">>).
-define(UNKNOWN_MIMETYPE, <<"application/octet-stream">>).
-define(DEFAULT_DOCROOT,  <<"./public_gemini">>).


-spec default_options() -> Result
              when Result :: map().
default_options() ->
    #{ index            => "index.gemini",
       unknown_mimetype => <<"application/octet-stream">>,
       bare_mimetype    => <<"text/gemini">>
     }.


-spec validate_options(Opts) -> Result
              when Opts   :: map(),
                   Result :: 'ok' | {'error', term()}.
validate_options(#{ docroot          := Docroot,
                    index            := Index,
                    bare_mimetype    := BareMimetype,
                    unknown_mimetype := UnknownMimetype })
  when is_list(Docroot), is_list(Index), Index =/= "",
       is_binary(BareMimetype), is_binary(UnknownMimetype) ->
    case filelib:is_dir(filename:absname(Docroot)) of
        true -> ok;
        false -> {error, {docroot_not_a_directory, Docroot}}
    end;
validate_options(Opts) ->
    {error, {invalid_options, Opts}}.

-spec cancel(Pid) -> Result
              when Pid    :: pid(),
                   Result :: ok.
cancel(Pid) when is_pid(Pid) ->
    ok.

% Called by the servlet
%
-spec request(Matches, Req, ServerConfig, RouteOpts) -> Result
              when Matches      :: path_matches(),
                   Req          :: request_details(),
                   ServerConfig :: server_config(),
                   RouteOpts    :: options(),
                   Result       :: {'immediate', gemini_response()} | 'defer'.

request(Matches, _Req, _ServerConfig, RouteOpts) ->
    #{ <<"PATH">> := Path } = Matches,
    Response = serve_file(Path, RouteOpts),
    {immediate, Response}.


-spec serve(Matches, Req, ServerConfig, RouteOpts) -> Result
              when Matches      :: path_matches(),
                   Req          :: request_details(),
                   ServerConfig :: server_config(),
                   RouteOpts    :: options(),
                   Result       :: gateway_result().
serve(_, _, _, _) ->
    {gateway_error, unimplemented}.


% If there's a valid file requested, then get its full path, so that
% it can be sendfile()'d back to the client. If it's a directory, redirect
% to an index file.
%
% Path is resolved via blizanci_path:confine/2 (the same canonicalise-
% and-confine mechanism CGI and Titan use) rather than a bare
% filename:join/2, so a symlink inside Docroot can't be used to read
% or redirect to a file outside it.
-spec serve_file(Path, Opts) -> Result
              when Path   :: binary(),
                   Opts   :: options(),
                   Result :: gemini_response().

serve_file(Path, Opts) ->
    Docroot = ensure_list(maps:get(docroot, Opts, ?DEFAULT_DOCROOT)),
    % hidden files are never served, except under a ".well-known"
    % directory at the top of the docroot
    case blizanci_path:confine(Path, Docroot, #{ well_known => true }) of
        {error, file_not_found} -> {error_code, file_not_found};
        {ok, Cmd} -> serve_confined_file(Cmd, Path, Opts)
    end.


-spec serve_confined_file(Full, Path, Opts) -> Result
              when Full   :: binary(),
                   Path   :: binary(),
                   Opts   :: options(),
                   Result :: gemini_response().

serve_confined_file(Full, Path, Opts) ->
    case {filelib:is_dir(Full), filelib:is_regular(Full)} of
        {true, _} ->
            Index = maps:get(index, Opts, ?INDEX),
            Redirect = filename:join(Path, Index),
            {redirect, Redirect};
        {false, true} ->
            BareMimeType = maps:get(bare_mimetype, Opts, ?BARE_MIMETYPE),
            UnkMimeType = maps:get(unknown_mimetype, Opts, ?UNKNOWN_MIMETYPE),
            MimeType = mime_type(Full, BareMimeType, UnkMimeType),
            {file, MimeType, Full};
        _ ->
            {error_code, file_not_found}
    end.


-spec ensure_list(Input) -> Result
              when Input  :: list()
                           | binary(),
                   Result :: list().
ensure_list(X) when is_binary(X) -> binary_to_list(X);
ensure_list(X) when is_list(X)   -> X.


% Look up the MIME type for a given filename. If the filename doesn't contain
% a ".", then assume it's text/gemini. If it contains a "." but isn't in
% the MIME types dataset, then assume it's application/octet-stream.
-spec mime_type(Path, BareMimeType, UnknownMimeType) -> Result
              when Path            :: binary(),
                   BareMimeType    :: binary(),
                   UnknownMimeType :: binary(),
                   Result          :: binary().

mime_type(Path, BareMimeType, UnknownMimeType)
  when is_binary(Path)
       and is_binary(BareMimeType)
       and is_binary(UnknownMimeType) ->
    case binary_to_list(filename:extension(Path)) of
        [] -> BareMimeType;
        [_Dot|Rest] -> Key = erlang:list_to_binary(Rest),
                      case mime_lookup:lookup(Key) of
                          {ok, Result} when is_binary(Result) -> Result;
                          notfound -> UnknownMimeType;
                          _ -> UnknownMimeType
                      end
    end.


-spec handle_client_data(Pid, Binary) -> Result
              when Pid    :: pid(),
                   Binary :: binary(),
                   Result :: gemini_response().
handle_client_data(_, _) -> none.


-spec start() -> Result
              when Result :: ok.
start() ->
    ok.


-spec stop() -> Result
              when Result :: ok.
stop() ->
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    Unique = integer_to_list(erlang:unique_integer([positive])),
    Base = list_to_binary(filename:absname("static_test_" ++ Unique)),
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
    Docs = binary_to_list(filename:join(Base, <<"docs">>)),
    Opts = maps:merge(default_options(), #{ docroot => Docs }),
    Request = fun(Path) ->
                  request(#{ <<"PATH">> => Path }, #{}, none, Opts)
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
    Unique = integer_to_list(erlang:unique_integer([positive])),
    Base = filename:absname("static_hidden_test_" ++ Unique),
    Docs = filename:join(Base, "docs"),
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
    Docs = filename:join(Base, "docs"),
    Opts = maps:merge(default_options(), #{ docroot => Docs }),
    Request = fun(Path) ->
                  request(#{ <<"PATH">> => Path }, #{}, none, Opts)
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
