%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_static).

-moduledoc """
A servlet serving static files from a route's docroot. Its request/4
answers immediately, never deferring to a servlet process: a request
for a regular file yields a `{file, MimeType, Path}` response, a request
for a directory is redirected to that directory's index file, and
anything else (including paths that escape the docroot) is a
file_not_found. Paths are canonicalised and confined by blizanci_path.
""".
-behaviour(blizanci_servlet).

-include("blizanci_types.hrl").

-export([serve/4, cancel/1, request/4, default_options/0,
         validate_options/1, handle_client_data/2, start/0, stop/0]).

-type options() :: #{ bare_mimetype := binary(),
                      unknown_mimetype := binary(),
                      docroot := binary(),
                      index := string()
                    }.


-define(INDEX,              "index.gemini").
-define(BARE_MIMETYPE,    <<"text/gemini">>).
-define(UNKNOWN_MIMETYPE, <<"application/octet-stream">>).


-spec default_options() -> Result
              when Result :: map().
default_options() ->
    #{ index            => ?INDEX,
       unknown_mimetype => ?UNKNOWN_MIMETYPE,
       bare_mimetype    => ?BARE_MIMETYPE
     }.


-spec validate_options(Opts) -> Result
              when Opts   :: map(),
                   Result :: 'ok' | {'error', term()}.
validate_options(#{ docroot          := Docroot,
                    index            := Index,
                    bare_mimetype    := BareMimetype,
                    unknown_mimetype := UnknownMimetype })
  when is_binary(Docroot), is_list(Index), Index =/= "",
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

% Called by blizanci_servlet_container, in the connection process
%
-spec request(Matches, Req, ServerConfig, RouteOpts) -> Result
              when Matches      :: path_matches(),
                   Req          :: request_details(),
                   ServerConfig :: server_config(),
                   RouteOpts    :: options(),
                   Result       :: {'immediate', gemini_response()} | 'defer'.

request(Matches, _Req, _ServerConfig, RouteOpts) ->
    Response = serve_file(Matches, RouteOpts),
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
-spec serve_file(Matches, Opts) -> Result
              when Matches :: path_matches(),
                   Opts    :: options(),
                   Result  :: gemini_response().

serve_file(Matches, Opts) ->
    #{ <<"PATH">> := Path } = Matches,
    Docroot = maps:get(docroot, Opts),
    % hidden files are never served, except under a ".well-known"
    % directory at the top of the docroot
    case blizanci_path:resolve_request(Matches, Docroot,
                                       #{ well_known => true }) of
        {error, file_not_found} -> {error_code, file_not_found};
        {ok, Cmd} ->
            % a redirect is to the whole requested path, which under a
            % mount is more than PATH
            Whole = maps:get(<<"FULL_PATH">>, Matches, Path),
            serve_confined_file(Cmd, Whole, Opts)
    end.


-spec serve_confined_file(Full, Path, Opts) -> Result
              when Full   :: binary(),
                   Path   :: binary(),
                   Opts   :: options(),
                   Result :: gemini_response().

serve_confined_file(Full, Path, Opts) ->
    case {filelib:is_dir(Full), filelib:is_regular(Full)} of
        {true, _} ->
            Index = maps:get(index, Opts),
            Redirect = filename:join(Path, Index),
            {redirect, Redirect};
        {false, true} ->
            BareMimeType = maps:get(bare_mimetype, Opts),
            UnkMimeType = maps:get(unknown_mimetype, Opts),
            MimeType = mime_type(Full, BareMimeType, UnkMimeType),
            {file, MimeType, Full};
        _ ->
            {error_code, file_not_found}
    end.


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
                          {ok, Result} -> Result;
                          notfound -> UnknownMimeType
                      end
    end.


-spec handle_client_data(Pid, Binary) -> Result
              when Pid    :: pid(),
                   Binary :: binary(),
                   Result :: gemini_response().
handle_client_data(_, _) -> none.


-spec start() -> Result
              when Result :: 'ok' | {'error', term()}.
start() ->
    ok.


-spec stop() -> Result
              when Result :: ok.
stop() ->
    ok.
