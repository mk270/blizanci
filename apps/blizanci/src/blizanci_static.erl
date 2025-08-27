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

-export([serve/4, cancel/1, request/4, default_options/0,
         handle_client_data/2]).

-type options() :: #{ bare_mimetype := binary(),
                      unknown_mimetype := binary(),
                      docroot := string(),
                      index := string()
                    }.


-define(INDEX,              "index.gemini").
-define(BARE_MIMETYPE,    <<"text/gemini">>).
-define(UNKNOWN_MIMETYPE, <<"application/octet-stream">>).
-define(DEFAULT_DOCROOT,  <<"./public_gemini">>).


-spec default_options() -> map().
default_options() ->
    #{ index            => "index.gemini",
       unknown_mimetype => <<"application/octet-stream">>,
       bare_mimetype    => <<"text/gemini">>
     }.

-spec cancel(pid()) -> ok.
cancel(Pid) when is_pid(Pid) ->
    ok.

% Called by the servlet
%
-spec request(path_matches(), request_details(), server_config(), options()) ->
                         {'immediate', gemini_response()} |
                         'defer'.
request(Matches, _Req, _ServerConfig, RouteOpts) ->
    #{ <<"PATH">> := Path } = Matches,
    Response = serve_file(Path, RouteOpts),
    {immediate, Response}.


-spec serve(path_matches(), request_details(), server_config(), options()) ->
                   gateway_result().
serve(_, _, _, _) ->
    {gateway_error, unimplemented}.


% If there's a valid file requested, then get its full path, so that
% it can be sendfile()'d back to the client. If it's a directory, redirect
% to an index file.
-spec serve_file(Path, Opts) -> Response
              when Path     :: binary(),
                   Opts     :: options(),
                   Response :: gemini_response().

serve_file(Path, Opts) ->
    Docroot = maps:get(docroot, Opts, ?DEFAULT_DOCROOT),
    Full = filename:join(Docroot, Path),
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


-spec handle_client_data(Pid, Binary) -> Response
              when Pid      :: pid(),
                   Binary   :: binary(),
                   Response :: gemini_response().
handle_client_data(_, _) -> none.
