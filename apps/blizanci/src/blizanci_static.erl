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

-export([serve/3, cancel/1, request/3]).

-type options() :: #{ bare_mimetype := binary(),
                      unknown_mimetype := binary(),
                      docroot := string(),
                      index := string(),
                      authorisation := 'public' | 'restricted' | 'secret'
                    }.

-define(INDEX, "index.gemini").
-define(BARE_MIMETYPE, <<"text/gemini">>).
-define(UNKNOWN_MIMETYPE, <<"application/octet-stream">>).
-define(DEFAULT_DOCROOT, <<"./public_gemini">>).

-spec cancel(pid()) -> ok.
cancel(_) ->
    ok.

% Called by the servlet
%
-spec request(path_matches(), request_details(), options()) ->
                         {'immediate', gemini_response()} |
                         'defer'.
request(Matches, Req, Options) ->
    #{ <<"PATH">> := Path } = Matches,
    {ok, Auth} = blizanci_auth:authorisation_policy(Options),
    Response = case blizanci_auth:authorised(Auth, Req) of
                   authorised -> serve_file(Path, Options);
                   Error -> Error
               end,
    {immediate, Response}.


-spec serve(path_matches(), request_details(), options()) ->
                   gateway_result().
serve(_, _, _) ->
    {gateway_error, unimplemented}.


% If there's a valid file requested, then get its full path, so that
% it can be sendfile()'d back to the client. If it's a directory, redirect
% to an index file.
-spec serve_file(binary(), options()) -> gemini_response().
serve_file(Path, Opts) ->
    Docroot = maps:get(docroot, Opts, ?DEFAULT_DOCROOT),
    Full = filename:join(Docroot, Path),
    case {filelib:is_dir(Full), filelib:is_regular(Full)} of
        {true, _} ->
            Index = maps:get(index, Opts, ?INDEX),
            Redirect = filename:join(Path, Index),
            {redirect, Redirect};
        {false, true} ->
            MimeType = mime_type(Full, Opts),
            {file, MimeType, Full};
        _ ->
            {error_code, file_not_found}
    end.


% Look up the MIME type for a given filename. If the filename doesn't contain
% a ".", then assume it's text/gemini. If it contains a "." but isn't in
% the MIME types dataset, then assume it's application/octet-stream.
-spec mime_type(binary(), options()) -> binary().
mime_type(Path, Opts) when is_binary(Path) ->
    case binary_to_list(filename:extension(Path)) of
        [] -> maps:get(bare_mimetype, Opts, ?BARE_MIMETYPE);
        [_Dot|Rest] -> Key = erlang:list_to_binary(Rest),
                      case mime_lookup:lookup(Key) of
                          notfound -> maps:get(unknown_mimetype, Opts,
                                               ?UNKNOWN_MIMETYPE);
                          {ok, Result} -> Result
                      end
    end.
