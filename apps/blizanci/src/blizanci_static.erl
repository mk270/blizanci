%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_static).
-behaviour(blizanci_servlet).

-include("blizanci_types.hrl").

-export([serve/3, cancel/1, request/3]).

-define(INDEX, "index.gemini").

cancel(_) ->
    ok.

% Called by the servlet
%
-spec request(binary(), request_details(), server_config()) ->
                     {'immediate', gemini_response()} |
                     'defer'.
request(Path = <<"restricted/", _Rest/binary>>, Req, Config) ->
    {immediate,
     serve_file(Path, Req, Config#server_config.docroot, restricted)};
request(Path = <<"private/", _Rest/binary>>, Req, Config) ->
    {immediate,
     serve_file(Path, Req, Config#server_config.docroot, private)};
request(Path, Req, Config) ->
    {immediate,
     serve_file(Path, Req, Config#server_config.docroot, public)}.


serve(_, _, _) ->
    {gateway_error, unimplemented}.

% private: certificate must be signed by a particular CA
% restricted: certificate must be presented
% public: no certificate requirement
-spec serve_file(binary(), map(), string(), authorisation())
                -> gemini_response().
serve_file(Path, _Req, Docroot, public) ->
    serve_file(Path, Docroot);
serve_file(Path, Req, Docroot, Auth) ->
    #{ client_cert := Cert } = Req,
    CertInfo = blizanci_x509:peercert_cn(Cert),
    serve_restricted_file(Path, Docroot, Auth, CertInfo).


-spec serve_restricted_file(binary(), string(), authorisation(),
                            any()) ->
                                   gemini_response().
serve_restricted_file(_Path, _Docroot, _Auth, error) ->
    {error_code, cert_required};
serve_restricted_file(Path, Docroot, Auth, {ok, CertInfo}) ->
    #{ common_name := Subject,
       issuer_common_name := Issuer } = CertInfo,
    lager:info("~p object requested, peercert: ~p/~p", [Auth, Subject, Issuer]),
    serve_file(Path, Docroot).


% If there's a valid file requested, then get its full path, so that
% it can be sendfile()'d back to the client. If it's a directory, redirect
% to an index file.
-spec serve_file(binary(), string()) -> gemini_response().
serve_file(Path, Docroot) ->
    Full = filename:join(Docroot, Path),
    case {filelib:is_dir(Full), filelib:is_regular(Full)} of
        {true, _} ->
            Redirect = filename:join(Path, ?INDEX),
            {redirect, Redirect};
        {false, true} ->
            MimeType = mime_type(Full),
            {file, MimeType, Full};
        _ ->
            {error_code, file_not_found}
    end.


% Look up the MIME type for a given filename. If the filename doesn't contain
% a ".", then assume it's text/gemini. If it contains a "." but isn't in
% the MIME types dataset, then assume it's application/octet-stream.
-spec mime_type(binary()) -> binary().
mime_type(Path) when is_binary(Path) ->
    case binary_to_list(filename:extension(Path)) of
        [] -> <<"text/gemini">>;
        [_Dot|Rest] -> Key = erlang:list_to_binary(Rest),
                      case mime_lookup:lookup(Key) of
                          notfound -> <<"application/octet-stream">>;
                          {ok, Result} -> Result
                      end
    end.
