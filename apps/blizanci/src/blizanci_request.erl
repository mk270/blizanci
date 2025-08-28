%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

% The main code path consists in transforming data and handing it off
% down the call chain (always towards the bottom of the file) as follows:
%
%   handle_line(...):
%     decode URL from line (i.e., Unicode, RFC-3986 percent escaping)
%
%   handle_parsed_url(...):
%     provide defaults for missing URL parts, such as port number
%
%   handle_url(...):
%     strip out requests for non-Gemini URLs, requests to proxy, etc
%
%   handle_gemini_url(...):
%     strip out Gemini requests unrelated to this server, normalize URL
%
%   handle_path(...):
%     strip out multiple leading slashes from path component of URL
%
%
%   [the processing is handed off to a servlet, which through callbacks then
%    does something like the following:]
%
%
%   handle_file(...):
%     strip out requests involving ".."
%
%   serve_file(...):
%     redirect requests for directories to index files; otherwise return
%     file contents if file exists
%
% All calls below handle_request return a response of type gemini_response().

-module(blizanci_request).
-include_lib("eunit/include/eunit.hrl").
-include("blizanci_types.hrl").

-export([handle_line/4]).

-define(PROTO, <<"gemini">>).
-define(CRLF, <<"\r\n">>).

%% @doc
%% @hidden
%% @end

% Take the request line which has been received in full from the client
% and check that it's valid UTF8; if so, break it down into its URL parts
-spec handle_line(Cmd, Config, Cert, Rest) -> Result
              when Cmd    :: binary(),
                   Config :: server_config(),
                   Cert   :: peer_cert(),
                   Rest   :: binary(),
                   Result :: gemini_response().

handle_line(Cmd, _Config, _Cert, _Rest) when is_binary(Cmd),
                                             size(Cmd) > 1024 ->
    {error_code, request_too_long};

handle_line(Cmd, Config, Cert, Rest) when is_binary(Cmd) ->
    Recoded = unicode:characters_to_binary(<<Cmd/binary>>, utf8),
    case Recoded of
        {error, _, _}      -> {error_code, bad_unicode};
        {incomplete, _, _} -> {error_code, bad_unicode};
        S ->
            lager:debug("Request: ~p", [S]),
            case uri_string:parse(S) of
                {error, _, _} -> {error_code, request_not_parsed};
                URI -> handle_parsed_url(URI, Config, Cert, Rest)
            end
    end.

% Extract the parts of the URL, providing defaults where necessary
-spec handle_parsed_url(URI, Config, Cert, Rest) -> Result
              when URI    :: map(),
                   Config :: server_config(),
                   Cert   :: peer_cert(),
                   Rest   :: binary(),
                   Result :: gemini_response().

handle_parsed_url(#{ userinfo := _U}, _Config, _Cert, _Rest) ->
    {error_code, userinfo_supplied};
handle_parsed_url(URI, Config, Cert, Rest) ->
    try
        ReqHost = maps:get(host, URI, <<>>),
        Path    = maps:get(path, URI, <<"/">>),
        Scheme  = maps:get(scheme, URI, ?PROTO),
        ReqPort = maps:get(port, URI, Config#server_config.port),
        ReqPath = case Path of
                      <<>> -> <<"/">>;
                      P -> P
                  end,
        Query = maps:get(query, URI, <<>>),
        %% TBD: make this a struct
        #{ scheme        => Scheme,
           host          => ReqHost,
           port          => ReqPort,
           path          => ReqPath,
           query         => Query,
           client_cert   => Cert,
           rest_of_input => Rest
         }
    of
        Matches -> handle_url(Matches, Config)
    catch
        _ -> {error_code, request_not_parsed}
    end.


% Handle a request whose URL has been broken up thus:
%   [Scheme, Hostname, Port, Path]
-spec handle_url(Request, Config) -> Result
              when Request :: request_details(),
                   Config  :: server_config(),
                   Result  :: gemini_response().

handle_url(#{ scheme := <<"gopher">> }, _) -> {error_code, proxy_refused};
handle_url(#{ scheme := <<"https">>  }, _) -> {error_code, proxy_refused};
handle_url(#{ scheme := <<"http">>   }, _) -> {error_code, proxy_refused};
handle_url(#{ scheme := Scheme } = Request, Config) ->
    case protocol_supported(Scheme) of
        {true, Proto} -> handle_gemini_url(Proto, Request, Config);
        {false, _} -> lager:info("unrecognised protocol: ~p", [Request]),
                      {error_code, unrecognised_protocol}
    end.


-spec protocol_supported(Proto) -> Result
              when Proto  :: binary(),
                   Result :: {boolean(), atom()}.

protocol_supported(?PROTO)      -> {true, gemini};
protocol_supported(<<"titan">>) -> {true, titan};
protocol_supported(_)           -> {false, none}.


% Handle a request which has been determined to be a Gemini URL, but not
% necessarily one which should have come to this server (e.g., a proxy
% request)
-spec handle_gemini_url(Proto, Req, Config) -> Result
              when Proto  :: atom(),
                   Req    :: request_details(),
                   Config :: server_config(),
                   Result :: gemini_response().

handle_gemini_url(Proto,
                  Req=#{ host := Host,
                         port := Port,
                         path := Path },
                  Config=#server_config{hostname=Host, port=Port}) ->
    handle_path(Proto, uri_string:normalize(Path), Req, Config);

handle_gemini_url(_Proto, #{ host := <<>>, port := Port },
                  #server_config{port=Port}) ->
    {error_code, bad_hostname};

handle_gemini_url(_Proto, #{ port := Port },
                  #server_config{port=Port}) ->
    {error_code, host_unrecognised};

handle_gemini_url(_Proto, #{ port := ReqPort },
                  #server_config{port=Port})
  when ReqPort =/= Port ->
    {error_code, port_unrecognised};

handle_gemini_url(_, _, _) -> {error_code, host_unrecognised}.


% Strip leading slash(es) from URL
-spec handle_path(Proto, Path, Req, Config) -> Result
              when Proto  :: atom(),
                   Path   :: binary(),
                   Req    :: request_details(),
                   Config :: server_config(),
                   Result :: gemini_response().

handle_path(Proto, <<$/, Trailing/binary>>, Req, Config) ->
    handle_path(Proto, Trailing, Req, Config);
handle_path(Proto, Path, Req, Config) ->
    handle_file(Proto, Path, Req, Config).


% Deal with ".." attempts
-spec handle_file(Proto, Path, Req, Config) -> Result
              when Proto  :: atom(),
                   Path   :: binary(),
                   Req    :: request_details(),
                   Config :: server_config(),
                   Result :: gemini_response().

handle_file(Proto, Path, Req, Config) when is_binary(Path) ->
    case string:split(Path, "..") of
        [_] -> serve(Proto, Path, Req, Config);
        [_, _] -> {error_code, bad_filename}
    end.


% Separate out CGI
-spec serve(Proto, Path, Req, Config) -> Result
              when Proto  :: atom(),
                   Path   :: binary(),
                   Req    :: request_details(),
                   Config :: server_config(),
                   Result :: gemini_response().

serve(Proto, Path, Req, Config) ->
    blizanci_router:route(Proto, Path, Req, Config).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


handle_line_test_data() ->
    [
     {{file, <<"text/gemini">>, <<"/dir1/index.gemini">>},
      <<"gemini://this.host.dev/dir1/index.gemini">>},

     {{redirect, <<"dir1/index.gemini">>},
      <<"gemini://this.host.dev/dir1/">>},

     {{redirect, <<"dir1/index.gemini">>},
      <<"gemini://this.host.dev:1965/dir1/">>}
    ].

test_handle_line() ->
    Docroot = "test-docroot",
    _Opts = [{docroot, Docroot}],
    Opts = #{ docroot => Docroot },
    Routes = [{gemini, <<"(?<PATH>.*)">>, blizanci_static, public, Opts}],
    {ok, Routing} = blizanci_router:prepare(Routes),

    ServerConfig = #server_config{
                      hostname= <<"this.host.dev">>,
                      port= 1965,
                      routing=Routing,
                      docroot=Docroot
                     },
    Cert = {error, no_peercert},

    [ ?_assertEqual(Expected, handle_line(TestInput, ServerConfig,
                                          Cert, <<"">>)) ||
        {Expected, TestInput} <- handle_line_test_data() ].

handle_line_test_() ->
    App = mime_lookup,
    {
     setup,
     fun() -> {ok, _} = application:ensure_all_started(App) end,
     fun(_) -> ok = application:stop(App) end,
     fun() -> test_handle_line() end
    }.
