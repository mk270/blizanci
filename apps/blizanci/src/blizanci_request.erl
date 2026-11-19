%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_request).

-moduledoc """
The second half of the request path: from the parsed request line
(handle_line/4) to the servlet.

See the manual chapter [Request handling](request-handling.md).
""".
-include("blizanci_types.hrl").

-export([handle_line/4]).

-ifdef(TEST).
%% Exported for the tests in test/blizanci_request_tests.erl.
-export([handle_gemini_url/3]).
-endif.

-define(PROTO, <<"gemini">>).
-define(CRLF, <<"\r\n">>).

% The longest request line (the URL) allowed, in bytes, excluding the
% CRLF. Fixed by the Gemini specification: a longer request is answered
% with 59. This is checked once the line is complete; the connection
% layer's separate ?MAX_REQUEST_BYTES guards its buffer before that.
-define(MAX_URL_BYTES, 1024).

-doc false.

% Take the request line which has been received in full from the client
% and check that it's valid UTF8; if so, break it down into its URL parts
-spec handle_line(Cmd, Config, Cert, Rest) -> Result
              when Cmd    :: binary(),
                   Config :: server_config(),
                   Cert   :: raw_peer_cert(),
                   Rest   :: binary(),
                   Result :: gemini_response().

handle_line(Cmd, _Config, _Cert, _Rest) when is_binary(Cmd),
                                             size(Cmd) > ?MAX_URL_BYTES ->
    {error_code, request_too_long};

handle_line(Cmd, Config, Cert, Rest) when is_binary(Cmd) ->
    Recoded = unicode:characters_to_binary(<<Cmd/binary>>, utf8),
    case Recoded of
        {error, _, _}      -> {error_code, bad_unicode};
        {incomplete, _, _} -> {error_code, bad_unicode};
        S ->
            case uri_string:parse(S) of
                {error, _, _} -> {error_code, request_not_parsed};
                URI -> handle_parsed_url(URI, Config, Cert, Rest)
            end
    end.

% Extract the parts of the URL, providing defaults where necessary
-spec handle_parsed_url(URI, Config, Cert, Rest) -> Result
              when URI    :: map(),
                   Config :: server_config(),
                   Cert   :: raw_peer_cert(),
                   Rest   :: binary(),
                   Result :: gemini_response().

handle_parsed_url(#{ userinfo := _U}, _Config, _Cert, _Rest) ->
    {error_code, userinfo_supplied};
% A fragment is for the client alone: the specification says clients MUST
% NOT send one and servers MUST reject a request that has one.
handle_parsed_url(#{ fragment := _F}, _Config, _Cert, _Rest) ->
    {error_code, request_not_parsed};
% A request is an absolute URI: without a scheme it isn't one, and the
% server doesn't guess which scheme was meant.
handle_parsed_url(URI, _Config, _Cert, _Rest)
  when not is_map_key(scheme, URI) ->
    {error_code, request_not_parsed};
handle_parsed_url(URI, Config, Cert, Rest) ->
    ReqHost = maps:get(host, URI, <<>>),
    Path    = maps:get(path, URI, <<"/">>),
    Scheme  = maps:get(scheme, URI),
    ReqPort = maps:get(port, URI, Config#server_config.port),
    ReqPath = case Path of
                  <<>> -> <<"/">>;
                  P -> P
              end,

    SchemeLC  = blizanci_config:ascii_lowercase(Scheme),
    ReqHostLC = blizanci_config:ascii_lowercase(ReqHost),

    Query = maps:get(query, URI, <<>>),
    handle_url(#{ scheme        => SchemeLC,
                  host          => ReqHostLC,
                  port          => ReqPort,
                  path          => ReqPath,
                  query         => Query,
                  client_cert   => Cert,
                  rest_of_input => Rest
                }, Config).


% Handle a request whose URL has been broken up thus:
%   [Scheme, Hostname, Port, Path]
%
% In particular we are concerned to filter out requests for URL schemes
% other than gemini:// and titan://, with an appropriate response
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
        {false, _}    -> {error_code, unrecognised_protocol}
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

handle_gemini_url(_Proto, #{ host := <<>>, port := Port },
                  #server_config{port=Port}) ->
    {error_code, bad_hostname};

handle_gemini_url(Proto,
                  Req=#{ host := Host,
                         port := Port,
                         path := Path },
                  Config=#server_config{hostname=ConfHost, port=Port}) ->

    % Both names are already lower case: the request host was folded
    % once, in handle_parsed_url/4, and a site's name is stored lower
    % case (blizanci_config:site/3 refuses any other), so a plain
    % comparison is a case-insensitive one.
    if Host =/= ConfHost -> {error_code, host_unrecognised};
       true -> case uri_string:normalize(Path) of
                   {error, _, _} -> {error_code, request_not_parsed};
                   Normalised    -> handle_path(Proto, Normalised, Req, Config)
               end
    end;

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


% Percent-decode the path (see blizanci_urlpath for the rules), and
% disallow URLs with a ".." in them. This is the only place a request path
% is decoded: from here on, Path is the decoded path, and must never be
% decoded again. The query is not touched.
-spec handle_file(Proto, Path, Req, Config) -> Result
              when Proto  :: atom(),
                   Path   :: binary(),
                   Req    :: request_details(),
                   Config :: server_config(),
                   Result :: gemini_response().

handle_file(Proto, Path, Req, Config) when is_binary(Path) ->
    case decode(Proto, Path) of
        {ok, Decoded} -> handle_decoded(Proto, Decoded, Req, Config);
        {error, Code} -> {error_code, Code}
    end.


-spec handle_decoded(Proto, Path, Req, Config) -> Result
              when Proto  :: atom(),
                   Path   :: binary(),
                   Req    :: request_details(),
                   Config :: server_config(),
                   Result :: gemini_response().

handle_decoded(Proto, Path, Req, Config) ->
    case string:split(Path, "..") of
        [_] -> serve(Proto, Path, Req, Config);
        [_, _] -> {error_code, bad_filename}
    end.


% A Titan path is "file;parameters": the parameters are split off the RAW
% path first, so that an escaped ";" in the file name can never be taken
% for their start, and only the file part is decoded. A decoded ";" is
% refused for Titan, since it could not be told from the delimiter if the
% path were split again. The parameters are passed on exactly as sent
% (the Titan servlet reads them).
-spec decode(Proto, Path) -> Result
              when Proto  :: atom(),
                   Path   :: binary(),
                   Result :: {ok, binary()}
                           | {error, request_not_parsed | bad_filename}.

decode(titan, Path) ->
    {File, Params} = blizanci_urlpath:split_titan(Path),
    case blizanci_urlpath:decode_path(File) of
        {ok, {decoded_path, Decoded}} ->
            case binary:match(Decoded, <<";">>) of
                nomatch when Params =:= none -> {ok, Decoded};
                nomatch -> {ok, <<Decoded/binary, ";", Params/binary>>};
                _ -> {error, bad_filename}
            end;
        {error, _} = Error -> Error
    end;
decode(_Proto, Path) ->
    case blizanci_urlpath:decode_path(Path) of
        {ok, {decoded_path, Decoded}} -> {ok, Decoded};
        {error, _} = Error -> Error
    end.


% Pass the request off to the router
-spec serve(Proto, Path, Req, Config) -> Result
              when Proto  :: atom(),
                   Path   :: binary(),
                   Req    :: request_details(),
                   Config :: server_config(),
                   Result :: gemini_response().

serve(Proto, Path, Req, Config) ->
    blizanci_router:route(Proto, Path, Req, Config).
