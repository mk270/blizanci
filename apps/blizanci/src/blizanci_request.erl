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
    try
        ReqHost = maps:get(host, URI, <<>>),
        Path    = maps:get(path, URI, <<"/">>),
        Scheme  = maps:get(scheme, URI),
        ReqPort = maps:get(port, URI, Config#server_config.port),
        ReqPath = case Path of
                      <<>> -> <<"/">>;
                      P -> P
                  end,

        SchemeLC  = strings:lowercase(Scheme),
        ReqHostLC = strings:lowercase(ReqHost),

        Query = maps:get(query, URI, <<>>),
        #{ scheme        => SchemeLC,
           host          => ReqHostLC,
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

    HostLC     = string:lowercase(Host),
    ConfHostLC = string:lowercase(ConfHost),

    if HostLC =/= ConfHostLC -> {error_code, host_unrecognised};
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


handle_line_test_data(Docroot) ->
    ExpectedFile = filename:join(filename:absname(Docroot),
                                 "dir1/index.gemini"),
    [
     {{file, <<"text/gemini">>, list_to_binary(ExpectedFile)},
      <<"gemini://this.host.dev/dir1/index.gemini">>},

     {{redirect, <<"dir1/index.gemini">>},
      <<"gemini://this.host.dev/dir1/">>},

     {{redirect, <<"dir1/index.gemini">>},
      <<"gemini://this.host.dev:1965/dir1/">>}
    ].

handle_line_test_() ->
    {
     setup,
     fun setup/0,
     fun teardown/1,
     fun tests/1
    }.

-define(APP, mime_lookup).

setup() ->
    {ok, _} = application:ensure_all_started(?APP).

teardown(_State) ->
    ok = application:stop(?APP).

tests(_State) ->
    Docroot = "test-docroot",
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

    HandleLine = fun(TestInput) ->
                         handle_line(TestInput, ServerConfig, Cert, <<"">>)
                 end,

    [[?_assertEqual(Expected, HandleLine(Arg))
      || {Expected, Arg} <- handle_line_test_data(Docroot)]].


%% Request URLs, checked against the Gemini specification. These need no
%% real file: a request that passes the URL checks and then names a file
%% that doesn't exist is answered with file_not_found, which tells the
%% two outcomes apart without involving MIME lookup.
-define(NOT_FOUND, {error_code, file_not_found}).

url_handler() ->
    Opts = #{ docroot => "test-docroot" },
    Routes = [{gemini, <<"(?<PATH>.*)">>, blizanci_static, public, Opts}],
    {ok, Routing} = blizanci_router:prepare(Routes),
    Config = #server_config{hostname = <<"this.host.dev">>,
                            port = 1965,
                            routing = Routing,
                            docroot = "test-docroot"},
    fun(Url) ->
        handle_line(Url, Config, {error, no_peercert}, <<>>)
    end.

%% A server MUST reject a request with a fragment, with status 59.
fragment_test_() ->
    H = url_handler(),
    [?_assertEqual({error_code, request_not_parsed},
                   H(<<"gemini://this.host.dev/nothere#frag">>)),
     ?_assertEqual({error_code, request_not_parsed},
                   H(<<"gemini://this.host.dev/nothere#">>)),
     ?_assertEqual({error_code, request_not_parsed},
                   H(<<"gemini://this.host.dev/#top">>)),
     ?_assertEqual({error_code, request_not_parsed},
                   H(<<"gemini://this.host.dev/nothere?q=1#frag">>)),
     ?_assertEqual({error_code, request_not_parsed},
                   H(<<"titan://this.host.dev/x;size=1#frag">>)),
     % an encoded "#" is part of the path, not a fragment
     ?_assertEqual(?NOT_FOUND, H(<<"gemini://this.host.dev/a%23b">>)),
     ?_assertEqual(?NOT_FOUND, H(<<"gemini://this.host.dev/nothere">>)),
     ?_assertEqual(?NOT_FOUND, H(<<"gemini://this.host.dev/nothere?q=1">>))].

%% A request is an absolute URI, so it must have a scheme: the server
%% mustn't assume one.
scheme_required_test_() ->
    H = url_handler(),
    [?_assertEqual({error_code, request_not_parsed},
                   H(<<"//this.host.dev/nothere">>)),
     ?_assertEqual({error_code, request_not_parsed},
                   H(<<"this.host.dev/nothere">>)),
     ?_assertEqual({error_code, request_not_parsed}, H(<<"/nothere">>)),
     ?_assertEqual({error_code, request_not_parsed}, H(<<"nothere">>)),
     ?_assertEqual({error_code, request_not_parsed}, H(<<>>)),
     % with a scheme, the request is handled as before
     ?_assertEqual(?NOT_FOUND, H(<<"gemini://this.host.dev/nothere">>)),
     ?_assertEqual(?NOT_FOUND, H(<<"gemini://this.host.dev:1965/nothere">>))].

%% The path of a URL is case-sensitive (RFC 3986) and must be left alone,
%% and a host that isn't ours is refused whatever its case.
case_sensitive_parts_test_() ->
    H = url_handler(),
    [?_assertEqual({error_code, host_unrecognised},
                   H(<<"gemini://other.host.dev/nothere">>)),
     ?_assertEqual({error_code, host_unrecognised},
                   H(<<"gemini://OTHER.HOST.DEV/nothere">>)),
     % there is no file "DIR1" (only "dir1")
     ?_assertEqual(?NOT_FOUND,
                   H(<<"gemini://this.host.dev/DIR1/index.gemini">>))].

%% A host name with a trailing dot ("this.host.dev.") is deliberately not
%% accepted as ours: it stays refused, whatever else changes about how
%% hosts are compared.
trailing_dot_host_refused_test_() ->
    H = url_handler(),
    [?_assertEqual({error_code, host_unrecognised},
                   H(<<"gemini://this.host.dev./nothere">>)),
     ?_assertEqual({error_code, host_unrecognised},
                   H(<<"gemini://this.host.dev.:1965/nothere">>)),
     ?_assertEqual({error_code, host_unrecognised},
                   H(<<"gemini://THIS.HOST.DEV./nothere">>))].

%% PENDING, so compiled out: the scheme and the host of a URL are
%% case-insensitive (RFC 3986), so a request that spells them in another
%% case is the same request. Blizanci doesn't do this yet: today these
%% tests fail ("GEMINI://" gets 59, "THIS.HOST.DEV" gets 53).
%%
%% To enable them, either delete the -ifdef and -endif lines around them,
%% or build with the macro defined, e.g. put
%%     {eunit_compile_opts, [{d, 'PENDING_CASE_INSENSITIVE'}]}.
%% on a line of its own in rebar.config (the file may lack a final
%% newline) and touch this file, since rebar3 won't notice the change.
%% Once the code is fixed, delete the -ifdef/-endif lines for good. The
%% matching SNI tests are in blizanci_config.erl, under the same macro.
-ifdef(PENDING_CASE_INSENSITIVE).
case_insensitive_scheme_and_host_test_() ->
    H = url_handler(),
    [?_assertEqual(?NOT_FOUND, H(<<"GEMINI://this.host.dev/nothere">>)),
     ?_assertEqual(?NOT_FOUND, H(<<"Gemini://this.host.dev/nothere">>)),
     ?_assertEqual(?NOT_FOUND, H(<<"gemini://THIS.HOST.DEV/nothere">>)),
     ?_assertEqual(?NOT_FOUND, H(<<"gemini://This.Host.Dev:1965/nothere">>)),
     ?_assertEqual(?NOT_FOUND, H(<<"GEMINI://THIS.HOST.DEV/nothere">>))].
-endif.


%% Percent-escapes in the path are decoded, exactly once, after the path
%% structure is known; the result must be printable ASCII, and must not
%% contain a "/" that was escaped. Fixture, under a fresh temporary
%% directory:
%%   Base/docs/{"a b", "100%", "%2e", "semi;colon", ".env",
%%              "sp ace/plain"}
%%   Base/titan/{"my file.txt", "x"}
%% Files have no extension, so no MIME lookup is involved.
percent_decoding_test_() ->
    {setup, fun percent_setup/0, fun percent_teardown/1,
     fun percent_tests/1}.

percent_setup() ->
    Unique = integer_to_list(erlang:unique_integer([positive])),
    Base = filename:absname("percent_test_" ++ Unique),
    ok = filelib:ensure_path(filename:join(Base, "docs/sp ace")),
    ok = filelib:ensure_path(filename:join(Base, "titan")),
    ok = filelib:ensure_path(filename:join(Base, "work")),
    [ ok = file:write_file(filename:join([Base, "docs", F]), <<"x">>)
      || F <- ["a b", "100%", "%2e", "semi;colon", ".env",
               "sp ace/plain"] ],
    [ ok = file:write_file(filename:join([Base, "titan", F]), <<"x">>)
      || F <- ["my file.txt", "x"] ],
    Base.

percent_teardown(Base) ->
    file:del_dir_r(Base).

percent_tests(Base) ->
    Docs = filename:join(Base, "docs"),
    Titan = filename:join(Base, "titan"),
    Routes = [{gemini, <<"(?<PATH>.*)">>, blizanci_static, public,
               #{ docroot => Docs }},
              {titan, <<"(?<PATH>.*)">>, blizanci_titan, public,
               #{ docroot => Titan,
                  work_dir => list_to_binary(filename:join(Base, "work")) }}],
    {ok, Routing} = blizanci_router:prepare(Routes),
    Config = #server_config{hostname = <<"this.host.dev">>, port = 1965,
                            routing = Routing, docroot = Docs},
    H = fun(Path) ->
            handle_line(<<"gemini://this.host.dev/", Path/binary>>,
                        Config, {error, no_peercert}, <<>>)
        end,
    T = fun(Path) ->
            handle_line(<<"titan://this.host.dev/", Path/binary>>,
                        Config, {error, no_peercert}, <<>>)
        end,
    Malformed = {error_code, request_not_parsed},
    BadName = {error_code, bad_filename},
    [% escapes are decoded: a space, a percent sign, upper or lower case
     ?_assertMatch({file, _, _}, H(<<"a%20b">>)),
     ?_assertMatch({file, _, _}, H(<<"100%25">>)),
     ?_assertMatch({file, _, _}, H(<<"semi%3Bcolon">>)),
     ?_assertMatch({file, _, _}, H(<<"semi%3bcolon">>)),
     ?_assertEqual({redirect, <<"sp ace/index.gemini">>},
                   H(<<"sp%20ace/">>)),
     ?_assertMatch({file, _, _}, H(<<"sp%20ace/plain">>)),
     % decoded once only: %252e is the text "%2e", a file that exists
     ?_assertMatch({file, _, _}, H(<<"%252e">>)),
     ?_assertEqual(?NOT_FOUND, H(<<"%252e%252e/x">>)),
     % an escape for "/" would make a new separator: refused, either case
     ?_assertEqual(BadName, H(<<"a%2Fb">>)),
     ?_assertEqual(BadName, H(<<"a%2fb">>)),
     % NUL, control characters, DEL
     ?_assertEqual(BadName, H(<<"a%00b">>)),
     ?_assertEqual(BadName, H(<<"a%0Ab">>)),
     ?_assertEqual(BadName, H(<<"a%09b">>)),
     ?_assertEqual(BadName, H(<<"a%7Fb">>)),
     % anything that decodes to a non-ASCII byte: valid UTF-8 or not
     ?_assertEqual(BadName, H(<<"%C3%A9.txt">>)),
     ?_assertEqual(BadName, H(<<"caf%E9">>)),
     ?_assertEqual(BadName, H(<<"%FF">>)),
     % malformed escapes
     ?_assertEqual(Malformed, H(<<"a%zzb">>)),
     ?_assertEqual(Malformed, H(<<"a%4">>)),
     ?_assertEqual(Malformed, H(<<"a%">>)),
     ?_assertEqual(Malformed, H(<<"%">>)),
     ?_assertEqual(Malformed, H(<<"%2">>)),
     ?_assertEqual(Malformed, H(<<"a%G0">>)),
     % dot segments, escaped or not, cannot climb out, and a hidden name
     % written with an escape is still hidden
     ?_assertEqual(?NOT_FOUND, H(<<"%2e%2e/x">>)),
     ?_assertEqual(?NOT_FOUND, H(<<"%2E%2E/%2E%2E/x">>)),
     ?_assertEqual(?NOT_FOUND, H(<<"%2Eenv">>)),
     ?_assertEqual(?NOT_FOUND, H(<<"sp%20ace/%2e%2e/%2eenv">>)),
     % the query is not touched
     ?_assertMatch({file, _, _}, H(<<"a%20b?x=%zz">>)),
     % Titan: the parameters are split off the raw path before the file
     % name is decoded
     % (Titan runs as a separate process, so what is checked is its effect:
     % the file with the decoded name is the one deleted)
     ?_test(begin
                ?assertMatch({init_servlet, _},
                             T(<<"my%20file.txt;size=0">>)),
                ?assert(gone(filename:join(Titan, "my file.txt")))
            end),
     % an escaped ";" cannot forge a parameter, nor be part of the name
     ?_assertEqual(BadName, T(<<"x%3Bsize=0;size=0">>)),
     ?_assertEqual(BadName, T(<<"x%3bsize=0;size=0">>)),
     ?_assert(filelib:is_file(filename:join(Titan, "x"))),
     ?_assertEqual(Malformed, T(<<"x%zz;size=0">>)),
     ?_assertEqual(BadName, T(<<"x%2Fy;size=0">>)),
     % the parameters after the first ";" are the client's own, undecoded
     % here (Titan reads them)
     ?_test(begin
                ?assertMatch({init_servlet, _},
                             T(<<"x;size=0;mime=text/plain">>)),
                ?assert(gone(filename:join(Titan, "x")))
            end)].

% Wait (up to two seconds) for a file to be gone.
gone(Path) ->
    gone(Path, 40).

gone(Path, 0) ->
    not filelib:is_file(Path);
gone(Path, N) ->
    case filelib:is_file(Path) of
        false -> true;
        true -> timer:sleep(50), gone(Path, N - 1)
    end.
