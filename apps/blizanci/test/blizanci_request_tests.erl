%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_request_tests).

-include_lib("eunit/include/eunit.hrl").
-include("blizanci_types.hrl").

-import(blizanci_request, [handle_gemini_url/3, handle_line/4]).

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

setup() ->
    blizanci_test_support:mime_up().

teardown(Prior) ->
    blizanci_test_support:mime_down(Prior).

tests(_State) ->
    Docroot = "test-docroot",
    Opts = #{ docroot => Docroot },
    Routes = [{gemini, <<"(?<PATH>.*)">>, blizanci_static, public, Opts}],
    {ok, Routing} = blizanci_router:prepare(Routes),

    ServerConfig = #server_config{
                      hostname= <<"this.host.dev">>,
                      port= 1965,
                      routing=Routing
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
                            routing = Routing},
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

%% The scheme and the host of a URL are case-insensitive (RFC 3986), so
%% a request that spells them in another case is the same request.
case_insensitive_scheme_and_host_test_() ->
    H = url_handler(),
    [?_assertEqual(?NOT_FOUND, H(<<"GEMINI://this.host.dev/nothere">>)),
     ?_assertEqual(?NOT_FOUND, H(<<"Gemini://this.host.dev/nothere">>)),
     ?_assertEqual(?NOT_FOUND, H(<<"gemini://THIS.HOST.DEV/nothere">>)),
     ?_assertEqual(?NOT_FOUND, H(<<"gemini://This.Host.Dev:1965/nothere">>)),
     ?_assertEqual(?NOT_FOUND, H(<<"GEMINI://THIS.HOST.DEV/nothere">>))].

%% The request host is folded once, on entry, and compared with the
%% stored (lower case) site name, so any ASCII case of the site's name
%% matches it end to end; a host that differs by more than case does not.
mixed_case_host_matches_site_test_() ->
    Opts = #{ docroot => "test-docroot" },
    Routes = [{gemini, <<"(?<PATH>.*)">>, blizanci_static, public, Opts}],
    {ok, Routing} = blizanci_router:prepare(Routes),
    Config = #server_config{hostname = <<"example.org">>, port = 1965,
                            routing = Routing},
    H = fun(Url) ->
            handle_line(Url, Config, {error, no_peercert}, <<>>)
        end,
    [?_assertEqual(?NOT_FOUND, H(<<"gemini://example.org/x">>)),
     ?_assertEqual(?NOT_FOUND, H(<<"gemini://Example.ORG/x">>)),
     ?_assertEqual(?NOT_FOUND, H(<<"gemini://EXAMPLE.org/x">>)),
     ?_assertEqual(?NOT_FOUND, H(<<"GEMINI://EXAMPLE.ORG:1965/x">>)),
     ?_assertEqual({error_code, host_unrecognised},
                   H(<<"gemini://Example.com/x">>)),
     ?_assertEqual({error_code, host_unrecognised},
                   H(<<"gemini://xexample.org/x">>)),
     ?_assertEqual({error_code, host_unrecognised},
                   H(<<"gemini://EXAMPLE.ORG.EVIL/x">>))].

%% The fold is ASCII-only: a host with the Kelvin sign (U+212A, bytes
%% 226,132,170) where "k" would go is not the configured host. The URL
%% parser may refuse it first; handle_gemini_url is called directly too,
%% so the comparison itself is tested.
kelvin_host_not_ascii_host_test_() ->
    H = url_handler(),
    Kelvin = <<226,132,170>>,
    Host = <<"this.host.dev">>,
    KHost = <<"in", Kelvin/binary, "ed.dev">>,
    Config = #server_config{hostname = <<"inked.dev">>, port = 1965,
                            routing = []},
    HK = fun(Url) ->
             handle_line(Url, Config, {error, no_peercert}, <<>>)
         end,
    Req = fun(Hst) -> #{ client_cert   => {error, no_peercert},
                         query         => <<>>,
                         rest_of_input => <<>>,
                         scheme        => <<"gemini">>,
                         host          => Hst,
                         port          => 1965,
                         path          => <<"/x">> }
          end,
    [?_assertNotEqual(?NOT_FOUND,
                      H(<<"gemini://this.host.dev", Kelvin/binary, "/x">>)),
     % end to end: never folded to ASCII "k", so never the site
     ?_assertMatch({error_code, _},
                   HK(<<"gemini://", KHost/binary, "/x">>)),
     ?_assertNotEqual({error_code, file_not_found},
                      HK(<<"gemini://", KHost/binary, "/x">>)),
     ?_assertEqual(KHost, blizanci_config:ascii_lowercase(KHost)),
     ?_assertEqual({error_code, host_unrecognised},
                   handle_gemini_url(gemini, Req(KHost), Config)),
     ?_assertEqual({error_code, host_unrecognised},
                   handle_gemini_url(gemini, Req(Host), Config)),
     % handle_gemini_url expects an already-folded host
     ?_assertEqual({error_code, host_unrecognised},
                   handle_gemini_url(gemini, Req(<<"INKED.dev">>),
                                     Config)),
     ?_assertNotEqual({error_code, host_unrecognised},
                      handle_gemini_url(gemini, Req(<<"inked.dev">>),
                                        Config))].

%% Percent-escapes in the path are decoded, exactly once, after the path
%% structure is known; the result must be printable ASCII, and must not
%% contain a "/" that was escaped. Fixture, under a fresh temporary
%% directory:
%%   Base/docs/{"a b", "100%", "%2e", "semi;colon", ".env",
%%              "sp ace/plain"}
%%   Base/titan/{"my file.txt", "x", "y"}
%% Files have no extension, so no MIME lookup is involved.
percent_decoding_test_() ->
    {setup, fun percent_setup/0, fun percent_teardown/1,
     fun percent_tests/1}.

percent_setup() ->
    Base = blizanci_test_support:tmp_path("percent_test"),
    ok = filelib:ensure_path(filename:join(Base, "docs/sp ace")),
    ok = filelib:ensure_path(filename:join(Base, "titan")),
    ok = filelib:ensure_path(filename:join(Base, "work")),
    [ ok = file:write_file(filename:join([Base, "docs", F]), <<"x">>)
      || F <- ["a b", "100%", "%2e", "semi;colon", ".env",
               "sp ace/plain"] ],
    [ ok = file:write_file(filename:join([Base, "titan", F]), <<"x">>)
      || F <- ["my file.txt", "x", "y"] ],
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
                            routing = Routing},
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
                deleted_by(fun() -> T(<<"my%20file.txt;size=0">>) end),
                ?assertNot(filelib:is_file(
                             filename:join(Titan, "my file.txt")))
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
                deleted_by(fun() ->
                               T(<<"y;size=0;mime=text/plain">>)
                           end),
                ?assertNot(filelib:is_file(filename:join(Titan, "y")))
            end)].

% Run a request that starts a Titan servlet, and wait for that servlet
% to finish: it has done its deletion by then. Done in a process of its
% own that traps exits, as the connection does, so that the servlet's
% exit does not take it down.
deleted_by(Request) ->
    blizanci_test_support:isolated(
      fun() ->
          process_flag(trap_exit, true),
          {init_servlet, Pid} = Request(),
          Ref = erlang:monitor(process, Pid),
          receive {'DOWN', Ref, process, Pid, _} -> ok
          after 5000 -> error(titan_did_not_finish)
          end
      end).
