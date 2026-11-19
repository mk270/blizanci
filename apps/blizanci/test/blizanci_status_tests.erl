%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_status_tests).

-include_lib("eunit/include/eunit.hrl").

-import(blizanci_status, [gemini_status/1, is_error/1]).

%% gemini_status/1 is total: unknown atoms give the 40 of
%% internal_server_error (with a warning); 'unimplemented' (static's
%% serve/4 and the cgi_error() type) has its own entry, so no warning.
gemini_status_test_() ->
    ISE = {40, <<"Internal server error">>},
    [?_assertEqual(Want, gemini_status(Code)) ||
        {Code, Want} <- [{file_not_found, {51, <<"File not found">>}},
                         {success, {20, <<"Success">>}},
                         {internal_server_error, ISE},
                         {unimplemented, {40, <<"Not implemented">>}},
                         {no_such_status, ISE}]].

% Gemini 60 (required), 61 (not authorised), 62 (not valid).
cert_status_test_() ->
    [?_assertMatch({Status, _}, gemini_status(Code)) ||
        {Code, Status} <- [{cert_required, 60},
                           {cert_not_authorised, 61},
                           {cert_expired, 62},
                           {cert_not_yet_valid, 62},
                           {cert_not_parsed, 62},
                           {cert_unsupported_encoding, 62}]]
    ++ [?_assertEqual({62, <<"Client certificate not yet valid">>},
                      gemini_status(cert_not_yet_valid))].

is_error_test_() ->
    [% failure statuses (40 and above)
     ?_assert(is_error(file_not_found)),
     ?_assert(is_error(gateway_busy)),
     ?_assert(is_error(internal_server_error)),
     ?_assert(is_error(cert_not_parsed)),
     ?_assert(is_error(cert_not_yet_valid)),
     ?_assert(is_error(bad_filename)),
     % mapped, but not failures
     ?_assertNot(is_error(success)),
     ?_assertNot(is_error(permanent_redirect)),
     ?_assert(is_error(unimplemented)),
     % not in the table
     ?_assertNot(is_error(no_such_status))].
