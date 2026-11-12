%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_status).
-include_lib("eunit/include/eunit.hrl").
-export([gemini_status/1, valid_code/1, is_error/1]).


% The status and explanation for an error atom. The function is total: an
% atom the table does not know (a typo, or a third-party servlet's own
% atom) is logged and answered as 40, as internal_server_error is.
-spec gemini_status(Code) -> Result
              when Code   :: atom(),
                   Result :: {integer(), binary()}.

gemini_status(Code) ->
    case status_entry(Code) of
        undefined ->
            logger:warning("no Gemini status for ~p; using 40", [Code]),
            status_entry(internal_server_error);
        Entry ->
            Entry
    end.


% The table proper; 'undefined' for an atom it does not contain.
-spec status_entry(Code) -> Result
              when Code   :: atom(),
                   Result :: {integer(), binary()} | 'undefined'.

status_entry(request_too_long) ->
    {59, <<"Request too long">>};
status_entry(request_not_parsed) ->
    {59, <<"Request not parsed">>};
status_entry(bad_query_string) ->
    {59, <<"Bad query string">>};
status_entry(success) ->
    {20, <<"Success">>};
status_entry(request_timeout) ->
    {59, <<"Request timeout">>};
status_entry(proxy_refused) ->
    {53, <<"Proxy request refused">>};
status_entry(host_unrecognised) ->
    {53, <<"Host unrecognised">>};
status_entry(port_unrecognised) ->
    {53, <<"Port unrecognised">>};
status_entry(unrecognised_protocol) ->
    {59, <<"Protocol not recognised">>};
status_entry(bad_unicode) ->
    {59, <<"Bad unicode in request">>};
status_entry(bad_filename) ->
    {59, <<"Illegal filename">>};
status_entry(bad_hostname) ->
    {59, <<"Illegal hostname">>};
status_entry(userinfo_supplied) ->
    {59, <<"Illegal username">>};
status_entry(internal_server_error) ->
    {40, <<"Internal server error">>};
status_entry(gateway_busy) ->
    {40, <<"Gateway too busy">>};
status_entry(cgi_exec_error) ->
    {40, <<"Gateway error">>};
status_entry(cgi_output_too_large) ->
    {40, <<"Gateway error">>};
status_entry(response_timeout) ->
    {40, <<"Timeout">>};
status_entry(cannot_overwrite) ->
    {40, <<"Cannot overwrite file">>};
status_entry(file_not_found) ->
    {51, <<"File not found">>};
status_entry(cert_required) ->
    {60, <<"Client certificate required">>};
status_entry(permanent_redirect) ->
    {31, <<"Moved permanently">>};
status_entry(cert_not_authorised) ->
    {60, <<"Client certificate unauthorised">>};
status_entry(cert_expired) ->
    {60, <<"Client certificate expired">>};
status_entry(cert_not_parsed) ->
    {60, <<"Client certificate not understood">>};
status_entry(cert_unsupported_encoding) ->
    {60, <<"Client certificate uses an unsupported name encoding">>};
status_entry(_) ->
    undefined.


% True if Code is an atom the status table maps to a failure status (40
% or above). Atoms with no mapping, and mappings to success or redirect
% codes, are not errors in this sense, so a servlet cannot make the server
% answer with a status the table does not define or a non-failure one.
-spec is_error(Code) -> Result
              when Code   :: atom(),
                   Result :: boolean().

is_error(Code) when is_atom(Code) ->
    case status_entry(Code) of
        {Status, _Explanation} -> Status >= 40;
        undefined -> false
    end.


-spec valid_code(Code) -> boolean()
              when Code :: integer().

valid_code(Code) ->
    lists:member(Code, [
        1, 2, 3, 4, 5, 6,
        10, 11,
        20,
        30, 31,
        40, 41, 42, 43, 44,
        50, 51, 52, 53, 59,
        60, 61, 62
    ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% gemini_status/1 is total: unknown atoms, including 'unimplemented'
%% (static's serve/4 and the cgi_error() type), give the 40 of
%% internal_server_error.
gemini_status_test_() ->
    ISE = {40, <<"Internal server error">>},
    [?_assertEqual(Want, gemini_status(Code)) ||
        {Code, Want} <- [{file_not_found, {51, <<"File not found">>}},
                         {success, {20, <<"Success">>}},
                         {internal_server_error, ISE},
                         {unimplemented, ISE},
                         {no_such_status, ISE}]].

is_error_test_() ->
    [% failure statuses (40 and above)
     ?_assert(is_error(file_not_found)),
     ?_assert(is_error(gateway_busy)),
     ?_assert(is_error(internal_server_error)),
     ?_assert(is_error(cert_not_parsed)),
     ?_assert(is_error(bad_filename)),
     % mapped, but not failures
     ?_assertNot(is_error(success)),
     ?_assertNot(is_error(permanent_redirect)),
     % not in the table
     ?_assertNot(is_error(unimplemented)),
     ?_assertNot(is_error(no_such_status))].
