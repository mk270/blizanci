%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_status).
-export([gemini_status/1]).


-spec gemini_status(Code) -> Result
              when Code   :: atom(),
                   Result :: {integer(), binary()}.

gemini_status(request_too_long)      -> {59, <<"Request too long">>};
gemini_status(request_not_parsed)    -> {59, <<"Request not parsed">>};
gemini_status(bad_query_string)      -> {59, <<"Bad query string">>};
gemini_status(success)               -> {20, <<"Success">>};
gemini_status(request_timeout)       -> {59, <<"Request timeout">>};
gemini_status(proxy_refused)         -> {53, <<"Proxy request refused">>};
gemini_status(host_unrecognised)     -> {53, <<"Host unrecognised">>};
gemini_status(port_unrecognised)     -> {53, <<"Port unrecognised">>};
gemini_status(unrecognised_protocol) -> {59, <<"Protocol not recognised">>};
gemini_status(bad_unicode)           -> {59, <<"Bad unicode in request">>};
gemini_status(bad_filename)          -> {59, <<"Illegal filename">>};
gemini_status(bad_hostname)          -> {59, <<"Illegal hostname">>};
gemini_status(userinfo_supplied)     -> {59, <<"Illegal username">>};
gemini_status(internal_server_error) -> {40, <<"Internal server error">>};
gemini_status(gateway_busy)          -> {40, <<"Gateway too busy">>};
gemini_status(cgi_exec_error)        -> {40, <<"Gateway error">>};
gemini_status(response_timeout)      -> {40, <<"Timeout">>};
gemini_status(cannot_overwrite)      -> {40, <<"Cannot overwrite file">>};
gemini_status(file_not_found)        -> {51, <<"File not found">>};
gemini_status(cert_required)         -> {60, <<"Client certificate required">>};
gemini_status(permanent_redirect)    -> {31, <<"Moved permanently">>};
gemini_status(cert_not_authorised)   -> {60, <<"Client certificate unauthorised">>}.
