%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-type cgi_status()   :: {pid(), integer(), binary()}.
-type servlet_proc() :: {'proc', pid()} | 'no_proc'.

-record(server_config,
        {hostname   :: binary(),
         port       :: integer(),
         docroot    :: string(),
         cgiroot    :: string(),
         cgiprefix  :: string()}).
-type server_config() :: #server_config{}.

-record(state,
        {transport    :: atom(),
         socket       :: inet:socket(),
         buffer       :: binary(),
         config       :: server_config(),
         requested    :: boolean(),
         servlet_proc :: servlet_proc(),
         client_cert  :: term()}).
-type state() :: #state{}.

-type gemini_response() :: {'file', binary(), binary()}
                         | {'error_code', atom()}
                         | {'redirect', binary()}
                         | hangup
                         | none
                         | {'init_servlet', pid()}
                         | {'servlet_output', binary()}.

-type gemini_session() :: continue
                        | finished 
                        | {'expect_servlet', pid()}.

-type authorisation() :: public | restricted | private.

-type env_list() :: [{string(), string()}].


-type cgi_error() :: 'cgi_exec_error' | 'file_not_found' | 'gateway_busy'.

-type gateway_result() :: {'gateway_output', binary()}
                        | {'gateway_error', cgi_error()}
                        | {'gateway_started', pid()}
                        | {'gateway_bypassed', gemini_response()}.

-type servlet_result() :: {'servlet_failed', atom()}
                        | {'servlet_complete', binary() }
                        | {'servlet_bypassed', gemini_response()}.
