%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-type cgi_status()   :: {pid(), integer(), binary()}.
-type servlet_proc() :: {'proc', pid()} | 'no_proc'.

-type mp() :: {re_pattern, _, _, _, _}.

-type authorisation() :: public | restricted | {private, [string()]}.

-type route_option() :: {atom(), any()}.

-record(route,
        {proto       :: atom(),
         pattern     :: mp(),
         module      :: module(),
         auth_policy :: authorisation(),
         options     :: map()}). % [route_option()]}).
-type route() :: #route{}.

-record(server_config,
        {hostname   :: binary(),
         port       :: integer(),
         routing    :: [route()],
         docroot    :: string()}).
-type server_config() :: #server_config{}.

-type peer_cert() :: {'ok', public_key:der_encoded() } |
                     {'error', term() }.

-record(state,
        {transport    :: atom(),
         socket       :: any(),
         buffer       :: binary(),
         config       :: server_config(),
         requested    :: boolean(),
         servlet_proc :: servlet_proc(),
         client_cert  :: peer_cert()}).
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

-type env_list() :: [{string(), string()}].


-type cgi_error() :: 'cgi_exec_error'
                   | 'file_not_found'
                   | 'gateway_busy'
                   | 'unimplemented'.

-type gateway_result() :: {'gateway_output', binary()}
                        | {'gateway_finished', gemini_response() }
                        | {'gateway_error', cgi_error()}
                        | {'gateway_started', pid()}.


-type servlet_result() :: {'servlet_failed', atom()}
                        | {'servlet_complete', binary() }.

-type cert_details() :: #{ common_name := binary(),
                           issuer_common_name := binary() }.

-type client_cert() :: 'error' | {'ok', cert_details()}.

-type request_details() :: #{ client_cert   := peer_cert(),
                              query         := binary(),
                              rest_of_input := binary(),
                              scheme        := binary(),
                              host          := binary(),
                              port          := integer(),
                              path          := binary()
                            }.

-type path_matches() :: #{ binary() => binary() }.
