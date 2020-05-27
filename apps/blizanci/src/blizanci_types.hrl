%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-type cgi_status() :: {pid(), integer(), binary()} | no_cgi.

-record(server_config,
        {hostname  :: binary(),
        port       :: integer(),
        docroot    :: string()}).
-type server_config() :: #server_config{}.

-record(state,
        {transport   :: atom(),
         socket      :: inet:socket(),
         buffer      :: binary(),
         config      :: server_config(),
         requested   :: boolean(),
         cgi_proc    :: cgi_status(),
         client_cert :: term()}).
-type state() :: #state{}.

-type gemini_response() :: {'file', binary(), binary()}
                         | {'error_code', atom()}
                         | {'redirect', binary()}
                         | hangup
                         | none
                         | {'init_cgi', pid(), integer()}
                         | {'cgi_output', binary()}.

-type gemini_session() :: continue
                        | finished 
                        | {'expect_cgi', pid(), integer()}.
