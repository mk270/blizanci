%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_config).

-export([ssl_opts/0, proto_opts/0]).

ssl_opts() ->
    {ok, Cert} = application:get_env(certfile),
    {ok, Key} = application:get_env(keyfile),
    {ok, Port} = application:get_env(port),
    VerifyFn = fun (ClientCert, Ev, Init) ->
                   blizanci_gemini:verify_cert(ClientCert, Ev, Init) end,
    SSL_Opts =
        [{port, Port},
         {certfile, Cert},
         {keyfile, Key},
         {verify, verify_peer},
         {cacertfile, "/dev/null"},
         {verify_fun, {VerifyFn, []}}
        ],
    SSL_Opts.

proto_opts() ->
    {ok, Hostname} = application:get_env(hostname),
    {ok, Docroot} = application:get_env(docroot),
    {ok, CGIroot} = application:get_env(cgiroot),
    {ok, Port} = application:get_env(port),
    Proto_Opts =
        [{hostname, Hostname},
         {docroot, Docroot},
         {cgiroot, CGIroot},
         {port, Port}
        ],
    Proto_Opts.
