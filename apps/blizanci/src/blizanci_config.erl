%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_config).

-export([ssl_opts/0, proto_opts/0]).

-define(PORT, 1965).

-spec ssl_opts() -> [{atom(), term()}].
ssl_opts() ->
    {ok, App} = application:get_application(),
    Cert = application:get_env(App, certfile,
                                               "./ssl/certificate.crt"),
    Key = application:get_env(App, keyfile,
                                              "./ssl/key.pem"),
    Options = #{
		hostname => hostname()
	       },
    ok = zotonic_ssl_certs:ensure_self_signed( Cert, Key, Options ),
    Port = application:get_env(App, port, ?PORT),
    VerifyFn = fun (ClientCert, Ev, Init) ->
                   blizanci_gemini:verify_cert(ClientCert, Ev, Init) end,

    [{port, Port},
     {certfile, Cert},
     {keyfile, Key},
     {verify, verify_peer},
     {verify_fun, {VerifyFn, []}},
     {versions, ['tlsv1.3']}
    ].

-spec proto_opts() -> [{atom(), term()}].
proto_opts() ->
    {ok, App} = application:get_application(),
    Docroot = application:get_env(App, docroot, "./public_gemini"),
    CGIroot = application:get_env(App, cgiroot, "./cgi-bin"),
    Port = application:get_env(App, port, ?PORT),
    Default_Routing = routing_table(hostname(), Docroot, CGIroot, Port),
    Routes = application:get_env(App, routing, Default_Routing),
    {ok, Routing} = blizanci_router:prepare(Routes),

    [{hostname, hostname() },
     {docroot, Docroot},
     {cgiroot, CGIroot},
     {port, Port},
     {routing, Routing}
    ].

-type route_entry() :: {string(), atom(), map()}.
-spec routing_table(string(), string(), string(), integer()) -> [route_entry()].
routing_table(Hostname, Docroot, CGIroot, Port) ->
    CGI_Opts =
        #{ hostname => Hostname,
           port => Port,
           cgiroot => CGIroot,
           cgiprefix => "/cgi-bin/" },
    Static_Opts =
        #{ index => "index.gemini",
           docroot => Docroot,
           unknown_mimetype => <<"application/octet-stream">>,
           bare_mimetype => <<"text/gemini">>,
           authorisation => public
         },
    WP_Opts =
        #{ index => "index.gemini",
           docroot => Docroot,
           unknown_mimetype => <<"application/octet-stream">>,
           bare_mimetype => <<"text/gemini">>,
           authorisation => public,
	   target => "https://www.piratenpartij.nl"
         },
    [
     {"wp/(?<PATH>.*)",
      blizanci_wp, WP_Opts },

     {"cgi-bin/(?<PATH>.*)",
      blizanci_cgi, CGI_Opts},

     {"(?<PATH>restricted.*)",
      blizanci_static, Static_Opts#{ authorisation => restricted } },

     {"(?<PATH>.*)",
      blizanci_static, Static_Opts}
    ].

%---
% getters...
%
hostname() -> 
	{ok, App} = application:get_application(),
	{ok, Default_Hostname} = inet:gethostname(),
	application:get_env(App, hostname, Default_Hostname).

