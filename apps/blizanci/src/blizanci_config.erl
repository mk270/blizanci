%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

%% @doc
%% Most of the configuration of blizanci is done via this module, albeit
%% there remain some key values hardwired into the source code of the other
%% modules.
%%
%% Generally, configuration data is hardwired into this module, or derived
%% from the application environment set in sys.config.
%%
%% This module is primarily called during the initialisation of the blizanci
%% app, before the ranch listener is started. Its output is then inherited
%% via the many processes started (directly or otherwise) by ranch.
%%
%% Invalid configuration items generally result in a crash of the process
%% which responds to a relevant Gemini request. However, future versions
%% of blizanci will become increasingly strict about verifying that the
%% configuration is valid at application start time.
%%
%% Configuration items:
%%
%% <dl>
%%   <dt><code>{ca_certs, [CACertPath::string()}]</code></dt>
%%   <dd>the path(s) to the certificate authority certificate(s)
%%       to be used for verifying client certificates;
%%       may be the empty list.</dd>
%%   <dt><code>{certfile, CertPath::string()}</code></dt>
%%   <dd>the path to the server's certificate</dd>
%%   <dt><code>{cgiroot, CGIRootPath::string()}</code></dt>
%%   <dd>the path to the root directory for CGI scripts; normally there would only be a single, top-level directory with no subdirectories</dd>
%%   <dt><code>{docroot, DocrootPath::string()}</code></dt>
%%   <dd>the path to the root of the documents to be served</dd>
%%   <dt><code>{hostname, Hostname::string()}</code></dt>
%%   <dd>the hostname (defaults to system hostname)</dd>
%%   <dt><code>{keyfile, PrivKeyPath::string()}</code></dt>
%%   <dd>the path to the server's private key</dd>
%%   <dt><code>{port, Port::integer()}</code></dt>
%%   <dd>the port to listen on (defaults to 1965)</dd>
%%   <dt><code>{routing, [Route]}</code></dt>
%%   <dd>the routing table (for the format of which, currently,
%%       see the source code). Each entry in the table has the form
%%       {Scheme, Regexp, Module, AuthPolicy, Options}
%%
%%   </dd>
%% </dl>
%%
%% Relative paths are allowed in all configuration items other than
%% <code>routing</code>
%%
%% @end

-module(blizanci_config).
-include("blizanci_types.hrl").

-export([make/0]).

-define(PORT, 1965).

-spec ssl_opts() -> [{atom(), term()}].
%% @doc
%% Generate the options required for ranch to initialise SSL
%% @end
ssl_opts() ->
    {ok, App} = application:get_application(),
    {ok, Cert} = get_pem_file_from_environment(App, certfile,
                                               "./ssl/certificate.pem"),
    {ok, Key} = get_pem_file_from_environment(App, keyfile,
                                              "./ssl/key.pem"),
    Port = application:get_env(App, port, ?PORT),
    VerifyFn = fun (ClientCert, Ev, Init) ->
                   blizanci_x509:verify_cert(ClientCert, Ev, Init) end,

    [{port, Port},
     {certfile, Cert},
     {keyfile, Key},
     {verify, verify_peer},
     {cacertfile, "/dev/null"},
     {verify_fun, {VerifyFn, []}},
     {fail_if_no_peer_cert, false},
     {versions, ['tlsv1.3']}
    ].

-spec proto_opts() -> [{atom(), term()}].
%% @doc
%% Generate the options required for ranch to initialise the TCP listener
%% @end
proto_opts() ->
    {ok, App} = application:get_application(),
    {ok, Default_Hostname} = inet:gethostname(),
    Hostname = application:get_env(App, hostname, Default_Hostname),
    Docroot = application:get_env(App, docroot, "./public_gemini"),
    CGIroot = application:get_env(App, cgiroot, "./cgi-bin"),
    CACerts = application:get_env(App, ca_certs, []),
    Port = application:get_env(App, port, ?PORT),
    Default_Routing = routing_table(Docroot, CGIroot, CACerts),
    Routes = application:get_env(App, routing, Default_Routing),
    {ok, Routing} = blizanci_router:prepare(Routes),

    [{hostname, Hostname},
     {docroot, Docroot},
     {cgiroot, CGIroot},
     {port, Port},
     {routing, Routing}
    ].


-type route_entry() :: {atom(), string(), atom(), atom(), map()}.

-spec routing_table(Docroot, CGIroot, CACerts) -> RoutingTable
              when Docroot      :: string(),
                   CGIroot      :: string(),
                   CACerts      :: [string()],
                   RoutingTable :: [route_entry()].

routing_table(Docroot, CGIroot, CACerts) ->
    CGI_Opts = #{ cgiroot => CGIroot },
    Static_Opts = #{ docroot => Docroot },
    CAs = CACerts, % e.g., CAs = ["./ssl/cacert0.pem"]
    Default_Route_Specs = [
     {gemini, "cgi-bin/(?<PATH>.*)",   blizanci_cgi,
      public,         CGI_Opts},

     {gemini, "(?<PATH>private.*)",    blizanci_static,
      {private, CAs}, Static_Opts},

     {gemini, "(?<PATH>restricted.*)", blizanci_static,
      restricted,     Static_Opts},

     {gemini, "(?<PATH>.*)",           blizanci_static,
      public,         Static_Opts},

     {titan,  "(?<PATH>.*)",           blizanci_titan,
      {private, CAs}, #{}}
    ],
    [ {Proto, Pattern, Module, AuthPolicy,
       maps:merge(Module:default_options(), Opts)
      }
      || {Proto, Pattern, Module, AuthPolicy, Opts} <- Default_Route_Specs ].


% this is a ridiculous utility function to return the unique modules in the
% routing table
-spec unique_module(Tuples) -> Results
             when Tuples  :: [tuple()],
                  Results :: [term()].
unique_module(Tuples) ->
   lists:usort([T#route.module || T <- Tuples, is_tuple(T), tuple_size(T) > 0]).


-spec get_pem_file_from_environment(App, Key, Default_Filename) -> Result
              when App              :: atom(),
                   Key              :: atom(),
                   Default_Filename :: string(),
                   Result           :: {ok, string()} |
                                       {fail, atom()}.

get_pem_file_from_environment(App, Key, Default_Filename) ->
    Value = application:get_env(App, Key, Default_Filename),
    blizanci_x509:validate_pem_file(Value).


-spec make() -> map().
make() ->
    SSL_Opts = ssl_opts(),
    Proto_Opts = proto_opts(),
    Routing = proplists:get_value(routing, Proto_Opts),
    ActiveServlets = unique_module(Routing),

    #{
      ssl_opts        => SSL_Opts,
      proto_opts      => Proto_Opts,
      active_servlets => ActiveServlets
     }.
