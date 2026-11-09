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
-include_lib("eunit/include/eunit.hrl").

-export([from_app_env/0, make/3, find_site/2, route/5,
         global/2, site/3, add_route/2]).

-define(PORT, 1965).
-define(DEFAULT_SERVLETS, [blizanci_static, blizanci_cgi, blizanci_titan]).

% Build and validate one route, eagerly: a bad route fails here, with a
% specific reason, the moment it is constructed rather than later
% inside a whole-config validation.
-spec route(Proto, Regex, Module, AuthPolicy, Opts) -> Result
              when Proto      :: atom(),
                   Regex      :: string() | binary(),
                   Module     :: module(),
                   AuthPolicy :: authorisation(),
                   Opts       :: map(),
                   Result     :: {'ok', route()} | {'error', term()}.
route(Proto, Regex, Module, AuthPolicy, Opts) ->
    blizanci_router:build_route(Proto, Regex, Module, AuthPolicy, Opts).


% The global part of a configuration: one per blizanci instance,
% however many sites it serves.
-spec global(Port, Servlets) -> Result
              when Port     :: integer(),
                   Servlets :: [module()],
                   Result   :: global_config().
global(Port, Servlets) ->
    #global{port = Port, servlets = Servlets}.


% A site with no routes yet: fully self-contained, knowing nothing of
% any other site. Add routes with add_route/2.
-spec site(Hostname, CertFile, KeyFile) -> Result
              when Hostname :: string(),
                   CertFile :: string(),
                   KeyFile  :: string(),
                   Result   :: site().
site(Hostname, CertFile, KeyFile) ->
    #site{hostname = erlang:list_to_binary(Hostname),
          certfile = CertFile,
          keyfile  = KeyFile,
          routing  = [],
          docroot  = undefined}.


% Append a route to a site's routing table. Routes are tried in order,
% first match wins, so this is "append" (iptables -A): a route added
% later is only reached if no earlier one matches.
-spec add_route(Site, Route) -> Result
              when Site   :: site(),
                   Route  :: route(),
                   Result :: site().
add_route(Site = #site{routing = Routing}, Route = #route{}) ->
    Site#site{routing = Routing ++ [Route]}.


% Build one site from explicit values.
-spec make_site(Hostname, Cert, Key, Docroot, Routes) -> Result
              when Hostname :: string(),
                   Cert     :: string(),
                   Key      :: string(),
                   Docroot  :: string(),
                   Routes   :: [route_entry()],
                   Result   :: site().
make_site(Hostname, Cert, Key, Docroot, Routes) ->
    {ok, Routing} = blizanci_router:prepare(Routes),
    Site = site(Hostname, Cert, Key),
    Site#site{routing = Routing, docroot = Docroot}.


-spec find_site(Hostname, Sites) -> Result
              when Hostname :: binary(),
                   Sites    :: [site()],
                   Result   :: {'ok', site()} | 'not_found'.
find_site(Hostname, Sites) ->
    case lists:keyfind(Hostname, #site.hostname, Sites) of
        false -> not_found;
        Site -> {ok, Site}
    end.


-spec ssl_opts(Port, Sites) -> Result
              when Port   :: integer(),
                   Sites  :: [site()],
                   Result :: [{atom(), term()}].
%% @doc
%% Generate the options required for ranch to initialise SSL
%% @end
ssl_opts(Port, Sites) ->
    VerifyFn = fun (ClientCert, Ev, Init) ->
                   blizanci_x509:verify_cert(ClientCert, Ev, Init) end,

    % No top-level certfile/keyfile: sni_fun is the sole source of TLS
    % credentials, so a client that doesn't send SNI (or sends SNI for
    % a hostname that isn't one of the configured sites) has nothing
    % to complete a handshake against and is refused at the TLS layer,
    % before any Gemini request is ever parsed.
    SniFun = fun(SniHostname) ->
                 case find_site(erlang:list_to_binary(SniHostname), Sites) of
                     {ok, Site} -> [{certfile, Site#site.certfile},
                                    {keyfile, Site#site.keyfile}];
                     not_found -> unrecognized
                 end
             end,

    [{port, Port},
     {sni_fun, SniFun},
     {verify, verify_peer},
     {cacerts, []},
     {certificate_authorities, false},
     {verify_fun, {VerifyFn, []}},
     {fail_if_no_peer_cert, false},
     {versions, ['tlsv1.3']}
    ].

-spec proto_opts(Port, Sites) -> Result
              when Port   :: integer(),
                   Sites  :: [site()],
                   Result :: [{atom(), term()}].
%% @doc
%% Generate the options required for ranch to initialise the TCP listener
%% @end
proto_opts(Port, Sites) ->
    [{port, Port},
     {sites, Sites}
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
    % blizanci_router:make_route/1 merges each route's Opts with the
    % module's own default_options/0; nothing to do here but pass
    % the per-route overrides through.
    Default_Route_Specs.


-spec get_pem_file_from_environment(App, Key, Default_Filename) -> Result
              when App              :: atom(),
                   Key              :: atom(),
                   Default_Filename :: string(),
                   Result           :: {ok, string()} |
                                       {fail, atom()}.

get_pem_file_from_environment(App, Key, Default_Filename) ->
    Value = application:get_env(App, Key, Default_Filename),
    blizanci_x509:validate_pem_file(Value).


% Which servlets this instance may ever route to -- a static,
% deploy-time decision (analogous to {applications, [...]} in
% blizanci.app.src), not something inferred from the routing table.
% This means routing-table updates can reference any declared servlet
% without needing to separately ensure its pool has been started,
% and lets a third-party servlet be registered without forking
% blizanci -- just add it to this list (or override via the
% `servlets` app env).
% Takes everything explicitly: a library caller builds its own port,
% servlet list and sites and passes them in; nothing here consults
% the application environment.
-spec make(Port, Servlets, Sites) -> Result
              when Port     :: integer(),
                   Servlets :: [module()],
                   Sites    :: [site()],
                   Result   :: map().
make(Port, Servlets, Sites) ->
    #{
      ssl_opts   => ssl_opts(Port, Sites),
      proto_opts => proto_opts(Port, Sites),
      servlets   => Servlets
     }.


% The one place that reads sys.config: pulls blizanci's app env into
% the explicit values make/3 takes. Must be called from within
% blizanci's own application context (application:get_application/0
% resolves via the calling process), i.e. from blizanci_app:start/2.
-spec from_app_env() -> Result
              when Result :: map().
from_app_env() ->
    {ok, App} = application:get_application(),
    {ok, Default_Hostname} = inet:gethostname(),
    Hostname = application:get_env(App, hostname, Default_Hostname),
    {ok, Cert} = get_pem_file_from_environment(App, certfile,
                                               "./ssl/certificate.pem"),
    {ok, Key} = get_pem_file_from_environment(App, keyfile,
                                              "./ssl/key.pem"),
    Docroot = application:get_env(App, docroot, "./public_gemini"),
    CGIroot = application:get_env(App, cgiroot, "./cgi-bin"),
    CACerts = application:get_env(App, ca_certs, []),
    Default_Routing = routing_table(Docroot, CGIroot, CACerts),
    Routes = application:get_env(App, routing, Default_Routing),
    % Today's sys.config only ever describes a single site, so this
    % is a one-element list; everything downstream looks sites up by
    % hostname and doesn't assume that.
    Sites = [make_site(Hostname, Cert, Key, Docroot, Routes)],
    Port = application:get_env(App, port, ?PORT),
    Servlets = application:get_env(App, servlets, ?DEFAULT_SERVLETS),
    make(Port, Servlets, Sites).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(TEST_DOCROOT, "test-docroot").

route_ok_test_() ->
    Opts = #{ docroot => ?TEST_DOCROOT },
    Static = fun(Regex, Auth) ->
                 route(gemini, Regex, blizanci_static, Auth, Opts)
             end,
    [?_assertMatch({ok, #route{proto = gemini,
                               module = blizanci_static,
                               auth_policy = public}},
                   Static("(?<PATH>.*)", public)),
     ?_assertMatch({ok, #route{}}, Static(<<"(?<PATH>.*)">>, public)),
     ?_assertMatch({ok, #route{auth_policy = restricted}},
                   Static("x", restricted)),
     ?_assertMatch({ok, #route{auth_policy = {private, ["ca.pem"]}}},
                   Static("x", {private, ["ca.pem"]}))].

route_merges_default_options_test() ->
    {ok, #route{options = Options}} =
        route(gemini, "x", blizanci_static, public,
              #{ docroot => ?TEST_DOCROOT }),
    Defaults = blizanci_static:default_options(),
    ?assertEqual(maps:merge(Defaults, #{ docroot => ?TEST_DOCROOT }),
                 Options).

route_errors_test_() ->
    Opts = #{ docroot => ?TEST_DOCROOT },
    [?_assertMatch({error, {invalid_proto, gopher}},
                   route(gopher, "x", blizanci_static, public, Opts)),
     ?_assertMatch({error, {invalid_regex, "(", _}},
                   route(gemini, "(", blizanci_static, public, Opts)),
     ?_assertMatch({error, {invalid_regex, 42, not_a_string}},
                   route(gemini, 42, blizanci_static, public, Opts)),
     ?_assertEqual({error, {unknown_servlet, no_such_servlet}},
                   route(gemini, "x", no_such_servlet, public, Opts)),
     ?_assertEqual({error, {not_a_servlet, lists}},
                   route(gemini, "x", lists, public, Opts)),
     ?_assertEqual({error, {not_a_servlet, "blizanci_static"}},
                   route(gemini, "x", "blizanci_static", public, Opts)),
     ?_assertEqual({error, {invalid_authz_policy, bogus}},
                   route(gemini, "x", blizanci_static, bogus, Opts)),
     ?_assertEqual({error, {invalid_route_opts, not_a_map}},
                   route(gemini, "x", blizanci_static, public, not_a_map)),
     ?_assertEqual({error, {invalid_route_options, blizanci_static,
                            {docroot_not_a_directory, "/no/such/dir"}}},
                   route(gemini, "x", blizanci_static, public,
                         #{ docroot => "/no/such/dir" }))].

global_test() ->
    ?assertEqual(#global{port = 1965, servlets = [blizanci_static]},
                 global(1965, [blizanci_static])).

site_test() ->
    ?assertEqual(#site{hostname = <<"example.org">>,
                       certfile = "c.pem",
                       keyfile  = "k.pem",
                       routing  = [],
                       docroot  = undefined},
                 site("example.org", "c.pem", "k.pem")).

add_route_appends_test() ->
    Opts = #{ docroot => ?TEST_DOCROOT },
    {ok, R1} = route(gemini, "a", blizanci_static, public, Opts),
    {ok, R2} = route(gemini, "b", blizanci_static, restricted, Opts),
    Site0 = site("example.org", "c.pem", "k.pem"),
    Site1 = add_route(Site0, R1),
    Site2 = add_route(Site1, R2),
    ?assertEqual([R1], Site1#site.routing),
    ?assertEqual([R1, R2], Site2#site.routing),
    ?assertEqual([], Site0#site.routing).

add_route_rejects_non_route_test() ->
    Site = site("example.org", "c.pem", "k.pem"),
    ?assertError(function_clause, add_route(Site, {not_a_route})).
