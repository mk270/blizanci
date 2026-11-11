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
         global/2, site/3, add_route/2, insert_route/3, routes/1,
         new/2, validate/1, apply/1, ascii_lowercase/1]).

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
              when Port     :: term(),
                   Servlets :: term(),
                   Result   :: global_config().
global(Port, Servlets) ->
    #global{port = Port, servlets = Servlets}.


% A site with no routes yet: fully self-contained, knowing nothing of
% any other site. Add routes with add_route/2.
%
% Hostnames are compared case-insensitively, by being stored in lower
% case (see find_site/2), so a name with an upper-case letter is
% refused here rather than silently altered. Use ascii_lowercase/1
% first if folding is what the caller wants.
-spec site(Hostname, CertFile, KeyFile) -> Result
              when Hostname :: string(),
                   CertFile :: string(),
                   KeyFile  :: string(),
                   Result   :: {'ok', site()}
                             | {'error', 'hostname_not_lowercase'}.
site(Hostname, CertFile, KeyFile) ->
    case Hostname =:= ascii_lowercase(Hostname) of
        false -> {error, hostname_not_lowercase};
        true  -> {ok, #site{hostname = erlang:list_to_binary(Hostname),
                            certfile = CertFile,
                            keyfile  = KeyFile,
                            routing  = [],
                            docroot  = undefined}}
    end.


% Lower-case A-Z only. Not string:lowercase/1, which folds Unicode
% (U+212A, the Kelvin sign, becomes "k"), and a hostname here is ASCII.
% Takes a string or a binary, and returns the same kind. This is the one
% fold for host names and URL schemes, here and in blizanci_request.
-spec ascii_lowercase(String) -> Result
              when String :: string(),
                   Result :: string();
                  (Binary) -> Result
              when Binary :: binary(),
                   Result :: binary().
ascii_lowercase(Binary) when is_binary(Binary) ->
    << <<(ascii_lower_char(C))>> || <<C>> <= Binary >>;
ascii_lowercase(String) ->
    [ ascii_lower_char(C) || C <- String ].

-spec ascii_lower_char(Char) -> Result
              when Char   :: char(),
                   Result :: char().
ascii_lower_char(C) when C >= $A, C =< $Z -> C + ($a - $A);
ascii_lower_char(C) -> C.


% Append a route to a site's routing table. Routes are tried in order,
% first match wins, so this is "append" (iptables -A): a route added
% later is only reached if no earlier one matches.
-spec add_route(Site, Route) -> Result
              when Site   :: site(),
                   Route  :: route(),
                   Result :: site().
add_route(Site = #site{routing = Routing}, Route = #route{}) ->
    Site#site{routing = Routing ++ [Route]}.


% Insert a route so that it becomes the Index'th route (1-based),
% ahead of whatever occupied that position (iptables -I). An Index
% that doesn't name an existing route is an error, not clamped; use
% add_route/2 to append.
-spec insert_route(Site, Index, Route) -> Result
              when Site   :: site(),
                   Index  :: pos_integer(),
                   Route  :: route(),
                   Result :: {'ok', site()} | {'error', term()}.
insert_route(Site = #site{routing = Routing}, Index, Route = #route{})
  when is_integer(Index), Index >= 1, Index =< length(Routing) ->
    {Before, After} = lists:split(Index - 1, Routing),
    {ok, Site#site{routing = Before ++ [Route | After]}};
insert_route(#site{routing = Routing}, Index, #route{}) ->
    {error, {index_out_of_range, Index, length(Routing)}}.


% A site's routes, in the order they are tried.
-spec routes(Site) -> Result
              when Site   :: site(),
                   Result :: [route()].
routes(#site{routing = Routing}) ->
    Routing.


% Combine a global configuration with the sites it serves.
-spec new(Global, Sites) -> Result
              when Global :: global_config(),
                   Sites  :: [site()],
                   Result :: config().
new(Global = #global{}, Sites) when is_list(Sites) ->
    #config{global = Global, sites = Sites}.


% Check a whole configuration, reporting the first problem found.
-spec validate(Config) -> Result
              when Config :: config(),
                   Result :: 'ok' | {'error', term()}.
validate(#config{global = Global, sites = Sites}) ->
    first_error([fun() -> validate_global(Global) end,
                 fun() -> validate_sites(Sites) end,
                 fun() -> validate_routes_use_servlets(Global, Sites) end]).


-spec first_error(Checks) -> Result
              when Checks :: [fun(() -> 'ok' | {'error', term()})],
                   Result :: 'ok' | {'error', term()}.
first_error([]) ->
    ok;
first_error([Check | Rest]) ->
    case Check() of
        ok -> first_error(Rest);
        {error, _} = Error -> Error
    end.


-spec validate_global(Global) -> Result
              when Global :: global_config(),
                   Result :: 'ok' | {'error', term()}.
validate_global(#global{port = Port, servlets = Servlets}) ->
    case is_integer(Port) andalso Port >= 1 andalso Port =< 65535 of
        true  -> validate_servlets(Servlets);
        false -> {error, {invalid_port, Port}}
    end.


-spec validate_servlets(Servlets) -> Result
              when Servlets :: term(),
                   Result   :: 'ok' | {'error', term()}.
validate_servlets(Servlets) when is_list(Servlets) ->
    first_error([fun() -> blizanci_router:check_servlet(S) end
                 || S <- Servlets]);
validate_servlets(Servlets) ->
    {error, {invalid_servlets, Servlets}}.


-spec validate_sites(Sites) -> Result
              when Sites  :: [site()],
                   Result :: 'ok' | {'error', term()}.
validate_sites([]) ->
    {error, no_sites};
validate_sites(Sites) ->
    Hostnames = [ S#site.hostname || S <- Sites ],
    case Hostnames -- lists:usort(Hostnames) of
        [] -> first_error([fun() -> validate_site(S) end || S <- Sites]);
        [Duplicate | _] -> {error, {duplicate_site, Duplicate}}
    end.


-spec validate_site(Site) -> Result
              when Site   :: site(),
                   Result :: 'ok' | {'error', term()}.
validate_site(#site{hostname = Hostname, certfile = Cert,
                    keyfile = Key, routing = Routing}) ->
    case {blizanci_x509:validate_pem_file(Cert),
          blizanci_x509:validate_pem_file(Key)} of
        {{fail, Why}, _} -> {error, {bad_certfile, Hostname, Why}};
        {_, {fail, Why}} -> {error, {bad_keyfile, Hostname, Why}};
        _ ->
            case blizanci_router:check_upload_dirs(Routing) of
                ok -> ok;
                {error, Why} -> {error, {bad_routes, Hostname, Why}}
            end
    end.


% A route may only name a servlet the global config declares, since only
% declared servlets have their pools started.
-spec validate_routes_use_servlets(Global, Sites) -> Result
              when Global :: global_config(),
                   Sites  :: [site()],
                   Result :: 'ok' | {'error', term()}.
validate_routes_use_servlets(#global{servlets = Servlets}, Sites) ->
    Undeclared = [ {S#site.hostname, R#route.module}
                   || S <- Sites, R <- S#site.routing,
                      not lists:member(R#route.module, Servlets) ],
    case Undeclared of
        [] -> ok;
        [{Hostname, Module} | _] ->
            {error, {undeclared_servlet, Hostname, Module}}
    end.


% Apply a configuration to the running instance, without restarting its
% listener: the sites (certificates, routes) and the declared servlet
% modules are replaced for every connection accepted from now on, while
% connections already in progress finish with what they started with.
% The start/0 of each added servlet is called before the switch and the
% stop/0 of each removed one after it, so a route never names a servlet
% whose resources aren't set up; a connection that is mid-request on a
% removed servlet may be cut off.
%
% The port is bound once when the listener opens, so a change of port
% is refused rather than applied. Nothing is applied if the instance
% isn't running, or the configuration doesn't validate.
-spec apply(Config) -> Result
              when Config :: config(),
                   Result :: 'ok' | {'error', term()}.
apply(Config = #config{global = #global{port = Port, servlets = Servlets},
                       sites = Sites}) ->
    case validate(Config) of
        ok -> swap(blizanci_conf_serv:current(), Port, Servlets, Sites);
        {error, _} = Error -> Error
    end.


-spec swap(Running, Port, Servlets, Sites) -> Result
              when Running  :: {'ok', blizanci_conf_serv:running()}
                             | 'not_running',
                   Port     :: integer(),
                   Servlets :: [module()],
                   Sites    :: [site()],
                   Result   :: 'ok' | {'error', term()}.
swap(not_running, _Port, _Servlets, _Sites) ->
    {error, not_running};
swap({ok, #{port := OldPort}}, Port, _Servlets, _Sites)
  when OldPort =/= Port ->
    {error, {port_change_requires_restart, OldPort, Port}};
swap({ok, #{servlets := OldServlets}}, Port, Servlets, Sites) ->
    Added   = [ S || S <- Servlets, not lists:member(S, OldServlets) ],
    Removed = [ S || S <- OldServlets, not lists:member(S, Servlets) ],
    case start_servlets(Added, []) of
        ok ->
            ok = blizanci_conf_serv:publish(Port, Servlets, Sites),
            [ ok = S:stop() || S <- Removed ],
            ok;
        {error, _} = Error ->
            Error
    end.


% Call start/0 on each servlet in turn; if one fails, call stop/0 on
% those already started so a failed apply leaves nothing behind.
-spec start_servlets(Servlets, Started) -> Result
              when Servlets :: [module()],
                   Started  :: [module()],
                   Result   :: 'ok' | {'error', term()}.
start_servlets([], _Started) ->
    ok;
start_servlets([Servlet | Rest], Started) ->
    try Servlet:start() of
        ok -> start_servlets(Rest, [Servlet | Started])
    catch
        Class:Reason ->
            [ ok = S:stop() || S <- Started ],
            {error, {servlet_start_failed, Servlet, {Class, Reason}}}
    end.


% Build one site from explicit values.
-spec make_site(Hostname, Cert, Key, Docroot, Routes) -> Result
              when Hostname :: string(),
                   Cert     :: string(),
                   Key      :: string(),
                   Docroot  :: string(),
                   Routes   :: [route_entry()],
                   Result   :: {'ok', site()}
                             | {'error', 'hostname_not_lowercase'}.
make_site(Hostname, Cert, Key, Docroot, Routes) ->
    {ok, Routing} = blizanci_router:prepare(Routes),
    case site(Hostname, Cert, Key) of
        {ok, Site} -> {ok, Site#site{routing = Routing, docroot = Docroot}};
        {error, _} = Error -> Error
    end.


-spec find_site(Hostname, Sites) -> Result
              when Hostname :: binary(),
                   Sites    :: [site()],
                   Result   :: {'ok', site()} | 'not_found'.
find_site(Hostname, Sites) ->
    % Site names are stored in lower case (see site/3), so fold the name
    % being looked up. This is the one place SNI and request lookups
    % get their case-insensitivity.
    Folded = ascii_lowercase(Hostname),
    case lists:keyfind(Folded, #site.hostname, Sites) of
        false -> not_found;
        Site -> {ok, Site}
    end.


-spec ssl_opts(Port) -> Result
              when Port   :: integer(),
                   Result :: [{atom(), term()}].
%% @doc
%% Generate the options required for ranch to initialise SSL
%% @end
ssl_opts(Port) ->
    VerifyFn = fun (ClientCert, Ev, Init) ->
                   blizanci_x509:verify_cert(ClientCert, Ev, Init) end,

    % No top-level certfile/keyfile: sni_fun is the sole source of TLS
    % credentials, so a client that doesn't send SNI (or sends SNI for
    % a hostname that isn't one of the configured sites) has nothing
    % to complete a handshake against and is refused at the TLS layer,
    % before any Gemini request is ever parsed. ranch fixes these
    % options when the listener opens, so the sites are looked up in
    % blizanci_conf_serv at handshake time, which is what lets apply/1
    % change them without restarting the listener.
    SniFun = fun(SniHostname) ->
                 case blizanci_conf_serv:find_site(
                        erlang:list_to_binary(SniHostname)) of
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

-spec proto_opts(Port) -> Result
              when Port   :: integer(),
                   Result :: [{atom(), term()}].
%% @doc
%% Generate the options required for ranch to initialise the TCP listener
%% @end
proto_opts(Port) ->
    [{port, Port}].


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
      ssl_opts   => ssl_opts(Port),
      proto_opts => proto_opts(Port),
      servlets   => Servlets,
      port       => Port,
      sites      => Sites
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
    % The operator's name, or the machine's own, may have capitals: fold
    % it here, at the edge, so that site/3 can insist on lower case.
    Hostname = ascii_lowercase(application:get_env(App, hostname,
                                                   Default_Hostname)),
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
    {ok, Site} = make_site(Hostname, Cert, Key, Docroot, Routes),
    Sites = [Site],
    Port = application:get_env(App, port, ?PORT),
    Servlets = application:get_env(App, servlets, ?DEFAULT_SERVLETS),
    make(Port, Servlets, Sites).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(TEST_DOCROOT, "test-docroot").

test_site(Hostname, Cert, Key) ->
    {ok, Site} = site(Hostname, Cert, Key),
    Site.

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
    % some of these arguments deliberately break the spec, so route/5 is
    % called through apply/3 to keep dialyzer from objecting
    BadRoute = fun(Proto, Regex, Module, Authz, RouteOpts) ->
                   apply(?MODULE, route,
                         [Proto, Regex, Module, Authz, RouteOpts])
               end,
    [?_assertMatch({error, {invalid_proto, gopher}},
                   route(gopher, "x", blizanci_static, public, Opts)),
     ?_assertMatch({error, {invalid_regex, "(", _}},
                   route(gemini, "(", blizanci_static, public, Opts)),
     ?_assertMatch({error, {invalid_regex, 42, not_a_string}},
                   BadRoute(gemini, 42, blizanci_static, public, Opts)),
     ?_assertEqual({error, {unknown_servlet, no_such_servlet}},
                   route(gemini, "x", no_such_servlet, public, Opts)),
     ?_assertEqual({error, {not_a_servlet, lists}},
                   route(gemini, "x", lists, public, Opts)),
     ?_assertEqual({error, {not_a_servlet, "blizanci_static"}},
                   BadRoute(gemini, "x", "blizanci_static", public, Opts)),
     ?_assertEqual({error, {invalid_authz_policy, bogus}},
                   BadRoute(gemini, "x", blizanci_static, bogus, Opts)),
     ?_assertEqual({error, {invalid_route_opts, not_a_map}},
                   BadRoute(gemini, "x", blizanci_static, public, not_a_map)),
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
                 test_site("example.org", "c.pem", "k.pem")).

site_rejects_uppercase_test_() ->
    [?_assertEqual({error, hostname_not_lowercase},
                   site("Example.org", "c.pem", "k.pem")),
     ?_assertEqual({error, hostname_not_lowercase},
                   site("EXAMPLE.ORG", "c.pem", "k.pem")),
     ?_assertMatch({ok, #site{hostname = <<"example.org">>}},
                   site("example.org", "c.pem", "k.pem"))].

ascii_lowercase_test_() ->
    [?_assertEqual("example.org", ascii_lowercase("Example.ORG")),
     ?_assertEqual("a-1.b", ascii_lowercase("A-1.B")),
     % only A-Z: the Kelvin sign is left alone, not folded to "k"
     ?_assertEqual([16#212A], ascii_lowercase([16#212A])),
     ?_assertEqual(<<"example.org">>, ascii_lowercase(<<"Example.ORG">>)),
     ?_assertEqual(<<"a-1.b">>, ascii_lowercase(<<"A-1.B">>)),
     ?_assertEqual(<<>>, ascii_lowercase(<<>>)),
     ?_assertEqual(<<226,132,170>>, ascii_lowercase(<<226,132,170>>)),
     ?_assertEqual(<<"a", 226,132,170, "b">>,
                   ascii_lowercase(<<"A", 226,132,170, "B">>))].

add_route_appends_test() ->
    Opts = #{ docroot => ?TEST_DOCROOT },
    {ok, R1} = route(gemini, "a", blizanci_static, public, Opts),
    {ok, R2} = route(gemini, "b", blizanci_static, restricted, Opts),
    Site0 = test_site("example.org", "c.pem", "k.pem"),
    Site1 = add_route(Site0, R1),
    Site2 = add_route(Site1, R2),
    ?assertEqual([R1], Site1#site.routing),
    ?assertEqual([R1, R2], Site2#site.routing),
    ?assertEqual([], Site0#site.routing).

add_route_rejects_non_route_test() ->
    Site = test_site("example.org", "c.pem", "k.pem"),
    % (through apply/3, as the argument deliberately breaks the spec)
    ?assertError(function_clause,
                 apply(?MODULE, add_route, [Site, {not_a_route}])).

find_site_test_() ->
    A = test_site("a.example", "a.pem", "a.key"),
    B = test_site("b.example", "b.pem", "b.key"),
    [?_assertEqual({ok, A}, find_site(<<"a.example">>, [A, B])),
     ?_assertEqual({ok, B}, find_site(<<"b.example">>, [A, B])),
     ?_assertEqual(not_found, find_site(<<"c.example">>, [A, B])),
     ?_assertEqual({ok, A}, find_site(<<"A.EXAMPLE">>, [A, B])),
     ?_assertEqual({ok, B}, find_site(<<"B.Example">>, [A, B])),
     ?_assertEqual(not_found, find_site(<<"a.example.">>, [A, B])),
     ?_assertEqual(not_found, find_site(<<"a.example">>, []))].

make_test() ->
    Site = test_site("a.example", "a.pem", "a.key"),
    Config = make(1966, [blizanci_static], [Site]),
    ?assertEqual([blizanci_static], maps:get(servlets, Config)),
    SSL = maps:get(ssl_opts, Config),
    Proto = maps:get(proto_opts, Config),
    ?assertEqual(1966, proplists:get_value(port, SSL)),
    ?assertEqual([{port, 1966}], Proto),
    ?assertEqual(1966, maps:get(port, Config)),
    ?assertEqual([Site], maps:get(sites, Config)).

% SNI is the only source of TLS credentials: no top-level cert/key, and
% the sni_fun serves exactly the configured sites.
sni_fun_test_() ->
    A = test_site("a.example", "a.pem", "a.key"),
    B = test_site("b.example", "b.pem", "b.key"),
    SSL = ssl_opts(1965),
    SniFun = proplists:get_value(sni_fun, SSL),
    {setup,
     fun() -> live_up(1965, [], [A, B]) end,
     fun(_) -> live_down() end,
     [?_assertEqual([{certfile, "a.pem"}, {keyfile, "a.key"}],
                    SniFun("a.example")),
      ?_assertEqual([{certfile, "b.pem"}, {keyfile, "b.key"}],
                    SniFun("b.example")),
      ?_assertEqual(unrecognized, SniFun("evil.example")),
      % a name that isn't ours is refused whatever its case
      ?_assertEqual(unrecognized, SniFun("EVIL.EXAMPLE")),
      % and a name with a trailing dot (not valid in an SNI) is refused
      ?_assertEqual(unrecognized, SniFun("a.example.")),
      ?_assertEqual(unrecognized, SniFun("A.EXAMPLE.")),
      ?_assertEqual(undefined, proplists:get_value(certfile, SSL)),
      ?_assertEqual(undefined, proplists:get_value(keyfile, SSL)),
      ?_assertEqual(['tlsv1.3'], proplists:get_value(versions, SSL)),
      % the same sni_fun sees a replaced site list: this is what lets
      % apply/1 change certificates on a running listener
      ?_test(begin
                 ok = blizanci_conf_serv:publish(1965, [], [B]),
                 ?assertEqual(unrecognized, SniFun("a.example")),
                 ?assertEqual([{certfile, "b.pem"}, {keyfile, "b.key"}],
                              SniFun("b.example"))
             end)]}.

%% A host name is case-insensitive, so the same site is found whatever
%% the case of the name the client sent in its SNI.
sni_fun_case_insensitive_test_() ->
    A = test_site("a.example", "a.pem", "a.key"),
    B = test_site("b.example", "b.pem", "b.key"),
    SniFun = proplists:get_value(sni_fun, ssl_opts(1965)),
    {setup,
     fun() -> live_up(1965, [], [A, B]) end,
     fun(_) -> live_down() end,
     [?_assertEqual([{certfile, "a.pem"}, {keyfile, "a.key"}],
                    SniFun("A.EXAMPLE")),
      ?_assertEqual([{certfile, "a.pem"}, {keyfile, "a.key"}],
                    SniFun("A.Example")),
      ?_assertEqual([{certfile, "b.pem"}, {keyfile, "b.key"}],
                    SniFun("B.example"))]}.

%% Builder: routes and configuration.

insert_route_test_() ->
    Opts = #{ docroot => ?TEST_DOCROOT },
    {ok, R1} = route(gemini, "a", blizanci_static, public, Opts),
    {ok, R2} = route(gemini, "b", blizanci_static, public, Opts),
    {ok, R3} = route(gemini, "c", blizanci_static, public, Opts),
    Site = add_route(add_route(test_site("h", "c.pem", "k.pem"), R1), R2),
    [?_assertEqual({ok, [R3, R1, R2]},
                   rmap(insert_route(Site, 1, R3))),
     ?_assertEqual({ok, [R1, R3, R2]},
                   rmap(insert_route(Site, 2, R3))),
     % past the end is an error, not an append
     ?_assertEqual({error, {index_out_of_range, 3, 2}},
                   insert_route(Site, 3, R3)),
     ?_assertEqual({error, {index_out_of_range, 0, 2}},
                   apply(?MODULE, insert_route, [Site, 0, R3])),
     ?_assertEqual({error, {index_out_of_range, 1, 0}},
                   insert_route(test_site("h", "c.pem", "k.pem"), 1, R3)),
     ?_assertEqual([R1, R2], routes(Site))].

rmap({ok, Site}) -> {ok, routes(Site)};
rmap(Error) -> Error.

new_test() ->
    G = global(1965, [blizanci_static]),
    S = test_site("a.example", "c.pem", "k.pem"),
    ?assertEqual(#config{global = G, sites = [S]}, new(G, [S])).

%% validate/1. Uses a real PEM file so the cert/key checks can pass.
validate_test_() ->
    {setup, fun write_test_pem/0, fun file:delete/1, fun validate_tests/1}.

validate_tests(Pem) ->
    {ok, R} = route(gemini, "(?<PATH>.*)", blizanci_static, public,
                    #{ docroot => ?TEST_DOCROOT }),
    Good = fun(Host) -> add_route(test_site(Host, Pem, Pem), R) end,
    G = global(1965, [blizanci_static]),
    V = fun(Gl, Sites) -> validate(new(Gl, Sites)) end,
    [?_assertEqual(ok, V(G, [Good("a"), Good("b")])),
     ?_assertEqual({error, {invalid_port, 0}},
                   V(global(0, [blizanci_static]), [Good("a")])),
     ?_assertEqual({error, {invalid_port, <<"x">>}},
                   V(global(<<"x">>, [blizanci_static]), [Good("a")])),
     ?_assertEqual({error, {unknown_servlet, nope}},
                   V(global(1965, [nope]), [Good("a")])),
     ?_assertEqual({error, no_sites}, V(G, [])),
     ?_assertEqual({error, {duplicate_site, <<"a">>}},
                   V(G, [Good("a"), Good("a")])),
     % a site with no routes is valid
     ?_assertEqual(ok, V(G, [test_site("a", Pem, Pem)])),
     ?_assertEqual({error, {bad_certfile, <<"a">>, couldnt_open_pem_file}},
                   V(G, [add_route(test_site("a", "/no/such.pem", Pem), R)])),
     ?_assertEqual({error, {bad_keyfile, <<"a">>, couldnt_open_pem_file}},
                   V(G, [add_route(test_site("a", Pem, "/no/such.pem"), R)])),
     ?_assertEqual({error, {undeclared_servlet, <<"a">>, blizanci_static}},
                   V(global(1965, []), [Good("a")]))].

write_test_pem() ->
    Unique = integer_to_list(erlang:unique_integer([positive])),
    Path = filename:absname("validate_test_" ++ Unique ++ ".pem"),
    Pem = public_key:pem_encode([{'Certificate', <<1, 2, 3>>,
                                  not_encrypted}]),
    ok = file:write_file(Path, Pem),
    Path.

%% apply/1.

%% Stand up / tear down the live configuration, as blizanci_sup would.
live_up(Port, Servlets, Sites) ->
    live_down(),
    {ok, _} = blizanci_conf_serv:start_link(#{port => Port,
                                         servlets => Servlets,
                                         sites => Sites}),
    ok.

live_down() ->
    case whereis(blizanci_conf_serv) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end.

apply_test_() ->
    {setup, fun write_test_pem/0,
     fun(Pem) -> file:delete(Pem), live_down() end,
     fun apply_tests/1}.

apply_tests(Pem) ->
    {ok, R} = route(gemini, "(?<PATH>.*)", blizanci_static, public,
                    #{ docroot => ?TEST_DOCROOT }),
    Site = fun(Host) -> add_route(test_site(Host, Pem, Pem), R) end,
    Conf = fun(Port, Servlets, Sites) ->
               new(global(Port, Servlets), Sites)
           end,
    Reset = fun() -> live_down() end,
    [?_test(begin
                Reset(),
                ?assertEqual({error, not_running},
                             apply(Conf(1965, [blizanci_static],
                                        [Site("a")])))
            end),
     ?_test(begin
                Reset(),
                live_up(1965, [blizanci_static], [Site("a")]),
                ?assertEqual({error, no_sites},
                             apply(Conf(1965, [blizanci_static], []))),
                % an invalid configuration changes nothing
                ?assertEqual([Site("a")], blizanci_conf_serv:sites())
            end),
     ?_test(begin
                Reset(),
                live_up(1965, [blizanci_static], [Site("a")]),
                ?assertEqual(ok, apply(Conf(1965, [blizanci_static],
                                            [Site("b"), Site("c")]))),
                Find = fun(Host) -> blizanci_conf_serv:find_site(Host) end,
                ?assertEqual(not_found, Find(<<"a">>)),
                ?assertMatch({ok, _}, Find(<<"b">>)),
                ?assertMatch({ok, _}, Find(<<"c">>))
            end),
     ?_test(begin
                Reset(),
                live_up(1965, [blizanci_static], [Site("a")]),
                ?assertEqual({error, {port_change_requires_restart,
                                      1965, 1966}},
                             apply(Conf(1966, [blizanci_static],
                                        [Site("b")]))),
                ?assertEqual({ok, #{port => 1965,
                                    servlets => [blizanci_static],
                                    sites => [Site("a")]}},
                             blizanci_conf_serv:current())
            end)].

%% Servlet pools start and stop as the declared list changes.
apply_servlets_test_() ->
    {setup,
     fun() ->
         {ok, _} = application:ensure_all_started(ppool),
         write_test_pem()
     end,
     fun(Pem) ->
         live_down(),
         file:delete(Pem)
     end,
     fun(Pem) ->
         ?_test(begin
             {ok, R} = route(gemini, "(?<PATH>.*)", blizanci_static,
                             public, #{ docroot => ?TEST_DOCROOT }),
             Site = add_route(test_site("a", Pem, Pem), R),
             Conf = fun(Servlets) ->
                        new(global(1965, Servlets), [Site])
                    end,
             live_up(1965, [blizanci_static], [Site]),
             ?assertEqual(undefined, whereis(blizanci_cgi)),
             ?assertEqual(ok, apply(Conf([blizanci_static,
                                          blizanci_cgi]))),
             ?assertNotEqual(undefined, whereis(blizanci_cgi)),
             ?assertEqual(ok, apply(Conf([blizanci_static]))),
             ?assertEqual(undefined, whereis(blizanci_cgi)),
             ?assertMatch({ok, #{servlets := [blizanci_static]}},
                          blizanci_conf_serv:current()),
             live_down()
         end)
     end}.

%% validate/1 refuses a site whose routes let a Titan upload land in a
%% CGI directory.
validate_upload_in_cgi_dir_test_() ->
    {setup,
     fun() -> {write_test_pem(), make_test_dir()} end,
     fun({Pem, Dir}) -> file:delete(Pem), file:del_dir_r(Dir) end,
     fun({Pem, Dir}) ->
         {ok, C} = route(gemini, "c", blizanci_cgi, public,
                         #{ cgiroot => Dir }),
         {ok, T} = route(titan, "t", blizanci_titan, public,
                         #{ docroot => Dir, work_dir => Dir }),
         Site = add_route(add_route(test_site("a", Pem, Pem), C), T),
         G = global(1965, [blizanci_cgi, blizanci_titan]),
         ?_assertMatch({error, {bad_routes, <<"a">>,
                                {upload_dir_overlaps_cgiroot, _, _, _}}},
                       validate(new(G, [Site])))
     end}.

make_test_dir() ->
    Unique = integer_to_list(erlang:unique_integer([positive])),
    Dir = filename:absname("validate_dir_test_" ++ Unique),
    ok = filelib:ensure_path(Dir),
    Dir.
