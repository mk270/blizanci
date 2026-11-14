%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

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
%% At start-up, from_app_env/0 builds the configuration from the
%% application environment and validates it, reporting every problem
%% found in one pass as a list (see describe_error/1 for the one-line
%% text of each); a bad configuration stops the application starting.
%% A site's certificate and private key must be a matching pair, or the
%% site is refused; a pair whose key type cannot be compared is accepted
%% with a warning.
%%
%% Configuration items:
%%
%% <dl>
%%   <dt><code>{ca_certs, [CACertPath::string()}]</code></dt>
%%   <dd>the path(s) to the certificate authority certificate(s)
%%       to be used for verifying client certificates;
%%       may be the empty list. A file may hold several certificates
%%       (a bundle), all of which are trusted. The files are read when
%%       the route is built; a change to one takes effect only on
%%       apply/1 or a restart.</dd>
%%   <dt><code>{realms, [{Name::binary(), [CACertPath::string()]}]}</code>
%%   </dt>
%%   <dd>authentication realms: named sets of CA certificates, global to
%%       all sites. A route names them as <code>{private, {realm,
%%       Name}}</code> or a list of those. <code>ca_certs</code> is
%%       shorthand for a realm named <code>&lt;&lt;"default"&gt;&gt;</code>;
%%       giving both it and a realm of that name is an error.</dd>
%%   <dt><code>{certfile, CertPath::string()}</code></dt>
%%   <dd>the path to the server's certificate</dd>
%%   <dt><code>{cgiroot, CGIRootPath::string()}</code></dt>
%%   <dd>the path to the root directory for CGI scripts; normally there would only be a single, top-level directory with no subdirectories</dd>
%%   <dt><code>{docroot, DocrootPath::string()}</code></dt>
%%   <dd>the path to the root of the documents to be served</dd>
%%   <dt><code>{hostname, Hostname::string()}</code></dt>
%%   <dd>the hostname, folded to lower case (defaults to system hostname)</dd>
%%   <dt><code>{keyfile, PrivKeyPath::string()}</code></dt>
%%   <dd>the path to the server's private key</dd>
%%   <dt><code>{port, Port::integer()}</code></dt>
%%   <dd>the port to listen on (defaults to 1965); 1 to 65535, so 0 is
%%       refused</dd>
%%   <dt><code>{routing, [Route]}</code></dt>
%%   <dd>the routing table: an ordered list, first match wins, one
%%       protocol per route. Each entry is
%%       <code>{mount, Proto, Path, Module, Auth, Opts}</code> (whole
%%       path segments; the servlet's PATH is the remainder after the
%%       mount), <code>{regex, Proto, Re, Module, Auth, Opts}</code>
%%       (anchored; needs a named group PATH), or the old
%%       <code>{Proto, Regex, Module, Auth, Opts}</code>, translated with
%%       its unanchored behaviour and one start-up warning. FULL_PATH is
%%       the whole request path. See blizanci_router:prepare/3 and the
%%       README, Routing. Without this key the default table is
%%       mounts /cgi-bin and /, plus a Titan route if titan_work_dir is
%%       set.</dd>
%%   <dt><code>{sites, [{Hostname::string(), [Option]}]}</code></dt>
%%   <dd>several sites on one port, instead of the single-site keys. A
%%       site takes <code>certfile</code>, <code>keyfile</code> and
%%       <code>routing</code> only, all three required; there is no
%%       default routing table and no shorthand key (docroot, cgiroot,
%%       titan_work_dir, ca_certs). <code>port</code>,
%%       <code>servlets</code> and <code>realms</code> stay global.
%%       Giving <code>sites</code> together with any single-site key
%%       (hostname, certfile, keyfile, docroot, cgiroot, ca_certs,
%%       titan_work_dir, routing) is refused. Hostnames are folded to
%%       lower case; a name given twice is an error. See the README,
%%       Several sites.</dd>
%%   <dt><code>{titan_work_dir, WorkDir::string()}</code></dt>
%%   <dd>opts in to the Titan route of the default routing table (ignored
%%       if routing is set): an absolute path to an existing directory on
%%       the docroot's filesystem. Titan has no default work directory.
%%       The route requires a client certificate from the realm
%%       <code>&lt;&lt;"default"&gt;&gt;</code>.</dd>
%% </dl>
%%
%% Relative paths are allowed in all configuration items, including the
%% docroot and cgiroot options of routing entries.
%%
%% @end

-module(blizanci_config).
-include("blizanci_types.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([from_app_env/0, make/3, find_site/2, route/2, route/5, route/6,
         realm/1, add_realm/3, realms/0, realms/1,
         global/2, site/3, add_route/2, insert_route/3, routes/1,
         new/2, validate/1, validate_all/1, apply/1, ascii_lowercase/1,
         describe_error/1, tls_versions/0, site_wants_client_cert/1]).

% A site record with no check on its files, for tests of everything but
% the files themselves; site/3 refuses a certificate and key that do not
% match.
-ifdef(TEST).
-export([test_site/3]).
-endif.

% The default port: the one assigned to Gemini.
-define(PORT, 1965).

% The highest valid TCP port number.
-define(MAX_PORT, 65535).
-define(DEFAULT_SERVLETS, [blizanci_static, blizanci_cgi, blizanci_titan]).

% Build and validate one route, eagerly: a bad route fails here, with a
% specific reason, the moment it is constructed rather than later
% inside a whole-config validation.
-spec route(Proto, Regex, Module, AuthPolicy, Opts) -> Result
              when Proto      :: atom(),
                   Regex      :: string() | binary(),
                   Module     :: module(),
                   AuthPolicy :: authorisation_config(),
                   Opts       :: map(),
                   Result     :: {'ok', route()} | {'error', term()}.
route(Proto, Regex, Module, AuthPolicy, Opts) ->
    blizanci_router:build_route(Proto, Regex, Module, AuthPolicy, Opts).


% Build and validate one route given as a tuple in either format: {mount,
% Proto, Path, Module, Auth, Opts}, {regex, Proto, Re, Module, Auth,
% Opts} or the old {Proto, Regex, Module, Auth, Opts} (see
% blizanci_router:prepare/3 for the rules). No warning for the old one.
-spec route(RouteInfo, Realms) -> Result
              when RouteInfo :: tuple(),
                   Realms    :: realm_table(),
                   Result    :: {'ok', route()} | {'error', term()}.
route(RouteInfo, Realms) ->
    blizanci_router:build_route(RouteInfo, Realms).


% As route/5, resolving realms against a table made with realms/1 or
% add_realm/3, at build time.
-spec route(Proto, Regex, Module, AuthPolicy, Opts, Realms) -> Result
              when Proto      :: atom(),
                   Regex      :: string() | binary(),
                   Module     :: module(),
                   AuthPolicy :: authorisation_config(),
                   Opts       :: map(),
                   Realms     :: realm_table(),
                   Result     :: {'ok', route()} | {'error', term()}.
route(Proto, Regex, Module, AuthPolicy, Opts, Realms) ->
    blizanci_router:build_route(Proto, Regex, Module, AuthPolicy, Opts,
                                Realms).


% The smart constructor for a realm name.
-spec realm(Name) -> Result
              when Name   :: term(),
                   Result :: {'ok', realm()} | {'error', term()}.
realm(Name) ->
    blizanci_auth:realm(Name).


% The empty realm table.
-spec realms() -> Result
              when Result :: realm_table().
realms() ->
    #{}.


% A realm table from a list of {Name, CAFiles}, the CA files read and
% decoded now.
-spec realms(Defs) -> Result
              when Defs   :: term(),
                   Result :: {'ok', realm_table()} | {'error', term()}.
realms(Defs) ->
    blizanci_auth:realm_table(Defs).


% Define a realm: a name and the CA files whose certificates it trusts.
% It takes effect for routes built with the returned table, and so at
% apply/1 once they are in a site.
-spec add_realm(Realms, Name, Files) -> Result
              when Realms :: realm_table(),
                   Name   :: term(),
                   Files  :: term(),
                   Result :: {'ok', realm_table()} | {'error', term()}.
add_realm(Realms, Name, Files) ->
    blizanci_auth:add_realm(Realms, Name, Files).


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
                             | {'error', 'hostname_not_lowercase'}
                             | {'error', pair_error()}.
site(Hostname, CertFile, KeyFile) ->
    case Hostname =:= ascii_lowercase(Hostname) of
        false -> {error, hostname_not_lowercase};
        true  ->
            Host = erlang:list_to_binary(Hostname),
            case check_pair(Host, CertFile, KeyFile) of
                {ok, Verified} ->
                    warn_unverified(Verified, Hostname, CertFile,
                                    KeyFile),
                    {ok, #site{hostname = Host,
                               certfile = CertFile,
                               keyfile  = KeyFile,
                               routing  = []}};
                {error, _} = Error -> Error
            end
    end.


% A site's certificate and key must be a matching pair, else TLS fails
% at handshake time, for every client: so it is refused here, naming
% the site and both files.
-type pair_error() :: {'cert_key_mismatch', binary(), term(), term()}
                    | {'bad_certfile', binary(), term()}
                    | {'bad_keyfile', binary(), term()}.

-spec check_pair(Hostname, CertFile, KeyFile) -> Result
              when Hostname :: binary(),
                   CertFile :: term(),
                   KeyFile  :: term(),
                   Result   :: {'ok', 'verified'}
                             | {'ok', {'unverified', term()}}
                             | {'error', pair_error()}.
check_pair(Hostname, CertFile, KeyFile) ->
    case blizanci_x509:cert_key_match(CertFile, KeyFile) of
        ok -> {ok, verified};
        {ok, unverified, Why} -> {ok, {unverified, Why}};
        {error, mismatch} ->
            {error, {cert_key_mismatch, Hostname, CertFile, KeyFile}};
        {error, {certfile, Why}} ->
            {error, {bad_certfile, Hostname, Why}};
        {error, {keyfile, Why}} ->
            {error, {bad_keyfile, Hostname, Why}}
    end.


% A pair of a kind the check cannot compare is accepted, with a warning.
% This is the one place that warns, once per site built by site/3;
% validation of a built site (problems_site/1) is silent about it.
-spec warn_unverified(Verified, Hostname, CertFile, KeyFile) -> ok
              when Verified :: 'verified' | {'unverified', term()},
                   Hostname :: string(),
                   CertFile :: string(),
                   KeyFile  :: string().
warn_unverified({unverified, {unsupported_key_type, Type}}, Hostname,
                CertFile, KeyFile) ->
    logger:warning("site ~s: cannot verify that certificate ~s and "
                   "private key ~s are a pair (unsupported key type ~s)",
                   [Hostname, CertFile, KeyFile, Type]);
warn_unverified(verified, _, _, _) ->
    ok.


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


% Check a whole configuration, reporting the first problem found. See
% validate_all/1 for every problem.
-spec validate(Config) -> Result
              when Config :: config(),
                   Result :: 'ok' | {'error', term()}.
validate(Config) ->
    case validate_all(Config) of
        ok -> ok;
        {error, [First | _]} -> {error, First}
    end.


% Check a whole configuration, reporting every problem found, in a
% stable order: the global part (port, then each servlet), then the sites
% (no sites, duplicate hostnames, then for each site in turn its
% certificate, key and the upload directories of its routes), then
% routes naming a servlet that is not declared. The errors are the
% reasons validate/1 returns; describe_error/1 gives each one a line.
-spec validate_all(Config) -> Result
              when Config :: config(),
                   Result :: 'ok' | {'error', [term()]}.
validate_all(#config{global = Global, sites = Sites}) ->
    case problems_global(Global) ++ problems_sites(Sites)
         ++ problems_servlet_use(Global, Sites) of
        [] -> ok;
        Problems -> {error, Problems}
    end.


-spec problems_global(Global) -> Problems
              when Global   :: global_config(),
                   Problems :: [term()].
problems_global(#global{port = Port, servlets = Servlets}) ->
    PortProblems = case is_integer(Port) andalso Port >= 1
                        andalso Port =< ?MAX_PORT of
                       true  -> [];
                       false -> [{invalid_port, Port}]
                   end,
    PortProblems ++ problems_servlets(Servlets).


-spec problems_servlets(Servlets) -> Problems
              when Servlets  :: term(),
                   Problems  :: [term()].
problems_servlets(Servlets) when is_list(Servlets) ->
    [ Reason || S <- Servlets,
                {error, Reason} <- [blizanci_router:check_servlet(S)] ];
problems_servlets(Servlets) ->
    [{invalid_servlets, Servlets}].


-spec problems_sites(Sites) -> Problems
              when Sites    :: [site()],
                   Problems :: [term()].
problems_sites([]) ->
    [no_sites];
problems_sites(Sites) ->
    Hostnames = [ S#site.hostname || S <- Sites ],
    Duplicates = lists:usort(Hostnames -- lists:usort(Hostnames)),
    [ {duplicate_site, H} || H <- Duplicates ]
    ++ lists:append([ problems_site(S) || S <- Sites ]).


-spec problems_site(Site) -> Problems
              when Site     :: site(),
                   Problems :: [term()].
problems_site(#site{hostname = Hostname, certfile = Cert,
                    keyfile = Key, routing = Routing}) ->
    FileProblems = file_problems(Hostname, Cert, Key),
    % the pair is only testable when both files are readable PEM
    PairProblems = case FileProblems of
                       [] ->
                           [ Reason
                             || {error, Reason}
                                    <- [check_pair(Hostname, Cert, Key)] ];
                       _ -> []
                   end,
    FileProblems ++ PairProblems
    ++ [ {bad_routes, Hostname, Why}
         || {error, Why} <- [blizanci_router:check_upload_dirs(Routing)] ].


% A route may only name a servlet the global config declares, since only
% declared servlets have their pools started.
-spec problems_servlet_use(Global, Sites) -> Problems
              when Global   :: global_config(),
                   Sites    :: [site()],
                   Problems :: [term()].
problems_servlet_use(#global{servlets = Servlets}, Sites)
  when is_list(Servlets) ->
    lists:uniq([ {undeclared_servlet, S#site.hostname, R#route.module}
                 || S <- Sites, R <- S#site.routing,
                    not lists:member(R#route.module, Servlets) ]);
problems_servlet_use(_, _) ->
    [].


% Apply a configuration to the running instance, without restarting its
% listener: the sites (certificates, routes) and the declared servlet
% modules are replaced for every connection accepted from now on, while
% connections already in progress finish with what they started with.
% The start/0 of each added servlet is called before the switch and the
% stop/0 of each removed one after it, so a route never names a servlet
% whose resources aren't set up; a connection that is mid-request on a
% removed servlet may be cut off. A failed start leaves nothing behind.
% The work is done by blizanci_servlets, which owns the servlets'
% resources, so the declared list and the real pools change together.
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
        ok -> blizanci_servlets:reconfigure(Port, Servlets, Sites);
        {error, _} = Error -> Error
    end.


% Build one site from explicit values.
-spec make_site(Hostname, Cert, Key, Routes) -> Result
              when Hostname :: string(),
                   Cert     :: string(),
                   Key      :: string(),
                   Routes   :: [route_entry()],
                   Result   :: {'ok', site()}
                             | {'error', 'hostname_not_lowercase'}
                             | {'error', pair_error()}
                             | {'error', {'invalid_routing', term()}}.
make_site(Hostname, Cert, Key, Routes) ->
    make_site(Hostname, Cert, Key, Routes, #{}).


-spec make_site(Hostname, Cert, Key, Routes, Realms) -> Result
              when Hostname :: string(),
                   Cert     :: string(),
                   Key      :: string(),
                   Routes   :: [route_entry()],
                   Realms   :: realm_table(),
                   Result   :: {'ok', site()}
                             | {'error', 'hostname_not_lowercase'}
                             | {'error', pair_error()}
                             | {'error', {'invalid_routing', term()}}.
make_site(Hostname, Cert, Key, Routes, Realms) ->
    build_site(Hostname, Cert, Key, Routes, Realms, #{}).


% As make_site/5, with options for blizanci_router:prepare/3.
-spec build_site(Hostname, Cert, Key, Routes, Realms, PrepareOpts) ->
              Result
              when Hostname    :: string(),
                   Cert        :: string(),
                   Key         :: string(),
                   Routes      :: [route_entry()],
                   Realms      :: realm_table(),
                   PrepareOpts :: map(),
                   Result      :: {'ok', site()}
                                | {'error', 'hostname_not_lowercase'}
                                | {'error', pair_error()}
                                | {'error', {'invalid_routing', term()}}.
build_site(Hostname, Cert, Key, Routes, Realms, PrepareOpts) ->
    case blizanci_router:prepare(Routes, Realms, PrepareOpts) of
        {ok, Routing} ->
            case site(Hostname, Cert, Key) of
                {ok, Site} ->
                    {ok, Site#site{routing = Routing}};
                {error, _} = Error -> Error
            end;
        {error, Reason} -> {error, {invalid_routing, Reason}}
    end.


% A one-line, human-readable account of an error from make_site/4 or
% from_app_env/0, or from starting the supervision tree (a servlet that
% will not start), for the log at start-up.
-spec describe_error(Error) -> Result
              when Error  :: term(),
                   Result :: string().
describe_error({invalid_routing, Reason}) ->
    "invalid routing table: " ++ describe_route_error(Reason);
describe_error(ca_certs_and_default_realm) ->
    "ca_certs is shorthand for the realm \"default\", so the realms key "
    "must not also define one of that name";
describe_error(hostname_not_lowercase) ->
    "hostname is not lower case";
describe_error({cert_key_mismatch, Hostname, CertFile, KeyFile}) ->
    flat("site ~s: certificate ~s and private key ~s are not a "
         "matching pair", [Hostname, CertFile, KeyFile]);
describe_error({shutdown, {failed_to_start_child, blizanci_servlets,
                           Reason}}) ->
    describe_error(Reason);
describe_error({servlet_start_failed, Servlet, Reason}) ->
    flat("servlet ~0p could not start: ~0p", [Servlet, Reason]);
describe_error({invalid_configuration, Errors}) when is_list(Errors) ->
    lists:flatten(lists:join("; ", [ describe_error(E) || E <- Errors ]));
% a reason that already names the site is not given the prefix twice
describe_error({site, _Hostname, Reason})
  when element(1, Reason) =:= cert_key_mismatch;
       element(1, Reason) =:= bad_certfile;
       element(1, Reason) =:= bad_keyfile ->
    describe_error(Reason);
describe_error({site, Hostname, Reason}) ->
    flat("site ~s: ~s", [host_text(Hostname), describe_error(Reason)]);
describe_error({invalid_hostname, Value}) ->
    flat("hostname ~0p is not a non-empty string", [Value]);
describe_error({bad_realm, Name, Reason}) ->
    flat("realm ~0p: ~s", [Name, describe_realm_error(Reason)]);
describe_error({realms, Reason}) ->
    flat("realms: ~s", [describe_realm_error(Reason)]);
describe_error({bad_certfile, Hostname, Why}) ->
    flat("site ~s: the certificate file is not a readable PEM file (~0p)",
         [host_text(Hostname), Why]);
describe_error({bad_keyfile, Hostname, Why}) ->
    flat("site ~s: the key file is not a readable PEM file (~0p)",
         [host_text(Hostname), Why]);
describe_error({bad_routes, Hostname, Reason}) ->
    flat("site ~s: ~s",
         [host_text(Hostname), describe_route_error(Reason)]);
describe_error({undeclared_servlet, Hostname, Module}) ->
    flat("site ~s: a route uses servlet ~0p, which is not in the "
         "servlets list", [host_text(Hostname), Module]);
describe_error({duplicate_site, Hostname}) ->
    flat("site ~s is configured more than once", [host_text(Hostname)]);
describe_error(no_sites) ->
    "no sites are configured";
describe_error({sites_and_single_site_keys, Keys}) ->
    flat("the sites key cannot be combined with the single-site keys "
         "~s; use one or the other",
         [lists:join(", ", [ atom_to_list(K) || K <- Keys ])]);
describe_error({invalid_sites, Value}) ->
    flat("sites must be a list of {Hostname, Options}, not ~0p", [Value]);
describe_error({invalid_site_entry, Entry}) ->
    flat("sites entry ~0p is not a {Hostname, Options} pair", [Entry]);
describe_error({site_options_not_a_list, Value}) ->
    flat("the site's options must be a list, not ~0p", [Value]);
describe_error({unknown_site_key, Key}) ->
    flat("~0p is not a key of a site (a site takes only certfile, "
         "keyfile and routing)", [Key]);
describe_error({duplicate_site_key, Key}) ->
    flat("the key ~0p is given more than once", [Key]);
describe_error({missing_site_key, Key}) ->
    flat("the required key ~0p is missing", [Key]);
describe_error({invalid_site_value, Key, Value}) ->
    flat("~0p must be a non-empty file name string, not ~0p",
         [Key, Value]);
describe_error({invalid_port, Port}) ->
    flat("port ~0p is not a TCP port number (1 to ~b)", [Port, ?MAX_PORT]);
describe_error({invalid_servlets, Servlets}) ->
    flat("servlets must be a list of modules, not ~0p", [Servlets]);
describe_error({Why, _} = Reason)
  when Why =:= unknown_servlet; Why =:= not_a_servlet ->
    describe_route_error(Reason);
describe_error({servlet_missing_callbacks, _, _} = Reason) ->
    describe_route_error(Reason);
describe_error(Other) ->
    flat("configuration error: ~0p", [Other]).

-spec describe_route_error(Reason) -> Result
              when Reason :: term(),
                   Result :: string().
describe_route_error({bad_route, Index, Kind, Reason}) ->
    flat("route ~b (~s): ~s",
         [Index, route_kind_name(Kind), describe_route_error(Reason)]);
describe_route_error(invalid_route) ->
    flat("not a route; expected {mount, Proto, Path, Module, Auth, Opts}, "
         "{regex, Proto, Re, Module, Auth, Opts} or the old "
         "{Proto, Regex, Module, Auth, Opts}", []);
describe_route_error({invalid_mount, Path, Why}) ->
    flat("mount path ~0p is invalid (~s); it must be a non-empty string "
         "starting with \"/\", with no empty, \".\" or \"..\" segments "
         "and no control characters",
         [Path, mount_problem(Why)]);
describe_route_error({reserved_group, Re, Name}) ->
    flat("regex ~0p has a group named ~s, which is reserved for the "
         "full request path", [Re, Name]);
describe_route_error({route_without_path_group, Regex, Module}) ->
    flat("route ~0p (servlet ~0p) has no named group PATH in its regex; "
         "add one, e.g. (?<PATH>.*)", [Regex, Module]);
describe_route_error({invalid_route_options, blizanci_titan,
                      work_dir_missing}) ->
    flat("the Titan route has no work_dir; set work_dir in its options "
         "to an existing absolute directory on the docroot's filesystem",
         []);
describe_route_error({invalid_route_options, blizanci_titan,
                      {work_dir_not_absolute, Dir}}) ->
    flat("the Titan route's work_dir ~ts is not an absolute path; set it "
         "to an existing absolute directory", [Dir]);
describe_route_error({invalid_route_options, blizanci_titan,
                      {work_dir_not_a_directory, Dir}}) ->
    flat("the Titan route's work_dir ~ts is not a directory; create it, "
         "or set work_dir in the Titan route's options to an existing "
         "directory", [Dir]);
describe_route_error({invalid_route_options, blizanci_titan,
                      {work_dir_other_filesystem, Dir}}) ->
    flat("the Titan route's work_dir ~ts is on a different filesystem "
         "from its docroot; uploads are installed by hard link and "
         "rename, so use a directory on the docroot's filesystem",
         [Dir]);
describe_route_error({unknown_realm, {realm, Name}, Regex, Module}) ->
    flat("route ~0p (servlet ~0p) names realm ~0p, which is not "
         "defined", [Regex, Module, Name]);
describe_route_error({unknown_servlet, Module}) ->
    flat("servlet ~0p cannot be loaded; check the module name and that it "
         "is installed", [Module]);
describe_route_error({not_a_servlet, Module}) ->
    flat("~0p is not a servlet module name", [Module]);
describe_route_error({servlet_missing_callbacks, Module, Missing}) ->
    flat("servlet ~0p lacks callbacks ~0p", [Module, Missing]);
describe_route_error({routing_not_a_list, Value}) ->
    flat("the routing key must be a list of routes, not ~0p", [Value]);
describe_route_error({upload_dir_overlaps_cgiroot, Kind, Dir, Root}) ->
    flat("the Titan ~s ~ts overlaps the CGI root ~ts; uploads must not "
         "land where scripts run", [Kind, Dir, Root]);
describe_route_error({cannot_resolve_dir, Dir, Reason}) ->
    flat("directory ~ts cannot be resolved (~0p)", [Dir, Reason]);
describe_route_error(file_list_in_new_route) ->
    flat("a mount or regex route cannot take a list of CA files in "
         "{private, Files}; define a realm in the realms key and name "
         "it, e.g. {private, {realm, <<\"staff\">>}}", []);
describe_route_error({invalid_ca_file, File, Why}) ->
    flat("a private route's CA file ~ts cannot be used (~0p); check that "
         "it exists and holds a PEM certificate", [File, Why]);
describe_route_error({invalid_route_options, Module, Reason}) ->
    flat("route for servlet ~p has invalid options: ~0p", [Module, Reason]);
describe_route_error(Reason) ->
    flat("~0p", [Reason]).

% An error from building the realm table, in the context "realm X: " or
% "realms: ".
-spec describe_realm_error(Reason) -> Result
              when Reason :: term(),
                   Result :: string().
describe_realm_error({invalid_ca_file, File, Why}) ->
    flat("CA file ~ts cannot be used (~0p); check that it exists and "
         "holds a PEM certificate", [File, Why]);
describe_realm_error({invalid_realm_name, Name}) ->
    flat("~0p is not a valid realm name; it must be a binary of 1 to 64 "
         "printable ASCII characters", [Name]);
describe_realm_error({duplicate_realm, {realm, Name}}) ->
    flat("realm ~0p is defined more than once", [Name]);
describe_realm_error({invalid_realm_files, _}) ->
    "the CA files must be a list of file names";
describe_realm_error({invalid_realm, Def}) ->
    flat("~0p is not a {Name, CAFiles} pair", [Def]);
describe_realm_error({invalid_realms, Value}) ->
    flat("the realms key must be a list of {Name, CAFiles} pairs, not "
         "~0p", [Value]);
describe_realm_error(ca_certs_and_default_realm) ->
    describe_error(ca_certs_and_default_realm);
describe_realm_error(Reason) ->
    flat("~0p", [Reason]).

% A hostname as it appears in a line: the stored binary, or whatever the
% operator wrote if that is not a string.
-spec host_text(Hostname) -> Result
              when Hostname :: term(),
                   Result   :: string().
host_text(Hostname) when is_binary(Hostname) ->
    binary_to_list(Hostname);
host_text(Hostname) ->
    case io_lib:printable_unicode_list(Hostname) of
        true -> Hostname;
        false -> flat("~0p", [Hostname])
    end.

-spec route_kind_name(Kind) -> Result
              when Kind   :: term(),
                   Result :: string().
route_kind_name(mount) -> "mount route";
route_kind_name(regex) -> "regex route";
route_kind_name(tuple) -> "old-format route";
route_kind_name(_) -> "unknown kind of route".

-spec mount_problem(Why) -> Result
              when Why    :: term(),
                   Result :: string().
mount_problem(not_a_string) -> "not a string";
mount_problem(not_utf8) -> "not valid UTF-8";
mount_problem(empty) -> "empty";
mount_problem(not_absolute) -> "does not start with \"/\"";
mount_problem(empty_segment) -> "empty segment";
mount_problem(dot_segment) -> "\".\" or \"..\" segment";
mount_problem(control_character) -> "control character";
mount_problem(Why) -> flat("~0p", [Why]).

-spec flat(Format, Args) -> Result
              when Format :: string(),
                   Args   :: [term()],
                   Result :: string().
flat(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).


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
    % change them (and whether a site asks for a client certificate)
    % without restarting the listener.
    %
    % TLS 1.3 and 1.2 are offered (tls_versions/0); cipher suites and
    % the rest are OTP's defaults. The listener-wide verify_peer asks
    % for a client certificate; a site with only public routes
    % overrides it with verify_none (site_tls_opts/1), so that nothing
    % is sent, in the clear under 1.2, where it is not needed. No CA
    % list is sent, so that self-signed identity certificates are not
    % filtered out by the client; trust is decided per route by
    % blizanci_auth.
    SniFun = fun(SniHostname) ->
                 case blizanci_conf_serv:find_site(
                        erlang:list_to_binary(SniHostname)) of
                     {ok, Site} -> site_tls_opts(Site);
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
     {versions, tls_versions()}
    ].

-spec tls_versions() -> Result
              when Result :: [ssl:protocol_version()].
%% @doc
%% The TLS versions blizanci offers, most preferred first: 1.3 and 1.2.
%% This is the one place the list is set. Cipher suites and the other
%% TLS options are OTP's defaults.
%% @end
tls_versions() ->
    ['tlsv1.3', 'tlsv1.2'].

-spec site_wants_client_cert(Routes) -> Result
              when Routes :: [route()],
                   Result :: boolean().
%% @doc
%% Whether a site with these routes should ask the client for a
%% certificate during the TLS handshake: true if any route, gemini or
%% titan, is restricted or private, false if every route is public
%% (or there are none). The path is not known at handshake time, so one
%% such route means every client is asked. A site that does not ask
%% never has a certificate sent to it, which matters in TLS 1.2, where
%% the client certificate travels unencrypted.
%% @end
site_wants_client_cert(Routes) ->
    lists:any(fun(#route{auth_policy = public}) -> false;
                 (#route{}) -> true
              end, Routes).

-spec site_tls_opts(Site) -> Result
              when Site   :: site(),
                   Result :: [{atom(), term()}].
site_tls_opts(#site{certfile = Cert, keyfile = Key, routing = Routes}) ->
    Creds = [{certfile, Cert}, {keyfile, Key}],
    case site_wants_client_cert(Routes) of
        true -> Creds;
        false -> Creds ++ [{verify, verify_none}]
    end.

-spec proto_opts(Port) -> Result
              when Port   :: integer(),
                   Result :: [{atom(), term()}].
%% @doc
%% Generate the options required for ranch to initialise the TCP listener
%% @end
proto_opts(Port) ->
    [{port, Port}].


-type route_entry() :: tuple().  % see blizanci_router:route_info()

-spec routing_table(Docroot, CGIroot, CACerts) -> RoutingTable
              when Docroot      :: string(),
                   CGIroot      :: string(),
                   CACerts      :: [string()],
                   RoutingTable :: [route_entry()].

routing_table(Docroot, CGIroot, CACerts) ->
    routing_table(Docroot, CGIroot, CACerts, undefined).

% As routing_table/3; the Titan route is added only if a work_dir is
% given, as Titan has no default work directory.
-spec routing_table(Docroot, CGIroot, CACerts, WorkDir) -> RoutingTable
              when Docroot      :: string(),
                   CGIroot      :: string(),
                   CACerts      :: [string()],
                   WorkDir      :: string() | 'undefined',
                   RoutingTable :: [route_entry()].

routing_table(Docroot, CGIroot, CACerts, WorkDir) ->
    % Each mount names its own root, as a servlet's PATH is what follows
    % the mount. CACerts is not used here: the Titan route names the
    % realm "default", which from_app_env/0 defines from ca_certs.
    _ = CACerts,
    [{mount, gemini, "/cgi-bin", blizanci_cgi, public,
      #{ cgiroot => CGIroot }},
     {mount, gemini, "/", blizanci_static, public,
      #{ docroot => Docroot }}]
    ++ titan_route(Docroot, WorkDir).


-spec titan_route(Docroot, WorkDir) -> Routes
              when Docroot :: string(),
                   WorkDir :: string() | 'undefined',
                   Routes  :: [route_entry()].
titan_route(_Docroot, undefined) ->
    [];
titan_route(Docroot, WorkDir) ->
    [{mount, titan, "/", blizanci_titan, {private, {realm, <<"default">>}},
      #{ docroot => Docroot, work_dir => WorkDir }}].


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
%
% Every problem is reported in one pass, as a list of errors, each of
% which describe_error/1 turns into one line. The configuration is built
% without stopping at the first error, and then validate_all/1 is run on
% what was built, so that a bad realm, a bad route, an unreadable
% certificate and an undeclared servlet are all found together. The
% order of the list is stable: first the problems of the realms key, then
% those found building the site (its hostname, then its routes in
% order), each as {site, Hostname, Reason}, then those of
% validate_all/1.
%
% Where one error makes a later check meaningless, it is not allowed to
% cause a second, misleading one:
%  - a route that names a realm whose definition failed to build gets no
%    unknown_realm error (the realm's own error is already listed); if
%    the realms key is so malformed that no name can be said to have
%    failed, no unknown_realm error is reported at all;
%  - a route that failed to build is left out of the site, and the
%    checks that need the routes (upload directories, declared servlets)
%    run on those that did build;
%  - if the site itself cannot be built (a bad hostname, or an error
%    from site/3), it is left out of the configuration, so that the
%    sites are not validated and "no sites" is not reported; only the
%    global checks and the servlets its routes name are.
-spec from_app_env() -> Result
              when Result :: {'ok', map()} | {'error', [term()]}.
from_app_env() ->
    {ok, App} = application:get_application(),
    case application:get_env(App, sites) of
        {ok, Sites} -> sites_from_env(App, Sites);
        undefined -> single_site_from_env(App)
    end.


-spec single_site_from_env(App) -> Result
              when App    :: atom(),
                   Result :: {'ok', map()} | {'error', [term()]}.
single_site_from_env(App) ->
    {ok, Default_Hostname} = inet:gethostname(),
    Cert = application:get_env(App, certfile, "./ssl/certificate.pem"),
    Key = application:get_env(App, keyfile, "./ssl/key.pem"),
    Docroot = application:get_env(App, docroot, "./public_gemini"),
    CGIroot = application:get_env(App, cgiroot, "./cgi-bin"),
    CACerts = application:get_env(App, ca_certs, []),
    TitanWork = application:get_env(App, titan_work_dir, undefined),
    Default_Routing = routing_table(Docroot, CGIroot, CACerts, TitanWork),
    % The default table is in the new format, so only an operator's own
    % routing key can trigger the old-format warning.
    RouteList = case application:get_env(App, routing) of
                    {ok, Own} -> Own;
                    undefined -> Default_Routing
                end,
    Port = application:get_env(App, port, ?PORT),
    Servlets = application:get_env(App, servlets, ?DEFAULT_SERVLETS),
    Global = global(Port, Servlets),
    {Realms, Failed, RealmErrors} = realms_from_env(App, CACerts),
    Outcome = site_from_env(
                application:get_env(App, hostname, Default_Hostname),
                Cert, Key, RouteList, Realms, Failed, #{}),
    assemble(Global, RealmErrors, [Outcome]).


% The keys of the single-site configuration, which the sites key
% replaces: giving both is refused.
-define(SINGLE_SITE_KEYS, [hostname, certfile, keyfile, docroot, cgiroot,
                           ca_certs, titan_work_dir, routing]).

% The keys a site of the sites key takes, all required.
-define(SITE_KEYS, [certfile, keyfile, routing]).

% The sites key. Each site is built independently, by the same code as
% the single site, from its own certfile, keyfile and routing, and the
% realms (and port, servlets) are the global keys. The sites key with
% any key of the single-site configuration is refused, as one error
% naming the keys, with nothing else reported (it is not known which
% configuration was meant). Otherwise every problem of every site is
% reported in one pass, in the order of the sites, each labelled by its
% site; see from_app_env/0 for the order and the cascade rule, which
% holds for each site.
-spec sites_from_env(App, Value) -> Result
              when App    :: atom(),
                   Value  :: term(),
                   Result :: {'ok', map()} | {'error', [term()]}.
sites_from_env(App, Value) ->
    case [ K || K <- ?SINGLE_SITE_KEYS,
                application:get_env(App, K) =/= undefined ] of
        [] ->
            Port = application:get_env(App, port, ?PORT),
            Servlets = application:get_env(App, servlets,
                                           ?DEFAULT_SERVLETS),
            Global = global(Port, Servlets),
            % no ca_certs, titan_work_dir or routing key here, so the
            % realms are just the realms key
            {Realms, Failed, RealmErrors} = realms_from_env(App, []),
            case is_list(Value) of
                true ->
                    Outcomes = [ site_entry(E, Realms, Failed)
                                 || E <- Value ],
                    assemble(Global, RealmErrors, Outcomes);
                false ->
                    {error, RealmErrors ++ [{invalid_sites, Value}]
                            ++ problems_global(Global)}
            end;
        Keys ->
            {error, [{sites_and_single_site_keys, Keys}]}
    end.


% One entry of the sites key, as the outcome of building that site.
-spec site_entry(Entry, Realms, Failed) -> Outcome
              when Entry   :: term(),
                   Realms  :: realm_table(),
                   Failed  :: 'all' | [term()],
                   Outcome :: outcome().
site_entry({Host, Opts}, Realms, Failed) when is_list(Opts) ->
    Pairs = [ O || O <- Opts, is_tuple(O), tuple_size(O) =:= 2 ],
    Keys = [ element(1, O) || O <- Pairs ],
    Unknown = [ element_key(O) || O <- Opts,
                                  not is_known_option(O) ],
    Repeated = lists:usort(Keys -- lists:usort(Keys)),
    Missing = [ K || K <- ?SITE_KEYS, not lists:member(K, Keys) ],
    Cert = proplists:get_value(certfile, Pairs),
    Key = proplists:get_value(keyfile, Pairs),
    Routing = proplists:get_value(routing, Pairs, []),
    Bad = [ {invalid_site_value, K, V}
            || {K, V} <- [{certfile, Cert}, {keyfile, Key}],
               V =/= undefined, not is_path(V) ],
    SiteProblems = [ {unknown_site_key, K} || K <- Unknown ]
        ++ [ {duplicate_site_key, K} || K <- Repeated ]
        ++ [ {missing_site_key, K} || K <- Missing ] ++ Bad,
    HostResult = hostname_from_env(Host),
    PrepareOpts = case HostResult of
                      {ok, Name} -> #{site => Name};
                      {error, _} -> #{}
                  end,
    case SiteProblems of
        [] ->
            site_from_env(Host, Cert, Key, Routing, Realms, Failed,
                          PrepareOpts);
        _ ->
            % the files cannot be tried, but the routes can
            {Routes, RouteErrors} = routes_from_env(
                                      Routing, Realms, Failed,
                                      PrepareOpts),
            {Label, HostErrors} = case HostResult of
                                      {ok, N} -> {N, []};
                                      {error, E} -> {Host, [E]}
                                  end,
            {unbuilt, Label, Routes,
             HostErrors
             ++ [ {site, Label, E} || E <- SiteProblems ++ RouteErrors ]}
    end;
site_entry({Host, Other}, _, _) ->
    {unbuilt, Host, [], [{site, Host, {site_options_not_a_list, Other}}]};
site_entry(Other, _, _) ->
    {unbuilt, Other, [], [{invalid_site_entry, Other}]}.


-spec is_known_option(Option) -> Result
              when Option :: term(),
                   Result :: boolean().
is_known_option(O) when is_tuple(O), tuple_size(O) =:= 2 ->
    lists:member(element(1, O), ?SITE_KEYS);
is_known_option(_) ->
    false.


-spec element_key(Option) -> Result
              when Option :: term(),
                   Result :: term().
element_key(O) when is_tuple(O), tuple_size(O) =:= 2 -> element(1, O);
element_key(O) -> O.


-spec is_path(Value) -> Result
              when Value  :: term(),
                   Result :: boolean().
is_path(Value) ->
    Value =/= [] andalso io_lib:char_list(Value).


% What building one site from the environment came to: the site, and the
% errors that were found (already labelled), or the site's label, its
% routes that built and the errors, when the site itself could not be
% built.
-type outcome() :: {'built', site(), [term()]}
                 | {'unbuilt', term(), [route()], [term()]}.

% Build one site: the hostname folded and checked, its routes, and then
% site/3 for the files.
-spec site_from_env(HostValue, Cert, Key, RouteList, Realms, Failed,
                    PrepareOpts) -> Outcome
              when HostValue   :: term(),
                   Cert        :: string(),
                   Key         :: string(),
                   RouteList   :: term(),
                   Realms      :: realm_table(),
                   Failed      :: 'all' | [term()],
                   PrepareOpts :: map(),
                   Outcome     :: outcome().
site_from_env(HostValue, Cert, Key, RouteList, Realms, Failed, Opts) ->
    HostResult = hostname_from_env(HostValue),
    {Routes, RouteErrors} = routes_from_env(RouteList, Realms, Failed,
                                            Opts),
    case HostResult of
        {ok, Hostname} ->
            case site(Hostname, Cert, Key) of
                {ok, Site} ->
                    {built, Site#site{routing = Routes},
                     [ {site, Hostname, E} || E <- RouteErrors ]};
                {error, Why} ->
                    {unbuilt, Hostname, Routes,
                     [ site_error(Hostname, E)
                       || E <- RouteErrors
                              ++ site_build_errors(Why, Hostname, Cert,
                                                   Key) ]}
            end;
        {error, {invalid_hostname, Bad} = Why} ->
            {unbuilt, Bad, Routes,
             [ site_error(Bad, E) || E <- [Why | RouteErrors] ]}
    end.


% Put the sites together: the realm errors, then each site's own errors
% in order, then the problems of validate_all/1 on the sites that were
% built, then the servlets the routes of the unbuilt sites name. When no
% site could be built the sites are not validated, so that "no sites" is
% not reported on top of the errors of the sites that were given.
-spec assemble(Global, RealmErrors, Outcomes) -> Result
              when Global      :: global_config(),
                   RealmErrors :: [term()],
                   Outcomes    :: [outcome()],
                   Result      :: {'ok', map()} | {'error', [term()]}.
assemble(Global = #global{port = Port, servlets = Servlets}, RealmErrors,
         Outcomes) ->
    Built = [ S || {built, S, _} <- Outcomes ],
    Stands = [ stand_in(L, R) || {unbuilt, L, R, _} <- Outcomes ],
    SiteErrors = lists:append([ case O of
                                    {built, _, Es} -> Es;
                                    {unbuilt, _, _, Es} -> Es
                                end || O <- Outcomes ]),
    Problems = case {Built, Stands} of
                   {[], []} ->
                       errors_of(validate_all(new(Global, [])));
                   {[], _} ->
                       problems_global(Global)
                       ++ problems_servlet_use(Global, Stands);
                   {_, _} ->
                       errors_of(validate_all(new(Global, Built)))
                       ++ problems_servlet_use(Global, Stands)
               end,
    case RealmErrors ++ SiteErrors ++ Problems of
        [] -> {ok, make(Port, Servlets, Built)};
        All -> {error, All}
    end.


-spec errors_of(Result) -> Errors
              when Result :: 'ok' | {'error', [term()]},
                   Errors :: [term()].
errors_of(ok) -> [];
errors_of({error, Errors}) -> Errors.


% A site that could not be built, as far as the servlet-use check needs.
-spec stand_in(Label, Routes) -> Site
              when Label  :: term(),
                   Routes :: [route()],
                   Site   :: site().
stand_in(Label, Routes) ->
    Name = case is_list(Label) of
               true -> try erlang:list_to_binary(Label)
                       catch error:badarg -> <<>> end;
               false -> <<>>
           end,
    #site{hostname = Name, certfile = "", keyfile = "", routing = Routes}.


% What site/3 refused, as errors. site/3 stops at the first unreadable
% file, but the certificate and the key are independent problems, so
% when it was a file the operator is told about both in one pass.
-spec site_build_errors(Why, Hostname, Cert, Key) -> Errors
              when Why      :: term(),
                   Hostname :: string(),
                   Cert     :: string(),
                   Key      :: string(),
                   Errors   :: [term()].
site_build_errors(Why, Hostname, Cert, Key)
  when element(1, Why) =:= bad_certfile;
       element(1, Why) =:= bad_keyfile ->
    case file_problems(erlang:list_to_binary(Hostname), Cert, Key) of
        [] -> [Why];    % not readable-PEM trouble, e.g. no certificate in it
        Problems -> Problems
    end;
site_build_errors(Why, _Hostname, _Cert, _Key) ->
    [Why].


% The certificate and the key file, each checked for being readable PEM.
-spec file_problems(Hostname, Cert, Key) -> Problems
              when Hostname :: binary(),
                   Cert     :: string(),
                   Key      :: string(),
                   Problems :: [term()].
file_problems(Hostname, Cert, Key) ->
    [ {bad_certfile, Hostname, Why}
      || {fail, Why} <- [blizanci_x509:validate_pem_file(Cert)] ]
    ++ [ {bad_keyfile, Hostname, Why}
         || {fail, Why} <- [blizanci_x509:validate_pem_file(Key)] ].


% An error found building the site, named by the site, unless it is that
% the site has no name.
-spec site_error(Label, Error) -> Result
              when Label  :: term(),
                   Error  :: term(),
                   Result :: term().
site_error(_, {invalid_hostname, _} = Error) -> Error;
% these already name the site
site_error(_, Error) when element(1, Error) =:= bad_certfile;
                          element(1, Error) =:= bad_keyfile;
                          element(1, Error) =:= cert_key_mismatch ->
    Error;
site_error(Label, Error) -> {site, Label, Error}.


% The hostname, folded to lower case: the operator's name, or the
% machine's own, may have capitals, so it is folded here, at the edge,
% so that site/3 can insist on lower case.
-spec hostname_from_env(Value) -> Result
              when Value  :: term(),
                   Result :: {'ok', string()}
                           | {'error', {'invalid_hostname', term()}}.
hostname_from_env(Value) ->
    IsByte = fun(C) -> is_integer(C) andalso C >= 0 andalso C =< 255 end,
    case is_list(Value) andalso Value =/= []
         andalso lists:all(IsByte, Value) of
        true  -> {ok, ascii_lowercase(Value)};
        false -> {error, {invalid_hostname, Value}}
    end.


% The site's routes: those that build, and an error for each that does
% not, less any unknown_realm error for a realm already reported as
% failed.
-spec routes_from_env(RouteList, Realms, Failed, Opts) -> Result
              when RouteList :: term(),
                   Realms    :: realm_table(),
                   Failed    :: 'all' | [term()],
                   Opts      :: map(),
                   Result    :: {[route()], [term()]}.
routes_from_env(RouteList, Realms, Failed, Opts)
  when is_list(RouteList) ->
    {Routes, Errors} = blizanci_router:prepare_all(RouteList, Realms, Opts),
    Relevant = [ E || E <- Errors, not realm_failed(E, Failed) ],
    {Routes, [ {invalid_routing, E} || E <- Relevant ]};
routes_from_env(RouteList, _, _, _) ->
    {[], [{invalid_routing, {routing_not_a_list, RouteList}}]}.


-spec realm_failed(Error, Failed) -> Result
              when Error  :: term(),
                   Failed :: 'all' | [term()],
                   Result :: boolean().
realm_failed({bad_route, _, _, {unknown_realm, {realm, Name}, _, _}},
             Failed) ->
    Failed =:= all orelse lists:member(Name, Failed);
realm_failed(_, _) ->
    false.


% The global realms key, plus ca_certs as the realm "default" when that
% key is set; both defining "default" is an error. The default table's
% Titan route (no routing key, titan_work_dir set) names the realm
% "default", so it is also defined, empty (authorising nobody until CAs
% are configured), when ca_certs is unset and realms does not have it.
%
% Every definition is tried, so every bad one is reported: the result is
% the table of those that built, the names of those that did not (or
% 'all' if some definition is not even a {Name, Files} pair, or the key
% is not a list, so no name can be blamed), and the errors, each
% {bad_realm, Name, Reason} or {realms, Reason}. Routes naming a failed
% realm are not also reported (see from_app_env/0).
-spec realms_from_env(App, CACerts) -> Result
              when App     :: atom(),
                   CACerts :: term(),
                   Result  :: {realm_table(), 'all' | [term()], [term()]}.
realms_from_env(App, CACerts) ->
    Defs = application:get_env(App, realms, []),
    case is_list(Defs) of
        false ->
            {realms(), all, [{realms, {invalid_realms, Defs}}]};
        true ->
            Default = <<"default">>,
            HasCA = application:get_env(App, ca_certs) =/= undefined,
            Has = lists:keymember(Default, 1, Defs),
            Titan = application:get_env(App, routing) =:= undefined
                andalso application:get_env(App, titan_work_dir,
                                            undefined) =/= undefined,
            {Extra, Start} =
                case {HasCA, Has, Titan} of
                    {true, true, _} ->
                        {[], [{realms, ca_certs_and_default_realm}]};
                    {true, false, _} -> {[{Default, CACerts}], []};
                    {false, false, true} -> {[{Default, []}], []};
                    {false, _, _} -> {[], []}
                end,
            realm_defs(Defs ++ Extra, realms(), [], Start)
    end.


-spec realm_defs(Defs, Table, Failed, Errors) -> Result
              when Defs   :: [term()],
                   Table  :: realm_table(),
                   Failed :: [term()],
                   Errors :: [term()],
                   Result :: {realm_table(), 'all' | [term()], [term()]}.
realm_defs([], Table, Failed, Errors) ->
    case lists:member(all, Failed) of
        true -> {Table, all, Errors};
        false -> {Table, Failed, Errors}
    end;
realm_defs([{Name, Files} | Rest], Table, Failed, Errors) ->
    case add_realm(Table, Name, Files) of
        {ok, Table2} ->
            realm_defs(Rest, Table2, Failed, Errors);
        {error, Why} ->
            Error = {bad_realm, Name, Why},
            realm_defs(Rest, Table, Failed ++ [Name], Errors ++ [Error])
    end;
realm_defs([Other | Rest], Table, Failed, Errors) ->
    realm_defs(Rest, Table, Failed ++ [all],
               Errors ++ [{realms, {invalid_realm, Other}}]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(TEST_DOCROOT, "test-docroot").
-define(RX, "(?<PATH>x)").

test_site(Hostname, Cert, Key) ->
    #site{hostname = erlang:list_to_binary(Hostname), certfile = Cert,
          keyfile = Key, routing = []}.

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
                   Static(?RX, restricted)),
     ?_assertMatch({ok, #route{auth_policy = {private, [], []}}},
                   Static(?RX, {private, []}))].

% A private route's CA files are checked when it is built
route_private_ca_test_() ->
    {setup,
     fun() -> blizanci_x509:test_tmp_dir() end,
     fun(Dir) -> file:del_dir_r(Dir) end,
     fun(Dir) ->
         Opts = #{ docroot => ?TEST_DOCROOT },
         Static = fun(CAs) ->
                      route(gemini, ?RX, blizanci_static,
                            {private, CAs}, Opts)
                  end,
         CA = blizanci_x509:test_write_pem(
                Dir, "ca.pem",
                [{'Certificate',
                  maps:get(ca, blizanci_x509:test_pki()),
                  not_encrypted}]),
         Missing = filename:join(Dir, "missing.pem"),
         [?_assertMatch({ok, #route{auth_policy = {private, [CA], [_]}}},
                        Static([CA])),
          ?_assertEqual({error, {invalid_ca_file, Missing, enoent}},
                        Static([CA, Missing]))]
     end}.

% A route's realms are resolved when it is built, against the realm table.
route_realm_test_() ->
    {setup,
     fun() -> blizanci_x509:test_tmp_dir() end,
     fun(Dir) -> file:del_dir_r(Dir) end,
     fun(Dir) ->
         Opts = #{ docroot => ?TEST_DOCROOT },
         CA = blizanci_x509:test_write_pem(
                Dir, "ca.pem",
                [{'Certificate',
                  maps:get(ca, blizanci_x509:test_pki()),
                  not_encrypted}]),
         {ok, T0} = add_realm(realms(), <<"staff">>, [CA]),
         {ok, T} = add_realm(T0, <<"none">>, []),
         Staff = {realm, <<"staff">>},
         R = fun(Auth) ->
                 route(gemini, ?RX, blizanci_static, Auth, Opts, T)
             end,
         [?_assertMatch({ok, #route{auth_policy = {private, [Staff], [_]}}},
                        R({private, Staff})),
          ?_assertMatch({ok, #route{auth_policy = {private, [Staff], [_]}}},
                        R({private, [Staff]})),
          ?_assertEqual({error, {unknown_realm, {realm, <<"x">>}, ?RX,
                                 blizanci_static}},
                        R({private, {realm, <<"x">>}})),
          % a realm with no files is allowed
          ?_assertMatch({ok, #route{auth_policy = {private, [_], []}}},
                        R({private, {realm, <<"none">>}})),
          ?_assertEqual({error, {unknown_realm, Staff, ?RX,
                                 blizanci_static}},
                        route(gemini, ?RX, blizanci_static,
                              {private, Staff}, Opts)),
          ?_assertMatch({ok, #route{auth_policy = {private, [CA], [_]}}},
                        R({private, [CA]})),
          ?_assertEqual({error, {duplicate_realm, Staff}},
                        add_realm(T, <<"staff">>, [CA])),
          ?_assertEqual({error, {invalid_realm_name, <<>>}},
                        add_realm(T, <<>>, [CA])),
          ?_assertEqual({ok, Staff}, realm(<<"staff">>)),
          ?_assertEqual(
             "route \"(?<PATH>x)\" (servlet blizanci_static) names realm "
             "<<\"x\">>, which is not defined",
             describe_route_error({unknown_realm, {realm, <<"x">>}, ?RX,
                                   blizanci_static})),
          ?_assertMatch({ok, [#route{auth_policy = {private, [Staff], _}}]},
                        blizanci_router:prepare(
                          [{gemini, ?RX, blizanci_static,
                            {private, Staff}, Opts}], T))]
         ++ new_format_file_list_tests(CA, T, Opts)
     end}.

% New-format routes take realm policies only; old tuples keep the file
% list
new_format_file_list_tests(CA, T, Opts) ->
    Staff = {realm, <<"staff">>},
    Mount = fun(Auth) ->
                {mount, gemini, "/x", blizanci_static, Auth, Opts}
            end,
    Regex = fun(Auth) ->
                {regex, gemini, ?RX, blizanci_static, Auth, Opts}
            end,
    Old = fun(Auth) ->
              {gemini, ?RX, blizanci_static, Auth, Opts}
          end,
    Err = {error, file_list_in_new_route},
    Prep = fun(Routes) -> blizanci_router:prepare(Routes, T) end,
    [?_assertEqual(Err, route(Mount({private, [CA]}), T)),
     ?_assertEqual(Err, route(Mount({private, []}), T)),
     ?_assertEqual(Err, route(Regex({private, [CA]}), T)),
     ?_assertEqual(Err, route(Regex({private, []}), T)),
     ?_assertEqual({error, {bad_route, 2, regex, file_list_in_new_route}},
                   Prep([Mount(public), Regex({private, [CA]})])),
     ?_assertEqual({error, {bad_route, 1, mount, file_list_in_new_route}},
                   Prep([Mount({private, []})])),
     ?_assertEqual(
        "route 2 (regex route): a mount or regex route cannot take a list of "
        "CA files in {private, Files}; define a realm in the realms key "
        "and name it, e.g. {private, {realm, <<\"staff\">>}}",
        describe_route_error({bad_route, 2, regex,
                              file_list_in_new_route})),
     % realm policies are still fine in new-format routes
     ?_assertMatch({ok, #route{auth_policy = {private, [Staff], [_]}}},
                   route(Mount({private, Staff}), T)),
     ?_assertMatch({ok, #route{auth_policy = {private, [Staff], [_]}}},
                   route(Regex({private, [Staff]}), T)),
     % old tuples are unchanged
     ?_assertMatch({ok, #route{auth_policy = {private, [CA], [_]}}},
                   route(Old({private, [CA]}), T)),
     ?_assertMatch({ok, #route{auth_policy = {private, [], []}}},
                   route(Old({private, []}), T)),
     ?_assertMatch({ok, [#route{auth_policy = {private, [CA], [_]}},
                         #route{auth_policy = {private, [], []}}]},
                   Prep([Old({private, [CA]}), Old({private, []})]))].

% realms_from_env/2 in the shape of blizanci_auth:realm_table/1: the
% table, or the first error.
realms_env(App, CACerts) ->
    case realms_from_env(App, CACerts) of
        {Table, _, []} -> {ok, Table};
        {_, _, [{_, _, Why} | _]} -> {error, Why};
        {_, _, [{_, Why} | _]} -> {error, Why}
    end.

% Every bad realm is reported, and the good ones still build.
realms_from_env_collects_test_() ->
    {setup,
     fun() -> blizanci_x509:test_tmp_dir() end,
     fun(Dir) ->
         application:unset_env(blizanci_realms_test, realms),
         application:unset_env(blizanci_realms_test, ca_certs),
         file:del_dir_r(Dir)
     end,
     fun(Dir) ->
         CA = blizanci_x509:test_write_pem(
                Dir, "ca.pem",
                [{'Certificate',
                  maps:get(ca, blizanci_x509:test_pki()),
                  not_encrypted}]),
         App = blizanci_realms_test,
         Set = fun(Key, Val) -> application:set_env(App, Key, Val) end,
         Missing = filename:join(Dir, "missing.pem"),
         [?_test(begin
                     application:unset_env(App, ca_certs),
                     Set(realms, [{<<"a">>, [Missing]},
                                  {<<"b">>, [CA]},
                                  {<<"c">>, [Missing]}]),
                     {T, Failed, Errors} = realms_from_env(App, []),
                     ?assertEqual([{realm, <<"b">>}], maps:keys(T)),
                     ?assertEqual([<<"a">>, <<"c">>], Failed),
                     ?assertEqual(
                        [{bad_realm, <<"a">>,
                          {invalid_ca_file, Missing, enoent}},
                         {bad_realm, <<"c">>,
                          {invalid_ca_file, Missing, enoent}}],
                        Errors)
                 end),
          % a definition that is not a pair: no name can be blamed
          ?_test(begin
                     application:unset_env(App, ca_certs),
                     Set(realms, [{<<"b">>, [CA]}, oops]),
                     ?assertMatch(
                        {_, all, [{realms, {invalid_realm, oops}}]},
                        realms_from_env(App, []))
                 end),
          ?_test(begin
                     application:unset_env(App, ca_certs),
                     Set(realms, nonsense),
                     ?assertMatch(
                        {_, all, [{realms, {invalid_realms, nonsense}}]},
                        realms_from_env(App, []))
                 end)]
     end}.

% The sys.config key realms, and ca_certs as the realm "default".
realms_from_env_test_() ->
    {setup,
     fun() -> blizanci_x509:test_tmp_dir() end,
     fun(Dir) ->
         application:unset_env(blizanci_realms_test, realms),
         application:unset_env(blizanci_realms_test, ca_certs),
         application:unset_env(blizanci_realms_test, titan_work_dir),
         application:unset_env(blizanci_realms_test, routing),
         file:del_dir_r(Dir)
     end,
     fun(Dir) ->
         CA = blizanci_x509:test_write_pem(
                Dir, "ca.pem",
                [{'Certificate',
                  maps:get(ca, blizanci_x509:test_pki()),
                  not_encrypted}]),
         App = blizanci_realms_test,
         Set = fun(Key, Val) -> application:set_env(App, Key, Val) end,
         Unset = fun(Key) -> application:unset_env(App, Key) end,
         Reset = fun() -> [Unset(K) || K <- [realms, ca_certs, routing,
                                              titan_work_dir]] end,
         Staff = {realm, <<"staff">>},
         Default = {realm, <<"default">>},
         [?_test(begin
                     Reset(),
                     ?assertEqual({ok, #{}}, realms_env(App, []))
                 end),
          ?_test(begin
                     Reset(),
                     Set(realms, [{<<"staff">>, [CA]}]),
                     {ok, T} = realms_env(App, []),
                     ?assertEqual([Staff], maps:keys(T))
                 end),
          ?_test(begin
                     Reset(),
                     Set(ca_certs, [CA]),
                     {ok, T} = realms_env(App, [CA]),
                     ?assertEqual([Default], maps:keys(T))
                 end),
          ?_test(begin
                     Reset(),
                     Set(ca_certs, [CA]),
                     Set(realms, [{<<"staff">>, [CA]}]),
                     {ok, T} = realms_env(App, [CA]),
                     ?assertEqual([Default, Staff], lists:sort(maps:keys(T)))
                 end),
          ?_test(begin
                     Reset(),
                     Set(ca_certs, [CA]),
                     Set(realms, [{<<"default">>, [CA]}]),
                     ?assertEqual({error, ca_certs_and_default_realm},
                                  realms_env(App, [CA]))
                 end),
          % a realm "default" is fine when ca_certs is not set
          ?_test(begin
                     Reset(),
                     Set(realms, [{<<"default">>, [CA]}]),
                     ?assertMatch({ok, _}, realms_env(App, []))
                 end),
          % the default table's Titan route needs the realm "default",
          % empty if ca_certs is not set
          ?_test(begin
                     Reset(),
                     Set(titan_work_dir, "/w"),
                     {ok, T} = realms_env(App, []),
                     ?assertEqual([Default], maps:keys(T)),
                     ?assertEqual({ok, {private, [Default], []}},
                                  blizanci_auth:valid_authz_policy(
                                    {private, Default}, T))
                 end),
          ?_test(begin
                     Reset(),
                     Set(titan_work_dir, "/w"),
                     Set(ca_certs, []),
                     {ok, T} = realms_env(App, []),
                     ?assertEqual([Default], maps:keys(T))
                 end),
          ?_test(begin
                     Reset(),
                     Set(titan_work_dir, "/w"),
                     Set(realms, [{<<"default">>, [CA]}]),
                     {ok, T} = realms_env(App, []),
                     ?assertMatch({_, [_]}, maps:get(Default, T))
                 end),
          ?_test(begin
                     Reset(),
                     Set(titan_work_dir, "/w"),
                     Set(routing, []),
                     {ok, T} = realms_env(App, []),
                     ?assertEqual([], maps:keys(T))
                 end),
          ?_test(begin
                     Reset(),
                     Set(titan_work_dir, "/w"),
                     Set(ca_certs, [CA]),
                     Set(realms, [{<<"default">>, [CA]}]),
                     ?assertEqual({error, ca_certs_and_default_realm},
                                  realms_env(App, [CA]))
                 end),
          ?_test(begin
                     Reset(),
                     Set(realms, [{<<"staff">>, ["/nonexistent.pem"]}]),
                     ?assertMatch({error, {invalid_ca_file, _, _}},
                                  realms_env(App, []))
                 end),
          ?_test(begin
                     Reset(),
                     Set(realms, [{staff, [CA]}]),
                     ?assertEqual({error, {invalid_realm_name, staff}},
                                  realms_env(App, []))
                 end)]
     end}.

route_merges_default_options_test() ->
    {ok, #route{options = Options}} =
        route(gemini, ?RX, blizanci_static, public,
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
                   route(gopher, ?RX, blizanci_static, public, Opts)),
     ?_assertMatch({error, {invalid_regex, "(", _}},
                   route(gemini, "(", blizanci_static, public, Opts)),
     ?_assertMatch({error, {invalid_regex, 42, not_a_string}},
                   BadRoute(gemini, 42, blizanci_static, public, Opts)),
     ?_assertEqual({error, {unknown_servlet, no_such_servlet}},
                   route(gemini, ?RX, no_such_servlet, public, Opts)),
     ?_assertEqual({error, {servlet_missing_callbacks, lists,
                            [{default_options, 0}, {validate_options, 1},
                             {request, 4}, {serve, 4}, {start, 0},
                             {stop, 0}, {cancel, 1},
                             {handle_client_data, 2}]}},
                   route(gemini, ?RX, lists, public, Opts)),
     % a servlet needs request/4 and serve/4 as well as the options
     % callbacks
     ?_assertEqual({error, {servlet_missing_callbacks,
                            blizanci_test_partial, [{serve, 4}]}},
                   route(gemini, ?RX, blizanci_test_partial, public,
                         #{})),
     ?_assertEqual(ok, blizanci_router:check_servlet(blizanci_test_stub)),
     ?_assertEqual({error, {servlet_missing_callbacks,
                            blizanci_test_partial, [{serve, 4}]}},
                   blizanci_router:check_servlet(blizanci_test_partial)),
     ?_assertMatch({ok, #route{}},
                   route(gemini, ?RX, blizanci_test_stub, public, #{})),
     % each callback is checked: a servlet missing exactly one is refused
     ?_assertEqual({error, {servlet_missing_callbacks,
                            blizanci_test_no_start, [{start, 0}]}},
                   blizanci_router:check_servlet(blizanci_test_no_start)),
     ?_assertEqual({error, {servlet_missing_callbacks,
                            blizanci_test_no_stop, [{stop, 0}]}},
                   blizanci_router:check_servlet(blizanci_test_no_stop)),
     ?_assertEqual({error, {servlet_missing_callbacks,
                            blizanci_test_no_cancel, [{cancel, 1}]}},
                   blizanci_router:check_servlet(blizanci_test_no_cancel)),
     ?_assertEqual({error, {servlet_missing_callbacks,
                            blizanci_test_no_hcd,
                            [{handle_client_data, 2}]}},
                   blizanci_router:check_servlet(blizanci_test_no_hcd)),
     ?_assertEqual({error, {not_a_servlet, "blizanci_static"}},
                   BadRoute(gemini, ?RX, "blizanci_static", public, Opts)),
     ?_assertEqual({error, {invalid_authz_policy, bogus}},
                   BadRoute(gemini, ?RX, blizanci_static, bogus, Opts)),
     ?_assertEqual({error, {invalid_route_opts, not_a_map}},
                   BadRoute(gemini, ?RX, blizanci_static, public, not_a_map)),
     ?_assertEqual({error, {invalid_route_options, blizanci_static,
                            {docroot_not_a_directory, "/no/such/dir"}}},
                   route(gemini, ?RX, blizanci_static, public,
                         #{ docroot => "/no/such/dir" }))].

% A directory holding a real certificate and key, for tests of sites
% built with site/3, which refuses a pair that does not match.
pair_fixture(Tests) ->
    {setup,
     fun() ->
         Dir = blizanci_x509:test_tmp_dir(),
         {Dir, blizanci_x509:test_pair_files(Dir, "a", rsa),
          blizanci_x509:test_pair_files(Dir, "b", rsa)}
     end,
     fun({Dir, _, _}) -> file:del_dir_r(Dir) end,
     fun({_, A, B}) -> Tests(A, B) end}.

make_site_test_() ->
    pair_fixture(fun make_site_tests/2).

make_site_tests({Cert, Key}, {_, OtherKey}) ->
    Opts = #{ docroot => ?TEST_DOCROOT },
    Good = {gemini, ?RX, blizanci_static, public, Opts},
    Bad = {gemini, "x", blizanci_static, public, Opts},
    Make = fun(Host, Routes) ->
               make_site(Host, Cert, Key, Routes)
           end,
    [?_assertMatch({ok, #site{routing = [#route{}]}},
                   Make("example.org", [Good])),
     % a route error is returned, not raised as a badmatch
     ?_assertEqual({error, {invalid_routing,
                            {bad_route, 2, tuple,
                             {route_without_path_group, "x",
                              blizanci_static}}}},
                   Make("example.org", [Good, Bad])),
     ?_assertMatch({error, {invalid_routing,
                            {bad_route, 1, tuple,
                             {invalid_proto, gopher}}}},
                   Make("example.org",
                        [{gopher, ?RX, blizanci_static, public, Opts}])),
     ?_assertEqual({error, hostname_not_lowercase},
                   Make("Example.org", [Good])),
     % a certificate that does not match the key is refused
     ?_assertEqual({error, {cert_key_mismatch, <<"example.org">>,
                            Cert, OtherKey}},
                   make_site("example.org", Cert, OtherKey, [Good]))].

describe_titan_work_dir_test() ->
    Work = filename:join(blizanci_x509:test_tmp_dir(), "no-such-dir"),
    {error, Reason} =
        blizanci_router:prepare([{titan, ?RX, blizanci_titan, public,
                                  #{ docroot => ?TEST_DOCROOT,
                                     work_dir => list_to_binary(Work) }}]),
    ?assertEqual({bad_route, 1, tuple,
                  {invalid_route_options, blizanci_titan,
                   {work_dir_not_a_directory, list_to_binary(Work)}}},
                 Reason),
    ?assertEqual("invalid routing table: route 1 (old-format route): "
                 "the Titan route's work_dir "
                 ++ Work ++ " is not a directory; create it, "
                 "or set work_dir in the Titan route's "
                 "options to an existing directory",
                 describe_error({invalid_routing, Reason})).

titan_work_dir_refusals_test_() ->
    Prep = fun(Opts) ->
               blizanci_router:prepare(
                 [{titan, ?RX, blizanci_titan, public,
                   maps:merge(#{ docroot => ?TEST_DOCROOT }, Opts)}])
           end,
    Bad = fun(Why) -> {error, {bad_route, 1, tuple,
                               {invalid_route_options, blizanci_titan,
                                Why}}} end,
    Msg = fun(Why) ->
              describe_error({invalid_routing,
                              {bad_route, 1, tuple,
                               {invalid_route_options, blizanci_titan,
                                Why}}})
          end,
    [?_assertEqual(Bad(work_dir_missing), Prep(#{})),
     ?_assertEqual(Bad({work_dir_not_absolute, "rel"}),
                   Prep(#{ work_dir => "rel" })),
     ?_assertEqual(Bad({work_dir_not_absolute, <<"rel">>}),
                   Prep(#{ work_dir => <<"rel">> })),
     ?_assertEqual(Bad({work_dir_other_filesystem, "/proc"}),
                   Prep(#{ work_dir => "/proc" })),
     ?_assertMatch({ok, _}, Prep(#{ work_dir => test_tmp() })),
     ?_assertEqual("invalid routing table: route 1 (old-format route): "
                   "the Titan route has no work_dir; set work_dir in "
                   "its options to an existing absolute directory on the docroot's "
                   "filesystem", Msg(work_dir_missing)),
     ?_assertEqual("invalid routing table: route 1 (old-format route): "
                   "the Titan route's work_dir rel is not an absolute "
                   "path; set it to an existing "
                   "absolute directory",
                   Msg({work_dir_not_absolute, "rel"})),
     ?_assertEqual("invalid routing table: route 1 (old-format route): "
                   "the Titan route's work_dir /proc is on a different "
                   "filesystem from its "
                   "docroot; uploads are installed by hard link and "
                   "rename, so use a directory on the docroot's "
                   "filesystem",
                   Msg({work_dir_other_filesystem, "/proc"}))].

% An existing directory on the same filesystem as the test docroot.
test_tmp() ->
    filename:absname(?TEST_DOCROOT).

% The project's own default table must not trigger the shadowing warning.
default_table_no_shadow_warning_test_() ->
    {setup,
     fun() -> blizanci_x509:test_tmp_dir() end,
     fun(Dir) -> file:del_dir_r(Dir) end,
     fun(CGI) ->
         CA = blizanci_x509:test_write_pem(
                CGI, "ca.pem",
                [{'Certificate',
                  maps:get(ca, blizanci_x509:test_pki()),
                  not_encrypted}]),
         {ok, Realms} = add_realm(realms(), <<"default">>, [CA]),
         Warnings =
             fun(Table) ->
                 Id = default_table_log_handler,
                 ok = logger:add_handler(Id, blizanci_test_log_handler,
                                         #{config =>
                                           #{notify => self()}}),
                 try {ok, _} = blizanci_router:prepare(Table, Realms),
                     drain()
                 after ok = logger:remove_handler(Id)
                 end
             end,
         [?_assertEqual([], Warnings(routing_table(?TEST_DOCROOT, CGI,
                                                   [CA]))),
          ?_assertEqual([], Warnings(routing_table(?TEST_DOCROOT, CGI,
                                                   [CA], test_tmp())))]
     end}.

drain() ->
    receive {logged, _, "routing: " ++ _ = T} -> [T | drain()];
            {logged, _, _} -> drain()
    after 0 -> []
    end.

default_titan_route_test_() ->
    Titan = fun(L) -> [R || {mount, titan, _, _, _, _} = R <- L] end,
    Default = {private, {realm, <<"default">>}},
    [?_assertEqual([], Titan(routing_table("d", "c", []))),
     ?_assertEqual([], Titan(routing_table("d", "c", [], undefined))),
     ?_assertEqual([], Titan(routing_table("d", "c", ["ca"]))),
     ?_assertEqual([{mount, titan, "/", blizanci_titan, Default,
                     #{ docroot => "d", work_dir => "/w" }}],
                   Titan(routing_table("d", "c", ["ca"], "/w"))),
     % opting in with no ca_certs is fine: the realm is empty
     ?_assertEqual([{mount, titan, "/", blizanci_titan, Default,
                     #{ docroot => "d", work_dir => "/w" }}],
                   Titan(routing_table("d", "c", [], "/w")))].

% The default table, over a docroot (fixture: Base/docroot/{hello.gmi,
% sub/index.gemini, sub/x.gmi, private/x.gmi, private/sub/index.gemini,
% restricted/x.gmi, privatefoo/x.gmi}, Base/cgi).
default_table_test_() ->
    {setup,
     fun default_table_setup/0,
     fun default_table_teardown/1,
     fun default_table_tests/1}.

default_table_teardown({Mime, Base}) ->
    file:del_dir_r(Base),
    blizanci_test_support:mime_down(Mime).

default_table_setup() ->
    Mime = blizanci_test_support:mime_up(),
    Base =blizanci_test_support:tmp_path("default_table_test"),
    Docroot = filename:join(Base, "docroot"),
    [ ok = filelib:ensure_path(filename:join(Docroot, D))
      || D <- ["sub", "private/sub", "restricted", "privatefoo"] ],
    ok = filelib:ensure_path(filename:join(Base, "cgi")),
    [ ok = file:write_file(filename:join(Docroot, F), <<"x">>)
      || F <- ["hello.gmi", "sub/index.gemini", "sub/x.gmi",
               "private/x.gmi", "private/sub/index.gemini",
               "restricted/x.gmi", "privatefoo/x.gmi"] ],
    {Mime, Base}.

default_table_tests({_Mime, Base}) ->
    Docroot = filename:join(Base, "docroot"),
    Cgi = filename:join(Base, "cgi"),
    Full = fun(Rel) ->
               {file, <<"text/gemini">>,
                list_to_binary(filename:absname(filename:join(Docroot,
                                                               Rel)))}
           end,
    Table = routing_table(Docroot, Cgi, ["ca"]),
    Plain = routing_table(Docroot ++ "/", Cgi, ["ca"]),
    [?_assertEqual(
        [{mount, gemini, "/cgi-bin", blizanci_cgi, public,
          #{ cgiroot => Cgi }},
         {mount, gemini, "/", blizanci_static, public,
          #{ docroot => Docroot }}],
        Table),
     % ca_certs does not change the table; only titan_work_dir does
     ?_assertEqual(Table, routing_table(Docroot, Cgi, [])),
     % a docroot with a trailing slash is kept as given
     ?_assertEqual([Docroot ++ "/"],
                   [ D || {mount, gemini, "/", _, _, #{ docroot := D }}
                              <- Plain ]),
     ?_test(default_table_serves(Full, Table)),
     ?_test(default_table_serves(Full, Plain))].

% What the table does, through the request handler, with no client
% certificate: everything is public; a directory is redirected to its
% index, with the path kept.
default_table_serves(Full, Table) ->
    {ok, Routing} = blizanci_router:prepare(Table, #{}),
    H = fun(Path) -> default_table_get(Routing, Path) end,
    ?assertEqual(Full("hello.gmi"), H(<<"hello.gmi">>)),
    ?assertEqual(Full("sub/x.gmi"), H(<<"sub/x.gmi">>)),
    ?assertEqual({redirect, <<"sub/index.gemini">>}, H(<<"sub">>)),
    ?assertEqual({redirect, <<"sub/index.gemini">>}, H(<<"sub/">>)),
    ?assertEqual({error_code, file_not_found}, H(<<"nothere.gmi">>)),
    ?assertEqual({error_code, file_not_found}, H(<<"sub/nothere">>)).

% An explicit routing table with /private and /restricted mounts over
% subdirectories of the docroot, beside /cgi-bin and /; the fixture is
% the one of default_table_test_.
mounts_table_test_() ->
    {setup,
     fun default_table_setup/0,
     fun default_table_teardown/1,
     fun mounts_table_tests/1}.

mounts_table(Docroot, Cgi, Private) ->
    [{mount, gemini, "/cgi-bin", blizanci_cgi, public,
      #{ cgiroot => Cgi }},
     {mount, gemini, "/private", blizanci_static, Private,
      #{ docroot => filename:join(Docroot, "private") }},
     {mount, gemini, "/restricted", blizanci_static, restricted,
      #{ docroot => filename:join(Docroot, "restricted") }},
     {mount, gemini, "/", blizanci_static, public,
      #{ docroot => Docroot }}].

mounts_table_tests({_Mime, Base}) ->
    Docroot = filename:join(Base, "docroot"),
    Cgi = filename:join(Base, "cgi"),
    Full = fun(Rel) ->
               {file, <<"text/gemini">>,
                list_to_binary(filename:absname(filename:join(Docroot,
                                                               Rel)))}
           end,
    Realm = {private, {realm, <<"default">>}},
    Table = mounts_table(Docroot, Cgi, Realm),
    [?_test(mounts_table_serves(Full, Table, Docroot)),
     ?_test(mounts_table_with_cert(Docroot, Full, Table)),
     ?_test(mounts_table_empty_realm(Docroot, Cgi))].

% What the table does, through the request handler, with no client
% certificate: private and restricted ask for one (status 60); the rest
% is served, and a directory is redirected to its index, with the mount
% in the path.
mounts_table_serves(Full, Table, Docroot) ->
    {ok, Routing} = blizanci_router:prepare(
                      Table, mounts_realms(Docroot,
                                           blizanci_x509:test_pki())),
    H = fun(Path) -> default_table_get(Routing, Path) end,
    NotFound = {error_code, file_not_found},
    ?assertEqual(Full("hello.gmi"), H(<<"hello.gmi">>)),
    % "private" is a whole segment
    ?assertEqual(Full("privatefoo/x.gmi"), H(<<"privatefoo/x.gmi">>)),
    ?assertEqual(NotFound, H(<<"privatefoo/nothere">>)),
    ?assertEqual({redirect, <<"privatefoo/index.gemini">>},
                 H(<<"privatefoo">>)),
    Need = {error_code, cert_required},
    [ ?assertEqual(Need, H(P))
      || P <- [<<"private/x.gmi">>, <<"restricted/x.gmi">>,
               <<"private">>, <<"private/">>, <<"private/nothere">>,
               <<"restricted">>] ],
    ?assertEqual(NotFound, H(<<"nothere.gmi">>)).

% With the client certificate of the CA behind realm "default", private
% serves <docroot>/private, and a certificate from any CA suffices for
% restricted; redirects keep the mount.
mounts_table_with_cert(Docroot, Full, Table) ->
    Pki = blizanci_x509:test_pki(),
    {ok, Routing} = blizanci_router:prepare(
                      Table, mounts_realms(Docroot, Pki)),
    #{ peer := Peer } = Pki,
    H = fun(Path) -> default_table_get(Routing, {ok, Peer}, Path) end,
    ?assertEqual(Full("private/x.gmi"), H(<<"private/x.gmi">>)),
    ?assertEqual(Full("restricted/x.gmi"), H(<<"restricted/x.gmi">>)),
    ?assertEqual({redirect, <<"private/index.gemini">>}, H(<<"private">>)),
    ?assertEqual({redirect, <<"private/index.gemini">>},
                 H(<<"private/">>)),
    ?assertEqual({redirect, <<"private/sub/index.gemini">>},
                 H(<<"private/sub">>)),
    ?assertEqual({redirect, <<"private/sub/index.gemini">>},
                 H(<<"private/sub/">>)),
    ?assertEqual({redirect, <<"restricted/index.gemini">>},
                 H(<<"restricted/">>)),
    ?assertEqual({error_code, file_not_found}, H(<<"private/nothere">>)).

% A private mount on a realm with no CA files builds and prepares, and
% authorises nobody, even a certificate (rather than falling through to
% the public mount).
mounts_table_empty_realm(Docroot, Cgi) ->
    {ok, Realms} = realms([{<<"default">>, []}]),
    {ok, Routing} = blizanci_router:prepare(
                      mounts_table(Docroot, Cgi,
                                   {private, {realm, <<"default">>}}),
                      Realms),
    #{ peer := Peer } = blizanci_x509:test_pki(),
    NotFound = {error_code, file_not_found},
    ?assertEqual({error_code, cert_not_authorised},
                 default_table_get(Routing, {ok, Peer},
                                   <<"private/x.gmi">>)),
    ?assertEqual({error_code, cert_required},
                 default_table_get(Routing, {error, no_peercert},
                                   <<"private/x.gmi">>)),
    ?assertNotEqual(NotFound, default_table_get(
                                Routing, {ok, Peer}, <<"private">>)).

mounts_realms(Docroot, Pki) ->
    CA = blizanci_x509:test_write_pem(
           filename:dirname(Docroot), "default-ca.pem",
           [{'Certificate', maps:get(ca, Pki),
             not_encrypted}]),
    {ok, T} = realms([{<<"default">>, [CA]}]),
    T.

default_table_get(Routing, Path) ->
    default_table_get(Routing, {error, no_peercert}, Path).

default_table_get(Routing, Cert, Path) ->
    Config = #server_config{hostname = <<"h">>, port = 1965,
                            routing = Routing},
    blizanci_request:handle_line(<<"gemini://h/", Path/binary>>, Config,
                                 Cert, <<>>).

describe_error_test_() ->
    [?_assertEqual("invalid routing table: route \"x\" (servlet "
                   "blizanci_static) has no named group PATH in its "
                   "regex; add one, e.g. (?<PATH>.*)",
                   describe_error(
                     {invalid_routing,
                      {route_without_path_group, "x", blizanci_static}})),
     ?_assertEqual("invalid routing table: a private route's CA file "
                   "/x/ca.pem cannot be used (enoent); check that it "
                   "exists and holds a PEM certificate",
                   describe_error(
                     {invalid_routing,
                      {invalid_ca_file, "/x/ca.pem", enoent}})),
     ?_assertEqual("invalid routing table: route for servlet "
                   "blizanci_static has invalid options: "
                   "{docroot_not_a_directory,\"/nope\"}",
                   describe_error(
                     {invalid_routing,
                      {invalid_route_options, blizanci_static,
                       {docroot_not_a_directory, "/nope"}}})),
     ?_assertEqual("servlet blizanci_cgi could not start: "
                   "{invalid_limit,0}",
                   describe_error(
                     {shutdown,
                      {failed_to_start_child, blizanci_servlets,
                       {servlet_start_failed, blizanci_cgi,
                        {invalid_limit, 0}}}})),
     ?_assertEqual("servlet blizanci_titan could not start: "
                   "{invalid_limit,\"5\"}",
                   describe_error(
                     {servlet_start_failed, blizanci_titan,
                      {invalid_limit, "5"}})),
     ?_assertEqual("servlet blizanci_cgi could not start: "
                   "{invalid_module,5}",
                   describe_error(
                     {servlet_start_failed, blizanci_cgi,
                      {invalid_module, 5}}))].

global_test() ->
    ?assertEqual(#global{port = 1965, servlets = [blizanci_static]},
                 global(1965, [blizanci_static])).

site_test() ->
    ?assertEqual(#site{hostname = <<"example.org">>,
                       certfile = "c.pem",
                       keyfile  = "k.pem",
                       routing  = []},
                 test_site("example.org", "c.pem", "k.pem")).

site_rejects_uppercase_test_() ->
    pair_fixture(fun({Cert, Key}, _) ->
        [?_assertEqual({error, hostname_not_lowercase},
                       site("Example.org", Cert, Key)),
         ?_assertEqual({error, hostname_not_lowercase},
                       site("EXAMPLE.ORG", Cert, Key)),
         ?_assertMatch({ok, #site{hostname = <<"example.org">>}},
                       site("example.org", Cert, Key))]
    end).

% site/3 refuses a certificate and key that are not a pair, naming the
% site and both files.
site_cert_key_pair_test_() ->
    pair_fixture(fun({Cert, Key}, {OtherCert, OtherKey}) ->
        Missing = filename:join(filename:dirname(Cert), "missing.pem"),
        [?_assertMatch({ok, #site{certfile = Cert, keyfile = Key}},
                       site("a.example", Cert, Key)),
         ?_assertEqual({error, {cert_key_mismatch, <<"a.example">>,
                                Cert, OtherKey}},
                       site("a.example", Cert, OtherKey)),
         ?_assertEqual({error, {cert_key_mismatch, <<"b.example">>,
                                OtherCert, Key}},
                       site("b.example", OtherCert, Key)),
         ?_assertEqual({error, {bad_certfile, <<"a.example">>,
                                {unreadable, enoent}}},
                       site("a.example", Missing, Key)),
         ?_assertEqual({error, {bad_keyfile, <<"a.example">>,
                                no_private_key}},
                       site("a.example", Cert, Cert)),
         ?_assertEqual("site a.example: certificate " ++ Cert
                       ++ " and private key " ++ OtherKey
                       ++ " are not a matching pair",
                       describe_error(
                         {cert_key_mismatch, <<"a.example">>,
                          Cert, OtherKey}))]
    end).

% A pair the check cannot compare (DSA, a curve it does not know) is
% accepted with exactly one warning, from site/3; validating the built
% site and applying an unchanged configuration say nothing more.
site_unverified_pair_test_() ->
    {setup,
     fun() ->
         Dir = blizanci_x509:test_tmp_dir(),
         {Dir, [ {K, blizanci_x509:test_pair_files(Dir, atom_to_list(K),
                                                   K)}
                 || K <- [dsa, p224, rsa] ]}
     end,
     fun({Dir, _}) -> file:del_dir_r(Dir) end,
     fun({_, Pairs}) ->
         {DsaCert, DsaKey} = proplists:get_value(dsa, Pairs),
         {EcCert, EcKey} = proplists:get_value(p224, Pairs),
         {RsaCert, _} = proplists:get_value(rsa, Pairs),
         Warnings =
             fun(Fun) ->
                 Id = unverified_pair_log_handler,
                 ok = logger:add_handler(Id, blizanci_test_log_handler,
                                         #{config =>
                                           #{notify => self()}}),
                 try Fun(), drain_warnings()
                 after ok = logger:remove_handler(Id)
                 end
             end,
         Text = fun(Cert, Key, Type) ->
                    "site a.example: cannot verify that certificate "
                    ++ Cert ++ " and private key " ++ Key
                    ++ " are a pair (unsupported key type " ++ Type ++ ")"
                end,
         [?_assertEqual([Text(DsaCert, DsaKey, "DSA")],
                        Warnings(fun() ->
                            {ok, #site{}} = site("a.example", DsaCert,
                                                 DsaKey)
                        end)),
          ?_assertEqual([Text(EcCert, EcKey, "EC secp224r1")],
                        Warnings(fun() ->
                            {ok, #site{}} = site("a.example", EcCert,
                                                 EcKey)
                        end)),
          % an RSA certificate with a DSA key is a mismatch, not a warning
          ?_assertEqual({[], {error, {cert_key_mismatch, <<"a.example">>,
                                      RsaCert, DsaKey}}},
                        begin
                            Ref = make_ref(),
                            W = Warnings(fun() ->
                                put(Ref, site("a.example", RsaCert,
                                              DsaKey))
                            end),
                            {W, erase(Ref)}
                        end),
          % the start-up path builds the site once, so warns once
          ?_assertEqual([Text(DsaCert, DsaKey, "DSA")],
                        Warnings(fun() ->
                            {ok, _} = make_site("a.example", DsaCert,
                                                DsaKey, [])
                        end)),
          % validating a site built without site/3, silent
          ?_assertEqual({[], []},
                        begin
                            Ref = make_ref(),
                            W = Warnings(fun() ->
                                put(Ref, problems_site(
                                           test_site("a.example",
                                                     DsaCert, DsaKey)))
                            end),
                            {W, erase(Ref)}
                        end)]
     end}.

drain_warnings() ->
    receive {logged, warning, T} -> [T | drain_warnings()];
            {logged, _, _} -> drain_warnings()
    after 0 -> []
    end.

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
    {ok, R1} = route(gemini, "(?<PATH>a)", blizanci_static, public, Opts),
    {ok, R2} = route(gemini, "(?<PATH>b)", blizanci_static, restricted, Opts),
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
     fun(_) -> blizanci_test_support:live_down() end,
     [?_assertEqual([{certfile, "a.pem"}, {keyfile, "a.key"},
                     {verify, verify_none}],
                    SniFun("a.example")),
      ?_assertEqual([{certfile, "b.pem"}, {keyfile, "b.key"},
                     {verify, verify_none}],
                    SniFun("b.example")),
      ?_assertEqual(unrecognized, SniFun("evil.example")),
      % a name that isn't ours is refused whatever its case
      ?_assertEqual(unrecognized, SniFun("EVIL.EXAMPLE")),
      % and a name with a trailing dot (not valid in an SNI) is refused
      ?_assertEqual(unrecognized, SniFun("a.example.")),
      ?_assertEqual(unrecognized, SniFun("A.EXAMPLE.")),
      ?_assertEqual(undefined, proplists:get_value(certfile, SSL)),
      ?_assertEqual(undefined, proplists:get_value(keyfile, SSL)),
      ?_assertEqual(['tlsv1.3', 'tlsv1.2'],
                    proplists:get_value(versions, SSL)),
      ?_assertEqual(['tlsv1.3', 'tlsv1.2'], tls_versions()),
      % the same sni_fun sees a replaced site list: this is what lets
      % apply/1 change certificates on a running listener
      ?_test(begin
                 ok = blizanci_conf_serv:publish(1965, [], [B]),
                 ?assertEqual(unrecognized, SniFun("a.example")),
                 ?assertEqual([{certfile, "b.pem"}, {keyfile, "b.key"},
                     {verify, verify_none}],
                              SniFun("b.example"))
             end)]}.

%% Whether a site asks for a client certificate follows from its routes.
site_wants_client_cert_test_() ->
    {setup,
     fun() -> blizanci_x509:test_tmp_dir() end,
     fun(Dir) -> file:del_dir_r(Dir) end,
     fun(Dir) ->
         #{ca := CA} = blizanci_x509:test_pki(),
         CAFile = blizanci_x509:test_write_pem(
                    Dir, "ca.pem", [{'Certificate', CA, not_encrypted}]),
         Opts = #{ docroot => ?TEST_DOCROOT,
                   work_dir => filename:absname(?TEST_DOCROOT) },
         R = fun(Proto, Mod, Auth) ->
                 {ok, Route} = route(Proto, ?RX, Mod, Auth, Opts),
                 Route
             end,
         Pub = R(gemini, blizanci_static, public),
         Res = R(gemini, blizanci_static, restricted),
         Priv = R(gemini, blizanci_static, {private, [CAFile]}),
         TitanPub = R(titan, blizanci_titan, public),
         TitanPriv = R(titan, blizanci_titan, {private, [CAFile]}),
         TitanRes = R(titan, blizanci_titan, restricted),
         [?_assertMatch(#route{auth_policy = {private, [_], [_]}}, Priv),
          ?_assertNot(site_wants_client_cert([])),
          ?_assertNot(site_wants_client_cert([Pub])),
          ?_assertNot(site_wants_client_cert([Pub, TitanPub, Pub])),
          ?_assert(site_wants_client_cert([Res])),
          ?_assert(site_wants_client_cert([Priv])),
          ?_assert(site_wants_client_cert([Pub, Res, Pub])),
          ?_assert(site_wants_client_cert([Pub, Priv])),
          ?_assert(site_wants_client_cert([Priv, Res])),
          ?_assert(site_wants_client_cert([Pub, TitanPriv])),
          ?_assert(site_wants_client_cert([TitanRes])),
          ?_assert(site_wants_client_cert([TitanPub, Pub, TitanPriv]))]
     end}.

%% The sni_fun result follows the live site, and so follows apply/1.
sni_fun_client_cert_test_() ->
    {setup,
     fun() -> {write_test_pem(), blizanci_x509:test_tmp_dir()} end,
     fun({Pem, Dir}) ->
         blizanci_test_support:live_down(),
         file:delete(Pem),
         file:del_dir_r(Dir)
     end,
     fun({Pem, Dir}) ->
         ?_test(begin
             #{ca := CA} = blizanci_x509:test_pki(),
             CAFile = blizanci_x509:test_write_pem(
                        Dir, "ca.pem", [{'Certificate', CA, not_encrypted}]),
             Opts = #{ docroot => ?TEST_DOCROOT },
             Site = fun(Auth) ->
                 {ok, Route} = route(gemini, ?RX, blizanci_static,
                                     Auth, Opts),
                 add_route(test_site("a.example", Pem, Pem), Route)
             end,
             Conf = fun(Auth) ->
                 new(global(1965, [blizanci_static]), [Site(Auth)])
             end,
             SniFun = proplists:get_value(sni_fun, ssl_opts(1965)),
             Creds = [{certfile, Pem}, {keyfile, Pem}],
             live_up(1965, [blizanci_static], [Site(public)]),
             ?assertEqual(Creds ++ [{verify, verify_none}],
                          SniFun("a.example")),
             ?assertEqual(ok, apply(Conf({private, [CAFile]}))),
             ?assertEqual(Creds, SniFun("a.example")),
             ?assertEqual(ok, apply(Conf(restricted))),
             ?assertEqual(Creds, SniFun("a.example")),
             ?assertEqual(ok, apply(Conf(public))),
             ?assertEqual(Creds ++ [{verify, verify_none}],
                          SniFun("a.example"))
         end)
     end}.

%% A host name is case-insensitive, so the same site is found whatever
%% the case of the name the client sent in its SNI.
sni_fun_case_insensitive_test_() ->
    A = test_site("a.example", "a.pem", "a.key"),
    B = test_site("b.example", "b.pem", "b.key"),
    SniFun = proplists:get_value(sni_fun, ssl_opts(1965)),
    {setup,
     fun() -> live_up(1965, [], [A, B]) end,
     fun(_) -> blizanci_test_support:live_down() end,
     [?_assertEqual([{certfile, "a.pem"}, {keyfile, "a.key"},
                     {verify, verify_none}],
                    SniFun("A.EXAMPLE")),
      ?_assertEqual([{certfile, "a.pem"}, {keyfile, "a.key"},
                     {verify, verify_none}],
                    SniFun("A.Example")),
      ?_assertEqual([{certfile, "b.pem"}, {keyfile, "b.key"},
                     {verify, verify_none}],
                    SniFun("B.example"))]}.

%% Builder: routes and configuration.

insert_route_test_() ->
    Opts = #{ docroot => ?TEST_DOCROOT },
    {ok, R1} = route(gemini, "(?<PATH>a)", blizanci_static, public, Opts),
    {ok, R2} = route(gemini, "(?<PATH>b)", blizanci_static, public, Opts),
    {ok, R3} = route(gemini, "(?<PATH>c)", blizanci_static, public, Opts),
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

% validate_all/1 reports every problem, in a stable order, and
% validate/1 the first of them.
validate_all_test_() ->
    {setup, fun write_test_pem/0, fun file:delete/1,
     fun validate_all_tests/1}.

validate_all_tests(Pem) ->
    {ok, R} = route(gemini, "(?<PATH>.*)", blizanci_static, public,
                    #{ docroot => ?TEST_DOCROOT }),
    Bad = add_route(test_site("a", "/no/such.pem", "/no/such.key"), R),
    Good = add_route(test_site("b", Pem, Pem), R),
    G = global(0, [nope]),
    Config = new(G, [Bad, Good]),
    All = [{invalid_port, 0},
           {unknown_servlet, nope},
           {bad_certfile, <<"a">>, couldnt_open_pem_file},
           {bad_keyfile, <<"a">>, couldnt_open_pem_file},
           {undeclared_servlet, <<"a">>, blizanci_static},
           {undeclared_servlet, <<"b">>, blizanci_static}],
    [?_assertEqual({error, All}, validate_all(Config)),
     ?_assertEqual({error, hd(All)}, validate(Config)),
     ?_assertEqual(ok, validate_all(new(global(1965, [blizanci_static]),
                                        [Good]))),
     ?_assertEqual({error, [no_sites]},
                   validate_all(new(global(1965, []), []))),
     ?_assertEqual({error, [{duplicate_site, <<"b">>}]},
                   validate_all(new(global(1965, [blizanci_static]),
                                    [Good, Good]))),
     ?_assertEqual(
        ["site a: the certificate file is not a readable PEM file "
         "(couldnt_open_pem_file)",
         "site a: a route uses servlet blizanci_static, which is not in "
         "the servlets list",
         "port 0 is not a TCP port number (1 to 65535)"],
        [ describe_error(E)
          || E <- [lists:nth(3, All), lists:nth(5, All), hd(All)] ]),
     ?_assertEqual(
        "site a.example: hostname is not lower case; realm <<\"r\">>: "
        "CA file /x cannot be used (enoent); check that it exists and "
        "holds a PEM certificate",
        describe_error(
          {invalid_configuration,
           [{site, "a.example", hostname_not_lowercase},
            {bad_realm, <<"r">>, {invalid_ca_file, "/x", enoent}}]}))].

write_test_pem() ->
    Path = blizanci_test_support:tmp_path("validate_test") ++ ".pem",
    % A real certificate and its key in one file, so that the pair
    % check passes when the file is used as both.
    Dir = blizanci_x509:test_tmp_dir(),
    {Cert, Key} = blizanci_x509:test_pair_files(Dir, "p", rsa),
    {ok, CertPem} = file:read_file(Cert),
    {ok, KeyPem} = file:read_file(Key),
    ok = file:write_file(Path, [CertPem, KeyPem]),
    ok = file:del_dir_r(Dir),
    Path.

%% apply/1.

%% Stand up the live configuration, as blizanci_sup would.
live_up(Port, Servlets, Sites) ->
    blizanci_test_support:live_up(Port, Servlets, Sites).

apply_test_() ->
    {setup, fun write_test_pem/0,
     fun(Pem) -> file:delete(Pem), blizanci_test_support:live_down() end,
     fun apply_tests/1}.

apply_tests(Pem) ->
    {ok, R} = route(gemini, "(?<PATH>.*)", blizanci_static, public,
                    #{ docroot => ?TEST_DOCROOT }),
    Site = fun(Host) -> add_route(test_site(Host, Pem, Pem), R) end,
    Conf = fun(Port, Servlets, Sites) ->
               new(global(Port, Servlets), Sites)
           end,
    Reset = fun() -> blizanci_test_support:live_down() end,
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

%% An apply/1 whose site has a certificate and key that are not a pair
%% is refused, and the configuration that was serving keeps serving. The
%% bad site is one rebuilt from a key file replaced since site/3 passed
%% it, and one built without the check, as a record.
apply_cert_key_mismatch_test_() ->
    pair_fixture(
      fun({Cert, Key}, {_, OtherKey}) ->
          {ok, R} = route(gemini, "(?<PATH>.*)", blizanci_static, public,
                          #{ docroot => ?TEST_DOCROOT }),
          Conf = fun(Sites) ->
                     new(global(1965, [blizanci_static]), Sites)
                 end,
          {setup,
           fun() ->
               {ok, Good} = site("a.example", Cert, Key),
               Old = add_route(Good, R),
               live_up(1965, [blizanci_static], [Old]),
               Old
           end,
           fun(_) -> blizanci_test_support:live_down() end,
           fun(Old) ->
               Bad = add_route(test_site("a.example", Cert, OtherKey), R),
               Expected = {error, {cert_key_mismatch, <<"a.example">>,
                                   Cert, OtherKey}},
               Unchanged = fun() ->
                   ?assertEqual({ok, #{port => 1965,
                                       servlets => [blizanci_static],
                                       sites => [Old]}},
                                blizanci_conf_serv:current()),
                   ?assertEqual({ok, Old},
                                blizanci_conf_serv:find_site(
                                  <<"a.example">>))
               end,
               [?_test(begin
                   ?assertEqual(Expected, apply(Conf([Bad]))),
                   Unchanged()
                end),
                % one bad site among good ones refuses the whole lot
                ?_test(begin
                    Other = add_route(test_site("b.example", Cert, Key), R),
                    ?assertEqual(Expected, apply(Conf([Other, Bad]))),
                    Unchanged()
                end),
                ?_test(begin
                    ?assertEqual(ok, apply(Conf([Old]))),
                    Unchanged()
                end)]
           end}
      end).

%% A private route holds the CA certificates decoded when it was built:
%% a CA file changed afterwards takes effect only when a rebuilt route is
%% applied.
apply_ca_file_test_() ->
    {setup,
     fun() -> {write_test_pem(), blizanci_x509:test_tmp_dir()} end,
     fun({Pem, Dir}) ->
         blizanci_test_support:live_down(),
         file:delete(Pem),
         file:del_dir_r(Dir)
     end,
     fun({Pem, Dir}) ->
         ?_test(begin
             #{ca := CA, ca2 := CA2, peer := Peer, peer_ca2 := PeerCA2} =
                 blizanci_x509:test_pki(),
             CAFile = blizanci_x509:test_write_pem(
                        Dir, "ca.pem", [{'Certificate', CA, not_encrypted}]),
             Build = fun() ->
                 {ok, R} = route(gemini, "(?<PATH>.*)", blizanci_static,
                                 {private, [CAFile]},
                                 #{ docroot => ?TEST_DOCROOT }),
                 add_route(test_site("a", Pem, Pem), R)
             end,
             Live = fun(Cert) ->
                 {ok, #site{routing = [#route{auth_policy = P}]}} =
                     blizanci_conf_serv:find_site(<<"a">>),
                 blizanci_auth:authorised(
                   P, blizanci_test_support:test_request(
                        #{ client_cert => {ok, Cert} }))
             end,
             live_up(1965, [blizanci_static], [Build()]),
             ?assertEqual(authorised, Live(Peer)),
             ?assertEqual({error_code, cert_not_authorised}, Live(PeerCA2)),
             % rewrite the file: nothing changes until it is applied
             blizanci_x509:test_write_pem(
               Dir, "ca.pem", [{'Certificate', CA2, not_encrypted}]),
             ?assertEqual(authorised, Live(Peer)),
             ?assertEqual({error_code, cert_not_authorised}, Live(PeerCA2)),
             ?assertEqual(ok, apply(new(global(1965, [blizanci_static]),
                                        [Build()]))),
             ?assertEqual({error_code, cert_not_authorised}, Live(Peer)),
             ?assertEqual(authorised, Live(PeerCA2)),
             % a file that has gone bad refuses the new route, and the
             % running one is left as it was
             ok = file:write_file(CAFile, <<"garbage\n">>),
             ?assertMatch({error, {invalid_ca_file, CAFile,
                                   no_certificate_in_file}},
                          route(gemini, "(?<PATH>.*)", blizanci_static,
                                {private, [CAFile]},
                                #{ docroot => ?TEST_DOCROOT })),
             ?assertEqual(authorised, Live(PeerCA2))
         end)
     end}.

%% Servlet pools start and stop as the declared list changes.
apply_servlets_test_() ->
    {setup,
     fun() ->
         {ok, _} = application:ensure_all_started(ppool),
         write_test_pem()
     end,
     fun(Pem) ->
         blizanci_test_support:live_down(),
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
             blizanci_test_support:live_down()
         end)
     end}.

%% A servlet whose start/0 returns {error, boom}: apply/1 refuses, with
%% the servlets it had already started in that call stopped again,
%% nothing published, and the running configuration and pools as before.
apply_servlet_start_error_test_() ->
    {setup,
     fun() ->
         {ok, _} = application:ensure_all_started(ppool),
         write_test_pem()
     end,
     fun(Pem) ->
         blizanci_test_support:live_down(),
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
             Before = blizanci_conf_serv:current(),
             ?assertEqual({error, {servlet_start_failed,
                                   blizanci_test_failing, boom}},
                          apply(Conf([blizanci_static, blizanci_cgi,
                                      blizanci_test_failing]))),
             ?assertEqual(undefined, whereis(blizanci_cgi)),
             ?assertEqual(Before, blizanci_conf_serv:current()),
             % the process is still there, and can still reconfigure
             ?assertEqual(ok, apply(Conf([blizanci_static,
                                          blizanci_cgi]))),
             ?assertNotEqual(undefined, whereis(blizanci_cgi)),
             ?assertEqual({error, {servlet_start_failed,
                                   blizanci_test_failing, boom}},
                          apply(Conf([blizanci_static,
                                      blizanci_test_failing]))),
             ?assertNotEqual(undefined, whereis(blizanci_cgi)),
             ?assertMatch({ok, #{servlets := [blizanci_static,
                                              blizanci_cgi]}},
                          blizanci_conf_serv:current()),
             blizanci_test_support:live_down()
         end)
     end}.

%% validate/1 refuses a site whose routes let a Titan upload land in a
%% CGI directory.
validate_upload_in_cgi_dir_test_() ->
    {setup,
     fun() -> {write_test_pem(), make_test_dir()} end,
     fun({Pem, Dir}) -> file:delete(Pem), file:del_dir_r(Dir) end,
     fun({Pem, Dir}) ->
         {ok, C} = route(gemini, "(?<PATH>c)", blizanci_cgi, public,
                         #{ cgiroot => Dir }),
         {ok, T} = route(titan, "(?<PATH>t)", blizanci_titan, public,
                         #{ docroot => Dir, work_dir => Dir }),
         Site = add_route(add_route(test_site("a", Pem, Pem), C), T),
         G = global(1965, [blizanci_cgi, blizanci_titan]),
         ?_assertMatch({error, {bad_routes, <<"a">>,
                                {upload_dir_overlaps_cgiroot, _, _, _}}},
                       validate(new(G, [Site])))
     end}.

make_test_dir() ->
    Dir = blizanci_test_support:tmp_path("validate_dir_test"),
    ok = filelib:ensure_path(Dir),
    Dir.
