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
%%   <dd>the hostname (defaults to system hostname)</dd>
%%   <dt><code>{keyfile, PrivKeyPath::string()}</code></dt>
%%   <dd>the path to the server's private key</dd>
%%   <dt><code>{port, Port::integer()}</code></dt>
%%   <dd>the port to listen on (defaults to 1965)</dd>
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
         new/2, validate/1, apply/1, ascii_lowercase/1,
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
                ok -> {ok, #site{hostname = Host,
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
                   Result   :: 'ok' | {'error', pair_error()}.
check_pair(Hostname, CertFile, KeyFile) ->
    case blizanci_x509:cert_key_match(CertFile, KeyFile) of
        ok -> ok;
        {error, mismatch} ->
            {error, {cert_key_mismatch, Hostname, CertFile, KeyFile}};
        {error, {certfile, Why}} ->
            {error, {bad_certfile, Hostname, Why}};
        {error, {keyfile, Why}} ->
            {error, {bad_keyfile, Hostname, Why}}
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
    case is_integer(Port) andalso Port >= 1 andalso Port =< ?MAX_PORT of
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
            case check_pair(Hostname, Cert, Key) of
                ok ->
                    case blizanci_router:check_upload_dirs(Routing) of
                        ok -> ok;
                        {error, Why} ->
                            {error, {bad_routes, Hostname, Why}}
                    end;
                {error, _} = Error -> Error
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
              when Result :: {'ok', map()} | {'error', term()}.
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
    TitanWork = application:get_env(App, titan_work_dir, undefined),
    Default_Routing = routing_table(Docroot, CGIroot, CACerts, TitanWork),
    % The default table is in the new format, so only an operator's own
    % routing key can trigger the old-format warning.
    Routes = case application:get_env(App, routing) of
                 {ok, Own} -> Own;
                 undefined -> Default_Routing
             end,
    Prep = #{},
    case realms_from_env(App, CACerts) of
        {ok, Realms} ->
            site_from_env(App, Hostname, Cert, Key, Routes, Realms, Prep);
        {error, _} = Error -> Error
    end.


% The global realms key, plus ca_certs as the realm "default" when that
% key is set; both defining "default" is an error. The default table's
% Titan route (no routing key, titan_work_dir set) names the realm
% "default", so it is also defined, empty (authorising nobody until CAs
% are configured), when ca_certs is unset and realms does not have it.
-spec realms_from_env(App, CACerts) -> Result
              when App     :: atom(),
                   CACerts :: term(),
                   Result  :: {'ok', realm_table()} | {'error', term()}.
realms_from_env(App, CACerts) ->
    Defs = application:get_env(App, realms, []),
    Default = <<"default">>,
    case application:get_env(App, ca_certs) of
        {ok, _} when is_list(Defs) ->
            case lists:keymember(Default, 1, Defs) of
                true -> {error, ca_certs_and_default_realm};
                false -> realms(Defs ++ [{Default, CACerts}])
            end;
        _ when is_list(Defs) ->
            NeedsDefault =
                application:get_env(App, routing) =:= undefined
                andalso application:get_env(App, titan_work_dir,
                                            undefined) =/= undefined
                andalso not lists:keymember(Default, 1, Defs),
            case NeedsDefault of
                true -> realms(Defs ++ [{Default, []}]);
                false -> realms(Defs)
            end;
        _ ->
            realms(Defs)
    end.


-spec site_from_env(App, Hostname, Cert, Key, Routes, Realms, Prep) ->
              Result
              when App      :: atom(),
                   Hostname :: string(),
                   Cert     :: string(),
                   Key      :: string(),
                   Routes   :: [route_entry()],
                   Realms   :: realm_table(),
                   Prep     :: map(),
                   Result   :: {'ok', map()} | {'error', term()}.
site_from_env(App, Hostname, Cert, Key, Routes, Realms, Prep) ->
    % Today's sys.config only ever describes a single site, so this
    % is a one-element list; everything downstream looks sites up by
    % hostname and doesn't assume that.
    case build_site(Hostname, Cert, Key, Routes, Realms, Prep) of
        {ok, Site} ->
            Port = application:get_env(App, port, ?PORT),
            Servlets = application:get_env(App, servlets,
                                           ?DEFAULT_SERVLETS),
            {ok, make(Port, Servlets, [Site])};
        {error, _} = Error -> Error
    end.


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
                     ?assertEqual({ok, #{}}, realms_from_env(App, []))
                 end),
          ?_test(begin
                     Reset(),
                     Set(realms, [{<<"staff">>, [CA]}]),
                     {ok, T} = realms_from_env(App, []),
                     ?assertEqual([Staff], maps:keys(T))
                 end),
          ?_test(begin
                     Reset(),
                     Set(ca_certs, [CA]),
                     {ok, T} = realms_from_env(App, [CA]),
                     ?assertEqual([Default], maps:keys(T))
                 end),
          ?_test(begin
                     Reset(),
                     Set(ca_certs, [CA]),
                     Set(realms, [{<<"staff">>, [CA]}]),
                     {ok, T} = realms_from_env(App, [CA]),
                     ?assertEqual([Default, Staff], lists:sort(maps:keys(T)))
                 end),
          ?_test(begin
                     Reset(),
                     Set(ca_certs, [CA]),
                     Set(realms, [{<<"default">>, [CA]}]),
                     ?assertEqual({error, ca_certs_and_default_realm},
                                  realms_from_env(App, [CA]))
                 end),
          % a realm "default" is fine when ca_certs is not set
          ?_test(begin
                     Reset(),
                     Set(realms, [{<<"default">>, [CA]}]),
                     ?assertMatch({ok, _}, realms_from_env(App, []))
                 end),
          % the default table's Titan route needs the realm "default",
          % empty if ca_certs is not set
          ?_test(begin
                     Reset(),
                     Set(titan_work_dir, "/w"),
                     {ok, T} = realms_from_env(App, []),
                     ?assertEqual([Default], maps:keys(T)),
                     ?assertEqual({ok, {private, [Default], []}},
                                  blizanci_auth:valid_authz_policy(
                                    {private, Default}, T))
                 end),
          ?_test(begin
                     Reset(),
                     Set(titan_work_dir, "/w"),
                     Set(ca_certs, []),
                     {ok, T} = realms_from_env(App, []),
                     ?assertEqual([Default], maps:keys(T))
                 end),
          ?_test(begin
                     Reset(),
                     Set(titan_work_dir, "/w"),
                     Set(realms, [{<<"default">>, [CA]}]),
                     {ok, T} = realms_from_env(App, []),
                     ?assertMatch({_, [_]}, maps:get(Default, T))
                 end),
          ?_test(begin
                     Reset(),
                     Set(titan_work_dir, "/w"),
                     Set(routing, []),
                     {ok, T} = realms_from_env(App, []),
                     ?assertEqual([], maps:keys(T))
                 end),
          ?_test(begin
                     Reset(),
                     Set(titan_work_dir, "/w"),
                     Set(ca_certs, [CA]),
                     Set(realms, [{<<"default">>, [CA]}]),
                     ?assertEqual({error, ca_certs_and_default_realm},
                                  realms_from_env(App, [CA]))
                 end),
          ?_test(begin
                     Reset(),
                     Set(realms, [{<<"staff">>, ["/nonexistent.pem"]}]),
                     ?assertMatch({error, {invalid_ca_file, _, _}},
                                  realms_from_env(App, []))
                 end),
          ?_test(begin
                     Reset(),
                     Set(realms, [{staff, [CA]}]),
                     ?assertEqual({error, {invalid_realm_name, staff}},
                                  realms_from_env(App, []))
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
