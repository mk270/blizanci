%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_config).

-moduledoc """
The configuration of a blizanci instance: built from the application
environment (sys.config) by from_app_env/0, or from explicit values by
the builder functions (new/2, site/3, add_route/2 and so on).

See the manual chapter [Configuration](configuration.md).
""".
-include("blizanci_types.hrl").

-export([from_app_env/0, make/3, find_site/2, route/2, route/5, route/6,
         realm/1, add_realm/3, realms/0, realms/1,
         global/2, site/3, add_route/2, insert_route/3, routes/1,
         new/2, validate/1, validate_all/1, apply/1, ascii_lowercase/1,
         describe_error/1, tls_versions/0, site_wants_client_cert/1]).

% A site record with no check on its files, for tests of everything but
% the files themselves; site/3 refuses a certificate and key that do not
% match.
-ifdef(TEST).
%% Exported for the tests in test/blizanci_config_tests.erl.
-export([describe_route_error/1, problems_site/1, realms_from_env/1,
         ssl_opts/1, test_site/3]).
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


% A one-line, human-readable account of an error from make_site/4 or
% from_app_env/0, or from starting the supervision tree (a servlet that
% will not start), for the log at start-up.
-spec describe_error(Error) -> Result
              when Error  :: term(),
                   Result :: string().
describe_error({invalid_routing, Reason}) ->
    "invalid routing table: " ++ describe_route_error(Reason);
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
    "no sites are configured; the sites key must list at least one";
describe_error({single_site_keys_not_supported, Keys}) ->
    flat("the single-site keys ~s are no longer supported; move them "
         "into an entry of the sites key",
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
describe_route_error({invalid_mount, Path, non_ascii}) ->
    flat("mount path ~w is invalid (not plain ASCII); a mount must "
         "consist of printable ASCII characters only", [Path]);
describe_route_error({invalid_mount, Path, Why}) ->
    flat("mount path ~0p is invalid (~s); it must be a non-empty string "
         "starting with \"/\", with no empty, \".\" or \"..\" segments "
         "and no control characters or non-ASCII",
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
describe_route_error({invalid_path_option, Key, Value}) ->
    flat("the ~s option must be a plain ASCII file name (printable "
         "ASCII characters only), not ~w", [Key, Value]);
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
describe_route_error({invalid_route_options, _Module,
                      {docroot_not_a_directory, Dir}}) ->
    flat("the route's docroot ~ts is not a directory; create it, or "
         "set docroot to an existing directory", [Dir]);
describe_route_error({invalid_route_options, blizanci_cgi,
                      {cgiroot_not_a_directory, Dir}}) ->
    flat("the route's cgiroot ~ts is not a directory; create it, or "
         "set cgiroot to an existing directory", [Dir]);
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
-doc """
Generate the options required for ranch to initialise SSL
""".
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
-doc """
The TLS versions blizanci offers, most preferred first: 1.3 and 1.2.
This is the one place the list is set. Cipher suites and the other
TLS options are OTP's defaults.
""".
tls_versions() ->
    ['tlsv1.3', 'tlsv1.2'].

-spec site_wants_client_cert(Routes) -> Result
              when Routes :: [route()],
                   Result :: boolean().
-doc """
Whether a site with these routes should ask the client for a
certificate during the TLS handshake: true if any route, gemini or
titan, is restricted or private, false if every route is public
(or there are none). The path is not known at handshake time, so one
such route means every client is asked. A site that does not ask
never has a certificate sent to it, which matters in TLS 1.2, where
the client certificate travels unencrypted.
""".
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
-doc """
Generate the options required for ranch to initialise the TCP listener
""".
proto_opts(Port) ->
    [{port, Port}].


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


% The flat keys of the single-site form of earlier versions, which are
% no longer supported: their presence is refused.
-define(OLD_KEYS, [hostname, certfile, keyfile, docroot, cgiroot,
                   ca_certs, titan_work_dir, routing]).

% The one place that reads sys.config: pulls blizanci's app env into
% the explicit values make/3 takes. Must be called from within
% blizanci's own application context (application:get_application/0
% resolves via the calling process), i.e. from blizanci_app:start/2.
%
% The sites are configured by the sites key only; the global keys are
% port, servlets and realms. Any of the flat keys of the old
% single-site form (see ?OLD_KEYS), with or without sites, is refused
% as one error naming the keys found, with nothing else reported (it
% is not known which form was meant). With no sites key the list of
% sites is empty, and the error is no_sites.
%
% Otherwise every problem is reported in one pass, as a list of errors,
% each of which describe_error/1 turns into one line. The configuration
% is built without stopping at the first error, and then validate_all/1
% is run on what was built, so that a bad realm, a bad route, an
% unreadable certificate and an undeclared servlet are all found
% together. The order of the list is stable: first the problems of the
% realms key, then those found building each site in turn (its
% hostname, then its routes in order), each as {site, Hostname, Reason},
% then those of validate_all/1.
%
% Where one error makes a later check meaningless, it is not allowed to
% cause a second, misleading one:
%  - a route that names a realm whose definition failed to build gets no
%    unknown_realm error (the realm's own error is already listed); if
%    the realms key is so malformed that no name can be said to have
%    failed, no unknown_realm error is reported at all;
%  - a route that failed to build is left out of its site, and the
%    checks that need the routes (upload directories, declared servlets)
%    run on those that did build;
%  - if a site itself cannot be built (a bad hostname, or an error
%    from site/3), it is left out of the configuration, so that the
%    sites are not validated and "no sites" is not reported; only the
%    global checks and the servlets its routes name are.
-spec from_app_env() -> Result
              when Result :: {'ok', map()} | {'error', [term()]}.
from_app_env() ->
    {ok, App} = application:get_application(),
    case [ K || K <- ?OLD_KEYS,
                application:get_env(App, K) =/= undefined ] of
        [] -> sites_from_env(App, application:get_env(App, sites, []));
        Keys -> {error, [{single_site_keys_not_supported, Keys}]}
    end.


% The keys a site of the sites key takes, all required.
-define(SITE_KEYS, [certfile, keyfile, routing]).

% The sites key. Each site is built independently from its own certfile,
% keyfile and routing, and the realms (and port, servlets) are the
% global keys. Every problem of every site is reported in one pass, in
% the order of the sites, each labelled by its site; see from_app_env/0
% for the order and the cascade rule, which holds for each site.
-spec sites_from_env(App, Value) -> Result
              when App    :: atom(),
                   Value  :: term(),
                   Result :: {'ok', map()} | {'error', [term()]}.
sites_from_env(App, Value) ->
    Port = application:get_env(App, port, ?PORT),
    Servlets = application:get_env(App, servlets, ?DEFAULT_SERVLETS),
    Global = global(Port, Servlets),
    {Realms, Failed, RealmErrors} = realms_from_env(App),
    case is_list(Value) of
        true ->
            Outcomes = [ site_entry(E, Realms, Failed) || E <- Value ],
            assemble(Global, RealmErrors, Outcomes);
        false ->
            {error, RealmErrors ++ [{invalid_sites, Value}]
                    ++ problems_global(Global)}
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


% The global realms key.
%
% Every definition is tried, so every bad one is reported: the result is
% the table of those that built, the names of those that did not (or
% 'all' if some definition is not even a {Name, Files} pair, or the key
% is not a list, so no name can be blamed), and the errors, each
% {bad_realm, Name, Reason} or {realms, Reason}. Routes naming a failed
% realm are not also reported (see from_app_env/0).
-spec realms_from_env(App) -> Result
              when App    :: atom(),
                   Result :: {realm_table(), 'all' | [term()], [term()]}.
realms_from_env(App) ->
    Defs = application:get_env(App, realms, []),
    case is_list(Defs) of
        false ->
            {realms(), all, [{realms, {invalid_realms, Defs}}]};
        true ->
            realm_defs(Defs, realms(), [], [])
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

-ifdef(TEST).
%% A site record with no check on its files (see the export above).
test_site(Hostname, Cert, Key) ->
    #site{hostname = erlang:list_to_binary(Hostname), certfile = Cert,
          keyfile = Key, routing = []}.
-endif.
