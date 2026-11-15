# Blizanci Changelog

## 7.0.1

- Declare OTP 27 as the minimum version in rebar.config, so that rebar3
  refuses an older OTP with a clear message (erlexec 2.4.0, a
  dependency since 5.10.0, already needed it)

## 7.0.0

- Remove single-site mode: the `sites` key is now the only way to
  configure the sites of a server in sys.config (breaking change)
- UPGRADING: the keys `hostname`, `certfile`, `keyfile`, `docroot`,
  `cgiroot`, `ca_certs`, `titan_work_dir` and `routing`, when set at the
  top level of the `blizanci` section, are no longer read: start-up is
  refused with one line naming them. Move them into an entry of `sites`,
  `{Hostname, [{certfile, ...}, {keyfile, ...}, {routing, [...]}]}`
- UPGRADING: the default routing table, the `titan_work_dir` shorthand
  and the realm `default` that `ca_certs` defined are gone with it:
  write the routes, and define the realms in `realms`, explicitly
- A configuration with no sites is refused with a message that names
  the `sites` key
- Rewrite the README's configuration section and
  `config/sys.config.example` for the one format, with short examples
- Correct stale and garbled comments in several module headers; the
  logging header now says what `module_level/2` needs to show debug
  output

## 6.0.0

- Multi-site support: several sites on one port, each with its own
  certificate, key and routes, chosen by SNI
- Multi-site necessitates a breaking change to the config system: a new
  route format, authentication realms, a new default routing table,
  and binary directory options (the details follow)
- UPGRADING: the default routing table no longer has `/private` or
  `/restricted` routes. Files under `docroot/private` and
  `docroot/restricted`, which the old default table protected, are now
  served publicly unless you write your own `routing` key; the README
  gives an example with both routes
- Add two route tuples, `{mount, Proto, Path, Module, Auth, Opts}` and
  `{regex, Proto, Re, Module, Auth, Opts}`, kept in one ordered list
  per site, first match wins, one protocol per route (see the README,
  Routing)
- A mount matches whole path segments only: `/docs` matches `/docs`,
  `/docs/` and `/docs/a.gmi`, not `/docsx/a.gmi`. A mount path must
  start with `/`, have no empty, `.` or `..` segment, and be printable
  ASCII (codes 32 to 126); anything else is refused when the routes
  are built
- A regex route is anchored to the whole request path and must have a
  named group `PATH` and none named `FULL_PATH`
- For a mount, the servlet's `PATH` is the remainder after the mount,
  so each mount names its own root directory; every kind of route also
  passes `FULL_PATH`, the whole request path as the router sees it
- Translate the old route tuple `{Proto, Regex, Module, Auth, Opts}`
  with unchanged behaviour (its regex stays unanchored) and log one
  warning at start-up, with the count, if any are present
- Warn at start-up, once per route, about a route that an earlier route
  of the same protocol always matches first; this never refuses a table
- Add authentication realms: a named set of CA files, global to all
  sites, defined by the new `realms` key, for example
  `{realms, [{<<"staff">>, ["ca1.pem"]}]}`. A route's policy may be
  `{private, {realm, Name}}` or a list of them, and an undefined realm
  is an error naming the route
- New-format routes accept only realm policies; `{private, Files}` in a
  mount or regex route is an error naming the route. Old route tuples
  still take a file list
- `ca_certs` is now shorthand for the realm `<<"default">>`; setting it
  as well as a realm of that name in `realms` is a start-up error
- Allow a realm with no CA files; a private route on it authorises
  nobody, and nothing is logged
- The default routing table is now `/cgi-bin` (cgiroot) and `/`
  (docroot), in the new format, plus a Titan route if the new
  `titan_work_dir` key is set. The Titan route uses the configured
  docroot and requires a client certificate from the realm `default`;
  with `ca_certs` unset, nobody can upload
- Titan's `work_dir` has no default. Start-up refuses a Titan route
  whose `work_dir` is missing, not an absolute path, not an existing
  directory, or on a different filesystem from the route's docroot
- Library callers: remove the docroot field from the `#site` and
  `#server_config` records and the Docroot argument of `make_site`.
  `make_site/4` is `(Hostname, Cert, Key, Routes)` and `make_site/5`
  now takes a realm table as its last argument, so a call written for
  the old `make_site/5` will not do what it did. The single-site
  `docroot` key of sys.config stays, as input to the default table
- The static servlet's directory redirects use `FULL_PATH`, so they
  are to the whole requested path
- Routing table errors say which route they are about, by its position
  and kind (mount, regex or old-format)
- Add the `sites` key to sys.config: several sites on one port, each
  `{Hostname, [{certfile, ...}, {keyfile, ...}, {routing, [...]}]}`.
  A site takes those three keys only, all required, with no default
  table and no shorthand keys; `port`, `servlets` and `realms` stay
  global. SNI picks the site and its certificate. Hostnames are folded
  to lower case, and the same hostname twice is an error (see the
  README, Several sites)
- Refuse the `sites` key together with any single-site key (`hostname`,
  `certfile`, `keyfile`, `docroot`, `cgiroot`, `ca_certs`,
  `titan_work_dir`, `routing`), with one line listing the keys found
- Require a site's certificate and private key to be a matching pair:
  a mismatch is refused when the site is built, also by the builder API
  and `apply/1`, naming the site and both files. A pair whose key type
  cannot be compared (for example DSA) is accepted with one warning
- Report every configuration problem at start-up in one pass, one
  `blizanci: ...` error line per problem naming the site or route,
  instead of the first or a badmatch. `blizanci_config:from_app_env/0`
  now returns `{error, [Reason]}`, a list
- Refuse port 0 at start-up on the sys.config path (valid ports are
  1 to 65535)
- Name the site in the shadowed-route and old-format warnings of a
  multi-site configuration: `routing: site b.example: ...`
- Convert the route options `docroot`, `cgiroot` and `work_dir` to
  binaries once, when a route is built (a string or a binary is
  accepted), in every route format, the default table and the `sites`
  key. Servlets and the path code see only binaries, and
  `blizanci_path:confine/2,3` and `resolve_request/3` take a binary
  root and refuse a string root
- Refuse a `docroot`, `cgiroot` or `work_dir` that is not a string or
  binary or has a character outside printable ASCII, with a one-line
  error naming the route and the option; `docroot_not_a_directory` and
  `cgiroot_not_a_directory` now carry binaries and have their own
  messages

## 5.34.0

- Accept TLS 1.2 as well as TLS 1.3
- Request a client certificate only for a site that has a restricted or
  private route; a site whose routes are all public never requests one
- Keep the running configuration in ETS as one row per site, so that
  looking up a site copies only that site

## 5.33.1

- Add tests of a successful CGI run, of the error paths of
  blizanci_app:start/2, and a Common Test check that a run of the
  server leaves no processes, ports, tables, names or files behind

## 5.33.0

- Remove the relx release configuration from rebar.config, so that
  `rebar3 release` no longer builds a release; `manage.sh start-daemon`
  and `stop-daemon` report that no release is available
- Name the remaining bare numeric limits and timeouts in the source as
  macros with comments

## 5.32.1

- Check the arguments of `blizanci_pool:start/3` before calling ppool:
  a pool name that is not an atom, a limit that is not an integer of at
  least 1, or a module that is not an atom is refused with
  `{error, {invalid_name | invalid_limit | invalid_module, Value}}`
- Replace the per-file copyright lines with an SPDX licence identifier
  (`Apache-2.0`), and put one copyright and licence notice in the
  README; the package's `licenses` entry now reads `Apache-2.0`

## 5.32.0

- Check at configuration time that a servlet module exports every
  blizanci_servlet callback: start/0, stop/0, cancel/1 and
  handle_client_data/2 are now checked as well as the four checked
  before, and a module missing any is refused with
  {servlet_missing_callbacks, Module, Missing} (breaking change for a
  servlet that omits one)
- Stop all servlets at shutdown, on reconfiguration and after a failed
  start even if one servlet's stop/0 returns something other than ok or
  raises; the servlet is logged as a warning and treated as stopped

## 5.31.3

- Document the servlet contract in three parts (module, servlet process
  and gateway) in the blizanci_servlet and blizanci_servlet_container
  module docs, with links from the README and the overview
- Fix the doc comments in blizanci_path and blizanci_x509 so that
  `rebar3 edoc` builds
- Rewrite docs-to-gh-pages to use a temporary git worktree and to
  create the gh-pages branch when it does not exist

## 5.31.2

- Pass Titan's upload state and job around as records, in place of
  long positional argument lists; no behaviour change

## 5.31.1

- Rename the exported types `peer_cert` and `client_cert` to
  `raw_peer_cert` and `client_cert_details`; no behaviour change
- Type a route's options as `map()` in the router and servlet
  container specs; no behaviour change

## 5.31.0

- A servlet's `start/0` callback may now return `{error, Reason}` as
  well as `ok`; a failed start gives
  `{error, {servlet_start_failed, Module, Reason}}` from
  `blizanci_config:apply/1` (the servlets already started by that call
  are stopped and nothing is published), and stops the servlets
  process from starting at application start
- Report a servlet that cannot start at application start as one line,
  `blizanci: servlet <Module> could not start: <Reason>`, and return
  `{error, Reason}` from the application's `start/2`, instead of
  crashing on a badmatch
- `blizanci_pool:start/3` returns `{error, Reason}` for any error from
  ppool other than an already-started pool, instead of crashing; an
  unexpected return becomes `{error, {unexpected, Value}}`
- Depend on ppool 1.3.0, which validates a pool's name, limit and
  worker spec when it is started

## 5.30.2

- Report a client certificate that is not yet valid as
  `cert_not_yet_valid` ("Client certificate not yet valid"), not as
  expired; `blizanci_x509:check_cert/1` and `blizanci_auth:authorised/2`
  can now return it
- Use the Gemini status codes for client certificate errors: 61 for
  `cert_not_authorised`, and 62 for `cert_expired`, `cert_not_yet_valid`,
  `cert_not_parsed` and `cert_unsupported_encoding` (they were all 60;
  only `cert_required` stays 60)
- A client certificate with no Common Name no longer gives an empty
  `REMOTE_USER` in CGI; the variable is left unset. In
  `cert_details()` the `common_name` and `issuer_common_name` may be
  `undefined`
- Answer 51 (a missing file or a directory) or 40 (any other open
  error) when a static file cannot be opened, instead of a `20` header
  with an empty body; the file is opened before the header is sent, and
  sent from the open handle
- Breaking for library callers: read and decode a private route's CA
  files once, when the route is built, instead of on every request. All
  the certificates in a file are trusted (a bundle works), where only the
  first was; a file with an undecodable certificate is refused at build
  with `{invalid_ca_file, File, certificate_not_decodable}`; a changed
  CA file takes effect only when the route is rebuilt and applied, or on
  a restart. The built route's `auth_policy` is now
  `{private, Files, Certs}`, and `blizanci_x509:certificate_from_file/1`
  is replaced by `certificates_from_file/1`

## 5.30.1

- Add unit tests for `blizanci_osenv` and the CGI environment it
  sanitises, for the runtime log-level control in `blizanci_log`, and
  for the worker pool helpers in `blizanci_pool`

## 5.30.0

- Answer 40 and log, rather than crash the connection, when a servlet
  returns a reply `respond/2` has no clause for, or an error atom with
  no status; `blizanci_status:gemini_status/1` is now total, giving
  `{40, <<"Internal server error">>}` and a warning for an unknown atom
- Give the atom `unimplemented` its own status, 40 "Not implemented";
  as it now counts as an error, the servlet container reports it as an
  ordinary gateway error, not an init error
- Take the success status from the status table instead of a literal 20
- Fold the host of a request to lower case once, on entry, and compare
  it directly with the site's (already lower-case) name
- `blizanci_static` no longer falls back to built-in values for a
  missing `docroot`, `index`, `bare_mimetype` or `unknown_mimetype`,
  and has no default docroot; `validate_options/1` already required
  all four

## 5.29.0

- Breaking for library callers: `blizanci_connection:servlet_result/2`
  and `blizanci_servlet_container:gateway_exit/2` are now casts, so a
  servlet or CGI worker never blocks on the connection or the
  container, and never exits if the other is gone; this removes a 5
  second deadlock that lost CGI output when the client sent data just
  as the script finished
- `blizanci_servlet_container:handle_client_data/2` answers
  `{error_code, internal_server_error}` (status 40) when the container
  has gone, instead of returning `ok`

## 5.28.0

- Add a Common Test suite (`rebar3 ct`) exercising the running
  application over TLS: static files, CGI, Titan uploads, SNI with a
  live `blizanci_config:apply/1`, and stop/start in one VM

## 5.27.0

- Supervise the servlets' worker pools in the application's own tree:
  a new `blizanci_servlets` process starts and stops the declared
  servlets' resources, and `blizanci_conf_serv` now only owns the
  configuration table. The tree is `rest_for_one` (table, servlets,
  listener), with restart intensity 3 per 10 seconds
- Breaking for library callers: starting the live configuration by
  hand now needs `blizanci_servlets:start_link/0` after
  `blizanci_conf_serv:start_link/1`; the latter no longer starts the
  servlets
- A restart of the servlets process no longer reverts a configuration
  applied with `blizanci_config:apply/1`, and the pools survive it
- `blizanci_config:apply/1` now makes its changes through
  `blizanci_servlets`, so concurrent applies are serialised
- Declare `public_key` and `ssl` as applications blizanci depends on,
  and drop the hand-kept module list from the `.app` file
- Add the standard copyright header to the test support module and the
  test stubs

## 5.26.0

- Refuse at start-up a route whose regex has no named group PATH
  (`{route_without_path_group, Regex, Servlet}`); such a route used to
  crash on its first request
- Check a private route's CA files when the route is built, refusing a
  missing, unreadable or non-certificate file with
  `{invalid_ca_file, File, Why}`; `{private, X}` with a non-list X is
  now `{error, invalid_authz_policy}`; new
  `blizanci_x509:validate_ca_file/1`
- Require a servlet module to export `request/4` and `serve/4` as well
  as `default_options/0` and `validate_options/1`, refusing it with
  `{servlet_missing_callbacks, Module, Missing}` (breaking for servlets
  lacking them)
- Breaking for library callers: `blizanci_config:make_site/5` returns
  `{error, {invalid_routing, Reason}}` for a bad route, and
  `from_app_env/0` returns `{ok, Config} | {error, Reason}`; the
  application fails to start with a one-line message (new
  `blizanci_config:describe_error/1`)
- Say at start-up, naming the directory and how to fix it, when the
  Titan route's work_dir is not a directory
- Answer 40 when request handling raises an error or an exit, not only
  a throw, logging the class, reason and stack
- Accept a string regex in `blizanci_router`'s `route_info()`

## 5.25.0

- Fold the host and the scheme of a request, and site names, with an
  ASCII-only lowercase, exported as `blizanci_config:ascii_lowercase/1`
- Answer a CGI request refused before the script runs with the refusal's
  own status: 51 for a missing or hidden script, 60 for an unreadable
  client certificate
- Keep the existing resource, and remove the temporary file, when a Titan
  upload cannot be put in place (the work directory on another filesystem,
  or a directory as the target), answering 40
- Answer a Titan upload normally when its temporary file cannot be
  removed (for example in a read-only work directory), logging the failure
  instead of crashing; `delete/1` now returns `ok | {error, atom()}`
- Log a path refused for a hidden segment, a control character or a failed
  resolution at debug level instead of warning; only a path resolving
  outside the root, and root misconfiguration, still log a warning
- Log a CGI script's stderr at debug level (long chunks truncated); the
  response to the client is unchanged
- Stop a CGI script, and its worker, when the servlet container that
  started it dies without cancelling it (the worker now monitors its
  parent); previously the script ran on to completion
- Have the connection process trap exits for its whole life and answer 40
  only for an exit from its own servlet; an exit from any other linked
  process is logged and ignored
- Add `blizanci_path:resolve_request/3`, the shared step that confines a
  request's PATH under a route root, used by the static, CGI and Titan
  servlets

## 5.24.0

- Treat the URL scheme, the URL host and the SNI name as case-insensitive;
  site names are now stored in lower case, so `blizanci_config:site/3`
  returns `{ok, Site}` or `{error, hostname_not_lowercase}`, a breaking
  change for library callers

## 5.23.0

- Decode percent-escapes in the path of a request, once, a segment at a time
  (new module blizanci_urlpath)
- Refuse a request path that decodes to an escaped "/", a NUL, a control
  character or a non-ASCII byte, with 59
- Answer a malformed percent-escape with 59, instead of crashing the
  connection process
- Split Titan parameters off the raw path before decoding it, and refuse a
  decoded ";" in a Titan file name
- Percent-encode the location of a redirect
- Limit CGI script paths to letters, digits and ". _ - /"

## 5.22.0

- Handle file names as bytes in blizanci_path:confine/2,3: the request path is
  a binary and the result is a binary, and the root may be a binary or a
  string; control characters (below 0x20, and DEL) are refused, and other
  bytes are allowed, whether or not they are valid UTF-8. This replaces the
  printable-ASCII restriction, and is a breaking change for any third-party
  servlet that calls confine/2,3 with strings
- Static and Titan pass binary paths straight through; CGI script names remain
  ASCII-only
- Update realpath to 1.5.0, which accepts and returns binaries

## 5.21.1

- Update realpath to 1.4.1, which resolves a symlink whose target is not valid
  UTF-8 instead of ignoring it; in a UTF-8 VM the old version let a symlink in
  the served tree lead out of the root unnoticed, and such a link is now
  refused
- Track rebar.lock in git, so builds pin the exact version and content hash of
  every dependency

## 5.21.0

- Refuse hidden files: a request path with any segment beginning with a dot
  (such as `.git/config` or `.env`) now gets 51 File not found, from static,
  Titan (creating, overwriting and deleting) and CGI; the path is checked as
  requested and as resolved, so symlinks to or from dot names are caught, and
  there is no opt-out yet
- Allow `.well-known` as the first segment under the root for static and Titan
  (not CGI), as RFC 8615 places it; a `.well-known` further down, or a dotfile
  inside it, is still refused

## 5.20.1

- Make `blizanci_path:confine/2` refuse a request path containing anything
  other than printable ASCII (control characters, NUL and non-ASCII are
  refused), closing a case where the static servlet could follow a symlink
  named with a non-ASCII character out of the docroot when the VM's file-name
  encoding was UTF-8
- Add tests that a host name or SNI name with a trailing dot stays refused

## 5.20.0

- Reject a request whose URL has a fragment, with status 59, as the Gemini
  specification requires; it was previously served as if the fragment weren't
  there
- Reject a request with no URL scheme, with status 59, instead of assuming
  `gemini` for `//host/path`
- Add tests, compiled out until the code is fixed (see
  `PENDING_CASE_INSENSITIVE` in the sources), for treating the URL scheme and
  host, and the SNI host name, as case-insensitive

## 5.19.0

- Refuse Titan uploads that declare a size over 10 MiB, before any data is
  read, with status 59; the limit is fixed in the source for now
- Refuse Titan uploads that send more data than they declared, with status 59,
  installing nothing and deleting what was received; this includes a `size=0`
  request that carries data
- Fix the Titan temp-file purge treating a file with a modification time
  slightly in the future as stale, which could delete the temp file of an
  upload still in progress

## 5.18.1

- Create Titan upload temp files exclusively, with names built from the OS
  process id, time and a unique counter, so concurrent uploads can no longer
  overwrite each other's temp file and a symlink planted at a temp-file name
  is not followed

## 5.18.0

- A Titan request with `size=0` now deletes the resource, as the Titan
  specification says, instead of creating an empty file; only regular files
  inside the upload root are deleted, and a zero-size request that carries
  data is refused

## 5.17.1

- Default a missing Titan `mime` parameter to `text/gemini`, as the Titan
  specification says, instead of crashing the request
- Resolve a Titan upload's target path again immediately before writing it, so
  a directory swapped for a symlink during a long upload can no longer
  redirect the write outside the upload root

## 5.17.0

- Reject Titan requests whose `size` is missing, not a number or negative,
  with a status 59 error instead of crashing the upload worker
- Refuse to start, or to validate a config, where a Titan route's `docroot` or
  `work_dir` is, contains or lies inside a CGI route's `cgiroot`; this can
  reject configurations that previously ran
- Only purge blizanci's own temp files from the Titan `work_dir`: they now
  carry a `blizanci-tmp-` prefix, and the purge leaves everything else alone;
  temp files left by earlier versions (digit-only names) are no longer purged
  and can be deleted by hand
- Fix the Titan temp-file age check, which compared only the time-of-day part
  of a file's age and used its access time instead of its modification time

## 5.16.2

- Rename the internal `blizanci_live` module to `blizanci_conf_serv`, document
  what it manages, and list it in `blizanci.app.src`

## 5.16.1

- Fix a crash in connection handling when a client disconnects immediately
  after the TLS handshake
- Hold the live site configuration in a supervised ETS table instead of
  `persistent_term`; the declared servlets are now started and stopped by the
  process that owns it

## 5.16.0

- Add `blizanci_config:new/2`, `routes/1`, `insert_route/3` and `validate/1`,
  completing the function-based config builder
- Add `blizanci_config:apply/1`, which reconfigures a running instance without
  restarting its listener: sites, certificates and the declared servlets can
  change live (a change of port is refused)
- Look sites up in a live store shared by the TLS layer and connections, so
  site changes take effect for every new handshake

## 5.15.0

- Add `blizanci_config:route/5`, which builds and validates a single route and
  returns a specific `{error, Reason}` for a bad regex, protocol, servlet,
  auth policy or servlet options, instead of crashing
- Add `blizanci_config:global/2`, `site/3` and `add_route/2` as the first
  pieces of a function-based config builder
- Fix `blizanci_path:confine/2` accepting a sibling directory whose name
  begins with the root's name (e.g. `/srv/docs-secret` for a root of
  `/srv/docs`)

## 5.14.2

- Make `blizanci_config` take its inputs explicitly (`make/3`), confining
  sys.config reading to `from_app_env/0`, so the config can be built from
  outside blizanci's own application context

## 5.14.1

- Introduce a `site()` record and look sites up by hostname for both the SNI
  callback and per-connection routing, ready for more than one site per
  instance

## 5.14.0

- Require SNI for every connection; a client that doesn't send it (or sends an
  unrecognised hostname) is now refused at the TLS handshake, before any
  request is processed
- Fix a stray `Response` return-variable name left over from the
  spec-alignment work in `blizanci_cgi`

## 5.13.0

- Cap accumulated CGI output at 10 MiB; a script that exceeds it has its whole
  process group killed and gets reported as a gateway error, instead of the
  buffer growing unbounded
- Fix CGI process cleanup to kill a script's entire process group, not just
  the top-level shell, so a piped subprocess can no longer be left running as
  an orphan
- Extract shared `blizanci_pool` helpers (start/stop/cancel/enqueue) used by
  both `blizanci_cgi` and `blizanci_titan`, removing duplicated `ppool`
  boilerplate

## 5.12.0

- Declare servlets explicitly via app config instead of inferring them from
  the routing table, so a routing update can reference any declared servlet
  without needing its pool started separately

## 5.11.0

- Add a `validate_options/1` counterpart to `default_options/0` on the
  `blizanci_servlet` behaviour, so a bad root directory fails at startup
  instead of on first request
- Add missing `docroot`/`titan-temp` fixture directories for Titan's own
  defaults

## 5.10.0

- Add a `stop/0` counterpart to the `blizanci_servlet` behaviour's `start/0`,
  so driver pools don't leak across a stop/start cycle in the same VM
- Fix a CWD-sensitive stale assertion in the request-handling test suite

## 5.9.3

- Remove unused gen_server.hrl
- Realign specs in the Titan module
- Use a proper record for Titan job state instead of an anonymous tuple

## 5.9.2

- clear the OS environment when doing CGI, via erlexec's own facility

## 5.9.1

- Register missing logging module

## 5.9.0

- Supervise the ranch listener properly, so a crash gets restarted
  automatically

## 5.8.0

- Verify client certificates against a configured issuer by signature,
  not by name comparison: a self-signed certificate with a spoofed
  Issuer field previously satisfied the `private` auth policy without
  ever needing the issuer's private key

## 5.7.1

- Disallow symlinks out from under root dirs for static / CGI / Titan

## 5.7.0

- Reject expired client certificates (previously accepted indefinitely
  regardless of the auth policy's issuer check)
- Never crash on a client cert we can't fully parse; report it as a
  distinct, dedicated status instead
- Document NUL-byte handling in the CGI environment

## 5.6.0

## 5.5.1

- Use later version of erlexec
- Adapt to recent OTP requirements around CA certs
- Pass $PATH_INFO through
- Improve handling of CGI (stray messages, internal records)
- Remove deprecated `format_status/2` callback
- Various spec fixes

## 5.5.0

- Parse CGI output more sensibly; adopt NPH (Non-Parsed Headers) by default

## 5.4.0

- Fix path normalisation in `fix_path`
- Add missing member of `gemini_response()` type enum

## 5.3.2

- Fix minor path normalisation issue

## 5.3.1

- Improve test suite: use test generators; adjust for paths in test rig

## 5.3.0

- Remove lager logging framework; switch to OTP logger
- Switch to unified config with improved active module detection
- Make `start/0` mandatory for servlet behaviour
- Break main module into two parts

## 5.2.0

- Rename central module
- Fix types for client certs and propagate through codebase
- Use dialyzer types for request details
- Break handler function out of callback

## 5.1.2

- Deal with newer SSL apps that force client certificates
- Document routing format
- Break Gemini status codes out into own module
- Refactor Titan response system
- Enforce handling of paths as binary

## 5.1.0

- Break tmpdir code out into own module
- Improve handling of servlet errors
- Handle attempts to save a directory over a file
- Fix missing apps in release

## 5.0.0

- Add Titan protocol implementation
- Remove Titan temp files older than one hour
- Send correct protocol response on timeout
- Set timeout on servlet startup
- Protect Titan uploads with authorisation checks

## 4.8.0

- Add stub Titan module with protocol plumbing
- Support protocols with more than one line of input
- Pass protocol scheme through as atom

## 4.7.0

- Add CA cert-style certificate verification
- Move x509-related functions into appropriate module
- Adapt auth checker to new private key reference scheme
- Enforce valid authorisation policies at app start time
- Remove CGI dependency on hostname/port config
- Move auth checks from servlet into main server

## 4.6.0

- Support separate auth policy per routing table entry
- New routing table format (breaking change)

## 4.5.0

- Pass server config through to servlets
- Split authorisation out into own module
- Add GitHub Pages documentation tooling
- Document the configuration system

## 4.4.0

- Handle CGI processes that fail before starting
- Clarify how the URL matches/routing system works

## 4.3.0

- Move certificate verification function out of protocol handler module

## 4.2.1

- Fix certificate import

## 4.2.0

- Require that certificates be configured at startup

## 4.1.0

- Pin dependency versions more tightly

## 4.0.0

- Upgrade to Cowboy 2.0.0

## 3.0.0

- Force use of TLS 1.3

## 2.1.0

- Use router config to handle restricted files
- Remove servlet-specific config from main Gemini state

## 2.0.0

- Major refactor: convert to routing table architecture
- Use map for CGI options
- More explicit servlet interface and client cert types

## 1.13.0

- Add formal behaviour definition for servlets

## 1.12.0

- Rename `blizanci_servlet` to `blizanci_servlet_container`

## 1.10.0

- Fix module list in release

## 1.9.2

- Remove bypass support
- Rename static file module

## 1.9.1

- Split CGI and static files into abstract deferrable servlets
- Disentangle type puns in servlet interface

## 1.9.0

- Use callback functions for inter-process communication instead of
  out-of-band messages

## 1.7.0

- Fix application lifecycle: add top-level supervisor
- Handle null query strings
- Add tools for sanitising the OS environment passed to CGI

## 1.6.0

- Split MIME lookup out into a separate application

## 1.5.0

- Use a worker pool for CGI processes
- Improve defaults

## 1.3.0

- Break config out into separate module
- Prevent userinfo component in URLs

## 1.2.2

- Tidy up CGI cancellation logic

## 1.2.1

- Use synchronous messaging to cancel CGI servlets

## 1.2.0

- Handle CGI via servlet architecture

## 1.1.2

- Kill CGI processes on client hangup
- Terminate server gracefully on shutdown
- Add access log
- Handle non-existence of CGI scripts
- Improve MIME type initialisation robustness

## 1.1.1

- Handle certificates with no subject common name

## 1.1.0

- Only set `REMOTE_USER` environment variable when client is authenticated

## 1.0.0

- Reorganise configuration; enforce CGI root and client cert settings
- Use hex.pm for dependencies
- Improve CGI environment, including cert subject common name
- Handle client certificates properly
- Split x509 handling into own module
- Use conventional libraries for URL and config processing

## 0.1.0

- Initial release
