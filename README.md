blizanci
========

Blizanci is a [Gemini protocol](https://gemini.circumlunar.space/) server.
It is designed primarily for robustness and security.

Features:

* [TLS client certificate](https://en.wikipedia.org/wiki/Public_key_certificate#TLS/SSL_client_certificate) support
* single-user [CGI](https://en.wikipedia.org/wiki/Common_Gateway_Interface)
* distributed PKI: authorisation by subject or *issuer* of certificates
* Titan protocol uploads
* servlet interface for in-process dynamic page generation; the contract
  a servlet module must meet, in three layers (module, servlet process,
  gateway), is described in the module documentation of
  `blizanci_servlet` (`apps/blizanci/src/blizanci_servlet.erl`, or
  `blizanci_servlet.html` in the generated docs)

Prerequisites
-------------

You'll need Erlang/OTP 27 or later, and Rebar. rebar3 refuses to build on an older OTP. (Check yours with `erl -noshell -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt().'`; a packaged erlang may be older than 27.) The build and the run must use the same OTP. On a Debian-derived system like Ubuntu, this means installing the rebar3 executable from the rebar3 website, and installing the deb package erlang if it is recent enough:

* [Rebar3 installation instructions](https://www.rebar3.org/docs/getting-started#installing-binary)
* `apt-get install erlang`

Config
------

Currently, a config file is required. In due course, this will become optional.

Create a `sys.config` file, for example in the `config/` directory, in
the format suggested in `config/sys.config.example`. (The example
`vm.args` is for a release, which is not available for now; the way of
running blizanci described under Run does not read it.)

A minimal configuration serves one site, with one public route. The
`sites` key lists the sites; each has a hostname, a certificate, a
private key and a routing table. Nothing here is defaulted: the paths
are yours to choose, and the certificate and key must be a matching
pair that blizanci can read, or it will refuse to start:

    [{blizanci, [
       {sites, [
         {"example.org", [
           {certfile, "ssl/certificate.pem"},
           {keyfile, "ssl/key.pem"},
           {routing, [
             {mount, gemini, "/", blizanci_static, public,
              #{docroot => "./public_gemini"}}
           ]}]}
       ]}
     ]}].

To keep a part of the site for holders of a client certificate, define a
realm (a named set of CA certificates) in the top-level `realms` key and
name it in a private route. Routes are tried in order, so the more
specific mount comes first:

    [{blizanci, [
       {realms, [{<<"staff">>, ["ssl/staff-ca.pem"]}]},
       {sites, [
         {"example.org", [
           {certfile, "ssl/certificate.pem"},
           {keyfile, "ssl/key.pem"},
           {routing, [
             {mount, gemini, "/private", blizanci_static,
              {private, {realm, <<"staff">>}},
              #{docroot => "./public_gemini/private"}},
             {mount, gemini, "/", blizanci_static, public,
              #{docroot => "./public_gemini"}}
           ]}]}
       ]}
     ]}].

The old flat keys (`hostname`, `certfile`, `keyfile`, `docroot`,
`cgiroot`, `ca_certs`, `titan_work_dir` and `routing` at the top level
of the blizanci section) are no longer supported; see Sites, below.
`config/sys.config.example` is a fuller example.

Routing
-------

Each site has one routing table: an ordered list of routes. A request
is matched against the routes for its protocol (gemini or titan) in
order, and the first route that matches serves it. A route serves
exactly one protocol, so a static site that also takes Titan uploads
needs a gemini route and a titan route. If no route matches, the
client gets "not found". The table is the `routing` key of the site, in
`sys.config` (see Sites, below); there is no default table.

### Route kinds

A route is one of two tagged tuples:

    {mount, Proto, Path,  Module, Auth, Opts}
    {regex, Proto, Re,    Module, Auth, Opts}

`Proto` is `gemini` or `titan`. `Module` is the servlet, e.g.
`blizanci_static`, `blizanci_cgi` or `blizanci_titan`. `Auth` is the
authorisation policy (below), and `Opts` is the servlet's options map,
such as `#{docroot => "./public_gemini"}`. Each route names its own
root directory in `Opts`.

### Mounts

A mount selects requests by path prefix, in whole path segments. The
mount `"/docs"` (or `"/docs/"`, which is the same) matches `/docs`,
`/docs/` and `/docs/a.gmi`, but not `/docsx/a.gmi`. The mount `"/"`
matches everything.

The validator requires a mount path to be a non-empty string (or
binary) that starts with `/`. It may have no empty segment (so no
`//`), though one trailing `/` is allowed and ignored. It may have no
`.` or `..` segment. Only printable ASCII (codes 32 to 126) is
allowed: no control character and nothing non-ASCII, as a request path
is always printable ASCII once decoded.

What the servlet receives:

* `PATH`, the remainder of the request path after the mount, with no
  leading slash. For mount `/docs` and request `/docs/a/b.gmi`, PATH is
  `a/b.gmi`; for the request `/docs` itself it is empty. This is the
  path the servlet resolves under its own root, so a mount's `Opts`
  must name the directory that corresponds to the mount, not the
  site's whole document root.
* `FULL_PATH`, the full request path as the router sees it (decoded and
  normalised), with no leading slash: `docs/a/b.gmi` in the example.

### Regex routes

A regex route selects requests with a regular expression (Erlang `re`,
i.e. PCRE) that must match the whole request path: blizanci anchors it
itself, so `"docs/(?<PATH>.*)"` does not match `x/docs/a.gmi`. The
path matched has no leading slash. The regex must have a named group
`PATH`, which is what the servlet resolves under its root, and may not
have one named `FULL_PATH`. Other named groups pass through to the
servlet (CGI reads `PATH_INFO` this way). `FULL_PATH` is supplied as
for mounts. In `(?x)` mode, avoid a trailing `#` comment, as the
anchoring text is appended to the pattern.

### Authorisation

The `Auth` element of a route is one of:

* `public`: anyone may request it
* `restricted`: the client must present a certificate; which one does
  not matter
* `{private, {realm, Name}}`: the client's certificate must be signed
  by a CA certificate of the named realm
* `{private, [{realm, Name1}, {realm, Name2}]}`: as above, for any of
  the realms in the list

Realm names are binaries, e.g. `{realm, <<"staff">>}`; see Realms.
A realm is looked up when the route is built, at start-up (or at
`blizanci_config:apply/1`), not per request. A realm that is not
defined is an error naming the route and the realm. Mounts and
regex routes accept only these realm policies; a `{private, CAFiles}`
(a list of file names, even an empty one) in them is an error that says
to define a realm. Old 5-tuple routes (below) may still use
`{private, CAFiles}`.

### Old-format routes

The old 5-tuple, `{Proto, Regex, Module, Auth, Opts}`, is still
supported, and there is no plan to remove it. It is translated to the
new format and behaves exactly as before: its regex is NOT anchored
but searched for anywhere in the path, and it must have a `PATH`
group. It also receives `FULL_PATH` (unless its own regex has a group
of that name). If any route in the table uses the old format, one
line is logged at start-up, at warning level:

    routing: 5 routes use the old tuple format {Proto, Regex, Module,
    Auth, Opts}; translated, behaviour unchanged (see README, Routing)

(the count varies). Rewrite the routes as mounts or anchored regexes to
silence it; note that unanchored `"(?<PATH>private.*)"` also matches
`docs/private/x.gmi`, which an anchored regex or a mount would not.

### Shadowing

When the table is built, blizanci checks for routes that can never be
used because an earlier route of the same protocol matches every
request they could. This only warns, and never stops start-up or
changes routing. One line is logged per shadowed route, for example:

    routing: route 2 (gemini mount "/docs") is shadowed by route 1
    (gemini mount "/") and is never used

Mounts are compared exactly. Where a regex is involved, overlap is
probed with sample paths, and a partial overlap is not reported, so
the check can miss a shadowed route but should not report a false one.
Authorisation policies are not considered.

### Realms

A realm is a named set of CA certificates, and nothing more. Realms
are global: defined once, in the top-level `realms` key, and shared by
all sites:

    {realms, [{<<"staff">>, ["ssl/staff-ca1.pem", "ssl/staff-ca2.pem"]}]}

A client certificate is accepted for a route if it is signed by any
certificate in any file of any realm the route names; a file may be a
bundle of several certificates. Names are case-sensitive binaries of
1 to 64 printable ASCII characters. The files are read, and must hold
a certificate, when the table is built; a change to a CA file takes
effect only at `blizanci_config:apply/1` or on restart.

A realm may have an empty file list. A private route on it then
authorises nobody: a client sending no certificate gets status 60
(certificate required), and one sending a valid certificate gets 61
(certificate unauthorised). No warning is logged.

### Titan work directories

Titan stages uploads in a work directory before installing them in the
docroot. A Titan route's `work_dir` has no default and must be given.
Start-up fails unless it is an absolute path, exists, is a directory
and is on the same filesystem as the route's docroot (uploads are
installed by hard link and rename). Neither the docroot nor the work
directory of a Titan route may overlap a CGI root.

### Directory options

The directory options of a route (`docroot`, `cgiroot` and `work_dir`)
may be written as a string or as a binary. They are converted to
binaries once, when the route is built, and servlets only ever see
binaries. Library callers that supply their own servlets must therefore
expect a binary `docroot`, `cgiroot` and `work_dir` in their options.

They must be plain ASCII file names (printable characters only, codes
32 to 126). Anything else is refused at start-up with an error that
names the route and the option. Relative paths are kept as written.
`cgiprefix` is a URL prefix, not a file name, and is not affected.

### Example: a routing table

A table with protected areas: `/private` for clients with certificates
from the realm `staff`, and `/restricted` for any client with a
certificate. Each mount names its own subdirectory as its root, and
the more specific mounts come before `/`:

    {routing, [
      {mount, gemini, "/cgi-bin", blizanci_cgi, public,
       #{cgiroot => "./cgi-bin"}},
      {mount, gemini, "/private", blizanci_static,
       {private, {realm, <<"staff">>}},
       #{docroot => "./public_gemini/private"}},
      {mount, gemini, "/restricted", blizanci_static, restricted,
       #{docroot => "./public_gemini/restricted"}},
      {mount, gemini, "/", blizanci_static, public,
       #{docroot => "./public_gemini"}},
      {mount, titan, "/", blizanci_titan,
       {private, {realm, <<"staff">>}},
       #{docroot => "./public_gemini", work_dir => "/srv/titan-work"}}
    ]}

The realm `staff` must be defined in the top-level `realms` key. Relative
paths are allowed in `docroot`, `cgiroot` and `work_dir` (a Titan
`work_dir` must still be absolute, as above). Without the `/private` and
`/restricted` routes, the `/` route would serve those directories
publicly.

Sites
-----

The sites blizanci serves are set by the `sites` key. To serve several
hostnames from one port, each with its own certificate and routes, give
one entry for each:

    {blizanci, [
      {port, 1965},
      {sites, [
        {"a.example", [{certfile, "/srv/a/cert.pem"},
                       {keyfile, "/srv/a/key.pem"},
                       {routing, [{mount, gemini, "/", blizanci_static,
                                   public, #{docroot => "/srv/a"}}]}]},
        {"b.example", [{certfile, "/srv/b/cert.pem"},
                       {keyfile, "/srv/b/key.pem"},
                       {routing, [{mount, gemini, "/", blizanci_static,
                                   public, #{docroot => "/srv/b"}}]}]}
      ]}
    ]}

The rules:

* A site is `{Hostname, Options}`, and the hostname is a string. A site
  takes `certfile`, `keyfile` and `routing`, and nothing else; all
  three are required. There is no default routing table and there are
  no shorthand keys such as `docroot` or `cgiroot`. Every site spells
  out its own routes, and each route names its own root in its options
  (see Routing, above).
* `port` and `servlets` are global, as is `realms`. A route in a site
  names realms defined in the global `realms` key.
* The flat keys of the single-site form of earlier versions are no
  longer supported: if any of `hostname`, `certfile`, `keyfile`,
  `docroot`, `cgiroot`, `ca_certs`, `titan_work_dir` or `routing` is set
  at the top level of the blizanci section, with or without `sites`,
  start-up is refused with one line listing the ones found. Move them
  into an entry of `sites`; a CA given as `ca_certs` becomes an explicit
  realm in `realms`.
* One hostname per site. TLS clients send the name they are connecting
  to (SNI), and that picks the site and so its certificate and its
  routes. A client that sends a name that is not a site, or sends no
  name at all, is refused in the TLS handshake, before any request is
  read. If the host in the request URL differs from the SNI name, the
  client gets status 53 (host unrecognised).
* Hostnames are folded to lower case, so "A.Example" and "a.example"
  are the same name. The same name twice, after folding, is an error.
* A site whose routes are all public does not ask clients for a
  certificate; a site with a restricted or private route does.

A worked example: `a.example` is a public static site, and `b.example`
is a static site that also takes Titan uploads from clients holding a
certificate signed by the CA in the realm `staff`:

    {blizanci, [
      {port, 1965},
      {realms, [{<<"staff">>, ["/srv/ssl/staff-ca.pem"]}]},
      {sites, [
        {"a.example", [
          {certfile, "/srv/a/cert.pem"},
          {keyfile, "/srv/a/key.pem"},
          {routing, [
            {mount, gemini, "/", blizanci_static, public,
             #{docroot => "/srv/a/gemini"}}
          ]}]},
        {"b.example", [
          {certfile, "/srv/b/cert.pem"},
          {keyfile, "/srv/b/key.pem"},
          {routing, [
            {mount, gemini, "/", blizanci_static, public,
             #{docroot => "/srv/b/gemini"}},
            {mount, titan, "/", blizanci_titan,
             {private, {realm, <<"staff">>}},
             #{docroot => "/srv/b/gemini",
               work_dir => "/srv/b/titan-work"}}
          ]}]}
      ]}
    ]}

The warnings about old-format and shadowed routes (see Routing) are
logged for each site's table, and in a multi-site configuration the
line names the site, for example (one line in the log):

    routing: site b.example: route 2 (gemini mount "/docs") is
    shadowed by route 1 (gemini mount "/") and is never used

Start-up errors
---------------

At start-up blizanci checks the whole configuration and reports every
problem it finds in one pass, not just the first. Each problem is one
error-level log line starting `blizanci:` and naming the site or the
route concerned, so you can fix them all before the next attempt.
Start-up then fails. For example (these would not all occur together,
and each is one line in the log):

    blizanci: site a.example: certificate /srv/a/cert.pem and private
    key /srv/b/key.pem are not a matching pair
    blizanci: site b.example: the required key certfile is missing
    blizanci: port 0 is not a TCP port number (1 to 65535)
    blizanci: the single-site keys hostname, certfile are no longer
    supported; move them into an entry of the sites key
    blizanci: no sites are configured; the sites key must list at least
    one

Other problems reported this way include a hostname that is not a
string, a site configured twice, an unreadable or undecodable
certificate or key file, a route that names an undefined realm or a
servlet that is not in `servlets`, an invalid route, and a Titan work
directory that is missing or on the wrong filesystem.

For each site, the certificate and the private key must be a matching
pair; otherwise every TLS handshake for that site would fail, so
start-up is refused instead and the line names both files. If the key
is of a type that cannot be compared with its certificate (DSA, for
example), the pair is accepted with one warning (one line in the log):

    site a.example: cannot verify that certificate /srv/a/cert.pem and
    private key /srv/a/key.pem are a pair (unsupported key type DSA)

Warnings never stop start-up. They are: a shadowed route, a table that
uses the old route format, and a pair that could not be verified.

Build
-----

    $ rebar3 compile

The relx release config was removed pending the packaging phase, so
`rebar3 release` is not available for now.

Run
---

There is no release for now (see Build), so run blizanci with `erl`
against the compiled code, using the OTP that built it:

    $ erl -noshell -pa _build/default/lib/*/ebin \
          -config config/sys.config \
          -eval '{ok, _} = application:ensure_all_started(blizanci)'

It runs in the foreground. A bad configuration makes it exit at once
with status 1 and a one-line error for each problem. To stop it, send
the VM a SIGTERM (`kill`, or `systemctl stop` if it runs under
systemd, with `exec erl ...` in the launch script so that the VM is
the service's main process); it shuts down cleanly in about a second.

`./manage.sh start-daemon` and `stop-daemon` need a release, so for
now they only print an error; `./manage.sh repl` starts a rebar3 shell.

Trivia
------

"blizanci" / "близанци" is Serbo-Croatian for "twins" (i.e., Gemini).

Documentation
-------------

A work in progress at [https://mk270.github.io/blizanci/](https://mk270.github.io/blizanci/).

Status
------

The source code of Blizanci is largely "finished". There are no plans to add new features.

But software is more than just programs. There is already some documentation of blizanci's internal APIs - eventually a manual will be produced and some installation advice and packaging / tooling.

## Licence

Copyright (C) 2020-2026 Martin Keegan. Licensed under the Apache
Licence, Version 2.0; see the file LICENCE.
