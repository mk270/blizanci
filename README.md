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

You'll need Erlang and Rebar. On a Debian-derived system like Ubuntu, this means installing the rebar3 executable from the rebar3 website, and installing the deb package erlang:

* [Rebar3 installation instructions](https://www.rebar3.org/docs/getting-started#installing-binary)
* `apt-get install erlang`

Config
------

Currently, a config file is required. In due course, this will become optional.

Create an appropriate `sys.config` and `vm.args` file in the `config/`
directory in the format suggested in the examples in that directory.

You'll also need to put TLS certificates in `ssl/certificate.pem` and
`ssl/key.pem`; these locations can be reconfigured, but blizanci will
fail on startup with a `badmatch` / `couldnt_open_pem_file` message if
the certificates cannot be read.

Routing
-------

Each site has one routing table: an ordered list of routes. A request
is matched against the routes for its protocol (gemini or titan) in
order, and the first route that matches serves it. A route serves
exactly one protocol, so a static site that also takes Titan uploads
needs a gemini route and a titan route. If no route matches, the
client gets "not found". The table is set by the `routing` key in
`sys.config`; without one, a default table is used (see below).

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
`.` or `..` segment and no control character.

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

The single-site key `ca_certs` is shorthand for a realm named
`<<"default">>`. Setting `ca_certs` and also defining a realm called
`<<"default">>` in `realms` is a start-up error.

A realm may have an empty file list. A private route on it then
authorises nobody: a client sending no certificate gets status 60
(certificate required), and one sending a valid certificate gets 61
(certificate unauthorised). No warning is logged.

### The default routing table

If there is no `routing` key, the table is built from the keys
`docroot` (default `./public_gemini`), `cgiroot` (default `./cgi-bin`)
and `titan_work_dir`:

    {mount, gemini, "/cgi-bin", blizanci_cgi,    public,
     #{cgiroot => Cgiroot}},
    {mount, gemini, "/",        blizanci_static, public,
     #{docroot => Docroot}}

and, only if `titan_work_dir` is set:

    {mount, titan,  "/",        blizanci_titan,
     {private, {realm, <<"default">>}},
     #{docroot => Docroot, work_dir => TitanWorkDir}}

So Titan uploads are off unless `titan_work_dir` is set, and then they
are accepted only from clients whose certificates are signed by the
CAs in `ca_certs` (realm `default`). With `titan_work_dir` set but
`ca_certs` unset, the `default` realm is empty and nobody can upload.
The default table has no `/private` or `/restricted` mounts: write your
own `routing` key if you want them (next section).

### Titan work directories

Titan stages uploads in a work directory before installing them in the
docroot. A Titan route's `work_dir` has no default and must be given.
Start-up fails unless it is an absolute path, exists, is a directory
and is on the same filesystem as the route's docroot (uploads are
installed by hard link and rename). Neither the docroot nor the work
directory of a Titan route may overlap a CGI root.

### Example: an explicit routing table

This table reproduces the protected areas that the default table had
before version 6: `/private` for clients with certificates from the
`default` realm (`ca_certs`), and `/restricted` for any client with a
certificate. Each mount names its own subdirectory as its root, and
the more specific mounts come before `/`:

    {routing, [
      {mount, gemini, "/cgi-bin", blizanci_cgi, public,
       #{cgiroot => "./cgi-bin"}},
      {mount, gemini, "/private", blizanci_static,
       {private, {realm, <<"default">>}},
       #{docroot => "./public_gemini/private"}},
      {mount, gemini, "/restricted", blizanci_static, restricted,
       #{docroot => "./public_gemini/restricted"}},
      {mount, gemini, "/", blizanci_static, public,
       #{docroot => "./public_gemini"}},
      {mount, titan, "/", blizanci_titan,
       {private, {realm, <<"default">>}},
       #{docroot => "./public_gemini", work_dir => "/srv/titan-work"}}
    ]}

The realm `default` exists here because `ca_certs` is set; with an
explicit `routing` key, `titan_work_dir` is not used (name the work
directory in the Titan route, as above). Relative paths are allowed in
`docroot` and `cgiroot`. Without the `/private` and `/restricted`
routes, the `/` route serves those directories publicly.

Several sites
-------------

By default blizanci serves one site, set by the single-site keys
(`hostname`, `certfile`, `keyfile`, `docroot`, `cgiroot`, `ca_certs`,
`titan_work_dir` and `routing`). To serve several hostnames from one
port, each with its own certificate and routes, use the `sites` key
instead:

    {blizanci, [
      {port, 1965},
      {sites, [
        {"a.example", [{certfile, "/srv/a/cert.pem"},
                       {keyfile, "/srv/a/key.pem"},
                       {routing, [...]}]},
        {"b.example", [{certfile, "/srv/b/cert.pem"},
                       {keyfile, "/srv/b/key.pem"},
                       {routing, [...]}]}
      ]}
    ]}

The rules:

* A site is `{Hostname, Options}`, and the hostname is a string. A site
  takes `certfile`, `keyfile` and `routing`, and nothing else; all
  three are required. There is no default routing table and there are
  no shorthand keys: `docroot`, `cgiroot`, `titan_work_dir` and
  `ca_certs` are not accepted in a site. Every site spells out its own
  routes, and each route names its own root in its options (see
  Routing, above).
* `port` and `servlets` stay global, as does `realms`. A route in a
  site names realms defined in the global `realms` key. Since
  `ca_certs` cannot be used with `sites`, there is no realm `default`
  unless you define one in `realms`.
* `sites` cannot be combined with any single-site key: `hostname`,
  `certfile`, `keyfile`, `docroot`, `cgiroot`, `ca_certs`,
  `titan_work_dir` or `routing`. If any of them is also set, start-up
  is refused with one line listing the ones found; blizanci does not
  guess which form was meant.
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
    blizanci: the sites key cannot be combined with the single-site
    keys hostname, certfile; use one or the other

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

    $ ./manage.sh start-daemon

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
