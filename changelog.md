# Blizanci Changelog

## Unreleased

- Answer a Titan upload normally when its temporary file cannot be
  removed (for example in a read-only work directory), logging the failure
  instead of crashing; `delete/1` now returns `ok | {error, atom()}`
- Log a path refused for a hidden segment, a control character or a failed
  resolution at debug level instead of warning; only a path resolving
  outside the root, and root misconfiguration, still log a warning
- Fold the host and the scheme of a request, and site names, with an
  ASCII-only lowercase, exported as `blizanci_config:ascii_lowercase/1`
- Answer a CGI request refused before the script runs with the refusal's own
  status: 51 for a missing or hidden script, 60 for an unreadable client
  certificate
- Keep the existing resource, and remove the temporary file, when a Titan
  upload cannot be put in place (the work directory on another filesystem,
  or a directory as the target), answering 40
- Log a CGI script's stderr at debug level (long chunks truncated); the
  response to the client is unchanged
- Stop a CGI script, and its worker, when the servlet container that
  started it dies without cancelling it (the worker now monitors its
  parent); previously the script ran on to completion

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
