# Blizanci Changelog

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

- Use callback functions for inter-process communication instead of out-of-band messages

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
