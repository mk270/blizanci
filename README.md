blizanci
========

Blizanci is a [Gemini protocol](https://gemini.circumlunar.space/) server.
It is designed primarily for robustness and security.

Features:

* [TLS client certificate](https://en.wikipedia.org/wiki/Public_key_certificate#TLS/SSL_client_certificate) support
* single-user [CGI](https://en.wikipedia.org/wiki/Common_Gateway_Interface)
* distributed PKI: authorisation by subject or *issuer* of certificates
* Titan protocol uploads
* servlet interface for in-process dynamic page generation

Manual
------

The reference for configuration, routing, authorisation, servlets, CGI,
Titan and the rest is the manual, in `doc/manual/`. It is written in
Gemtext (`index.gmi` is the first chapter); Markdown copies are generated
from it by `doc/manual/build-md.sh`. The script downloads a pinned,
checksummed release of the `gemtext2md` escript from
github.com/mk270/erl_gemtext2md on first use (it needs curl and
Erlang/OTP 27 or later on the PATH); set `GEMTEXT2MD` to use a converter
of your own instead. Run that script before `rebar3 ex_doc`, which
includes the Markdown copies in the generated documentation. ex_doc
does not work on Erlang/OTP 28.0 (it fails on a regular expression
function); 28.4.1 is known to work. Building and running blizanci
itself needs only OTP 27.

Prerequisites
-------------

You'll need Erlang/OTP 27 or later, and Rebar. rebar3 refuses to build on an older OTP. (Check yours with `erl -noshell -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt().'`; a packaged erlang may be older than 27.) The build and the run must use the same OTP. On a Debian-derived system like Ubuntu, this means installing the rebar3 executable from the rebar3 website, and installing the deb package erlang if it is recent enough:

* [Rebar3 installation instructions](https://www.rebar3.org/docs/getting-started#installing-binary)
* `apt-get install erlang`

Config
------

Currently, a config file is required. Create a `sys.config` file, for
example in the `config/` directory, in the format suggested in
`config/sys.config.example`. The keys, sites, routes and realms are
described in the manual (`doc/manual/configuration.gmi`).

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

Trivia
------

"blizanci" / "близанци" is Serbo-Croatian for "twins" (i.e., Gemini).

Documentation
-------------

A work in progress at [https://mk270.github.io/blizanci/](https://mk270.github.io/blizanci/).

Status
------

The source code of Blizanci is largely "finished". There are no plans to add new features.

## Licence

Copyright (C) 2020-2026 Martin Keegan. Licensed under the Apache
Licence, Version 2.0; see the file LICENCE.
