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

Build
-----

    $ rebar3 release

Run
---

    $ ./manage.sh start-daemon

Trivia
------

"blizanci" / "близанци" is Serbo-Croatian for "twins" (i.e., Gemini).

Documentation
-------------

A work in progress at [https://mk270.github.io/blizanci/](https://mk270.github.io/blizanci/).
