# Blizanci - a Gemini server

Author: Martin Keegan <martin@no.ucant.org>

Copyright (C) 2020-2026, Martin Keegan

A Gemini server.

This documentation is largely for server internals.

Dynamic content is produced by servlets; the contract a servlet module
must meet is described, in three layers (module, servlet process,
gateway), in `m:blizanci_servlet`, with the container's side of the
last two in `m:blizanci_servlet_container`.
