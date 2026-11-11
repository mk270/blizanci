%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2026  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

%% A module that is almost a servlet, for tests of servlet validation:
%% it has the options callbacks and request/4, but no serve/4.

-module(blizanci_test_partial).

-export([request/4, default_options/0, validate_options/1]).

default_options() -> #{}.

validate_options(_Opts) -> ok.

request(_Matches, _Req, _Config, _Opts) -> defer.
