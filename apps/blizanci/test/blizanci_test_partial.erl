%% A module that is almost a servlet, for tests of servlet validation:
%% it has the options callbacks and request/4, but no serve/4.

-module(blizanci_test_partial).

-export([request/4, default_options/0, validate_options/1]).

default_options() -> #{}.

validate_options(_Opts) -> ok.

request(_Matches, _Req, _Config, _Opts) -> defer.
