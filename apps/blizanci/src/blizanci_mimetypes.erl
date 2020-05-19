
-module(blizanci_mimetypes).

%% API
-export([types/0]).

types() ->
    [{"gmi", "text/gemini"},
     {"gemini", "text/gemini"}].

