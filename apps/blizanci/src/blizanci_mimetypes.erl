%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_mimetypes).

%% API
-export([types/0]).

types() ->
    [{"gmi", "text/gemini"},
     {"gemini", "text/gemini"}].
