%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_path).

-export([fix_path/1, path_under_root/2]).

-spec fix_path(list()) -> {ok, string()} | {error, atom()}.
fix_path(S) ->
    {ok, S2} = realpath:normalise(S),
    {ok, S3} = realpath:canonicalise(S2),
    {ok, S3}.

-spec path_under_root(string(), string()) -> true | false.
path_under_root(S, Root) ->
    L = string:len(Root),
    Sl = string:slice(S, 0, L),
    Sl =:= Root.

