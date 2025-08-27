%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_path).

-export([fix_path/1, path_under_root/2]).

%% @doc Canonicalise and normalise (q.v.) the path S
%% @todo does not support relative paths
%% @end
-spec fix_path(S) -> Result
              when S      :: list(),
                   Result :: {ok, string()} | {error, atom()}.
fix_path(S) ->
    {ok, S2} = realpath:normalise(S),
    {ok, S3} = realpath:canonicalise(S2),
    {ok, S3}.

%% @doc
%% Is the path S underneath Root? This largely boils down to whether the former
%% is an initial substring of the latter.
%%
%% It is (currently) the responsibility of the caller to canonicalise/normalise
%% the paths before calling this function.
%% @param S the path to check
%% @param Root the directory under which S is supposed to be located
%% @end
-spec path_under_root(S, Root) -> Result
              when S      :: string(),
                   Root   :: string(),
                   Result :: boolean().
path_under_root(S, Root) ->
    L = string:len(Root),
    Sl = string:slice(S, 0, L),
    Sl =:= Root.

