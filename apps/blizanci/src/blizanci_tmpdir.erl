%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_tmpdir).

-export([tmp_file_name/0, stale/2]).

% TBD
tmp_file_name() ->
    Hash = erlang:phash2(make_ref()),
    integer_to_list(Hash).

%TBD
stale(Dir, MaxAge) ->
    FullDir = Dir,
    {ok, Files} = file:list_dir_all(FullDir),
    FullFilenames = [ filename:join(FullDir, F) || F <- Files ],
    [ F || F <- FullFilenames, older_than(F, MaxAge) ].

older_than(Filename, MaxAge) ->
    {ok, Stat} = file:read_file_info(Filename),
    Stamp = erlang:element(5, Stat),

    Now = calendar:now_to_local_time(erlang:timestamp()),
    {Days, Time} = calendar:time_difference(Stamp, Now),
    Secs = calendar:time_to_seconds(Time),

    case {Days >= 0, Secs >= MaxAge} of
        {true, false} -> false;
        {false, _} -> true; % just write it off - filestamp in future?
        {true, true} -> true
    end.
