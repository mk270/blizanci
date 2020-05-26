%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_cgi).

-export([serve/2]).

-define(TIMEOUT_MS, 5000).
-define(SERVER, ?MODULE).

serve(Path, Docroot) ->
    Bin = filename:join([Docroot, "..", "cgi-bin", Path]),
    Cmd = [binary_to_list(Bin)],
    Env = [{"GATEWAY", "CGI"}],
    lager:info("trying to execute ~p with ~p", [Bin, Env]),
    {ok, Pid, OsPid} = exec:run(Cmd, [monitor,
                                      {env, Env},
                                      stdout,
                                      stderr]),
    {init_cgi, Pid, OsPid}.

