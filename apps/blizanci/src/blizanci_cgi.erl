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
    lager:info("Should have run: ~p ~p", [Path, Docroot]),
    Cmd = ["/usr/bin/env"],
    {ok, Pid, OsPid} = exec:run(Cmd, [monitor,
                                      stdout,
                                      stderr]),
    {init_cgi, Pid, OsPid}.

