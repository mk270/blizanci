%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_servlet).
-include("blizanci_types.hrl").

-callback request(binary(), map(), server_config()) ->
    {'immediate', gemini_response()} |
    'defer'.

-callback serve(binary(), map(), server_config()) -> gateway_result().

-callback cancel(pid()) -> 'ok'.
