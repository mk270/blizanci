%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

%% A logger handler for tests: it sends each event it is given to the
%% process named in the handler's config, as {logged, Level, Text}.
%% Add it with logger:add_handler(Id, blizanci_test_log_handler,
%% #{config => #{notify => self()}}).
-module(blizanci_test_log_handler).
-export([log/2]).

log(#{level := Level, msg := Msg}, #{config := #{notify := Pid}}) ->
    Text = case Msg of
               {string, S} -> S;
               {Format, Args} when is_list(Format) ->
                   io_lib:format(Format, Args);
               _ -> ""
           end,
    Pid ! {logged, Level, lists:flatten(Text)},
    ok.
