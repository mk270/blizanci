%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_app).

-moduledoc false.

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    case blizanci_config:from_app_env() of
        {ok, Config} ->
            case blizanci_sup:start_link(Config) of
                {ok, Pid} ->
                    {ok, Pid};
                {error, Reason} ->
                    logger:error("blizanci: ~s",
                                 [blizanci_config:describe_error(Reason)]),
                    {error, Reason}
            end;
        {error, Errors} ->
            % every problem found, one line each, so that the operator
            % can fix them all before the next start
            [ logger:error("blizanci: ~s",
                           [blizanci_config:describe_error(E)])
              || E <- Errors ],
            {error, {invalid_configuration, Errors}}
    end.

% The declared servlets' resources are set up and torn down by
% blizanci_servlets, which tracks the declared list as it changes
% (blizanci_config:apply/1).
stop(_State) ->
    ok.
