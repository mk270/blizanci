%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

%% @doc
%% Runtime log-level control for blizanci.
%%
%% The OTP logger has two filtering layers: a primary (global) level that
%% gates all log events before they reach any handler, and a per-handler
%% level that controls what each handler actually emits.  `level/1' sets
%% both together so the observable behaviour is straightforward.
%%
%% The default configuration keeps the primary level at `debug', which
%% means `module_level/2' can enable debug output for individual modules
%% at runtime without restarting the node.
%% @end

-module(blizanci_log).

-export([level/0, level/1, module_level/2, reset_module_level/1]).

-type log_level() :: debug | info | notice | warning | error | critical | alert | emergency.

%% @doc Return the current primary log level.
%% @end
-spec level() -> Result
              when Result :: log_level().
level() ->
    #{level := Level} = logger:get_primary_config(),
    Level.

%% @doc
%% Set the global log level.
%%
%% Both the primary filter and the default handler are updated, so the
%% change takes effect immediately for all modules.  Valid levels in
%% increasing severity: `debug', `info', `notice', `warning', `error',
%% `critical', `alert', `emergency'.
%% @end
-spec level(Level) -> Result
              when Level  :: log_level(),
                   Result :: ok.
level(Level) ->
    ok = logger:set_primary_config(level, Level),
    ok = logger:set_handler_config(default, level, Level).

%% @doc
%% Override the log level for a specific module.
%%
%% Module-level overrides take precedence over the primary level, allowing
%% temporary debug logging for one module without increasing global
%% verbosity.  Clear the override with `reset_module_level/1'.
%% @end
-spec module_level(Module, Level) -> Result
              when Module :: module(),
                   Level  :: log_level(),
                   Result :: ok.
module_level(Module, Level) ->
    ok = logger:set_module_level(Module, Level).

%% @doc Clear the per-module log level override set by `module_level/2'.
%% @end
-spec reset_module_level(Module) -> Result
              when Module :: module(),
                   Result :: ok.
reset_module_level(Module) ->
    ok = logger:unset_module_level(Module).
