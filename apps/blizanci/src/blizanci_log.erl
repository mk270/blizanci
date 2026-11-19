%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_log).

-moduledoc """
Runtime log-level control for blizanci.

See the manual chapter [Logging](logging.md).
""".


-export([level/0, level/1, module_level/2, reset_module_level/1]).

-type log_level() :: debug
                   | info
                   | notice
                   | warning
                   | error
                   | critical
                   | alert
                   | emergency.

-doc """
Return the current primary log level.
""".
-spec level() -> Result
              when Result :: log_level().
level() ->
    #{level := Level} = logger:get_primary_config(),
    Level.

-doc """
Set the global log level.

Both the primary filter and the default handler are updated, so the
change takes effect immediately for all modules.  Valid levels in
increasing severity: `debug`, `info`, `notice`, `warning`, `error`,
`critical`, `alert`, `emergency`.
""".
-spec level(Level) -> Result
              when Level  :: log_level(),
                   Result :: ok.
level(Level) ->
    ok = logger:set_primary_config(level, Level),
    ok = logger:set_handler_config(default, level, Level).

-doc """
Override the log level for a specific module.

Module-level overrides take precedence over the primary level, allowing
temporary debug logging for one module without increasing global
verbosity.  Clear the override with `reset_module_level/1`.
""".
-spec module_level(Module, Level) -> Result
              when Module :: module(),
                   Level  :: log_level(),
                   Result :: ok.
module_level(Module, Level) ->
    ok = logger:set_module_level(Module, Level).

-doc """
Clear the per-module log level override set by `module_level/2`.
""".
-spec reset_module_level(Module) -> Result
              when Module :: module(),
                   Result :: ok.
reset_module_level(Module) ->
    ok = logger:unset_module_level(Module).
