%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_log).

-export([level/0, level/1, module_level/2, reset_module_level/1]).

-type log_level() :: debug | info | notice | warning | error | critical | alert | emergency.

-spec level() -> Result
              when Result :: log_level().
level() ->
    #{level := Level} = logger:get_primary_config(),
    Level.

-spec level(Level) -> Result
              when Level  :: log_level(),
                   Result :: ok.
level(Level) ->
    ok = logger:set_primary_config(level, Level),
    ok = logger:set_handler_config(default, level, Level).

-spec module_level(Module, Level) -> Result
              when Module :: module(),
                   Level  :: log_level(),
                   Result :: ok.
module_level(Module, Level) ->
    ok = logger:set_module_level(Module, Level).

-spec reset_module_level(Module) -> Result
              when Module :: module(),
                   Result :: ok.
reset_module_level(Module) ->
    ok = logger:unset_module_level(Module).
