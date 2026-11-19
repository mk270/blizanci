%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_log_tests).

-include_lib("eunit/include/eunit.hrl").

-import(blizanci_log, [level/0, level/1, module_level/2,
                       reset_module_level/1]).

%% These tests change the logger's global levels, so each runs in a
%% process of its own and puts the primary and default-handler levels
%% back as it found them, whether or not it passes.
with_restored_levels(Fun) ->
    blizanci_test_support:isolated(
      fun() ->
          #{level := Primary} = logger:get_primary_config(),
          {ok, #{level := Handler}} = logger:get_handler_config(default),
          try Fun()
          after
              ok = logger:set_primary_config(level, Primary),
              ok = logger:set_handler_config(default, level, Handler)
          end
      end).

handler_level() ->
    {ok, #{level := Level}} = logger:get_handler_config(default),
    Level.

levels() ->
    [debug, info, notice, warning, error, critical, alert, emergency].

%% Setting a level sets the primary and the handler level together, and
%% level/0 reads the primary back.
level_round_trip_test() ->
    with_restored_levels(
      fun() ->
          [ begin
                ?assertEqual(ok, level(L)),
                ?assertEqual(L, level()),
                ?assertEqual(L, handler_level()),
                #{level := P} = logger:get_primary_config(),
                ?assertEqual(L, P)
            end || L <- levels() ]
      end).

%% Whatever the tests above (and these) did, the levels are as they were.
levels_restored_test() ->
    Before = {level(), handler_level()},
    Inside = with_restored_levels(
               fun() ->
                   ok = level(emergency),
                   {level(), handler_level()}
               end),
    ?assertEqual({emergency, emergency}, Inside),
    ?assertEqual(Before, {level(), handler_level()}),
    % ... and also when the body fails
    ?assertError({isolated_failed, _},
                 with_restored_levels(
                   fun() ->
                       ok = level(alert),
                       erlang:error({isolated_failed, x})
                   end)),
    ?assertEqual(Before, {level(), handler_level()}).

%% Only the primary and default-handler levels are touched: the other
%% handlers (if any) keep theirs.
other_handlers_untouched_test() ->
    with_restored_levels(
      fun() ->
          Others = [ {Id, L} || #{id := Id, level := L}
                                    <- logger:get_handler_config(),
                                Id =/= default ],
          ok = level(emergency),
          ?assertEqual(Others,
                       [ {Id, L} || #{id := Id, level := L}
                                        <- logger:get_handler_config(),
                                    Id =/= default ])
      end).

%% A level that is not a level is refused, and nothing changes.
bad_level_test() ->
    with_restored_levels(
      fun() ->
          Before = {level(), handler_level()},
          ?assertError(_, apply(blizanci_log, level, [loud])),
          ?assertError(_, apply(blizanci_log, level, ["debug"])),
          ?assertEqual(Before, {level(), handler_level()})
      end).

%% A module's override is set, read back and cleared; it does not
%% touch the global levels. (Uses a module name no code is running.)
module_level_test() ->
    with_restored_levels(
      fun() ->
          M = blizanci_log_test_no_such_module,
          Before = {level(), handler_level()},
          try
              ?assertEqual(ok, module_level(M, debug)),
              ?assertEqual(debug,
                           proplists:get_value(M, logger:get_module_level())),
              ?assertEqual(Before, {level(), handler_level()}),
              ?assertEqual(ok, module_level(M, error)),
              ?assertEqual(error,
                           proplists:get_value(M, logger:get_module_level()))
          after
              ok = reset_module_level(M)
          end,
          ?assertEqual(undefined,
                       proplists:get_value(M, logger:get_module_level())),
          % clearing one that is not set is fine
          ?assertEqual(ok, reset_module_level(M))
      end).
