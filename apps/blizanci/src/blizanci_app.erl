%%%-------------------------------------------------------------------
%% @doc blizanci public API
%% @end
%%%-------------------------------------------------------------------

-module(blizanci_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Mimes = blizanci_mimetypes:types(),
    ok = application:ensure_started(ranch),

    {ok, Cert} = application:get_env(certfile),
    {ok, Key} = application:get_env(keyfile),
    SSL_Opts =
        [{port, 1965},
         {certfile, Cert},
         {keyfile, Key}
        ],
    {ok, _} = ranch:start_listener(blizanci_service_clear,
                                   ranch_ssl, SSL_Opts,
                                   blizanci_proto, []
                                  ),
    blizanci_sup:start_link().

stop(_State) ->
    ok.

%% internal functions