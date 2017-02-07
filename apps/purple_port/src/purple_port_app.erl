-module(purple_port_app).
-behaviour(application).
-export([start/0, start/2, stop/0, stop/1]).

start() ->
    application:ensure_all_started(purple_port).

stop() ->
    application:stop(purple_port).

start(_StartType, _StartArgs) ->
    purple_port_sup:start_link().

stop(_State) ->
    ok.
