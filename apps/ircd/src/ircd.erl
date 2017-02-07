-module(ircd).
-export([start/0]).

start() ->
    application:start(ircd).
