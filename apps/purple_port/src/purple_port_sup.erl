-module(purple_port_sup).
-behaviour(supervisor).
-export([start_link/0, init/1, start_purple/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_purple(From) ->
    supervisor:start_child(?MODULE, [From]).

init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestart = 30,
    MaxTime = 60,
    ChildSpec = {
      purple_port_sup,
      {purple_port, start_link, []}, % pass the socket to the childs
      temporary, 1000, worker, []},
    {ok, {{RestartStrategy, MaxRestart, MaxTime},
          [ChildSpec]}}.
