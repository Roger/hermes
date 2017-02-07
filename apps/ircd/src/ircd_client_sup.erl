%%%-------------------------------------------------------------------
%% @doc ircd client supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ircd_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_socket/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(PORT, 4242).
-define(TCP_OPTIONS, [binary, {packet, line},
                      {reuseaddr, true}, {keepalive, true},
                      {backlog, 30}, {active, once}]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    {ok, ListenSocket} = gen_tcp:listen(?PORT, ?TCP_OPTIONS),
    spawn_link(fun empty_listeners/0),
    {ok, {{simple_one_for_one, 60, 3600},
           [{socket_sup,
            {ircd_client_fsm, start_link, [ListenSocket]}, % pass the socket to the childs
             temporary, 1000, worker, []}
            ]}}.

%%====================================================================
%% Internal functions
%%====================================================================

%% starts a listening socket
start_socket() ->
    supervisor:start_child(?MODULE, []).

%% starts 20 listening sockets
empty_listeners() ->
    [start_socket() || _ <- lists:seq(1, 20)],
    ok.
