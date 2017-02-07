%%%-------------------------------------------------------------------
%% @doc ircd channel supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ircd_channel_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_channel/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    {ok, {{simple_one_for_one, 60, 3600},
           [{channel_sup,
            {ircd_channel, start_link, []},
             temporary, 1000, worker, []}
            ]}}.

%%====================================================================
%% Internal functions
%%====================================================================

start_channel(Channel) ->
    supervisor:start_child(?MODULE, [Channel]).
