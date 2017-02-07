%%%-------------------------------------------------------------------
%% @doc ircd top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ircd_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

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
    {ok, {{one_for_one, 30, 60},
        [{client_sup,   % client sockets sup
          {ircd_client_sup, start_link, []},
          permanent,
          infinity,
          supervisor,
          []},
         {main_server,
          {ircd_server, start_link, []},
          permanent,
          infinity,
          worker,
          []},
         {channel_sup, % channels instances sup
          {ircd_channel_sup, start_link, []},
          permanent,
          infinity,
          supervisor,
          []}
        ]}}.
