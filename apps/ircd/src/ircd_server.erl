-module(ircd_server).

-behaviour(gen_server).

%% API functions
-export([start_link/0, join/1, sendmsg/3, login/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

login(Nick) ->
    gen_server:call(?MODULE, {login, Nick}).

join(Channel) ->
    gen_server:call(?MODULE, {join, Channel}).

sendmsg(Channel, Nick, Msg) ->
    gen_server:call(?MODULE, {sendmsg, Channel, Nick, Msg}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #{channels => [], users => []}}.

handle_call({login, Nick}, _From, State=#{users:=Users}) ->
    case lists:member(Nick, Users) of
        true -> {reply, exists, State};
        false ->
            {reply, ok, State#{users := [Nick|Users]}}
    end;

handle_call({join, Channel}, {From, _Ref}, State=#{channels:=Channels}) ->
    {RState, ChanPid} = case lists:keyfind(Channel, 1, Channels) of
         false -> % it's a new channel
             {ok, Pid} = ircd_channel_sup:start_channel(Channel),
             NewState = State#{channels := [{Channel, Pid}|Channels]},
             {NewState, Pid};
         {_C, Pid} ->
             {State, Pid}
    end,
    gen_server:call(ChanPid, {join, From}),
    {reply, ok, RState};

handle_call({sendmsg, Channel, Nick, Msg}, {From, _Ref}, State=#{channels:=Channels}) ->
    {ok, Pid} = case lists:keyfind(Channel, 1, Channels) of
         {_C, P} ->
             {ok, P};
         false ->
             {error, "Channel not found"}
    end,
    gen_server:call(Pid, {privmsg, From, Nick, Msg}),
    {reply, ok, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
