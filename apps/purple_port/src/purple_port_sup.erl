-module(purple_port_sup).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3, start_link/0]).

-export([login/3, sendmsg/2]).

-record(state, {port}).

-define(APPNAME, purple).
-define(BINNAME, purple_port).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


terminate(_Reason, #state{port=Port}) ->
    catch port_close(Port),
    ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

login(User, Password, Protocol) ->
    gen_server:call(?MODULE, {login, User, Password, Protocol}).

sendmsg(To, Msg) ->
    gen_server:call(?MODULE, {sendmsg, To, Msg}).

handle_call({login, User, Password, Protocol}, _From, #state{port=Port}=State) ->
    port_command(Port, term_to_binary({login, {42, User, Password, Protocol}})),
    {reply, ok, State};

handle_call({sendmsg, _To, _Msg}=Msg, _From, #state{port=Port}=State) ->
    port_command(Port, term_to_binary(Msg)),
    {noreply, State}.

handle_cast(Msg, _) ->
    exit({unknown_cast, Msg}).

init([]) ->
    BinName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?BINNAME]);
                _ ->
                    filename:join([priv, ?BINNAME])
            end;
        Dir ->
            filename:join(Dir, ?BINNAME)
    end,
    Port = open_port({spawn, BinName}, [{packet, 2}, binary]),
    {ok, #state{port=Port}}.

handle_info({Port, {data, Data}}, #state{port=Port}=State) ->
    io:format("From Purple: ~p~n", [binary_to_term(Data)]),
    {noreply, State};
handle_info({Port, {exit_status, _Status}}, #state{port=Port}=State) ->
    {stop, port_died, State};
handle_info(Msg, _) -> exit({unknown_info, Msg}).
