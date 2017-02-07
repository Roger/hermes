-module(ircd_client_fsm).
-behaviour(gen_fsm).

-export([start_link/1]).
-export([init/1, handle_event/3, handle_info/3, handle_sync_event/4,
         code_change/4, terminate/3]).
-export([connecting/2, login/2, chat/2]).


start_link(Socket) ->
    gen_fsm:start_link(?MODULE, Socket, []).

init(Socket) ->
    %% Because accepting a connection is a blocking function call,
    %% we can not do it in here. Forward to the server loop!
    gen_fsm:send_event(self(), accept),
    {ok, connecting, #{
       socket => Socket,
       acc => <<>>,
       nick => no_nick,
       mode => <<"+s">>,
       host => <<"localhost">>
      }}.

%%====================================================================
%% gen_fsm callbacks
%%====================================================================

handle_sync_event(E, From, StateName, State) ->
    io:format("Unknown sync_event: ~p ~p ~p~n", [E, From, StateName]),
    {next_state, StateName, State}.

handle_event(E, StateName, State) ->
    io:format("Unknown event: ~p ~p~n", [E, StateName]),
    {next_state, StateName, State}.

connecting(accept, S=#{socket := ListenSocket}) ->
    %% this is the socket acceptance mentioned earlier
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    %% Remember that thou art dust, and to dust thou shalt return.
    %% We want to always keep a given number of children in this app.
    ircd_client_sup:start_socket(), % a new acceptor is born, praise the lord
    {next_state, login, S#{socket => AcceptSocket}}.

on_login(Nick, State=#{nick := OldNick}) ->
    case ircd_server:login(Nick) of
        ok ->
            ircd_cmd:nick(OldNick, Nick, State),
            NewState = State#{nick := Nick},
            ircd_cmd:login(NewState),
            {next_state, chat, NewState};
        exists -> 
            ircd_cmd:nick_used(Nick, State),
            {next_state, login, State}
    end.

% this can happend on login or in chat..
login({nick, Nick}, State=#{nick := OldNick}) ->
    case OldNick =/= no_nick of
        true ->
            on_login(Nick, State);
        false ->
            {next_state, login, State#{nick := Nick}}
    end;

login({pass, _Pass}, State=#{socket := _Socket}) ->
    io:format("TODO: support pass~n"),
    {next_state, login, State};

login({user, User, _H, _S, _RealName}, State) ->
    on_login(User, State);

login({cap, _Type}, State=#{socket := _Socket}) ->
    io:format("TODO: support caps~n"),
    {next_state, login, State}.

chat({nick, Nick}, State=#{nick := OldNick}) ->
    case ircd_server:login(Nick) of
        ok ->
            ircd_cmd:nick(OldNick, Nick, State),
            {next_state, chat, State#{nick := Nick}};
        exists -> 
            ircd_cmd:nick_used(Nick, State),
            {next_state, chat, State}
    end;

% yay matching Who with state and receive
chat({mode, Who, Mode}, State=#{nick := Who}) ->
    {next_state, chat, State#{mode => Mode}};

chat({ping, _Host}, State) ->
    ircd_cmd:pong(State),
    {next_state, chat, State};

chat({privmsg, <<"&hermes">>=Chan, Msg}, State) ->
    Reply = case binary:split(Msg, <<" ">>) of
        [<<"account">>, <<"list">>] ->
            [begin
                 {_, A, S, P} = R,
                 io_lib:format("~s, ~s (~s)", [P, A, S])
             end || R <- purple_port:ls_accounts()];
        [<<"help">>] ->
            [<<"No help.. you are on your own buddy">>];
        [Cmd, Args] -> 
            [<<"Unknown command..">>];
        _ ->
            [<<"Unknown command..">>]
    end,
    [ircd_cmd:priv(Chan, <<"root">>, R, State) || R <- Reply],
    {next_state, chat, State};

chat({privmsg, Channel, Msg}, State=#{nick := N}) ->
    ircd_server:sendmsg(Channel, N, Msg),
    {next_state, chat, State};

chat({join, Channels}, State=#{socket := _S, host := _H}) ->
    [begin 
         ircd_server:join(C),
         ircd_cmd:topic(C, "Topic..", State),
         ircd_cmd:join(C, State)
    end || C <- Channels],
    {next_state, chat, State};

chat({sendmsg, Channel, Nick, Msg}, State) ->
    ircd_cmd:priv(Channel, Nick, Msg, State),
    {next_state, chat, State};

chat(E, S) ->
    io:format("Unknown chat event:~p ~n", [E]),
    {next_state, chat, S}.

%%====================================================================
%% Socket Receive
%%====================================================================

handle_info({tcp_closed, _Socket}, _StateName, S) ->
    {stop, normal, S};
handle_info({tcp_error, _Socket, _}, _StateName,S) ->
    {stop, normal, S};

handle_info({tcp, _Port, Msg}, StateName, S=#{socket:=Socket, acc:=Acc}) ->
    inet:setopts(Socket, [{active, once}]),
    AccData = <<Acc/binary, Msg/binary>>,
    AccLen = byte_size(AccData) - 2,
    % if not ends with newline, means the message is not complete
    case AccData of 
        <<Cmd:AccLen/binary, "\r\n">> ->
            ?MODULE:StateName(irc_parser:parse(Cmd), S);
        _ ->
            {next_state, StateName, S#{acc := AccData}}
    end;

handle_info(E, StateName, State=#{socket := Socket}) ->
    inet:setopts(Socket, [{active, once}]),
    io:format("Unexpected info: ~p ~p~n", [E, StateName]),
    {next_state, StateName, State}.

terminate(normal, _StateName, _State) ->
    ok;
terminate(Reason, _StateName, _State) ->
    io:format("terminate reason: ~p~n", [Reason]).

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, next_state, StateName, State}.
