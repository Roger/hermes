-module(ircd_cmd).
-export([ping/1, pong/1, nick/3, nick_used/2, priv/4, login/1, join/2, join_control/1, motd/1, topic/3]).
-export([send_num/3, send_num/4, send_msg/2, send_msg/3]).

ping(State) ->
    send_msg("PING", State).

pong(State) ->
    send_msg("PONG", State).

nick(OldNick, Nick, State) ->
    send_msg(":~s NICK :~s", [OldNick, Nick], State).

nick_used(Nick, State) ->
    send_num("433", "* ~s :Nickname is already in use.", [Nick], State).

priv(Channel, Nick, Msg, State) ->
    send_msg(":~s PRIVMSG ~s :~s", [Nick, Channel, Msg], State).

login(State=#{nick:=Nick, host:=Host}) ->
    Umodes = <<"abisw">>,
    UmodesPriv = <<"Ro">>,
    Cmodes = <<"ntC">>,
    Ctypes = <<"&#">>,
    MaxNick = 24,

    send_num("001", "~s :Welcome to the internet ~s", [Nick, Nick], State),
    send_num("002", "~s :Host ~s is using hermes", [Nick, Host], State),
    send_num("003", "~s :Ircd info hermes", [Nick], State),
    send_num("004", "~s :hermes 0.1  ~s ~s ~s",[Nick, Umodes, UmodesPriv, Cmodes], State),
    send_num("005",
           "~s PREFIX=(ohv)@%+ CHANTYPES=~s CHANMODES=,,,~s NICKLEN=~B "
           "CHANNELLEN=~B NETWORK=hermes SAFELIST CASEMAPPING=rfc1459 "
           "MAXTARGETS=1 WATCH=128 FLOOD=0/9999 :are supported by this server",
           [Nick, Ctypes, Cmodes, MaxNick, MaxNick], State),
    join_control(State),
    motd(State).

join_control(State=#{nick := Nick, host := Host}) ->
    send_msg(":~s JOIN :&hermes", [Nick], State),
    send_msg(":~s MODE &hermes +Ct", [Host], State),
    send_num("353", "~s = &hermes :@~s @root", [Nick, Nick], State),
    send_num("366", "~s &hermes :End of /NAMES list", [Nick], State).

join(Channel, State=#{nick := Nick, host := Host}) ->
    send_msg(":~s JOIN :~s", [Nick, Channel], State),
    send_msg(":~s MODE ~s +Ct", [Host, Channel], State),
    send_num("353", "~s = ~s :@~s @root", [Nick, Channel, Nick], State),
    send_num("366", "~s ~s :End of /NAMES list", [Nick, Channel], State).

motd(State=#{nick := Nick}) ->
    send_num("375", "~s :- long life and prosperity -", [Nick], State),
    send_num("376", "~s :End of /MOTD command.", [Nick], State).

topic(Channel, Topic, State=#{nick := Nick}) ->
    send_num("332", "~s ~s :~s", [Nick, Channel, Topic], State).

send_num(Code, Msg, Args, State) ->
    NewMsg = io_lib:format(Msg, Args),
    send_num(Code, NewMsg, State).

send_num(Code, Msg, _State=#{socket:=Socket, host:=H}) ->
    gen_tcp:send(Socket, [":", H, " ", Code, " ", Msg, "\r\n"]).

send_msg(Msg, Args, State) ->
    NewMsg = io_lib:format(Msg, Args),
    send_msg(NewMsg, State).

send_msg(Msg, _State=#{socket := Socket}) ->
    gen_tcp:send(Socket, [Msg, "\r\n"]).
