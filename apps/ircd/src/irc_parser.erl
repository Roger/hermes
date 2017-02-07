-module(irc_parser).
-export([parse/1]).

parse(Cmd) ->
    parser(tokenize(Cmd)).

parser([<<"PASS">>, Pass]) ->
    {pass, Pass};

parser([<<"USER">>, User, Host, Server, RealName]) ->
    {user, User, Host, Server, RealName};

parser([<<"NICK">>, Nick]) ->
    {nick, Nick};

parser([<<"QUIT">>, Msg]) ->
    {quit, Msg};

parser([<<"PING">>, Host]) ->
    {ping, Host};

parser([<<"PONG">>, Host]) ->
    {pong, Host};

parser([<<"JOIN">>, Channels]) ->
    {join, binary:split(Channels, <<" ">>, [global])};
parser([<<"JOIN">>, Channels, Keys]) ->
    {join, binary:split(Channels, <<" ">>),
           binary:split(Keys, <<" ">>, [global])
    };

parser([<<"NAMES">>]) ->
    {names};

parser([<<"NAMES">>, Chan|_]) ->
    {names, binary:split(Chan, <<" ">>, [global])};

parser([<<"PART">>, Channel]) ->
    {part, Channel};

parser([<<"WHOIS">>, Who]) ->
    {whois, Who};

parser([<<"WHOWAS">>, WhoWas]) ->
    {whowas, WhoWas};

parser([<<"MOTD">>]) ->
    {motd};

parser([<<"MODE">>, User, Mode]) ->
    {mode, User, Mode};

parser([<<"MODE">>, Channel]) ->
    {mode, Channel};

parser([<<"WHO">>]) ->
    {whois};

parser([<<"PRIVMSG">>, Channel, Msg]) ->
    {privmsg, Channel, Msg};

parser([<<"NOTICE">>, Target, Msg]) ->
    {notice, Target, Msg};

parser([<<"AWAY">>, Msg]) ->
    {away, Msg};

parser([<<"AWAY">>]) ->
    {away};

parser([<<"CAP">>, Type]) ->
    {cap, Type};

parser(Cmd) ->
    {unknown, Cmd}.

tokenize(Cmd) ->
    case binary:split(Cmd, <<" :">>) of
        [Cmd2, Rest] ->
            binary:split(Cmd2, <<" ">>, [global]) ++ [Rest];
        [Cmd2] ->
            binary:split(Cmd2, <<" ">>, [global])
    end.
