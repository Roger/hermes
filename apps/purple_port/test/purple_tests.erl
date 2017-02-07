-module(purple_tests).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    ?assertException(error, badarg, purple:purple(foo)).
