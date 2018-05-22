-module(bookish_spork_format_test).
-include_lib("eunit/include/eunit.hrl").

-define(T, bookish_spork_format).
-define(NOW, {{2018, 4, 28}, {5, 51, 50}}).

rfc2616_date_test() ->
    ?assertEqual(<<"Sat, 28 Apr 2018 05:51:50 GMT">>, ?T:rfc2616_date(?NOW)).

timepad_test() ->
    ?assertEqual(<<"02">>, ?T:timepad(2)),
    ?assertEqual(<<"20">>, ?T:timepad(20)).
