-module(bookish_spork_response_test).
-include_lib("eunit/include/eunit.hrl").

-define(T, bookish_spork_response).
-define(NOW, {{2018, 4, 28}, {5, 51, 50}}).

status_line_test() ->
    ?assertEqual(<<"HTTP/1.1 204 No Content">>, ?T:status_line()).

response_headers_test() ->
    Actual = ?T:headers(?NOW),
    Expected = <<
        "Date: Sat, 28 Apr 2018 05:51:50 GMT\r\n",
        "Server: BookishSpork/0.0.1\r\n"
    >>,
    ?assertEqual(Actual, Expected).
