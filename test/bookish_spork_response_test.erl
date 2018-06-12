-module(bookish_spork_response_test).
-include_lib("eunit/include/eunit.hrl").

-define(T, bookish_spork_response).
-define(NOW, {{2018, 4, 28}, {5, 51, 50}}).

status_line_test() ->
    ?assertEqual(<<"HTTP/1.1 204 No Content">>, ?T:status_line(204)).

response_headers_test() ->
    Actual = ?T:headers(#{}, <<"XXX">>, ?NOW),
    Expected = <<
        "Content-Length: 3\r\n",
        "Date: Sat, 28 Apr 2018 05:51:50 GMT\r\n",
        "Server: BookishSpork/0.0.1\r\n"
    >>,
    ?assertEqual(Expected, Actual).

write_str_test() ->
    Response = ?T:new(200, #{
        <<"Content-Type">> => <<"text/plain">>,
        <<"Date">> => <<"Tue, 12 Jun 2018 14:12:37 GMT">>
    }, <<"Hello">>),
    Expected = <<
        "HTTP/1.1 200 OK\r\n",
        "Content-Length: 5\r\n",
        "Content-Type: text/plain\r\n",
        "Date: Tue, 12 Jun 2018 14:12:37 GMT\r\n",
        "Server: BookishSpork/0.0.1\r\n"
        "\r\n",
        "Hello"
    >>,
    Actual = ?T:write_str(Response),
    ?assertEqual(Expected, Actual).
