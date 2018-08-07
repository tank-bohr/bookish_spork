-module(bookish_spork_response_test).
-include_lib("eunit/include/eunit.hrl").

-define(NOW, {{2018, 4, 28}, {5, 51, 50}}).

status_line_test() ->
    ?assertEqual(<<"HTTP/1.1 204 No Content">>, bookish_spork_response:status_line(204)).

response_headers_test() ->
    Actual = bookish_spork_response:headers(#{}, <<"XXX">>, ?NOW),
    Expected = <<
        "Content-Length: 3\r\n",
        "Date: Sat, 28 Apr 2018 05:51:50 GMT\r\n",
        "Server: BookishSpork/0.0.1\r\n"
    >>,
    ?assertEqual(Expected, Actual).

write_str_test_() ->
    DefaultResponse = bookish_spork_response:new(),
    CustomsStatus = bookish_spork_response:new(502),
    CustomsStatusAndContent = bookish_spork_response:new(409, <<"Booookish">>),
    CustomsStatusAndHeaders = bookish_spork_response:new(307,
        #{<<"Location">> => <<"https://example.com">>}),
    CustomResponse = bookish_spork_response:new(200,
        #{<<"Content-Type">> => <<"text/plain">>}, <<"Hello">>),
    [
        ?_assertEqual(<<
            "HTTP/1.1 204 No Content\r\n",
            "Content-Length: 0\r\n",
            "Date: Sat, 28 Apr 2018 05:51:50 GMT\r\n",
            "Server: BookishSpork/0.0.1\r\n",
            "\r\n"
        >>, bookish_spork_response:write_str(DefaultResponse, ?NOW)),
        ?_assertEqual(<<
            "HTTP/1.1 502 Bad Gateway\r\n",
            "Content-Length: 0\r\n",
            "Date: Sat, 28 Apr 2018 05:51:50 GMT\r\n",
            "Server: BookishSpork/0.0.1\r\n"
            "\r\n"
        >>, bookish_spork_response:write_str(CustomsStatus, ?NOW)),
        ?_assertEqual(<<
            "HTTP/1.1 409 Conflict\r\n",
            "Content-Length: 9\r\n",
            "Date: Sat, 28 Apr 2018 05:51:50 GMT\r\n",
            "Server: BookishSpork/0.0.1\r\n"
            "\r\n",
            "Booookish"
        >>, bookish_spork_response:write_str(CustomsStatusAndContent, ?NOW)),
        ?_assertEqual(<<
            "HTTP/1.1 307 Temporary Redirect\r\n",
            "Content-Length: 0\r\n",
            "Date: Sat, 28 Apr 2018 05:51:50 GMT\r\n",
            "Location: https://example.com\r\n"
            "Server: BookishSpork/0.0.1\r\n"
            "\r\n"
        >>, bookish_spork_response:write_str(CustomsStatusAndHeaders, ?NOW)),
        ?_assertEqual(<<
            "HTTP/1.1 200 OK\r\n",
            "Content-Length: 5\r\n",
            "Content-Type: text/plain\r\n",
            "Date: Sat, 28 Apr 2018 05:51:50 GMT\r\n",
            "Server: BookishSpork/0.0.1\r\n"
            "\r\n",
            "Hello"
        >>, bookish_spork_response:write_str(CustomResponse, ?NOW))
    ].
