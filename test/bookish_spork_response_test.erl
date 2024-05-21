-module(bookish_spork_response_test).
-include_lib("eunit/include/eunit.hrl").

-define(NOW, {{2018, 4, 28}, {5, 51, 50}}).

new_test_() ->
    Status = 200,
    Headers = #{<<"X-Test">> => <<"pants">>},
    Content = <<"Hello">>,
    Expected = bookish_spork_response:new(Status, Headers, Content),
    [?_assertEqual(Expected, bookish_spork_response:new(Expected)),
    ?_assertEqual(Expected, bookish_spork_response:new({Status, Headers, Content})),
    ?_assertEqual(Expected, bookish_spork_response:new([Status, Headers, Content])),
    ?_assertEqual(Expected, bookish_spork_response:new([Status, [{<<"X-Test">>, <<"pants">>}], Content]))].

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

write_str_test() ->
    Response = bookish_spork_response:new(200,
        #{<<"Content-Type">> => <<"text/plain">>}, <<"Hello">>),
    ?assertEqual(<<
            "HTTP/1.1 200 OK\r\n",
            "Content-Length: 5\r\n",
            "Content-Type: text/plain\r\n",
            "Date: Sat, 28 Apr 2018 05:51:50 GMT\r\n",
            "Server: BookishSpork/0.0.1\r\n"
            "\r\n",
            "Hello"
    >>, bookish_spork_response:write_str(Response, ?NOW)).

iodata_test() ->
    % From OTP 27's json:encode(#{ message => <<"Hello, world!">> }).
    Body = ["{",[[34,<<"message">>,34],58,34,<<"Hello, world!">>,34],[],"}"],
    Response = bookish_spork_response:new(200,
        #{<<"Content-Type">> => <<"application/json">>}, Body),
    ?assertEqual(<<
            "HTTP/1.1 200 OK\r\n",
            "Content-Length: 27\r\n",
            "Content-Type: application/json\r\n",
            "Date: Sat, 28 Apr 2018 05:51:50 GMT\r\n",
            "Server: BookishSpork/0.0.1\r\n"
            "\r\n",
            "{\"message\":\"Hello, world!\"}"
    >>, bookish_spork_response:write_str(Response, ?NOW)).
