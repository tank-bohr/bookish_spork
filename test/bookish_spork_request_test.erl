-module(bookish_spork_request_test).

-include_lib("eunit/include/eunit.hrl").

content_length_test_() ->
    Request = bookish_spork_request:new(),
    RequestWithContentLength = bookish_spork_request:add_header(Request, "Content-Length", "17"),
    [?_assertEqual(0, bookish_spork_request:content_length(Request)),
    ?_assertEqual(17, bookish_spork_request:content_length(RequestWithContentLength))].

add_header_test() ->
    Request = bookish_spork_request:add_header(bookish_spork_request:new(), "X-Lol", "kjk"),
    ?assertEqual(#{"x-lol" => "kjk"}, bookish_spork_request:headers(Request),
        "Converts header name to lower case").


is_keepalive_test_() ->
    Http10Req = bookish_spork_request:request_line(
        bookish_spork_request:new(), 'GET', "/", {1, 0}),
    Http10KeepAliveReq = bookish_spork_request:add_header(
        bookish_spork_request:request_line(
            bookish_spork_request:new(),'GET', "/", {1, 0}), "Connection", "Keep-Alive"),
    ConnectionCloseReq = bookish_spork_request:add_header(
        bookish_spork_request:request_line(
            bookish_spork_request:new(),'GET', "/", {1, 1}), "Connection", "close"),
    ReqularRequest = bookish_spork_request:new(),
    [?_assertEqual(false, bookish_spork_request:is_keepalive(Http10Req)),
    ?_assertEqual(true, bookish_spork_request:is_keepalive(Http10KeepAliveReq)),
    ?_assertEqual(false, bookish_spork_request:is_keepalive(ConnectionCloseReq)),
    ?_assertEqual(true, bookish_spork_request:is_keepalive(ReqularRequest))].
