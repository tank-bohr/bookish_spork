-module(bookish_spork_request_test).

-include_lib("eunit/include/eunit.hrl").

-define(T, bookish_spork_request).

content_length_test() ->
    Request = ?T:new(),
    ?assertEqual(0, ?T:content_length(Request)),
    RequestWithContentLength = ?T:add_header(Request, "Content-Length", "17"),
    ?assertEqual(17, ?T:content_length(RequestWithContentLength)).

add_header_test() ->
    Request = ?T:add_header(?T:new(), "X-Lol", "kjk"),
    ?assertEqual(#{"x-lol" => "kjk"}, ?T:headers(Request), "Converts header name to lower case").
