-module(bookish_spork_request_test).

-include_lib("eunit/include/eunit.hrl").

elixir_interface_test_() ->
    Request = bookish_spork_request:new(),
    [?_assertEqual(Request, bookish_spork_request:'__struct__'()),
    ?_assertEqual(Request, bookish_spork_request:'__struct__'([])),
    ?_assertEqual(Request, bookish_spork_request:'__struct__'(#{}))].

new_test_() ->
    Method = 'POST',
    Uri = "/foo/bar",
    Version = {1, 1},
    Body = <<"Hello">>,
    Request = lists:foldl(fun(F, Acc) -> F(Acc) end, bookish_spork_request:new(), [
        fun(Req) -> bookish_spork_request:request_line(Req, Method, Uri, Version) end,
        fun(Req) -> bookish_spork_request:add_header(Req, "X-Foo", "Bar") end,
        fun(Req) -> bookish_spork_request:body(Req, Body) end
    ]),
    Map = #{
        method => Method,
        uri => list_to_binary(Uri),
        version => Version,
        headers => #{"x-foo" => "Bar"},
        body => Body
    },
    List = [
        {method, Method},
        {uri, list_to_binary(Uri)},
        {version, Version},
        {headers, #{"x-foo" => "Bar"}},
        {body, Body}
    ],
    [?_assertEqual(Request, bookish_spork_request:new(Map)),
    ?_assertEqual(Request, bookish_spork_request:new(List))].

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

transport_attribute_accessors_test_() ->
    Attrs = [socket, ssl_info, tls_ext, connection_id, transport],
    Dummy = [{A, A} || A <- Attrs],
    Request = bookish_spork_request:new(Dummy),
    [?_assertEqual(A, apply(bookish_spork_request, A, [Request])) || A <- Attrs].
