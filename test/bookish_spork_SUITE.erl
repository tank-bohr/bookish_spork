-module(bookish_spork_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    all/0
]).
-export([
    base_integration_test/1,
    customized_response_test/1,
    failed_capture_test/1,
    stub_multiple_requests_test/1
]).

all() ->
    [base_integration_test, customized_response_test,
    failed_capture_test, stub_multiple_requests_test].

base_integration_test(_Config) ->
    {ok, _Pid} = bookish_spork:start_server(),
    {ok, _Acceptor} = bookish_spork:stub_request(),
    RequestHeaders = [],
    {ok, {{"HTTP/1.1", 204, "No Content"}, _ResponseHeaders, _Body}} =
      httpc:request(get, {"http://localhost:32002/o/lo/lo?q=kjk", RequestHeaders}, [], []),
    {ok, Request} = bookish_spork:capture_request(),
    ?assertEqual('GET', bookish_spork_request:method(Request)),
    ?assertEqual("/o/lo/lo?q=kjk", bookish_spork_request:uri(Request)),
    ?assertEqual({1, 1}, bookish_spork_request:version(Request)),
    ?assertMatch(#{"host" := "localhost:32002"}, bookish_spork_request:headers(Request)),
    ok = bookish_spork:stop_server().

customized_response_test(_Config) ->
    {ok, _Pid} = bookish_spork:start_server(9871),
    bookish_spork:stub_request(200,
        #{<<"X-Custom-Response-Header">> => <<"test">>},
        <<"Hello, Test">>),
    RequestBody = <<"{\"name\": \"John Doe\", \"email\": \"john@doe.com\"}">>,
    {ok, {{"HTTP/1.1", 200, "OK"}, ResponseHeaders, Body}} = httpc:request(post, {
        "http://localhost:9871/api/v1/users",
        [{"Accept", "text/plain"}],
        "application/json",
        RequestBody
    }, [], [{body_format, binary}]),
    ?assertEqual("test", proplists:get_value("x-custom-response-header", ResponseHeaders)),
    ?assertEqual(<<"Hello, Test">>, string:chomp(Body)),
    {ok, Request} = bookish_spork:capture_request(),
    ?assertEqual('POST', bookish_spork_request:method(Request)),
    ?assertEqual("/api/v1/users", bookish_spork_request:uri(Request)),
    ?assertEqual({1, 1}, bookish_spork_request:version(Request)),
    ?assertEqual(RequestBody, bookish_spork_request:body(Request)),
    ?assertMatch(#{"accept" := "text/plain"}, bookish_spork_request:headers(Request)),
    ok = bookish_spork:stop_server().

failed_capture_test(_Config) ->
    {ok, _Pid} = bookish_spork:start_server(),
    ?assertEqual(timeout, bookish_spork:capture_request(), "Got timeout when there is no stub"),
    ok = bookish_spork:stop_server().

stub_multiple_requests_test(_Config) ->
    {ok, _Pid} = bookish_spork:start_server(),
    bookish_spork:stub_request(200, #{<<"X-Number">> => <<"The First">>}),
    bookish_spork:stub_request(200, <<"The second">>),
    bookish_spork:stub_request(202),
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers1, Body1}} = httpc:request(get,
        {"http://localhost:32002/first/request", []}, [], [{body_format, binary}]),
    {ok, {{"HTTP/1.1", 200, "OK"}, _Headers, Body2}} = httpc:request(get,
        {"http://localhost:32002/second/request", []}, [], [{body_format, binary}]),
    {ok, {{"HTTP/1.1", 202, "Accepted"}, _, _}} = httpc:request(get,
        {"http://localhost:32002/third/request", []}, [], [{body_format, binary}]),
    ?assertEqual("The First", proplists:get_value("x-number", Headers1)),
    ?assertEqual(<<>>, Body1),
    ?assertEqual(<<"The second">>, Body2),
    {ok, Request1} = bookish_spork:capture_request(),
    {ok, Request2} = bookish_spork:capture_request(),
    {ok, Request3} = bookish_spork:capture_request(),
    ?assertEqual("/first/request", bookish_spork_request:uri(Request1)),
    ?assertEqual("/second/request", bookish_spork_request:uri(Request2)),
    ?assertEqual("/third/request", bookish_spork_request:uri(Request3)),
    ok = bookish_spork:stop_server().
