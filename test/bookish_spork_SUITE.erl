-module(bookish_spork_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(T, bookish_spork).

-export([
    all/0
]).
-export([
    base_integration_test/1,
    customized_response_test/1
]).

all() ->
    [base_integration_test, customized_response_test].

base_integration_test(_Config) ->
    {ok, _Pid} = ?T:start_server(),
    {ok, _Acceptor} = ?T:stub_request(),

    RequestHeaders = [],
    {ok, {{"HTTP/1.1", 204, "No Content"}, _ResponseHeaders, _Body}} =
      httpc:request(get, {"http://localhost:5432/o/lo/lo?q=kjk", RequestHeaders}, [], []),

    {ok, Request} = ?T:capture_request(),
    ?assertEqual('GET', bookish_spork_request:method(Request)),
    ?assertEqual("/o/lo/lo?q=kjk", bookish_spork_request:uri(Request)),
    ?assertEqual({1, 1}, bookish_spork_request:version(Request)),
    ?assertMatch(#{"host" := "localhost:5432"}, bookish_spork_request:headers(Request)),
    ok = ?T:stop_server().



customized_response_test(_Config) ->
    {ok, _Pid} = ?T:start_server(9871),
    ?T:stub_request(200,
        #{<<"X-Custom-Response-Header">> => <<"test">>},
        <<"Hello, Test">>),

    RequestBody = <<"{\"name\": \"John Doe\", \"email\": \"john@doe.com\"}">>,
    {ok, {{"HTTP/1.1", 200, "OK"}, ResponseHeaders, Body}} = httpc:request(post, {
        "http://localhost:9871/api/v1/users",
        [{"Accept", "text/plain"}],
        "application/json",
        RequestBody
    }, [], [{body_format, binary}]),
    ?assertEqual({"x-custom-response-header", "test"},
        proplists:lookup("x-custom-response-header", ResponseHeaders)),
    ?assertEqual(<<"Hello, Test">>, string:chomp(Body)),

    {ok, Request} = ?T:capture_request(),
    ?assertEqual('POST', bookish_spork_request:method(Request)),
    ?assertEqual("/api/v1/users", bookish_spork_request:uri(Request)),
    ?assertEqual({1, 1}, bookish_spork_request:version(Request)),
    ?assertEqual(RequestBody, bookish_spork_request:body(Request)),
    ?assertMatch(#{"accept" := "text/plain"}, bookish_spork_request:headers(Request)),

    ok = ?T:stop_server().
