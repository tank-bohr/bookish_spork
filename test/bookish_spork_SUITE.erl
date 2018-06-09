-module(bookish_spork_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

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
    Server = ?T:start_server(),

    RequestHeaders = [],
    {ok, {{"HTTP/1.1", 204, "No Content"}, _ResponseHeaders, _Body}} =
      httpc:request(get, {"http://localhost:5432/o/lo/lo?q=kjk", RequestHeaders}, [], []),

    {ok, Request} = receive
        {bookish_spork, Req} ->
            {ok, Req}
        after 2000 ->
            timeout
    end,
    ?assertEqual('GET', bookish_spork_request:method(Request)),
    ?assertEqual("/o/lo/lo?q=kjk", bookish_spork_request:uri(Request)),
    ?assertEqual({1, 1}, bookish_spork_request:version(Request)),
    ?assertMatch(#{"host" := "localhost:5432"}, bookish_spork_request:headers(Request)),

    ?assertEqual(waiting, bookish_spork_server:status(Server)),
    ok = ?T:stop_server(Server),
    socket_closed = receive
        {bookish_spork, Event} ->
            Event
        after 2000 ->
            timeout
    end,
    ?assertEqual(dead, bookish_spork_server:status(Server)).




customized_response_test(_Config) ->
    Server = ?T:start_server(#{port => 9871}),

    ?T:tag(custom_tag),
    ?T:status(200),
    ?T:header(<<"X-Custom-Response-Header">>, <<"test">>),
    ?T:content(<<"Hello, Test">>),

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

    {ok, Request} = receive
        {custom_tag, Req} ->
            {ok, Req}
        after 2000 ->
            timeout
    end,
    ?assertEqual('POST', bookish_spork_request:method(Request)),
    ?assertEqual("/api/v1/users", bookish_spork_request:uri(Request)),
    ?assertEqual({1, 1}, bookish_spork_request:version(Request)),
    ?assertEqual(RequestBody, bookish_spork_request:body(Request)),
    ?assertMatch(#{"accept" := "text/plain"}, bookish_spork_request:headers(Request)),

    ok = ?T:stop_server(Server).

