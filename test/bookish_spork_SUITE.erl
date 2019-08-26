-module(bookish_spork_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).
-export([
    base_integration_test/1,
    customized_response_test/1,
    failed_capture_test/1,
    stub_multiple_requests/1,
    stub_with_fun/1,
    ssl_test/1,
    tls_ext_test/1,
    keepalive_connection/1,
    without_keepalive/1
]).

all() ->
    [base_integration_test, customized_response_test, failed_capture_test,
    stub_multiple_requests, stub_with_fun, keepalive_connection, without_keepalive,
    ssl_test, tls_ext_test].

init_per_suite(Config) ->
    ok = application:ensure_started(inets),
    ok = application:ensure_started(ssl),
    {ok, _} = application:ensure_all_started(gun),
    Config.

end_per_suite(_Config) ->
    ok.

base_integration_test(_Config) ->
    {ok, _Pid} = bookish_spork:start_server(),
    ok = bookish_spork:stub_request(),
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
    {ok, _Pid} = bookish_spork:start_server([{port, 9871}]),
    bookish_spork:stub_request([200,
        #{<<"X-Custom-Response-Header">> => <<"test">>},
        <<"Hello, Test">>]),
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
    ?assertMatch({error, _}, bookish_spork:capture_request(), "Got an error when there is no stub"),
    ok = bookish_spork:stop_server().

stub_multiple_requests(_Config) ->
    {ok, _} = bookish_spork:start_server(),
    bookish_spork:stub_request([200, #{}, <<"Multi-pulti">>], _Times = 2),
    {ok, {{"HTTP/1.1", 200, "OK"}, _, Body}} = httpc:request(get,
        {"http://localhost:32002/multi", []}, [], [{body_format, binary}]),
    {ok, Request1} = bookish_spork:capture_request(),
    ?assertEqual("/multi", bookish_spork_request:uri(Request1)),
    {ok, {{"HTTP/1.1", 200, "OK"}, _, Body}} = httpc:request(get,
        {"http://localhost:32002/pulti", []}, [], [{body_format, binary}]),
    {ok, Request2} = bookish_spork:capture_request(),
    ?assertEqual("/pulti", bookish_spork_request:uri(Request2)),
    ?assertMatch({error, _},
        httpc:request(get, {"http://localhost:32002", []}, [], [{body_format, binary}])),
    ok = bookish_spork:stop_server().

stub_with_fun(_Config) ->
    {ok, _Pid} = bookish_spork:start_server(),
    bookish_spork:stub_request(fun response/1, _Times = 2),
    {ok, {{"HTTP/1.1", 200, "OK"}, _, WalrusBody}} = httpc:request(get,
        {"http://localhost:32002/walrus", []}, [], [{body_format, binary}]),
    ?assertEqual(<<"Walrus">>, string:chomp(WalrusBody)),
    {ok, {{"HTTP/1.1", 200, "OK"}, _, LentilsBody}} = httpc:request(get,
        {"http://localhost:32002/lentils", []}, [], [{body_format, binary}]),
    ?assertEqual(<<"Unknown">>, string:chomp(LentilsBody)),
    ok = bookish_spork:stop_server().

ssl_test(_Config) ->
    {ok, _Pid} = bookish_spork:start_server([ssl]),
    ok = bookish_spork:stub_request(),
    {ok, {{"HTTP/1.1", 204, "No Content"}, _, _}} = httpc:request(get,
        {"https://localhost:32002/secure", [{"Connection", "close"}]}, [], []),
    {ok, Request} = bookish_spork:capture_request(),
    SslInfo = bookish_spork_request:ssl_info(Request),
    Ciphers = proplists:get_value(ciphers, SslInfo, []),
    ?assert(length(Ciphers) > 0),
    ok = bookish_spork:stop_server().

-ifdef(OTP_RELEASE).
tls_ext_test(_Config) ->
    {ok, _Pid} = bookish_spork:start_server([ssl]),
    ok = bookish_spork:stub_request(),
    {ok, {{"HTTP/1.1", 204, "No Content"}, _, _}} = httpc:request(get,
        {"https://localhost:32002/tls", [{"Connection", "close"}]}, [], []),
    {ok, Request} = bookish_spork:capture_request(),
    TlsExt = bookish_spork_request:tls_ext(Request),
    ?assertMatch(#{
        renegotiation_info := _,
        ec_point_formats   := _,
        elliptic_curves    := _,
        signature_algs     := _,
        alpn               := _,
        sni                := _,
        srp                := _
    }, TlsExt),
    ok = bookish_spork:stop_server().
-else.
tls_ext_test(_Config) ->
    {skip, "Nothing to test"}.
-endif.

keepalive_connection(_Config) ->
    {ok, _Pid} = bookish_spork:start_server(),
    bookish_spork:stub_request([200, #{}, <<"OK1">>]),
    bookish_spork:stub_request([200, #{}, <<"OK2">>]),
    bookish_spork:stub_request([200, #{}, <<"OK3">>]),
    {ok, ConnectionPid1} = gun:open("localhost", 32002),
    ?assertEqual(<<"OK1">>, gun_request(ConnectionPid1)),
    ?assertEqual(<<"OK2">>, gun_request(ConnectionPid1)),
    ok = gun:close(ConnectionPid1),
    {ok, ConnectionPid2} = gun:open("localhost", 32002),
    ?assertEqual(<<"OK3">>, gun_request(ConnectionPid2)),
    ok = gun:close(ConnectionPid2),
    ok = bookish_spork:stop_server(),
    ok = application:stop(gun).

without_keepalive(_Config) ->
    {ok, _Pid} = bookish_spork:start_server(),
    bookish_spork:stub_request([204, #{}, <<>>], _Times = 2),
    {ok, {{"HTTP/1.1", 204, "No Content"}, _, _}} = httpc:request(get,
        {"http://localhost:32002", [{"Connection", "close"}]}, [], [{body_format, binary}]),
    {ok, {{"HTTP/1.1", 204, "No Content"}, _, _}} = httpc:request(get,
        {"http://localhost:32002", [{"Connection", "close"}]}, [], [{body_format, binary}]),
    ok = bookish_spork:stop_server().

gun_request(ConnectionPid) ->
    StreamRef = gun:get(ConnectionPid, "/"),
    {ok, Body} = gun:await_body(ConnectionPid, StreamRef),
    Body.

response(Request) ->
    Body = case bookish_spork_request:uri(Request) of
        "/walrus" ->
            <<"Walrus">>;
        _ ->
            <<"Unknown">>
    end,
    [200, #{}, Body].
