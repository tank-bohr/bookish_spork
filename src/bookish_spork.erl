-module(bookish_spork).

-export([
    start_server/0,
    start_server/1,
    stop_server/0,
    stub_request/0,
    stub_request/1,
    stub_request/2,
    stub_request/3,
    capture_request/0
]).

-define(DEFAUT_PORT, 5432).
-define(RECEIVE_REQUEST_TIMEOUT_MILLIS, 1000).

start_server() ->
    start_server(?DEFAUT_PORT).

start_server(Port) ->
    bookish_spork_server:start(Port).

stop_server() ->
    bookish_spork_server:stop().

stub_request() ->
    bookish_spork_server:respond_with(bookish_spork_response:new()).

stub_request(Status) ->
    bookish_spork_server:respond_with(bookish_spork_response:new(Status)).

stub_request(Status, ContentOrHeaders) ->
    bookish_spork_server:respond_with(bookish_spork_response:new(Status, ContentOrHeaders)).

stub_request(Status, Headers, Content) ->
    bookish_spork_server:respond_with(bookish_spork_response:new(Status, Headers, Content)).

-spec capture_request() -> bookish_spork_request:bookish_spork_request().
capture_request() ->
    receive
        {bookish_spork, Request} ->
            {ok, Request};
        Unexpected ->
            {unexpected, Unexpected}
        after ?RECEIVE_REQUEST_TIMEOUT_MILLIS ->
            timeout
    end.
