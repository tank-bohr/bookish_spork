-module(bookish_spork).

-export([
    start_server/0,
    start_server/1,
    stop_server/1,
    stub_request/1,
    stub_request/2,
    stub_request/3,
    stub_request/4,
    capture_request/0
]).

-define(DEFAUT_PORT, 5432).
-define(RECEIVE_REQUEST_TIMEOUT_MILLIS, 1000).

-spec start_server() -> bookish_spork_server:server().
start_server() ->
    start_server(?DEFAUT_PORT).

-spec start_server(Port :: non_neg_integer()) -> bookish_spork_server:server().
start_server(Port) ->
    bookish_spork_server:start(Port).

-spec stop_server(Server :: bookish_spork_server:server()) -> ok.
stop_server(Server) ->
    bookish_spork_server:stop(Server).

-spec stub_request(Server :: bookish_spork_server:server()) -> Acceptor :: pid().
stub_request(Server) ->
    bookish_spork_server:listen(Server, bookish_spork_response:new(), self()).

-spec stub_request(
    Server :: bookish_spork_server:server(),
    Status :: non_neg_integer()
) -> Acceptor :: pid().
stub_request(Server, Status) ->
    bookish_spork_server:listen(Server, bookish_spork_response:new(Status), self()).

-spec stub_request(
    Server :: bookish_spork_server:server(),
    Status :: non_neg_integer(),
    ContentOrHeaders :: binary() | map()
) -> Acceptor :: pid().
stub_request(Server, Status, ContentOrHeaders) ->
    bookish_spork_server:listen(Server,
        bookish_spork_response:new(Status, ContentOrHeaders), self()).

-spec stub_request(
    Server  :: bookish_spork_server:server(),
    Status  :: non_neg_integer(),
    Headers :: map(),
    Content :: binary()
) -> Acceptor :: pid().
stub_request(Server, Status, Headers, Content) ->
    bookish_spork_server:listen(Server,
        bookish_spork_response:new(Status, Headers, Content), self()).

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
