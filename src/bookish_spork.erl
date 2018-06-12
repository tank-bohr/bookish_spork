-module(bookish_spork).

-export([
    start_server/0,
    start_server/1,
    stop_server/1,
    capture_request/4,
    receive_request/0
]).

-export([
    status/1,
    header/2,
    content/1
]).

-define(DEFAUT_PORT, 5432).
-define(RECEIVE_REQUEST_TIMEOUT_MILLIS, 1000).

-type settings() :: #{
    port     => non_neg_integer(),
    receiver => pid()
}.

-spec start_server() -> bookish_spork_server:server().
start_server() ->
    start_server(#{}).

-spec start_server(Settings :: settings()) -> bookish_spork_server:server().
start_server(Settings) ->
    Port = maps:get(port, Settings, ?DEFAUT_PORT),
    Receiver = maps:get(receiver, Settings, self()),
    bookish_spork_server:start(Port, Receiver).

-spec stop_server(Server :: bookish_spork_server:server()) -> ok.
stop_server(Server) ->
    bookish_spork_server:stop(Server).

-spec capture_request() -> bookish_spork_request:bookish_spork_request().
capture_request() ->
    receive_request().

-spec receive_request() -> bookish_spork_request:bookish_spork_request().
receive_request() ->
    receive
        {bookish_spork, Request} ->
            {ok, Request};
        Unexpected ->
            {unexpected, Unexpected}
        after ?RECEIVE_REQUEST_TIMEOUT_MILLIS ->
            timeout
    end.

-spec status(Status :: non_neg_integer()) -> true.
status(Status) ->
    bookish_spork_settings:status(Status).

-spec headers(Header :: map()) -> true.
status(Status) ->
    bookish_spork_settings:headers(Headers).

-spec header(Name :: binary(), Value :: binary()) -> true.
header(Name, Value) ->
    bookish_spork_settings:header(Name, Value).

-spec content(Content :: binary()) -> true.
content(Content) ->
    bookish_spork_settings:content(Content).
