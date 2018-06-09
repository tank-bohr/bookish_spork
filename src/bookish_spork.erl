-module(bookish_spork).

-export([
    start_server/0,
    start_server/1,
    stop_server/1
]).

-export([
    tag/1,
    status/1,
    header/2,
    content/1
]).

-define(DEFAUT_PORT, 5432).

-type settings() :: #{
    port    => non_neg_integer(),
    capture => pid()
}.

-spec start_server() -> bookish_spork_server:server().
start_server() ->
    start_server(#{}).

-spec start_server(Settings :: settings()) -> bookish_spork_server:server().
start_server(Settings) ->
    Port = maps:get(port, Settings, ?DEFAUT_PORT),
    RequestCapture = maps:get(capture, Settings, self()),
    bookish_spork_server:start(Port, RequestCapture).

-spec stop_server(Server :: bookish_spork_server:server()) -> ok.
stop_server(Server) ->
    bookish_spork_server:stop(Server).

-spec tag(Tag :: atom()) -> true.
tag(Tag) ->
    bookish_spork_settings:tag(Tag).

-spec status(Status :: non_neg_integer()) -> true.
status(Status) ->
    bookish_spork_settings:status(Status).

-spec header(Name :: binary(), Value :: binary()) -> true.
header(Name, Value) ->
    bookish_spork_settings:header(Name, Value).

-spec content(Content :: binary()) -> true.
content(Content) ->
    bookish_spork_settings:content(Content).
