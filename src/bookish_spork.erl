-module(bookish_spork).

-export([
    start_server/0,
    start_server/1,
    stop_server/1
]).

-define(DEFAUT_PORT, 5432).

start_server() ->
    start_server(?DEFAUT_PORT).

start_server(Port) ->
    bookish_spork_server:start(Port).

stop_server(Server) ->
    bookish_spork_server:stop(Server).
