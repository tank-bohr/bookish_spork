-module(bookish_spork_tcp).

-behaviour(bookish_spork_transport).

-export([
    listen/2,
    accept/1,
    recv/2,
    send/2,
    close/1,
    shutdown/2,
    setopts/2,
    connection_information/1
]).

listen(Port, Options) ->
    gen_tcp:listen(Port, Options).

accept(ListenSocket) ->
    gen_tcp:accept(ListenSocket).

recv(Socket, Length) ->
    gen_tcp:recv(Socket, Length).

send(Socket, Data) ->
    gen_tcp:send(Socket, Data).

close(Socket) ->
    gen_tcp:close(Socket).

shutdown(Socket, How) ->
    gen_tcp:shutdown(Socket, How).

setopts(Socket, Options) ->
    inet:setopts(Socket, Options).

connection_information(_Socket) ->
    {ok, []}.
