-module(bookish_spork_server).

-export([
    start/1,
    stop/1
]).

start(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [
        binary,
        {packet, http},
        {active, false}
    ]),
    accept_connection_loop(ListenSocket),
    ListenSocket.

accept_connection_loop(ListenSocket) ->
    accept_connection_loop(ListenSocket, self()).

accept_connection_loop(ListenSocket, Parent) ->
    spawn_link(fun () ->
        accept_connection(ListenSocket, Parent),
        accept_connection_loop(ListenSocket, Parent)
    end).

stop(ListenSocket) ->
    gen_tcp:close(ListenSocket).

accept_connection(ListenSocket, Parent) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    Request = receive_request(Socket),
    ok = reply(Socket),
    ok = gen_tcp:close(Socket),
    Parent ! {?MODULE, Request}.

receive_request(Socket) ->
    receive_request(Socket, #{}).

receive_request(Socket, Headers) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, {http_request, _HttpMethod, _HttpUri, _HttpVersion}} ->
            receive_request(Socket, Headers);
        {ok, {http_header, _, Header, _, Value}} ->
            receive_request(Socket, maps:put(Header, Value, Headers));
        {ok, http_eoh} ->
            Body = read_body(Socket, Headers),
            {Headers, Body}
        end.

read_body(Socket, #{'Content-Length' := ContentLength}) ->
    Length = list_to_integer(ContentLength),
    inet:setopts(Socket, [{packet, raw}]),
    {ok, Body} = gen_tcp:recv(Socket, Length),
    inet:setopts(Socket, [{packet, http}]),
    Body;
read_body(_Socket, _Headers) ->
    <<>>.

reply(Socket) ->
    Response = bookish_spork_response:all(),
    gen_tcp:send(Socket, [Response]).
