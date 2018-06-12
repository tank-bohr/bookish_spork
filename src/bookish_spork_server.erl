-module(bookish_spork_server).

-export([
    start/1,
    stop/1,
    listen/3
]).

-record(server, {
    socket :: gen_tcp:socket()
}).
-opaque server() :: #server{}.

-export_type([server/0]).

-spec start(Port :: integer()) -> Server :: server().
start(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [
        binary,
        {packet, http},
        {active, false},
        {reuseaddr, true}
    ]),
    #server{socket = ListenSocket}.

-spec stop(Server :: server()) -> ok.
stop(#server{socket = Socket}) ->
    gen_tcp:close(Socket).

-spec listen(
    Server   :: server(),
    Response :: bookish_spork_response:response(),
    Receiver :: pid()
) -> Acceptor :: pid().
listen(#server{socket = ListenSocket}, Response, Receiver) ->
    spawn_link(fun() ->
        accept_connection_loop(ListenSocket, Response, Receiver)
    end).

accept_connection_loop(ListenSocket, Response, Receiver) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            Request = accept(Socket, Response),
            Receiver ! {bookish_spork, Request},
            accept_connection_loop(ListenSocket, Response, Receiver);
        {error, closed} ->
            Receiver ! {bookish_spork, socket_closed}
    end.

accept(Socket, Response) ->
    Request = receive_request(Socket),
    ok = reply(Socket, Response),
    ok = gen_tcp:shutdown(Socket, write),
    Request.

receive_request(Socket) ->
    receive_request(Socket, bookish_spork_request:new()).

receive_request(Socket, RequestIn) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, {http_request, Method, {abs_path, Uri}, Version}} ->
            RequestOut = bookish_spork_request:request_line(RequestIn, Method, Uri, Version),
            receive_request(Socket, RequestOut);
        {ok, {http_header, _, Header, _, Value}} ->
            RequestOut = bookish_spork_request:add_header(RequestIn, Header, Value),
            receive_request(Socket, RequestOut);
        {ok, http_eoh} ->
            Body = read_body(Socket, bookish_spork_request:content_length(RequestIn)),
            bookish_spork_request:body(RequestIn, Body)
    end.

read_body(_Socket, 0) ->
    <<>>;
read_body(Socket, ContentLength) ->
    inet:setopts(Socket, [{packet, raw}]),
    {ok, Body} = gen_tcp:recv(Socket, ContentLength),
    inet:setopts(Socket, [{packet, http}]),
    Body.

reply(Socket, Response) ->
    String = bookish_spork_response:write_str(Response),
    gen_tcp:send(Socket, [String]).
