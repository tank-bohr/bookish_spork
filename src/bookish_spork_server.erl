-module(bookish_spork_server).

-export([
    start/2,
    stop/1,
    status/1
]).

-record(server, {
    socket :: gen_tcp:socket(),
    pid    :: pid()
}).
-opaque server() :: #server{}.

-export_type([server/0]).

-spec start(Port :: integer(), Receiver :: pid()) -> Server :: server().
start(Port, Receiver) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [
        binary,
        {packet, http},
        {active, false},
        {reuseaddr, true}
    ]),
    Pid = spawn(fun() ->
        accept_connection_loop(ListenSocket, Receiver)
    end),
    #server{socket = ListenSocket, pid = Pid}.

-spec stop(Server :: server()) -> ok.
stop(#server{socket = Socket}) ->
    gen_tcp:close(Socket).

-spec status(Server :: server()) -> atom().
status(#server{pid = Pid}) ->
    case process_info(Pid) of
        undefined ->
            dead;
        ProcessInfo ->
            proplists:get_value(status, ProcessInfo)
    end.

accept_connection_loop(ListenSocket, Receiver) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            Request = accept(Socket),
            Receiver ! {bookish_spork, Request},
            accept_connection_loop(ListenSocket, Receiver);
        {error, closed} ->
            Receiver ! {bookish_spork, socket_closed}
    end.

accept(Socket) ->
    Request = receive_request(Socket),
    ok = reply(Socket),
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

reply(Socket) ->
    Response = bookish_spork_response:all(),
    gen_tcp:send(Socket, [Response]).
