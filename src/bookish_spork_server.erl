-module(bookish_spork_server).

-export([
    start/1,
    stop/0,
    respond_with/1
]).

-behaviour(gen_server).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {
    socket :: gen_tcp:socket()
}).

start(Port) ->
    gen_server:start({local, ?SERVER}, ?MODULE, Port, []).

stop() ->
    gen_server:stop(?SERVER).

respond_with(Response) ->
    gen_server:call(?SERVER, {respond_with, Response}).

init(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [
        binary,
        {packet, http},
        {active, false},
        {reuseaddr, true}
    ]),
    {ok, #state{socket = ListenSocket}}.

handle_call({respond_with, Response}, {Receiver, _Ref}, #state{socket = ListenSocket} = State) ->
    {reply, accept(ListenSocket, Response, Receiver), State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{socket = ListenSocket}) ->
    gen_tcp:close(ListenSocket).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

accept(ListenSocket, Response, Receiver) ->
    Pid = spawn_link(fun() ->
        {ok, Socket} = gen_tcp:accept(ListenSocket),
        Request = receive_request(Socket),
        Receiver ! {bookish_spork, Request},
        ok = reply(Socket, Response),
        ok = gen_tcp:shutdown(Socket, read_write)
    end),
    {ok, Pid}.

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
