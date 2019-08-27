-module(bookish_spork_acceptor).

-export([start_link/3]).

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-type transport() :: gen_tcp | bookish_spork_ssl.
-type socket() :: gen_tcp:socket() | ssl:sslsocket().

-record(state, {
    server :: pid(),
    transport :: transport(),
    listen_socket :: socket(),
    connection_id :: undefined | binary(),
    socket :: undefined | socket(),
    tls_ext :: undefined | ssl:protocol_extensions()
}).

-type state() :: #state{}.

-spec start_link(Server, Transport, ListenSocket) -> {ok, pid()} when
    Server :: pid(),
    Transport :: transport(),
    ListenSocket :: socket().
start_link(Server, Transport, ListenSocket) ->
    gen_server:start_link(?MODULE, {Server, Transport, ListenSocket}, []).

-spec init({Server, Transport, ListenSocket}) -> {ok, State} when
    State :: state(),
    Server :: pid(),
    Transport :: transport(),
    ListenSocket :: socket().
%% @private
init({Server, Transport, ListenSocket}) ->
    ok = accept(),
    {ok, #state{server = Server, transport = Transport, listen_socket = ListenSocket}}.

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @private
handle_cast(accept, State) ->
    {ok, NewState} = accept(State),
    {noreply, NewState};
handle_cast(handle_connection, State) ->
    ok = handle_connection(State),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec accept() -> ok.
%% @private
accept() ->
    gen_server:cast(self(), accept).

-spec accept(State) -> {ok, NewState} when
    State :: state(),
    NewState :: state().
%% @private
accept(#state{transport = Transport, listen_socket = ListenSocket} = State) ->
    {Socket, TlsExt} = case Transport:accept(ListenSocket) of
        {ok, Sock, Ext} ->
            {Sock, Ext};
        {ok, Sock} ->
            {Sock, undefined}
    end,
    ConnectionId = generate_id(),
    NewState = State#state{connection_id = ConnectionId, socket = Socket, tls_ext = TlsExt},
    ok = handle_connection(),
    {ok, NewState}.

-spec handle_connection() -> ok.
%% @private
handle_connection() ->
    gen_server:cast(self(), handle_connection).

-spec handle_connection(State :: state()) -> ok.
%% @private
handle_connection(#state{transport = Transport, socket = Socket, server = Server} = State) ->
    case receive_request(State) of
        {ok, Request} ->
            ok = bookish_spork_server:store_request(Server, Request),
            case bookish_spork_server:response(Server) of
                {ok, Response} ->
                    ok = reply(Transport, Socket, Response, Request),
                    ok = complete_connection(State, Request);
                {error, no_response} ->
                    ok = Transport:close(Socket),
                    ok = accept()

            end;
        socket_closed ->
            accept()
    end.

-spec complete_connection(State, Request) -> ok when
    State :: state(),
    Request :: bookish_spork_request:t().
%% @private
complete_connection(#state{transport = Transport, socket = Socket}, Request) ->
    case bookish_spork_request:is_keepalive(Request) of
        true ->
            ok = handle_connection();
        false ->
            ok = Transport:shutdown(Socket, read_write),
            ok = accept()
    end.

-spec receive_request(State :: state()) -> Result when
    Result :: {ok, Request} | socket_closed,
    Request :: bookish_spork_request:t().
%% @private
receive_request(State) ->
    #state{
        transport = Transport,
        connection_id = ConnectionId,
        socket = Socket,
        tls_ext = TlsExt
    } = State,
    Request = bookish_spork_request:new(ConnectionId, Socket, TlsExt),
    read_from_socket(Transport, Socket, Request).

-spec read_from_socket(Transport, Socket, RequestIn) -> Result when
    Transport :: transport(),
    Socket :: socket(),
    RequestIn :: bookish_spork_request:t(),
    Result :: {ok, RequestOut} | socket_closed,
    RequestOut :: bookish_spork_request:t().
%% @private
read_from_socket(Transport, Socket, RequestIn) ->
    case Transport:recv(Socket, 0) of
        {ok, {http_request, Method, {abs_path, Uri}, Version}} ->
            RequestOut = bookish_spork_request:request_line(RequestIn, Method, Uri, Version),
            read_from_socket(Transport, Socket, RequestOut);
        {ok, {http_header, _, Header, _, Value}} ->
            RequestOut = bookish_spork_request:add_header(RequestIn, Header, Value),
            read_from_socket(Transport, Socket, RequestOut);
        {ok, http_eoh} ->
            Body = read_body(Transport, Socket, bookish_spork_request:content_length(RequestIn)),
            RequestOut = bookish_spork_request:body(RequestIn, Body),
            {ok, RequestOut};
        {ok, {http_error, HttpError}} ->
            erlang:error({http_error, HttpError}, [Transport, Socket, RequestIn]);
        {error, closed} ->
            socket_closed
    end.

-spec read_body(Transport, Socket, ContentLength) -> Body when
    Transport :: transport(),
    Socket :: socket(),
    ContentLength :: non_neg_integer(),
    Body :: binary().
%% @private
read_body(_Transport, _Socket, 0) ->
    <<>>;
read_body(Transport, Socket, ContentLength) ->
    inet:setopts(Socket, [{packet, raw}]),
    {ok, Body} = Transport:recv(Socket, ContentLength),
    inet:setopts(Socket, [{packet, http}]),
    Body.

-spec reply(Transport, Socket, Response, Request) -> ok when
    Transport :: transport(),
    Socket :: socket(),
    Response :: bookish_spork:stub_request_fun() | bookish_spork_response:t(),
    Request :: bookish_spork_request:t().
%% @private
reply(Transport, Socket, ResponseFun, Request) when is_function(ResponseFun) ->
    Response = ResponseFun(Request),
    reply(Transport, Socket, bookish_spork_response:new(Response), Request);
reply(Transport, Socket, Response, _Request) ->
    String = bookish_spork_response:write_str(Response, calendar:universal_time()),
    Transport:send(Socket, [String]).

-spec generate_id() -> Id :: binary().
%% @doc generates unique id to be a connection id
generate_id() ->
    Bytes = crypto:strong_rand_bytes(7),
    Base64 = base64:encode(Bytes),
    string:trim(Base64, trailing, "=").
