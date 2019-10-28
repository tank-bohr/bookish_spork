-module(bookish_spork_handler).

-export([start_link/5]).

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-type transport()     :: gen_tcp | bookish_spork_ssl.
-type socket()        :: gen_tcp:socket() | ssl:sslsocket().
-type tls_ext()       :: undefined | ssl:protocol_extensions().
-type connection_id() :: binary().

-record(state, {
    server        :: pid(),
    transport     :: transport(),
    connection_id :: connection_id(),
    socket        :: socket(),
    tls_ext       :: tls_ext()
}).

-type state() :: #state{}.

-spec start_link(Server, Transport, Socket, TlsExt, ConnectionId) -> {ok, pid()} when
    Server       :: pid(),
    Transport    :: transport(),
    Socket       :: socket(),
    TlsExt       :: tls_ext(),
    ConnectionId :: connection_id().
start_link(Server, Transport, Socket, TlsExt, ConnectionId) ->
    Args = {Server, Transport, Socket, TlsExt, ConnectionId},
    gen_server:start_link(?MODULE, Args, []).

-spec init({Server, Transport, Socket, TlsExt, ConnectionId}) -> {ok, State} when
    State        :: state(),
    Server       :: pid(),
    Transport    :: transport(),
    Socket       :: socket(),
    TlsExt       :: tls_ext(),
    ConnectionId :: connection_id().
%% @private
init({Server, Transport, Socket, TlsExt, ConnectionId}) ->
    handle_connection(),
    {ok, #state{
        server        = Server,
        transport     = Transport,
        socket        = Socket,
        tls_ext       = TlsExt,
        connection_id = ConnectionId
    }}.

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @private
handle_cast(handle_connection, State) ->
    handle_connection(State);
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

-spec handle_connection() -> ok.
%% @private
handle_connection() ->
    gen_server:cast(self(), handle_connection).

-spec handle_connection(state()) -> {noreply, state()} | {stop, normal, state()}.
%% @private
handle_connection(#state{transport = Transport, socket = Socket, server = Server} = State) ->
    case receive_request(State) of
        {ok, Request} ->
            ok = bookish_spork_server:store_request(Server, Request),
            case bookish_spork_server:response(Server) of
                {ok, Response} ->
                    reply(Transport, Socket, Response, Request),
                    complete_connection(State, Request);
                {error, no_response} ->
                    Transport:close(Socket),
                    {stop, normal, State}

            end;
        socket_closed ->
            {stop, normal, State}
    end.

-spec complete_connection(State, Request) -> {noreply, State} | {stop, normal, State} when
    State   :: state(),
    Request :: bookish_spork_request:t().
%% @private
complete_connection(#state{transport = Transport, socket = Socket} = State, Request) ->
    case bookish_spork_request:is_keepalive(Request) of
        true ->
            handle_connection(),
            {noreply, State};
        false ->
            Transport:shutdown(Socket, read_write),
            {stop, normal, State}
    end.

-spec receive_request(State :: state()) -> Result when
    Result :: {ok, Request} | socket_closed,
    Request :: bookish_spork_request:t().
%% @private
receive_request(State) ->
    #state{
        transport     = Transport,
        connection_id = ConnectionId,
        socket        = Socket,
        tls_ext       = TlsExt
    } = State,
    Request = bookish_spork_request:new(ConnectionId, Socket, TlsExt),
    read_from_socket(Transport, Socket, Request).

-spec read_from_socket(Transport, Socket, RequestIn) -> Result when
    Transport  :: transport(),
    Socket     :: socket(),
    RequestIn  :: bookish_spork_request:t(),
    Result     :: {ok, RequestOut} | socket_closed,
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
            socket_closed;
        {error, enotconn} ->
            socket_closed
    end.

-spec read_body(Transport, Socket, ContentLength) -> Body when
    Transport     :: transport(),
    Socket        :: socket(),
    ContentLength :: non_neg_integer(),
    Body          :: binary().
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
    Socket    :: socket(),
    Response  :: bookish_spork:stub_request_fun() | bookish_spork_response:t(),
    Request   :: bookish_spork_request:t().
%% @private
reply(Transport, Socket, ResponseFun, Request) when is_function(ResponseFun) ->
    Response = ResponseFun(Request),
    reply(Transport, Socket, bookish_spork_response:new(Response), Request);
reply(Transport, Socket, Response, _Request) ->
    String = bookish_spork_response:write_str(Response, calendar:universal_time()),
    Transport:send(Socket, [String]).
