-module(bookish_spork_acceptor).

-export([
    start_link/3
]).

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-record(state, {
    server    :: pid(),
    transport :: gen_tcp | bookish_spork_ssl,
    socket    :: gen_tcp:socket() | ssl:sslsocket(),
    sup       :: pid()
}).

start_link(Server, Transport, ListenSocket) ->
    gen_server:start_link(?MODULE, {Server, Transport, ListenSocket}, []).

%% @private
init({Server, Transport, ListenSocket}) ->
    {ok, Sup} = bookish_spork_sup:start_handler_sup(Server, Transport),
    accept(),
    {ok, #state{
        server = Server,
        transport = Transport,
        socket = ListenSocket,
        sup = Sup
    }}.

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @private
handle_cast(accept, #state{transport = Transport, socket = Socket, sup = Sup} = State) ->
    accept(Transport, Socket, Sup),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, #state{sup = Sup}) ->
    bookish_spork_sup:stop(Sup).

accept() ->
    gen_server:cast(self(), accept).

accept(Transport, ListenSocket, Sup) ->
    {Socket, TlsExt} = case Transport:accept(ListenSocket) of
        {ok, Sock, Ext} ->
            {Sock, Ext};
        {ok, Sock} ->
            {Sock, undefined}
    end,
    ConnectionId = generate_id(),
    bookish_spork_sup:start_handler(Sup, Socket, TlsExt, ConnectionId),
    accept().

-spec generate_id() -> Id :: binary().
%% @doc generates unique id to be a connection id
generate_id() ->
    Bytes = crypto:strong_rand_bytes(7),
    Base64 = base64:encode(Bytes),
    string:trim(Base64, trailing, "=").
