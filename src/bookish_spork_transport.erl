-module(bookish_spork_transport).

-export([
    listen/2,
    accept/1,
    recv/1,
    read_raw/2,
    send/2,
    close/1,
    shutdown/1
]).

-export([
    socket/1,
    ssl_ext/1,
    ssl_info/1,
    connection_id/1
]).

-type callback_module() :: gen_tcp | bookish_spork_ssl.
-type socket() :: gen_tcp:socket() | ssl:sslsocket().

-record(listen, {
    module :: callback_module(),
    socket :: socket()
}).

-record(transport, {
    id      :: binary(),
    module  :: callback_module(),
    socket  :: socket(),
    ssl_ext :: undefined | ssl:protocol_extensions()
}).

-callback listen(Port, Options) -> Result when
    Port         :: inet:port_number(),
    Options      :: [gen_tcp:listen_option()],
    Result       :: {ok, ListenSocket} | {error, Reason},
    ListenSocket :: socket(),
    Reason       :: system_limit | inet:posix().
-callback accept(ListenSocket) -> Result when
    ListenSocket :: socket(),
    Result       :: {ok, Socket} | {ok, Socket, Ext} | {error, Reason},
    Socket       :: socket(),
    Ext          :: ssl:protocol_extensions(),
    Reason       :: closed | timeout | system_limit | inet:posix().
-callback recv(Socket, Length) -> {ok, Packet} | {error, Reason} when
    Socket :: socket(),
    Length :: non_neg_integer(),
    Packet :: term(),
    Reason :: closed | timeout | inet:posix().
-callback send(Socket, Packet) -> ok | {error, Reason} when
    Socket :: socket(),
    Packet :: iodata(),
    Reason :: closed | inet:posix().
-callback close(socket()) -> ok.
-callback shutdown(Socket, How) -> ok | {error, Reason} when
    Socket :: socket(),
    How    :: read | write | read_write,
    Reason :: inet:posix().

-opaque t() :: #transport{}.
-opaque listen() :: #listen{}.
-export_type([
    t/0,
    listen/0,
    socket/0
]).

-define(LISTEN_OPTIONS, [
    binary,
    {packet, http},
    {active, false},
    {reuseaddr, true}
]).
-define(IS_SSL_SOCKET(Socket), is_tuple(Socket) andalso element(1, Socket) =:= sslsocket).

-spec listen(callback_module(), inet:port_number()) -> listen().
listen(Module, Port) ->
    {ok, Socket} = Module:listen(Port, ?LISTEN_OPTIONS),
    #listen{socket = Socket, module = Module}.

-spec accept(listen()) -> t().
accept(#listen{socket = ListenSocket, module = Module}) ->
    case Module:accept(ListenSocket) of
        {ok, Socket, Ext} ->
            #transport{id = generate_id(), module = Module, socket = Socket, ssl_ext = Ext};
        {ok, Socket} ->
            #transport{id = generate_id(), module = Module, socket = Socket}
    end.

-spec recv(t()) -> {ok, term()} | {error, term()}.
recv(#transport{socket = Socket, module = Module}) ->
    Module:recv(Socket, 0).

-spec read_raw(t(), integer()) -> binary().
read_raw(_, 0) ->
    <<>>;
read_raw(#transport{socket = Socket, module = Module}, ContentLength) ->
    inet:setopts(Socket, [{packet, raw}]),
    {ok, Body} = Module:recv(Socket, ContentLength),
    inet:setopts(Socket, [{packet, http}]),
    Body.

-spec send(t(), iodata()) -> ok.
send(#transport{socket = Socket, module = Module}, String) ->
    Module:send(Socket, [String]).

-spec close(listen() | t()) -> ok.
close(#listen{socket = Socket, module = Module}) ->
    Module:close(Socket);
close(#transport{socket = Socket, module = Module}) ->
    Module:close(Socket).

-spec shutdown(t()) -> ok.
shutdown(#transport{socket = Socket, module = Module}) ->
    Module:shutdown(Socket, read_write).

-spec socket(t()) -> socket().
socket(#transport{socket = Socket}) ->
    Socket.

-spec connection_id(t()) -> binary().
connection_id(#transport{id = Id}) ->
    Id.

-spec ssl_ext(t()) -> undefined | ssl:protocol_extensions().
ssl_ext(#transport{ssl_ext = Ext}) ->
    Ext.

-spec ssl_info(t()) -> undefined | proplists:proplist().
ssl_info(#transport{socket = Socket}) when ?IS_SSL_SOCKET(Socket) ->
    {ok, Info} = ssl:connection_information(Socket),
    Info;
ssl_info(_) ->
    nil.

-spec generate_id() -> Id :: binary().
%% @doc generates unique id to be a connection id
generate_id() ->
    Bytes = crypto:strong_rand_bytes(7),
    Base64 = base64:encode(Bytes),
    string:trim(Base64, trailing, "=").
