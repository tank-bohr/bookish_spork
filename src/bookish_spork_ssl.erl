-module(bookish_spork_ssl).

-behaviour(bookish_spork_transport).

-export([
    listen/2,
    accept/2,
    recv/2,
    send/2,
    close/1,
    shutdown/2
]).

-define(SSL_OPTIONS, [
    {certfile, filename:join(code:priv_dir(bookish_spork), "cert/cert.pem")},
    {keyfile, filename:join(code:priv_dir(bookish_spork), "cert/key.pem")},
    {verify, verify_none}
]).

-ifdef(OTP_RELEASE).
-define(HELLO, [{handshake, hello}]).
-else.
-define(HELLO, []).
-endif.

listen(Port, Options) ->
    ssl:listen(Port, Options ++ ?SSL_OPTIONS ++ ?HELLO).

-ifdef(OTP_RELEASE).
accept(ListenSocket, Timeout) ->
    {ok, Socket} = ssl:transport_accept(ListenSocket, Timeout),
    {ok, HsSocket, Ext} = ssl:handshake(Socket),
    {ok, SslSocket} = ssl:handshake_continue(HsSocket, []),
    {ok, SslSocket, Ext}.
-else.
accept(ListenSocket, Timeout) ->
    {ok, Socket} = ssl:transport_accept(ListenSocket, Timeout),
    ok = ssl:ssl_accept(Socket),
    {ok, Socket}.
-endif.

recv(Socket, Length) ->
    ssl:recv(Socket, Length).

send(Socket, Data) ->
    ssl:send(Socket, Data).

close(Socket) ->
    ssl:close(Socket).

shutdown(Socket, How) ->
    ssl:shutdown(Socket, How).
