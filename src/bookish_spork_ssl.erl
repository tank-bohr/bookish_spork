-module(bookish_spork_ssl).

-export([
    listen/2,
    accept/1,
    recv/2,
    send/2,
    close/1,
    shutdown/2
]).

-define(SSL_OPTIONS, [
    {certfile, filename:join(code:priv_dir(bookish_spork), "cert/cert.pem")},
    {keyfile, filename:join(code:priv_dir(bookish_spork), "cert/key.pem")},
    {verify, verify_none},
    {handshake, hello}
]).

listen(Port, Options) ->
    ssl:listen(Port, Options ++ ?SSL_OPTIONS).

-ifdef(OTP_RELEASE).
accept(ListenSocket) ->
    {ok, Socket} = ssl:transport_accept(ListenSocket),
    {ok, HsSocket, Ext} = ssl:handshake(Socket),
    {ok, SslSocket} = ssl:handshake_continue(HsSocket, []),
    {ok, SslSocket, Ext}.
-else.
accept(ListenSocket) ->
    {ok, Socket} = ssl:transport_accept(ListenSocket),
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
