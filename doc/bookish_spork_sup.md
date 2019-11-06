

# Module bookish_spork_sup #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`supervisor`](supervisor.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#start_acceptor_sup-3">start_acceptor_sup/3</a></td><td></td></tr><tr><td valign="top"><a href="#start_handler-4">start_handler/4</a></td><td></td></tr><tr><td valign="top"><a href="#start_handler_sup-2">start_handler_sup/2</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="start_acceptor_sup-3"></a>

### start_acceptor_sup/3 ###

<pre><code>
start_acceptor_sup(Server, Transport, ListenSocket) -&gt; {ok, pid()}
</code></pre>

<ul class="definitions"><li><code>Server = pid()</code></li><li><code>Transport = gen_tcp | bookish_spork_ssl</code></li><li><code>ListenSocket = term()</code></li></ul>

<a name="start_handler-4"></a>

### start_handler/4 ###

`start_handler(Sup, Socket, TlsExt, ConnectionId) -> any()`

<a name="start_handler_sup-2"></a>

### start_handler_sup/2 ###

`start_handler_sup(Server, Transport) -> any()`

<a name="stop-1"></a>

### stop/1 ###

`stop(Sup) -> any()`

