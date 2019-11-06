

# Module bookish_spork_handler #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="types"></a>

## Data Types ##




### <a name="type-connection_id">connection_id()</a> ###


<pre><code>
connection_id() = binary()
</code></pre>




### <a name="type-socket">socket()</a> ###


<pre><code>
socket() = <a href="gen_tcp.md#type-socket">gen_tcp:socket()</a> | <a href="ssl.md#type-sslsocket">ssl:sslsocket()</a>
</code></pre>




### <a name="type-tls_ext">tls_ext()</a> ###


<pre><code>
tls_ext() = undefined | <a href="ssl.md#type-protocol_extensions">ssl:protocol_extensions()</a>
</code></pre>




### <a name="type-transport">transport()</a> ###


<pre><code>
transport() = gen_tcp | bookish_spork_ssl
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#child_spec-1">child_spec/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-5">start_link/5</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="child_spec-1"></a>

### child_spec/1 ###

`child_spec(Args) -> any()`

<a name="start_link-5"></a>

### start_link/5 ###

<pre><code>
start_link(Server, Transport, Socket, TlsExt, ConnectionId) -&gt; {ok, pid()}
</code></pre>

<ul class="definitions"><li><code>Server = pid()</code></li><li><code>Transport = <a href="#type-transport">transport()</a></code></li><li><code>Socket = <a href="#type-socket">socket()</a></code></li><li><code>TlsExt = <a href="#type-tls_ext">tls_ext()</a></code></li><li><code>ConnectionId = <a href="#type-connection_id">connection_id()</a></code></li></ul>

