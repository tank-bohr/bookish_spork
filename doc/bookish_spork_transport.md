

# Module bookish_spork_transport #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__This module defines the `bookish_spork_transport` behaviour.__<br /> Required callback functions: `listen/2`, `accept/1`, `recv/2`, `send/2`, `close/1`, `shutdown/2`.

<a name="types"></a>

## Data Types ##




### <a name="type-callback_module">callback_module()</a> ###


<pre><code>
callback_module() = gen_tcp | bookish_spork_ssl
</code></pre>




### <a name="type-listen">listen()</a> ###


__abstract datatype__: `listen()`




### <a name="type-socket">socket()</a> ###


<pre><code>
socket() = <a href="gen_tcp.md#type-socket">gen_tcp:socket()</a> | <a href="ssl.md#type-sslsocket">ssl:sslsocket()</a>
</code></pre>




### <a name="type-t">t()</a> ###


__abstract datatype__: `t()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#accept-1">accept/1</a></td><td></td></tr><tr><td valign="top"><a href="#close-1">close/1</a></td><td></td></tr><tr><td valign="top"><a href="#connection_id-1">connection_id/1</a></td><td></td></tr><tr><td valign="top"><a href="#listen-2">listen/2</a></td><td></td></tr><tr><td valign="top"><a href="#read_raw-2">read_raw/2</a></td><td></td></tr><tr><td valign="top"><a href="#recv-1">recv/1</a></td><td></td></tr><tr><td valign="top"><a href="#send-2">send/2</a></td><td></td></tr><tr><td valign="top"><a href="#shutdown-1">shutdown/1</a></td><td></td></tr><tr><td valign="top"><a href="#socket-1">socket/1</a></td><td></td></tr><tr><td valign="top"><a href="#ssl_ext-1">ssl_ext/1</a></td><td></td></tr><tr><td valign="top"><a href="#ssl_info-1">ssl_info/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="accept-1"></a>

### accept/1 ###

<pre><code>
accept(Listen::<a href="#type-listen">listen()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="close-1"></a>

### close/1 ###

<pre><code>
close(Listen::<a href="#type-listen">listen()</a> | <a href="#type-t">t()</a>) -&gt; ok
</code></pre>
<br />

<a name="connection_id-1"></a>

### connection_id/1 ###

<pre><code>
connection_id(Transport::<a href="#type-t">t()</a>) -&gt; binary()
</code></pre>
<br />

<a name="listen-2"></a>

### listen/2 ###

<pre><code>
listen(Module::<a href="#type-callback_module">callback_module()</a>, Port::<a href="inet.md#type-port_number">inet:port_number()</a>) -&gt; <a href="#type-listen">listen()</a>
</code></pre>
<br />

<a name="read_raw-2"></a>

### read_raw/2 ###

<pre><code>
read_raw(Transport::<a href="#type-t">t()</a>, ContentLength::integer()) -&gt; binary()
</code></pre>
<br />

<a name="recv-1"></a>

### recv/1 ###

<pre><code>
recv(Transport::<a href="#type-t">t()</a>) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

<a name="send-2"></a>

### send/2 ###

<pre><code>
send(Transport::<a href="#type-t">t()</a>, String::iodata()) -&gt; ok
</code></pre>
<br />

<a name="shutdown-1"></a>

### shutdown/1 ###

<pre><code>
shutdown(Transport::<a href="#type-t">t()</a>) -&gt; ok
</code></pre>
<br />

<a name="socket-1"></a>

### socket/1 ###

<pre><code>
socket(Transport::<a href="#type-t">t()</a>) -&gt; <a href="#type-socket">socket()</a>
</code></pre>
<br />

<a name="ssl_ext-1"></a>

### ssl_ext/1 ###

<pre><code>
ssl_ext(Transport::<a href="#type-t">t()</a>) -&gt; undefined | <a href="ssl.md#type-protocol_extensions">ssl:protocol_extensions()</a>
</code></pre>
<br />

<a name="ssl_info-1"></a>

### ssl_info/1 ###

<pre><code>
ssl_info(Transport::<a href="#type-t">t()</a>) -&gt; undefined | <a href="proplists.md#type-proplist">proplists:proplist()</a>
</code></pre>
<br />

