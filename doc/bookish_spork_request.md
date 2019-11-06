

# Module bookish_spork_request #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-socket">socket()</a> ###


<pre><code>
socket() = <a href="gen_tcp.md#type-socket">gen_tcp:socket()</a> | <a href="ssl.md#type-sslsocket">ssl:sslsocket()</a>
</code></pre>




### <a name="type-t">t()</a> ###


__abstract datatype__: `t()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#body-1">body/1</a></td><td>request body.</td></tr><tr><td valign="top"><a href="#content_length-1">content_length/1</a></td><td>Content-Length header value as intger.</td></tr><tr><td valign="top"><a href="#header-2">header/2</a></td><td>Returns a particular header from request.</td></tr><tr><td valign="top"><a href="#headers-1">headers/1</a></td><td>http headers map.</td></tr><tr><td valign="top"><a href="#is_keepalive-1">is_keepalive/1</a></td><td>tells you if the request is keepalive or not <a href="https://tools.ietf.org.md/rfc6223" target="_top"><tt>https://tools.ietf.org/html/rfc6223</tt></a></td></tr><tr><td valign="top"><a href="#method-1">method/1</a></td><td>http verb: 'GET', 'POST','PUT', 'DELETE', 'OPTIONS', ...</td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td>creates request with ssl info if socket is an ssl socket.</td></tr><tr><td valign="top"><a href="#uri-1">uri/1</a></td><td>path with query string.</td></tr><tr><td valign="top"><a href="#version-1">version/1</a></td><td>http protocol version tuple.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="body-1"></a>

### body/1 ###

<pre><code>
body(Request::<a href="#type-t">t()</a>) -&gt; binary()
</code></pre>
<br />

request body

<a name="content_length-1"></a>

### content_length/1 ###

<pre><code>
content_length(Request::<a href="#type-t">t()</a>) -&gt; integer()
</code></pre>
<br />

Content-Length header value as intger

<a name="header-2"></a>

### header/2 ###

<pre><code>
header(Request::<a href="#type-t">t()</a>, HeaderName::string()) -&gt; string() | nil
</code></pre>
<br />

Returns a particular header from request. Header name is lowerced

<a name="headers-1"></a>

### headers/1 ###

<pre><code>
headers(Request::<a href="#type-t">t()</a>) -&gt; map()
</code></pre>
<br />

http headers map. Header names are normalized and lowercased

<a name="is_keepalive-1"></a>

### is_keepalive/1 ###

<pre><code>
is_keepalive(Request::<a href="#type-t">t()</a>) -&gt; boolean()
</code></pre>
<br />

tells you if the request is keepalive or not [`https://tools.ietf.org/html/rfc6223`](https://tools.ietf.org.md/rfc6223)

<a name="method-1"></a>

### method/1 ###

<pre><code>
method(Request::<a href="#type-t">t()</a>) -&gt; atom()
</code></pre>
<br />

http verb: 'GET', 'POST','PUT', 'DELETE', 'OPTIONS', ...

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(ConnectionId, Socket, TlsExt) -&gt; Request
</code></pre>

<ul class="definitions"><li><code>ConnectionId = binary()</code></li><li><code>Socket = <a href="#type-socket">socket()</a></code></li><li><code>TlsExt = undefined | nil | <a href="ssl.md#type-protocol_extensions">ssl:protocol_extensions()</a></code></li><li><code>Request = <a href="#type-t">t()</a></code></li></ul>

creates request with ssl info if socket is an ssl socket

<a name="uri-1"></a>

### uri/1 ###

<pre><code>
uri(Request::<a href="#type-t">t()</a>) -&gt; string()
</code></pre>
<br />

path with query string

<a name="version-1"></a>

### version/1 ###

<pre><code>
version(Request::<a href="#type-t">t()</a>) -&gt; string() | nil
</code></pre>
<br />

http protocol version tuple. Most often would be `{1, 1}`

