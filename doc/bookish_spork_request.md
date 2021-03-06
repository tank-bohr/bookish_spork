

# Module bookish_spork_request #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-t">t()</a> ###


__abstract datatype__: `t()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#body-1">body/1</a></td><td>request body.</td></tr><tr><td valign="top"><a href="#content_length-1">content_length/1</a></td><td>Content-Length header value as intger.</td></tr><tr><td valign="top"><a href="#from_transport-1">from_transport/1</a></td><td></td></tr><tr><td valign="top"><a href="#header-2">header/2</a></td><td>Returns a particular header from request.</td></tr><tr><td valign="top"><a href="#headers-1">headers/1</a></td><td>HTTP headers map.</td></tr><tr><td valign="top"><a href="#is_keepalive-1">is_keepalive/1</a></td><td>tells you if the request is keepalive or not <a href="https://tools.ietf.org.md/rfc6223" target="_top"><tt>https://tools.ietf.org/html/rfc6223</tt></a></td></tr><tr><td valign="top"><a href="#method-1">method/1</a></td><td>http verb in lower case: get, post, put, delete, options, ...</td></tr><tr><td valign="top"><a href="#raw_headers-1">raw_headers/1</a></td><td>HTTP raw headers.</td></tr><tr><td valign="top"><a href="#transport-1">transport/1</a></td><td></td></tr><tr><td valign="top"><a href="#uri-1">uri/1</a></td><td>path with query string.</td></tr><tr><td valign="top"><a href="#version-1">version/1</a></td><td>http protocol version tuple.</td></tr></table>


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

<a name="from_transport-1"></a>

### from_transport/1 ###

<pre><code>
from_transport(Transport::<a href="bookish_spork_transport.md#type-t">bookish_spork_transport:t()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="header-2"></a>

### header/2 ###

<pre><code>
header(Request::<a href="#type-t">t()</a>, HeaderName::string() | binary()) -&gt; binary() | nil
</code></pre>
<br />

Returns a particular header from request.

<a name="headers-1"></a>

### headers/1 ###

<pre><code>
headers(Request::<a href="#type-t">t()</a>) -&gt; map()
</code></pre>
<br />

HTTP headers map. Header names are normalized and lowercased

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

http verb in lower case: get, post, put, delete, options, ...

<a name="raw_headers-1"></a>

### raw_headers/1 ###

<pre><code>
raw_headers(Request::<a href="#type-t">t()</a>) -&gt; <a href="proplists.md#type-proplist">proplists:proplist()</a>
</code></pre>
<br />

HTTP raw headers. Headers order and case are preserved

<a name="transport-1"></a>

### transport/1 ###

`transport(X1) -> any()`

<a name="uri-1"></a>

### uri/1 ###

<pre><code>
uri(Request::<a href="#type-t">t()</a>) -&gt; binary() | nil
</code></pre>
<br />

path with query string

<a name="version-1"></a>

### version/1 ###

<pre><code>
version(Request::<a href="#type-t">t()</a>) -&gt; tuple() | nil
</code></pre>
<br />

http protocol version tuple. Most often would be `{1, 1}`

