

# Module bookish_spork_server #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="types"></a>

## Data Types ##




### <a name="type-request">request()</a> ###


<pre><code>
request() = <a href="bookish_spork_request.md#type-t">bookish_spork_request:t()</a>
</code></pre>




### <a name="type-response">response()</a> ###


<pre><code>
response() = <a href="bookish_spork_response.md#type-t">bookish_spork_response:t()</a> | <a href="bookish_spork.md#type-stub_request_fun">bookish_spork:stub_request_fun()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#respond_with-2">respond_with/2</a></td><td></td></tr><tr><td valign="top"><a href="#response-1">response/1</a></td><td>Used by <a href="bookish_spork_acceptor.md"><code>bookish_spork_acceptor</code></a></td></tr><tr><td valign="top"><a href="#retrieve_request-1">retrieve_request/1</a></td><td></td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td>starts server.</td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td>stops server.</td></tr><tr><td valign="top"><a href="#store_request-2">store_request/2</a></td><td>Used by <a href="bookish_spork_acceptor.md"><code>bookish_spork_acceptor</code></a></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="respond_with-2"></a>

### respond_with/2 ###

<pre><code>
respond_with(Response::<a href="#type-response">response()</a>, Times::non_neg_integer()) -&gt; ok
</code></pre>
<br />

<a name="response-1"></a>

### response/1 ###

<pre><code>
response(Server::pid()) -&gt; {ok, <a href="#type-response">response()</a>} | {error, no_response}
</code></pre>
<br />

Used by [`bookish_spork_acceptor`](bookish_spork_acceptor.md)

<a name="retrieve_request-1"></a>

### retrieve_request/1 ###

<pre><code>
retrieve_request(Timeout) -&gt; {ok, Request} | {error, Error}
</code></pre>

<ul class="definitions"><li><code>Timeout = non_neg_integer()</code></li><li><code>Request = <a href="#type-request">request()</a></code></li><li><code>Error = term()</code></li></ul>

<a name="start-1"></a>

### start/1 ###

<pre><code>
start(Options::<a href="proplists.md#type-proplist">proplists:proplist()</a>) -&gt; {ok, pid()} | {error, Error::term()}
</code></pre>
<br />

starts server

<a name="stop-0"></a>

### stop/0 ###

<pre><code>
stop() -&gt; ok
</code></pre>
<br />

stops server

<a name="store_request-2"></a>

### store_request/2 ###

<pre><code>
store_request(Server::pid(), Request::<a href="#type-request">request()</a>) -&gt; ok
</code></pre>
<br />

Used by [`bookish_spork_acceptor`](bookish_spork_acceptor.md)

