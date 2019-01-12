

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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#respond_with-2">respond_with/2</a></td><td></td></tr><tr><td valign="top"><a href="#retrieve_request-0">retrieve_request/0</a></td><td></td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td>starts server.</td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td>stops server.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="respond_with-2"></a>

### respond_with/2 ###

<pre><code>
respond_with(Response::<a href="#type-response">response()</a>, Times::non_neg_integer()) -&gt; ok
</code></pre>
<br />

<a name="retrieve_request-0"></a>

### retrieve_request/0 ###

<pre><code>
retrieve_request() -&gt; {ok, Request::<a href="#type-request">request()</a>} | {error, Error::term()}
</code></pre>
<br />

<a name="start-1"></a>

### start/1 ###

<pre><code>
start(Port::non_neg_integer()) -&gt; {ok, pid()} | {error, Error::term()}
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

