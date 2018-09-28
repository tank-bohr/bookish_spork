

# Module bookish_spork_server #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="types"></a>

## Data Types ##




### <a name="type-response">response()</a> ###


<pre><code>
response() = <a href="bookish_spork_response.md#type-response">bookish_spork_response:response()</a> | function()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#respond_with-1">respond_with/1</a></td><td></td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td>starts server.</td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td>stops server.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="respond_with-1"></a>

### respond_with/1 ###

<pre><code>
respond_with(Response::<a href="#type-response">response()</a>) -&gt; ok
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

