

# Module bookish_spork_blocking_queue #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="types"></a>

## Data Types ##




### <a name="type-element">element()</a> ###


<pre><code>
element() = any()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#in-2">in/2</a></td><td></td></tr><tr><td valign="top"><a href="#out-1">out/1</a></td><td></td></tr><tr><td valign="top"><a href="#out-2">out/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="in-2"></a>

### in/2 ###

<pre><code>
in(Pid::pid(), Element::<a href="#type-element">element()</a>) -&gt; ok
</code></pre>
<br />

<a name="out-1"></a>

### out/1 ###

<pre><code>
out(Pid::pid()) -&gt; {ok, <a href="#type-element">element()</a>} | {error, timeout}
</code></pre>
<br />

<a name="out-2"></a>

### out/2 ###

<pre><code>
out(Pid, Timeout) -&gt; {ok, <a href="#type-element">element()</a>} | {error, timeout}
</code></pre>

<ul class="definitions"><li><code>Pid = pid()</code></li><li><code>Timeout = timeout()</code></li></ul>

<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`

