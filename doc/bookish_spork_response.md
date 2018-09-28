

# Module bookish_spork_response #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-response">response()</a> ###


__abstract datatype__: `response()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-0">new/0</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td></td></tr><tr><td valign="top"><a href="#write_str-2">write_str/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="#type-response">response()</a>
</code></pre>
<br />

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Status::non_neg_integer()) -&gt; <a href="#type-response">response()</a>
</code></pre>
<br />

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Status::non_neg_integer(), ContentOrHeaders::binary() | map()) -&gt; <a href="#type-response">response()</a>
</code></pre>
<br />

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(Status::non_neg_integer(), Headers::map(), Content::binary()) -&gt; <a href="#type-response">response()</a>
</code></pre>
<br />

<a name="write_str-2"></a>

### write_str/2 ###

<pre><code>
write_str(Response::<a href="#type-response">response()</a>, Now::<a href="calendar.md#type-datetime">calendar:datetime()</a>) -&gt; binary()
</code></pre>
<br />

