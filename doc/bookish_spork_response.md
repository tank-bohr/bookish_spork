

# Module bookish_spork_response #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-response">response()</a> ###


<pre><code>
response() = <a href="#type-t">t()</a> | non_neg_integer() | {non_neg_integer(), map(), binary()} | {non_neg_integer(), list(), binary()} | nonempty_list()
</code></pre>




### <a name="type-t">t()</a> ###


__abstract datatype__: `t()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-0">new/0</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Constructs a response data structure.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr><tr><td valign="top"><a href="#write_str-2">write_str/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Response::<a href="#type-response">response()</a>) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

Constructs a response data structure

Arg can be one of

* Http status code [`https://tools.ietf.org/html/rfc2616#section-6.1.1`](https://tools.ietf.org.md/rfc2616#section-6.1.1)

* Response tuple `{Status, Headers, Body}`

* Response list `[Status, Headers, Body]`

* Response record. Then returns itself


Headers may be map or proplist

Example:

```
  bookish_spork_response:new([200, #{}, <<"Hello">>])
```

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Status::non_neg_integer(), ContentOrHeaders::binary() | map()) -&gt; <a href="#type-t">t()</a>
</code></pre>
<br />

<a name="write_str-2"></a>

### write_str/2 ###

<pre><code>
write_str(Response::<a href="#type-t">t()</a>, Now::<a href="calendar.md#type-datetime">calendar:datetime()</a>) -&gt; binary()
</code></pre>
<br />

