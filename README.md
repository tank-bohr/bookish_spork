

# Bookish spork #

Copyright (c) 2018-2019 Alexey Nikitin

__Version:__ 0.3.0

__Authors:__ Alexey Nikitin ([`tank@bohr.su`](mailto:tank@bohr.su)) (_web site:_ [`https://twitter.com/tank_bohr`](https://twitter.com/tank_bohr)).

An erlang library to test http requests. Inspired by Ruby's [WebMock](https://github.com/bblimke/webmock).

Suitable for Elixir.

[![Build Status](https://travis-ci.org/tank-bohr/bookish_spork.svg?branch=master)
](https://travis-ci.org/tank-bohr/bookish_spork)
[![Coverage Status](https://coveralls.io/repos/github/tank-bohr/bookish_spork/badge.svg?branch=master)
](https://coveralls.io/github/tank-bohr/bookish_spork?branch=master)
[![Hex.pm](https://img.shields.io/hexpm/v/bookish_spork.svg)
](https://hex.pm/packages/bookish_spork)
[![Gitter](https://badges.gitter.im/join.svg)
](https://gitter.im/bookish_spork)


### <a name="Rationale">Rationale</a> ###

There are several ways to test your http interaction

* Real http request to real servers: not very reliable, requires internet
* You can use external http server like [`https://httpbin.org/`](https://httpbin.org/) (hackney approach)
* You can mock your http client library
* Also you can run an http-server within your application on your localhost on a particualr port


The last approach is the best IMHO. It is absolutely http-client agnostic. It doesn't require internet connection or any external utilities.

bookish_spork provides you facilities to test your requests with 
<strong>real</strong>
 http server.


### <a name="Usage">Usage</a> ###

Bookish spork supports Erlang/OTP 20.3 or later.

First step: add to your rebar config

```erlang

{profiles, [
    {test, [
        {deps, [
            {bookish_spork, "0.3.0"}
        ]}
    ]}
]}.
```

Second: start server in your tests.

```erlang

bookish_spork:start_server().

```

It starts process without link. Thus you can use it in `init_per_group` and in `init_per_suite` callbacks. Default port is 32002 but you can specify any port you like with `bookish_spork:start_server/1`


#### <a name="Stub_request">Stub request</a> ####

The simplest stub you can do is

```erlang

bookish_spork:stub_request().

```

It will stub your requests with `204 No Content` response with empty body.

If you need specify response you easily can do this:

```erlang

bookish_spork:stub_request([Status, Headers, Content]).

```


#### <a name="Capture_request">Capture request</a> ####

As usual the main goal is to test that you send the correct request

```erlang

{ok, Request} = bookish_spork:capture_request().

```

It returns you an opaque structure of the request. You can inspect it with

* [`bookish_spork_request:method/1`](http://github.com/tank-bohr/bookish_spork/blob/master/doc/bookish_spork_request.md#method-1)
* [`bookish_spork_request:uri/1`](http://github.com/tank-bohr/bookish_spork/blob/master/doc/bookish_spork_request.md#uri-1)
* [`bookish_spork_request:headers/1`](http://github.com/tank-bohr/bookish_spork/blob/master/doc/bookish_spork_request.md#headers-1)
* [`bookish_spork_request:body/1`](http://github.com/tank-bohr/bookish_spork/blob/master/doc/bookish_spork_request.md#body-1)



#### <a name="Bypass_comparision">Bypass comparision</a> ####

An elixir library [bypass](https://github.com/PSPDFKit-labs/bypass) does pretty much the same. And illustrates the same approach. It starts a cowboy web-server to replace a real service for test

But bookish_spork has some advantages:

* Bypass depends on `cowboy` and `plug`. Bookish spork has zero dependencies
* Bookish spork works seamlessly with both erlang and elixir. Bypass is supposed to be an elixir only library
* Bookish spork much simpler (I believe)


#### <a name="Examples">Examples</a> ####

Setup and teardown

```erlang

init_per_group(_GroupName, Config) ->
    {ok, _} = bookish_spork:start_server(),
    Config.

end_per_group(_GroupName, _Config) ->
    ok = bookish_spork:stop_server().

```

Set expectation

```erlang
init_per_testcase(random_test, Config) ->
    bookish_spork:stub_request([200, #{}
        <<"{\"value\": \"Chuck Norris' favourite word: chunk.\"}">>]),
    Config.

```

Make assertions

```erlang
random_test(_Config) ->
    ?assertEqual(<<"Chuck Norris' favourite word: chunk.">>, testee:make_request()),
    {ok, Request} = bookish_spork:capture_request(),
    ?ssertEqual("/jokes/random", bookish_spork_request:uri(Request)).

```

As you can see there are two types of assertions:

* we check a testee function result
* we check a side effect: verifying outgoing request has correct attributes (uri in this case)


<h5><a name="More_complex_expectations">More complex expectations</a></h5>

There are cases when the testee function initiates more than one request. But if you know the order of your requests, you can set several expectations

```erlang
bookish_spork:stub_request([200, #{}, <<"{\"value\": \"The first response\"}">>]),
bookish_spork:stub_request([200, #{}, <<"{\"value\": \"The second response\"}">>]).

```

The library will response in the order the stubs were defined.

Sometimes you can't guarantee the order of requests. Then you may stub request with the fun

```erlang
bookish_spork:stub_request(fun(Request) ->
    case bookish_spork_request:uri(Request) of
        "/bookish/spork" ->
            [200, #{}, <<"Hello">>];
        "/admin/sporks" ->
            [403, #{}, <<"It is not possible here">>]
    end
end)

```

[Module to work with request](http://github.com/tank-bohr/bookish_spork/blob/master/doc/bookish_spork_request.md)

[Module to work with response](http://github.com/tank-bohr/bookish_spork/blob/master/doc/bookish_spork_response.md)

<h5><a name="Stub_multiple_requests_with_one_response">Stub multiple requests with one response</a></h5>

It can be usefull to stub several requests with one command

```erlang

bookish_spork:stub_request([200, #{<<"Content-Type" => "text/plan">>}, <<"Pants">>], _Times = 20)

```

The same with the `fun`

```erlang

bookish_spork:stub_request(fun(Req) ->
    Body = bookish_spork_request:body(Req),
    [200, #{<<"X-Respond-With">> => <<"echo">>}, Body]
end, _Times = 150)

```

As you can see that it's not necessary to build response structure yourself. You can use handy [three-element tuple or list syntax](https://github.com/tank-bohr/bookish_spork/issues/32) to define the response. But the [`bookish_spork_response:new/1`](http://github.com/tank-bohr/bookish_spork/blob/master/doc/bookish_spork_response.md#new-1) still works.

<h5><a name="Elixir_example">Elixir example</a></h5>

```elixir

defmodule ChuckNorrisApiTest do
  use ExUnit.Case
  doctest ChuckNorrisApi

  setup_all do
    {:ok, _} = :bookish_spork.start_server
    {:ok, %{}}
  end

  test "retrieves a random joke" do
    :bookish_spork.stub_request([200, %{}, "{
      \"value\": \"Chuck norris tried to crank that soulja boy but it wouldn't crank up\"
    }"])
    assert ChuckNorrisApi.random == "Chuck norris tried to crank that soulja boy but it wouldn't crank up"

    {:ok, request} = :bookish_spork.capture_request
    assert :bookish_spork_request.uri(request) == '/jokes/random'
  end
end

```
For more details see examples dir.


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/tank-bohr/bookish_spork/blob/master/doc/bookish_spork.md" class="module">bookish_spork</a></td></tr>
<tr><td><a href="http://github.com/tank-bohr/bookish_spork/blob/master/doc/bookish_spork_format.md" class="module">bookish_spork_format</a></td></tr>
<tr><td><a href="http://github.com/tank-bohr/bookish_spork/blob/master/doc/bookish_spork_request.md" class="module">bookish_spork_request</a></td></tr>
<tr><td><a href="http://github.com/tank-bohr/bookish_spork/blob/master/doc/bookish_spork_response.md" class="module">bookish_spork_response</a></td></tr>
<tr><td><a href="http://github.com/tank-bohr/bookish_spork/blob/master/doc/bookish_spork_server.md" class="module">bookish_spork_server</a></td></tr></table>

