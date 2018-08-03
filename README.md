# Bookish spork

[![Build Status](https://travis-ci.org/tank-bohr/bookish_spork.svg?branch=master)](https://travis-ci.org/tank-bohr/bookish_spork) [![Coverage Status](https://coveralls.io/repos/github/tank-bohr/bookish_spork/badge.svg?branch=master)](https://coveralls.io/github/tank-bohr/bookish_spork?branch=master) [![codecov](https://codecov.io/gh/tank-bohr/bookish_spork/branch/master/graph/badge.svg)](https://codecov.io/gh/tank-bohr/bookish_spork)

An erlang library to test http requests. Inspired by Ruby's [WebMock](https://github.com/bblimke/webmock).

Suitable for Elixir.

## Rationale


There are several ways to test your http interaction

* Real http request to real servers: not very reliable, requires internet

* You can use external http server like [httpbin](https://httpbin.org/) (hackney approach)

* You can mock your http client library (elixir approach)

* Also you can run an http-server within your application on your localhost on a particualr port

The last approach is the best IMHO. It is absolutely http-client agnostic. It doesn't require internet connection or any external utilities.

bookish_spork provides you facilities to test your requests with *real* http server.


## Usage


First step: add to your rebar config

```erlang
{profiles, [
    {test, [
        {deps, [
            {bookish_spork, "0.1.1"}
        ]}
    ]}
]}.
```

Second: start server in your tests.

```erlang
bookish_spork:start_server().
```

It starts process without link. Thus you can use it in `init_per_group` and in `init_per_suite` callbacks. Default port is 32002 but you can specify any port you like with `bookish_spork:start_server/1`


### Stub request

The simplest stub you can do is

```erlang
bookish_spork:stub_request().
```

It will stub your requests with `204 No Content` response with empty body.

If you need specify response you easily can do this:


```erlang
bookish_spork:stub_request(Status, Headers, Content).
```


### Capture request

As usual the main goal is to test that you send the correct request


```erlang
{ok, Request} = bookish_spork:capture_request().
```

It returns you an opaque structure of the request. You can inspect it with

* `bookish_spork_request:method/1`
* `bookish_spork_request:uri/1`
* `bookish_spork_request:headers/1`
* `bookish_spork_request:body/1`


## Examples

See examples dir.
