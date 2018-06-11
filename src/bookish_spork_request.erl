-module(bookish_spork_request).

-export([
    new/0,
    request_line/4,
    add_header/3,
    content_length/1
]).

-export([
    method/1,
    uri/1,
    version/1,
    headers/1,
    body/1,
    body/2
]).

-type http_version() :: {
    Major :: integer(),
    Minor :: integer()
}.

-record(request, {
    method        :: atom(),
    uri           :: string(),
    version       :: http_version(),
    headers = #{} :: map(),
    body          :: binary()
}).

-opaque request() :: #request{}.

-export_type([
    request/0
]).

-spec new() -> request().
new() -> #request{}.

-spec request_line(
    Request :: request(),
    Method  :: atom(),
    Uri     :: string(),
    Version :: string()
) -> request().
request_line(Request, Method, Uri, Version) ->
    Request#request{ method = Method, uri = Uri, version = Version }.

-spec add_header(Request :: request(), Name :: string(), Value :: string()) -> request().
add_header(Request, Name, Value) when is_atom(Name) ->
    add_header(Request, atom_to_list(Name), Value);
add_header(#request{ headers = Headers } = Request, Name, Value) ->
    HeaderName  = string:lowercase(Name),
    Request#request{ headers = maps:put(HeaderName, Value, Headers) }.

-spec content_length(Request :: request()) -> integer().
content_length(#request{ headers = Headers }) ->
    case maps:get("content-length", Headers, undefined) of
        undefined ->
            0;
        ContentLength ->
            list_to_integer(ContentLength)
    end.

-spec method(Request :: request()) -> atom().
method(#request{ method = Method}) -> Method.

-spec uri(Request :: request()) -> string().
uri(#request{ uri = Uri}) -> Uri.

-spec version(Request :: request()) -> string().
version(#request{ version = Version }) -> Version.

-spec headers(Request :: request()) -> map().
headers(#request{ headers = Headers }) -> Headers.

-spec body(Request :: request()) -> binary().
body(#request{ body = Body }) -> Body.

-spec body(Request :: request(), Body :: binary()) -> request().
body(Request, Body) -> Request#request{ body = Body }.
