-module(bookish_spork_request).

-export([
    '__struct__'/0,
    '__struct__'/1
]).

-export([
    new/0,
    request_line/4,
    add_header/3,
    content_length/1,
    is_keepalive/1
]).

-export([
    method/1,
    uri/1,
    version/1,
    header/2,
    headers/1,
    body/1,
    body/2
]).

-type http_version() :: {
    Major :: integer(),
    Minor :: integer()
}.

-opaque t() :: #{
    '__struct__'  := ?MODULE,
    method        := nil | atom(),
    uri           := nil | string(),
    version       := nil | http_version(),
    headers       := map(),
    body          := nil | binary()
}.

-export_type([
    t/0
]).

-spec '__struct__'() -> t().
%% @private
'__struct__'() ->
    new().

-spec '__struct__'(From :: list() | map()) -> t().
%% @private
'__struct__'(From) ->
    new(From).

-spec new() -> t().
%% @private
new() ->
    #{
        '__struct__' => ?MODULE,
        method => nil,
        uri => nil,
        version => nil,
        headers => #{},
        body => nil
    }.

-spec new(From :: list() | map()) -> t().
%% @private
new(List) when is_list(List) ->
    new(maps:from_list(List));
new(Map) when is_map(Map) ->
    maps:fold(fun maps:update/3, new(), Map).

-spec request_line(
    Request :: t(),
    Method  :: atom(),
    Uri     :: string() | undefined,
    Version :: http_version() | undefined
) -> t().
%% @private
request_line(Request, Method, Uri, Version) ->
    maps:merge(Request, #{ method => Method, uri => Uri, version => Version }).

-spec add_header(Request :: t(), Name :: string(), Value :: string()) -> t().
%% @private
add_header(Request, Name, Value) when is_atom(Name) ->
    add_header(Request, atom_to_list(Name), Value);
add_header(#{ headers := Headers } = Request, Name, Value) ->
    HeaderName = string:lowercase(Name),
    maps:update(headers, maps:put(HeaderName, Value, Headers), Request).

-spec content_length(Request :: t()) -> integer().
%% @doc Content-Length header value as intger
content_length(Request) ->
    case header(Request, "content-length") of
        nil ->
            0;
        ContentLength ->
            list_to_integer(ContentLength)
    end.

-spec method(Request :: t()) -> atom().
%% @doc http verb: 'GET', 'POST','PUT', 'DELETE', 'OPTIONS', ...
method(#{ method := Method}) ->
    Method.

-spec uri(Request :: t()) -> string().
%% @doc path with query string
uri(#{ uri := Uri}) ->
    Uri.

-spec version(Request :: t()) -> string() | nil.
%% @doc http protocol version tuple. Most often would be `{1, 1}'
version(#{ version := Version }) ->
    Version.

-spec header(Request :: t(), HeaderName :: string()) -> string() | nil.
%% @doc Returns a particular header from request. Header name is lowerced
header(#{ headers := Headers }, HeaderName) ->
    maps:get(HeaderName, Headers, nil).

-spec headers(Request :: t()) -> map().
%% @doc http headers map. Header names are normalized and lowercased
headers(#{ headers := Headers }) ->
    Headers.

-spec body(Request :: t()) -> binary().
%% @doc request body
body(#{ body := Body }) ->
    Body.

-spec body(Request :: t(), Body :: binary()) -> t().
%% @private
body(Request, Body) ->
    maps:update(body, Body, Request).

-spec is_keepalive(Request :: t()) -> boolean().
%% @doc tells you if the request is keepalive or not [https://tools.ietf.org/html/rfc6223]
is_keepalive(#{ headers := #{"connection" := Conn }, version := {1, 0} }) ->
    string:lowercase(Conn) =:= "keep-alive";
is_keepalive(#{ version := {1, 0} }) ->
    false;
is_keepalive(#{ headers := #{"connection" := "close"}, version := {1, 1} }) ->
    false;
is_keepalive(_) ->
    true.
