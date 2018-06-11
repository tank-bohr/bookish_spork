-module(bookish_spork_settings).

-export([
    status/0,
    status/1,
    headers/0,
    header/2,
    content/0,
    content/1
]).

-define(TAB, ?MODULE).
-define(DEFAULT_TAG, bookish_spork).
-define(DEFAULT_STATUS, 204).
-define(DEFAULT_HEADERS, #{}).
-define(DEFAULT_CONTENT, <<>>).

-spec init() -> true.
init() ->
    case ets:info(?TAB) of
        undefined ->
            ets:new(?TAB, [set, public, named_table]);
        _ ->
            true
    end.

-spec status() -> non_neg_integer().
status() ->
    init(),
    case ets:lookup(?TAB, status) of
        [{ status, Status }] -> Status;
        _ -> ?DEFAULT_STATUS
    end.

-spec status(Status :: non_neg_integer()) -> true.
status(Status) when Status >= 100 andalso Status < 600 ->
    init(),
    ets:insert(?TAB, { status, Status }).

-spec headers() -> map().
headers() ->
    init(),
    case ets:lookup(?TAB, headers) of
        [{ headers, Headers }] -> Headers;
        _ -> ?DEFAULT_HEADERS
    end.

-spec header(Name :: binary(), Value :: binary()) -> true.
header(Name, Value) when is_binary(Name) andalso is_binary(Value) ->
    Headers = headers(),
    ets:insert(?TAB, { headers, maps:put(Name, Value, Headers) }).

-spec content() -> binary().
content() ->
    init(),
    case ets:lookup(?TAB, content) of
        [{ content, Content }] -> Content;
        _ -> ?DEFAULT_CONTENT
    end.

-spec content(Content :: binary()) -> true.
content(Content) when is_binary(Content) ->
    init(),
    ets:insert(?TAB, { content, Content }).
