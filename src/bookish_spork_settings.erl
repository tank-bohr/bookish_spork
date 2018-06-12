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
    lookup(status, ?DEFAULT_STATUS).

-spec status(Status :: non_neg_integer()) -> true.
status(Status) when Status >= 100 andalso Status < 600 ->
    init(),
    ets:insert(?TAB, { status, Status }).

-spec headers() -> map().
headers() ->
    lookup(headers, ?DEFAULT_HEADERS).

-spec header(Name :: binary(), Value :: binary()) -> true.
header(Name, Value) when is_binary(Name) andalso is_binary(Value) ->
    init(),
    Headers = headers(),
    ets:insert(?TAB, { headers, maps:put(Name, Value, Headers) }).

-spec content() -> binary().
content() ->
    lookup(content, ?DEFAULT_CONTENT).

-spec content(Content :: binary()) -> true.
content(Content) when is_binary(Content) ->
    init(),
    ets:insert(?TAB, { content, Content }).

lookup(Name, Default) ->
    Found = ets:info(?TAB) =/= undefined andalso ets:lookup(?TAB, Name),
    case Found of
        [{ Name, Value }] ->
            Value;
        _ ->
            Default
    end.
