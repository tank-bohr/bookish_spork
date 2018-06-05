-module(bookish_spork_setings).

-export([
    init/0,
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

init() ->
    case ets:info(?TAB) of
        undefined ->
            ets:new(?TAB, [set, public, named_table]);
        _ ->
            true
    end.

status() ->
    init(),
    case ets:lookup(?TAB, status) of
        [{ status, Status }] -> Status;
        _ -> ?DEFAULT_STATUS
    end.

status(Status) when Status >= 100 andalso Status < 600 ->
    init(),
    ets:insert(?TAB, { status, Status }).

headers() ->
    init(),
    case ets:lookup(?TAB, headers) of
        [{ headers, Headers }] -> Headers;
        _ -> ?DEFAULT_HEADERS
    end.

header(Name, Value) when is_binary(Name) andalso is_binary(Value) ->
    Headers = headers(),
    ets:insert(?TAB, { headers, maps:put(Name, Value, Headers) }).

content() ->
    init(),
    case ets:lookup(?TAB, content) of
        [{ content, Content }] -> Content;
        _ -> ?DEFAULT_CONTENT
    end.

content(Content) when is_binary(Content) ->
    init(),
    ets:insert(?TAB, { content, Content }).
