-module(bookish_spork_setings).

-export([
    init/0,
    header/2,
    headers/0,
    content/0,
    content/1,
    clear_heders/0,
    clear_content/0
]).

-define(TAB, ?MODULE).
-define(DEFAULT_HEADERS, #{}).

init() ->
    case ets:info(?TAB) of
        undefined ->
            ets:new(?TAB, [set, public, named_table]);
        _ ->
            true
    end.

header(Name, Value) ->
    Headers = headers(),
    ets:insert(?TAB, maps:put(Name, Value, Headers)).

headers() ->
    init(),
    case ets:lookup(?TAB, headers) of
        [Headers] -> Headers;
        _ -> ?DEFAULT_HEADERS
    end.

content() ->
    <<>>.

content(Content) ->
    ok.

clear_heders() ->
    ok.

clear_content() ->
    ok.
