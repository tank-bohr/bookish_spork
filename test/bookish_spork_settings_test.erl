-module(bookish_spork_settings_test).
-include_lib("eunit/include/eunit.hrl").

-define(T, bookish_spork_setings).

clear() ->
    case ets:info(?T) of
        undefined -> true;
        _ -> ets:delete(?T)
    end.

init_test() ->
    clear(),
    ?T:init(),
    ?T:init(),
    Info = ets:info(?T),
    ?assert(lists:any(fun(_) -> true end, Info), "Can be called twice idempotent").

status_test() ->
    clear(),
    ?T:status(302),
    ?assertEqual(302, ?T:status()).

headers_test() ->
    clear(),
    ?T:header(<<"x-lol">>, <<"kjk">>),
    ?assertMatch(#{<<"x-lol">> := <<"kjk">>}, ?T:headers()).

content_test() ->
    clear(),
    ?T:content(<<"Hello">>),
    ?assertEqual(<<"Hello">>, ?T:content()).
