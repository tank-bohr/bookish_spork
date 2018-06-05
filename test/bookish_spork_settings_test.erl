-module(bookish_spork_settings_test).
-include_lib("eunit/include/eunit.hrl").

-define(T, bookish_spork_setings).

delete_ets() ->
    case ets:info(?T) of
        undefined -> true;
        _ -> ets:delete(?T)
    end.

without_ets_test_() ->
    {
        setup,
        fun delete_ets/0,
        [
            fun init_twice/0
        ]
    }.

init_twice() ->
    ?T:init(),
    ?T:init(),
    Info = ets:info(?T),
    ?_assert(lists:any(fun(_) -> true end, Info)).
