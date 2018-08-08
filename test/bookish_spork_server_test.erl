-module(bookish_spork_server_test).
-include_lib("eunit/include/eunit.hrl").

useless_test_() ->
    State = {state},
    [?_assertEqual({reply, {error, unknown_call}, State},
        bookish_spork_server:handle_call(unknown, {self(), make_ref()}, State)),
    ?_assertEqual({noreply, State}, bookish_spork_server:handle_cast(unknown, State)),
    ?_assertEqual({noreply, State}, bookish_spork_server:handle_info(unknown, State))].
