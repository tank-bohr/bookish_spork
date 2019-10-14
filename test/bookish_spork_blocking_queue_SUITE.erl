-module (bookish_spork_blocking_queue_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    all/0
]).

-export([
    basic_test/1
]).

all() ->
    [basic_test].

basic_test(_Config) ->
    {ok, Pid} = bookish_spork_blocking_queue:start_link(),
    {FetcherPid, Ref} = spawn_monitor(fun() ->
        {ok, Value} = bookish_spork_blocking_queue:out(Pid),
        ?assertEqual(pants, Value)
    end),
    ct:sleep(500),
    ok = bookish_spork_blocking_queue:in(Pid, pants),
    ?assertEqual({error, timeout}, bookish_spork_blocking_queue:out(Pid, 500)),
    ok = receive
        {'DOWN', Ref, process, FetcherPid, normal} -> ok
        after 500                                  -> timeout
    end.
