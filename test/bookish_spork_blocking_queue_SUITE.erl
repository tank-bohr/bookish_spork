-module (bookish_spork_blocking_queue_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    all/0
]).

-export([
    basic_test/1,
    dead_process_awaiting_test/1,
    wait_for_timer_test/1,
    multiple_waiters_test/1,
    double_wait_test/1
]).

all() ->
    [basic_test, dead_process_awaiting_test, wait_for_timer_test, multiple_waiters_test, double_wait_test].

basic_test(_Config) ->
    {ok, Pid} = bookish_spork_blocking_queue:start_link(),
    {FetcherPid, Ref} = spawn_monitor(fun() ->
        {ok, Value} = bookish_spork_blocking_queue:out(Pid, infinity),
        ?assertEqual(pants, Value)
    end),
    ct:sleep(500),
    ok = bookish_spork_blocking_queue:in(Pid, pants),
    ?assertEqual({error, timeout}, bookish_spork_blocking_queue:out(Pid, 500)),
    ok = receive
        {'DOWN', Ref, process, FetcherPid, normal} -> ok
        after 500                                  -> timeout
    end.

dead_process_awaiting_test(_Config) ->
    {ok, Pid} = bookish_spork_blocking_queue:start_link(),
    {DeadMan, Ref1} = spawn_monitor(fun() -> bookish_spork_blocking_queue:out(Pid) end),
    ct:sleep(500),
    exit(DeadMan, sucker_punch),
    sucker_punch = receive
        {'DOWN', Ref1, process, DeadMan, Reason1} -> Reason1
    end,
    {Fetcher, Ref2} = spawn_monitor(fun() ->
        {ok, Value} = bookish_spork_blocking_queue:out(Pid),
        ?assertEqual(pants, Value)
    end),
    ok = bookish_spork_blocking_queue:in(Pid, pants),
    normal = receive
        {'DOWN', Ref2, process, Fetcher, Reason2} -> Reason2
    end.

wait_for_timer_test(_Config) ->
    {ok, Pid} = bookish_spork_blocking_queue:start_link(),
    {Client, Ref} = spawn_monitor(fun() ->
        {ok, Value} = bookish_spork_blocking_queue:out(Pid, 1000),
        ?assertEqual(pants, Value)
    end),
    ct:sleep(500),
    ok = bookish_spork_blocking_queue:in(Pid, pants),
    ct:sleep(500),
    ok = receive
        {'DOWN', Ref, process, Client, normal} -> ok
    end.

multiple_waiters_test(_Config) ->
    {ok, Pid} = bookish_spork_blocking_queue:start_link(),
    {FetcherPid, Ref} = spawn_monitor(fun() ->
        {ok, Value} = bookish_spork_blocking_queue:out(Pid, 1000),
        ?assertEqual(pants, Value)
    end),
    {error, timeout} = bookish_spork_blocking_queue:out(Pid, 1),
    ct:sleep(500),
    ok = bookish_spork_blocking_queue:in(Pid, pants),
    ok = receive
        {'DOWN', Ref, process, FetcherPid, normal} -> ok
    after 0 ->
        erlang:error(timeout)
    end.

double_wait_test(_Config) ->
    {ok, Pid} = bookish_spork_blocking_queue:start_link(),
    {error, timeout} = bookish_spork_blocking_queue:out(Pid, 0),
    ok = bookish_spork_blocking_queue:in(Pid, pants),
    ok = receive
        Message -> erlang:error({unexpected_message, Message})
        after 0 -> ok
    end,
    {ok, pants} = bookish_spork_blocking_queue:out(Pid).
