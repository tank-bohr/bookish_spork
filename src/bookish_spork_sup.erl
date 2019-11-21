-module(bookish_spork_sup).
-export([
    start_acceptor_sup/2,
    stop/1
]).

-behaviour(supervisor).
-export([
    init/1
]).

-define(SUP_FLAGS, #{
    strategy  => one_for_one,
    intensity => 5,
    period    => 10
}).

-spec start_acceptor_sup(Server, ListenSocket) -> {ok, pid()} when
    Server       :: pid(),
    ListenSocket :: bookish_spork_transport:listen().
start_acceptor_sup(Server, ListenSocket) ->
    supervisor:start_link(?MODULE, [Server, ListenSocket]).

stop(Sup) ->
    ok = gen_server:stop(Sup).

%% @private
init(Args) ->
    {ok, {?SUP_FLAGS, [
        bookish_spork_acceptor:child_spec(Args)
    ]}}.
