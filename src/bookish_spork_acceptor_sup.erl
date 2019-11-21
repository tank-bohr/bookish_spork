-module(bookish_spork_acceptor_sup).
-export([start_link/2]).

-behaviour(supervisor).
-export([init/1]).

-define(SUP_FLAGS, #{
    strategy  => one_for_one,
    intensity => 5,
    period    => 10
}).

-spec start_link(Server, ListenSocket) -> {ok, pid()} when
    Server       :: pid(),
    ListenSocket :: bookish_spork_transport:listen().
start_link(Server, ListenSocket) ->
    supervisor:start_link(?MODULE, [Server, ListenSocket]).

%% @private
init(Args) ->
    {ok, {?SUP_FLAGS, [
        bookish_spork_acceptor:child_spec(Args)
    ]}}.
