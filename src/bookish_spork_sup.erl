-module(bookish_spork_sup).
-export([
    start_acceptor_sup/2,
    start_handler_sup/1,
    start_handler/2,
    stop/1
]).

-behaviour(supervisor).
-export([
    init/1
]).

-define(ACCEPTOR_SUP_FLAGS, #{
    strategy  => one_for_one,
    intensity => 5,
    period    => 10
}).

-define(HANDLER_SUP_FLAGS, #{
    strategy  => simple_one_for_one,
    intensity => 5,
    period    => 10
}).

-spec start_acceptor_sup(Server, ListenSocket) -> {ok, pid()} when
    Server       :: pid(),
    ListenSocket :: bookish_spork_transport:listen().
start_acceptor_sup(Server, ListenSocket) ->
    supervisor:start_link(?MODULE, {acceptor, [Server, ListenSocket]}).

start_handler_sup(Server) ->
    supervisor:start_link(?MODULE, {handler, [Server]}).

start_handler(Sup, Transport) ->
    supervisor:start_child(Sup, [Transport]).

stop(Sup) ->
    ok = gen_server:stop(Sup).

%% @private
init({acceptor, Args}) ->
    {ok, {?ACCEPTOR_SUP_FLAGS, [bookish_spork_acceptor:child_spec(Args)]}};
init({handler, Args}) ->
    {ok, {?HANDLER_SUP_FLAGS, [bookish_spork_handler:child_spec(Args)]}}.
