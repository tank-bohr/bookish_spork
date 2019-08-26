-module(bookish_spork_acceptor_sup).
-export([
    start_link/3,
    stop/1
]).

-behaviour(supervisor).
-export([
    init/1
]).

-define(CHILD(I, Args), #{
    id => I,
    start => {I, start_link, Args},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [I]
}).

-define(SUP_FLAGS, #{
    strategy => one_for_one,
    intensity => 5,
    period => 10
}).

-spec start_link(Server, Transport, ListenSocket) -> {ok, pid()} when
    Server :: pid(),
    Transport :: gen_tcp | bookish_spork_ssl,
    ListenSocket :: gen_tcp:socket() | ssl:sslsocket().
start_link(Server, Transport, ListenSocket) ->
    supervisor:start_link(?MODULE, [Server, Transport, ListenSocket]).

stop(Sup) ->
    ok = supervisor:terminate_child(Sup, bookish_spork_acceptor),
    ok = supervisor:delete_child(Sup, bookish_spork_acceptor),
    ok = gen_server:stop(Sup).

%% @private
init(Args) ->
    ChildSpec = ?CHILD(bookish_spork_acceptor, Args),
    {ok, {?SUP_FLAGS, [ChildSpec]}}.
