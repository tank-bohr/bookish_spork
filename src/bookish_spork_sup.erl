-module(bookish_spork_sup).
-export([
    start_acceptor_sup/3,
    start_handler_sup/2,
    start_handler/4,
    stop/1
]).

-behaviour(supervisor).
-export([
    init/1
]).

-define(CHILD(I, Args), #{
    id       => I,
    start    => {I, start_link, Args},
    restart  => permanent,
    shutdown => 5000,
    type     => worker,
    modules  => [I]
}).

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

-spec start_acceptor_sup(Server, Transport, ListenSocket) -> {ok, pid()} when
    Server       :: pid(),
    Transport    :: gen_tcp | bookish_spork_ssl,
    ListenSocket :: term().
start_acceptor_sup(Server, Transport, ListenSocket) ->
    supervisor:start_link(?MODULE, {acceptor, [Server, Transport, ListenSocket]}).

start_handler_sup(Server, Transport) ->
    supervisor:start_link(?MODULE, {handler, [Server, Transport]}).

start_handler(Sup, Socket, TlsExt, ConnectionId) ->
    supervisor:start_child(Sup, [Socket, TlsExt, ConnectionId]).

stop(Sup) ->
    ok = gen_server:stop(Sup).

%% @private
init({acceptor, Args}) ->
    {ok, {?ACCEPTOR_SUP_FLAGS, [?CHILD(bookish_spork_acceptor, Args)]}};
init({handler, Args}) ->
    {ok, {?HANDLER_SUP_FLAGS, [?CHILD(bookish_spork_handler, Args)]}}.
