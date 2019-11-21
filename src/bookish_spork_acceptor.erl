-module(bookish_spork_acceptor).

-export([
    child_spec/1,
    start_link/2
]).

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-record(state, {
    server        :: pid(),
    listen_socket :: bookish_spork_transport:listen()
}).

child_spec(Args) ->
    #{
        id       => ?MODULE,
        start    => {?MODULE, start_link, Args},
        restart  => permanent,
        shutdown => 5000,
        type     => worker,
        modules  => [?MODULE]
    }.

start_link(Server, ListenSocket) ->
    gen_server:start_link(?MODULE, {Server, ListenSocket}, []).

%% @private
init({Server, ListenSocket}) ->
    accept(),
    {ok, #state{
        server = Server,
        listen_socket = ListenSocket
    }}.

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @private
handle_cast(accept, #state{listen_socket = ListenSocket, server = Server} = State) ->
    accept(ListenSocket, Server),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

accept() ->
    gen_server:cast(self(), accept).

-spec accept(ListenSocket, Server) -> ok when
    ListenSocket :: bookish_spork_transport:listen(),
    Server       :: pid().
accept(ListenSocket, Server) ->
    Transport = bookish_spork_transport:accept(ListenSocket),
    bookish_spork_handler:start_link(Server, Transport),
    accept().
