-module(bookish_spork_server).

-export([
    start/1,
    stop/0,
    respond_with/2,
    retrieve_request/0
]).

-export([
    store_request/2,
    response/1
]).

-behaviour(gen_server).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 32002).

-type response() :: bookish_spork_response:t() | bookish_spork:stub_request_fun().
-type request() :: bookish_spork_request:t().

-record(state, {
    response_queue = queue:new() :: queue:queue(response()),
    request_queue = queue:new() :: queue:queue(request()),
    acceptor_sup :: pid(),
    socket :: gen_tcp:socket() | ssl:sslsocket(),
    transport :: gen_tcp | bookish_spork_ssl
}).

-type state() :: #state{}.

-spec start(Options :: proplists:proplist()) -> {ok, pid()} | {error, Error :: term()}.
%% @doc starts server
start(Options) ->
    gen_server:start({local, ?SERVER}, ?MODULE, Options, []).

-spec stop() -> ok.
%% @doc stops server
stop() ->
    gen_server:stop(?SERVER).

-spec respond_with(Response :: response(), Times :: non_neg_integer()) -> ok.
respond_with(Response, Times) ->
    gen_server:call(?SERVER, {respond_with, Response, Times}).

-spec retrieve_request() -> {ok, Request :: request()} | {error, Error :: term()}.
retrieve_request() ->
    gen_server:call(?SERVER, request).

-spec store_request(Server :: pid(), Request :: request()) -> ok.
%% @doc Used by {@link bookish_spork_acceptor}
store_request(Server, Request) ->
    gen_server:call(Server, {request, Request}).

-spec response(Server :: pid()) -> {ok, response()} | {error, no_response}.
%% @doc Used by {@link bookish_spork_acceptor}
response(Server) ->
    gen_server:call(Server, response).

-spec init(Options :: proplists:proplist()) -> {ok, state()}.
%% @private
init(Options) ->
    Port = proplists:get_value(port, Options, ?DEFAULT_PORT),
    Transport = detect_transport(Options),
    {ok, ListenSocket} = Transport:listen(Port, [
        binary,
        {packet, http},
        {active, false},
        {reuseaddr, true}
    ]),
    {ok, AcceptorSup} = bookish_spork_acceptor_sup:start_link(self(), Transport, ListenSocket),
    {ok, #state{transport = Transport, socket = ListenSocket, acceptor_sup = AcceptorSup}}.

-spec handle_call(
    {respond_with, bookish_spork_response:response()},
    From :: {pid(), reference()},
    State :: state()
) -> {reply, {ok, pid()}, state()}.
%% @private
handle_call({respond_with, Response, Times}, _From, #state{response_queue = Q1} = State) ->
    Q2 = lists:foldl(fun(_, Q) -> queue:in(Response, Q) end, Q1, lists:seq(1, Times)),
    {reply, ok, State#state{response_queue = Q2}};
handle_call({request, Request}, _From, #state{request_queue = Q} = State) ->
    {reply, ok, State#state{request_queue = queue:in(Request, Q)}};
handle_call(response, _From, #state{response_queue = Q1} = State) ->
    case queue:out(Q1) of
        {{value, Val}, Q2} ->
            {reply, {ok, Val}, State#state{response_queue = Q2}};
        {empty, _} ->
            {reply, {error, no_response}, State}
    end;
handle_call(request, _From, #state{request_queue = Q1} = State) ->
    case queue:out(Q1) of
        {{value, Val}, Q2} ->
            {reply, {ok, Val}, State#state{request_queue = Q2}};
        {empty, _} ->
            {reply, {error, no_request}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(Message :: term(), State :: state()) -> {noreply, state()}.
%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(Info :: term(), State :: state()) -> {noreply, state()}.
%% @private
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(Reason :: term(), State :: state()) -> ok.
%% @private
terminate(_Reason, State) ->
    #state{
        transport = Transport,
        socket = ListenSocket,
        acceptor_sup = AcceptorSup
    } = State,
    ok = bookish_spork_acceptor_sup:stop(AcceptorSup),
    ok = Transport:close(ListenSocket).

%% @private
detect_transport(Options) ->
    case proplists:get_bool(ssl, Options) of
        true ->
            bookish_spork_ssl;
        _ ->
            gen_tcp
    end.
