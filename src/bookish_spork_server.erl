-module(bookish_spork_server).

-export([
    start/1,
    stop/0,
    respond_with/1
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

-type response() :: bookish_spork_response:response() | function().

-record(state, {
    response_queue = queue:new() :: queue:queue({response(), pid()}),
    acceptor :: pid(),
    socket :: gen_tcp:socket()
}).

-type state() :: #state{}.

-spec start(Port :: non_neg_integer()) -> {ok, pid()} | {error, Error :: term()}.
%% @doc starts server
start(Port) ->
    gen_server:start({local, ?SERVER}, ?MODULE, Port, []).

-spec stop() -> ok.
%% @doc stops server
stop() ->
    gen_server:stop(?SERVER).

-spec respond_with(Response :: response()) -> ok.
respond_with(Response) ->
    gen_server:call(?SERVER, {respond_with, Response}).

-spec response() -> {Response :: response(), Receiver :: pid()}.
%% @private
response() ->
    gen_server:call(?SERVER, response).

-spec init(Port :: non_neg_integer()) -> {ok, state()}.
%% @private
init(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [
        binary,
        {packet, http},
        {active, false},
        {reuseaddr, true}
    ]),
    {ok, Acceptor} = accept(ListenSocket),
    {ok, #state{socket = ListenSocket, acceptor = Acceptor}}.

-spec handle_call(
    {respond_with, bookish_spork_response:response()},
    From :: {pid(), reference()},
    State :: state()
) -> {reply, {ok, pid()}, state()}.
%% @private
handle_call({respond_with, Response}, {Receiver, _Ref}, #state{response_queue = Q1} = State) ->
    Q2 = queue:in({Response, Receiver}, Q1),
    {reply, ok, State#state{response_queue = Q2}};
handle_call(response, _From, #state{response_queue = Q1} = State) ->
    {{value, Val}, Q2} = queue:out(Q1),
    {reply, Val, State#state{response_queue = Q2}};
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
terminate(_Reason, #state{socket = ListenSocket, acceptor = Acceptor}) ->
    exit(Acceptor, kill),
    gen_tcp:close(ListenSocket).

%% @private
accept(ListenSocket) ->
    AcceptorPid = spawn_link(fun AcceptorFun() ->
        {ok, Socket} = gen_tcp:accept(ListenSocket),
        ok = handle_connection(Socket),
        AcceptorFun()
    end),
    {ok, AcceptorPid}.

%% @private
handle_connection(Socket) ->
    case receive_request(Socket) of
        {ok, Request} ->
            {Response, Receiver} = response(),
            Receiver ! {bookish_spork, Request},
            ok = reply(Socket, Response, Request),
            complete_connection(Socket, Request);
        socket_closed ->
            ok
    end.

%% @private
complete_connection(Socket, Request) ->
    case bookish_spork_request:is_keepalive(Request) of
        true ->
            handle_connection(Socket);
        false ->
            gen_tcp:shutdown(Socket, read_write)
    end.

%% @private
receive_request(Socket) ->
    receive_request(Socket, bookish_spork_request:new()).

%% @private
receive_request(Socket, RequestIn) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, {http_request, Method, {abs_path, Uri}, Version}} ->
            RequestOut = bookish_spork_request:request_line(RequestIn, Method, Uri, Version),
            receive_request(Socket, RequestOut);
        {ok, {http_header, _, Header, _, Value}} ->
            RequestOut = bookish_spork_request:add_header(RequestIn, Header, Value),
            receive_request(Socket, RequestOut);
        {ok, http_eoh} ->
            Body = read_body(Socket, bookish_spork_request:content_length(RequestIn)),
            RequestOut = bookish_spork_request:body(RequestIn, Body),
            {ok, RequestOut};
        {error, closed} ->
            socket_closed
    end.

%% @private
read_body(_Socket, 0) ->
    <<>>;
read_body(Socket, ContentLength) ->
    inet:setopts(Socket, [{packet, raw}]),
    {ok, Body} = gen_tcp:recv(Socket, ContentLength),
    inet:setopts(Socket, [{packet, http}]),
    Body.

%% @private
reply(Socket, ResponseFun, Request) when is_function(ResponseFun) ->
    Response = ResponseFun(Request),
    reply(Socket, Response, Request);
reply(Socket, Response, _Request) ->
    String = bookish_spork_response:write_str(Response, calendar:universal_time()),
    gen_tcp:send(Socket, [String]).
