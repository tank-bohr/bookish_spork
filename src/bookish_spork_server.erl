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

-record(state, {
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

-spec respond_with(Response :: bookish_spork_response:response()) -> {ok, Acceptor :: pid()}.
respond_with(Response) ->
    gen_server:call(?SERVER, {respond_with, Response}).

-spec init(Port :: non_neg_integer()) -> {ok, state()}.
%% @private
init(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [
        binary,
        {packet, http},
        {active, false},
        {reuseaddr, true}
    ]),
    {ok, #state{socket = ListenSocket}}.

-spec handle_call(
    {respond_with, bookish_spork_response:response()},
    From :: {pid(), reference()},
    State :: state()
) -> {reply, {ok, pid()}, state()}.
%% @private
handle_call({respond_with, Response}, {Receiver, _Ref}, #state{socket = ListenSocket} = State) ->
    {reply, accept(ListenSocket, Response, Receiver), State};
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
terminate(_Reason, #state{socket = ListenSocket}) ->
    gen_tcp:close(ListenSocket).

%% @private
accept(ListenSocket, Response, Receiver) ->
    Pid = spawn_link(fun() ->
        {ok, Socket} = gen_tcp:accept(ListenSocket),
        Request = receive_request(Socket),
        Receiver ! {bookish_spork, Request},
        ok = reply(Socket, Response),
        ok = gen_tcp:shutdown(Socket, read_write)
    end),
    {ok, Pid}.

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
            bookish_spork_request:body(RequestIn, Body)
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
reply(Socket, Response) ->
    String = bookish_spork_response:write_str(Response, calendar:universal_time()),
    gen_tcp:send(Socket, [String]).
