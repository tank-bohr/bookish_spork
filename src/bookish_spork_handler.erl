-module(bookish_spork_handler).

-export([
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
    server    :: pid(),
    transport :: bookish_spork_transport:t(),
    request   :: undefined | bookish_spork_request:t(),
    response  :: undefined | bookish_spork_response:t() | bookish_spork:stub_request_fun()
}).

-type state() :: #state{}.

-spec start_link(Server, Transport) -> {ok, pid()} when
    Server       :: pid(),
    Transport    :: bookish_spork_transport:t().
start_link(Server, Transport) ->
    gen_server:start_link(?MODULE, {Server, Transport}, []).

-spec init({Server, Transport}) -> {ok, State} when
    State     :: state(),
    Server    :: pid(),
    Transport :: bookish_spork_transport:t().
%% @private
init({Server, Transport}) ->
    keepalive_loop(),
    {ok, #state{
        server    = Server,
        transport = Transport
    }}.

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @private
handle_cast(keepalive_loop, State) ->
    keepalive_loop(State);
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

-spec keepalive_loop() -> ok.
%% @private
keepalive_loop() ->
    gen_server:cast(self(), keepalive_loop).

-spec keepalive_loop(state()) -> {noreply, state()} | {stop, normal, state()}.
%% @private
keepalive_loop(State) ->
    reduce_while(State, [
        fun reset/1,
        fun receive_request/1,
        fun store_request/1,
        fun pick_response/1,
        fun reply/1,
        fun complete_connection/1
    ]).

reset(#state{server = Server, transport = Transport}) ->
    Request = bookish_spork_request:from_transport(Transport),
    {cont, #state{
        server    = Server,
        request   = Request,
        transport = Transport
    }}.

-spec receive_request(state()) -> {cont, state()} | {halt, normal}.
%% @private
receive_request(#state{transport = Transport, request = RequestIn} = State) ->
    case bookish_spork_transport:recv(Transport) of
        {ok, {http_request, Method, {abs_path, Uri}, Version}} ->
            RequestOut = bookish_spork_request:request_line(RequestIn, Method, Uri, Version),
            ?FUNCTION_NAME(State#state{request = RequestOut});
        {ok, {http_header, _, Header, _, Value}} ->
            RequestOut = bookish_spork_request:add_header(RequestIn, Header, Value),
            ?FUNCTION_NAME(State#state{request = RequestOut});
        {ok, http_eoh} ->
            ContentLength = bookish_spork_request:content_length(RequestIn),
            Body = bookish_spork_transport:read_raw(Transport, ContentLength),
            RequestOut = bookish_spork_request:body(RequestIn, Body),
            {cont, State#state{request = RequestOut}};
        {ok, {http_error, HttpError}} ->
            erlang:error({http_error, HttpError}, [Transport, RequestIn]);
        {error, closed} ->
            {halt, normal};
        {error, enotconn} ->
            {halt, normal}
    end.

store_request(#state{server = Server, request = Request} = State) ->
    bookish_spork_server:store_request(Server, Request),
    {cont, State}.

pick_response(#state{server = Server, transport = Transport} = State) ->
    case bookish_spork_server:response(Server) of
        {ok, Response} ->
            {cont, State#state{response = Response}};
        {error, no_response} ->
            bookish_spork_transport:close(Transport),
            {halt, normal}
    end.

-spec reply(state()) -> {cont, state()}.
%% @private
reply(State = #state{request = Request, response = ResponseFun}) when is_function(ResponseFun) ->
    Response = ResponseFun(Request),
    ?FUNCTION_NAME(State#state{response = bookish_spork_response:new(Response)});
reply(State = #state{transport = Transport, response = Response}) ->
    String = bookish_spork_response:write_str(Response, calendar:universal_time()),
    bookish_spork_transport:send(Transport, [String]),
    {cont, State}.

-spec complete_connection(state()) -> {cont, state()} | {halt, normal}.
%% @private
complete_connection(State = #state{transport = Transport, request = Request}) ->
    case bookish_spork_request:is_keepalive(Request) of
        true ->
            keepalive_loop(),
            {cont, State};
        false ->
            bookish_spork_transport:shutdown(Transport),
            {halt, normal}
    end.

reduce_while(State, []) ->
    {noreply, State};
reduce_while(State, [Fun|Rest]) ->
    case Fun(State) of
        {cont, NewState} ->
            reduce_while(NewState, Rest);
        {halt, Reason} ->
            {stop, Reason, State}
    end.
