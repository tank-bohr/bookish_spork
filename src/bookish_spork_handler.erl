-module(bookish_spork_handler).

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
    server    :: pid(),
    transport :: bookish_spork_transport:t()
}).

-type state() :: #state{}.

child_spec(Args) ->
    #{
        id       => ?MODULE,
        start    => {?MODULE, start_link, Args},
        restart  => temporary,
        shutdown => 5000,
        type     => worker,
        modules  => [?MODULE]
    }.

-spec start_link(Server, Transport) -> {ok, pid()} when
    Server       :: pid(),
    Transport    :: bookish_spork_transport:t().
start_link(Server, Transport) ->
    Args = {Server, Transport},
    gen_server:start_link(?MODULE, Args, []).

-spec init({Server, Transport}) -> {ok, State} when
    State     :: state(),
    Server    :: pid(),
    Transport :: bookish_spork_transport:t().
%% @private
init({Server, Transport}) ->
    handle_connection(),
    {ok, #state{
        server    = Server,
        transport = Transport
    }}.

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @private
handle_cast(handle_connection, State) ->
    handle_connection(State);
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

-spec handle_connection() -> ok.
%% @private
handle_connection() ->
    gen_server:cast(self(), handle_connection).

-spec handle_connection(state()) -> {noreply, state()} | {stop, normal, state()}.
%% @private
handle_connection(#state{transport = Transport, server = Server} = State) ->
    case receive_request(State) of
        {ok, Request} ->
            ok = bookish_spork_server:store_request(Server, Request),
            case bookish_spork_server:response(Server) of
                {ok, Response} ->
                    reply(Transport, Response, Request),
                    complete_connection(State, Request);
                {error, no_response} ->
                    bookish_spork_transport:close(Transport),
                    {stop, normal, State}

            end;
        socket_closed ->
            {stop, normal, State}
    end.

-spec complete_connection(State, Request) -> {noreply, State} | {stop, normal, State} when
    State   :: state(),
    Request :: bookish_spork_request:t().
%% @private
complete_connection(#state{transport = Transport} = State, Request) ->
    case bookish_spork_request:is_keepalive(Request) of
        true ->
            handle_connection(),
            {noreply, State};
        false ->
            bookish_spork_transport:shutdown(Transport),
            {stop, normal, State}
    end.

-spec receive_request(State :: state()) -> Result when
    Result :: {ok, Request} | socket_closed,
    Request :: bookish_spork_request:t().
%% @private
receive_request(#state{transport = Transport}) ->
    Request = bookish_spork_request:from_transport(Transport),
    read_from_socket(Transport, Request).

-spec read_from_socket(Transport, RequestIn) -> Result when
    Transport  :: bookish_spork_transport:t(),
    RequestIn  :: bookish_spork_request:t(),
    Result     :: {ok, RequestOut} | socket_closed,
    RequestOut :: bookish_spork_request:t().
%% @private
read_from_socket(Transport, RequestIn) ->
    case bookish_spork_transport:recv(Transport) of
        {ok, {http_request, Method, {abs_path, Uri}, Version}} ->
            RequestOut = bookish_spork_request:request_line(RequestIn, Method, Uri, Version),
            read_from_socket(Transport, RequestOut);
        {ok, {http_header, _, Header, _, Value}} ->
            RequestOut = bookish_spork_request:add_header(RequestIn, Header, Value),
            read_from_socket(Transport, RequestOut);
        {ok, http_eoh} ->
            Body = read_body(Transport, bookish_spork_request:content_length(RequestIn)),
            RequestOut = bookish_spork_request:body(RequestIn, Body),
            {ok, RequestOut};
        {ok, {http_error, HttpError}} ->
            erlang:error({http_error, HttpError}, [Transport, RequestIn]);
        {error, closed} ->
            socket_closed;
        {error, enotconn} ->
            socket_closed
    end.

-spec read_body(Transport, ContentLength) -> Body when
    Transport     :: bookish_spork_transport:t(),
    ContentLength :: non_neg_integer(),
    Body          :: binary().
%% @private
read_body(Transport, ContentLength) ->
    bookish_spork_transport:read_raw(Transport, ContentLength).

-spec reply(Transport, Response, Request) -> ok when
    Transport :: bookish_spork_transport:t(),
    Response  :: bookish_spork:stub_request_fun() | bookish_spork_response:t(),
    Request   :: bookish_spork_request:t().
%% @private
reply(Transport, ResponseFun, Request) when is_function(ResponseFun) ->
    Response = ResponseFun(Request),
    reply(Transport, bookish_spork_response:new(Response), Request);
reply(Transport, Response, _Request) ->
    String = bookish_spork_response:write_str(Response, calendar:universal_time()),
    bookish_spork_transport:send(Transport, [String]).
