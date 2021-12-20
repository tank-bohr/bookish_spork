-module(bookish_spork_blocking_queue).

-export([
    start_link/0,
    in/2,
    out/1,
    out/2
]).

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-type element() :: any().

-record(state, {
    waiting = queue:new() :: queue:queue(element()),
    elements = queue:new() :: queue:queue(element())
}).

-define(DEFAULT_TIMEOUT_MILLIS, 5000).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

-spec in(Pid :: pid(), Element :: element()) -> ok.
in(Pid, Element) ->
    gen_server:call(Pid, {enqueue, Element}).

-spec out(Pid :: pid()) -> {ok, element()} | {error, timeout}.
out(Pid) ->
    out(Pid, ?DEFAULT_TIMEOUT_MILLIS).

-spec out(Pid, Timeout) -> {ok, element()} | {error, timeout} when
    Pid :: pid(),
    Timeout :: timeout().
out(Pid, Timeout) ->
    gen_server:call(Pid, {dequeue, Timeout}, infinity).


%% @private
init([]) ->
    {ok, #state{}}.

%% @private
handle_call({dequeue, Timeout}, From, #state{waiting = Waiting} = State) ->
    case queue:out(State#state.elements) of
        {empty, _} ->
            %% no elements therefore we wait
            {noreply, State#state{waiting = enqueue_client(From, Waiting, Timeout)}};
        {{value, Element}, RemainingElements} ->
            %% return the next element
            {reply, {ok, Element}, State#state{elements = RemainingElements}}
    end;
handle_call({enqueue, Element}, From, #state{elements = Elements} = State) ->
    case queue:out(State#state.waiting) of
        {empty, _} ->
            %% no waiters we just save the item
            {reply, ok, State#state{elements = queue:in(Element, Elements)}};
        {{value, {Pid, _} = Client}, Remaining} ->
            %% feed next waiter
            case is_process_alive(Pid) of
                false ->
                    handle_call({enqueue, Element}, From, State#state{waiting = Remaining});
                true ->
                    gen_server:reply(Client, {ok, Element}),
                    {reply, ok, State#state{waiting = Remaining}}
            end
    end.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({timeout, Client}, #state{waiting = Waiting} = State) ->
    Remaining = case queue:member(Client, Waiting) of
        true ->
            gen_server:reply(Client, {error, timeout}),
            queue:delete(Client, Waiting);
        false ->
            %% Already replied
            Waiting
    end,
    {noreply, State#state{waiting = Remaining}}.

enqueue_client(Client, Waiting, 0) ->
    gen_server:reply(Client, {error, timeout}),
    Waiting;
enqueue_client(Client, Waiting, Timeout) when is_atom(Timeout) ->
    queue:in(Client, Waiting);
enqueue_client(Client, Waiting, Timeout) when is_integer(Timeout) andalso Timeout > 0 ->
    {ok, _} = timer:send_after(Timeout, {timeout, Client}),
    queue:in(Client, Waiting).
