%% @doc This is the main interface module
%%
%% It provides basic functions for using library
%%
%% @end

-module(bookish_spork).

-export([
    start_server/0,
    start_server/1,
    stop_server/0,
    stub_request/0,
    stub_request/1,
    stub_request/2,
    stub_request/3,
    capture_request/0
]).

-define(DEFAUT_PORT, 32002).
-define(RECEIVE_REQUEST_TIMEOUT_MILLIS, 1000).

-type http_status() :: non_neg_integer().

-spec start_server() -> {ok, pid()} | {error, Error :: term()}.
%% @equiv start_server(32002)
start_server() ->
    start_server(?DEFAUT_PORT).

-spec start_server(Port :: non_neg_integer()) -> {ok, pid()} | {error, Error :: term()}.
%% @doc starts http server on a particular port
start_server(Port) ->
    bookish_spork_server:start(Port).

-spec stop_server() -> ok.
%% @doc stops http server
stop_server() ->
    bookish_spork_server:stop().

-spec stub_request() -> ok.
%% @equiv
%% stub_request(204, #{
%%   <<"Server">> => <<"BookishSpork/0.0.1">>,
%%   <<"Date">> => <<"Sat, 28 Apr 2018 05:51:50 GMT">>
%% }, <<>>)
%% @end
stub_request() ->
    bookish_spork_server:respond_with(bookish_spork_response:new()).

-spec stub_request(function() | http_status()) -> ok.
%% @doc stub request with fun or particular status
%%
%% Fun must be {@type fun((bookish_spork_request:t()) -> bookish_spork_response:t())}
%%
%% Example:
%%
%% ```
%% stub_request(fun(Request) ->
%%     case bookish_spork_request:uri(Request) of
%%         "/bookish/spork" ->
%%             bookish_spork_response:new(200, <<"Hello">>);
%%         "/admin/sporks" ->
%%             bookish_spork_response:new(403, <<"It is not possible here">>)
%%     end
%% end)'''
%%
%% @end
stub_request(Fun) when is_function(Fun) ->
    bookish_spork_server:respond_with(Fun);
stub_request(Status) ->
    bookish_spork_server:respond_with(bookish_spork_response:new(Status)).

-spec stub_request(http_status(), ContentOrHeaders :: binary() | map()) -> ok.
%% @doc stub request with particular status and content/headers
stub_request(Status, ContentOrHeaders) ->
    bookish_spork_server:respond_with(bookish_spork_response:new(Status, ContentOrHeaders)).

-spec stub_request(http_status(), Headers :: map(), Content :: binary()) -> ok.
stub_request(Status, Headers, Content) ->
    bookish_spork_server:respond_with(bookish_spork_response:new(Status, Headers, Content)).

-spec capture_request() -> bookish_spork_request:bookish_spork_request().
capture_request() ->
    receive
        {bookish_spork, Request} ->
            {ok, Request};
        Unexpected ->
            {unexpected, Unexpected}
        after ?RECEIVE_REQUEST_TIMEOUT_MILLIS ->
            timeout
    end.
