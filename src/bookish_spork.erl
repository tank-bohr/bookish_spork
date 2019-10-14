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
    capture_request/0,
    capture_request/1
]).

-define(DEFAULT_STATUS, 204).
-define(DEFAULT_HEADERS, #{}).
-define(DEFAULT_CONTENT, <<>>).

-type stub_request_fun() :: fun((bookish_spork_request:t()) -> bookish_spork_response:response()).

-export_type([
    stub_request_fun/0
]).

-spec start_server() -> {ok, pid()} | {error, Error :: term()}.
%% @equiv start_server(32002)
start_server() ->
    start_server([]).

-spec start_server(Options :: proplists:proplist()) -> {ok, pid()} | {error, Error :: term()}.
%% @doc starts http server on a particular port
start_server(Options) ->
    bookish_spork_server:start(Options).

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
    stub_request([?DEFAULT_STATUS, ?DEFAULT_HEADERS, ?DEFAULT_CONTENT]).

-spec stub_request(stub_request_fun() | bookish_spork_response:response()) -> ok.
%% @equiv stub_request(Response, 1)
stub_request(Response) ->
    stub_request(Response, 1).

-spec stub_request(
    Response :: stub_request_fun() | bookish_spork_response:response(),
    Times :: non_neg_integer()
) -> ok.
%% @doc stub request with fun or particular response
%%
%% `Response' can be
%% <ul>
%%   <li>either {@type fun((bookish_spork_request:t()) -> bookish_spork_response:response())}</li>
%%   <li>or response data structure {@type bookish_spork_response:response()}</li>
%% </ul>
%%
%% Example:
%%
%% ```
%% {@module}:stub_request(fun(Request) ->
%%     case bookish_spork_request:uri(Request) of
%%         "/bookish/spork" ->
%%             [200, [], <<"Hello">>];
%%         "/admin/sporks" ->
%%             [403, [], <<"It is not possible here">>]
%%     end
%% end, _Times = 20)'''
%%
%% @end
stub_request(Fun, Times) when is_function(Fun) ->
    bookish_spork_server:respond_with(Fun, Times);
stub_request(Response, Times) ->
    bookish_spork_server:respond_with(bookish_spork_response:new(Response), Times).

-spec capture_request() -> {ok, Request :: bookish_spork_request:t()} | {error, Error :: term()}.
capture_request() ->
    capture_request(_Timeout = 0).

-spec capture_request(Timeout) -> {ok, Request} | {error, Error} when
    Error :: term(),
    Request :: bookish_spork_request:t(),
    Timeout :: non_neg_integer().
capture_request(Timeout) ->
    bookish_spork_server:retrieve_request(Timeout).
