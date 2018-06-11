-module(chuck_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    groups/0,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    random_test/1,
    random_with_category_test/1,
    categories_test/1
]).

all() ->
    [{group, api_methods}].

groups() ->
    [{api_methods,
        [shuffle, sequence], [
            random_test,
            random_with_category_test,
            categories_test
        ]
    }].

init_per_group(_Group, Config) ->
    application:ensure_all_started(hackney),
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_Test, Config) ->
    Server = bookish_spork:start_server(),
    bookish_spork:status(200),
    [{server, Server} | Config].

end_per_testcase(_Test, Config) ->
    Server = ?config(server, Config),
    bookish_spork:stop_server(Server).

random_test(_Config) ->
    bookish_spork:content(<<"{\"value\": \"Chuck Norris' favourite word: chunk.\"}">>),
    Result = chuck:random(),
    ?assertEqual(<<"Chuck Norris' favourite word: chunk.">>, Result),
    {ok, Request} = bookish_spork:receive_request(),
    ?assertEqual("/jokes/random", bookish_spork_request:uri(Request)).

random_with_category_test(_Config) ->
    bookish_spork:content(<<"{\"value\": \"Chuck Norris has an oscillating penis.\"}">>),
    Result = chuck:random("explicit"),
    ?assertEqual(<<"Chuck Norris has an oscillating penis.">>, Result),
    {ok, Request} = bookish_spork:receive_request(),
    ?assertEqual("/jokes/random?category=explicit", bookish_spork_request:uri(Request)).

categories_test(_Config) ->
    bookish_spork:content(<<"[\"explicit\", \"dev\", \"movie\"]">>),
    Result = chuck:categories(),
    ?assertEqual([<<"explicit">>, <<"dev">>, <<"movie">>], Result),
    {ok, Request} = bookish_spork:receive_request(),
    ?assertEqual("/jokes/categories", bookish_spork_request:uri(Request)).
