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

init_per_group(api_methods, Config) ->
    {ok, _} = application:ensure_all_started(hackney),
    {ok, _} = bookish_spork:start_server(),
    Config.

end_per_group(api_methods, _Config) ->
    ok = bookish_spork:stop_server().

init_per_testcase(random_test, Config) ->
    bookish_spork:stub_request(200,
        <<"{\"value\": \"Chuck Norris' favourite word: chunk.\"}">>),
    Config;
init_per_testcase(random_with_category_test, Config) ->
    bookish_spork:stub_request(200,
        <<"{\"value\": \"Chuck Norris has an oscillating penis.\"}">>),
    Config;
init_per_testcase(categories_test, Config) ->
    bookish_spork:stub_request(200, <<"[\"explicit\", \"dev\", \"movie\"]">>),
    Config.

end_per_testcase(_Test, _Config) ->
    ok.

random_test(_Config) ->
    ?assertEqual(<<"Chuck Norris' favourite word: chunk.">>, chuck:random()),
    {ok, Request} = bookish_spork:capture_request(),
    ?assertEqual("/jokes/random", bookish_spork_request:uri(Request)).

random_with_category_test(_Config) ->
    ?assertEqual(<<"Chuck Norris has an oscillating penis.">>, chuck:random("explicit")),
    {ok, Request} = bookish_spork:capture_request(),
    ?assertEqual("/jokes/random?category=explicit", bookish_spork_request:uri(Request)).

categories_test(_Config) ->
    ?assertEqual([<<"explicit">>, <<"dev">>, <<"movie">>], chuck:categories()),
    {ok, Request} = bookish_spork:capture_request(),
    ?assertEqual("/jokes/categories", bookish_spork_request:uri(Request)).
