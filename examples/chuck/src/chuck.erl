-module(chuck).

-export([
    random/0,
    random/1,
    categories/0
]).

-ifndef(TEST).
-define(PROTO, "https").
-define(HOST, "api.chucknorris.io").
-else.
-define(PROTO, "http").
-define(HOST, "localhost:5432").
-endif.

random() ->
    maps:get(value, request("random")).

random(Category) ->
    maps:get(value, request("random", #{"category" => Category})).

categories() ->
    request("categories").

request(Resource) ->
    request(Resource, #{}).

request(Resource, Params) ->
    Url = url(Resource, Params),
    {ok, 200, _RespHeaders, ClientRef} = hackney:get(Url, [], <<>>, []),
    {ok, Body} = hackney:body(ClientRef),
    decode(Body).

decode(Json) ->
    jsx:decode(Json, [return_maps, {labels, atom}]).

url(Resource, Params) ->
    QueryString = query_string(Params),
    iolist_to_binary([?PROTO, "://", ?HOST, "/jokes/", Resource, QueryString]).

query_string(Params) when map_size(Params) =:= 0 ->
    <<>>;
query_string(Params) ->
    QueryString = iolist_to_binary(lists:join("&", maps:fold(fun(K, V, Acc) ->
        Pair = [K, "=", V],
        [Pair | Acc]
    end, [], Params))),
    <<"?", QueryString/binary>>.
