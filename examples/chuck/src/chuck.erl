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
    Result = request("random"),
    maps:get(value, Result).

random(Category) ->
    Result = request("random", #{"category" => Category}),
    maps:get(value, Result).

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
