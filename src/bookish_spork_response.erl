-module (bookish_spork_response).

-export([
    new/0,
    new/1,
    new/2,
    new/3,
    write_str/1
]).

-define(SP, " ").
-define(CRLF, "\r\n").
-define(COLON, ": ").
-define(HTTP11, "HTTP/1.1").
-define(DEFAULT_SERVER, <<"BookishSpork/0.0.1">>).

-define(DEFAULT_STATUS, 204).
-define(DEFAULT_HEADERS, #{}).
-define(DEFAULT_CONTENT, <<>>).

-record(response, {
    status  = ?DEFAULT_STATUS  :: non_neg_integer(),
    headers = ?DEFAULT_HEADERS :: map(),
    content = ?DEFAULT_CONTENT :: binary()
}).

-opaque response() :: #response{}.

-export_type([
    response/0
]).

-spec new() -> response().
new() ->
    #response{}.

-spec new(Status :: non_neg_integer()) -> response().
new(Status) ->
    #response{ status = Status }.

-spec new(
    Status :: non_neg_integer(),
    ContentOrHeaders :: binary() | map()
) -> response().
new(Status, Content) when is_binary(Content) ->
    #response{ status = Status, content = Content };
new(Status, Headers) when is_map(Headers) ->
    #response{ status = Status, headers = Headers }.

-spec new(
    Status  :: non_neg_integer(),
    Headers :: map(),
    Content :: binary()
) -> response().
new(Status, Headers, Content) ->
    #response{ status = Status, headers = Headers, content = Content}.

-spec write_str(Response :: response()) -> binary().
write_str(#response{ status = Status, headers = ExtraHeaders, content = Content}) ->
    StatusLine = status_line(Status),
    Headers = headers(ExtraHeaders, Content),
    <<
        StatusLine/binary, ?CRLF,
        Headers/binary, ?CRLF,
        Content/binary
    >>.

%% @private
status_line(Status) ->
    iolist_to_binary([
        [?HTTP11, ?SP],
        [integer_to_list(Status), ?SP],
        bookish_spork_format:reason_phrase(Status)
    ]).

%% @private
headers(ExtraHeaders, Content) ->
    Headers = maps:merge(#{
        <<"Server">> => ?DEFAULT_SERVER,
        <<"Date">> => bookish_spork_format:rfc2616_date(utc_now()),
        <<"Content-Length">> => list_to_binary(integer_to_list(size(Content)))
    }, ExtraHeaders),
    maps:fold(fun(K, V, Acc) ->
        <<
            Acc/binary,
            K/binary, ?COLON,
            V/binary, ?CRLF
        >>
    end, <<>>, Headers).

-ifdef(TEST).
%% @private
utc_now() ->
    {{2018, 4, 28}, {5, 51, 50}}.
-else.
%% @private
utc_now() ->
    calendar:universal_time().
-endif.
