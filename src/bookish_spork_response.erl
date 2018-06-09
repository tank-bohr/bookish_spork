-module (bookish_spork_response).
-export([all/0]).

-define(SP, " ").
-define(CRLF, "\r\n").
-define(COLON, ": ").
-define(HTTP11, "HTTP/1.1").
-define(DEFAULT_SERVER, <<"BookishSpork/0.0.1">>).
-define(DEFAULT_STATUS_CODE, 204).

-spec all() -> binary().
all() ->
    StatusLine = status_line(),
    Content = content(),
    Headers = headers(Content),
    <<
        StatusLine/binary, ?CRLF,
        Headers/binary, ?CRLF,
        Content/binary
    >>.

status_line() ->
    StatusCode = bookish_spork_settings:status(),
    ReasonPhrase = bookish_spork_format:reason_phrase(StatusCode),
    iolist_to_binary([
        [?HTTP11, ?SP],
        [integer_to_list(StatusCode), ?SP],
        ReasonPhrase
    ]).

headers(Content) ->
    headers(Content, calendar:universal_time()).

headers(Content, Now) ->
    Headers = maps:merge(#{
        <<"Server">> => ?DEFAULT_SERVER,
        <<"Date">> => bookish_spork_format:rfc2616_date(Now),
        <<"Content-Length">> => list_to_binary(integer_to_list(size(Content)))
    }, bookish_spork_settings:headers()),
    maps:fold(fun(K, V, Acc) ->
        <<
            Acc/binary,
            K/binary, ?COLON,
            V/binary, ?CRLF
        >>
    end, <<>>, Headers).

content() ->
    bookish_spork_settings:content().
