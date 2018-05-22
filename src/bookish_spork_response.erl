-module (bookish_spork_response).
-export([all/0]).

-define(SP, " ").
-define(CRLF, "\r\n").
-define(COLON, ": ").
-define(HTTP11, "HTTP/1.1").
-define(DEFAULT_SERVER, <<"BookishSpork/0.0.1">>).
-define(DEFAULT_STATUS_CODE, 204).

all() ->
    StatusLine = status_line(),
    Headers = headers(),
    Content = content(),
    <<
        StatusLine/binary, ?CRLF,
        Headers/binary,
        Content/binary
    >>.

status_line() ->
    StatusCode = ?DEFAULT_STATUS_CODE,
    ReasonPhrase = bookish_spork_format:reason_phrase(StatusCode),
    iolist_to_binary([
        [?HTTP11, ?SP],
        [integer_to_list(StatusCode), ?SP],
        ReasonPhrase
    ]).

headers() ->
    headers(calendar:universal_time()).

headers(Now) ->
    Server = ?DEFAULT_SERVER,
    Date = bookish_spork_format:rfc2616_date(Now),
    Headers = #{
        <<"Server">> => Server,
        <<"Date">> => Date
    },
    maps:fold(fun(K, V, Acc) ->
        <<
        Acc/binary,
        K/binary, ?COLON,
        V/binary, ?CRLF
        >>
    end, <<>>, Headers).

content() ->
    <<>>.
