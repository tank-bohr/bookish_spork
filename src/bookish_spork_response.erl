-module (bookish_spork_response).

-export([
    new/1,
    write_str/2
]).

-define(SP, " ").
-define(CRLF, "\r\n").
-define(COLON, ": ").
-define(HTTP11, "HTTP/1.1").
-define(DEFAULT_SERVER, <<"BookishSpork/0.0.1">>).

-type response() :: t() | non_neg_integer() |
    {non_neg_integer(), map(), binary()} |
    {non_neg_integer(), list(), binary()} |
    nonempty_list().

-record(response, {
    status  :: non_neg_integer(),
    headers :: map(),
    content :: binary()
}).

-opaque t() :: #response{}.

-export_type([
    response/0,
    t/0
]).

-spec new(Response :: response()) -> t().
%% @doc Constructs a response data structure
%%
%% Arg can be one of
%%
%% <ul>
%%   <li>
%%     Http status code [https://tools.ietf.org/html/rfc2616#section-6.1.1]
%%   </li>
%%   <li>Response tuple `{Status, Headers, Body}'</li>
%%   <li>Response list `[Status, Headers, Body]'</li>
%%   <li>Response record. Then returns itself</li>
%% </ul>
%%
%% Headers may be map or proplist
%%
%% Example:
%%
%% ```
%% {@module}:new([200, #{}, <<"Hello">>])
%% '''
new(Response) when is_record(Response, response) ->
    Response;
new({Status, Headers, Content}) ->
    new(Status, Headers, Content);
new([Status, Headers, Content]) ->
    new(Status, Headers, Content).

-spec new(
    Status  :: non_neg_integer(),
    Headers :: map() | list(),
    Content :: binary()
) -> t().
%% @private
new(Status, Headers, Content) when is_list(Headers) ->
    new(Status, maps:from_list(Headers), Content);
new(Status, Headers, Content) ->
    #response{ status = Status, headers = Headers, content = Content}.

-spec write_str(Response :: t(), Now :: calendar:datetime()) -> binary().
write_str(#response{ status = Status, headers = ExtraHeaders, content = Content}, Now) ->
    StatusLine = status_line(Status),
    Headers = headers(ExtraHeaders, Content, Now),
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
headers(ExtraHeaders, Content, Now) ->
    Headers = maps:merge(#{
        <<"Server">> => ?DEFAULT_SERVER,
        <<"Date">> => bookish_spork_format:rfc2616_date(Now),
        <<"Content-Length">> => list_to_binary(integer_to_list(size(Content)))
    }, ExtraHeaders),
    maps:fold(fun(K, V, Acc) ->
        <<
            Acc/binary,
            K/binary, ?COLON,
            V/binary, ?CRLF
        >>
    end, <<>>, Headers).
