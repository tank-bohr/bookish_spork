-module(bookish_spork_format).

-export([
    rfc2616_date/1,
    reason_phrase/1
]).

-define(HTTP_STATUSES, #{
    100 => <<"Continue">>,
    101 => <<"Switching Protocols">>,
    200 => <<"OK">>,
    201 => <<"Created">>,
    202 => <<"Accepted">>,
    203 => <<"Non-Authoritative Information">>,
    204 => <<"No Content">>,
    205 => <<"Reset Content">>,
    206 => <<"Partial Content">>,
    300 => <<"Multiple Choices">>,
    301 => <<"Moved Permanently">>,
    302 => <<"Found">>,
    303 => <<"See Other">>,
    304 => <<"Not Modified">>,
    305 => <<"Use Proxy">>,
    307 => <<"Temporary Redirect">>,
    400 => <<"Bad Request">>,
    401 => <<"Unauthorized">>,
    402 => <<"Payment Required">>,
    403 => <<"Forbidden">>,
    404 => <<"Not Found">>,
    405 => <<"Method Not Allowed">>,
    406 => <<"Not Acceptable">>,
    407 => <<"Proxy Authentication Required">>,
    408 => <<"Request Time-out">>,
    409 => <<"Conflict">>,
    410 => <<"Gone">>,
    411 => <<"Length Required">>,
    412 => <<"Precondition Failed">>,
    413 => <<"Request Entity Too Large">>,
    414 => <<"Request-URI Too Large">>,
    415 => <<"Unsupported Media Type">>,
    416 => <<"Requested range not satisfiable">>,
    417 => <<"Expectation Failed">>,
    500 => <<"Internal Server Error">>,
    501 => <<"Not Implemented">>,
    502 => <<"Bad Gateway">>,
    503 => <<"Service Unavailable">>,
    504 => <<"Gateway Time-out">>,
    505 => <<"HTTP Version not supported">>
}).

-spec rfc2616_date(DateTime :: calendar:datetime()) -> binary().
%% @doc formats date and time for Server header
rfc2616_date({Date, Time}) ->
    {Y, M, D} = Date,
    {H, Min, Sec} = Time,
    WeekdayNum = calendar:day_of_the_week(Date),
    Weekday = weekday(WeekdayNum),
    Month = month(M),
    iolist_to_binary([
        [Weekday, <<", ">>],
        [integer_to_list(D), <<" ">>],
        [Month, <<" ">>],
        [integer_to_list(Y), " "],
        [timepad(H), ":"],
        [timepad(Min), ":"],
        [timepad(Sec), " "],
        [<<"GMT">>]
    ]).

-spec reason_phrase(Status :: non_neg_integer()) -> binary().
reason_phrase(Status) ->
    maps:get(Status, ?HTTP_STATUSES, <<"Unknown">>).

%% @private
weekday(DayNum) ->
    element(DayNum, {<<"Mon">>, <<"Tue">>, <<"Wed">>, <<"Thu">>, <<"Fri">>, <<"Sat">>, <<"Sun">>}).

%% @private
month(MonthNum) ->
    element(MonthNum, {
        <<"Jan">>, <<"Feb">>, <<"Mar">>, <<"Apr">>,
        <<"May">>, <<"Jun">>, <<"Jul">>, <<"Aug">>,
        <<"Sep">>, <<"Oct">>, <<"Nov">>, <<"Dec">>
    }).

%% @private
timepad(Int) ->
    iolist_to_binary(string:pad(integer_to_list(Int), 2, leading, $0)).
