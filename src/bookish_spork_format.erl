-module(bookish_spork_format).

-export([
    rfc2616_date/1,
    reason_phrase/1
]).

-spec rfc2616_date(DateTime :: calendar:datetime()) -> binary().
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
reason_phrase(100) -> <<"Continue">>;
reason_phrase(101) -> <<"Switching Protocols">>;
reason_phrase(200) -> <<"OK">>;
reason_phrase(201) -> <<"Created">>;
reason_phrase(202) -> <<"Accepted">>;
reason_phrase(203) -> <<"Non-Authoritative Information">>;
reason_phrase(204) -> <<"No Content">>;
reason_phrase(205) -> <<"Reset Content">>;
reason_phrase(206) -> <<"Partial Content">>;
reason_phrase(300) -> <<"Multiple Choices">>;
reason_phrase(301) -> <<"Moved Permanently">>;
reason_phrase(302) -> <<"Found">>;
reason_phrase(303) -> <<"See Other">>;
reason_phrase(304) -> <<"Not Modified">>;
reason_phrase(305) -> <<"Use Proxy">>;
reason_phrase(307) -> <<"Temporary Redirect">>;
reason_phrase(400) -> <<"Bad Request">>;
reason_phrase(401) -> <<"Unauthorized">>;
reason_phrase(402) -> <<"Payment Required">>;
reason_phrase(403) -> <<"Forbidden">>;
reason_phrase(404) -> <<"Not Found">>;
reason_phrase(405) -> <<"Method Not Allowed">>;
reason_phrase(406) -> <<"Not Acceptable">>;
reason_phrase(407) -> <<"Proxy Authentication Required">>;
reason_phrase(408) -> <<"Request Time-out">>;
reason_phrase(409) -> <<"Conflict">>;
reason_phrase(410) -> <<"Gone">>;
reason_phrase(411) -> <<"Length Required">>;
reason_phrase(412) -> <<"Precondition Failed">>;
reason_phrase(413) -> <<"Request Entity Too Large">>;
reason_phrase(414) -> <<"Request-URI Too Large">>;
reason_phrase(415) -> <<"Unsupported Media Type">>;
reason_phrase(416) -> <<"Requested range not satisfiable">>;
reason_phrase(417) -> <<"Expectation Failed">>;
reason_phrase(500) -> <<"Internal Server Error">>;
reason_phrase(501) -> <<"Not Implemented">>;
reason_phrase(502) -> <<"Bad Gateway">>;
reason_phrase(503) -> <<"Service Unavailable">>;
reason_phrase(504) -> <<"Gateway Time-out">>;
reason_phrase(505) -> <<"HTTP Version not supported">>;
reason_phrase(_)   -> <<"Unknown">>.

weekday(1) -> <<"Mon">>;
weekday(2) -> <<"Tue">>;
weekday(3) -> <<"Wed">>;
weekday(4) -> <<"Thu">>;
weekday(5) -> <<"Fri">>;
weekday(6) -> <<"Sat">>;
weekday(7) -> <<"Sun">>.

month(1)  -> <<"Jan">>;
month(2)  -> <<"Feb">>;
month(3)  -> <<"Mar">>;
month(4)  -> <<"Apr">>;
month(5)  -> <<"May">>;
month(6)  -> <<"Jun">>;
month(7)  -> <<"Jul">>;
month(8)  -> <<"Aug">>;
month(9)  -> <<"Sep">>;
month(10) -> <<"Oct">>;
month(11) -> <<"Nov">>;
month(12) -> <<"Dec">>.

timepad(Int) ->
    iolist_to_binary(string:pad(integer_to_list(Int), 2, leading, $0)).
