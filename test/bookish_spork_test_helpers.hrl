-ifndef(BOOKISH_SPORK_TEST_HELPERS_HRL).
-define(BOOKISH_SPORK_TEST_HELPERS_HRL, true).

-define(CUSTOM_PORT, 9871).
-define(HTTPC_SUCCESS(Status), {ok, {{_, Status, _}, _, _}}).
-define(HTTPC_OK, ?HTTPC_SUCCESS(200)).
-define(HTTPC_NO_CONTENT, ?HTTPC_SUCCESS(204)).

-define(CUSTOM_CASE_HEADERS,  [
    {"DNT", "1"},
    {"Connection", "close"},
    {"x-vEry-cAmEl-cAsE", "yEs"}
]).

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 23).
-define(RAW_HEADERS, [
    {<<"DNT">>, <<"1">>},
    {<<"Connection">>, <<"close">>},
    {<<"x-vEry-cAmEl-cAsE">>, <<"yEs">>}
]).
-else.
-define(RAW_HEADERS, [
    {<<"Dnt">>, <<"1">>},
    {<<"Connection">>, <<"close">>},
    {<<"X-Very-Camel-Case">>, <<"yEs">>}
]).
-endif.
-else.
-define(RAW_HEADERS, [
    {<<"Dnt">>, <<"1">>},
    {<<"Connection">>, <<"close">>},
    {<<"X-Very-Camel-Case">>, <<"yEs">>}
]).
-endif.

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 23).
-define(TLS_EXT, #{
    alpn                   := _,
    client_hello_versions  := _,
    cookie                 := _,
    ec_point_formats       := _,
    elliptic_curves        := _,
    key_share              := _,
    pre_shared_key         := _,
    psk_key_exchange_modes := _,
    signature_algs         := _,
    signature_algs_cert    := _,
    sni                    := _
}).
-else.
-define(TLS_EXT, #{
    alpn               := _,
    ec_point_formats   := _,
    elliptic_curves    := _,
    renegotiation_info := _,
    signature_algs     := _,
    sni                := _,
    srp                := _
}).
-endif.
-endif.

-endif.
