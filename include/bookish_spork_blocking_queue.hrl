-ifndef(BOOKISH_SPORK_BLOCKING_QUEUE_HRL).
-define(BOOKISH_SPORK_BLOCKING_QUEUE_HRL, true).

-ifdef(OTP_RELEASE).
  -if(?OTP_RELEASE >= 24).
    -define(QUEUE_DELETE(Item, Queue), queue:delete(Item, Queue)).
  -else.
    -define(QUEUE_DELETE(Item, Queue), queue:filter(fun(I) -> I =/= Item end, Queue)).
  -endif.
-else.
  -define(QUEUE_DELETE(Item, Queue), queue:filter(fun(I) -> I =/= Item end, Queue)).
-endif.

-define(DEFAULT_TIMEOUT_MILLIS, 5000).

-endif.
