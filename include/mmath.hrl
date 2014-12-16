%% Those two must match!
-define(BITS, 60).
-define(INT_TYPE, signed-integer).

-define(NONE, 0).
-define(INT, 1).
-define(TYPE_SIZE, 4).

-define(DATA_SIZE, ((?BITS + ?TYPE_SIZE) div 8)).


-define(B2L(B), mmath_bin:to_list(B)).
-define(L2B(L), mmath_bin:from_list(L)).
