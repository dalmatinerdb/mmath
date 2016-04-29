%% Those two must match!
%% Number of bits for an integer value, 56 bit is a sweetspot where it it not
%% yet converted in a big int but still byte alligned for binary conversion
-define(BITS, 56).

%% The type of data we store.
-define(INT_TYPE, signed-integer).

%% The size of the type field
-define(TYPE_SIZE, 8).

%% The size of exponent field for floating point decimals
-define(DEC_EXP_SIZE, 8).

%% The size of coefficient field for floating point decimals
-define(DEC_COEF_SIZE, 48).

%% Value types defined so far.

%% this field does not contain a value
-define(NONE, 0).

%% this field does contain an integer value
-define(INT, 1).

%% Type used by floating point decimal using base 10
-define(DEC, 2).

%% Decimal number precision (number of kept digits)
-define(DEC_PRECISION, 14).

%% Point data size
-define(DATA_SIZE, ((?BITS + ?TYPE_SIZE) div 8)).

%% realized (expanded) data size
-define(RDATA_SIZE, 16).

-define(B2L(B), mmath_bin:to_list(B)).
-define(L2B(L), mmath_bin:from_list(L)).
