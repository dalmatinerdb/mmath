-module(mmath_aggr_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


mul_test() ->
    ?assertEqual(<<1, 36:56>>, mmath_aggr:mul(<<1, 12:56>>, 3)).

div_test() ->
    ?assertEqual(<<1, 4:56>>, mmath_aggr:divide(<<1, 12:56>>, 3)).
