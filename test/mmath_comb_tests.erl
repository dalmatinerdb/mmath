-module(mmath_comb_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


sum_test() ->
    A = mmath_bin:from_list([36, 2.25,  1 ]),
    B = mmath_bin:from_list([12, 1.5, 0.000000005]),
    ?assertEqual([48, 3.75, 1.000000005] , mmath_bin:to_list(mmath_comb:sum(A, B))).
