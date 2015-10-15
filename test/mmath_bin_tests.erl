-module(mmath_bin_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


to_list_test() ->
    ?assertEqual([36] , mmath_bin:to_list(<<1,0,0,0,0,0,0,36>>)).

form_pos_int_test() ->
    ?assertEqual(<<1,0,0,0,0,0,0,36>>, mmath_bin:from_list([36])).

from_neg_int_test() ->
    ?assertEqual(<<1,255,255,255,255,255,255,254>>, mmath_bin:from_list([-2])).

from_positive_float_test() ->
    %% 0.361 = 36 100 000 000 000 * 10^-14
    ?assertEqual(<<2,-13,36100000000000:48>>, mmath_bin:from_list([3.61])).

from_negative_float_test() ->
    %% 0.361 = -36 100 000 000 000 * 10^-14
    ?assertEqual(<<2,-13,-36100000000000:48>>, mmath_bin:from_list([-3.61])).

from_large_float_test() ->
    %% 3.61e24 = 36 100 000 000 000 * 10^11
    ?assertEqual(<<2,11,36100000000000:48>>, mmath_bin:from_list([3.61e24])).

%%from_decimal_string_test() ->
%%    ?assertEqual(<<2,255,0,0,0,0,0,36>>, mmath_bin:from_list(["3.6"])).

