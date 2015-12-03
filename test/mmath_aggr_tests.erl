-module(mmath_aggr_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


mul_test() ->
    ?assertEqual(<<1, 36:56>>, mmath_aggr:mul(<<1, 12:56>>, 3)).

div_test() ->
    ?assertEqual(<<1, 4:56>>, mmath_aggr:divide(<<1, 12:56>>, 3)).

sum_test() ->
    Points = mmath_bin:from_list([-1.25, 21.0]),
    Sum = mmath_aggr:sum(Points, 2),
    ?assertEqual([19.75], mmath_bin:to_list(Sum)).

derivate_test() ->
    Points = mmath_bin:from_list([36, 12, -2, 1.25, 1.25, 21, 4001]),
    Derivate = mmath_aggr:derivate(Points),
    ?assertEqual([-24, -14, 3.25, 0.0, 19.75, 3980], mmath_bin:to_list(Derivate)).

max_test() ->
    Points = mmath_bin:from_list([-1.25, 21.0, 12.6]),
    Max = mmath_aggr:max(Points, 3),
    ?assertEqual([21.0], mmath_bin:to_list(Max)).

min_test() ->
    Points = mmath_bin:from_list([-5.25, -11, 21, 12.6]),
    Min = mmath_aggr:min(Points, 4),
    ?assertEqual([-5.25], mmath_bin:to_list(Min)).

% TODO: Investigate why uncommenting this is causing ebin segfaults
%%add_test() ->
%%    Points = mmath_bin:from_list([0.21, 0]),
%%    ?assertEqual(<<2, -14, 21000000000000:48, 1, 0,0,0,0,0,0,0>>, Points),
%%    Sum = mmath_aggr:sum(Points, 2),
%%    ?assertEqual([0.21], mmath_bin:to_list(Sum)).

%% This one is supposed to test decimal accuracy
add_2_test() ->
    Points = mmath_bin:from_list([0.1, 0.1]),
    Sum = mmath_aggr:sum(Points, 2),
    ?assertEqual([0.2], mmath_bin:to_list(Sum)).

avg_test() ->
    Points = mmath_bin:from_list([0.93, 0.93]),
    _R = mmath_bin:realize(Points),
    ?assertEqual([0.93], mmath_bin:to_list(mmath_aggr:avg(Points, 2))).
