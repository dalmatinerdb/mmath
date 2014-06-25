-module(mmath_comb_test).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../include/mmath.hrl").

-import(mmath_helper, [int_array/0, float_array/0, non_neg_int/0, pos_int/0,
                       i_or_f_list/0, i_or_f_array/0, out/1]).

-compile(export_all).

prop_sum_int() ->
    ?FORALL({La, _, Ba}, int_array(),
            begin
                R1 = sum(La, La),
                R2 = mmath_bin:to_list(mmath_comb:sum([Ba, Ba])),
                ?WHENFAIL(io:format(user, "~p /= ~p~n", [R1, R2]), R1 == R2)
            end).

prop_avg_int() ->
    ?FORALL({La, _, Ba}, int_array(),
            begin
                R1 = avg(La, La),
                R2 = mmath_bin:to_list(mmath_comb:avg([Ba, Ba])),
                ?WHENFAIL(io:format(user, "~p /= ~p~n", [R1, R2]), R1 == R2)
            end).

prop_merge_int() ->
    ?FORALL({{La, _, Ba}, {Lb, _, Bb}}, {int_array(), int_array()},
            merge(La, Lb) == mmath_bin:to_list(mmath_comb:merge([Ba, Bb]))).

prop_merge_float() ->
    ?FORALL({{La, _, Ba}, {Lb, _, Bb}}, {float_array(), float_array()},
            merge(La, Lb) == mmath_bin:to_list(mmath_comb:merge([Ba, Bb]))).

merge(A, B) ->
    merge(A, B, []).

merge([{false, _} | R1], [{true, V} | R2], Acc) ->
    merge(R1, R2, [V | Acc]);
merge([{true, V} | R1], [_ | R2], Acc) ->
    merge(R1, R2, [V | Acc]);
merge([_ | R1], [_ | R2], [Last | _] = Acc) ->
    merge(R1, R2, [Last | Acc]);
merge([_ | R1], [_ | R2], []) ->
    merge(R1, R2, [0]);
merge([], [], Acc) ->
    lists:reverse(Acc);
merge([], [{true, V} | R], Acc ) ->
    merge([], R, [V | Acc]);
merge([], [{false, _} | R], []) ->
    merge([], R, [0]);
merge([], [{false, _} | R], [Last | _] = Acc) ->
    merge([], R, [Last | Acc]);
merge(A, [], Acc) ->
    merge([], A, Acc).

avg(A, B) ->
    [N div 2 ||  N <- sum(A, B)].

sum(A, B) ->
    sum(A, B, 0, 0, []).

sum([{false, _} | R1], [{true, B} | R2], LA, _, Acc) ->
    sum(R1, R2, LA, B, [LA + B | Acc]);
sum([{true, A} | R1], [{false, _} | R2], _, LB, Acc) ->
    sum(R1, R2, A, LB, [A + LB | Acc]);
sum([{false, _} | R1], [{false, _} | R2], LA, LB, Acc) ->
    sum(R1, R2, LA, LB, [LA + LB | Acc]);
sum([{true, A} | R1], [{true, B} | R2], _, _, Acc) ->
    sum(R1, R2, A, B, [A + B | Acc]);
sum([], [], _, _, Acc) ->
    lists:reverse(Acc);
sum([], [{true, B} | R], LA, _, Acc) ->
    sum([], R, LA, B, [LA + B | Acc]);
sum([], [{false, _} | R], LA, LB, Acc) ->
    sum([], R, LA, LB, [LA + LB | Acc]);
sum(A, [], LA, LB, Acc) ->
    sum([], A, LA, LB, Acc).

-include("eqc_helper.hrl").