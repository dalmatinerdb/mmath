-module(mmath_comb_eqc).

-include("../include/mmath.hrl").

-import(mmath_helper, [number_array/0, defined_number_array/0, almost_equal/2]).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

prop_sum() ->
    ?FORALL({La, _, Ba}, number_array(),
            begin
                R1 = sum(La, La),
                R2 = mmath_bin:to_list(mmath_comb:sum([Ba, Ba])),
                ?WHENFAIL(io:format(user, "~p /= ~p~n", [R1, R2]),
                          almost_equal(R1, R2))
            end).

prop_mul() ->
    ?FORALL({La, _, Ba}, number_array(),
            begin
                R1 = mul(La, La),
                R2 = mmath_bin:to_list(mmath_comb:mul([Ba, Ba])),
                ?WHENFAIL(io:format(user, "~p /= ~p~n", [R1, R2]),
                          almost_equal(R1, R2))
            end).

prop_avg() ->
    ?FORALL({La, _, Ba}, number_array(),
            begin
                R1 = avg(La, La),
                R2 = mmath_bin:to_list(mmath_comb:avg([Ba, Ba])),
                ?WHENFAIL(io:format(user, "~p /= ~p~n", [R1, R2]),
                          almost_equal(R1, R2))
            end).

prop_zip() ->
    ?FORALL({La, _, Ba}, number_array(),
            begin
				F = fun(A, B) -> A * B end,
                R1 = mul(La, La),
                R2 = mmath_bin:to_list(mmath_comb:zip(F, [Ba, Ba])),
                ?WHENFAIL(io:format(user, "~p /= ~p~n", [R1, R2]),
                          almost_equal(R1, R2))
            end).

prop_merge() ->
    ?FORALL({{La, _, Ba}, {Lb, _, Bb}}, {number_array(), number_array()},
            begin
                R1 = merge(La, Lb),
                R2 = mmath_bin:to_list(mmath_comb:merge([Ba, Bb])),
                ?WHENFAIL(io:format(user, "~p /= ~p~n", [R1, R2]),
                          almost_equal(R1, R2))
            end).

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
    [N / 2 ||  N <- sum(A, B)].

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


mul(A, B) ->
    mul(A, B, 1, 1, []).

mul([{false, _} | R1], [{true, B} | R2], LA, _, Acc) ->
    mul(R1, R2, LA, B, [LA * B | Acc]);
mul([{true, A} | R1], [{false, _} | R2], _, LB, Acc) ->
    mul(R1, R2, A, LB, [A * LB | Acc]);
mul([{false, _} | R1], [{false, _} | R2], LA, LB, Acc) ->
    mul(R1, R2, LA, LB, [LA * LB | Acc]);
mul([{true, A} | R1], [{true, B} | R2], _, _, Acc) ->
    mul(R1, R2, A, B, [A * B | Acc]);
mul([], [], _, _, Acc) ->
    lists:reverse(Acc);
mul([], [{true, B} | R], LA, _, Acc) ->
    mul([], R, LA, B, [LA * B | Acc]);
mul([], [{false, _} | R], LA, LB, Acc) ->
    mul([], R, LA, LB, [LA * LB | Acc]);
mul(A, [], LA, LB, Acc) ->
    mul([], A, LA, LB, Acc).
