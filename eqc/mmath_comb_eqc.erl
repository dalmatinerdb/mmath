-module(mmath_comb_eqc).

-include("../include/mmath.hrl").

-import(mmath_helper, [number_array/0, number_array/1, defined_number_array/0,
                       almost_equal/2, realise/1, pos_int/0]).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

array_size() ->
    choose(1, 50).

prop_sum() ->
    ?FORALL(
       N, array_size(),
       ?FORALL({{La, _, Ba}, {Lb, _, Bb}},
               {number_array(N), number_array(N)},
               begin
                   Ra = realise(La),
                   Rb = realise(Lb),
                   R1 = sum(Ra, Rb),
                   R2 = mmath_comb:sum([Ba, Bb]),
                   R3 = mmath_bin:to_list(mmath_bin:derealize(R2)),
                   ?WHENFAIL(io:format(user,
                                       "Expected: ~p~n"
                                       "Result:   ~p~n", [R1, R3]),
                             almost_equal(R1, R3))
               end)).

prop_sum3() ->
    ?FORALL(
       N, array_size(),
       ?FORALL({{La, _, Ba}, {Lb, _, Bb}, {Lc, _, Bc}},
               {number_array(N), number_array(N), number_array(N)},
               begin
                   Ra = realise(La),
                   Rb = realise(Lb),
                   Rc = realise(Lc),
                   R1 = sum(Ra, sum(Rb, Rc)),
                   R2 = mmath_comb:sum([Ba, Bb, Bc]),
                   R3 = mmath_bin:to_list(mmath_bin:derealize(R2)),
                   ?WHENFAIL(io:format(user,
                                       "Expected: ~p~n"
                                       "Result:   ~p~n", [R1, R3]),
                             almost_equal(R1, R3))
               end)).

prop_avg() ->
    ?FORALL(
       N, array_size(),
       ?FORALL({La, _, Ba}, number_array(N),
               begin
                   Lr = realise(La),
                   R1 = avg(Lr, Lr),
                   R2 = mmath_comb:avg([Ba, Ba]),
                   R3 = mmath_bin:to_list(mmath_bin:derealize(R2)),
                   ?WHENFAIL(io:format(user, "~p /= ~p~n", [R1, R3]),
                             almost_equal(R1, R3))
               end)).

prop_min() ->
    ?FORALL(
       N, array_size(),
       ?FORALL({{La, _, Ba}, {Lb, _, Bb}},
               {number_array(N), number_array(N)},
               begin
                   Ra = realise(La),
                   Rb = realise(Lb),
                   R1 = min_(Ra, Rb),
                   R2 = mmath_comb:min([Ba, Bb]),
                   R3 = mmath_bin:to_list(mmath_bin:derealize(R2)),
                   ?WHENFAIL(io:format(user, "~p /= ~p~n", [R1, R3]),
                             almost_equal(R1, R3))
               end)).

prop_min3() ->
    ?FORALL(
       N, array_size(),
       ?FORALL({{La, _, Ba}, {Lb, _, Bb}, {Lc, _, Bc}},
               {number_array(N), number_array(N), number_array(N)},
               begin
                   Ra = realise(La),
                   Rb = realise(Lb),
                   Rc = realise(Lc),
                   R1 = min_(Rc, min_(Ra, Rb)),
                   R2 = mmath_comb:min([Ba, Bb, Bc]),
                   R3 = mmath_bin:to_list(mmath_bin:derealize(R2)),
                   ?WHENFAIL(io:format(user,
                                       "Expected: ~p~n"
                                       "Result:   ~p~n", [R1, R3]),
                             almost_equal(R1, R3))
               end)).

prop_max() ->
    ?FORALL(
       N, array_size(),
       ?FORALL({{La, _, Ba}, {Lb, _, Bb}},
               {number_array(N), number_array(N)},
               begin
                   Ra = realise(La),
                   Rb = realise(Lb),
                   R1 = max_(Ra, Rb),
                   R2 = mmath_comb:max([Ba, Bb]),
                   R3 = mmath_bin:to_list(mmath_bin:derealize(R2)),
                   ?WHENFAIL(io:format(user, "~p /= ~p~n", [R1, R3]),
                             almost_equal(R1, R3))
               end)).

prop_max3() ->
    ?FORALL(
       N, array_size(),
       ?FORALL({{La, _, Ba}, {Lb, _, Bb}, {Lc, _, Bc}},
               {number_array(N), number_array(N), number_array(N)},
               begin
                   Ra = realise(La),
                   Rb = realise(Lb),
                   Rc = realise(Lc),
                   R1 = max_(Rc, max_(Ra, Rb)),
                   R2 = mmath_comb:max([Ba, Bb, Bc]),
                   R3 = mmath_bin:to_list(mmath_bin:derealize(R2)),
                   ?WHENFAIL(io:format(user,
                                       "Expected: ~p~n"
                                       "Result:   ~p~n", [R1, R3]),
                             almost_equal(R1, R3))
               end)).



%% prop_mul() ->
%%     ?FORALL({La, _, Ba}, number_array(),
%%             begin
%%                 R1 = mul(La, La),
%%                 R2 = mmath_bin:to_list(mmath_comb:mul([Ba, Ba])),
%%                 ?WHENFAIL(io:format(user, "~p /= ~p~n", [R1, R2]),
%%                           almost_equal(R1, R2))
%%             end).

%% prop_zip() ->
%%     ?FORALL({La, _, Ba}, number_array(),
%%             begin
%%              F = fun(A, B) -> A * B end,
%%                 R1 = mul(La, La),
%%                 R2 = mmath_bin:to_list(mmath_comb:zip(F, [Ba, Ba])),
%%                 ?WHENFAIL(io:format(user, "~p /= ~p~n", [R1, R2]),
%%                           almost_equal(R1, R2))
%%             end).

avg(A, B) ->
    [N / 2 ||  N <- sum(A, B)].

sum(A, B) ->
    sum(A, B, []).

sum([A | R1], [B | R2], Acc) ->
    sum(R1, R2, [A + B | Acc]);
sum([], [], Acc) ->
    lists:reverse(Acc).

min_(A, B) ->
    min_(A, B, []).
min_([A | R1], [B | R2], Acc) ->
    min_(R1, R2, [min(A, B) | Acc]);
min_([], [], Acc) ->
    lists:reverse(Acc).

max_(A, B) ->
    max_(A, B, []).
max_([A | R1], [B | R2], Acc) ->
    max_(R1, R2, [max(A, B) | Acc]);
max_([], [], Acc) ->
    lists:reverse(Acc).


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
