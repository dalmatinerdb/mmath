-module(mmath_comb_eqc).

-include("../include/mmath.hrl").

-import(mmath_helper, [number_array/0, number_array/1, defined_number_array/0,
                       almost_equal/2, realise/1, realise/3, pos_int/0]).

-include_lib("eqc/include/eqc.hrl").

-export([avg/2]).

-export([prop_sum/0,
         prop_sum3/0,
         prop_diff/0,
         prop_diff3/0,
         prop_product/0,
         prop_product3/0,
         prop_quotient/0,
         prop_quotient3/0,
         prop_avg/0,
         prop_min/0,
         prop_min3/0,
         prop_max/0,
         prop_max3/0]).

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

prop_diff() ->
    ?FORALL(
       N, array_size(),
       ?FORALL({{La, _, Ba}, {Lb, _, Bb}},
               {number_array(N), number_array(N)},
               begin
                   Ra = realise(La),
                   Rb = realise(Lb),
                   R1 = diff(Ra, Rb),
                   R2 = mmath_comb:diff([Ba, Bb]),
                   R3 = mmath_bin:to_list(mmath_bin:derealize(R2)),
                   ?WHENFAIL(io:format(user,
                                       "Expected: ~p~n"
                                       "Result:   ~p~n", [R1, R3]),
                             almost_equal(R1, R3))
               end)).

prop_diff3() ->
    ?FORALL(
       N, array_size(),
       ?FORALL({{La, _, Ba}, {Lb, _, Bb}, {Lc, _, Bc}},
               {number_array(N), number_array(N), number_array(N)},
               begin
                   Ra = realise(La),
                   Rb = realise(Lb),
                   Rc = realise(Lc),
                   R1 = diff(diff(Ra, Rb), Rc),
                   R2 = mmath_comb:diff([Ba, Bb, Bc]),
                   R3 = mmath_bin:to_list(mmath_bin:derealize(R2)),
                   ?WHENFAIL(io:format(user,
                                       "Expected: ~p~n"
                                       "Result:   ~p~n", [R1, R3]),
                             almost_equal(R1, R3))
               end)).

prop_product() ->
    ?FORALL(
       N, array_size(),
       ?FORALL({{La, _, Ba}, {Lb, _, Bb}},
               {number_array(N), number_array(N)},
               begin
                   Ra = realise(La),
                   Rb = realise(Lb),
                   R1 = product(Ra, Rb),
                   R2 = mmath_comb:product([Ba, Bb]),
                   R3 = mmath_bin:to_list(mmath_bin:derealize(R2)),
                   ?WHENFAIL(io:format(user,
                                       "Expected: ~p~n"
                                       "Result:   ~p~n", [R1, R3]),
                             almost_equal(R1, R3))
               end)).

prop_product3() ->
    ?FORALL(
       N, array_size(),
       ?FORALL({{La, _, Ba}, {Lb, _, Bb}, {Lc, _, Bc}},
               {number_array(N), number_array(N), number_array(N)},
               begin
                   Ra = realise(La),
                   Rb = realise(Lb),
                   Rc = realise(Lc),
                   R1 = product(Ra, product(Rb, Rc)),
                   R2 = mmath_comb:product([Ba, Bb, Bc]),
                   R3 = mmath_bin:to_list(mmath_bin:derealize(R2)),
                   ?WHENFAIL(io:format(user,
                                       "Expected: ~p~n"
                                       "Result:   ~p~n", [R1, R3]),
                             almost_equal(R1, R3))
               end)).

prop_quotient() ->
    ?FORALL(
       N, array_size(),
       ?FORALL({{La, _, Ba}, {Lb, _, Bb}},
               {number_array(N), number_array(N)},
               begin
                   Ra = realise(La),
                   Rb = realise(Lb),
                   R1 = quotient(Ra, Rb),
                   R2 = mmath_comb:quotient([Ba, Bb]),
                   R3 = mmath_bin:to_list(mmath_bin:derealize(R2)),
                   ?WHENFAIL(io:format(user,
                                       "Expected: ~p~n"
                                       "Result:   ~p~n", [R1, R3]),
                             almost_equal(R1, R3))
               end)).

prop_quotient3() ->
    ?FORALL(
       N, array_size(),
       ?FORALL({{La, _, Ba}, {Lb, _, Bb}, {Lc, _, Bc}},
               {number_array(N), number_array(N), number_array(N)},
               begin
                   Ra = realise(La),
                   Rb = realise(Lb),
                   Rc = realise(Lc),
                   R1 = quotient(quotient(Ra, Rb), Rc),
                   R2 = mmath_comb:quotient([Ba, Bb, Bc]),
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

diff(A, B) ->
    diff(A, B, []).
diff([A | R1], [B | R2], Acc) ->
    diff(R1, R2, [A - B | Acc]);
diff([], [], Acc) ->
    lists:reverse(Acc).

product(A, B) ->
    product(A, B, []).
product([A | R1], [B | R2], Acc) ->
    product(R1, R2, [A * B | Acc]);
product([], [], Acc) ->
    lists:reverse(Acc).

quotient(A, B) ->
    quotient(A, B, []).
quotient([A | R1], [B | R2], Acc) when B == 0 ->
    quotient(R1, R2, [A | Acc]);
quotient([A | R1], [B | R2], Acc) ->
    quotient(R1, R2, [A / B | Acc]);
quotient([], [], Acc) ->
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

