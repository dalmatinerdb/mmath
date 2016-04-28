-module(mmath_aggr_eqc).

-include("../include/mmath.hrl").

-import(mmath_helper,
        [int_array/0, pos_int/0, non_neg_int/0, defined_int_array/0, epsilon/2,
         non_empty_i_list/0, fully_defined_int_array/0, realise/1, realise/3]).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

prop_n_length_chunks() ->
    ?FORALL({L, N}, {list(int()), pos_int()},
            ceiling(length(L) / N) =:= length(n_length_chunks(L, N))).

prop_avg_all() ->
    ?FORALL(L, non_empty_i_list(),
            begin
                BRes = ?B2L(mmath_aggr:avg(?L2B(L), length(L))),
                LRes = [lists:sum(L) / length(L)],
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [BRes, LRes]),
                   epsilon(BRes, LRes))
            end).

prop_avg_len() ->
    ?FORALL({L, N}, {non_empty_i_list(), pos_int()},
            ceiling(length(L)/N) == length(?B2L(mmath_aggr:avg(?L2B(L), N)))).


prop_scale_r_comp() ->
    ?FORALL({{_, _, B}, N}, {fully_defined_int_array(), pos_int()},
            begin
                R = mmath_bin:realize(B),
                BRes = mmath_aggr:scale(B, N),
                RRes = mmath_aggr:scale_r(R, N),
                RRes2 = mmath_bin:derealize(RRes),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [?B2L(BRes), ?B2L(RRes2)]),
                   ?B2L(BRes) == ?B2L(RRes2))
            end).

prop_mul_r_comp() ->
    ?FORALL({{_, _, B}, N}, {fully_defined_int_array(), pos_int()},
            begin
                R = mmath_bin:realize(B),
                BRes = mmath_aggr:mul(B, N),
                RRes = mmath_aggr:mul_r(R, N),
                RRes2 = mmath_bin:derealize(RRes),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [?B2L(BRes), ?B2L(RRes2)]),
                   ?B2L(BRes) == ?B2L(RRes2))
            end).

prop_div_r_comp() ->
    ?FORALL({{L, _, B}, N}, {int_array(), pos_int()},
            begin
                R = mmath_bin:realize(B),
                B1 = ?L2B(realise(L)),
                BRes = mmath_aggr:divide(B1, N),
                RRes = mmath_aggr:divide_r(R, N),
                RRes2 = mmath_bin:derealize(RRes),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [?B2L(BRes), ?B2L(RRes2)]),
                   ?B2L(BRes) == ?B2L(RRes2))
            end).

prop_min_r_comp() ->
    ?FORALL({{_, _, B}, N}, {fully_defined_int_array(), pos_int()},
            begin
                R = mmath_bin:realize(B),
                BRes = mmath_aggr:min(B, N),
                RRes = mmath_aggr:min_r(R, N),
                RRes2 = mmath_bin:derealize(RRes),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [?B2L(BRes), ?B2L(RRes2)]),
                   ?B2L(BRes) == ?B2L(RRes2))
            end).

prop_avg_r_comp() ->
    ?FORALL({{_, _, B}, N}, {fully_defined_int_array(), pos_int()},
            begin
                R = mmath_bin:realize(B),
                BRes = mmath_aggr:avg(B, N),
                RRes = mmath_aggr:avg_r(R, N),
                RRes2 = mmath_bin:derealize(RRes),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [?B2L(BRes), ?B2L(RRes2)]),
                   ?B2L(BRes) == ?B2L(RRes2))
            end).


prop_max_r_comp() ->
    ?FORALL({{_, _, B}, N}, {fully_defined_int_array(), pos_int()},
            begin
                R = mmath_bin:realize(B),
                BRes = mmath_aggr:max(B, N),
                RRes = mmath_aggr:max_r(R, N),
                RRes2 = mmath_bin:derealize(RRes),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [?B2L(BRes), ?B2L(RRes2)]),
                   ?B2L(BRes) == ?B2L(RRes2))
            end).

prop_sum_r_comp() ->
    ?FORALL({{_, _, B}, N}, {fully_defined_int_array(), pos_int()},
            begin
                R = mmath_bin:realize(B),
                BRes = mmath_aggr:sum(B, N),
                RRes = mmath_aggr:sum_r(R, N),
                RRes2 = mmath_bin:derealize(RRes),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [?B2L(BRes), ?B2L(RRes2)]),
                   ?B2L(BRes) == ?B2L(RRes2))
            end).

prop_avg_impl() ->
    ?FORALL({{_, L, B}, N}, {fully_defined_int_array(), pos_int()},
            begin
                LRes = avg(L, N),
                BRes = mmath_bin:to_list(mmath_aggr:avg(B, N)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [LRes, BRes]),
                   epsilon(LRes, BRes))
            end).

prop_avg_len_undefined() ->
    ?FORALL({L, N}, {non_neg_int(), pos_int()},
            ceiling(L/N) == mmath_bin:length(mmath_aggr:avg(mmath_bin:empty(L), N))).

prop_sum() ->
    ?FORALL({{L, _, B}, N}, {defined_int_array(), pos_int()},
            begin
                LRes = sum(L, N),
                BRes = mmath_bin:to_list(mmath_aggr:sum(B, N)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [LRes, BRes]),
                   LRes == BRes)
            end).

prop_sum_len_undefined() ->
    ?FORALL({L, N}, {non_neg_int(), pos_int()},
            ceiling(L/N) == mmath_bin:length(mmath_aggr:sum(mmath_bin:empty(L), N))).

%% We need to know about unset values for min!
prop_min() ->
    ?FORALL({{L, _, B}, N}, {defined_int_array(), pos_int()},
            min_list(L, N) == mmath_bin:to_list(mmath_aggr:min(B, N))).

prop_min_len_undefined() ->
    ?FORALL({L, N}, {non_neg_int(), pos_int()},
            ceiling(L/N) == mmath_bin:length(mmath_aggr:min(mmath_bin:empty(L), N))).

%% We need to know about unset values for min!
prop_max() ->
    ?FORALL({{L, _, B}, N}, {defined_int_array(), pos_int()},
            begin
                LRes = max_list(L, N),
                BRes = mmath_bin:to_list(mmath_aggr:max(B, N)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [LRes, BRes]),
                   LRes == BRes)
            end).

prop_max_len_undefined() ->
    ?FORALL({L, N}, {non_neg_int(), pos_int()},
            begin
                LRes = ceiling(L/N),
                BRes = mmath_bin:length(mmath_aggr:max(mmath_bin:empty(L), N)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [LRes, BRes]),
                   LRes == BRes)
            end).

prop_der() ->
    ?FORALL({L, _, B}, defined_int_array(),
            begin
                LRes = derivate(L),
                BRes = mmath_bin:to_list(mmath_aggr:derivate(B)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [LRes, BRes]),
                   LRes == BRes)
            end).

prop_der_len_undefined() ->
    ?FORALL(L, non_neg_int(),
            erlang:max(0, L - 1) == mmath_bin:length(mmath_aggr:derivate(mmath_bin:empty(L)))).

prop_scale_int() ->
    ?FORALL({{_, L, B}, S}, {defined_int_array(), real()},
            begin
                LRes = scale_i(L, S),
                BRes = mmath_bin:to_list(mmath_aggr:scale(B,S)),
            ?WHENFAIL(
               io:format(user, "~p =/= ~p~n",
                         [LRes, BRes]),
               LRes == BRes)
            end).

prop_mul_int() ->
    ?FORALL({{_, L, B}, S}, {defined_int_array(), int()},
            begin
                LRes = mul_i(L, S),
                BRes = mmath_bin:to_list(mmath_aggr:mul(B,S)),
            ?WHENFAIL(
               io:format(user, "~p =/= ~p~n",
                         [LRes, BRes]),
               LRes == BRes)
            end).

prop_div_int() ->
    ?FORALL({{_, L, B}, S}, {defined_int_array(), pos_int()},
            begin
                LRes = div_i(L, S),
                BRes = mmath_bin:to_list(mmath_aggr:divide(B,S)),
            ?WHENFAIL(
               io:format(user, "~p =/= ~p~n",
                         [LRes, BRes]),
               epsilon(LRes, BRes))
            end).

prop_map_int() ->
    ?FORALL({{_, L, B}, S}, {defined_int_array(), real()},
			begin
				Scale = fun(V) ->
								round(V * S)
						end,
                scale_i(L, S) == mmath_bin:to_list(mmath_aggr:map(B, Scale))
			end).

prop_scale_len_undefined() ->
    ?FORALL(L, non_neg_int(),
            L == mmath_bin:length(mmath_aggr:scale(mmath_bin:empty(L), 1))).

prop_combine_sum_identity() ->
    ?FORALL({_, _, A}, defined_int_array(),
            mmath_comb:sum([A]) == A).

prop_combine_sum_N() ->
    ?FORALL({{_, _, A}, N}, {defined_int_array(), pos_int()},
            begin
                LRes = mmath_aggr:mul(A, N),
                BRes = mmath_comb:sum([A || _ <- lists:seq(1, N)]),
                ?WHENFAIL(
                   io:format(user, "sum(~p*~p) -> ~p =/= ~p~n",
                             [A, N, LRes, BRes]),
                   LRes == BRes)
            end).

prop_combine_sum_r_comp() ->
    ?FORALL({{_, _, B1}, {_, _, B2}}, {fully_defined_int_array(), fully_defined_int_array()},
            begin
                RB1 = mmath_bin:realize(B1),
                RB2 = mmath_bin:realize(B2),

                BRes = mmath_comb:sum([B1, B2]),
                RRes = mmath_comb:sum_r([RB1, RB2]),

                RRes2 = mmath_bin:derealize(RRes),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [?B2L(BRes), ?B2L(RRes2)]),
                   ?B2L(BRes) == ?B2L(RRes2))
            end).

prop_combine_avg_N() ->
    ?FORALL({{_, _, A}, N}, {defined_int_array(), pos_int()},
            begin
                LRes = ?B2L(mmath_aggr:mul(A, 1)),
                BRes = ?B2L(mmath_comb:avg([A || _ <- lists:seq(1, N+1)])),
                ?WHENFAIL(
                   io:format(user, "avg(~p*~p) -> ~p =/= ~p~n",
                             [A, N, LRes, BRes]),
                   epsilon(LRes, BRes))
            end).

prop_combine_avg2_r_comp() ->
    ?FORALL({{_, _, B1}, {_, _, B2}}, {fully_defined_int_array(), fully_defined_int_array()},
            begin
                RB1 = mmath_bin:realize(B1),
                RB2 = mmath_bin:realize(B2),

                BRes = mmath_comb:avg([B1, B2]),
                RRes = mmath_comb:avg_r([RB1, RB2]),

                RRes2 = mmath_bin:derealize(RRes),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [?B2L(BRes), ?B2L(RRes2)]),
                   ?B2L(BRes) == ?B2L(RRes2))
            end).

prop_combine_avg3_r_comp() ->
    ?FORALL({{_, _, B1}, {_, _, B2}, {_, _, B3}},
            {fully_defined_int_array(), fully_defined_int_array(), fully_defined_int_array()},
            begin
                RB1 = mmath_bin:realize(B1),
                RB2 = mmath_bin:realize(B2),
                RB3 = mmath_bin:realize(B3),

                BRes = mmath_comb:avg([B1, B2, B3]),
                RRes = mmath_comb:avg_r([RB1, RB2, RB3]),

                RRes2 = mmath_bin:derealize(RRes),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [?B2L(BRes), ?B2L(RRes2)]),
                   ?B2L(BRes) == ?B2L(RRes2))
            end).


prop_count_empty() ->
    ?FORALL({{L, _, B}, N}, {int_array(), pos_int()},
            begin
                Act = mmath_bin:to_list(mmath_aggr:empty(B, N)),
                Exp = empty(L, N),
                ?WHENFAIL(io:format(user, "~p /= ~p~n", [Act, Exp]),
                          Act == Exp)
            end).

prop_combine_percentile_int() ->
    ?FORALL({{L, _, A}, N, PRaw}, {defined_int_array(), pos_int(), choose(0, 1000)},
            begin
                P = PRaw/1000,
                Exp = percentile(L, N, P),
                Act = to_list(mmath_aggr:percentile(A, N, P), []),
                ?WHENFAIL(io:format(user, "Exp: ~p~nAct: ~p~n",
                                    [Exp, Act]),
                          Exp == Act)
            end).


to_list(<<?INT:?TYPE_SIZE, V:?BITS/?INT_TYPE, R/binary>>, Acc) ->
    to_list(R, [V | Acc]);
to_list(<<?NONE:?TYPE_SIZE, _:?BITS/?INT_TYPE, R/binary>>, Acc) ->
    to_list(R, [0 | Acc]);
to_list(<<>>, Acc) ->
    lists:reverse(Acc).

scale_i(L, S) ->
    [round(N*S) || N <- L].

mul_i(L, S) ->
    [N * S || N <- L].

div_i(L, S) ->
    [N / S || N <- L].

scale_f(L, S) ->
    [N*S || N <- L].

percentile(L, N, P) ->
    apply_n(L, N, fun(L1, N1) -> percentile_(L1, N1, P) end).

percentile_(L, _N, P) ->
    case realise(L, 0, []) of
        [] ->
            0;
        L1 ->
            Len = length(L1),
            lists:nth(min(Len, round(Len * P) + 1), lists:sort(L1))
    end.

avg(L, N) ->
    apply_n(L, N, fun avg_/2).

sum(L, N) ->
    apply_n(realise(L, 0, []), N, fun sum_/2).

min_list(L, N) ->
    apply_n(L, N, fun min_/2).

max_list(L, N) ->
    apply_n(L, N, fun max_/2).

empty(L, N) ->
    apply_n(L, N, fun empty_/2).

apply_n(L, N, F) ->
    fix_list([F(SL, N) || SL <- n_length_chunks(L, N)], 0, []).

empty_(L, N) ->
    lists:sum([1 || {false, _} <- L]) + (N - length(L)).

avg_(L, N) ->
    case length(L) of
        N ->
            lists:sum(L);
        Len ->
            lists:sum(L) + (lists:last(L) * (N - Len))
    end / N.

sum_(L, N) ->
    case length(L) of
        N ->
            lists:sum(L);
        Len ->
            lists:sum(L) + (lists:last(L) * (N - Len))
    end.
%%     sum_(L, 0, 0, N).

%% %%sum_(L, Sum, Last, Count)
%% sum_([], Sum, _Last, 0)  ->
%%     Sum;

%% sum_([], Sum, Last, C)  ->
%%     Sum + (Last  * C);

%% sum_([{true, V} | R], Sum, _Last, C)  ->
%%     sum_(R, Sum + V, V, C -1);

%% sum_([{false, _} | R], Sum, Last, C)  ->
%%     sum_(R, Sum + Last, Last, C -1).

min_(L, _N) ->
    case lists:sort([V || {true, V} <- L]) of
        [] ->
            undefined;
        [S | _ ] ->
            S
    end.

max_(L, _N) ->
    case lists:sort([V || {true, V} <- L]) of
        [] ->
            undefined;
        L1 ->
            lists:last(L1)
    end.

fix_list([undefined | T], Last, Acc) ->
    fix_list(T, Last, [Last | Acc]);
fix_list([V | T], _, Acc) ->
    fix_list(T, V, [V | Acc]);
fix_list([],_,Acc) ->
    lists:reverse(Acc).

derivate([]) ->
    [];
derivate([{true, H} | T]) ->
    derivate(H, T, []);
derivate([{false, _} | T]) ->
    derivate(find_first(T), T, []).

derivate(H, [{true, H1} | T], Acc) ->
    derivate(H1, T, [H1 - H | Acc]);
derivate(H, [{false, _} | T], Acc) ->
    derivate(H, T, [0 | Acc]);
derivate(_, [], Acc) ->
    lists:reverse(Acc).


find_first([]) ->
    0;
find_first([{true, V} | _]) ->
    V;
find_first([{false, _}| R]) ->
    find_first(R).

prop_ceiling() ->
    ?FORALL(F, real(),
            F =< ceiling(F)).

%% Taken from: http://schemecookbook.org/Erlang/NumberRounding
ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.
%% taken from http://stackoverflow.com/questions/12534898/splitting-a-list-in-equal-sized-chunks-in-erlang
n_length_chunks([],_) -> [];
n_length_chunks(List,Len) when Len > length(List) ->
    [List];
n_length_chunks(List,Len) ->
    {Head,Tail} = lists:split(Len,List),
    [Head | n_length_chunks(Tail,Len)].
