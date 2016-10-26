-module(mmath_aggr_eqc).

-include("../include/mmath.hrl").

-import(mmath_helper,
        [number_array/0, pos_int/0, non_neg_int/0, defined_number_array/0,
         non_empty_number_list/0, fully_defined_number_array/0, realise/1,
         realise/3, confidence/1, almost_equal/2]).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

prop_n_length_chunks() ->
    ?FORALL({L, N}, {list(int()), pos_int()},
            ceiling(length(L) / N) =:= length(n_length_chunks(L, N))).

prop_percentile_all() ->
    ?FORALL({L, N}, {non_empty_number_list(), pos_int()},
            begin
                Len = length(L),
                N1 = N rem Len,
                P = N1 / Len,
                BList = mmath_bin:realize(?L2B(L)),
                BRes = to_list_d(mmath_aggr:percentile(BList, P, Len)),
                LRes = [percentile(P, lists:sort(L))],
                ?WHENFAIL(
                   io:format(user, "~p/~p(~p)~n~p =/= ~p~n",
                             [L, Len, P, BRes, LRes]),
                   almost_equal(BRes, LRes))
            end).

percentile(Perc, List) ->
    Size = length(List),
    Element = max(1, round(Perc * Size)),
    lists:nth(Element, List).

prop_avg_all() ->
    ?FORALL(L, non_empty_number_list(),
            begin
                BRes = to_list_d(mmath_aggr:avg(mmath_bin:realize(?L2B(L)), length(L))),
                LRes = [lists:sum(L) / length(L)],
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [BRes, LRes]),
                   almost_equal(BRes, LRes))
            end).

prop_avg_len() ->
    ?FORALL({L, N}, {non_empty_number_list(), pos_int()},
            ceiling(length(L)/N) == len_r(mmath_aggr:avg(mmath_bin:realize(?L2B(L)), N))).

prop_combine_avg_N() ->
    ?FORALL({{_, _, A}, N}, {defined_number_array(), pos_int()},
            begin
                LRes = to_list_d(mmath_trans:mul(A, 1)),
                BRes = to_list_d(mmath_comb:avg([A || _ <- lists:seq(1, N+1)])),
                ?WHENFAIL(
                   io:format(user, "avg(~p*~p) -> ~p =/= ~p~n",
                             [A, N, LRes, BRes]),
                   almost_equal(LRes, BRes))
            end).


prop_avg_impl() ->
    ?FORALL({{L0, _, B}, N}, {fully_defined_number_array(), pos_int()},
            begin
                L = realise(L0),
                LRes = avg(L, N),
                BRes = to_list_d(mmath_aggr:avg(B, N)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [LRes, BRes]),
                   almost_equal(LRes, BRes))
            end).

prop_avg_len_undefined() ->
    ?FORALL({L, N}, {non_neg_int(), pos_int()},
            ceiling(L/N) == len_r(mmath_aggr:avg(empty_r(L), N))).

prop_sum() ->
    ?FORALL({{L0, _, B}, N}, {defined_number_array(), pos_int()},
            begin
                L = realise(L0),
                LRes = sum(L, N),
                BRes = to_list_d(mmath_aggr:sum(B, N)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [LRes, BRes]),
                   almost_equal(LRes, BRes))
            end).


prop_sum_len_undefined() ->
    ?FORALL({L, N}, {non_neg_int(), pos_int()},
            ceiling(L/N) == len_r(mmath_aggr:sum(empty_r(L), N))).

%% We need to know about unset values for min!
prop_min() ->
    ?FORALL({{L0, _, B}, N}, {defined_number_array(), pos_int()},
            begin
                L = realise(L0),
                almost_equal(min_list(L, N), to_list_d(mmath_aggr:min(B, N)))
            end).

prop_min_len_undefined() ->
    ?FORALL({L, N}, {non_neg_int(), pos_int()},
            ceiling(L/N) == len_r(mmath_aggr:min(empty_r(L), N))).

%% We need to know about unset values for min!
prop_max() ->
    ?FORALL({{L0, _, B}, N}, {defined_number_array(), pos_int()},
            begin
                L = realise(L0),
                LRes = max_list(L, N),
                BRes = to_list_d(mmath_aggr:max(B, N)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [LRes, BRes]),
                   almost_equal(LRes, BRes))
            end).

prop_max_len_undefined() ->
    ?FORALL({L, N}, {non_neg_int(), pos_int()},
            begin
                LRes = ceiling(L/N),
                BRes = len_r(mmath_aggr:max(empty_r(L), N)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [LRes, BRes]),
                   LRes == BRes)
            end).

%% TODO: fix floating point arithmetic for large test samples
prop_stddev() ->
    ?FORALL({{L0, _, B}, N}, {fully_defined_number_array(), pos_int()},
            begin
                L = realise(L0),
                LRes = stddev(L, N),
                BRes = to_list_d(mmath_aggr:stddev(B, N)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [LRes, BRes]),
                   almost_equal(LRes, BRes))
            end).

prop_first_below() ->
    ?FORALL({{L0, _, B}, N, T}, {defined_number_array(), pos_int(), pos_int()},
            begin
                Threshold = T + 0.0,
                L = realise(L0),
                LRes = first_below_(L, N, Threshold),
                BRes = to_list_d(mmath_aggr:first_below(B, Threshold, N)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [LRes, BRes]),
                   almost_equal(LRes, BRes))
            end).

prop_first_above() ->
    ?FORALL({{L0, _, B}, N, T}, {defined_number_array(), pos_int(), pos_int()},
            begin
                Threshold = T + 0.0,
                L = realise(L0),
                LRes = first_above_(L, N, Threshold),
                BRes = to_list_d(mmath_aggr:first_above(B, Threshold, N)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [LRes, BRes]),
                   almost_equal(LRes, BRes))
            end).

prop_last_below() ->
    ?FORALL({{L0, _, B}, N, T}, {defined_number_array(), pos_int(), pos_int()},
            begin
                Threshold = T + 0.0,
                L = realise(L0),
                LRes = last_below_(L, N, Threshold),
                BRes = to_list_d(mmath_aggr:last_below(B, Threshold, N)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [LRes, BRes]),
                   almost_equal(LRes, BRes))
            end).

prop_last_above() ->
    ?FORALL({{L0, _, B}, N, T}, {defined_number_array(), pos_int(), pos_int()},
            begin
                Threshold = T + 0.0,
                L = realise(L0),
                LRes = last_above_(L, N, Threshold),
                BRes = to_list_d(mmath_aggr:last_above(B, Threshold, N)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [LRes, BRes]),
                   almost_equal(LRes, BRes))
            end).

prop_count_above() ->
    ?FORALL({{L0, _, B}, N, T}, {defined_number_array(), pos_int(), pos_int()},
            begin
                Threshold = T + 0.0,
                L = realise(L0),
                LRes = count_above_(L, N, Threshold),
                BRes = to_list_d(mmath_aggr:count_above(B, Threshold, N)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [LRes, BRes]),
                   almost_equal(LRes, BRes))
            end).

prop_count_below() ->
    ?FORALL({{L0, _, B}, N, T}, {defined_number_array(), pos_int(), pos_int()},
            begin
                Threshold = T + 0.0,
                L = realise(L0),
                LRes = count_below_(L, N, Threshold),
                BRes = to_list_d(mmath_aggr:count_below(B, Threshold, N)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [LRes, BRes]),
                   almost_equal(LRes, BRes))
            end).

%% prop_combine_sum_r_comp() ->
%%     ?FORALL({{_, _, B1}, {_, _, B2}}, {fully_defined_number_array(), fully_defined_number_array()},
%%             begin
%%                 RB1 = mmath_bin:realize(B1),
%%                 RB2 = mmath_bin:realize(B2),

%%                 BRes = mmath_comb:sum([B1, B2]),
%%                 RRes = mmath_comb:sum_r([RB1, RB2]),

%%                 RRes2 = mmath_bin:derealize(RRes),
%%                 ?WHENFAIL(
%%                    io:format(user, "~p =/= ~p~n",
%%                              [?B2L(BRes), ?B2L(RRes2)]),
%%                    almost_equal(?B2L(BRes), ?B2L(RRes2)))
%%             end).

%% prop_map() ->
%%     ?FORALL({{_, L, B}, S}, {defined_number_array(), real()},
%%                      begin
%%                              Scale = fun(V) -> V * S end,
%%                 LRes = scale_n(L, S),
%%                 BRes = mmath_bin:to_list(mmath_aggr:map(B, Scale)),
%%             ?WHENFAIL(
%%                io:format(user, "~p =/= ~p~n",
%%                          [LRes, BRes]),
%%                almost_equal(LRes, BRes))
%%                      end).

%% prop_scale() ->
%%     ?FORALL({{_, L, B}, S}, {defined_number_array(), real()},
%%             begin
%%                 LRes = scale_n(L, S),
%%                 BRes = mmath_bin:to_list(mmath_aggr:scale(B,S)),
%%             ?WHENFAIL(
%%                io:format(user, "~p =/= ~p~n",
%%                          [LRes, BRes]),
%%                almost_equal(LRes, BRes))
%%             end).

%% prop_scale_len_undefined() ->
%%     ?FORALL(L, non_neg_int(),
%%             L == mmath_bin:length(mmath_aggr:scale(empty_r(L), 1))).

%% prop_combine_avg2_r_comp() ->
%%     ?FORALL({{_, _, B1}, {_, _, B2}}, {fully_defined_number_array(), fully_defined_number_array()},
%%             begin
%%                 RB1 = mmath_bin:realize(B1),
%%                 RB2 = mmath_bin:realize(B2),

%%                 BRes = mmath_comb:avg([B1, B2]),
%%                 RRes = mmath_comb:avg_r([RB1, RB2]),

%%                 RRes2 = mmath_bin:derealize(RRes),
%%                 ?WHENFAIL(
%%                    io:format(user, "~p =/= ~p~n",
%%                              [?B2L(BRes), ?B2L(RRes2)]),
%%                    ?B2L(BRes) == ?B2L(RRes2))
%%             end).

%% prop_combine_avg3_r_comp() ->
%%     ?FORALL({{_, _, B1}, {_, _, B2}, {_, _, B3}},
%%             {fully_defined_number_array(), fully_defined_number_array(), fully_defined_number_array()},
%%             begin
%%                 RB1 = mmath_bin:realize(B1),
%%                 RB2 = mmath_bin:realize(B2),
%%                 RB3 = mmath_bin:realize(B3),

%%                 BRes = mmath_comb:avg([B1, B2, B3]),
%%                 RRes = mmath_comb:avg_r([RB1, RB2, RB3]),

%%                 RRes2 = mmath_bin:derealize(RRes),
%%                 ?WHENFAIL(
%%                    io:format(user, "~p =/= ~p~n",
%%                              [?B2L(BRes), ?B2L(RRes2)]),
%%                    ?B2L(BRes) == ?B2L(RRes2))
%%             end).

%% prop_combine_percentile_int() ->
%%     ?FORALL({{L, _, A}, N, PRaw}, {defined_number_array(), pos_int(), choose(0, 1000)},
%%             begin
%%                 P = PRaw/1000,
%%                 Exp = percentile(L, N, P),
%%                 Act = mmath_bin:to_list(mmath_aggr:percentile(A, N, P)),
%%                 ?WHENFAIL(io:format(user, "Exp: ~p~nAct: ~p~n",
%%                                     [Exp, Act]),
%%                           almost_equal(Exp, Act))
%%             end).


to_list_d(X) ->
     mmath_bin:to_list(mmath_bin:derealize(X)).

empty_r(L) ->
    mmath_bin:realize(mmath_bin:empty(L)).

len_r(X) ->
    mmath_bin:length(mmath_bin:derealize(X)).


mul_n(L, S) ->
    [N * S || N <- L].

div_n(L, S) ->
    [N / S || N <- L].

avg(L, N) ->
    apply_n(L, N, fun avg_/2).

sum(L, N) ->
    apply_n(L, N, fun sum_/2).

stddev(L, N) ->
    apply_n(L, N, fun stddev_/2).

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

min_(L, _N) ->
    case lists:sort(L) of
        [] ->
            undefined;
        [S | _ ] ->
            S
    end.

max_(L, _N) ->
    case lists:sort(L) of
        [] ->
            undefined;
        L1 ->
            lists:last(L1)
    end.

stddev_(L, N) ->
    Avg = avg_(L, N),
    Deltas = lists:map(fun(E) ->
                               Diff = E - Avg,
                               Diff * Diff
                       end, L),
    Variance = avg_(Deltas, N),
    math:sqrt(Variance).

first_below_(L, N, T) ->
    Fun = fun(InnerL, _N) ->
        case lists:filter(fun(X) -> X < T end, InnerL) of
            [] ->
                0.0;
            [H | _] ->
                H
        end
    end,
    apply_n(L, N, Fun).

first_above_(L, N, T) ->
    Fun = fun(InnerL, _N) ->
        case lists:filter(fun(X) -> X > T end, InnerL) of
            [] ->
                0.0;
            [H | _] ->
                H
        end
    end,
    apply_n(L, N, Fun).

last_below_(L, N, T) ->
    Fun = fun(InnerL, _N) ->
        case lists:filter(fun(X) -> X < T end, lists:reverse(InnerL)) of
            [] ->
                0.0;
            [H | _] ->
                H
        end
    end,
    apply_n(L, N, Fun).

last_above_(L, N, T) ->
    Fun = fun(InnerL, _N) ->
        case lists:filter(fun(X) -> X > T end, lists:reverse(InnerL)) of
            [] ->
                0.0;
            [H | _] ->
                H
        end
    end,
    apply_n(L, N, Fun).

count_above_(L, N, T) ->
    Fun = fun(InnerL, _N) ->
        length(lists:filter(fun(X) -> X > T end, InnerL))
    end,
    apply_n(L, N, Fun).


count_below_(L, N, T) ->
    Fun = fun(InnerL, _N) ->
        length(lists:filter(fun(X) -> X < T end, InnerL))
    end,
    apply_n(L, N, Fun).

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
