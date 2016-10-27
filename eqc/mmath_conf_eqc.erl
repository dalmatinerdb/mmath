-module(mmath_conf_eqc).

-include("../include/mmath.hrl").

-import(mmath_helper,
        [number_array/0, defined_number_array/0, pos_int/0, within_epsilon/3,
         confidence/1, pad_to_n/2, realise/1, almost_equal/2, to_list_d/1,
         apply_n/3]).

-import(mmath_aggr_eqc,
        [avg/2]).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

array_and_int() ->
    ?SUCHTHAT({{L, _, _}, _I},
              {number_array(), pos_int()},
              length(L) > 0).

prop_avg_conf() ->
    ?FORALL({{L, _, B}, N}, array_and_int(),
            begin
                ConfL = confidence(L),
                ConfLPad = pad_to_n(ConfL, N),
                CAvgL = avg(ConfLPad, N),
                A = mmath_aggr:avg(B, N),
                C = mmath_trans:confidence(A),
                ConfR = ?B2L(mmath_bin:derealize(C)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [CAvgL, ConfR]),
                   %% we adjust within_epsilon since the confidence function
                   %% only returns 3 digets after the decimal point
                   within_epsilon(CAvgL, ConfR, 0.000001))
            end).

prop_perc_conf() ->
    ?FORALL({{L, _, B}, N}, array_and_int(),
            begin
                ConfL = confidence(L),
                ConfLPad = pad_to_n(ConfL, N),
                CAvgL = avg(ConfLPad, N),
                A = mmath_aggr:percentile(B, 0.0, N),
                C = mmath_trans:confidence(A),
                ConfR = ?B2L(mmath_bin:derealize(C)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [CAvgL, ConfR]),
                   %% we adjust within_epsilon since the confidence function
                   %% only returns 3 digets after the decimal point
                   within_epsilon(CAvgL, ConfR, 0.000001))
            end).

prop_sum_conf() ->
    ?FORALL({{L, _, B}, N}, array_and_int(),
            begin
                ConfL = confidence(L),
                ConfLPad = pad_to_n(ConfL, N),
                CAvgL = avg(ConfLPad, N),
                A = mmath_aggr:sum(B, N),
                C = mmath_trans:confidence(A),
                ConfR = ?B2L(mmath_bin:derealize(C)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [CAvgL, ConfR]),
                   %% we adjust within_epsilon since the confidence function
                   %% only returns 3 digets after the decimal point
                   within_epsilon(CAvgL, ConfR, 0.000001))
            end).
prop_max_conf() ->
    ?FORALL({{L, _, B}, N}, array_and_int(),
            begin
                ConfL = confidence(L),
                ConfLPad = pad_to_n(ConfL, N),
                CAvgL = avg(ConfLPad, N),
                A = mmath_aggr:max(B, N),
                C = mmath_trans:confidence(A),
                ConfR = ?B2L(mmath_bin:derealize(C)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [CAvgL, ConfR]),
                   %% we adjust within_epsilon since the confidence function
                   %% only returns 3 digets after the decimal point
                   within_epsilon(CAvgL, ConfR, 0.000001))
            end).

prop_min_conf() ->
    ?FORALL({{L, _, B}, N}, array_and_int(),
            begin
                ConfL = confidence(L),
                ConfLPad = pad_to_n(ConfL, N),
                CAvgL = avg(ConfLPad, N),
                A = mmath_aggr:min(B, N),
                C = mmath_trans:confidence(A),
                ConfR = ?B2L(mmath_bin:derealize(C)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [CAvgL, ConfR]),
                   %% we adjust within_epsilon since the confidence function
                   %% only returns 3 digets after the decimal point
                   within_epsilon(CAvgL, ConfR, 0.000001))
            end).

prop_comb_sum2_conf() ->
    ?FORALL({{L1, _, B1}, {L2, _, B2}},
            {number_array(), number_array()},
            begin
                N = max(length(L1), length(L2)),
                ConfL1 = confidence(L1),
                ConfL2 = confidence(L2),
                ConfL1Pad = pad_to_n(ConfL1, N),
                ConfL2Pad = pad_to_n(ConfL2, N),
                CAvgL = mmath_comb_eqc:avg(ConfL1Pad, ConfL2Pad),
                Comb = mmath_comb:sum([B1, B2]),
                C = mmath_trans:confidence(Comb),
                ConfR = ?B2L(mmath_bin:derealize(C)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p (~p)~n",
                             [CAvgL, ConfR, {B1, B2}]),
                   %% we adjust within_epsilon since the confidence function
                   %% only returns 3 digits after the decimal point
                   within_epsilon(CAvgL, ConfR, 0.000001))
            end).

prop_confidence() ->
    ?FORALL({L, _, B}, number_array(),
            begin
                CExp = confidence(L),
                CCalc = ?B2L(mmath_bin:derealize(mmath_trans:confidence(B))),
                ?WHENFAIL(io:format(user, "~p =/= ~p~n",
                                    [CExp, CCalc]),
                          CExp == CCalc)
            end).

prop_first_above_conf() ->
    ?FORALL({{L0, _, B}, N, T}, {defined_number_array(), pos_int(), pos_int()},
            begin
                Threshold = T + 0.0,
                L = realise(L0),
                C = confidence(L0),
                LC = lists:zip(L, C),
                LRes = first_above_conf_(LC, N, Threshold),
                BRes = to_list_d(mmath_aggr:first_above_conf(B, Threshold, N)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [LRes, BRes]),
                   almost_equal(LRes, BRes))
            end).

prop_first_below_conf() ->
    ?FORALL({{L0, _, B}, N, T}, {defined_number_array(), pos_int(), pos_int()},
            begin
                Threshold = T + 0.0,
                L = realise(L0),
                C = confidence(L0),
                LC = lists:zip(L, C),
                LRes = first_below_conf_(LC, N, Threshold),
                BRes = to_list_d(mmath_aggr:first_below_conf(B, Threshold, N)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [LRes, BRes]),
                   almost_equal(LRes, BRes))
            end).

prop_last_above_conf() ->
    ?FORALL({{L0, _, B}, N, T}, {defined_number_array(), pos_int(), pos_int()},
            begin
                Threshold = T + 0.0,
                L = realise(L0),
                C = confidence(L0),
                LC = lists:zip(L, C),
                LRes = last_above_conf_(LC, N, Threshold),
                BRes = to_list_d(mmath_aggr:last_above_conf(B, Threshold, N)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [LRes, BRes]),
                   almost_equal(LRes, BRes))
            end).

prop_last_below_conf() ->
    ?FORALL({{L0, _, B}, N, T}, {defined_number_array(), pos_int(), pos_int()},
            begin
                Threshold = T + 0.0,
                L = realise(L0),
                C = confidence(L0),
                LC = lists:zip(L, C),
                LRes = last_below_conf_(LC, N, Threshold),
                BRes = to_list_d(mmath_aggr:last_below_conf(B, Threshold, N)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [LRes, BRes]),
                   almost_equal(LRes, BRes))
            end).

prop_count_above_conf() ->
    ?FORALL({{L0, _, B}, N, T}, {defined_number_array(), pos_int(), pos_int()},
            begin
                Threshold = T + 0.0,
                L = realise(L0),
                C = confidence(L0),
                LC = lists:zip(L, C),
                LRes = count_above_conf_(LC, N, Threshold),
                BRes = to_list_d(mmath_aggr:count_above_conf(B, Threshold, N)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [LRes, BRes]),
                   almost_equal(LRes, BRes))
            end).

prop_count_below_conf() ->
    ?FORALL({{L0, _, B}, N, T}, {defined_number_array(), pos_int(), pos_int()},
            begin
                Threshold = T + 0.0,
                L = realise(L0),
                C = confidence(L0),
                LC = lists:zip(L, C),
                LRes = count_below_conf_(LC, N, Threshold),
                BRes = to_list_d(mmath_aggr:count_below_conf(B, Threshold, N)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [LRes, BRes]),
                   almost_equal(LRes, BRes))
            end).

first_above_conf_(L, N, T) ->
    Fun = fun(InnerL, _N) ->
        case lists:filter(fun({_X, C}) -> C > T end, InnerL) of
            [] ->
                0.0;
            [{H, _C} | _] ->
                H
        end
    end,
    apply_n(L, N, Fun).

first_below_conf_(L, N, T) ->
    Fun = fun(InnerL, _N) ->
        case lists:filter(fun({_X, C}) -> C < T end, InnerL) of
            [] ->
                0.0;
            [{H, _C} | _] ->
                H
        end
    end,
    apply_n(L, N, Fun).

last_above_conf_(L, N, T) ->
    Fun = fun(InnerL, _N) ->
        case lists:filter(fun({_X, C}) -> C > T end, lists:reverse(InnerL)) of
            [] ->
                0.0;
            [{H, _C} | _] ->
                H
        end
    end,
    apply_n(L, N, Fun).

last_below_conf_(L, N, T) ->
    Fun = fun(InnerL, _N) ->
        case lists:filter(fun({_X, C}) -> C < T end, lists:reverse(InnerL)) of
            [] ->
                0.0;
            [{H, _C} | _] ->
                H
        end
    end,
    apply_n(L, N, Fun).

count_above_conf_(L, N, T) ->
    Fun = fun(InnerL, _N) ->
        length(lists:filter(fun({_X, C}) -> C > T end, InnerL))
    end,
    apply_n(L, N, Fun).

count_below_conf_(L, N, T) ->
    Fun = fun(InnerL, _N) ->
        length(lists:filter(fun({_X, C}) -> C < T end, InnerL))
    end,
    apply_n(L, N, Fun).
