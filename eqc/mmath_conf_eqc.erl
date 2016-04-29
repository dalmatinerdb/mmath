-module(mmath_conf_eqc).

-include("../include/mmath.hrl").

-import(mmath_helper,
        [number_array/0, pos_int/0, almost_equal/3, confidence/1, pad_to_n/2]).

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
                   %% we adjust almost_equal since the confidence function
                   %% only returns 3 digets after the decimal point
                   almost_equal(CAvgL, ConfR, 0.001))
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
                   %% we adjust almost_equal since the confidence function
                   %% only returns 3 digets after the decimal point
                   almost_equal(CAvgL, ConfR, 0.001))
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
                   %% we adjust almost_equal since the confidence function
                   %% only returns 3 digets after the decimal point
                   almost_equal(CAvgL, ConfR, 0.001))
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
                   %% we adjust almost_equal since the confidence function
                   %% only returns 3 digets after the decimal point
                   almost_equal(CAvgL, ConfR, 0.001))
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
                   %% we adjust almost_equal since the confidence function
                   %% only returns 3 digits after the decimal point
                   almost_equal(CAvgL, ConfR, 0.001))
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
