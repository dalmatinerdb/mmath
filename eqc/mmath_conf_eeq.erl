-module(mmath_conf_eeq).

-include("../include/mmath.hrl").

-import(mmath_helper,
        [int_array/0, pos_int/0, epsilon/3, confidence/1]).

-import(mmath_aggr_eqc,
        [avg/2]).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).



prop_avg_r_conf() ->
    ?FORALL({{L, _, B}, N}, {int_array(), pos_int()},
            begin
                ConfL = confidence(L),
                ConfLPad = pad_to_n(ConfL, N),
                CAvgL = avg(ConfLPad, N),
                R = mmath_bin:realize(B),
                A = mmath_aggr:avg_r(R, N),
                C = mmath_bin:confidence_r(A),
                ConfR = ?B2L(mmath_bin:derealize(C)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [CAvgL, ConfR]),
                   %% we adjust epsilon since the confidence function
                   %% only returns 3 digets after the decimal point
                   epsilon(CAvgL, ConfR, 0.001))
            end).

prop_sum_r_conf() ->
    ?FORALL({{L, _, B}, N}, {int_array(), pos_int()},
            begin
                ConfL = confidence(L),
                ConfLPad = pad_to_n(ConfL, N),
                CAvgL = avg(ConfLPad, N),
                R = mmath_bin:realize(B),
                A = mmath_aggr:sum_r(R, N),
                C = mmath_bin:confidence_r(A),
                ConfR = ?B2L(mmath_bin:derealize(C)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [CAvgL, ConfR]),
                   %% we adjust epsilon since the confidence function
                   %% only returns 3 digets after the decimal point
                   epsilon(CAvgL, ConfR, 0.001))
            end).
prop_max_r_conf() ->
    ?FORALL({{L, _, B}, N}, {int_array(), pos_int()},
            begin
                ConfL = confidence(L),
                ConfLPad = pad_to_n(ConfL, N),
                CAvgL = avg(ConfLPad, N),
                R = mmath_bin:realize(B),
                A = mmath_aggr:max_r(R, N),
                C = mmath_bin:confidence_r(A),
                ConfR = ?B2L(mmath_bin:derealize(C)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [CAvgL, ConfR]),
                   %% we adjust epsilon since the confidence function
                   %% only returns 3 digets after the decimal point
                   epsilon(CAvgL, ConfR, 0.001))
            end).

prop_min_r_conf() ->
    ?FORALL({{L, _, B}, N}, {int_array(), pos_int()},
            begin
                ConfL = confidence(L),
                ConfLPad = pad_to_n(ConfL, N),
                CAvgL = avg(ConfLPad, N),
                R = mmath_bin:realize(B),
                A = mmath_aggr:min_r(R, N),
                C = mmath_bin:confidence_r(A),
                ConfR = ?B2L(mmath_bin:derealize(C)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [CAvgL, ConfR]),
                   %% we adjust epsilon since the confidence function
                   %% only returns 3 digets after the decimal point
                   epsilon(CAvgL, ConfR, 0.001))
            end).

prop_comb_sum2_r_conf() ->
    ?FORALL({{L1, _, B1}, {L2, _, B2}},
            {int_array(), int_array()},
            begin
                N = max(length(L1), length(L2)),
                ConfL1 = confidence(L1),
                ConfL2 = confidence(L2),
                ConfL1Pad = pad_to_n(ConfL1, N),
                ConfL2Pad = pad_to_n(ConfL2, N),
                ConfL1PadS = [{true, V} || V <- ConfL1Pad],
                ConfL2PadS = [{true, V} || V <- ConfL2Pad],
                CAvgL = mmath_comb_eqc:avg(ConfL1PadS, ConfL2PadS),
                R1 = mmath_bin:realize(B1),
                R2 = mmath_bin:realize(B2),
                Comb = mmath_comb:sum_r([R1, R2]),
                C = mmath_bin:confidence_r(Comb),
                ConfR = ?B2L(mmath_bin:derealize(C)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [CAvgL, ConfR]),
                   %% we adjust epsilon since the confidence function
                   %% only returns 3 digets after the decimal point
                   epsilon(CAvgL, ConfR, 0.001))
            end).

%% yes this is bad, so what?!?
pad_to_n(_L, 0) ->
    [];
pad_to_n(L, N) when (length(L) rem N) == 0 ->
    L;
pad_to_n(L, N) ->
    pad_to_n(L ++ [0], N).
