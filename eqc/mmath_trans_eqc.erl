-module(mmath_trans_eqc).

-include("../include/mmath.hrl").

-import(mmath_helper,
        [number_array/0, pos_int/0, non_neg_int/0, defined_number_array/0,
         non_empty_number_list/0, fully_defined_number_array/0, realise/1,
         realise/3, confidence/1, almost_equal/2]).

-include_lib("eqc/include/eqc.hrl").

-export([prop_der/0,
         prop_sqrt_scale/0,
         prop_abs/0,
         prop_log10_scale/0,
         prop_der_len_undefined/0,
         prop_add/0,
         prop_minc/0,
         prop_maxc/0,
         prop_sub/0,
         prop_mul/0,
         prop_div/0,
         prop_combine_sum_identity/0,
         prop_combine_sum_N/0,
         prop_ceiling/0]).

prop_der() ->
    ?FORALL({L, _, B}, defined_number_array(),
            begin
                LRes = derivate(L),
                BRes = to_list_d(mmath_trans:derivate(B)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [LRes, BRes]),
                   almost_equal(LRes, BRes))
            end).

prop_sqrt_scale() ->
    ?FORALL({L, _, B}, defined_number_array(),
            begin
                LRes = sqrt_scale(L),
                BRes = to_list_d(mmath_trans:sqrt_scale(B)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [LRes, BRes]),
                   almost_equal(LRes, BRes))
            end).

prop_abs() ->
    ?FORALL({L, _, B}, defined_number_array(),
            begin
                LRes = abs_(L),
                BRes = to_list_d(mmath_trans:abs(B)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [LRes, BRes]),
                   almost_equal(LRes, BRes))
            end).

prop_log10_scale() ->
    ?FORALL({L, _, B}, defined_number_array(),
            begin
                LRes = log10_scale(L),
                BRes = to_list_d(mmath_trans:log10_scale(B)),
                ?WHENFAIL(
                   io:format(user, "~p =/= ~p~n",
                             [LRes, BRes]),
                   almost_equal(LRes, BRes))
            end).

prop_der_len_undefined() ->
    ?FORALL(L, non_neg_int(),
            L == len_r(mmath_trans:derivate(empty_r(L)))).

prop_add() ->
    ?FORALL({{L0, _, B}, S}, {defined_number_array(), int()},
            begin
                L = realise(L0),
                LRes = add_n(L, S),
                BRes = to_list_d(mmath_trans:add(B,S)),
            ?WHENFAIL(
               io:format(user, "~p =/= ~p~n",
                         [LRes, BRes]),
               almost_equal(LRes, BRes))
            end).


prop_minc() ->
    ?FORALL({{_L0, _, B}, S}, {defined_number_array(), int()},
            begin
                BRes = to_list_d(mmath_trans:min(B, S)),
                Bres1 = [min(S, E) || E <- BRes],
            ?WHENFAIL(
               io:format(user, "~p =/= ~p~n",
                         [BRes, Bres1]),
               almost_equal(BRes, Bres1))
            end).

prop_maxc() ->
    ?FORALL({{_L0, _, B}, S}, {defined_number_array(), int()},
            begin
                BRes = to_list_d(mmath_trans:max(B, S)),
                Bres1 = [max(S, E) || E <- BRes],
            ?WHENFAIL(
               io:format(user, "~p =/= ~p~n",
                         [BRes, Bres1]),
               almost_equal(BRes, Bres1))
            end).

prop_sub() ->
    ?FORALL({{L0, _, B}, S}, {defined_number_array(), int()},
            begin
                L = realise(L0),
                LRes = sub_n(L, S),
                BRes = to_list_d(mmath_trans:sub(B,S)),
            ?WHENFAIL(
               io:format(user, "~p =/= ~p~n",
                         [LRes, BRes]),
               almost_equal(LRes, BRes))
            end).

prop_mul() ->
    ?FORALL({{L0, _, B}, S}, {defined_number_array(), int()},
            begin
                L = realise(L0),
                LRes = mul_n(L, S),
                BRes = to_list_d(mmath_trans:mul(B,S)),
            ?WHENFAIL(
               io:format(user, "~p =/= ~p~n",
                         [LRes, BRes]),
               almost_equal(LRes, BRes))
            end).

prop_div() ->
    ?FORALL({{L0, _, B}, S}, {defined_number_array(), pos_int()},
            begin
                L = realise(L0),
                LRes = div_n(L, S),
                BRes = to_list_d(mmath_trans:divide(B,S)),
            ?WHENFAIL(
               io:format(user, "~p =/= ~p~n",
                         [LRes, BRes]),
               almost_equal(LRes, BRes))
            end).

prop_combine_sum_identity() ->
    ?FORALL({_, _, A}, defined_number_array(),
            mmath_comb:sum([A]) == A).

prop_combine_sum_N() ->
    ?FORALL({{_, _, A}, N}, {defined_number_array(), pos_int()},
            begin
                LRes = to_list_d(mmath_trans:mul(A, N)),
                BRes = to_list_d(mmath_comb:sum([A || _ <- lists:seq(1, N)])),
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
%% 			begin
%% 				Scale = fun(V) -> V * S end,
%%                 LRes = scale_n(L, S),
%%                 BRes = mmath_bin:to_list(mmath_aggr:map(B, Scale)),
%%             ?WHENFAIL(
%%                io:format(user, "~p =/= ~p~n",
%%                          [LRes, BRes]),
%%                almost_equal(LRes, BRes))
%% 			end).

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


add_n(L, S) ->
    [N + S || N <- L].

sub_n(L, S) ->
    [N - S || N <- L].

mul_n(L, S) ->
    [N * S || N <- L].

div_n(L, S) ->
    [N / S || N <- L].


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
%% Handle the case where we got a 1 element list,
%% we default to zero.
derivate(_, [], []) ->
    [0];
derivate(_, [], Acc) ->
    [ H | T] = lists:reverse(Acc),
    %% We need to ensure that derivate(X) has the
    %% same lenght as X, we do this by including
    %% the first element once
    [H, H | T].

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

sqrt_scale([{false, _}|L]) ->
    sqrt_scale([{true, find_first(L)} | L], 0, []);
sqrt_scale(L) ->
    sqrt_scale(L, 0, []).

sqrt_scale([], _, Acc) ->
    lists:reverse(Acc);
sqrt_scale([{true, X} | R], _, Acc) when X < 0->
    sqrt_scale(R, X * -1, [math:sqrt(X * -1) * -1 | Acc]);
sqrt_scale([{true, X} | R], _, Acc) ->
    sqrt_scale(R, X, [math:sqrt(X) | Acc]);
sqrt_scale([{false, _} | R], X, Acc) ->
    sqrt_scale(R, X, [math:sqrt(X) | Acc]).

log10_scale([{false, _}|L]) ->
    log10_scale([{true, find_first(L)} | L], 0, []);
log10_scale(L) ->
    log10_scale(L, 0, []).

log10_scale([], _, Acc) ->
    lists:reverse(Acc);
log10_scale([{true, X} | R], _, Acc) when X == 0->
    log10_scale(R, 0, [0 | Acc]);
log10_scale([{true, X} | R], _, Acc) when X < 0->
    log10_scale(R, X * -1, [math:log10(X * -1) * -1 | Acc]);
log10_scale([{true, X} | R], _, Acc) ->
    log10_scale(R, X, [math:log10(X) | Acc]);
log10_scale([{false, _} | R], 0, Acc) ->
    log10_scale(R, 0, [0 | Acc]);
log10_scale([{false, _} | R], X, Acc) ->
    log10_scale(R, X, [math:log10(X) | Acc]).

abs_([{false, _}|L]) ->
    abs_([{true, find_first(L)} | L], 0, []);
abs_(L) ->
    abs_(L, 0, []).

abs_([], _, Acc) ->
    lists:reverse(Acc);
abs_([{true, X} | R], _, Acc) ->
    abs_(R, X, [abs(X) | Acc]);
abs_([{false, _} | R], X, Acc) ->
    abs_(R, X, [abs(X) | Acc]).

