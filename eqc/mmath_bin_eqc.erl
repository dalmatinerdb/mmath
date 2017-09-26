-module(mmath_bin_eqc).

-include("../include/mmath.hrl").

-import(mmath_helper, [number_array/0, pos_int/0, non_neg_int/0,
                       supported_number/0, defined_number_array/0,
                       raw_number_array/0, fully_defined_number_array/0, from_decimal/1,
                       realise/1, confidence/1, almost_equal/2,
                       within_epsilon/3, almost_equal/2]).

-include_lib("eqc/include/eqc.hrl").

-export([prop_empty/0,
         prop_length/0,
         prop_length_r/0,
         prop_l2b_b2l/0,
         prop_b2l/0,
         prop_l2b/0,
         prop_realize_derealize/0,
         prop_realize/0,
         prop_deserialize_v01/0,
         prop_deserialize_v02alpha/0,
         prop_deserialize_v02/0]).

prop_empty() ->
    ?FORALL(Length, non_neg_int(),
            byte_size(mmath_bin:empty(Length)) == Length*?DATA_SIZE).

prop_length() ->
    ?FORALL(Length, non_neg_int(),
            mmath_bin:length(mmath_bin:empty(Length)) == Length).

prop_length_r() ->
    ?FORALL(Length, non_neg_int(),
            begin
                R = mmath_bin:realize(mmath_bin:empty(Length)),
                mmath_bin:length_r(R) == Length
            end).

prop_l2b_b2l() ->
    ?FORALL(List, list(supported_number()),
            almost_equal(List, ?B2L(?L2B(List)))).

prop_b2l() ->
    ?FORALL({L0, _, B}, number_array(),
            almost_equal(realise(L0), ?B2L(mmath_bin:derealize(B)))).

prop_l2b() ->
    ?FORALL({_, L, B}, fully_defined_number_array(),
            begin
                B1 = mmath_bin:realize(?L2B(L)),
                L1 = ?B2L(mmath_bin:derealize(B)),
                L2 = ?B2L(mmath_bin:derealize(B1)),
                ?WHENFAIL(io:format(user, "~p =/= ~p~n",
                                    [L1, L2]),
                          almost_equal(L1, L2))
            end).

prop_realize_derealize() ->
    ?FORALL({L, _, B}, number_array(),
            begin
                Exp = realise(L),
                Calc = ?B2L(mmath_bin:derealize(mmath_bin:realize(mmath_bin:derealize(B)))),
                ?WHENFAIL(io:format(user, "~p =/= ~p~n",
                                    [Exp, Calc]),
                          almost_equal(Exp, Calc))
            end).

prop_realize() ->
    ?FORALL({T, _, B}, defined_number_array(),
            begin
                %% This unpacking pattern will work on 64 bit machines only.
                L1 = [I || <<I:64/float-native, _C:32/unsigned-native, _:32>>
                                                  <= B],
                L = realise(T),
                ?WHENFAIL(io:format(user, "~p =/= ~p~n",
                                    [L, L1]),
                          almost_equal(L, L1))
            end).

%% Make sure it reads intagers
prop_deserialize_v01() ->
    ?FORALL(I, int(),
            begin
                L1 = [I],
                L = mmath_bin:to_list(<<1:8, I:56>>),
                ?WHENFAIL(io:format(user, "~p =/= ~p~n",
                                    [L, L1]),
                          L == L1)
            end).

%% Make sure it reads decimal values breiefly intorduced in v0.2alpha
prop_deserialize_v02alpha() ->
    ?FORALL({Coef, Exp}, {choose(round(-1.0e14), round(1.0e14)), choose(-128, 127)},
            begin
                L1 = [Coef * math:pow(10, Exp)],
                L = mmath_bin:to_list(<<2:8, Exp:8, Coef:48>>),
                ?WHENFAIL(io:format(user, "~p =/= ~p~n",
                                    [L, L1]),
                          almost_equal(L,L1))
            end).

%% Make sure it reads suqashed floats introduced in v0.2 proper
prop_deserialize_v02() ->
    ?FORALL(V, real(),
            begin
                L1 = [V],
                <<VH:63, _:1>> = <<V:64/float>>,
                L = mmath_bin:to_list(<<1:1, VH:63>>),
                ?WHENFAIL(io:format(user, "~p =/= ~p~n",
                                    [L, L1]),
                          within_epsilon(L, L1, math:pow(2, -52)))
            end).
