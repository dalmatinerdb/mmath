-module(mmath_bin_eqc).

-include("../include/mmath.hrl").

-import(mmath_helper, [number_array/0, pos_int/0, non_neg_int/0,
                       supported_number/0, defined_number_array/0,
                       fully_defined_number_array/0, from_decimal/1,
                       realise/1, confidence/1, almost_equal/2]).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

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
                ?WHENFAIL(io:format(user, "~p =/= ~p~n",
                                    [B, B1]),
                          B == B1)
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
                L1 = [from_decimal({I, E}) || <<I:64/signed-native, _C:32/unsigned-native, E:8/signed-native, _:24>>
                                                  <= B],
                L = realise(T),
                ?WHENFAIL(io:format(user, "~p =/= ~p~n",
                                    [L, L1]),
                          almost_equal(L, L1))
            end).
