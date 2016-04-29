-module(mmath_bin_eqc).

-include("../include/mmath.hrl").

-import(mmath_helper, [number_array/0, pos_int/0, non_neg_int/0,
                       supported_number/0, defined_number_array/0,
                       fully_defined_number_array/0, from_decimal/1,
                       realise/1, almost_equal/2]).

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
    ?FORALL({_, L, B}, number_array(),
            almost_equal(L, ?B2L(B))).

prop_l2b() ->
    ?FORALL({_, L, B}, fully_defined_number_array(),
            begin
                B1 = ?L2B(L),
                ?WHENFAIL(io:format(user, "~p =/= ~p~n",
                                    [B, B1]),
                          B == B1)
            end).

prop_realize_derealize() ->
    ?FORALL({L, _, B}, number_array(),
            begin
                LRes = realise(L),
                BRes = ?B2L(mmath_bin:derealize(mmath_bin:realize(B))),
                ?WHENFAIL(io:format(user, "~p =/= ~p~n",
                                    [LRes, BRes]),
                          almost_equal(LRes, BRes))
            end).

prop_realize() ->
    ?FORALL({T, _, B}, defined_number_array(),
            begin
                %% This unpacking pattern will work on 64 bit machines only.
                L1 = [from_decimal({I, E}) || <<I:64/signed-native-integer, E:8/signed-native-integer, _:56>> 
                                                  <= mmath_bin:realize(B)],
                L = realise(T),
                ?WHENFAIL(io:format(user, "~p =/= ~p~n",
                                    [L, L1]),
                          almost_equal(L, L1))
            end).

prop_complete_size_r() ->
    ?FORALL({L, N, Reminder}, {non_neg_int(), non_neg_int(), choose(0,7)},
            begin
                R = mmath_bin:realize(mmath_bin:empty(L * N)),
                B = <<R/binary, 0:(Reminder * 8)/integer>>,
                Reminder == size(B) - mmath_bin:complete_size_r(B, N)
            end).
