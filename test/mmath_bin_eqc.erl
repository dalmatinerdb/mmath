-module(mmath_bin_eqc).

-ifdef(TEST).
-ifdef(EQC).

-include("../include/mmath.hrl").

-import(mmath_helper, [int_array/0, float_array/0, non_neg_int/0, pos_int/0,
                       i_or_f_list/0, i_or_f_array/0, out/1]).

-include_lib("fqc/include/fqc.hrl").

-compile(export_all).

non_obvious_list() ->
    oneof([
          ?LET({N, L}, {non_neg_int(), list(int())},
               oneof(
                 [{integer, <<(mmath_bin:empty(N))/binary, (mmath_bin:from_list(L))/binary>>} || L =/= []] ++
                     [{undefined, <<(mmath_bin:empty(N))/binary, (mmath_bin:from_list(L))/binary>>} || L == []])),
          ?LET({N, L}, {non_neg_int(), list(real())},
               oneof(
                 [{float, <<(mmath_bin:empty(N))/binary, (mmath_bin:from_list(L))/binary>>} || L =/= []] ++
                     [{undefined, <<(mmath_bin:empty(N))/binary, (mmath_bin:from_list(L))/binary>>} || L == []]))]).

prop_empty() ->
    ?FORALL(Length, non_neg_int(),
            byte_size(mmath_bin:empty(Length)) == Length*?DATA_SIZE).

prop_length() ->
    ?FORALL(Length, non_neg_int(),
            mmath_bin:length(mmath_bin:empty(Length)) == Length).

prop_l2b_b2l() ->
    ?FORALL(List, i_or_f_list(),
            List == ?B2L(?L2B(List))).

prop_b2l() ->
    ?FORALL({_, L, B}, i_or_f_array(),
            L == ?B2L(B)).

prop_find_type() ->
    ?FORALL({T, B}, non_obvious_list(),
            T == mmath_bin:find_type(B)).

-endif.
-endif.
