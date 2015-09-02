-module(mmath_bin_eqc).

-include("../include/mmath.hrl").

-import(mmath_helper, [int_array/0, pos_int/0, non_neg_int/0, defined_int_array/0,
                       non_empty_i_list/0]).

-include_lib("eqc/include/eqc.hrl").
-include_lib("fqc/include/fqci.hrl").

-compile(export_all).

prop_empty() ->
    ?FORALL(Length, non_neg_int(),
            byte_size(mmath_bin:empty(Length)) == Length*?DATA_SIZE).

prop_length() ->
    ?FORALL(Length, non_neg_int(),
            mmath_bin:length(mmath_bin:empty(Length)) == Length).

prop_l2b_b2l() ->
    ?FORALL(List, list(int()),
            List == ?B2L(?L2B(List))).

prop_b2l() ->
    ?FORALL({_, L, B}, int_array(),
            L == ?B2L(B)).

prop_realize_derealize() ->
    ?FORALL({_, _, B}, int_array(),
            ?B2L(B) == ?B2L(mmath_bin:derealize(mmath_bin:realize(B)))).
