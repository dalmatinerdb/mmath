-module(mmath_comb).
-include("mmath.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-export([avg/1, sum/1]).

sum(Es) ->
    rsum(Es, []).

sum(A,B) ->
    sum(A, B, 0, 0, <<>>).

sum(<<>>, <<>>, _LA, _LB, Acc) ->
    Acc;
sum(<<?INT, A:?BITS/signed-integer, RA/binary>>,
    <<?INT, B:?BITS/signed-integer, RB/binary>>, _LA, _LB, Acc) ->
    sum(RA, RB, A, B, <<Acc/binary, ?INT, (A+B):?BITS/signed-integer>>);
sum(<<?INT, A:?BITS/signed-integer, RA/binary>>,
    <<?NONE, _:?BITS/signed-integer, RB/binary>>, _LA, LB, Acc) ->
    sum(RA, RB, A, LB, <<Acc/binary, ?INT, (A+LB):?BITS/signed-integer>>);
sum(<<?NONE, _A:?BITS/signed-integer, RA/binary>>,
    <<?INT, B:?BITS/signed-integer, RB/binary>>, LA, _LB, Acc) ->
    sum(RA, RB, LA, B, <<Acc/binary, ?INT, (LA+B):?BITS/signed-integer>>);
sum(<<?NONE, _A:?BITS/signed-integer, RA/binary>>,
    <<?NONE, _B:?BITS/signed-integer, RB/binary>>, LA, LB, Acc) ->
    sum(RA, RB, LA, LB, <<Acc/binary, ?INT, (LA+LB):?BITS/signed-integer>>).


avg(Es) ->
    mmath_aggr:scale(sum(Es), 1/length(Es)).

rsum([A], [B]) ->
    sum(A, B);
rsum([], Acc) ->
    rsum(Acc, []);
rsum([A], []) ->
    A;
rsum([A, B, C, D | R], Acc) ->
    rsum(R, [sum(sum(A, B), sum(C, D)) | Acc]);
rsum([A, B | R], Acc) ->
    rsum(R, [sum(A, B) | Acc]);
rsum([A], Acc) ->
    rsum([A | Acc], []).
