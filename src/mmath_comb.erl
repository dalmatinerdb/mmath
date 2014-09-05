-module(mmath_comb).
-include("mmath.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-export([avg/1, sum/1, mul/1, merge/1, zip/2]).

sum(Es) ->
    rcomb(fun sum/2, Es, []).

avg(Es) ->
    mmath_aggr:scale(sum(Es), 1/length(Es)).

merge(Es) ->
    rcomb(fun merge/2, Es, []).

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

mul(Es) ->
    rcomb(fun mul/2, Es, []).

mul(A,B) ->
    mul(A, B, 1, 1, <<>>).

mul(<<>>, <<>>, _LA, _LB, Acc) ->
    Acc;
mul(<<?INT, A:?BITS/signed-integer, RA/binary>>,
    <<?INT, B:?BITS/signed-integer, RB/binary>>, _LA, _LB, Acc) ->
    mul(RA, RB, A, B, <<Acc/binary, ?INT, (A*B):?BITS/signed-integer>>);
mul(<<?INT, A:?BITS/signed-integer, RA/binary>>,
    <<?NONE, _:?BITS/signed-integer, RB/binary>>, _LA, LB, Acc) ->
    mul(RA, RB, A, LB, <<Acc/binary, ?INT, (A*LB):?BITS/signed-integer>>);
mul(<<?NONE, _A:?BITS/signed-integer, RA/binary>>,
    <<?INT, B:?BITS/signed-integer, RB/binary>>, LA, _LB, Acc) ->
    mul(RA, RB, LA, B, <<Acc/binary, ?INT, (LA*B):?BITS/signed-integer>>);
mul(<<?NONE, _A:?BITS/signed-integer, RA/binary>>,
    <<?NONE, _B:?BITS/signed-integer, RB/binary>>, LA, LB, Acc) ->
    mul(RA, RB, LA, LB, <<Acc/binary, ?INT, (LA*LB):?BITS/signed-integer>>).


zip(Fn, Es) ->
    rcomb(fun (A, B) -> zip(Fn, A, B) end, Es, []).

zip(Fn, A, B) ->
    zip(A, B, 1, 1, Fn, <<>>).

zip(<<>>, <<>>, _LA, _LB, _Fn, Acc) ->
    Acc;
zip(<<?INT, A:?BITS/signed-integer, RA/binary>>,
    <<?INT, B:?BITS/signed-integer, RB/binary>>, _LA, _LB, Fn, Acc) ->
    zip(RA, RB, A, B, Fn, apply(Fn, A, B, Acc));
zip(<<?FLOAT, A:?BITS/float, RA/binary>>,
    <<?INT, B:?BITS/signed-integer, RB/binary>>, _LA, _LB, Fn, Acc) ->
    zip(RA, RB, A, B, Fn, apply(Fn, A, B, Acc));
zip(<<?INT, A:?BITS/signed-integer, RA/binary>>,
    <<?FLOAT, B:?BITS/float, RB/binary>>, _LA, _LB, Fn, Acc) ->
    zip(RA, RB, A, B, Fn, apply(Fn, A, B, Acc));
zip(<<?FLOAT, A:?BITS/float, RA/binary>>,
    <<?FLOAT, B:?BITS/float, RB/binary>>, _LA, _LB, Fn, Acc) ->
    zip(RA, RB, A, B, Fn, apply(Fn, A, B, Acc));

zip(<<?INT, A:?BITS/signed-integer, RA/binary>>,
    <<?NONE, _:?BITS/signed-integer, RB/binary>>, _LA, LB, Fn, Acc) ->
    zip(RA, RB, A, LB, Fn,  apply(Fn, A, LB, Acc));
zip(<<?FLOAT, A:?BITS/float, RA/binary>>,
    <<?NONE, _:?BITS/signed-integer, RB/binary>>, _LA, LB, Fn, Acc) ->
    zip(RA, RB, A, LB, Fn,  apply(Fn, A, LB, Acc));
zip(<<?NONE, _A:?BITS/signed-integer, RA/binary>>,
    <<?INT, B:?BITS/signed-integer, RB/binary>>, LA, _LB, Fn, Acc) ->
    zip(RA, RB, LA, B, Fn, apply(Fn, LA, B, Acc));
zip(<<?NONE, _A:?BITS/signed-integer, RA/binary>>,
    <<?FLOAT, B:?BITS/float, RB/binary>>, LA, _LB, Fn, Acc) ->
    zip(RA, RB, LA, B, Fn, apply(Fn, LA, B, Acc));
zip(<<?NONE, _A:?BITS/signed-integer, RA/binary>>,
    <<?NONE, _B:?BITS/signed-integer, RB/binary>>, LA, LB, Fn, Acc) ->
    zip(RA, RB, LA, LB, Fn, apply(Fn, LA, LB, Acc)).

apply(Fn, A, B, Acc) ->
	case Fn(A, B) of
		V when is_integer(V) ->
			<<Acc/binary, ?INT, (A*B):?BITS/signed-integer>>;
		V when is_float(V) ->
			<<Acc/binary, ?FLOAT, (A*B):?BITS/signed-float>>
	end.

merge(A, B) ->
    merge(A, B, <<>>).

merge(<<?NONE, _:?BITS/signed-integer, R1/binary>>,
        <<D:?DATA_SIZE/binary, R2/binary>>,
        Acc) ->
    merge(R1, R2, <<Acc/binary, D/binary>>);
merge(<<D:?DATA_SIZE/binary, R1/binary>>,
        <<_:?DATA_SIZE/binary, R2/binary>>,
        Acc) ->
    merge(R1, R2, <<Acc/binary, D/binary>>);
merge(<<>>, <<>>, Acc) ->
    Acc;

merge(<<>>, D, Acc) ->
    <<Acc/binary, D/binary>>;
merge(D, <<>>, Acc) ->
    <<Acc/binary, D/binary>>.

rcomb(F, [A], [B]) ->
    F(A, B);
rcomb(F, [], Acc) ->
    rcomb(F, Acc, []);
rcomb(_, [A], []) ->
    A;
rcomb(F, [A, B, C, D | R], Acc) ->
    rcomb(F, R, [F(F(A, B), F(C, D)) | Acc]);
rcomb(F, [A, B | R], Acc) ->
    rcomb(F, R, [F(A, B) | Acc]);
rcomb(F, [A], Acc) ->
    rcomb(F, [A | Acc], []).
