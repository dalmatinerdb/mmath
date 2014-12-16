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

sum(A, B) ->
    sum(A, B, 0, 0, <<>>).

%% Optimistic case that will combine 10 values of data at once.
sum(<<?INT:?TYPE_SIZE, A0:?BITS/?INT_TYPE,
      ?INT:?TYPE_SIZE, A1:?BITS/?INT_TYPE,
      ?INT:?TYPE_SIZE, A2:?BITS/?INT_TYPE,
      ?INT:?TYPE_SIZE, A3:?BITS/?INT_TYPE,
      ?INT:?TYPE_SIZE, A4:?BITS/?INT_TYPE,
      ?INT:?TYPE_SIZE, A5:?BITS/?INT_TYPE,
      ?INT:?TYPE_SIZE, A6:?BITS/?INT_TYPE,
      ?INT:?TYPE_SIZE, A7:?BITS/?INT_TYPE,
      ?INT:?TYPE_SIZE, A8:?BITS/?INT_TYPE,
      ?INT:?TYPE_SIZE, A9:?BITS/?INT_TYPE,
      RA/binary>>,
    <<?INT:?TYPE_SIZE, B0:?BITS/?INT_TYPE,
      ?INT:?TYPE_SIZE, B1:?BITS/?INT_TYPE,
      ?INT:?TYPE_SIZE, B2:?BITS/?INT_TYPE,
      ?INT:?TYPE_SIZE, B3:?BITS/?INT_TYPE,
      ?INT:?TYPE_SIZE, B4:?BITS/?INT_TYPE,
      ?INT:?TYPE_SIZE, B5:?BITS/?INT_TYPE,
      ?INT:?TYPE_SIZE, B6:?BITS/?INT_TYPE,
      ?INT:?TYPE_SIZE, B7:?BITS/?INT_TYPE,
      ?INT:?TYPE_SIZE, B8:?BITS/?INT_TYPE,
      ?INT:?TYPE_SIZE, B9:?BITS/?INT_TYPE,
      RB/binary>>,
    _LA, _LB, Acc) ->
    Acc1 = <<Acc/binary,
             ?INT:?TYPE_SIZE, (A0+B0):?BITS/?INT_TYPE,
             ?INT:?TYPE_SIZE, (A1+B1):?BITS/?INT_TYPE,
             ?INT:?TYPE_SIZE, (A2+B2):?BITS/?INT_TYPE,
             ?INT:?TYPE_SIZE, (A3+B3):?BITS/?INT_TYPE,
             ?INT:?TYPE_SIZE, (A4+B4):?BITS/?INT_TYPE,
             ?INT:?TYPE_SIZE, (A5+B5):?BITS/?INT_TYPE,
             ?INT:?TYPE_SIZE, (A6+B6):?BITS/?INT_TYPE,
             ?INT:?TYPE_SIZE, (A7+B7):?BITS/?INT_TYPE,
             ?INT:?TYPE_SIZE, (A8+B8):?BITS/?INT_TYPE,
             ?INT:?TYPE_SIZE, (A9+B9):?BITS/?INT_TYPE>>,
    sum(RA, RB, A9, B9, Acc1);

sum(<<>>, <<>>, _LA, _LB, Acc) ->
    Acc;
sum(<<?INT:?TYPE_SIZE, A:?BITS/?INT_TYPE, RA/binary>>,
    <<>>, _LA, LB, Acc) ->
    sum(RA, <<>>, A, LB, <<Acc/binary, ?INT:?TYPE_SIZE, (A+LB):?BITS/?INT_TYPE>>);
sum(<<?NONE:?TYPE_SIZE, _:?BITS/?INT_TYPE, RA/binary>>,
    <<>>, LA, LB, Acc) ->
    sum(RA, <<>>, LA, LB, <<Acc/binary, ?INT:?TYPE_SIZE, (LA+LB):?BITS/?INT_TYPE>>);
sum(<<>>,
    <<?INT:?TYPE_SIZE, B:?BITS/?INT_TYPE, RB/binary>>, LA, _LB, Acc) ->
    sum(<<>>, RB, LA, B, <<Acc/binary, ?INT:?TYPE_SIZE, (LA+B):?BITS/?INT_TYPE>>);
sum(<<>>,
    <<?NONE:?TYPE_SIZE, _:?BITS/?INT_TYPE, RB/binary>>, LA, LB, Acc) ->
    sum(<<>>, RB, LA, LB, <<Acc/binary, ?INT:?TYPE_SIZE, (LA+LB):?BITS/?INT_TYPE>>);
sum(<<?INT:?TYPE_SIZE, A:?BITS/?INT_TYPE, RA/binary>>,
    <<?INT:?TYPE_SIZE, B:?BITS/?INT_TYPE, RB/binary>>, _LA, _LB, Acc) ->
    sum(RA, RB, A, B, <<Acc/binary, ?INT:?TYPE_SIZE, (A+B):?BITS/?INT_TYPE>>);
sum(<<?INT:?TYPE_SIZE, A:?BITS/?INT_TYPE, RA/binary>>,
    <<?NONE:?TYPE_SIZE, _:?BITS/?INT_TYPE, RB/binary>>, _LA, LB, Acc) ->
    sum(RA, RB, A, LB, <<Acc/binary, ?INT:?TYPE_SIZE, (A+LB):?BITS/?INT_TYPE>>);
sum(<<?NONE:?TYPE_SIZE, _A:?BITS/?INT_TYPE, RA/binary>>,
    <<?INT:?TYPE_SIZE, B:?BITS/?INT_TYPE, RB/binary>>, LA, _LB, Acc) ->
    sum(RA, RB, LA, B, <<Acc/binary, ?INT:?TYPE_SIZE, (LA+B):?BITS/?INT_TYPE>>);
sum(<<?NONE:?TYPE_SIZE, _A:?BITS/?INT_TYPE, RA/binary>>,
    <<?NONE:?TYPE_SIZE, _B:?BITS/?INT_TYPE, RB/binary>>, LA, LB, Acc) ->
    sum(RA, RB, LA, LB, <<Acc/binary, ?INT:?TYPE_SIZE, (LA+LB):?BITS/?INT_TYPE>>).

mul(Es) ->
    rcomb(fun mul/2, Es, []).

mul(A,B) ->
    mul(A, B, 1, 1, <<>>).

mul(<<>>, <<>>, _LA, _LB, Acc) ->
    Acc;
mul(<<?INT:?TYPE_SIZE, A:?BITS/?INT_TYPE, RA/binary>>,
    <<?INT:?TYPE_SIZE, B:?BITS/?INT_TYPE, RB/binary>>, _LA, _LB, Acc) ->
    mul(RA, RB, A, B, <<Acc/binary, ?INT:?TYPE_SIZE, (A*B):?BITS/?INT_TYPE>>);
mul(<<?INT:?TYPE_SIZE, A:?BITS/?INT_TYPE, RA/binary>>,
    <<?NONE:?TYPE_SIZE, _:?BITS/?INT_TYPE, RB/binary>>, _LA, LB, Acc) ->
    mul(RA, RB, A, LB, <<Acc/binary, ?INT:?TYPE_SIZE, (A*LB):?BITS/?INT_TYPE>>);
mul(<<?NONE:?TYPE_SIZE, _A:?BITS/?INT_TYPE, RA/binary>>,
    <<?INT:?TYPE_SIZE, B:?BITS/?INT_TYPE, RB/binary>>, LA, _LB, Acc) ->
    mul(RA, RB, LA, B, <<Acc/binary, ?INT:?TYPE_SIZE, (LA*B):?BITS/?INT_TYPE>>);
mul(<<?NONE:?TYPE_SIZE, _A:?BITS/?INT_TYPE, RA/binary>>,
    <<?NONE:?TYPE_SIZE, _B:?BITS/?INT_TYPE, RB/binary>>, LA, LB, Acc) ->
    mul(RA, RB, LA, LB, <<Acc/binary, ?INT:?TYPE_SIZE, (LA*LB):?BITS/?INT_TYPE>>).


zip(Fn, Es) ->
    rcomb(fun (A, B) -> zip(Fn, A, B) end, Es, []).

zip(Fn, A, B) ->
    zip(A, B, 1, 1, Fn, <<>>).

zip(<<>>, <<>>, _LA, _LB, _Fn, Acc) ->
    Acc;
zip(<<?INT:?TYPE_SIZE, A:?BITS/?INT_TYPE, RA/binary>>,
    <<?INT:?TYPE_SIZE, B:?BITS/?INT_TYPE, RB/binary>>, _LA, _LB, Fn, Acc) ->
    zip(RA, RB, A, B, Fn, apply(Fn, A, B, Acc));

zip(<<?INT:?TYPE_SIZE, A:?BITS/?INT_TYPE, RA/binary>>,
    <<?NONE:?TYPE_SIZE, _:?BITS/?INT_TYPE, RB/binary>>, _LA, LB, Fn, Acc) ->
    zip(RA, RB, A, LB, Fn,  apply(Fn, A, LB, Acc));
zip(<<?NONE:?TYPE_SIZE, _A:?BITS/?INT_TYPE, RA/binary>>,
    <<?INT:?TYPE_SIZE, B:?BITS/?INT_TYPE, RB/binary>>, LA, _LB, Fn, Acc) ->
    zip(RA, RB, LA, B, Fn, apply(Fn, LA, B, Acc));
zip(<<?NONE:?TYPE_SIZE, _A:?BITS/?INT_TYPE, RA/binary>>,
    <<?NONE:?TYPE_SIZE, _B:?BITS/?INT_TYPE, RB/binary>>, LA, LB, Fn, Acc) ->
    zip(RA, RB, LA, LB, Fn, apply(Fn, LA, LB, Acc)).

apply(Fn, A, B, Acc) ->
	case Fn(A, B) of
		V when is_integer(V) ->
			<<Acc/binary, ?INT:?TYPE_SIZE, (A*B):?BITS/?INT_TYPE>>;
		V when is_float(V) ->
			<<Acc/binary, ?INT:?TYPE_SIZE, (round(A*B)):?BITS/?INT_TYPE>>
	end.

merge(A, B) ->
    merge(A, B, <<>>).

merge(<<?NONE:?TYPE_SIZE, _:?BITS/?INT_TYPE, R1/binary>>,
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
