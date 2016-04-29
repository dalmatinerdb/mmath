-module(mmath_comb).
-include("mmath.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-export([avg/1,
         avg_r/1,
         sum/1,
         sum_r/1,
         mul/1, merge/1, zip/2]).


-define(APPNAME, mmath).
-define(LIBNAME, comb_nif).
-on_load(load_nif/0).
load_nif() ->
    SoName = case code:priv_dir(?APPNAME) of
                 {error, bad_name} ->
                     case filelib:is_dir(filename:join(["..", priv])) of
                         true ->
                             filename:join(["..", priv, ?LIBNAME]);
                         _ ->
                             filename:join([priv, ?LIBNAME])
                     end;
                 Dir ->
                     filename:join(Dir, ?LIBNAME)
             end,
    erlang:load_nif(SoName, 0).

sum_r([A, B]) ->
    sum_r(A, B);

sum_r([A, B, C]) ->
    sum_r(A, B, C);

sum_r(Es) ->
    rcomb(fun sum_r/2, fun sum_r/3, Es, []).

sum_r(A, B) ->
    mmath_bin:realize(sum(mmath_bin:derealize(A),
                          mmath_bin:derealize(B))).

sum_r(A, B, C) ->
    mmath_bin:realize(sum(mmath_bin:derealize(A),
                          mmath_bin:derealize(B),
                          mmath_bin:derealize(C))).

sum([A, B]) ->
    sum(A, B);

sum([A, B, C]) ->
    sum(A, B, C);

sum(Es) ->
    rcomb(fun sum/2, fun sum/3, Es, []).

avg_r(Es) ->
    mmath_aggr:divide_r(sum_r(Es), length(Es)).

avg(Es) ->
    mmath_aggr:divide(sum(Es), length(Es)).

merge(Es) ->
    rcomb(fun merge/2, Es, []).

sum(A, B) ->
    sum(A, B, 0, 0, <<>>).

sum(A, B, C) ->
    sum(A, sum(B, C)).

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

mul(A, B) ->
    zip(fun (VA, VB) -> 
                VA * VB
        end, A, B).

zip(Fn, Es) ->
    rcomb(fun (A, B) -> zip(Fn, A, B) end, Es, []).

zip(Fn, A, B) ->
    zip(A, B, 1, 1, Fn, <<>>).

zip(<<>>, <<>>, _LA, _LB, _Fn, Acc) ->
    Acc;

zip(<<DA:?DATA_SIZE/binary, RA/binary>>,
    <<DB:?DATA_SIZE/binary, RB/binary>>, LA, LB, Fn, Acc) ->
    A = binary_to_value(DA, LA),
    B = binary_to_value(DB, LB),
    zip(RA, RB, A, B, Fn, apply(Fn, A, B, Acc)).

binary_to_value(<<?NONE:?TYPE_SIZE, _:?BITS/?INT_TYPE>>, L) ->
    L;
binary_to_value(<<?INT:?TYPE_SIZE, I:?BITS/?INT_TYPE>>, _L) ->
    I;
binary_to_value(<<D:?DATA_SIZE/binary>>, _L) ->
    [V] = mmath_bin:to_list(D),
    V.

apply(Fn, A, B, Acc) ->
    case Fn(A, B) of
        V when is_integer(V), abs(V) < (1 bsl (?BITS - 1)) ->
            <<Acc/binary, ?INT:?TYPE_SIZE, (V):?BITS/?INT_TYPE>>;
        V ->
            <<Acc/binary, (mmath_bin:from_list([float(V)]))/binary>>
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

rcomb(F2, In, Acc) ->
    rcomb(F2, undefined, In, Acc).

rcomb(F2, _F3, [A], [B]) ->
    F2(A, B);
rcomb(F2, F3, [], Acc) ->
    rcomb(F2, F3, Acc, []);
rcomb(_, _, [A], []) ->
    A;

rcomb(F2, F3, [A, B, C, D | R], Acc) ->
    rcomb(F2, F3, R, [F2(F2(A, B), F2(C, D)) | Acc]);

rcomb(F2, F3, [A, B, C | R], Acc) when is_function(F3) ->
    rcomb(F2, F3, R, [F3(A, B, C) | Acc]);

rcomb(F2, F3, [A, B | R], Acc) ->
    rcomb(F2, F3, R, [F2(A, B) | Acc]);

rcomb(F2, F3, [A], Acc) ->
    rcomb(F2, F3, [A | Acc], []).
