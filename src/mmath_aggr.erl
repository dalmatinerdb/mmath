%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2014, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created :  8 Jun 2014 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(mmath_aggr).

-export([load_nif/0]).
-export([empty/2, empty_r/2,
         sum/2, sum_r/2,
         avg/2, avg_r/2,
         min/2, min_r/2,
         max/2, max_r/2]).
-export([map/2,
         scale/2, scale_r/2,
         derivate/1, derivate_r/1,
         mul/2, mul_r/2,
         divide/2, divide_r/2]).
-export([percentile/3]).

-include("mmath.hrl").

-define(APPNAME, mmath).
-define(LIBNAME, aggr_nif).
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

scale_r(Data, F) ->
    mmath_bin:realize(scale(mmath_bin:derealize(Data), F)).

scale(<<>>, _) ->
    <<>>;

scale(Bin, Scale) ->
    scale_int(Bin, 0, Scale, <<>>).

empty(Data, Count) ->
    empty(Data, 0, Count, Count, <<>>).

empty_r(Data, Count) ->
    mmath_bin:realize(empty(mmath_bin:derealize(Data), Count)).

percentile(Data, Count, Percentile) ->
    Pos = erlang:min(Count, round(Count * Percentile) + 1),
    Size = ?DATA_SIZE * Count,
    percentile_int(Data, Size, Pos, Percentile, <<>>).

avg(Data, Count) when Count > 0->
    avg(Data, 0, 0, Count, Count, <<>>).

avg_r(Data, Count) ->
    mmath_bin:realize(avg(mmath_bin:derealize(Data), Count)).

sum(Data, Count) ->
    sum_int(Data, 0, 0, Count, Count, <<>>).

sum_r(Data, Count) ->
    mmath_bin:realize(sum(mmath_bin:derealize(Data), Count)).

min(Data, Count) ->
    min_int(Data, undefined, Count, Count, <<>>).

min_r(Data, Count) ->
    mmath_bin:realize(?MODULE:min(mmath_bin:derealize(Data), Count)).

max(Data, Count) ->
    max_int(Data, undefined, Count, Count, <<>>).

max_r(Data, Count) ->
    mmath_bin:realize(?MODULE:max(mmath_bin:derealize(Data), Count)).

%% ------------------

empty(R, Empty, 0, Count, Acc) ->
    Acc1 = <<Acc/binary, ?INT:?TYPE_SIZE, Empty:?BITS/?INT_TYPE>>,
    empty(R, 0, Count, Count, Acc1);

empty(<<?NONE:?TYPE_SIZE, 0:?BITS/?INT_TYPE, R/binary>>, Sum, N, Count, Acc) ->
    empty(R, Sum + 1, N-1, Count, Acc);

empty(<<_:?TYPE_SIZE, _I:?BITS/?INT_TYPE, R/binary>>, Sum, N, Count, Acc) ->
    empty(R, Sum, N - 1, Count, Acc);

empty(<<>>, 0, _Count, _Count, Acc) ->
    Acc;

empty(<<>>, Sum, Missing, _Count, Acc) ->
    Empty = Sum + Missing,
    <<Acc/binary, ?INT:?TYPE_SIZE, Empty:?BITS/?INT_TYPE>>.


percentile_int(<<>>, _, _, _, Acc) ->
    Acc;

percentile_int(Data, Size, Pos, Percentile, Acc)
  when byte_size(Data) >= Size->
    <<D:Size/binary, R/binary>> = Data,
    L = mmath_bin:to_list(D),
    V = lists:nth(Pos, lists:sort(L)),
    percentile_int(R, Size, Pos, Percentile,
                   <<Acc/binary, ?INT:?TYPE_SIZE, V:?BITS/?INT_TYPE>>);

percentile_int(D, _, _, Percentile, Acc) ->
    L = mmath_bin:to_list(D),
    Len = mmath_bin:length(D),
    V = lists:nth(erlang:min(Len, round(Len * Percentile) + 1), lists:sort(L)),
    <<Acc/binary, ?INT:?TYPE_SIZE, V:?BITS/?INT_TYPE>>.


avg(R, Last, Sum, 0, Count, Acc) ->
    Avg = Sum div Count,
    Acc1 = <<Acc/binary, ?INT:?TYPE_SIZE, Avg:?BITS/?INT_TYPE>>,
    avg(R, Last, 0, Count, Count, Acc1);

%% Optimistic case to grab a bunch of data at once

avg(<<?INT:?TYPE_SIZE, I0:?BITS/?INT_TYPE,
      ?INT:?TYPE_SIZE, I1:?BITS/?INT_TYPE,
      ?INT:?TYPE_SIZE, I2:?BITS/?INT_TYPE,
      ?INT:?TYPE_SIZE, I3:?BITS/?INT_TYPE,
      ?INT:?TYPE_SIZE, I4:?BITS/?INT_TYPE,
      ?INT:?TYPE_SIZE, I5:?BITS/?INT_TYPE,
      ?INT:?TYPE_SIZE, I6:?BITS/?INT_TYPE,
      ?INT:?TYPE_SIZE, I7:?BITS/?INT_TYPE,
      ?INT:?TYPE_SIZE, I8:?BITS/?INT_TYPE,
      ?INT:?TYPE_SIZE, I9:?BITS/?INT_TYPE,
      R/binary>>, _Last, Sum, N, Count, Acc) when N >= 10 ->
    Sum1 = Sum + I0 + I1 + I2 + I3 + I4 + I5 + I6 + I7 + I8 + I9,
    avg(R, I9, Sum1, N - 10, Count, Acc);

avg(<<?INT:?TYPE_SIZE, I:?BITS/?INT_TYPE, R/binary>>, _Last, Sum, N, Count, Acc) ->
    avg(R, I, Sum + I, N - 1, Count, Acc);

avg(<<?NONE:?TYPE_SIZE, 0:?BITS/?INT_TYPE, R/binary>>, Last, Sum, N, Count, Acc) ->
    avg(R, Last, Sum + Last, N-1, Count, Acc);

avg(<<>>, _, 0, _Count, _Count, Acc) ->
    Acc;

avg(<<>>, _, Sum, _Missing, Count, Acc) ->
    Avg = Sum div Count,
    <<Acc/binary, ?INT:?TYPE_SIZE, Avg:?BITS/?INT_TYPE>>.

sum_int(R, Last, Sum, 0, Count, Acc) ->
    Acc1 = <<Acc/binary, ?INT:?TYPE_SIZE, Sum:?BITS/?INT_TYPE>>,
    sum_int(R, Last, 0, Count, Count, Acc1);
sum_int(<<?INT:?TYPE_SIZE, I:?BITS/?INT_TYPE, R/binary>>, _, Sum, N, Count, Acc) ->
    sum_int(R, I, Sum+I, N-1, Count, Acc);
sum_int(<<?NONE:?TYPE_SIZE, 0:?BITS/?INT_TYPE, R/binary>>, Last, Sum, N, Count, Acc) ->
    sum_int(R, Last, Sum+Last, N-1, Count, Acc);
sum_int(<<>>, _, 0, _Count, _Count, Acc) ->
    Acc;
sum_int(<<>>, _, Sum, _, _, Acc) ->
    <<Acc/binary, ?INT:?TYPE_SIZE, Sum:?BITS/?INT_TYPE>>.

min_int(R, undefined, 0, Count, Acc) ->
    Acc1 = <<Acc/binary, ?NONE:?TYPE_SIZE, 0:?BITS/?INT_TYPE>>,
    min_int(R, undefined, Count, Count, Acc1);
min_int(R, V, 0, Count, Acc) ->
    Acc1 = <<Acc/binary, ?INT:?TYPE_SIZE, V:?BITS/?INT_TYPE>>,
    min_int(R, undefined, Count, Count, Acc1);
min_int(<<?INT:?TYPE_SIZE, V:?BITS/?INT_TYPE, R/binary>>, undefined, N, Count, Acc) ->
    min_int(R, V, N-1, Count, Acc);
min_int(<<?INT:?TYPE_SIZE, V:?BITS/?INT_TYPE, R/binary>>, Min, N, Count, Acc)
  when V <  Min->
    min_int(R, V, N-1, Count, Acc);
min_int(<<_:?TYPE_SIZE, _:?BITS/?INT_TYPE, R/binary>>, Min, N, Count, Acc) ->
    min_int(R, Min, N-1, Count, Acc);
min_int(<<>>, _, _Count, _Count, Acc) ->
    Acc;
min_int(<<>>, undefined, _, _, Acc) ->
    <<Acc/binary, ?NONE:?TYPE_SIZE, 0:?BITS/?INT_TYPE>>;
min_int(<<>>, Min, _, _, Acc) ->
    <<Acc/binary, ?INT:?TYPE_SIZE, Min:?BITS/?INT_TYPE>>.

max_int(R, undefined, 0, Count, Acc) ->
    Acc1 = <<Acc/binary, ?NONE:?TYPE_SIZE, 0:?BITS/?INT_TYPE>>,
    max_int(R, undefined, Count, Count, Acc1);
max_int(R, V, 0, Count, Acc) ->
    Acc1 = <<Acc/binary, ?INT:?TYPE_SIZE, V:?BITS/?INT_TYPE>>,
    max_int(R, undefined, Count, Count, Acc1);
max_int(<<?INT:?TYPE_SIZE, V:?BITS/?INT_TYPE, R/binary>>, undefined, N, Count, Acc) ->
    max_int(R, V, N-1, Count, Acc);
max_int(<<?INT:?TYPE_SIZE, V:?BITS/?INT_TYPE, R/binary>>, Max, N, Count, Acc)
  when V >  Max->
    max_int(R, V, N-1, Count, Acc);
max_int(<<_:?TYPE_SIZE, _:?BITS/?INT_TYPE, R/binary>>, Max, N, Count, Acc) ->
    max_int(R, Max, N-1, Count, Acc);
max_int(<<>>, _, _Count, _Count, Acc) ->
    Acc;
max_int(<<>>, undefined, _, _, Acc) ->
    <<Acc/binary, ?NONE:?TYPE_SIZE, 0:?BITS/?INT_TYPE>>;
max_int(<<>>, Max, _, _, Acc) ->
    <<Acc/binary, ?INT:?TYPE_SIZE, Max:?BITS/?INT_TYPE>>.

scale_int(<<?INT:?TYPE_SIZE, I:?BITS/?INT_TYPE, Rest/binary>>, _, S, Acc) ->
    scale_int(Rest, I, S, <<Acc/binary, ?INT:?TYPE_SIZE, (round(I*S)):?BITS/?INT_TYPE>>);
scale_int(<<?NONE:?TYPE_SIZE, _:?BITS/?INT_TYPE, Rest/binary>>, I, S, Acc) ->
    scale_int(Rest, I, S, <<Acc/binary, ?INT:?TYPE_SIZE, (round(I*S)):?BITS/?INT_TYPE>>);
scale_int(<<>>, _, _, Acc) ->
    Acc.

mul_r(M, D) ->
    mmath_bin:realize(mul(mmath_bin:derealize(M), D)).

mul(<<>>, _) ->
    <<>>;

mul(Bin, Mul) when is_integer(Mul) ->
    mul_int(Bin, 0, Mul, <<>>).

mul_int(<<?INT:?TYPE_SIZE, I:?BITS/?INT_TYPE, Rest/binary>>, _, S, Acc) ->
    mul_int(Rest, I, S, <<Acc/binary, ?INT:?TYPE_SIZE, (I*S):?BITS/?INT_TYPE>>);
mul_int(<<?NONE:?TYPE_SIZE, _:?BITS/?INT_TYPE, Rest/binary>>, I, S, Acc) ->
    mul_int(Rest, I, S, <<Acc/binary, ?INT:?TYPE_SIZE, (I*S):?BITS/?INT_TYPE>>);
mul_int(<<>>, _, _, Acc) ->
    Acc.

divide_r(M, D) ->
        mmath_bin:realize(divide(mmath_bin:derealize(M), D)).

divide(<<>>, _) ->
    <<>>;

divide(Bin, Divide) when is_integer(Divide) ->
    divide_int(Bin, 0, Divide, <<>>).

divide_int(<<?INT:?TYPE_SIZE, I:?BITS/?INT_TYPE, Rest/binary>>, _, S, Acc) ->
    divide_int(Rest, I, S, <<Acc/binary, ?INT:?TYPE_SIZE, (I div S):?BITS/?INT_TYPE>>);
divide_int(<<?NONE:?TYPE_SIZE, _:?BITS/?INT_TYPE, Rest/binary>>, I, S, Acc) ->
    divide_int(Rest, I, S, <<Acc/binary, ?INT:?TYPE_SIZE, (I div S):?BITS/?INT_TYPE>>);
divide_int(<<>>, _, _, Acc) ->
    Acc.

derivate_r(M) ->
    mmath_bin:realize(derivate(mmath_bin:derealize(M))).

derivate(<<>>) ->
    <<>>;

derivate(<<?INT:?TYPE_SIZE, I:?BITS/?INT_TYPE, Rest/binary>>) ->
    der_int(Rest, I, <<>>);

derivate(<<?NONE:?TYPE_SIZE, 0:?BITS/?INT_TYPE, Rest/binary>>) ->
    der_int(Rest, find_first(Rest), <<>>).

der_int(<<?INT:?TYPE_SIZE, I:?BITS/?INT_TYPE, Rest/binary>>, Last, Acc) ->
    der_int(Rest, I, <<Acc/binary, ?INT:?TYPE_SIZE, (I - Last):?BITS/?INT_TYPE>>);
der_int(<<?NONE:?TYPE_SIZE, 0:?BITS/?INT_TYPE, Rest/binary>>, Last, Acc) ->
    der_int(Rest, Last, <<Acc/binary, ?INT:?TYPE_SIZE, 0:?BITS/?INT_TYPE>>);
der_int(<<>>, _, Acc) ->
    Acc.

map(Bin, Fn) ->
	map(Bin, 0, Fn, <<>>).

map(<<?INT:?TYPE_SIZE, I:?BITS/?INT_TYPE, Rest/binary>>, _, Fn, Acc) ->
	map(Rest, I, Fn, apl(Fn, I, Acc));
map(<<?NONE:?TYPE_SIZE, _:?BITS/?INT_TYPE, Rest/binary>>, L, Fn, Acc) ->
	map(Rest, L, Fn, apl(Fn, L, Acc));
map(<<>>, _, _, Acc) ->
    Acc.

apl(Fn, V, Acc) ->
	case Fn(V) of
		V1 when is_integer(V1) ->
			<<Acc/binary, ?INT:?TYPE_SIZE, V1:?BITS/?INT_TYPE>>;
		V1 when is_float(V1) ->
			<<Acc/binary, ?INT:?TYPE_SIZE, (round(V1)):?BITS/?INT_TYPE>>
	end.

find_first(<<>>) ->
    0;
find_first(<<?INT:?TYPE_SIZE, I:?BITS/?INT_TYPE, _/binary>>) ->
    I;
find_first(<<?NONE:?TYPE_SIZE, 0:?BITS/?INT_TYPE, Rest/binary>>) ->
    find_first(Rest).
