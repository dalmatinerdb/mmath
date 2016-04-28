-module(mmath_helper).

-include_lib("eqc/include/eqc.hrl").

-include("../include/mmath.hrl").

-export([int_array/0, pos_int/0, non_neg_int/0, defined_int_array/0,
         non_empty_i_list/0, fully_defined_int_array/0, realise/1, realise/3,
         epsilon/3, epsilon/2]).
-define(EPSILON, 0.00001).

defined_int_array() ->
    ?SUCHTHAT({R, _, _}, int_array(), [ok || {true, _} <- R] =/= []).

fully_defined_int_array() ->
    ?SUCHTHAT({R, _, _}, int_array(), [ok || {false, _} <- R] =:= []).

int_array() ->
    ?LET(L, list({frequency([{2, false}, {8, true}]), int()}),
         {L, to_list(L, 0, []), to_bin(L, <<>>)}).

pos_int() ->
    ?LET(I, int(), abs(I)+1).

non_neg_int() ->
    ?LET(I, int(), abs(I)+1).

non_empty_i_list() ->
    ?SUCHTHAT(L, list(int()), L =/= []).

to_list([{false, _} | R], Last, Acc) ->
    to_list(R, Last, [Last | Acc]);
to_list([{true, V} | R], _, Acc) ->
    to_list(R, V, [V | Acc]);
to_list([], _, Acc) ->
    lists:reverse(Acc).

to_bin([{false, _} | R], Acc) ->
    to_bin(R, <<Acc/binary, ?NONE:?TYPE_SIZE, 0:?BITS/?INT_TYPE>>);

to_bin([{true, V} | R], Acc) when is_integer(V) ->
    to_bin(R, <<Acc/binary, ?INT:?TYPE_SIZE, V:?BITS/?INT_TYPE>>);

to_bin([], Acc) ->
    Acc.


realise([]) ->
    [];
realise(L) ->
    realise(L, 1).

realise([{false, _} | L], N) ->
    realise(L, N+1);
realise([{true, V} | L], N) ->
    Acc = [V || _ <- lists:seq(1,N)],
    realise(L, V, Acc);
realise([], N) ->
    [0 || _ <- lists:seq(1,N - 1)].

realise([], _, Acc) ->
    lists:reverse(Acc);
realise([{true, V} | R], _, Acc) ->
    realise(R, V, [V | Acc]);
realise([{false, _} | R], L, Acc) ->
    realise(R, L, [L | Acc]).

epsilon(A, B) ->
    epsilon(A, B, ?EPSILON).

epsilon(A, B, E) when is_number(A), is_number(B) ->
    abs(A - B) < E;
epsilon([A | Ra], [B | Rb], E) ->
    abs(A - B) < E andalso epsilon(Ra, Rb, E);
epsilon([], [], _) ->
    true.


