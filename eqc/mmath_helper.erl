-module(mmath_helper).

-include_lib("eqc/include/eqc.hrl").

-include("../include/mmath.hrl").

-export([int_array/0, number_array/0, pos_int/0, non_neg_int/0, defined_int_array/0,
         non_empty_i_list/0, fully_defined_int_array/0, from_decimal/1, realise/1, realise/3]).

defined_int_array() ->
    ?SUCHTHAT({R, _, _}, int_array(), [ok || {true, _} <- R] =/= []).

fully_defined_int_array() ->
    ?SUCHTHAT({R, _, _}, int_array(), [ok || {false, _} <- R] =:= []).

int_array() ->
    ?LET(L, list({frequency([{2, false}, {8, true}]), int()}),
         {L, to_list(L, 0, []), to_bin(L, <<>>)}).

number_array() ->
    ?LET(L, list({frequency([{2, false}, {8, true}]), supported_number()}),
         {L, to_list(L, 0, []), to_bin(L, <<>>)}).

pos_int() ->
    ?LET(I, int(), abs(I)+1).

non_neg_int() ->
    ?LET(I, int(), abs(I)+1).

supported_number() ->
    oneof([supported_int(), real()]).

supported_int() ->
    choose(-(1 bsl (?COEFFICIENT_SIZE - 1)), (1 bsl ?COEFFICIENT_SIZE) - 1).

non_empty_i_list() ->
    ?SUCHTHAT(L, list(int()), L =/= []).

floor(X) when X < 0 ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T - 1
    end;
floor(X) -> 
    trunc(X).

to_decimal(V) when is_integer(V) ->
    {V, 0};
to_decimal(V) when is_float(V) ->
    E = floor(math:log10(abs(V))) - ?DEC_PRECISION + 1,
    C = trunc(V / math:pow(10, E)),
    {C, E}.

from_decimal({C, E}) ->
    C * math:pow(10, E).

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

to_bin([{true, V} | R], Acc) when is_float(V) ->
    {C, E} = to_decimal(V),
    to_bin(R, <<Acc/binary, ?DEC:?TYPE_SIZE, E:?EXPONENT_SIZE/?INT_TYPE, C:?COEFFICIENT_SIZE/?INT_TYPE>>);

to_bin([], Acc) ->
    Acc.


realise([]) ->
    [];
realise(L) ->
    realise(L, 1).

realise([{false, _} | L], N) ->
    realise(L, N+1);
realise([{true, V} | L], N) ->
    D = to_decimal(V),
    Acc = [D || _ <- lists:seq(1,N)],
    realise(L, D, Acc);
realise([], N) ->
    [0 || _ <- lists:seq(1,N - 1)].

realise([], _, Acc) ->
    lists:reverse(Acc);
realise([{true, V} | R], _, Acc) ->
    D = to_decimal(V),
    realise(R, D, [D | Acc]);
realise([{false, _} | R], L, Acc) ->
    realise(R, L, [L | Acc]).
