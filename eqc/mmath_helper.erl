-module(mmath_helper).

-include_lib("eqc/include/eqc.hrl").

-include("../include/mmath.hrl").

-export([int_array/0, number_array/0, pos_int/0, non_neg_int/0, defined_int_array/0,
         defined_number_array/0, non_empty_i_list/0, fully_defined_int_array/0,
         fully_defined_number_array/0, to_decimal/1, from_decimal/1, realise/1,
         realise/3, almost_equal/2]).

-define(MAX_RELATIVE_ERROR, math:pow(10, 2 - ?DEC_PRECISION)).

defined_int_array() ->
    ?SUCHTHAT({R, _, _}, int_array(), [ok || {true, _} <- R] =/= []).

defined_number_array() ->
    ?SUCHTHAT({R, _, _}, number_array(), [ok || {true, _} <- R] =/= []).

fully_defined_int_array() ->
    ?SUCHTHAT({R, _, _}, int_array(), [ok || {false, _} <- R] =:= []).

fully_defined_number_array() ->
    ?SUCHTHAT({R, _, _}, number_array(), [ok || {false, _} <- R] =/= []).

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

to_decimal(V) when V == 0.0 ->
    {0, 0};
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


%% Expand values to list of values that can be understand by mmath_bin:from_list
%%
realise([]) ->
    [];
realise(L) ->
    realise(L, 1).

realise([{false, _} | L], N) ->
    realise(L, N+1);
realise([{true, V} | L], N) ->
    %D = to_decimal(V),
    Acc = [V || _ <- lists:seq(1,N)],
    realise(L, V, Acc);
realise([], N) ->
    [0 || _ <- lists:seq(1,N - 1)].

realise([], _, Acc) ->
    lists:reverse(Acc);
realise([{true, V} | R], _, Acc) ->
    %D = to_decimal(V),
    realise(R, V, [V | Acc]);
realise([{false, _} | R], L, Acc) ->
    realise(R, L, [L | Acc]).

almost_equal_number(A, B) ->
    RelativeError = case abs(A - B) of
                       Error when A /= 0 -> 
                            Error / A;
                       Error -> Error
                    end,
    RelativeError =< ?MAX_RELATIVE_ERROR.

almost_equal([], []) ->
    true;
almost_equal([AFirst|ARest], [BFirst|BRest]) ->
    almost_equal_number(AFirst, BFirst) and almost_equal(ARest, BRest).
