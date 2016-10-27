-module(mmath_helper).

-include_lib("eqc/include/eqc.hrl").

-include("../include/mmath.hrl").

-export([number_array/0, pos_int/0, non_neg_int/0, supported_number/0,
         defined_number_array/0, non_empty_number_list/0, raw_number_array/0,
         pad_to_n/2, fully_defined_number_array/0, from_decimal/1, realise/1,
         realise/3, almost_equal/4, almost_equal/2, confidence/1, to_list_d/1,
         within_epsilon/3, number_array/1, apply_n/3, n_length_chunks/2]).

-define(EPSILON, math:pow(10, 3 - ?DEC_PRECISION)).
-define(DELTA, 0.99999999).

defined_number_array() ->
    ?SUCHTHAT({R, _, _}, number_array(), [ok || {true, _} <- R] =/= []).

fully_defined_number_array() ->
    ?SUCHTHAT({R, _, _}, number_array(), [ok || {false, _} <- R] =:= []).

raw_number_array() ->
    ?LET(L, number_array_(),
         {L, to_list(L, 0, []), to_bin(L, <<>>)}).

number_array() ->
    ?LET(L, number_array_(),
         {L, to_list(L, 0, []), to_bin(L)}).

number_array(N) ->
    ?LET(L,
         number_array_(N),
         {L, to_list(L, 0, []), to_bin(L)}).

number_array_(N) ->
    array_elements(N).

number_array_() ->
    list({frequency([{2, false}, {8, true}]), supported_number()}).

array_elements(0) ->
    [];

array_elements(N) ->
    [array_element() | array_elements(N -1)].
    %% ?LAZY(
    %%    ?LETSHRINK({A, L},
    %%               {},
    %%               [A | L])).

array_element() ->
    {frequency([{2, false}, {8, true}]), supported_number()}.

pos_int() ->
    ?LET(I, int(), abs(I)+1).

non_neg_int() ->
    ?LET(I, int(), abs(I)+1).

supported_number() ->
    oneof([supported_int(), real()]).

supported_int() ->
    choose(-(1 bsl (?DEC_COEF_SIZE - 1)), (1 bsl ?DEC_COEF_SIZE) - 1).

non_empty_number_list() ->
    ?SUCHTHAT(L, list(supported_number()), L =/= []).

ceil(X) when X < 0 ->
    trunc(X);
ceil(X) ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T + 1
    end.

to_decimal(V) when V == 0.0 ->
    {0, 0};
to_decimal(V) when is_integer(V) ->
    {V, 0};
to_decimal(V) when is_float(V) ->
    E = ceil(math:log10(abs(V))) - ?DEC_PRECISION,
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
to_bin(Es) ->
    mmath_bin:realize(to_bin(Es, <<>>)).

to_bin([{false, _} | R], Acc) ->
    to_bin(R, <<Acc/binary, ?NONE:?TYPE_SIZE, 0:?BITS/?INT_TYPE>>);

to_bin([{true, V} | R], Acc) when is_integer(V) ->
    to_bin(R, <<Acc/binary, ?INT:?TYPE_SIZE, V:?BITS/?INT_TYPE>>);

to_bin([{true, V} | R], Acc) when V == 0.0 ->
    to_bin(R, <<Acc/binary, ?INT:?TYPE_SIZE, 0:?BITS/?INT_TYPE>>);

to_bin([{true, V} | R], Acc) when is_float(V) ->
    {C, E} = to_decimal(V),
    to_bin(R, <<Acc/binary, ?DEC:?TYPE_SIZE, E:?DEC_EXP_SIZE/?INT_TYPE, C:?DEC_COEF_SIZE/?INT_TYPE>>);

to_bin([], Acc) ->
    Acc.


confidence_({false, _}) ->
    0.0;
confidence_(_) ->
    1.0.
confidence(L) ->
    [confidence_(E) || E <- L].

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

almost_equal(A, B) ->
    almost_equal(A, B, ?DELTA, ?EPSILON).

almost_equal([A | Ra], [B | Rb], D, E) ->
    almost_equal(A, B, D, E) andalso almost_equal(Ra, Rb, D, E);
almost_equal([], [], _, _) ->
    true;
almost_equal(A, B, D, E) when A == 0 ; B == 0 ->
    almost_equal(A + 1, B + 1, D, E);
almost_equal(A, B, D, E) ->
    AAbs = abs(A),
    BAbs = abs(B),
    (min(AAbs, BAbs)/ max(AAbs, BAbs) > D)
        orelse (abs(AAbs - BAbs) < E).

within_epsilon([A | Ra], [B | Rb], E) ->
    within_epsilon(A, B, E) andalso within_epsilon(Ra, Rb, E);
within_epsilon([], [], _) ->
    true;
within_epsilon(A, B, E) ->
    abs(A) - abs(B) < E.


%% yes this is bad, so what?!?
pad_to_n(_L, 0) ->
    [];
pad_to_n([], N)  ->
    pad_to_n([0], N);
pad_to_n(L, N) when (length(L) rem N) == 0 ->
    L;
pad_to_n(L, N) ->
    pad_to_n(L ++ [0], N).

apply_n(L, N, F) ->
    fix_list([F(SL, N) || SL <- n_length_chunks(L, N)], 0, []).

%% taken from http://stackoverflow.com/questions/12534898/splitting-a-list-in-equal-sized-chunks-in-erlang
n_length_chunks([], _) -> [];
n_length_chunks(List, Len) when Len > length(List) ->
    [List];
n_length_chunks(List, Len) ->
    {Head,Tail} = lists:split(Len,List),
    [Head | n_length_chunks(Tail,Len)].

to_list_d(X) ->
     mmath_bin:to_list(mmath_bin:derealize(X)).

%% ===========================================================================
%% Internal functions
%% ===========================================================================

fix_list([undefined | T], Last, Acc) ->
    fix_list(T, Last, [Last | Acc]);
fix_list([V | T], _, Acc) ->
    fix_list(T, V, [V | Acc]);
fix_list([],_,Acc) ->
    lists:reverse(Acc).
