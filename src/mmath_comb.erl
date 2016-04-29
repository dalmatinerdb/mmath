%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2016, Project-FiFo UG
%%% @doc
%%% Module that provide mmath functions that combine metrics.
%%% All functions take a list of realized metrics and return a single
%%% realized metric
%%% @end
%%% Created : 29 Apr 2016 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(mmath_comb).
-include("mmath.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-export([avg/1,
         sum/1
        %%,
         %%mul/1,
         %%merge/1,
         %%zip/2
        ]).


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

sum([A, B]) ->
    sum(A, B);

sum([A, B, C]) ->
    sum(A, B, C);

sum(Es) ->
    rcomb(fun sum/2, fun sum/3, Es, []).


sum(_A, _B) ->
    exit(nif_library_not_loaded).

sum(_A, _B, _C) ->
    exit(nif_library_not_loaded).


avg(Es) ->
    mmath_aggr:divide(sum(Es), length(Es)).

%%merge(Es) ->
%%    rcomb(fun merge/2, Es, []).


%% mul(Es) ->
%%     rcomb(fun mul/2, Es, []).

%% mul(A, B) ->
%%     zip(fun (VA, VB) ->
%%                 VA * VB
%%         end, A, B).

%% zip(Fn, Es) ->
%%     rcomb(fun (A, B) -> zip(Fn, A, B) end, Es, []).

%% zip(Fn, A, B) ->
%%     zip(A, B, 1, 1, Fn, <<>>).

%% zip(<<>>, <<>>, _LA, _LB, _Fn, Acc) ->
%%     Acc;

%% zip(<<DA:?DATA_SIZE/binary, RA/binary>>,
%%     <<DB:?DATA_SIZE/binary, RB/binary>>, LA, LB, Fn, Acc) ->
%%     A = binary_to_value(DA, LA),
%%     B = binary_to_value(DB, LB),
%%     zip(RA, RB, A, B, Fn, apply(Fn, A, B, Acc)).

%% binary_to_value(<<?NONE:?TYPE_SIZE, _:?BITS/?INT_TYPE>>, L) ->
%%     L;
%% binary_to_value(<<?INT:?TYPE_SIZE, I:?BITS/?INT_TYPE>>, _L) ->
%%     I;
%% binary_to_value(<<D:?DATA_SIZE/binary>>, _L) ->
%%     [V] = mmath_bin:to_list(D),
%%     V.

%% apply(Fn, A, B, Acc) ->
%%     case Fn(A, B) of
%%         V when is_integer(V), abs(V) < (1 bsl (?BITS - 1)) ->
%%             <<Acc/binary, ?INT:?TYPE_SIZE, (V):?BITS/?INT_TYPE>>;
%%         V ->
%%             <<Acc/binary, (mmath_bin:from_list([float(V)]))/binary>>
%%     end.

%% merge(A, B) ->
%%     merge(A, B, <<>>).

%% merge(<<?NONE:?TYPE_SIZE, _:?BITS/?INT_TYPE, R1/binary>>,
%%       <<D:?DATA_SIZE/binary, R2/binary>>,
%%       Acc) ->
%%     merge(R1, R2, <<Acc/binary, D/binary>>);
%% merge(<<D:?DATA_SIZE/binary, R1/binary>>,
%%       <<_:?DATA_SIZE/binary, R2/binary>>,
%%       Acc) ->
%%     merge(R1, R2, <<Acc/binary, D/binary>>);
%% merge(<<>>, <<>>, Acc) ->
%%     Acc;

%% merge(<<>>, D, Acc) ->
%%     <<Acc/binary, D/binary>>;
%% merge(D, <<>>, Acc) ->
%%     <<Acc/binary, D/binary>>.

%% rcomb(F2, In, Acc) ->
%%     rcomb(F2, undefined, In, Acc).

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
