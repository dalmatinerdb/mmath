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

%%--------------------------------------------------------------------
%% @doc
%% Creates a new dataset with each element being the sum of the
%% elements of the passed datasets.
%% @end
%%--------------------------------------------------------------------
sum([A, B]) ->
    sum(A, B);

sum([A, B, C]) ->
    sum(A, B, C);

sum(Es) ->
    rcomb(fun sum/2, fun sum/3, Es, []).





%%--------------------------------------------------------------------
%% @doc
%% Creates a new dataset with each element being the average (mean)
%% of the elements of the passed datasets.
%% @end
%%--------------------------------------------------------------------
avg(Es) ->
    mmath_trans:divide(sum(Es), length(Es)).

%%-------------------------------------------------------------------
%% Utility functions
%%-------------------------------------------------------------------

sum(_A, _B) ->
    exit(nif_library_not_loaded).

sum(_A, _B, _C) ->
    exit(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Combines a set of datasets with a given combinator function.
%% this requires the combinator to be associative!
%%--------------------------------------------------------------------

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
