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
-export([]).
-endif.

-export([avg/1,
         sum/1,
         diff/1,
         product/1,
         quotient/1,

         min/1,
         max/1

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
-spec sum([binary()]) -> binary().
sum([A, B]) ->
    sum_(A, B);

sum([A, B, C]) ->
    sum_(A, B, C);

sum(Es) when is_list(Es) ->
    rcomb(fun sum_/2, fun sum_/3, Es).

%%--------------------------------------------------------------------
%% @doc
%% Creates a new dataset with each element being the dfifference of the
%% elements of the passed datasets.
%% @end
%%--------------------------------------------------------------------
-spec diff([binary()]) -> binary().
diff([A, B]) ->
    sub_(A, B);

diff([A, B, C]) ->
    sub_(A, B, C);

diff(Es) when is_list(Es) ->
    rcomb(fun sub_/2, fun sub_/3, Es).

%%--------------------------------------------------------------------
%% @doc
%% Creates a new dataset with each element being the product of the
%% elements of the passed datasets.
%% @end
%%--------------------------------------------------------------------
-spec product([binary()]) -> binary().
product([A, B]) ->
    mul_(A, B);

product([A, B, C]) ->
    mul_(A, B, C);

product(Es) when is_list(Es) ->
    rcomb(fun mul_/2, fun mul_/3, Es).

%%--------------------------------------------------------------------
%% @doc
%% Creates a new dataset with each element being the quotient of the
%% elements of the passed datasets.
%%
%% It needs to be noted that as a convention a division by zero
%% is treated as a division by one. The reason here is that in a
%% metric stream we have no way of preventing zeros so some sensible
%% handling needs to be performed.
%% @end
%%--------------------------------------------------------------------
-spec quotient([binary()]) -> binary().
quotient([A, B]) ->
    div_(A, B);

quotient([A, B, C]) ->
    div_(A, B, C);

quotient(Es) when is_list(Es) ->
    rcomb(fun div_/2, fun div_/3, Es).

%%--------------------------------------------------------------------
%% @doc
%% Creates a new dataset with each element being the min of the
%% elements of the passed datasets.
%% @end
%%--------------------------------------------------------------------
-spec min([binary()]) -> binary().
min([A, B]) ->
    min_(A, B);

min([A, B, C]) ->
    min_(A, B, C);

min(Es) when is_list(Es) ->
    rcomb(fun min_/2, fun min_/3, Es).

%%--------------------------------------------------------------------
%% @doc
%% Creates a new dataset with each element being the max of the
%% elements of the passed datasets.
%% @end
%%--------------------------------------------------------------------
-spec max([binary()]) -> binary().
max([A, B]) ->
    max_(A, B);

max([A, B, C]) ->
    max_(A, B, C);

max(Es) when is_list(Es) ->
    rcomb(fun max_/2, fun max_/3, Es).

%%--------------------------------------------------------------------
%% @doc
%% Creates a new dataset with each element being the average (mean)
%% of the elements of the passed datasets.
%% @end
%%--------------------------------------------------------------------
-spec avg([binary()]) -> binary().
avg(Es) when is_list(Es), length(Es) > 0 ->
    mmath_trans:divide(sum(Es), length(Es)).

%%-------------------------------------------------------------------
%% Utility functions
%%-------------------------------------------------------------------

sum_(_A, _B) ->
    erlang:nif_error(nif_library_not_loaded).

sum_(_A, _B, _C) ->
    erlang:nif_error(nif_library_not_loaded).

sub_(_A, _B) ->
    erlang:nif_error(nif_library_not_loaded).

sub_(_A, _B, _C) ->
    erlang:nif_error(nif_library_not_loaded).

mul_(_A, _B) ->
    erlang:nif_error(nif_library_not_loaded).

mul_(_A, _B, _C) ->
    erlang:nif_error(nif_library_not_loaded).

div_(_A, _B) ->
    erlang:nif_error(nif_library_not_loaded).

div_(_A, _B, _C) ->
    erlang:nif_error(nif_library_not_loaded).

min_(_A, _B) ->
    erlang:nif_error(nif_library_not_loaded).

min_(_A, _B, _C) ->
    erlang:nif_error(nif_library_not_loaded).

max_(_A, _B) ->
    erlang:nif_error(nif_library_not_loaded).

max_(_A, _B, _C) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Combines a set of datasets with a given combinator function.
%% this requires the combinator to be associative!
%%--------------------------------------------------------------------

-type comb_fun2() :: fun((binary(), binary()) -> binary()).
-type comb_fun3() :: fun((binary(), binary(), binary()) -> binary()).

-spec rcomb(comb_fun2(), comb_fun3(), L :: [binary()]) ->
                   binary().
rcomb(F2, F3, [A, B, C | R]) when is_function(F3) ->
    rcomb(F2, F3, [F3(A, B, C) | R]);

rcomb(F2, F3, [A, B | R]) ->
    rcomb(F2, F3, [F2(A, B) | R]);
rcomb(_, _, [E]) ->
    E.
