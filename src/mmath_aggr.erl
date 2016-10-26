%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2014, Heinz Nikolaus Gies
%%% @doc
%%% Functions that aggregate metrics by grouping together chunks of values
%%% @end
%%% Created :  8 Jun 2014 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(mmath_aggr).

-export([load_nif/0]).
-export([sum/2,
         avg/2,
         min/2,
         max/2,
         variance/2,
         stddev/2,
         mean/2,
         percentile/3,
         first_below/3,
         last_below/3,
         first_above/3,
         last_above/3]).

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

%%--------------------------------------------------------------------
%% @doc
%% Aggregates a binary by combining the chunks into the average (mean)
%% of their values.
%% @end
%%--------------------------------------------------------------------
-spec avg(binary(), pos_integer()) -> binary().
avg(_Data, _Count) when _Count > 0 ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Aggregates a binary by combining the chunks into the sum of their
%% values.
%% @end
%%--------------------------------------------------------------------
-spec sum(binary(), pos_integer()) -> binary().
sum(_Data, _Count) when _Count > 0 ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Aggregates a binary by combining the chunks into the minimum value
%% in the chunk.
%% @end
%%--------------------------------------------------------------------
-spec min(binary(), pos_integer()) -> binary().
min(_Data, _Count) when _Count > 0 ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Aggregates a binary by combining the chunks into the maximum value
%% in the chunk.
%% @end
%%--------------------------------------------------------------------
-spec max(binary(), pos_integer()) -> binary().
max(_Data, _Count) when _Count > 0  ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Aggregates a binary by calculating the standard deviation for the
%% chunk.
%% @end
%%--------------------------------------------------------------------
-spec stddev(binary(), pos_integer()) -> binary().
stddev(Data, Count) ->
    mmath_trans:sqrt(variance(Data, Count)).

%%--------------------------------------------------------------------
%% @doc
%% Aggregates a binary by calculating the variance for the
%% chunk.
%% @end
%%--------------------------------------------------------------------
-spec variance(binary(), pos_integer()) -> binary().
variance(Data, Count) ->
    Mean = mmath_aggr:avg(Data, Count),
    Mean0 = mmath_bin:replicate(Mean, Count),
    Deltas = mmath_comb:diff([Data, Mean0]),
    SqrDeltas = mmath_comb:product([Deltas, Deltas]),
    mmath_aggr:avg(SqrDeltas, Count).

%%--------------------------------------------------------------------
%% @doc
%% Calculates the percentile of the data in a given chunk segmet.
%% percentiles are floates [0, 1]
%% @end
%%--------------------------------------------------------------------
-spec percentile(binary(), float(), pos_integer()) -> binary().
percentile(_Data, _N, _Count) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Return the first point in a given chunk that is below a given
%% threshold.
%% @end
%%--------------------------------------------------------------------
-spec first_below(binary(), float(), pos_integer()) -> binary().
first_below(_Data, _T, _Count) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Return the last point in a given chunk that is below a given
%% threshold.
%% @end
%%--------------------------------------------------------------------
-spec last_below(binary(), float(), pos_integer()) -> binary().
last_below(_Data, _T, _Count) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Return the first point in a given chunk that is above a given
%% threshold.
%% @end
%%--------------------------------------------------------------------
-spec first_above(binary(), float(), pos_integer()) -> binary().
first_above(_Data, _T, _Count) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Return the last point in a given chunk that is above a given
%% threshold.
%% @end
%%--------------------------------------------------------------------
-spec last_above(binary(), float(), pos_integer()) -> binary().
last_above(_Data, _T, _Count) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Calculates the mean of the data in a given chunk segmet.
%% @end
%%--------------------------------------------------------------------
-spec mean(binary(), pos_integer()) -> binary().
mean(Data, Count) ->
    percentile(Data, 0.5, Count).
