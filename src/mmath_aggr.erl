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
         stddev/2]).

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
