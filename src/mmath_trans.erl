%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2014, Heinz Nikolaus Gies
%%% @doc
%%% Functions that transform metric.
%%% @end
%%% Created :  8 Jun 2014 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(mmath_trans).

-export([
         derivate/1,
         confidence/1,
         mul/2,
         divide/2,
         add/2,
         sub/2,
         sqrt/1]).

-include("mmath.hrl").

-define(APPNAME, mmath).
-define(LIBNAME, trans_nif).
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
%% Multiplies each value in the binary with the provided integer.
%% @end
%%--------------------------------------------------------------------
-spec mul(binary(), number()) -> binary().
mul(_M, _D) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Divides each value in the binary with the provided integer.
%% @end
%%--------------------------------------------------------------------
-spec divide(binary(), number()) -> binary().
divide(_M, _D) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Adds each value in the binary with the provided integer.
%% @end
%%--------------------------------------------------------------------
-spec add(binary(), number()) -> binary().
add(_M, _D) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Substract the given number from each element.
%% @end
%%--------------------------------------------------------------------
-spec sub(binary(), number()) -> binary().
sub(_M, _D) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Calculates the derivate of the values, this means the first value
%% is dropped (or taken as the initial value) with each following
%% value being calculated by derivate(n) = value(n) - value(n-1).
%%
%% The resulting binary will be one ellement shorter!
%% @end
%%--------------------------------------------------------------------
-spec derivate(binary()) -> binary().
derivate(_) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Transforms the series into the confidence score for each value.
%% Unset values have a confidence of 0% while set values have a
%% confidence of 100%. Aggregated values have the aggreated confidence
%% score.
%% @end
%%--------------------------------------------------------------------
-spec confidence(binary()) -> binary().
confidence(_) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Calculates the square root of each value in a series
%% @end
%%--------------------------------------------------------------------
-spec sqrt(binary()) -> binary().
sqrt(_) ->
    erlang:nif_error(nif_library_not_loaded).
