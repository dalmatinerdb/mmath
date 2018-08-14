%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2014, Heinz Nikolaus Gies
%%% @doc
%%% Functions that transform metric.
%%% @end
%%% Created :  8 Jun 2014 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(mmath_trans).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
         derivate/1,
         confidence/1,
         replace_below_confidence/3,
         mul/2,
         divide/2,
         add/2,
         sub/2,
         min/2,
         max/2,
         abs/1,
         sqrt_scale/1,
         log10_scale/1
        ]).

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
%% Transforms the series such that values with confidence lower
%% than or equal to the threshold will be reset to the given default
%% value.
%% @end
%%--------------------------------------------------------------------
-spec replace_below_confidence(binary(), number(), number()) -> binary().
replace_below_confidence(_, _Threshold, _Default) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Calculates the square root of each value in a series.
%% Note for convinience: sqrt(-X) == - sqrt(X).
%% @end
%%--------------------------------------------------------------------
-spec sqrt_scale(binary()) -> binary().
sqrt_scale(_) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Calculates the log10 of each value in a series.
%% Note for convinience: log10(-X) == - log10(X).
%% @end
%%--------------------------------------------------------------------
-spec log10_scale(binary()) -> binary().
log10_scale(_) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Calculates abs for each value in a series
%% @end
%%--------------------------------------------------------------------
-spec abs(binary()) -> binary().
abs(_) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Sets the minimum to each element of the array.
%% @end
%%--------------------------------------------------------------------
-spec min(binary(), number()) -> binary().
min(_M, _D) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Sets the maximum to each element of the array.
%% @end
%%--------------------------------------------------------------------
-spec max(binary(), number()) -> binary().
max(_M, _D) ->
    erlang:nif_error(nif_library_not_loaded).

-ifdef(TEST).

%% first 8 octets is float serialized value
%% last 8 octets is float serialized confidence
-define(EMPTY, <<0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0>>).

unconvert_h(X) ->
    mmath_bin:to_list(mmath_bin:derealize(X)).

convert_h(X) ->
    mmath_bin:realize(mmath_bin:from_list(X)).

replace_below_confidence_test() ->
    ?assertEqual([1.0],
                 unconvert_h(
                   mmath_trans:replace_below_confidence(
                     ?EMPTY, 0.9, 1.0))),
    ?assertEqual([0.124, 2.0],
                 unconvert_h(
                   mmath_trans:replace_below_confidence(
                     iolist_to_binary([?EMPTY, convert_h([2.0])]),
                     0.9, 0.124))),
    ?assertEqual([0.124, 0.122, 2.0, 0.124],
                 unconvert_h(
                   mmath_trans:replace_below_confidence(
                     iolist_to_binary([?EMPTY, convert_h([0.122, 2.0]), ?EMPTY]),
                     0.9, 0.124))),
    ?assertEqual([0.124, 0.122, 2.0, 0.124, 14.09],
                 unconvert_h(
                   mmath_trans:replace_below_confidence(
                     iolist_to_binary([?EMPTY, convert_h([0.122, 2.0]), ?EMPTY,
                                      convert_h([14.09])]),
                     0.9, 0.124))).
-endif.
