%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2014, Heinz Nikolaus Gies
%%% @doc
%%% Binary manipulation functions.
%%% @end
%%% Created :  8 Jun 2014 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(mmath_hpts).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("mmath.hrl").
-export([from_list/1, to_list/1, empty/1, length/1, values/1, timestamps/1]).

-define(APPNAME, mmath).
-define(LIBNAME, hpts_nif).
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
%% Converts a list of values to it's binary representation.
%% @end
%%--------------------------------------------------------------------
-spec from_list([{non_neg_integer(), number()}]) -> binary().
from_list(L) ->
    from_list(L, <<>>).
from_list([], Acc) ->
    Acc;
from_list([{T, V} | R], Acc) ->
    from_list(R, <<Acc/binary, (mmath_bin:from_list([V]))/binary,
                   T:64/unsigned-integer>>).

%%--------------------------------------------------------------------
%% @doc
%% Converts a the binary repesentation back to a list of values.
%% @end
%%--------------------------------------------------------------------
-spec to_list(binary()) -> [{non_neg_integer(), number()}].
to_list(B) ->
    to_list(B, []).
to_list(<<>>, Acc) ->
    lists:reverse(Acc);
to_list(<<VB:8/binary, T:64/unsigned-integer, R/binary>>, Acc) ->
    [V] = mmath_bin:to_list(VB),
    to_list(R, [{T, V} | Acc]).

%%--------------------------------------------------------------------
%% @doc
%% Extracts values
%% @end
%%--------------------------------------------------------------------
values(_Data) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Extracts timestamps
%% @end
%%--------------------------------------------------------------------
timestamps(_Data) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Calculates the number of values in a binary representation.
%% @end
%%--------------------------------------------------------------------
length(B) ->
    mmath_bin:length(B) div 2.

%%--------------------------------------------------------------------
%% @doc
%% Creates an empty binary representaton if a given list
%% representation.
%% @end
%%--------------------------------------------------------------------
empty(C) ->
    mmath_bin:empty(C * 2).


-ifdef(TEST).
values_test() ->
    ?assertEqual(<<0:64, 2:64>>, values(<<0:64, 1:64, 2:64, 3:64>>)).
timestamps_test() ->
    ?assertEqual(<<1:64, 3:64>>, timestamps(<<0:64, 1:64, 2:64, 3:64>>)).
one_test() ->
    ?assertEqual([{0, 1}], to_list(from_list([{0, 1}]))),
    ?assertEqual([{0, 1.0}], to_list(from_list([{0, 1.0}]))),
    ?assertEqual([{0, 1}, {0, 1.0}, {0, 1}],
                 to_list(from_list([{0, 1}, {0, 1.0}, {0, 1}]))).
-endif.
