%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2014, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created :  8 Jun 2014 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(mmath_aggr).

-export([load_nif/0]).
-export([sum/2,
         avg/2,
         min/2,
         max/2]).

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

avg(_Data, _Count) when _Count > 0 ->
    exit(nif_library_not_loaded).


sum(_Data, _Count) when _Count > 0 ->
    exit(nif_library_not_loaded).

min(_Data, _Count) when _Count > 0 ->
    exit(nif_library_not_loaded).


max(_Data, _Count) when _Count > 0  ->
    exit(nif_library_not_loaded).


%% map(Bin, Fn) ->
%%     map(Bin, 0, Fn, <<>>).

%% map(<<?INT:?TYPE_SIZE, I:?BITS/?INT_TYPE, Rest/binary>>, _, Fn, Acc) ->
%%     map(Rest, I, Fn, apl(Fn, I, Acc));
%% map(<<?NONE:?TYPE_SIZE, _:?BITS/?INT_TYPE, Rest/binary>>, L, Fn, Acc) ->
%%     map(Rest, L, Fn, apl(Fn, L, Acc));
%% map(<<B:?DATA_SIZE/binary, Rest/binary>>, _, Fn, Acc) ->
%%     [V] = mmath_bin:to_list(B),
%%     map(Rest, V, Fn, apl(Fn, V, Acc));
%% map(<<>>, _, _, Acc) ->
%%     Acc.

%% apl(Fn, V, Acc) ->
%%     case Fn(V) of
%%         V1 when is_integer(V1) ->
%%             <<Acc/binary, ?INT:?TYPE_SIZE, V1:?BITS/?INT_TYPE>>;
%%         V1 when is_float(V1) ->
%%             <<Acc/binary, (mmath_bin:from_list([V1]))/binary>>
%%     end.

%% find_first(<<>>) ->
%%     0;
%% find_first(<<?INT:?TYPE_SIZE, I:?BITS/?INT_TYPE, _/binary>>) ->
%%     I;
%% find_first(<<?NONE:?TYPE_SIZE, 0:?BITS/?INT_TYPE, Rest/binary>>) ->
%%     find_first(Rest).
