%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2014, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created :  8 Jun 2014 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(mmath_trans).

-export([
         %%map/2,
         derivate/1,
         confidence/1,
         mul/2,
         divide/2]).

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

mul(_M, _D) ->
    exit(nif_library_not_loaded).

divide(_M, _D) ->
    exit(nif_library_not_loaded).

derivate(_) ->
    exit(nif_library_not_loaded).

confidence(_) ->
    exit(nif_library_not_loaded).
