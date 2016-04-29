%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2014, Heinz Nikolaus Gies
%%% @doc
%%% Binary manipulation functions.
%%% @end
%%% Created :  8 Jun 2014 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(mmath_bin).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("mmath.hrl").

-export([from_list/1, to_list/1, empty/1, length/1, length_r/1,
         realize/1, derealize/1, rdatasize/0]).

-define(APPNAME, mmath).
-define(LIBNAME, bin_nif).
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
from_list(_) ->
    exit(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Converts a the binary repesentation back to a list of values.
%% @end
%%--------------------------------------------------------------------
to_list(_) ->
    exit(nif_library_not_loaded).


%%--------------------------------------------------------------------
%% @doc
%% Calculates the number of values in a binary representation.
%% @end
%%--------------------------------------------------------------------
length(B) ->
    trunc(byte_size(B)/?DATA_SIZE).

%%--------------------------------------------------------------------
%% @doc
%% Calculates the number of values in a binary realized
%% representation.
%% @end
%%--------------------------------------------------------------------
length_r(B) ->
    trunc(byte_size(B)/?RDATA_SIZE).

%%--------------------------------------------------------------------
%% @doc
%% Creates an empty binary representaton if a given list
%% representation.
%% @end
%%--------------------------------------------------------------------
empty(Length) ->
    <<0:((?TYPE_SIZE + ?BITS)*Length)/?INT_TYPE>>.

%%--------------------------------------------------------------------
%% @doc
%% Converts a none realized to the realized data representation.
%% @end
%%--------------------------------------------------------------------
realize(_Data) ->
    exit(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Converts a none realized to the realized data representation.
%% @end
%%--------------------------------------------------------------------
derealize(_Data) ->
    exit(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Utility function to ensure that the ?RDATA_SIZE macro is set
%% correctly.
%% @end
%%--------------------------------------------------------------------
rdatasize() ->
    exit(nif_library_not_loaded).


-ifdef(TEST).

size_test() ->
    ?assertEqual(?RDATA_SIZE, rdatasize()).
-endif.
