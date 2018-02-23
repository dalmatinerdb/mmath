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
         realize/1, derealize/1, rdatasize/0, merge/2, replicate/2]).

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
-spec from_list([number()]) -> binary().
from_list(_) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Converts a the binary repesentation back to a list of values.
%% @end
%%--------------------------------------------------------------------
-spec to_list(binary()) -> [number()].
to_list(_) ->
    erlang:nif_error(nif_library_not_loaded).


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
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Converts a none realized to the realized data representation.
%% @end
%%--------------------------------------------------------------------
derealize(_Data) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% Utility function that replicates each element in `Data' by `Count'
%% times.  For example, replicate([1,2], 3) results in [1,1,1,2,2,2].
%% @end
%%--------------------------------------------------------------------
replicate(_Data, _Count) ->
    erlang:nif_error(nif_library_not_loaded).


%%--------------------------------------------------------------------
%% @doc
%% Utility function to ensure that the ?RDATA_SIZE macro is set
%% correctly.
%% @end
%%--------------------------------------------------------------------
rdatasize() ->
    erlang:nif_error(nif_library_not_loaded).


%%--------------------------------------------------------------------
%% @doc
%% Merges two metric lists, filling holes in one with the data of the
%% other.
%% @end
%%--------------------------------------------------------------------
merge(A, B) ->
    merge(A, B, <<>>).

merge(<<?NONE:?TYPE_SIZE, _:?BITS/?INT_TYPE, R1/binary>>,
      <<D:?DATA_SIZE/binary, R2/binary>>,
      Acc) ->
    merge(R1, R2, <<Acc/binary, D/binary>>);
merge(<<D:?DATA_SIZE/binary, R1/binary>>,
      <<_:?DATA_SIZE/binary, R2/binary>>,
      Acc) ->
    merge(R1, R2, <<Acc/binary, D/binary>>);
merge(<<>>, <<>>, Acc) ->
    Acc;

merge(<<>>, D, Acc) ->
    <<Acc/binary, D/binary>>;
merge(D, <<>>, Acc) ->
    <<Acc/binary, D/binary>>.

-ifdef(TEST).

size_test() ->
    ?assertEqual(?RDATA_SIZE, rdatasize()).

one_test() ->
    ?assertEqual([1], to_list(from_list([1]))),
    ?assertEqual([1.0], to_list(from_list([1.0]))),
    ?assertEqual([1, 1.0, 1], to_list(from_list([1, 1.0, 1]))).
-endif.
