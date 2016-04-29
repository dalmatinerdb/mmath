-module(mmath_bin).

-include("mmath.hrl").

-export([convert/1, from_list/1, to_list/1, empty/1, length/1, length_r/1,
         realize/1, derealize/1, complete_size_r/2]).

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

from_list([]) ->
    <<>>;

from_list([_V0 | _] = L) when is_integer(_V0) ->
    << <<?INT:?TYPE_SIZE, V:?BITS/?INT_TYPE>> || V <- L >>.

to_list(Bin) ->
    to_list_int(Bin, 0, []).

to_list_int(<<?INT:?TYPE_SIZE, I:?BITS/?INT_TYPE, R/binary>>, _, Acc) ->
    to_list_int(R, I, [I | Acc]);
to_list_int(<<?NONE:?TYPE_SIZE, _:?BITS/?INT_TYPE, R/binary>>, Last, Acc) ->
    to_list_int(R, Last, [Last | Acc]);
to_list_int(<<>>, _, Acc) ->
    lists:reverse(Acc).

length(B) ->
    trunc(byte_size(B)/?DATA_SIZE).

length_r(_B) ->
    exit(nif_library_not_loaded).

empty(Length) ->
    <<0:((?TYPE_SIZE + ?BITS)*Length)/?INT_TYPE>>.

convert(Data) ->
    convert(Data, <<>>).

convert(<<>>, Acc) ->
    Acc;
convert(<<0, _:64/?INT_TYPE, R/binary>>, Acc) ->
    convert(R, <<Acc/binary, ?NONE:?TYPE_SIZE, 0:?BITS/?INT_TYPE>>);
convert(<<1, I:64/?INT_TYPE, R/binary>>, Acc) ->
    convert(R, <<Acc/binary, ?INT:?TYPE_SIZE, I:?BITS/?INT_TYPE>>).


realize(Data) ->
    realize(Data, 0, <<>>).

realize(<<0:?TYPE_SIZE, _:?BITS/?INT_TYPE, R/binary>>, Last, Acc) ->
    realize(R, Last, <<Acc/binary, Last:64/integer-native>>);

realize(<<?INT:?TYPE_SIZE, I:?BITS/?INT_TYPE, R/binary>>, _Last, Acc) ->
    realize(R, I, <<Acc/binary, I:64/integer-native>>);
realize(<<>>, _, Acc) ->
    Acc.

derealize(Data) ->
    << <<?INT:?TYPE_SIZE, D:?BITS/?INT_TYPE>> ||
        <<D:64/integer-native>> <= Data>>.

complete_size_r(_B, _N) ->
    exit(nif_library_not_loaded).
