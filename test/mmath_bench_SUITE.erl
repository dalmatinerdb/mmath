%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2015, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 31 Aug 2015 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(mmath_bench_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(SIZE, 100000).

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
    comb_cases() ++ bin_cases() ++ aggr_cases().

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
bin_cases() ->
    [from_list_case, to_list_case].

aggr_cases() ->
    [sum_case, avg_case, min_case, max_case, empty_case, scale_up_case,
     scale_down_case, derivate_case, mul_case, div_case].

comb_cases() ->
    [comb_sum4_case, comb_sum3_case, comb_sum2_case].
%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------

from_list_case(_Config) ->
    List = random_list(?SIZE),
    {T, _} = timer:tc(mmath_bin, from_list, [List]),
    ct:print(default, "[aggr] to_bin: ~p", [T]),
    {comment, "T: " ++ integer_to_list(T)}.

to_list_case(_Config) ->
    Bin = random_bin(?SIZE),
    {T, _} = timer:tc(mmath_bin, to_list, [Bin]),
    ct:print(default, "[aggr] to_list: ~p", [T]),
    {comment, integer_to_list(T)}.

sum_case(Config) ->
    aggr_case(Config, sum).

avg_case(Config) ->
    aggr_case(Config, avg).

min_case(Config) ->
    aggr_case(Config, min).

max_case(Config) ->
    aggr_case(Config, max).

empty_case(Config) ->
    aggr_case(Config, empty).

aggr_case(_Config, Aggr) ->
    Bin = random_bin(?SIZE),
    {T, _} = timer:tc(mmath_aggr, Aggr, [Bin, ?SIZE div 10]),
    ct:print(default, "[aggr] ~s: ~p", [Aggr, T]),
    {comment, integer_to_list(T)}.

scale_up_case(_Config) ->
    Bin = random_bin(?SIZE),
    {T, _} = timer:tc(mmath_aggr, scale, [Bin, 5]),
    ct:print(default, "[aggr] scale_up: ~p", [T]),
    {comment, integer_to_list(T)}.

scale_down_case(_Config) ->
    Bin = random_bin(?SIZE),
    {T, _} = timer:tc(mmath_aggr, scale, [Bin, 0.5]),
    ct:print(default, "[aggr] scale_down: ~p", [T]),
    {comment, integer_to_list(T)}.

mul_case(_Config) ->
    Bin = random_bin(?SIZE),
    {T, _} = timer:tc(mmath_aggr, mul, [Bin, 5]),
    ct:print(default, "[aggr] mul: ~p", [T]),
    {comment, integer_to_list(T)}.

div_case(_Config) ->
    Bin = random_bin(?SIZE),
    {T, _} = timer:tc(mmath_aggr, divide, [Bin, 2]),
    ct:print(default, "[aggr] div: ~p", [T]),
    {comment, integer_to_list(T)}.


derivate_case(_Config) ->
    Bin = random_bin(?SIZE),
    {T, _} = timer:tc(mmath_aggr, derivate, [Bin]),
    ct:print(default, "[aggr] derivate: ~p", [T]),
    {comment, integer_to_list(T)}.

%%--------------------------------------------------------------------

comb_sum2_case(_Config) ->
    B1 = random_bin(?SIZE),
    B2 = random_bin(?SIZE),
    {T, _} = timer:tc(mmath_comb, sum, [[B1, B2]]),
    ct:print(default, "[comb] sum2: ~p", [T]),
    {comment, integer_to_list(T)}.

comb_sum3_case(_Config) ->
    B1 = random_bin(?SIZE),
    B2 = random_bin(?SIZE),
    B3 = random_bin(?SIZE),
    {T, _} = timer:tc(mmath_comb, sum, [[B1, B2, B3]]),
    ct:print(default, "[comb] sum3: ~p", [T]),
    {comment, integer_to_list(T)}.

comb_sum4_case(_Config) ->
    B1 = random_bin(?SIZE),
    B2 = random_bin(?SIZE),
    B3 = random_bin(?SIZE),
    B4 = random_bin(?SIZE),
    {T, _} = timer:tc(mmath_comb, sum, [[B1, B2, B3, B4]]),
    ct:print(default, "[comb] sum4: ~p", [T]),
    {comment, integer_to_list(T)}.

%%--------------------------------------------------------------------

random_list(Size) ->
    [random:uniform(1000000) || _ <- lists:seq(0, Size)].

random_bin(Size) ->
    mmath_bin:from_list(random_list(Size)).
