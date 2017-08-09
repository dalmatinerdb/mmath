%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2015, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 31 Aug 2015 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(mmath_bench_SUITE).

-export([suite/0, groups/0, all/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2,
         from_list_case/1,
         to_list_case/1,
         realize_case/1,
         derealize_case/1,
         aggr_case_/2,
         sum_case/1,
         avg_case/1,
         min_case/1,
         max_case/1,
         comb_sum2_case/1,
         comb_sum3_case/1,
         comb_sum4_case/1
        ]).

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
    bin_cases() ++ aggr_cases() ++ comb_cases().

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
bin_cases() ->
    [from_list_case, to_list_case, realize_case, derealize_case].

aggr_cases() ->
    [min_case,
     max_case,
     sum_case,
     avg_case].

comb_cases() ->
    [comb_sum2_case, comb_sum3_case, comb_sum4_case].

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
    ct:print(default, "[bin] from_list: ~p", [T]),
    {comment, integer_to_list(T)}.

to_list_case(_Config) ->
    Bin = random_bin(?SIZE),
    {T, _} = timer:tc(mmath_bin, to_list, [Bin]),
    ct:print(default, "[bin] to_list: ~p", [T]),
    {comment, integer_to_list(T)}.

realize_case(_Config) ->
    B = random_bin(?SIZE),
    {T, _} = timer:tc(mmath_bin, realize, [B]),
    ct:print(default, "[bin] realize: ~p", [T]),
    {comment, integer_to_list(T)}.

derealize_case(_Config) ->
    B = mmath_bin:realize(random_bin(?SIZE)),
    {T, _} = timer:tc(mmath_bin, derealize, [B]),
    ct:print(default, "[bin] derealize: ~p", [T]),
    {comment, integer_to_list(T)}.

%%--------------------------------------------------------------------

aggr_case_(_Config, Aggr) ->
    Bin = random_bin(?SIZE),
    BinR = mmath_bin:realize(Bin),
    {T, _} = timer:tc(mmath_aggr, Aggr, [BinR, ?SIZE div 10]),
    ct:print(default, "[aggr] ~s: ~p", [Aggr, T]),
    {comment, integer_to_list(T)}.

sum_case(Config) ->
    aggr_case_(Config, sum).

avg_case(Config) ->
    aggr_case_(Config, avg).

min_case(Config) ->
    aggr_case_(Config, min).

max_case(Config) ->
    aggr_case_(Config, max).

%%--------------------------------------------------------------------

comb_sum2_case(_Config) ->
    B1 = random_bin(?SIZE),
    B2 = random_bin(?SIZE),
    R1 = mmath_bin:realize(B1),
    R2 = mmath_bin:realize(B2),
    {T, _} = timer:tc(mmath_comb, sum, [[R1, R2]]),
    ct:print(default, "[comb] sum2: ~p", [T]),
    {comment, integer_to_list(T)}.

comb_sum3_case(_Config) ->
    B1 = random_bin(?SIZE),
    B2 = random_bin(?SIZE),
    B3 = random_bin(?SIZE),
    R1 = mmath_bin:realize(B1),
    R2 = mmath_bin:realize(B2),
    R3 = mmath_bin:realize(B3),
    {T, _} = timer:tc(mmath_comb, sum, [[R1, R2, R3]]),
    ct:print(default, "[comb] sum3: ~p", [T]),
    {comment, integer_to_list(T)}.

comb_sum4_case(_Config) ->
    B1 = random_bin(?SIZE),
    B2 = random_bin(?SIZE),
    B3 = random_bin(?SIZE),
    B4 = random_bin(?SIZE),
    R1 = mmath_bin:realize(B1),
    R2 = mmath_bin:realize(B2),
    R3 = mmath_bin:realize(B3),
    R4 = mmath_bin:realize(B4),
    {T, _} = timer:tc(mmath_comb, sum, [[R1, R2, R3, R4]]),
    ct:print(default, "[comb] sum4: ~p", [T]),
    {comment, integer_to_list(T)}.

%%--------------------------------------------------------------------

random_list(Size) ->
    [rand:uniform() * 1000000 || _ <- lists:seq(0, Size)].

random_bin(Size) ->
    mmath_bin:from_list(random_list(Size)).

%% random_bin_r(Size) ->
%%     mmath_bin:realize(random_bin(Size)).
