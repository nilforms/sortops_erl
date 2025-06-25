-module(topological_sort_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%%%-------------------------------------------------------------------
%% tests normal sorting
%%
%% @end
%%%-------------------------------------------------------------------
normal_sort_test() ->
    InputJob =[
        #{<<"name">> => <<"task_1">>, <<"command">> => <<"command_1">>},
        #{<<"name">> => <<"task_2">>, <<"command">> => <<"command_2">>, <<"requires">> => [<<"task_3">>]},
        #{<<"name">> => <<"task_3">>, <<"command">> => <<"command_3">>, <<"requires">> => [<<"task_1">>]},
        #{<<"name">> => <<"task_4">>, <<"command">> => <<"command_4">>, <<"requires">> => [<<"task_2">>, <<"task_3">>]}
    ],
    OutputJob = [
        #{<<"name">> => <<"task_1">>, <<"command">> => <<"command_1">>},
        #{<<"name">> => <<"task_3">>, <<"command">> => <<"command_3">>},
        #{<<"name">> => <<"task_2">>, <<"command">> => <<"command_2">>},
        #{<<"name">> => <<"task_4">>, <<"command">> => <<"command_4">>}
    ],
    ?assertEqual(topological_sort:sort(InputJob), {ok, OutputJob}).

%%%-------------------------------------------------------------------
%% tests error return if circular dependency is found
%%
%% @end
%%%-------------------------------------------------------------------
circular_deps_test() ->
    FailJob =[
                #{<<"name">> => <<"task_1">>, <<"command">> => <<"command1">>, <<"requires">> => [<<"task_2">>]},
                #{<<"name">> => <<"task_2">>, <<"command">> => <<"command2">>,  <<"requires">> => [<<"task_1">>]}
                ],
    ?assertEqual(topological_sort:sort(FailJob), {error, irresolvable_dependencies}).

%%%-------------------------------------------------------------------
%% tests if no error occurs if non-existing dependencies added
%%
%% @end
%%%-------------------------------------------------------------------

wrong_deps_test() ->
    FailJob =[
                #{<<"name">> => <<"task_1">>, <<"command">> => <<"command1">>, <<"requires">> => [<<"task_3">>]},
                #{<<"name">> => <<"task_2">>, <<"command">> => <<"command2">>,  <<"requires">> => [<<"task_4">>]}
                ],
    OutJob  =[
                #{<<"name">> => <<"task_2">>, <<"command">> => <<"command2">>},
                #{<<"name">> => <<"task_1">>, <<"command">> => <<"command1">>}
                ],
    ?assertEqual(topological_sort:sort(FailJob), {ok, OutJob}).