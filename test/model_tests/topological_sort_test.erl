-module(topological_sort_test).
-moduledoc """
Test suite for topological sorting algorithm.
""".

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-doc "tests normal sorting".
normal_sort_test() ->
    InJob = [
             #{<<"name">> => <<"task_1">>, <<"command">> => <<"command_1">>},
             #{<<"name">> => <<"task_2">>, <<"command">> => <<"command_2">>, <<"requires">> => [<<"task_3">>]},
             #{<<"name">> => <<"task_3">>, <<"command">> => <<"command_3">>, <<"requires">> => [<<"task_1">>]},
             #{<<"name">> => <<"task_4">>, <<"command">> => <<"command_4">>, <<"requires">> => [<<"task_2">>, <<"task_3">>]}
             ],
    OutJob = [
              #{<<"name">> => <<"task_1">>, <<"command">> => <<"command_1">>},
              #{<<"name">> => <<"task_3">>, <<"command">> => <<"command_3">>},
              #{<<"name">> => <<"task_2">>, <<"command">> => <<"command_2">>},
              #{<<"name">> => <<"task_4">>, <<"command">> => <<"command_4">>}
             ],
    ?assertEqual({ok, OutJob}, topological_sort:perform(InJob)).

-doc "tests error return if circular dependency is found".
circular_deps_test() ->
    FailJob = [
                #{<<"name">> => <<"task_1">>, <<"command">> => <<"command1">>, <<"requires">> => [<<"task_2">>]},
                #{<<"name">> => <<"task_2">>, <<"command">> => <<"command2">>,  <<"requires">> => [<<"task_1">>]}
              ],
    ?assertEqual({error, irresolvable_dependencies}, topological_sort:perform(FailJob)).

-doc "tests error return in case when non-existing tasks specified as dependencies".
wrong_deps_test() ->
    FailJob = [
               #{<<"name">> => <<"task_1">>, <<"command">> => <<"command1">>, <<"requires">> => [<<"task_3">>]},
               #{<<"name">> => <<"task_2">>, <<"command">> => <<"command2">>,  <<"requires">> => [<<"task_4">>]}
              ],
    ?assertEqual({error, non_existing_dependencies}, topological_sort:perform(FailJob)).

-doc "tests no error if dependencies are not specified".
no_deps_test() ->
    InJob = [
             #{<<"name">> => <<"task_1">>, <<"command">> => <<"command1">>},
             #{<<"name">> => <<"task_2">>, <<"command">> => <<"command2">>}
            ],
    ?assertEqual({ok, InJob}, topological_sort:perform(InJob)).

-doc "tests no error if empty r\"requires\" keys present in multiple tasks".
empty_requires_fields_test() ->
    InJob =  [
              #{<<"name">> => <<"task_1">>, <<"command">> => <<"command1">>, <<"requires">> => []},
              #{<<"name">> => <<"task_2">>, <<"command">> => <<"command2">>, <<"requires">> => []}
             ],
    OutJob = [
              #{<<"name">> => <<"task_1">>, <<"command">> => <<"command1">>},
              #{<<"name">> => <<"task_2">>, <<"command">> => <<"command2">>}
             ],
    ?assertEqual({ok, OutJob}, topological_sort:perform(InJob)).

-doc "tests error return if two tasks have the same name".
same_task_name_test() ->
    FailJob = [
               #{<<"name">> => <<"name1">>, <<"command">> => <<"command1">>},
               #{<<"name">> => <<"name1">>, <<"command">> => <<"command2">>}
              ],
    ?assertEqual({error, non_unique_task_names}, topological_sort:perform(FailJob)).

-doc "tests no error if single task is defined in a job".
singe_task_test() ->
    InJob = [
             #{<<"name">> => <<"task_1">>, <<"command">> => <<"command1">>}
            ],
    ?assertEqual({ok, InJob}, topological_sort:perform(InJob)).

-doc "tests error return if \"requires\" is defined in the single-task job".
same_task_requores_error_test() ->
    FailJob = [
               #{<<"name">> => <<"name1">>, <<"command">> => <<"command1">>, <<"requires">> => [<<"task_3">>]}
              ],
    ?assertEqual({error, non_existing_dependencies}, topological_sort:perform(FailJob)).