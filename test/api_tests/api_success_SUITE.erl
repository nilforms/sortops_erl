-module(api_success_SUITE).

-compile(export_all).

%% Includes
-include("test.hrl").

init_per_suite(Config) ->
    application:ensure_all_started(sortops_erl),
    Job =  #{<<"tasks">> =>
                [
                #{<<"name">> => <<"task_1">>, <<"command">> => <<"command1">>, <<"requires">> => [<<"task_2">>]},
                #{<<"name">> => <<"task_2">>, <<"command">> => <<"command2">>}
                ]
            },
    Json = json:encode(Job),
    [{json, iolist_to_binary(Json)} | Config].

end_per_suite(_Config) ->
    application:stop(sortops_erl),
    ok.



all() ->
    [
        sort_job,
        sort_and_show_bash
    ].

sort_job(Config) ->
    Json = proplists:get_value(json, Config),
    Endpoint = <<"/sort_job">>,
    OutJob = #{<<"tasks">> =>
                [
                    #{<<"name">> => <<"task_2">>, <<"command">> => <<"command2">>},
                    #{<<"name">> => <<"task_1">>, <<"command">> => <<"command1">>}
                ]
            },
    #{status := {Status, _Name}, body := Body} = jhn_shttpc:post(<<?BASE_URL/binary, Endpoint/binary>>, Json, ?OPTS),
    ?assertEqual(200, Status),
    ?assertEqual(OutJob, json:decode(Body)).

sort_and_show_bash(Config) ->
    Json = proplists:get_value(json, Config),
    Endpoint = <<"/sort_to_script">>,
    Output = #{<<"script">> =><<"#!/usr/bin/env bash\ncommand2\ncommand1">>},
    #{status := {Status, _Name}, body := Body} = jhn_shttpc:post(<<?BASE_URL/binary, Endpoint/binary>>, Json, ?OPTS),
    ?assertEqual(200, Status),
    ?assertEqual(Output, json:decode(Body)).