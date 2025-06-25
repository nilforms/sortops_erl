-module(api_error_test_SUITE).

-compile(export_all).

%% Includes
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(OPTS, #{close => true,
                headers => #{'Content-Type' => <<"application/json">>}}).
-define(BASE_URL, <<"http://localhost:8080/api">>).


init_per_suite(Config) ->
    application:ensure_all_started(sortops_erl),
    Config.

end_per_suite(_Config) ->
    application:stop(sortops_erl),
    ok.


all() ->
    [
        empty_body,
        missing_param,
        irresolvable_dep
    ].

empty_body(_Config) ->
    Json = <<>>,
    Endpoint = <<"/sort_job">>,
    #{status := {Status, _Name}, body := Body} = jhn_shttpc:post(<<?BASE_URL/binary, Endpoint/binary>>, Json, ?OPTS),
    ?assertEqual(400, Status),
    ?assertEqual(<<>>, Body).

missing_param(_Config) ->
    FailJob = #{<<"tasks">> =>
                [
                    #{<<"name">> => <<"task_2">>}
                ]
            },
    Json = iolist_to_binary(json:encode(FailJob)),
    Endpoint = <<"/sort_job">>,
    #{status := {Status, _Name}, body := Body} = jhn_shttpc:post(<<?BASE_URL/binary, Endpoint/binary>>, Json, ?OPTS),
    [#{<<"error_type">> := Error, <<"actual_value">> := MissinField}] = json:decode(Body),
    ?assertEqual(400, Status),
    ?assertEqual(<<"command">>, MissinField),
    ?assertEqual(<<"missing_required_property">>, Error).

irresolvable_dep(_Config) ->
    FailJob = #{<<"tasks">> =>
                [
                #{<<"name">> => <<"task_1">>, <<"command">> => <<"command1">>, <<"requires">> => [<<"task_2">>]},
                #{<<"name">> => <<"task_2">>, <<"command">> => <<"command2">>,  <<"requires">> => [<<"task_1">>]}
                ]
            },
    Json = iolist_to_binary(json:encode(FailJob)),
    Endpoint = <<"/sort_job">>,
    #{status := {Status, _Name}, body := Body} = jhn_shttpc:post(<<?BASE_URL/binary, Endpoint/binary>>, Json, ?OPTS),
    [#{<<"error_type">> := Error}] = json:decode(Body),
    ?assertEqual(400, Status),
    ?assertEqual(<<"irresolvable_dependency">>, Error).