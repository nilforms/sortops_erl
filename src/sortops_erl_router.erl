-module(sortops_erl_router).
-behaviour(nova_router).

-export([
         routes/1
        ]).

-include("router.hrl").

%% The Environment-variable is defined in your sys.config in {nova, [{environment, Value}]}
routes(_Environment) ->
    [
      #{prefix => "/api",
        security => false,
        plugins => [
                    {pre_request, nova_request_plugin, #{decode_json_body => true}},
		                {pre_request, nova_json_schemas, #{render_errors => true}}
                   ],
        routes => [
                    {"/", fun sortops_erl_api_controller:index/1, #{}},
                    {"/sort_job", fun sortops_erl_api_controller:show_json/1, ?ROUTE_OPTS},
                    {"/sort_to_script", fun sortops_erl_api_controller:show_script/1, ?ROUTE_OPTS}
                  ]
      }
    ].
