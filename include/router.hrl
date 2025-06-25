
-define(SCHEMA_PATH,  "./schemas/task.json").
-define(ROUTE_OPTS, #{methods => [post],
                      extra_state =>
                        #{json_schema => ?SCHEMA_PATH}}).