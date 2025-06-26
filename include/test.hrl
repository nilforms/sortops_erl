-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(OPTS, #{close => true,
                headers => #{'Content-Type' => <<"application/json">>}}).
-define(BASE_URL, <<"http://localhost:8080/api">>).
