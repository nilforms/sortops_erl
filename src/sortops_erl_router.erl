-module(sortops_erl_router).
-behaviour(nova_router).

-export([
         routes/1
        ]).

%% The Environment-variable is defined in your sys.config in {nova, [{environment, Value}]}
routes(_Environment) ->
    [#{prefix => "",
      security => false,
      routes => [
                 {"/", fun(_) -> {status, 403} end, #{methods => [get]}}
                ]
      }].
