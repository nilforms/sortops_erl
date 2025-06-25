-module(sortops_erl_api_controller).
-export([
         index/1
        ]).

index(_Req) ->
    {ok, [{message, "Hello world!"}]}.
