-module(sortops_erl_fallback_controller).
-export([
    resolve/2
]).

resolve(_Req, {error, Error}) ->
    {json, 400, #{}, [#{error_type => Error}]};
resolve(_Req, _Error) ->
    {json, 500, #{}, [#{error_type => unknown}]}.