-module(sortops_erl_fallback_controller).
-export([
    resolve/2
]).

resolve(Req, {error, Error}) ->
    {json, 400, #{}, #{<<"error_type">> => Error}}.