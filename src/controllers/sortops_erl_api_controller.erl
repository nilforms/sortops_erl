-module(sortops_erl_api_controller).
-export([
         index/1,
         show_json/1,
         show_script/1
        ]).

%% For correct error processing
-fallback_controller(sortops_erl_fallback_controller).

index(_Req) ->
    {status, 204}.

%% Since validation is performed using JSON schema
%% no default clause is needed
show_json(#{json := #{<<"tasks">> := _InputJob}}) ->
    {json, 200, #{}, #{}}.

show_script(#{json := #{<<"tasks">> := _InputJob}}) ->
    {json, 200, #{}, #{}}.
