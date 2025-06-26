-module(sortops_erl_api_controller).
-export([
         index/1,
         show_json/1,
         show_script/1
        ]).

-include("model.hrl").

%% For correct error processing
-fallback_controller(sortops_erl_fallback_controller).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Controller callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
index(_Req) ->
    {status, 204}.

%% Since validation is performed using JSON schema
%% no default clause is needed
show_json(#{json := #{<<"tasks">> := InputJob}}) ->
    case topological_sort:sort(InputJob) of
        {ok, OutputJob} ->
            {json, 200, #{}, #{tasks => OutputJob}};
        Error ->
            Error
    end.

show_script(#{json := #{<<"tasks">> := InputJob}}) ->
    case topological_sort:sort(InputJob) of
        {ok, OutputJob} ->
            {json, 200, #{}, #{script => render_bash(OutputJob)}};
        Error ->
            Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec render_bash(job()) -> bitstring().
render_bash(Job) ->
    Header = <<"#!/usr/bin/env bash">>,
    Delimiter = <<"\n">>,
    lists:foldl(fun(#{<<"command">> := Command}, Acc) ->
        <<Acc/binary, Delimiter/binary, Command/binary>>
    end, Header, Job).
