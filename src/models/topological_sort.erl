-module(topological_sort).

-moduledoc """
Implementation of topological sorting for a collection of linux tasks.
Based on https://rosettacode.org/wiki/Topological_sort#Erlang
""".

-export([perform/1]).

-include("model.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Model methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-doc "Main sorting algorithm".
-spec perform(job()) -> {ok, job()} | {error, term()}.
perform([Task | []] = Job) ->
    case is_map_key(<<"requires">>, Task) of
        true -> {error, non_existing_dependencies};
        false -> {ok, Job}
    end;
perform(InJob) ->
    G = build_dependency_graph(InJob),
    NumTasks = length(InJob),
    NumEdges = digraph:no_edges(G),
    NumVertices = digraph:no_vertices(G),
    Result = if
                NumVertices < NumTasks -> {error, non_unique_task_names};
                NumVertices > NumTasks -> {error, non_existing_dependencies};
                NumEdges =:= 0 -> {ok, form_job_from_vertices(G)};
                true ->
                    case digraph_utils:topsort(G) of
                        false -> {error, irresolvable_dependencies};
                        Vertices -> {ok, form_job_from_vertices(G, Vertices)}
                    end
             end,
    digraph:delete(G),
    Result.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-doc "Builds Dependency Graph from Job".
-spec build_dependency_graph(job()) -> digraph:graph().
-spec build_dependency_graph(job(), digraph:graph()) -> digraph:graph().
build_dependency_graph(Job) ->
    build_dependency_graph(Job, digraph:new()).

build_dependency_graph(Job, G) ->
    lists:foldl(fun(Task, Acc) ->
                    Name = maps:get(<<"name">>, Task),
                    case maps:take(<<"requires">>, Task) of
                        {Deps, TaskNoDep} ->
                            digraph:add_vertex(Acc, Name, TaskNoDep),
                            lists:foreach(fun (D) ->
                                                add_dependency(Acc, Name, D)
                                        end, Deps);
                        error ->
                            digraph:add_vertex(Acc, Name, Task)
                    end,
                    Acc
                end, G, Job).

-doc "Forms Job from sorted Graph vertices".
-spec form_job_from_vertices(Graph) -> Result
    when Graph :: digraph:graph(),
         Result :: job().
-spec form_job_from_vertices(Graph, Vertices) -> Result
    when Graph :: digraph:graph(),
         Vertices :: [digraph:vertex()],
         Result :: job().
form_job_from_vertices(Graph) ->
    form_job_from_vertices(Graph, digraph:vertices(Graph)).
form_job_from_vertices(Graph, [V | Vertices]) ->
    {_Name, Task} = digraph:vertex(Graph, V),
    [Task | form_job_from_vertices(Graph, Vertices)];
form_job_from_vertices(_Graph, []) -> [].

-doc "Adds dependecy to Graph".
-spec add_dependency(digraph:graph(), binary(), binary()) -> ok.
add_dependency(_G, L, L) ->
    ok;
add_dependency(G, L, D) ->
    case digraph:vertex(G, D) of
        false -> digraph:add_vertex(G, D);
        {_D, _Label} -> ok
    end,
    digraph:add_edge(G, D, L).