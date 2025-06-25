-module(topological_sort).

%%%-------------------------------------------------------------------
%% @doc
%% Implementation of topological sorting for a collection of
%% linux tasks.
%% Based on https://rosettacode.org/wiki/Topological_sort#Erlang
%% @end
%%%-------------------------------------------------------------------

-export([sort/1]).

%% Types defined

-type task() :: #{name => bitstring(),
                 command => bitstring(),
                 requires => [bitstring()]}.

-type job() :: [task()].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Model methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%-------------------------------------------------------------------
-spec sort(job()) -> {ok, job()} | {error, term()}.
%% @doc
%% Main sorting algorithm
%%
%% @end
%%%-------------------------------------------------------------------
sort(InJob) ->
    G = build_dependency_graph(InJob),
    case digraph:no_edges(G) of
        0 ->
            OutVertices = digraph:vertices(G),
            OutJob = get_job_from_verices(OutVertices, G),
            digraph:delete(G),
            {ok, OutJob};
        _Edges ->
            case digraph_utils:topsort(G) of
                false ->
                    digraph:delete(G),
                    {error, irresolvable_dependencies};
                Vertices ->
                    OutJob = get_job_from_verices(Vertices, G),
                    digraph:delete(G),
                    {ok, OutJob}
        end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%-------------------------------------------------------------------
-spec build_dependency_graph(job()) -> digraph:graph().
-spec build_dependency_graph(job(), digraph:graph()) -> digraph:graph().
%% @doc
%% Builds Dependency Graph from Job
%%
%% @end
%%%-------------------------------------------------------------------
build_dependency_graph(Job) ->
    build_dependency_graph(Job, digraph:new()).

build_dependency_graph(Job, G) ->
    lists:foldl(fun(#{<<"name">> := Name} = Task, Acc) ->
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

%%%-------------------------------------------------------------------
-spec get_job_from_verices([digraph:vertex()], digraph:graph()) -> job().
%% @doc
%% Forms Job from sorted Graph vertices
%%
%% @end
%%%-------------------------------------------------------------------
get_job_from_verices(Vertices, Graph) ->
    lists:foldr(fun(V, Acc) ->
                    case digraph:vertex(Graph, V) of
                        {_Name, Task} when is_map(Task) -> [Task | Acc];
                        _Vertex -> Acc % no task actually => non-existing dependecy
                    end
              end, [], Vertices).

%%%-------------------------------------------------------------------
-spec add_dependency(digraph:graph(), binary(), binary()) -> ok.
%% @doc
%% Adds dependecy to Graph
%%
%% @end
%%%-------------------------------------------------------------------
add_dependency(_G, L, L) ->
    ok;
add_dependency(G, L, D) ->
    case digraph:vertex(G, D) of
        false -> digraph:add_vertex(G, D);
        {_D, _Label} -> ok
    end,
    digraph:add_edge(G, D, L). % Dependencies represented as an edge D -> L