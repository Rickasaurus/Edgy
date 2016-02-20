module Edgy.DFS

open Edgy.Core
open Edgy.DirectedPath

/// Generalized depth-first search over a graph
let dfsPure onNewVertex onEdge onDoneVertex initialState (graph: IGraph<_,_>) =
    let adjlist = Graph.toAdjacencyList graph
    let lookupEdges v = adjlist.[v] |> Seq.map (fun e -> e.To) |> Seq.toList
    let rec inner work discovered state =
        match work with
        // Done
        | [] -> state
        // Post
        | (v, []) :: rest -> 
            let state, cont = onDoneVertex v state
            if not cont then state 
            else inner rest discovered state
        // Pre
        | (v, v' :: edges) :: rest ->
            let work' = (v, edges) :: rest
            if not (discovered |> Set.contains v') then
                let edges = lookupEdges v'
                let state, cont = state |> onNewVertex v'
                if not cont then state 
                else 
                    let state, cont = state |> onEdge v v'
                    if not cont then state else
                        inner ((v', edges) :: work') (discovered |> Set.add v') state
            else
                let state, cont = onEdge v v' state
                if not cont then state
                else inner work' discovered state   
    if adjlist |> Seq.isEmpty |> not then
        let first = adjlist |> Seq.nth 0
        inner [first.Key, lookupEdges first.Key] Set.empty initialState
    else initialState

/// Result container for the traverseWithTimes function
type TraversalResult<'a when 'a : comparison> = 
    {
        /// The time when the graph is done being traversed 
        FinalTime: int
        /// A map of verticies to when they were first visited
        EntryTimes: Map<'a, int>
        /// a map of verticies to when they were exited
        ExitTimes: Map<'a, int>
    }

/// Depth-first graph traversal with integer ordering of when nodes were visted
let traverseWithTimes (graph: IGraph<_,_>) = 
    let onnewvertex v (time, entrytime, exittime) = let time = time + 1 in (time, entrytime |> Map.add v time, exittime), true
    let onedge _ _ state = state, true
    let ondonevertex v (time, entrytime, exittime) = (time, entrytime, exittime |> Map.add v time), true
    let finaltime, entryTimes, exitTimes = dfsPure onnewvertex onedge ondonevertex (0, Map.empty, Map.empty) graph
    { FinalTime = finaltime; EntryTimes = entryTimes; ExitTimes = exitTimes}

/// Finds all verticies in the graph that match the predicate via Depth-first traversal
let findVertices (predicate: _ -> bool) (graph: IGraph<_,_>) = 
    let onNewVertex v acc = 
        let acc = if predicate v then acc |> Set.add v else acc
        acc, true
    let onEdge _ _ acc = acc, true
    let onDoneVertex _ acc = acc, true
    dfsPure onNewVertex onEdge onDoneVertex Set.empty graph 

/// Find first vertex in the graph that matches the predicate via Depth-first traversal
let findFirstVertex (predicate: _ -> bool) (graph: IGraph<_,_>) = 
    let onNewVertex v acc = 
        if predicate v 
        then Some v, false 
        else acc, true
    let onEdge _ _ acc = acc, true
    let onDoneVertex _ acc = acc, true
    dfsPure onNewVertex onEdge onDoneVertex None graph 

/// Finds all edges in the graph that match the predicate via Depth-first traversal
let findEdges (predicate: _ -> bool) (graph: IGraph<_,_>) = 
    let onNewVertex _ acc = acc, true
    let onEdge _ _ acc = acc, true
    let onDoneVertex _ acc = acc, true
    dfsPure onNewVertex onEdge onDoneVertex Set.empty graph 