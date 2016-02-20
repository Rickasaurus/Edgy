module Edgy.DFS

open Edgy.Core
open Edgy.DirectedPath

/// Generalized depth-first search over a graph
let dfsPure onNewVertex onEdge onDoneVertex initialState startAt (graph: IGraph<_,_>) =
    let adjlist = Graph.toAdjacencyList graph
    let lookupEdges v = adjlist.[v] |> Seq.toList
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
        | (v, e :: restEdges) :: rest ->
            let v' = (e : IEdge<_,_>).To 
            let work' = (v, restEdges) :: rest
            if not (discovered |> Set.contains v') then
                let state, cont = state |> onEdge v e
                if not cont then state else 
                    let newEdges = lookupEdges (v')
                    let state, cont = state |> onNewVertex v'
                    if not cont then state 
                    else inner ((v', newEdges) :: work') (discovered |> Set.add v') state
            else
                let state, cont = onEdge v e state
                if not cont then state
                else inner work' discovered state   
    if adjlist |> Map.isEmpty |> not then
        let state, cont = onNewVertex startAt initialState
        if not cont then state
        else inner [startAt, lookupEdges startAt] (Set.singleton startAt) state
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
let traverseWithNodeTimes (startAt: 'a) (graph: IGraph<_,_>) = 
    let onnewvertex v (time, entrytime, exittime) = let time = time + 1 in (time, entrytime |> Map.add v time, exittime), true
    let onedge _ _ state = state, true
    let ondonevertex v (time, entrytime, exittime) = (time, entrytime, exittime |> Map.add v time), true
    let finaltime, entryTimes, exitTimes = dfsPure onnewvertex onedge ondonevertex (0, Map.empty, Map.empty) startAt graph
    { FinalTime = finaltime; EntryTimes = entryTimes; ExitTimes = exitTimes}

/// Finds all verticies in the graph that match the predicate via Depth-first traversal, returns a list in visited order
let findVertices (startAt: 'a) (predicate: _ -> bool) (graph: IGraph<_,_>) = 
    let onNewVertex v acc = 
        let acc = if predicate v then v :: acc else acc
        acc, true
    let onEdge _ _ acc = acc, true
    let onDoneVertex _ acc = acc, true
    dfsPure onNewVertex onEdge onDoneVertex List.empty startAt graph 

/// Find first vertex in the graph that matches the predicate via Depth-first traversal
let findFirstVertex (startAt: 'a) (predicate: _ -> bool) (graph: IGraph<_,_>) = 
    let onNewVertex v acc = 
        if predicate v 
        then Some v, false 
        else acc, true
    let onEdge _ _ acc = acc, true
    let onDoneVertex _ acc = acc, true
    dfsPure onNewVertex onEdge onDoneVertex None startAt graph 

/// Finds all edges in the graph that match the predicate via Depth-first traversal, returns a list in visited order
let findEdges (startAt: 'a) (predicate: _ -> bool) (graph: IGraph<_,_>) = 
    let onNewVertex _ acc = acc, true
    let onEdge _ edge acc = 
        if predicate edge then edge :: acc, true
        else acc, true
    let onDoneVertex _ acc = acc, true
    dfsPure onNewVertex onEdge onDoneVertex List.empty startAt graph |> List.rev

/// Find first edge in the graph that matches the predicate via Depth-first traversal
let findFirstEdge  (startAt: 'a) (predicate: _ -> bool) (graph: IGraph<_,_>) = 
    let onNewVertex _ acc = acc, true
    let onEdge _ edge acc = 
        if predicate edge then Some edge, false
        else acc, true
    let onDoneVertex _ acc = acc, true
    dfsPure onNewVertex onEdge onDoneVertex None startAt graph 
