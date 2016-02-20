module Edgy.DFS

open Edgy.Core
open Edgy.DirectedPath

/// Generalized depth-first search over a graph
let dfsPure onNewVertex onEdge onDoneVertex (graph: IGraph<_,_>) =
    let adjlist = Graph.toAdjacencyList graph
    let lookupEdges v = adjlist.[v] |> Seq.map (fun e -> e.To) |> Seq.toList
    let rec inner work discovered state =
        match work with
        // Done
        | [] -> state
        // Post
        | (v, []) :: rest -> inner rest discovered (onDoneVertex v state)
        // Pre
        | (v, v' :: edges) :: rest ->
            let work' = (v, edges) :: rest
            if not (discovered |> Set.contains v') then
                let edges = lookupEdges v'
                inner ((v', edges) :: work') (discovered |> Set.add v') (state |> onNewVertex v' |> onEdge v v')
            else inner work' discovered (onEdge v v' state)       
    let first = adjlist |> Seq.nth 0
    inner [first.Key, lookupEdges first.Key] Set.empty

/// Depth-first graph traversal with integer ordering of when nodes were visted
let traverseWithTimes (graph: IGraph<_,_>) = 
    let onnewvertex v (time, entrytime, exittime) = let time = time + 1 in (time, entrytime |> Map.add v time, exittime)
    let onedge _ _ state = state
    let ondonevertex v (time, entrytime, exittime) = (time, entrytime, exittime |> Map.add v time)
    dfsPure onnewvertex onedge ondonevertex graph

let findVertices (predicate: _ -> bool) (graph: IGraph<_,_>) = 
    let onNewVertex v acc = if predicate v then Set.add v else acc
    let onEdge _ _ acc = acc
    let onDoneVertex v acc = acc
    let res = dfsPure onNewVertex onEdge onDoneVertex graph 
    ()