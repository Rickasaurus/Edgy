module Edgy.DFS

open Edgy.Core
open Edgy.DirectedPath

let dfsPure onnewvertex onedge ondonevertex (graph: IGraph<_,_>) =
    let adjlist = Graph.toAdjacencyList graph
    let lookupEdges v = adjlist.[v] |> Seq.map (fun e -> e.To) |> Seq.toList
    let rec inner work discovered state =
        match work with
        // Done
        | [] -> state
        // Post
        | (v, []) :: rest -> inner rest discovered (ondonevertex v state)
        // Pre
        | (v, v' :: edges) :: rest ->
            let work' = (v, edges) :: rest
            if not (discovered |> Set.contains v') then
                let edges = lookupEdges v'
                inner ((v', edges) :: work') (discovered |> Set.add v') (state |> onnewvertex v' |> onedge v v')
            else inner work' discovered (onedge v v' state)       
    let first = adjlist |> Seq.nth 0
    inner [first.Key, lookupEdges first.Key] Set.empty

let dfsPureWithTimes (graph: IGraph<_,_>) = 
    let onnewvertex v (time, entrytime, exittime) = let time = time + 1 in (time, entrytime |> Map.add v time, exittime)
    let onedge v v' state = state
    let ondonevertex v (time, entrytime, exittime) = (time, entrytime, exittime |> Map.add v time)
    dfsPure onnewvertex onedge ondonevertex graph
