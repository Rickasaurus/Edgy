module Graph

open Edgy.Core

let toAdjacencyList (graph: IGraph<_,_>) = 
    graph.Edges 
    |> Seq.groupBy (fun e -> e.From)
    |> Seq.map (fun (k,v) -> v |> Seq.map (fun edge -> edge.To))
    |> Seq.toArray

let toAdjacencyMatrix (graph: IGraph<_,_>) =
    let arr = Array2D.zeroCreate (graph.NumVertices) (graph.NumVertices)
    for edge in graph.Edges do
        arr.[edge.From, edge.To] <- 1
    arr