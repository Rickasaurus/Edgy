module Graph

open Edgy.Core

let toAdjacencyList (graph: IGraph<_,_>) = 
    graph.Edges 
    |> Seq.collect (fun edge -> [edge.To, None; edge.From, Some edge]) 
    |> Seq.groupBy fst
    |> Seq.map (fun (n, seq) -> n, seq |> Seq.map snd |> Seq.choose id) 
    |> Map.ofSeq

let toAdjacencyMatrix (graph: IGraph<_,_>) =
    let arr = Array2D.zeroCreate (graph.NumVertices) (graph.NumVertices)
    for edge in graph.Edges do
        arr.[edge.From, edge.To] <- 1
    arr