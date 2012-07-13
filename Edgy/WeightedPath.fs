module Edgy.WeightedDirectedPath
#nowarn "62"

#if INTERACTIVE 
#load "Utility.fs"
#load "Core.fs"
#endif

open Edgy.Core
open Edgy.Utility

/// Kruskal's algorithm for undirected weighted graphs.  Will treat all edges are non-directed.
/// O(NLogN) because of the Sort involved.
let kruskal (edges: (PathEdge<_>, _) Map) =
    let N = edges |> weightedEdgesToVertices |> Set.toSeq |> Seq.mapi (fun i n -> n,i) |> Map.ofSeq
    let rec inner S (V: QuickUWPC) F = 
        match S with
        | [] -> F
        | ((a,b), w) :: S -> 
            let ai, bi = N.[a], N.[b]            
            if not <| V.Find(ai,bi) 
            then V.Union(ai,bi)
                 inner S V (((a,b),w) :: F)
            else inner S V F
    let V = new QuickUWPC(N.Count)
    let S = edges |> Map.toList |> List.sortBy snd // sort low to high
    inner S V []

// Example
let totalWeightedGraph () =
    [
        WeightedPath "A" <=|1.0|= "B" =|0.5|=> "C" <=|0.2|= "D"
        WeightedPath "A" =|0.4|=> "B" =|0.2|=> "C"
    ] |> combineWeightedPaths

//val toalWeightedGraph : WeightedPath<string,float> =
//  {Tail = "D";
//   Edges =
//    map
//      [(("A", "E"), 0.4); (("B", "A"), 1.0); (("B", "C"), 0.5);
//       (("D", "C"), 0.2); (("E", "D"), 0.2)];}

let kruskaltest () =
    let graph = 
        [
            WeightedPath "A" =|7|=> "B" =|8|=> "C" =|5|=> "E" =|9|=> "G" =|11|=> "F" =|6|=> "D" =|5|=> "A"
            WeightedPath "D" =|9|=> "B" =|7|=> "E" =|15|=> "D"
            WeightedPath "E" =|8|=> "F"
        ] |> combineWeightedPaths
    printfn "%A" (kruskal (graph.Edges))
