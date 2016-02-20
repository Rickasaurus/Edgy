module Edgy.DirectedPath
#nowarn "62"

#if INTERACTIVE 
#load "Core.fs"
#endif

open Edgy
open Edgy.Core
open Edgy.DSL

/// Given a set of edges, find all leaves
let leaves (edges: IEdge<'e,'ew> seq) = 
    edges |> Seq.collect (fun edge -> [edge.From; edge.To]) |> Seq.countBy id |> Seq.filter (snd>>(=)1) |> Seq.map fst

/// Given a set of directed edges represented as tuples s.t. (a,b) implies a is the parent and b is the child, find all nodes and their parents 
let allNodesWithParents (edges: IEdge<'e,'ew> seq) =
    edges |> Seq.collect (fun edge -> [edge.To, Some edge; edge.From, None]) 
    |> Seq.groupBy fst |> Seq.map (fun (n, seq) -> n, seq |> Seq.map snd |> Seq.choose id)

/// Given a set of directed edges represented as tuples s.t. (a,b) implies a is the parent and b is the child, find all nodes with parents and those parents
let nodesWithParents (edges: IEdge<'e,'ew> seq) = 
    edges |> Seq.map (fun edge -> edge.From, edge ) |> Seq.groupBy fst |> Seq.map (fun (n, seq) -> n, seq |> Seq.map snd)

/// Given a set of edges, find all nodes and those they are connected to
let allNodesWithConnected (edges: IEdge<'e,'ew> seq) = 
    edges |> Seq.collect (fun edge -> [edge.From, edge; edge.To, edge]) 
    |> Seq.groupBy fst |> Seq.map (fun (n, seq) -> n, seq |> Seq.map snd)

