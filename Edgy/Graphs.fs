module Graphs

type Graph<'a when 'a: comparison> = 
    {   
        /// Element to attach to
        Current: 'a
        /// (From --> To) Set
        Edges: ('a * 'a) Set
    }

type Graph
    with
        // N <== a
        static member op_LessEqualsEquals (l: Graph<'a>, r: 'a) : Graph<'a> =
            { Current = r; Edges = l.Edges |> Set.add (r, l.Current) }
        // N ==> a
        static member op_EqualsEqualsGreater (l: Graph<'a>, r: 'a) : Graph<'a> =
            { Current = r; Edges = l.Edges |> Set.add (l.Current, r) }

/// Pseudo Graph Record Type Constructor
let Graph (inval: 'a) = { Current = inval; Edges = Set.empty }

/// Combines all graphs
let combine = Seq.reduce (fun l r -> { r with Edges = Set.union l.Edges r.Edges })

/// Finds all leaves
let leaves r = r.Edges |> Seq.collect (fun (l,r) -> [l;r]) |> Seq.countBy id |> Seq.filter (snd>>(=)1) |> Seq.map fst

/// Finds a set of all nodes
let allNodes r = r.Edges |> Seq.collect (fun (l,r) -> [l;r]) |> Set.ofSeq

/// Finds all nodes, and their parents 
let allNodesWithParents r = r.Edges 
                            |> Seq.collect (fun (l,r) -> [l, Set.empty; r, Set.singleton l]) 
                            |> Seq.groupBy fst 
                            |> Seq.map (fun (n, seq) -> n, seq |> Seq.map snd |> Seq.reduce Set.union)

/// Finds all nodes with parents, and those parents
let nodesWithParents r = r.Edges |> Seq.groupBy snd |> Seq.map (fun (n, seq) -> n, seq |> Seq.map fst)

// Example
let totalGraph = 
    [
        Graph 2 <== 1
        Graph 2 <== 1 ==> 3
        Graph 2 ==> 4 <== 3 ==> 5
        Graph 2 ==> 4 <== 3 <== 8
        Graph 4 ==> 6
    ] |> combine

// val totalGraph : Graph<int> =
//  {Current = 6;
//   Edges = set [(1, 2); (1, 3); (2, 4); (3, 4); (3, 5); (4, 6); (8, 3)];}