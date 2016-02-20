namespace Edgy
#nowarn "62"

/// The base types for the whole library
module Core = 
    /// The base type for Edgy edges
    type IEdge<'e, 'ew when 'e: comparison> =
        interface
            /// The vertex this edge comes from 
            abstract From: 'e
            /// The vertex this edge goes to
            abstract To: 'e
            /// The contents of the edge (for tagging and such)
            abstract Contents: 'ew
        end

    //type IVertex<'n, 'e, 'ec> 

    //type GraphProperties =
    //    | None = 0
    //    | Simple = 1
    //    | Cyclic = 2

    /// The base type for Edgy graphs
    type IGraph<'e, 'ec when 'e: comparison> =
        interface
            /// Enumerates the graph edges
            abstract Edges: IEdge<'e, 'ec> seq
            /// Enumerates the graph vertices
            abstract Vertices: 'e seq
            /// Gets the number of edges
            abstract NumEdges: int
            /// Gets the number of vertices
            abstract NumVertices: int
    //        abstract Properties: GraphProperties
        end

    /// The base type for an edge in a path
    type PathEdge<'e when 'e: comparison> = 'e * 'e

/// IGraph combinators
module Graph = 
    open Core

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


/// a DSL for constructing paths in F# 
module DSL =
    open Core

    type Path<'v, 'w when 'v: comparison and 'w: comparison> = 
        {   
            /// The last element attached
            Tail: 'v
            /// (From --> To) Set
            Edges: (PathEdge<'v>, 'w) Map
        }

    type Weight<'v, 'w when 'v: comparison and 'w: comparison> = 
        {
            /// Previous Path
            Prev: Path<'v,'w>
            /// Previous Weight
            Weight: 'w
        }

    type Path with
        /// 'a =| 'n
        static member (=|) (l: Path<'v,'w>, n: 'w) : Weight<'v,'w> = { Prev = l; Weight = n }
        /// 'a <=| 'n
        static member (<=|) (l: Path<'v,'w>, n: 'w) : Weight<'v,'w> = { Prev = l; Weight = n }
        /// N <== a (no weight)
        static member (<==) (l: Path<'v, unit>, r: 'v) : Path<'v, unit> =
            { Tail = r; Edges = l.Edges |> Map.add (r, l.Tail) () }
        /// N ==> a (no weight)
        static member (==>) (l: Path<'v, unit>, r: 'v) : Path<'v, unit> =
            { Tail = r; Edges = l.Edges |> Map.add (l.Tail, r) () }

    type Weight with
        /// 'n |=> 'a
        static member (|=>) (l: Weight<'v,'w>, r: 'v) : Path<'v,'w> =
            { Tail = r; Edges = l.Prev.Edges |> Map.add (l.Prev.Tail, r) l.Weight }
        /// 'n |= 'a
        static member (|=) (l: Weight<'v,'w>, r: 'v) : Path<'v,'w> =
            { Tail = r; Edges = l.Prev.Edges |> Map.add (r, l.Prev.Tail) l.Weight }

    /// Pseudo Graph Record Type Constructor
    let NewPath (inval: 'v) = { Tail = inval; Edges = Map.empty }

/// Path combinators
module Path = 
    open DSL
    open Core

    /// Combines a sequence of paths, duplicate edges are ignored
    let combine (paths: Path<'v,'w> seq) = 
        paths |> Seq.reduce (fun l r -> { r with Edges = Map.ofSeq [yield! r.Edges |> Map.toSeq; yield! l.Edges |> Map.toSeq] })

    /// Given a set of edges, find a set of all vertices
    let edgesToVertices (weightedEdges: (PathEdge<_>,  _) Map) = weightedEdges |> Seq.collect (fun kvp -> let (l,r) = kvp.Key in [l;r]) |> Set.ofSeq

    /// A graph that was constructed from Paths
    type PathGraph<'v, 'w when 'v: comparison and 'w: comparison> (path: Path<'v,'w>) =
        let edges = 
            [|
                for edge in path.Edges do 
                    let fromnode, tonode = edge.Key
                    yield { 
                            new IEdge<'v, 'w> with 
                                    member t.From = fromnode
                                    member t.To = tonode
                                    member t.Contents = edge.Value
                        }, edge.Value
            |]

        let vertices = edgesToVertices (path.Edges) |> Set.toArray

        interface IGraph<'v, 'w> with
            member t.Edges = edges |> Seq.map fst
            member t.NumEdges = edges.Length
            member t.Vertices = vertices |> Seq.ofArray
            member t.NumVertices = vertices.Length
//            member this.Properties = GraphProperties.None

        member t.Edges = edges |> Seq.map fst
        member t.Vertices = vertices
        member t.WeightedEdges = edges

    /// Converts a Path into a Graph
    let toGraph (path: Path<'v, 'w>) : PathGraph<'v, 'w> = 
        new PathGraph<'v, 'w>(path)