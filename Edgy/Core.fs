module Edgy.Core
#nowarn "62"

//
// Graph Interfaces
//

type IEdge<'e, 'ew when 'e: comparison> =
    interface
        abstract From: 'e
        abstract To: 'e
        abstract Contents: 'ew
    end

//type IVertex<'n, 'e, 'ec> 

type GraphProperties =
    | None = 0
    | Simple = 1
    | Cyclic = 2

type IGraph<'e, 'ec when 'e: comparison> =
    interface
        abstract Edges: IEdge<'e, 'ec> seq
        abstract Vertices: 'e seq
        abstract NumEdges: int
        abstract NumVertices: int
//        abstract Properties: GraphProperties
    end

//
// Standard Graph DSL Syntax
// 

[<AutoOpen>]
module Path =

    type PathEdge<'e when 'e: comparison> = 'e * 'e

    type Path<'v, 'w when 'v: comparison and 'w: comparison> = 
        {   
            /// The last element attached
            Tail: 'v
            /// (From --> To) Set
            Edges: (PathEdge<'v>, 'w) Map
        }
    and Weight<'v, 'w when 'v: comparison and 'w: comparison> = 
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


    /// Combines a sequence of paths, duplicate edges are ignored
    let combine (paths: Path<'v,'w> seq) = 
        paths |> Seq.reduce (fun l r -> { r with Edges = Map.ofSeq [yield! r.Edges |> Map.toSeq; yield! l.Edges |> Map.toSeq] })

    /// Given a set of edges, find a set of all nodes
    let edgesToVertices (weightedEdges: (PathEdge<_>,  _) Map) = weightedEdges |> Seq.collect (fun kvp -> let (l,r) = kvp.Key in [l;r]) |> Set.ofSeq



    type Graph<'v, 'w when 'v: comparison and 'w: comparison> (path: Path<'v,'w>) =
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
            member this.Edges = edges |> Seq.map fst
            member this.NumEdges = edges.Length
            member this.Vertices = vertices |> Seq.ofArray
            member this.NumVertices = vertices.Length
//            member this.Properties = GraphProperties.None

        member this.Edges = edges |> Seq.map fst
        member this.Vertices = vertices
        member t.WeightedEdges = edges

    let pathToGraph (path: Path<'v, 'w>) : Graph<'v, 'w> = 
        new Graph<'v, 'w>(path)

    /// Pseudo Graph Record Type Constructor
    let Path (inval: 'v) = { Tail = inval; Edges = Map.empty }
