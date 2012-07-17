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

type IGraph<'e, 'ec when 'e: comparison> =
    interface
        abstract Edges: IEdge<'e, 'ec> seq
        abstract Vertices: 'e seq
    end

//
// Standard Graph DSL Syntax
// 

[<AutoOpen>]
module Path =

    type PathEdge<'e when 'e: comparison> = 'e * 'e

    type Path<'n, 'w when 'n: comparison and 'w: comparison> = 
        {   
            /// The last element attached
            Tail: 'n
            /// (From --> To) Set
            Edges: (PathEdge<'n>, 'w) Map
        }
    and Weight<'n, 'w when 'n: comparison and 'w: comparison> = 
        {
            /// Previous Path
            Prev: Path<'n,'w>
            /// Previous Weight
            Weight: 'w
        }

    type Path with
        /// 'a =| 'n
        static member (=|) (l: Path<'a,'b>, n: 'b) : Weight<'a,'b> = { Prev = l; Weight = n }
        /// 'a <=| 'n
        static member (<=|) (l: Path<'a,'b>, n: 'b) : Weight<'a,'b> = { Prev = l; Weight = n }
        /// N <== a (no weight)
        static member (<==) (l: Path<'a, unit>, r: 'a) : Path<'a, unit> =
            { Tail = r; Edges = l.Edges |> Map.add (r, l.Tail) () }
        /// N ==> a (no weight)
        static member (==>) (l: Path<'a, unit>, r: 'a) : Path<'a, unit> =
            { Tail = r; Edges = l.Edges |> Map.add (l.Tail, r) () }

    type Weight with
        /// 'n |=> 'a
        static member (|=>) (l: Weight<'a,'b>, r: 'a) : Path<'a,'b> =
            { Tail = r; Edges = l.Prev.Edges |> Map.add (l.Prev.Tail, r) l.Weight }
        /// 'n |= 'a
        static member (|=) (l: Weight<'a,'b>, r: 'a) : Path<'a,'b> =
            { Tail = r; Edges = l.Prev.Edges |> Map.add (r, l.Prev.Tail) l.Weight }


    /// Combines a sequence of paths, duplicate edges are ignored
    let combine (paths: Path<_,_> seq) = 
        paths |> Seq.reduce (fun l r -> { r with Edges = Map.ofSeq [yield! r.Edges |> Map.toSeq; yield! l.Edges |> Map.toSeq] })

    /// Given a set of edges, find a set of all nodes
    let edgesToVertices (weightedEdges: (PathEdge<_>,  _) Map) = weightedEdges |> Seq.collect (fun kvp -> let (l,r) = kvp.Key in [l;r]) |> Set.ofSeq

    type Graph<'n, 'w when 'n: comparison and 'w: comparison> (path: Path<'n,'w>) =
        let edges = 
            seq { 
                for edge in path.Edges do 
                    let fromnode, tonode = edge.Key
                    yield { 
                            new IEdge<'n, 'w> with 
                                    member t.From = fromnode
                                    member t.To = tonode
                                    member t.Contents = edge.Value
                        }, edge.Value
            }
        let vertices = edgesToVertices (path.Edges) |> Set.toSeq

        interface IGraph<'n, 'w> with
            member this.Edges = edges |> Seq.map fst
            member this.Vertices = vertices

        member t.WeightedEdges = edges

    let weightedPathToWeightedGraph (path: Path<'a, 'b>) : Graph<'a, 'b> = 
        new Graph<'a, 'b>(path)

    /// Pseudo Graph Record Type Constructor
    let Path (inval: 'a) = { Tail = inval; Edges = Map.empty }
