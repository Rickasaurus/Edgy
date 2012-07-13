module Edgy.Core
#nowarn "62"

//
// Graph Interfaces
//

type IEdge<'e when 'e: comparison> =
    interface
        abstract From: 'e
        abstract To: 'e
    end

type IGraph<'e when 'e: comparison> =
    interface
        abstract Edges: IEdge<'e> seq
        abstract Vertices: 'e seq
    end

//
// Standard Graph DSL Syntax
// 

[<AutoOpen>]
module Path =
    type PathEdge<'a when 'a: comparison> = 'a * 'a

    type Path<'a when 'a: comparison> =
        {   
            /// The last element attached
            Tail: 'a
            /// (From --> To) Set
            Edges: PathEdge<'a> Set
        }


    type Path with
            /// N <== a
            static member (<==) (l: Path<'a>, r: 'a) : Path<'a> =
                { Tail = r; Edges = l.Edges |> Set.add (r, l.Tail) }
            /// N ==> a
            static member (==>) (l: Path<'a>, r: 'a) : Path<'a> =
                { Tail = r; Edges = l.Edges |> Set.add (l.Tail, r) }
            interface IGraph<'a,'a>
                

    let combinePaths (paths: Path<_> seq) : Path<_> =  
        paths |> Seq.reduce (fun l r -> { r with Edges = Set.union l.Edges r.Edges })

    let edgesToVertices (edges: PathEdge<_> seq) : _ Set = 
        edges |> Seq.collect (fun (l,r) -> [l;r]) |> Set.ofSeq    

    let pathToGraph (path: Path<'a>) : IGraph<'a> = 
        let edges = 
            seq { 
                for edge in path.Edges do 
                    let fromnode, tonode = edge
                    yield { 
                            new IEdge<'a> with 
                                    member t.From = fromnode
                                    member t.To = tonode
                        }
            }

        let vertices = edgesToVertices (path.Edges) |> Set.toSeq
        {
            new IGraph<'a> with
                member this.Edges = edges
                member this.Vertices = vertices
        }
                    
    /// Pseudo Graph Record Type Constructor
    let Path (inval: 'a) = { Tail = inval; Edges = Set.empty }  

//
// Weighted Graph DSL Syntax
//

[<AutoOpen>]
module WeightedGraph =

    type WeightedPath<'n, 'w when 'n: comparison and 'w: comparison> = 
        {   
            /// The last element attached
            Tail: 'n
            /// (From --> To) Set
            Edges: (PathEdge<'n>, 'w) Map
        }
    and Weight<'n, 'w when 'n: comparison and 'w: comparison> = 
        {
            /// Previous Path
            Prev: WeightedPath<'n,'w>
            /// Previous Weight
            Weight: 'w
        }

    type WeightedPath with
        /// 'a =| 'n
        static member (=|) (l: WeightedPath<'a,'b>, n: 'b) : Weight<'a,'b> = { Prev = l; Weight = n }
        /// 'a <=| 'n
        static member (<=|) (l: WeightedPath<'a,'b>, n: 'b) : Weight<'a,'b> = { Prev = l; Weight = n }

    type Weight with
        /// 'n |=> 'a
        static member (|=>) (l: Weight<'a,'b>, r: 'a) : WeightedPath<'a,'b> =
            { Tail = r; Edges = l.Prev.Edges |> Map.add (l.Prev.Tail, r) l.Weight }
        /// 'n |= 'a
        static member (|=) (l: Weight<'a,'b>, r: 'a) : WeightedPath<'a,'b> =
            { Tail = r; Edges = l.Prev.Edges |> Map.add (r, l.Prev.Tail) l.Weight }

    /// Combines a sequence of paths, duplicate edges are ignored
    let combineWeightedPaths (paths: WeightedPath<_,_> seq) = 
        paths |> Seq.reduce (fun l r -> { r with Edges = Map.ofSeq [yield! r.Edges |> Map.toSeq; yield! l.Edges |> Map.toSeq] })

    /// Given a set of edges, find a set of all nodes
    let weightedEdgesToVertices (weightedEdges: (PathEdge<_>,  _) Map) = weightedEdges |> Seq.collect (fun kvp -> let (l,r) = kvp.Key in [l;r]) |> Set.ofSeq


    type WeightedDirectedGraph<'n, 'w when 'n: comparison and 'w: comparison> (path: WeightedPath<'n,'w>) =
        let edges = 
            seq { 
                for edge in path.Edges do 
                    let fromnode, tonode = edge.Key
                    yield { 
                            new IEdge<'n> with 
                                    member t.From = fromnode
                                    member t.To = tonode
                        }, edge.Value
            }
        let vertices = weightedEdgesToVertices (path.Edges) |> Set.toSeq

        interface IGraph<'n> with
            member this.Edges = edges |> Seq.map fst
            member this.Vertices = vertices

        member t.WeightedEdges = edges

    let weightedPathToWeightedGraph (path: WeightedPath<'a, 'b>) : WeightedDirectedGraph<'a, 'b> = 
        new WeightedDirectedGraph<'a, 'b>(path)

    /// Pseudo Graph Record Type Constructor
    let WeightedPath (inval: 'a) = { Tail = inval; Edges = Map.empty }
