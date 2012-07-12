module Edgy.Core
#nowarn "62"

//
// Standard Graph Definition
// 

type Edge<'a> = 'a * 'a

type Path<'a when 'a: comparison> =
    {   
        /// The last element attached
        Tail: 'a
        /// (From --> To) Set
        Edges: Edge<'a> Set
    }

type Path
    with
        /// N <== a
        static member op_LessEqualsEquals (l: Path<'a>, r: 'a) : Path<'a> =
            { Tail = r; Edges = l.Edges |> Set.add (r, l.Tail) }
        /// N ==> a
        static member op_EqualsEqualsGreater (l: Path<'a>, r: 'a) : Path<'a> =
            { Tail = r; Edges = l.Edges |> Set.add (l.Tail, r) }


/// Pseudo Graph Record Type Constructor
let Path (inval: 'a) = { Tail = inval; Edges = Set.empty }

//
// Weighted Graph Definition
//

type WeightedPath<'a, 'b when 'a: comparison and 'b: comparison> = 
    {   
        /// The last element attached
        Tail: 'a
        /// (From --> To) Set
        Edges: (Edge<'a>, 'b) Map
    }
and Weight<'a, 'b when 'a: comparison and 'b: comparison> = 
    {
        /// Previous Path
        Prev: WeightedPath<'a,'b>
        /// Previous Weight
        Weight: 'b
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

/// Pseudo Graph Record Type Constructor
let WeightedPath (inval: 'a) = { Tail = inval; Edges = Map.empty }
