module WeightedGraph

type Edge<'a> = 'a * 'a

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

/// Combines a sequence of paths, duplicate edges are ignored
let Combine paths = 
    paths |> Seq.reduce (fun l r -> { r with Edges = Map.ofSeq [yield! r.Edges |> Map.toSeq; yield! l.Edges |> Map.toSeq] })

// Example
let toalWeightedGraph =
    [
        WeightedPath "A" <=|1.0|= "B" =|0.5|=> "C" <=|0.2|= "D"
        WeightedPath "A" =|0.4|=> "B" =|0.2|=> "C"
    ] |> Combine

//val toalWeightedGraph : WeightedPath<string,float> =
//  {Tail = "D";
//   Edges =
//    map
//      [(("A", "E"), 0.4); (("B", "A"), 1.0); (("B", "C"), 0.5);
//       (("D", "C"), 0.2); (("E", "D"), 0.2)];}
