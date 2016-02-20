module DirectedPathTests

open Edgy
open Edgy.Core
open Edgy.DSL

open NUnit.Framework

[<Test>]
let ``Path.combine should properly combine paths`` () = 
    let res = [
                NewPath 2 <== 1
                NewPath 2 <== 1 ==> 3
                NewPath 2 ==> 4 <== 3 ==> 5
                NewPath 2 ==> 4 <== 3 <== 8
                NewPath 4 ==> 6
              ] |> Path.combine
    let edges = res.Edges |> Map.toSeq |> Seq.map fst |> Seq.map (fun e -> e) |> Seq.toArray
    Assert.AreEqual([|(1, 2); (1, 3); (2, 4); (3, 4); (3, 5); (4, 6); (8, 3)|], edges)