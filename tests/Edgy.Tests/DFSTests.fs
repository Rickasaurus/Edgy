module DFSTests

open Edgy
open Edgy.Core
open Edgy.DSL

open NUnit.Framework

[<Test>]
let ``traverseWithNodeTimes should properly traverse and label a simple graph`` () =
    let graph = NewPath 0 ==> 1 ==> 2 ==> 3 ==> 4
                |> Path.toGraph
    let res = DFS.traverseWithNodeTimes 0 graph
    Assert.AreEqual(graph.Vertices.Length, res.EntryTimes.Count)
    Assert.True(res.EntryTimes |> Seq.exists (fun kv -> kv.Key <> kv.Value), sprintf "%A" res.EntryTimes)
    Assert.True(res.EntryTimes |> Seq.exists (fun kv -> kv.Key <> 4), sprintf "%A" res.EntryTimes)
    Assert.AreEqual(res.FinalTime, 5)
