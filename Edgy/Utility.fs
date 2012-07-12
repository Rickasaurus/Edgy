module Edgy.Utility

/// Weighted quick-union with path compression
/// Overall: O((M+N)lg*N) -- M union-find ops on a set of N objects
/// Implementation from: http://www.cs.princeton.edu/~rs/AlgsDS07/01UnionFind.pdf
type QuickUWPC private (id: int[], sz: int[]) =

    new (N: int) = 
        // Parent index, id[i] is parent of i
        let id = Array.init N (fun i -> i)
        // Number of elements rooted at i
        let sz = Array.create N 1
        QuickUWPC(id, sz) 

    /// Returns the root node which represents the set of the given node
    member t.Root(i: int) =
        let mutable q = i
        while (q <> id.[q]) do 
            id.[q] <- id.[id.[q]]
            q <- id.[q] 
        q

    /// Determine if p and q are in the same set (or if p is in set q)
    member t.Find(p, q) =
        t.Root(p) = t.Root(q)

    /// Union: Combine or merge two sets into a single set.
    member t.Union(p, q) =
        let i = t.Root(p)
        let j = t.Root(q)
        if sz.[i] < sz.[j] then id.[i] <- j; sz.[j] <- sz.[j] + sz.[i]
        else id.[j] <- i; sz.[i] <- sz.[i] + sz.[j]    

    /// Creates another QuickUWPC with the same internal structure
    member t.Clone() = new QuickUWPC(Array.copy id, Array.copy sz)