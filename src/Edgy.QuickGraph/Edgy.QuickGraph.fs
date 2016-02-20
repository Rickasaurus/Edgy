namespace Edgy

open Edgy.Core
open QuickGraph

// QuickGraph Edge Types:
//  classes
//      Edge<TVertex>, an vanilla implementation,
//      EquatableEdge<TVertex>, implements IEquatable<EquatableEdge<TVertex>>,
//      TaggedEdge<TVertex,TTag>, holds a tag,
//      TaggedEquatableEdge<TVertex,TTag>, equatable and holds a tag,
//      UndirectedEdge<TVertex>, an vanilla implementation,
//      TaggedUndirectedEdge<TVertex,TTag>, holds a tag,
//  structs
//      SEdge<TVertex>, an immutable edge,
//      SEquatableEdge<TVertex>, a struct that implements IEquatable<SEquatableEdge<TVertex>>,
//      STaggedEdge<TVertex, TTag>, holds a tag
//      STaggedEquatableEdge<TVertex,TTag>, equatable and holds a tag,
//      SUndirectedEdge<TVertex>, an immutable edge,
//      STaggedUndirectedEdge<TVertex, TTag>, holds a tag

// QuickGraph Graph Types:
//  Directed graphs
//      AdjacencyGraph provides access to vertices, edges and out edges of sparse directed graphs.
//      BidirectionalGraph extends AdjacencyGraph by providing access to in edges but it requires the double of memory space.
//      BidirectionalMatrixGraph represented by an array. Efficient for dense directed graphs with known number of vertices.
//      ArrayAdjacencyGraph and ArrayBidirectionalGraph are readonly equivalent of AdjacencyGraph and BidirectionalGraph. They also use slightly less memory.
//      CompressedSparseRowGraph is an adjacency graph using a compact representation called compressed sparse row. Only the vertex can be of custom type.
//      DelegateImplicitGraph wraps the IImplicitGraph interface on top of a delegate that a vertex to a set of out vertices. This representation is efficient for large graph that cannot be stored in memory at once
//      DelegateIncidenceGraph wraps the IIncidenceGraph interface, and extends DelegateImplicitGraph
//      DelegateVertexAndEdgeListGraph wraps the IVertexAndEdgeListGraph, extends the DelegateIncidenceGraph, and also uses an enumeration for the vertices
//  Undirected graphs
//      UndirectedGraph provides a representation for sparse undirected graphs.
//  Adapters
//      BidirectionalAdapterGraph wraps an adjacency graph into a bidirectional graph (access to in-edges)
//      UndirectedBidirectionalGraph wraps a bidirectional graph into a undirected graph.

module QuickGraph =

    /// Converts an non-weighted Edgy Graph to a QuickGraph graph.
    let toAdjacencyGraph (graph: Edgy.Core.IGraph<'v,unit>) = 
        let outGraph = new AdjacencyGraph<_,QuickGraph.Edge<_>>()
        outGraph.AddVerticesAndEdgeRange( seq { for e in graph.Edges do yield new QuickGraph.Edge<_>(e.From, e.To) }) |> ignore
        outGraph

    /// Converts a weighted Edgy Graph to a QuickGraph graph.
    let toWeightedAdjacencyGraph (graph: Edgy.Core.IGraph<'v,'e>) =
        let outGraph = new AdjacencyGraph<_,QuickGraph.TaggedEdge<_,_>>()
        outGraph.AddVerticesAndEdgeRange( seq { for e in graph.Edges do yield new QuickGraph.TaggedEdge<_,_>(e.From, e.To, e.Contents) }) |> ignore
        outGraph
