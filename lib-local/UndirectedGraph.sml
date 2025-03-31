structure UndirectedGraph :>
sig
  type t
  type graph = t
  type vertex = int

  (* The 4 functions below require O(1) work and span *)
  val degree: graph * vertex -> int
  val neighbors: graph * vertex -> vertex Seq.t
  val num_vertices: graph -> int
  val num_edges: graph -> int

  val load_from_directed_graph: Graph.graph -> graph
end =
struct
  datatype graph =
    G of {n: int Seq.t, off: int Seq.t}
  
  fun load_from_directed_graph (g : Graph.graph) : graph = 
    let
      val nv = Graph.num_vertices g

      val undirected_edges = Seq.tabulate (fn u =>
        Merge.dedup Int.compare (
          Merge.merge Int.compare
            (Graph.out_neighbors (g, u), Graph.in_neighbors (g, u))
        )
      ) nv

      val degrees = Seq.map Seq.length undirected_edges
      val (offset, _) = Seq.scan op+ 0 degrees
      val neighbors = Seq.flatten undirected_edges
    in
      {
        n = neighbors,
        offset = offset
      }
    end

  fun num_edges (G {n, ...}) =
    (Seq.length n) / 2

  fun num_vertices (G {off, ...}) =
    Seq.length off

  fun degree (g as G {off, ...}, v) =
    let
      val lo = Seq.nth off v
      val hi =
        if v = num_vertices g - 1 then num_edges g else Seq.nth off (v + 1)
    in
      hi - lo
    end

  fun neighbors (g as G {n, off} v) =
    Seq.subseq n (Seq.nth off v, degree (g, v))
end