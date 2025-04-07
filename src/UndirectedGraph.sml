structure UndirectedGraph :>
sig
  type t
  type graph = t
  type vertex = Graph.vertex

  (* The 4 functions below require O(1) work and span *)
  val degree: graph * vertex -> int
  val neighbors: graph * vertex -> vertex Seq.t
  val num_vertices: graph -> int
  val num_edges: graph -> int
 
  (* for louvain algorithm*)
  (* val aggregate_nodes: graph * int Seq.t -> graph *)

  (* for newman-girvan algorithm *)
  val remove_edge: graph * vertex * vertex -> graph

  val load_from_directed_graph: Graph.graph -> graph
end =
struct
  datatype graph =
    G of {
      n: int Seq.t, off: int Seq.t
    }
  type t = graph 
  type vertex = Graph.vertex
  
  fun load_from_directed_graph (g : Graph.graph) : graph = 
    let
      val nv = Graph.num_vertices g
      fun dedup cmp xs =
        let
            fun loop ([], acc) = List.rev acc
            | loop ([x], acc) = List.rev (x :: acc)
            | loop (x::y::rest, acc) =
                if cmp (x, y) = EQUAL then loop (y::rest, acc)
                else loop (y::rest, x::acc)
        in
            Seq.fromList (loop (Seq.toList (Mergesort.sort cmp xs), []))
        end

      val undirected_edges = Seq.tabulate (fn u =>
        dedup Int.compare (
          Merge.merge Int.compare (
            (* eliminate self-loop *)
            Seq.filter (fn v => v <> u) (Graph.out_neighbors (g, u)), 
            Seq.filter (fn v => v <> u) (Graph.in_neighbors (g, u))
          )
        )
      ) nv

      val degrees = Seq.map Seq.length undirected_edges
      val (offsets, _) = Seq.scan op+ 0 degrees
      val neighbors = Seq.flatten undirected_edges
      (* val weights = Seq.tabulate (fn (i) => 1.0) (Seq.length offsets) *)
      (* val node_members = Seq.tabulate (fn (i) => Seq.singleton i) (Seq.length neighbors) *)
    in
      G {
        n = neighbors,
        off = offsets,
      }
    end

  fun num_edges (G {n, ...}) =
    (Seq.length n) div 2

  fun num_vertices (G {off, ...}) =
    Seq.length off

  fun degree (g as G {n, off}, v:vertex) =
    let
      val lo = Seq.nth off v
      val hi =
        if v = num_vertices g - 1 then Seq.length n else Seq.nth off (v + 1)
    in
      hi - lo
    end

  fun neighbors (g as G {n, off}, v:vertex) =
    Seq.subseq n (Seq.nth off v, degree (g, v))

  (* W:O(n) for newman_girvan algorithm *)
  fun remove_edge (g as G {n, off}, u:vertex, v:vertex) : graph =
    let
      val u_lo = Seq.nth off u
      val u_hi = u_lo + (degree (g, u))
      val v_lo = Seq.nth off v
      val v_hi = v_lo + (degree (g, v))

      val n' = Parallel.filter (0, (Seq.length n)) 
        (fn (i) => Seq.nth n i)
        (fn (i) =>  
          if u_lo <= i andalso i < u_hi 
            andalso ((Seq.nth n i) = u orelse (Seq.nth n i) = v) then false
          else if v_lo <= i andalso i < v_hi 
            andalso ((Seq.nth n i) = u orelse (Seq.nth n i) = v) then false
          else true
        )
      val off' = Parallel.scan op+ 0 (0, (Seq.length off) - 1) 
        (fn (i) => if u <> i andalso v <> i then degree (g, i) else degree (g, i) - 1)
      (* val _ = Myprint.print_int_seq off' *)
    in
      G {n = n', off = off'}
    end
end