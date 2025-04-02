structure Brandes: 
sig
  val get_max_betweenness: UndirectedGraph.t -> int * int
end =
struct
  structure UGraph = UndirectedGraph
  fun get_max_betweenness (g: UGraph.t) : int * int =
  let
    val n = UGraph.num_vertices g

    (* 创建二维表来累计边介数 *)
    val edge_centrality = Array.tabulate(n, fn _ => Array.array(n, 0.0))

    (* 遍历每个源点 s *)
    fun process_source s =
      let
        val sigma = Array.array(n, 0)
        val dist = Array.array(n, ~1)
        val pred = Array.tabulate(n, fn _ => ref [])
        val delta = Array.array(n, 0.0)
        val stack = ref []
        val queue = ref [s]

        val _ = (Array.update(sigma, s, 1); Array.update(dist, s, 0))

        (* BFS *)
        fun bfs [] = ()
          | bfs (v::vs) =
              let
                val () = stack := v :: !stack
                val nbrs = UGraph.neighbors (g, v) |> Seq.toList
                fun visit [] = ()
                  | visit (w::ws) =
                      let
                        val d_v = Array.sub(dist, v)
                        val d_w = Array.sub(dist, w)
                      in
                        if d_w = ~1 then (
                          Array.update(dist, w, d_v + 1);
                          queue := !queue @ [w]
                        ) else ();
                        if Array.sub(dist, w) = d_v + 1 then (
                          Array.update(sigma, w, Array.sub(sigma, w) + Array.sub(sigma, v));
                          pred := Array.update(pred, w, ref (v :: !(Array.sub(pred, w))))
                        ) else ();
                        visit ws
                      end
              in
                visit nbrs;
                bfs vs
              end

        val _ = bfs [s]

        (* 回传依赖度 *)
        fun backprop [] = ()
          | backprop (w::ws) =
              let
                val s_w = Real.fromInt (Array.sub(sigma, w))
                val delta_w = Array.sub(delta, w)
                val ps = !(Array.sub(pred, w))
                fun update_pred [] = ()
                  | update_pred (v::vs) =
                      let
                        val s_v = Real.fromInt (Array.sub(sigma, v))
                        val contrib = (s_v / s_w) * (1.0 + delta_w)
                        val _ = Array.update(delta, v, Array.sub(delta, v) + contrib)
                        val (u, v') = if v < w then (v, w) else (w, v)
                        val old = Array.sub(Array.sub(edge_centrality, u), v')
                        val _ = Array.update(Array.sub(edge_centrality, u), v', old + contrib)
                      in
                        update_pred vs
                      end
              in
                update_pred ps;
                backprop ws
              end
      in
        backprop (!stack)
      end

    val _ = List.app process_source (List.tabulate(n, fn i => i))

    (* 找出最大边 *)
    fun find_max u v (best_u, best_v, best_val) =
      let
        val val_uv = Array.sub(Array.sub(edge_centrality, u), v)
      in
        if val_uv > best_val then (u, v, val_uv) else (best_u, best_v, best_val)
      end

    fun search u v (best_u, best_v, best_val) =
      if u >= n then (best_u, best_v)
      else if v >= n then search (u + 1) (u + 2) (best_u, best_v, best_val)
      else search u (v + 1) (find_max u v (best_u, best_v, best_val))

  in
    search 0 1 (0, 1, ~1.0)
  end
 
    
end