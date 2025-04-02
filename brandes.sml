structure Brandes: 
sig
  val get_max_betweenness: UndirectedGraph.t -> int * int
end =
struct
  structure UGraph = UndirectedGraph
  fun get_max_betweenness (g: UGraph.t) : int * int =
    let
        val n = UGraph.num_vertices g

        (* 创建二维数组：edge_centrality[u][v] 存储边 (u,v) 的介数值 *)
        val edge_centrality = Array.tabulate(n, fn _ => Array.array(n, 0.0))

        fun process_source s =
        let
            val sigma = Array.array(n, 0)
            val dist = Array.array(n, ~1)
            val pred = Array.tabulate(n, fn _ => ref ([]: int list))
            val delta = Array.array(n, 0.0)
            val stack = ref ([]: int list)
            val queue = ref [s]

            val _ = (Array.update(sigma, s, 1); Array.update(dist, s, 0))

            (* BFS 阶段 *)
            fun bfs [] = ()
            | bfs (v::vs) =
                let
                    val () = stack := v :: !stack
                    val nbrs = Seq.toList (UGraph.neighbors (g, v))

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
                            let
                                val old = !(Array.sub(pred, w))
                            in
                                Array.update(pred, w, ref (v :: old))
                            end
                            ) else ();
                            visit ws
                        end
                in
                    visit nbrs;
                    bfs vs
                end

            val _ = bfs [s]

            (* 回传依赖度阶段 *)
            fun backprop [] = ()
            | backprop (w::ws) =
                let
                    val sigma_w = Real.fromInt (Array.sub(sigma, w))
                    val delta_w = Array.sub(delta, w)
                    val ps = !(Array.sub(pred, w))

                    fun update_preds [] = ()
                    | update_preds (v::vs) =
                        let
                            val sigma_v = Real.fromInt (Array.sub(sigma, v))
                            val contrib = (sigma_v / sigma_w) * (1.0 + delta_w)
                            val _ = Array.update(delta, v, Array.sub(delta, v) + contrib)
                            val (u', v') = if v < w then (v, w) else (w, v)
                            val old_val = Array.sub(Array.sub(edge_centrality, u'), v')
                            val _ = Array.update(Array.sub(edge_centrality, u'), v', old_val + contrib)
                        in
                            update_preds vs
                        end
                in
                    update_preds ps;
                    backprop ws
                end
        in
            backprop (!stack)
        end

        val _ = List.app process_source (List.tabulate(n, fn i => i))

        fun g ((u1, v1, c1), (u2, v2, c2)) = if c1 > c2 then (u1, v1, c1) else (u2, v2, c2)
        val z = (0, 0, 0.0)
        fun f (u) = Parallel.reduce g z (u + 1, n) 
          (fn (v) => 
            let
              val _ = print ("[" ^ Int.toString u ^ "->" ^ Int.toString v ^ "]=" ^ Real.toString Array.sub(Array.sub(edge_centrality, u), v) ^ "\n")
            in
              (u, v, Array.sub(Array.sub(edge_centrality, u), v))
            end
            )
        
        val (best_u, best_v, best_c) = Parallel.reduce g z (0, n) f

        (* val _ = print ("[" ^ Int.toString u ^ "->" ^ Int.toString v ^ "]=" ^ Real.toString Array.sub(Array.sub(edge_centrality, u), v) ^ "\n") *)
    in
      (best_u, best_v)
    end   
end