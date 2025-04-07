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
  val aggregate_nodes: graph * int Seq.t -> graph

  (* for newman-girvan algorithm *)
  val remove_edge: graph * vertex * vertex -> graph

  val load_from_directed_graph: Graph.graph -> graph
end =
struct
  datatype graph =
    G of {
      n: int Seq.t, off: int Seq.t, 
      (* For Louvain: *) w: real Seq.t, super_node: int Seq.t Seq.t 
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
      val weights = Seq.tabulate (fn (i) => 1.0) (Seq.length offsets)
      val node_members = Seq.tabulate (fn (i) => Seq.singleton i) (Seq.length neighbors)
    in
      G {
        n = neighbors,
        off = offsets,
        w = weights,
        super_node = node_members
      }
    end

  fun num_edges (G {n, ...}) =
    (Seq.length n) div 2

  fun num_vertices (G {off, ...}) =
    Seq.length off

  fun degree (g as G {n, off, ...}, v:vertex) =
    let
      val lo = Seq.nth off v
      val hi =
        if v = num_vertices g - 1 then Seq.length n else Seq.nth off (v + 1)
    in
      hi - lo
    end

  fun neighbors (g as G {n, off, ...}, v:vertex) =
    Seq.subseq n (Seq.nth off v, degree (g, v))
  
  fun aggregate_nodes (g as G)

  (* W:O(n) for newman_girvan algorithm *)
  fun remove_edge (g as G {n, off, w, super_node}, u:vertex, v:vertex) : graph =
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
      G {n = n', off = off', w = w, super_node = sn}
    end
    (* 假设已经打开 Seq 模块或已绑定必要函数，如 Seq.map, Seq.filter 等 *)

  fun aggregate_nodes (G as G{n, off, w, super_node}, community: int Seq.t) =
    let
        (* 1. 找出所有不同社区并建立映射：old_comm_id -> new_index *)
      val comm_list = Seq.fromList (ListMergeSort.unique (Seq.toList community))
        (* 上面假设存在 ListMergeSort.unique 工具函数，将社区列表排序去重，
        如果没有该函数，可先排序再手动去重。这里 comm_list 是升序的不重复社区ID序列。 *)
      val new_comm_count = Seq.length comm_list
        (* 将排序后的社区ID映射为0..new_comm_count-1 *)
      val comm_to_new_list = Seq.toList comm_list
      fun newIndex cid = 
        let 
          fun findIndex ([], _) = ~1  (* 不会发生，此处为了模式匹配完整 *)
            | findIndex (x::xs, i) = if x = cid then i else findIndex(xs, i+1)
          in findIndex(comm_to_new_list, 0) end

        (* 2. 汇总社区间边：遍历原图邻接表，汇总不同社区节点间的边权 *)
        (* 收集社区间边 (u_new, v_new, weight) 的列表 *)
          val edge_list =
        let 
          val N = Seq.length community  (* 原图节点数量 *)
            (* 遍历每个节点及其邻居 *)
          val edges =
          Seq.flatten (Seq.tabulate(N, fn u =>
            let 
              val cu = newIndex (Seq.nth(community, u))     (* 节点u的社区的新索引 *)
              val start = Seq.nth(off, u)                   (* 邻接开始索引 *)
              val stop = if u < N-1 then Seq.nth(off, u+1)   (* 邻接结束索引 *)
                            else Seq.length n
                (* 为节点u生成其有效邻接边的序列 *)
              val nbr_edges = Seq.tabulate(stop - start, fn k =>
                let 
                  val j = Seq.nth(n, start + k)       (* 第k个邻居节点 *)
                  val wgt = Seq.nth(w, start + k)     (* 相应的边权重 *)
                  val cj = newIndex (Seq.nth(community, j))  (* 邻居节点j的社区新索引 *)
                in
                  if j > u andalso cu <> cj 
                  then SOME ((if cu < cj then cu else cj),    (* 边两端社区的新索引（小,大） *)
                    (if cu < cj then cj else cu),
                                wgt)
                  else NONE
                    end)
                in
                  Seq.filterMap (fn x => x) nbr_edges  (* 过滤出有效的边（三元组） *)
                end))
            (* 将 Seq 转换为列表便于后续处理 *)
            val edge_triples = Seq.toList edges
        in
            (* 对边按照社区对进行排序，以便合并权重 *)
            let 
            fun cmp ((u1,v1,_), (u2,v2,_)) = 
                case Int.compare(u1, u2) of
                EQUAL => Int.compare(v1, v2)
                | order => order
            val sorted_edges = List.sort cmp edge_triples
            (* 合并同一对社区间的边权 *)
            fun mergeEdges [] = []
              | mergeEdges ((u,v,w)::rest) =
                let 
                  val (same, others) = List.partition (fn (x,y,_) => x = u andalso y = v) rest
                  val total_w = List.foldl (fn ((_,_,w'), acc) => w' + acc) w same
                in 
                  (u, v, total_w) :: mergeEdges others
                end
            in
              mergeEdges sorted_edges
            end
        end

        (* 3. 构建新图的压缩表示：根据 edge_list 生成 n', off', w', super_node' *)
        val new_m = length edge_list                       (* 新图中唯一的社区间边数（每条计一次） *)
        (* 将每条无向边拆分成两个方向，以构建邻接表 *)
        val adj_edges = List.concat (List.map (fn (u,v,w) =>
                        [(u, v, w), (v, u, w)]) edge_list)
        (* 按照源节点排序邻接条目 *)
        val sorted_adj = List.sort (fn ((u1,_,_), (u2,_,_)) => Int.compare(u1,u2)) adj_edges
        (* 生成新图的偏移量 off' 和邻接序列 n', w' *)
        val new_off_list =
          let 
            fun count_accum([], _, acc) = List.rev acc
            | count_accum(((u,_,_)::rest) as edges, curr, acc) =
                if u <> curr then 
                    (* 如果没有邻接（孤立节点），设置off与前一相同 *)
                  count_accum(edges, curr+1, (hd acc)::acc)
                else 
                    (* 计算节点curr的度数d，并将curr的off设置为prev_off + d *)
                  let 
                    val (edges_curr, edges_rest) = List.span (fn (x,_,_) => x = curr) edges
                    val prev_off = if acc = [] then 0 else hd acc
                  in 
                    count_accum(edges_rest, curr+1, (prev_off + length edges_curr)::acc)
                  end
          in
            (* 注意：假设超级节点从0开始连续编号 *)
            count_accum(sorted_adj, 0, [0])
          end
        val new_n_list = List.map (fn (_, v, _) => v) sorted_adj
        val new_w_list = List.map (fn (_, _, w) => w) sorted_adj
        (* 构造 super_node'：每个新超级节点对应的原始节点集合 *)
        val new_super_nodes =
        Seq.tabulate(new_comm_count, fn idx =>
            let 
            val cid = Seq.nth(comm_list, idx)   (* 原社区编号 *)
            (* 收集所有原图中社区编号为cid的节点 *)
            val nodes = Seq.filter (fn u => Seq.nth(community, u) = cid)
                                    (Seq.tabulate(Seq.length community, I))
          in 
            nodes
          end)

      val _ = Myprint.print_int_seq (Seq.fromList new_n_list)
      val _ = Myprint.print_int_seq (Seq.fromList new_off_list)
      val _ = Myprint.print_int_seq (Seq.fromList new_w_list)
      val _ = Myprint.print_int_seq (new_super_nodes)
    in
      G {
        n   = Seq.fromList new_n_list,
        off = Seq.fromList new_off_list,
        w   = Seq.fromList new_w_list,
        super_node = new_super_nodes
        }
    end
end