structure Louvain: 
sig
  val louvain: UndirectedGraph.t -> int Seq.t
end =
struct
  structure UGraph = UndirectedGraph
  fun louvain (g: UGraph.t) : int Seq.t = 
    let
      val m = Real.fromInt (UGraph.num_edges g)
      (* W:O(n) S:O(n)  assign a different community to each node *)
      val communities = Seq.tabulate (fn i => i) (UGraph.num_vertices g)
      (* W:O(n) S:O(n)  the initial weight of each community is the degree of each noed *)
      val comm_weights = Seq.tabulate (fn i => (UGraph.degree (g, i))) (UGraph.num_vertices g)
      (* W:O(n) S:O(n)  a buffer for future use *)
      val neighbor_comm_delta_weights = Seq.tabulate (fn _ => 0) (UGraph.num_vertices g)
      (* W:O(d) S:O(log(d))  *)
      fun calculate_max_deltaQ (v) = 
        let
          val neighbors = UGraph.neighbors (g, v)
          val degree = UGraph.degree (g, v)
          val comm_old = Seq.nth communities v
          val k_i = Real.fromInt degree
          (* W:O(d) S:O(1)  reset neighbor_comm_delta_weights *)
          val _ = Parallel.parfor (0, degree) (fn (i) => 
              Seq.update neighbor_comm_delta_weights (Seq.nth communities (Seq.nth neighbors i)) 0
          )
          (* W:O(d) S:O(1)  calculate neighbor_comm_delta_weights *) (* !! risk *)
          val _ = Parallel.parfor (0, degree) (fn (i) => 
            let
              val comm_neighbor = Seq.nth communities (Seq.nth neighbors i)
              val current_weight = Seq.nth neighbor_comm_delta_weights comm_neighbor
            in
              Seq.update neighbor_comm_delta_weights comm_neighbor (current_weight + 1)
            end
          )
          fun g ((comm1, delta1), (comm2, delta2)) = 
            if delta1 > delta2 then (comm1, delta1) else (comm2, delta2)
          val z = (0, 0)
          fun f (i) = 
            let
              val comm_new = Seq.nth communities (Seq.nth neighbors i)
              val k_i_in = Real.fromInt (Seq.nth neighbor_comm_delta_weights comm_new)
              val sigma_tot = if comm_new = comm_old 
                then Real.fromInt(Seq.nth comm_weights comm_new)
                else Real.fromInt(Seq.nth comm_weights comm_new) - k_i
              val delta_Q = k_i_in - k_i * sigma_tot / 2.0 / m
            in
              (comm_new, delta_Q)
            end
        in
          (* W:O(d) S:O(log(d))  return {argmax_comm(delta_Q), max(delta_Q)}*)
          Parallel.reduce g z (0, degree) f
        end
      
      (* fun update_comm_until_stable () = 1 *)
      val (comm_new, delta_Q) = calculate_max_deltaQ(0)
    in
      Seq.singleton (comm_new)
    end
end