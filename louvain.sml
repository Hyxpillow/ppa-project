structure Louvain: 
sig
  val louvain: UndirectedGraph.t -> int array
end =
struct
  structure UGraph = UndirectedGraph
  structure Myprint = Myprint
  fun louvain (g: UGraph.t) : int array = 
    let
      val m = Real.fromInt (UGraph.num_edges g)
      (* W:O(n) S:O(n)  assign a different community to each node *)
      val communities = Array.tabulate ((UGraph.num_vertices g), (fn i => i))
      (* W:O(n) S:O(n)  the initial weight of each community is the degree of each noed *)
      val comm_weights = Array.tabulate ((UGraph.num_vertices g), (fn i => (UGraph.degree (g, i))))
      (* W:O(d) S:O(log(d)) *)
      fun calculate_max_deltaQ (v) = 
        let
          val neighbors = UGraph.neighbors (g, v)
          val degree = UGraph.degree (g, v)
          val comm_old = Array.sub (communities,v)
          val k_i = Real.fromInt degree
          (* W:O(d) S:O(log(d))  neighbor_comm_delta_weights *)
          fun calc_k_i_in (comm_new) = 
            Parallel.reduce op+ 0 (0, degree) (fn (neighbor_i) => 
              if comm_new = (Array.sub (communities, (Seq.nth neighbors neighbor_i))) then 1
              else 0
            )
         (* find the comm with greater deltaQ *)
          fun g ((comm1, delta1), (comm2, delta2)) = 
            if (delta1 > delta2) then 
              (comm1, delta1)
            else
              (comm2, delta2)
          val z = (0, 0.0)
          fun f (neighbor_i) = 
            let
              val comm_new = Array.sub (communities, (Seq.nth neighbors neighbor_i))
              val k_i_in = Real.fromInt (calc_k_i_in comm_new)
              val sigma_tot = if comm_new = comm_old 
                then Real.fromInt(Array.sub (comm_weights, comm_new)) - k_i
                else Real.fromInt(Array.sub (comm_weights, comm_new))
              val delta_Q = k_i_in - k_i * sigma_tot / m
              val _ = print (
                "nb_i:" ^ Int.toString neighbor_i ^ 
                " comm_new:" ^ Int.toString comm_new ^
                " k_i_in:" ^ Real.toString k_i_in ^
                " sigma_tot:" ^ Real.toString sigma_tot ^
                " k_i:" ^ Real.toString k_i ^
                " m:" ^ Real.toString m ^
                " delta_Q:" ^ Real.toString delta_Q ^ "\n"
              )
            in
              (comm_new, delta_Q)
            end
          val _ = print ("V: "^ Int.toString v ^ "\n")
        in
          (* W:O(d) S:O(log(d))  return {argmax_comm(delta_Q), max(delta_Q)}*)
          Parallel.reduce g z (0, degree) f
        end

      (* loop *)
      fun update_comm_until_stable (v, stable:bool) = 
        if v = (UGraph.num_vertices g) andalso stable then
          communities (* final result *)
        else if v = (UGraph.num_vertices g) andalso not stable then
          update_comm_until_stable (0, true)
        else
          let
            val (comm_new, deltaQ) = calculate_max_deltaQ(v)
            val comm_old = Array.sub (communities, v)
          in
            if deltaQ > 0.0 andalso comm_old <> comm_new then
              let
                val comm_weight_old = Array.sub (comm_weights, comm_old)
                val comm_weight_new = Array.sub (comm_weights, comm_new)
                val degree = UGraph.degree (g, v)
                val _ = Array.update (communities, v, comm_new)
                val _ = Array.update (comm_weights, comm_old, comm_weight_old - degree)
                val _ = Array.update (comm_weights, comm_new, comm_weight_new + degree)
                val _ = print ("Move from " ^ Int.toString comm_old ^ " to "^ Int.toString comm_new ^ "\n")
              in
                update_comm_until_stable (v + 1, false)
              end
            else
              update_comm_until_stable (v + 1, stable)
          end

    in
      update_comm_until_stable (0, true)
    end
end