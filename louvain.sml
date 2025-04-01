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
          
          fun g ((comm1, delta1, neighbor1), (comm2, delta2, neighbor2)) = 
            if delta1 > delta2 then (comm1, delta1, neighbor1) else (comm2, delta2, neighbor2)
          val z = (0, 0.0, 0)
          fun f (neighbor_i) = 
            let
              val neighbor = Seq.nth neighbors neighbor_i
              val comm_new = Array.sub (communities, neighbor)
              val k_i_in = Real.fromInt (calc_k_i_in comm_new)
              val sigma_tot = if comm_new = comm_old 
                then Real.fromInt(Array.sub (comm_weights, comm_new)) - k_i
                else Real.fromInt(Array.sub (comm_weights, comm_new))
              val delta_Q = k_i_in - k_i * sigma_tot / m
              (* val _ = print (
                "nb_i:" ^ Int.toString neighbor_i ^ 
                " comm_new:" ^ Int.toString comm_new ^
                " k_i_in:" ^ Real.toString k_i_in ^
                " sigma_tot:" ^ Real.toString sigma_tot ^
                " k_i:" ^ Real.toString k_i ^
                " m:" ^ Real.toString m ^
                " delta_Q:" ^ Real.toString delta_Q ^ "\n"
              ) *)
            in
              (comm_new, delta_Q, neighbor)
            end
          (* val _ = print ("V: "^ Int.toString v ^ "\n") *)
        in
          (* W:O(d) S:O(log(d))  return {argmax_comm(delta_Q), max(delta_Q)}*)
          Parallel.reduce g z (0, degree) f
        end
    
      fun update_comm_in_parallel () = 
        let
          val expected_comm = Parallel.tabulate (0, (UGraph.num_vertices g)) (fn (i) => calculate_max_deltaQ i)
          val changed_bit = Array.tabulate ((UGraph.num_vertices g), (fn (i) => false)) 
          fun update_comm (v, comm_old, comm_new, target_neighbor) = 
            let
              val comm_weight_old = Array.sub (comm_weights, comm_old)
              val comm_weight_new = Array.sub (comm_weights, comm_new)
              val degree = UGraph.degree (g, v)
              val _ = Array.update (communities, v, comm_new)
              val _ = Array.update (comm_weights, comm_old, comm_weight_old - degree)
              val _ = Array.update (comm_weights, comm_new, comm_weight_new + degree)
              val _ = Array.update (changed_bit, target_neighbor, true)
            in
              true (* updated in this round *)
            end
          fun try_update_comm (v, stable:bool) = 
            if v >= (UGraph.num_vertices g) then stable
            else
              let 
                val (comm_new, delta_Q, neighbor) = Seq.nth expected_comm v
                val comm_old = Array.sub (communities, v)
                val updated = 
                  if Array.sub (changed_bit, neighbor) then true
                  else if delta_Q > 0.0 andalso comm_old <> comm_new then update_comm (v, comm_old, comm_new, neighbor)
                  else false
              in
                try_update_comm (v + 1, if updated then false else stable)
              end
            
          val stable = try_update_comm (0, true)
        in
          stable
        end
      
      fun update_comm_until_stable (round) = 
        if update_comm_in_parallel 
          then round 
          else update_comm_until_stable (round + 1)
    
      val _ = update_comm_until_stable 0
    in
      communities
    end
end