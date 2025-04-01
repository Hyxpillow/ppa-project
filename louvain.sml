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
      val comm = Array.tabulate ((UGraph.num_vertices g), (fn i => i))
      (* W:O(n) S:O(n)  to avoid comm switching back and forth when parallel algo running *)
      val comm_history = Array.tabulate ((UGraph.num_vertices g), (fn i => i))
      (* W:O(n) S:O(n)  the initial weight of each community is the degree of each noed *)
      val comm_weights = Array.tabulate ((UGraph.num_vertices g), (fn i => (UGraph.degree (g, i))))
      (* W:O(d) S:O(log(d)) *)
      fun calculate_max_deltaQ (v) = 
        let
          val neighbors = UGraph.neighbors (g, v)
          val degree = UGraph.degree (g, v)
          val comm_old = Array.sub (comm,v)
          val k_i = Real.fromInt degree
          (* W:O(d) S:O(log(d))  neighbor_comm_delta_weights *)
          fun calc_k_i_in (comm_new) = 
            Parallel.reduce op+ 0 (0, degree) (fn (neighbor_i) => 
              if comm_new = (Array.sub (comm, (Seq.nth neighbors neighbor_i))) then 1
              else 0
            )
          
          fun g ((comm1, delta1), (comm2, delta2)) = 
            if delta1 > delta2 then (comm1, delta1) else (comm2, delta2)
          val z = (0, 0.0)
          fun f (neighbor_i) = 
            let
              val neighbor = Seq.nth neighbors neighbor_i
              val comm_new = Array.sub (comm, neighbor)
              val k_i_in = Real.fromInt (calc_k_i_in comm_new)
              val sigma_tot = if comm_new = comm_old 
                then Real.fromInt(Array.sub (comm_weights, comm_new)) - k_i
                else Real.fromInt(Array.sub (comm_weights, comm_new))
              val delta_Q = k_i_in - k_i * sigma_tot / m
            in
              (comm_new, delta_Q)
            end
        in
          (* W:O(d) S:O(log(d))  return {argmax_comm(delta_Q), max(delta_Q)}*)
          Parallel.reduce g z (0, degree) f
        end
    
      fun update_comm_in_parallel () = 
        let
          val stable = ref true
          fun update_comm (v, comm_old, comm_new) = 
            let
              (* Concurrent Safe *)
              val degree = UGraph.degree (g, v)
              val _ = Array.update (comm, v, comm_new)
              val _ = Array.update (comm_history, v, comm_old)
              val _ = print ((Int.toString v) ^ " Move from " ^ (Int.toString comm_old) ^ " to " ^ (Int.toString comm_new) ^ "\n")
              
              fun atomic_update_comm (comm, add_or_sub, delta_degree) = 
                let
                  val old_weight = Array.sub (comm_weights, comm)
                  val new_weight = add_or_sub (old_weight, delta_degree)
                  val result = Concurrency.casArray (comm_weights, comm) (old_weight, new_weight)
                in
                  if result = old_weight
                    then (stable := false) (* CAS succeeded *)
                    else atomic_update_comm(comm, add_or_sub, delta_degree) (* CAS failed, retry with the new current value *)
                end
              (* Concurrent Safe *)
              val _ = atomic_update_comm (comm_old, op-, degree)
              val _ = atomic_update_comm (comm_new, op+, degree)
            in
              ()
            end
          val expected_comm_seq = Parallel.tabulate (0, (UGraph.num_vertices g)) (fn (v) => calculate_max_deltaQ v)
          val _ = Parallel.parfor (0, (UGraph.num_vertices g)) (
            fn (v) => 
              let
                val (comm_new, delta_Q) = Seq.nth expected_comm_seq v
                val comm_old = Array.sub (comm, v)
                val comm_old_old = Array.sub (comm_history, v)
              in
                if delta_Q > 0.0 andalso comm_old <> comm_new andalso comm_old_old <> comm_new
                  then update_comm (v, comm_old, comm_new)
                  else ()
              end
            )
        in
          !stable
        end
      
      fun update_comm_until_stable (round) = 
        let
          val _ = print ("ROUND" ^ (Int.toString round) ^ "\n" )
        in
        if update_comm_in_parallel ()
          then round 
          else update_comm_until_stable (round + 1)
        end
    
      val round = update_comm_until_stable 0
    in
      comm
    end
end