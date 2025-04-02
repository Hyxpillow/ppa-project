structure NewmanGirvan: 
sig
  val newman_girvan: UndirectedGraph.t -> int array
end =
struct
  structure UGraph = UndirectedGraph
  structure Myprint = Myprint
  structure Brandes = Brandes

  fun newman_girvan (g:UndirectedGraph.t) : int array = 
    let
      val m = Real.fromInt (UGraph.num_edges g)
      
      fun get_comm (g': UGraph.t) : int array * int =
        let
          val comm = Array.array ((UGraph.num_vertices g'), ~1)
          fun dfs (v, label) =
            if Array.sub(comm, v) <> ~1 then ()
            else (
              Array.update(comm, v, label);
              let
                val nbrs = UGraph.neighbors (g', v)
                val start = 0
                val stop = UGraph.degree (g', v)
                fun loop i =
                  if i < stop then (
                    dfs (Seq.nth nbrs i, label);
                    loop (i + 1)
                  ) else ()
              in
                loop start
              end
            )

          fun visit_all (v, label) =
            if v = (UGraph.num_vertices g') then label
            else if Array.sub(comm, v) <> 0 then visit_all(v + 1, label)
            else (
              dfs(v, label);
              visit_all(v + 1, label + 1)
            )
          val comm_count = visit_all(0, 0)
        in
            (comm, comm_count)
        end
      
      (* find all the components and calculate the total Q *)
      fun get_Q (g', comm, comm_count) : real = 
        let
          fun get_comm_Q (comm_i) : real = 
            let
              val dc = Parallel.reduce op+ 0 (0, UGraph.num_vertices g)
                (fn (v) => if Array.sub(comm, v) <> comm_i then 0 
                  else UGraph.degree (g, v)
                )
              val lc = Parallel.reduce op+ 0 (0, UGraph.num_vertices g)
                (fn (v) => if Array.sub(comm, v) <> comm_i then 0 
                  else Parallel.reduce op+ 0 (0, UGraph.degree (g',v)) (fn (u) => if Array.sub(comm, u) <> comm_i then 0 else 1)
                )

              val dc_real = Real.fromInt dc
              val lc_real = Real.fromInt (lc div 2)
            in
              lc_real - (dc_real * dc_real / 4.0 / m)
            end
        in
          Parallel.reduce op+ 0.0 (0, comm_count) 
            (fn (comm_i) => get_comm_Q(comm_i))
        end

      val max_Q = ref 0.0
      val best_g = ref g
      val best_comm_count = ref (UGraph.num_vertices g)

      fun loop_until_no_edge (g) = 
        if (UGraph.num_edges g) = 0 then ()
        else 
          let
            val _ = print ("edge: " ^ Int.toString (UGraph.num_edges g) ^ "\n")
            val (u, v) = Brandes.get_max_betweenness (g)
            val _ = print ("remove: " ^ Int.toString u ^ " " ^ Int.toString v ^ "\n")
            val g' = UGraph.remove_edge (g, u, v)
            
            val (comm, comm_count) = get_comm (g')
            val _ = print ("comm_count: " ^ Int.toString comm_count ^ "\n")

            val cur_Q = if comm_count < !best_comm_count 
              then get_Q (g', comm, comm_count)
              else 0.0
            val _ = if cur_Q > !max_Q then best_g := g' else ()
            val _ = if cur_Q > !max_Q then best_comm_count := comm_count else ()
            val _ = if cur_Q > !max_Q then max_Q := cur_Q else ()
          in
            loop_until_no_edge g'
          end
      val _ = loop_until_no_edge g
      val (comm, _) = get_comm(!best_g)
    in
      comm
    end
end