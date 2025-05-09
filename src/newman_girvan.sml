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
      (* W:O(n + m) S:O(n + m) because of DFS *)
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
            else if Array.sub(comm, v) <> ~1 then visit_all(v + 1, label)
            else (
              dfs(v, label);
              visit_all(v + 1, label + 1)
            )
          val comm_count = visit_all(0, 0)
        in
            (comm, comm_count)
        end
      
      (* find all the components and calculate the total Q *)
      (* W:O(nm * comm_count) S:O(log(n) * log(m) * log(comm_count)) *)
      fun get_Q (g', comm, comm_count) : real = 
        let
          (* W:O(nm) S:O(log(n) * log(m)) *)
          fun get_comm_Q (comm_i) : real = 
            let
              val m' = Real.fromInt (UGraph.num_edges g)
              (* W:O(n) S:O(log n) *)
              val dc = Parallel.reduce op+ 0 (0, UGraph.num_vertices g')
                (fn (v) => if Array.sub(comm, v) <> comm_i then 0 
                  else UGraph.degree (g', v)
                )
              (* W:O(nm) S:O(log(n) * log(m)) *)
              val lc = Parallel.reduce op+ 0 (0, UGraph.num_vertices g')
                (fn (v) => if Array.sub(comm, v) <> comm_i then 0 
                  else 
                    let 
                      val nbrs = UGraph.neighbors (g', v)
                    in
                      Parallel.reduce op+ 0 (0, UGraph.degree (g',v)) 
                        (fn (nbr_i) => if Array.sub(comm, (Seq.nth nbrs nbr_i)) <> comm_i then 0 else 1)
                    end
                )

              val dc_real = Real.fromInt dc
              val lc_real = Real.fromInt (lc div 2)
            in
              (lc_real / m') - (dc_real * dc_real / 4.0 / m' / m')
            end
          
          (* W:O(nm * comm_count) S:O(log(n) * log(m) * log(comm_count)) *)
          val res = Parallel.reduce op+ 0.0 (0, comm_count) 
            (fn (comm_i) => get_comm_Q(comm_i))
        in
          res
        end

      val max_Q = ref 0.0
      val best_g = ref g
      val best_comm_count = ref (UGraph.num_vertices g)

      (* W:O(mn^2 + nm^2)) S:O(mn + m^2)*)
      fun loop_until_no_edge (g) = 
        if (UGraph.num_edges g) = 0 then ()
        else 
          let
            (* W:O(n^2 + nm)) S:O(n + m)*)
            val (u, v) = Brandes.get_max_betweenness (g)
            (* W:O(n + m) S:O(logm) *)
            val g' = UGraph.remove_edge (g, u, v)

            (* W:O(n + m) S:O(n + m) because of DFS *)
            val (comm, comm_count) = get_comm (g')
            val cur_Q = if comm_count <> !best_comm_count 
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