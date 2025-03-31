val test_graphs =
  [ "test-graph/g1.txt"
  , "test-graph/g2.txt"
  , "test-graph/g3.txt"
  , "test-graph/g4.txt"
  , "test-graph/g5.txt"
  , "test-graph/g6.txt"
  , "test-graph/g7.txt"
  ]

structure Louvain = Louvain
structure Myprint = Myprint
structure UndirectedGraph = UndirectedGraph

fun test_louvain i filename = 
  let
    val g = Graph.load_from_snap_file filename
    val ug = UndirectedGraph.load_from_directed_graph g
    val res = Louvain.louvain (ug)
    val _ = Myprint.print_real_array res
    val _ = print ("UV:" ^ (Int.toString (UndirectedGraph.num_vertices ug)) ^ " UE:" ^ (Int.toString (UndirectedGraph.num_edges ug)) ^ "\n")
    (* val _ = print ("V:" ^ (Int.toString (Graph.num_vertices g)) ^ " E:" ^ (Int.toString (Graph.num_edges g)) ^ "\n") *)
  in
    1
  end

val _ = List.foldl (fn (filename, i) => (test_louvain i filename; i + 1)) 1 test_graphs
