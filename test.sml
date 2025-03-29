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

fun test_louvain i filename = 
    let
    val g = Graph.load_from_snap_file filename
  in
    print
        (Louvain.louvain g ^ "V:" ^ Int.toString (Graph.num_vertices g) ^ " E:" ^ Int.toString (Graph.num_edges g) ^ "\n")
  end
val _ =
  List.foldl (fn (filename, i) => (test_louvain i filename; i + 1)) 1
    test_graphs
