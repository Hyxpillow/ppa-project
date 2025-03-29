val test_graphs =
  [ "test-graph/g1.txt"
  , "test-graph/g2.txt"
  , "test-graph/g3.txt"
  , "test-graph/g4.txt"
  , "test-graph/g5.txt"
  , "test-graph/g6.txt"
  , "test-graph/g7.txt"
  ]


fun test_tc (i, n) filename =
  let
    val g = Graph.load_from_snap_file filename
  in
    print
        ("V:" ^ Int.toString (Graph.num_vertices g) ^ " E:" Int.toString (Graph.num_edges g) ^ "\n")
  end

val num_tests = List.length test_graphs
val _ =
  List.foldl (fn (test, i) => (test_tc (i, num_tests) test; i + 1)) 1
    test_graphs
