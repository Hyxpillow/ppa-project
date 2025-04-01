(* val test_graphs = "test-graph/g6.txt" *)
val test_graphs = "cit-Patents.txt"

structure Louvain = Louvain
structure Myprint = Myprint
structure UndirectedGraph = UndirectedGraph

(* val filename =
  List.hd (CommandLineArgs.positional ())
  handle _ => Util.die "Usage: ./main @mpl procs <P> -- <SNAP_FILENAME>" *)

val _ = print "Loading graph (if large, this might take a while...)\n"
val (g, tm1) = Util.getTime (fn _ => Graph.load_from_snap_file test_graphs)
val _ = print ("Loaded graph in " ^ Time.fmt 4 tm1 ^ "s\n")
val (ug, tm2) = Util.getTime (fn _ => UndirectedGraph.load_from_directed_graph g)
val _ = print ("Loaded undirected graph in " ^ Time.fmt 4 tm2 ^ "s\n")
val _ = print ("V:" ^ (Int.toString (UndirectedGraph.num_vertices ug)) ^ " E:" ^ (Int.toString (UndirectedGraph.num_edges ug)) ^ "\n")

(* val res = Louvain.louvain (ug) *)
val _ = print "--------------------\n"
val res = Benchmark.run (fn _ => Louvain.louvain ug)
val _ = print "--------------------\n"
(* val _ = Myprint.print_int_array res *)
