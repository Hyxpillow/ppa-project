val test_graphs = "test-graph/g6.txt"

structure Louvain = Louvain
structure Myprint = Myprint
structure UndirectedGraph = UndirectedGraph

(* val filename =
  List.hd (CommandLineArgs.positional ())
  handle _ => Util.die "Usage: ./main @mpl procs <P> -- <SNAP_FILENAME>"

val _ = print "Loading graph (if large, this might take a while...)\n"
val (graph, tm) = Util.getTime (fn _ => Graph.load_from_snap_file filename)
val _ = print ("Loaded graph in " ^ Time.fmt 4 tm ^ "s\n")

val _ = print ("num vertices " ^ Int.toString (Graph.num_vertices graph) ^ "\n")
val _ = print ("num edges    " ^ Int.toString (Graph.num_edges graph) ^ "\n") *)

val g = Graph.load_from_snap_file filename
val ug = UndirectedGraph.load_from_directed_graph g
(* val res = Louvain.louvain (ug) *)
val _ = print "--------------------\n"
val res = Benchmark.run (fn _ => Louvain.louvain ug)
val _ = print "--------------------\n"
val _ = Myprint.print_int_array res
val _ = print ("UV:" ^ (Int.toString (UndirectedGraph.num_vertices ug)) ^ " UE:" ^ (Int.toString (UndirectedGraph.num_edges ug)) ^ "\n")
val _ = print ("V:" ^ (Int.toString (Graph.num_vertices g)) ^ " E:" ^ (Int.toString (Graph.num_edges g)) ^ "\n")
