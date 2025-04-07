val args = CommandLineArgs.positional ()
val filename =
  List.nth (args, 1)
  handle _ => Util.die "Usage: ./main @mpl procs <P> -- <ALGO=[gn|louvain]> <SNAP_FILENAME>"
val algo =
  List.nth (args, 0)
  handle _ => Util.die "Usage: ./main @mpl procs <P> -- <ALGO=[gn|louvain]> <SNAP_FILENAME>"
val _ =
  if algo <> "louvain" andalso algo <> "gn"
  then Util.die "Algorithm must be 'louvain' or 'gn'"
  else ()

structure Louvain = Louvain
structure GN = NewmanGirvan
structure Myprint = Myprint
structure UndirectedGraph = UndirectedGraph


val _ = print "Loading graph (if large, this might take a while...)\n"
val (g, tm1) = Util.getTime (fn _ => Graph.load_from_snap_file ("test-graph/" ^ filename))
val _ = print ("Loaded graph in " ^ Time.fmt 4 tm1 ^ "s\n")
val (ug, tm2) = Util.getTime (fn _ => UndirectedGraph.load_from_directed_graph g)
val _ = print ("Loaded undirected graph in " ^ Time.fmt 4 tm2 ^ "s\n")
val _ = print ("V:" ^ (Int.toString (UndirectedGraph.num_vertices ug)) ^ " E:" ^ (Int.toString (UndirectedGraph.num_edges ug)) ^ "\n")

val _ = print "--------------------\n"
val comm = 
  if algo = "louvain" then
    Benchmark.run (fn _ => Louvain.louvain ug)
  else 
    Benchmark.run (fn _ => GN.newman_girvan ug)
val _ = print "--------------------\n"
val _ = Myprint.f_print_int_array (comm, ("test-graph-output/" ^ filename))
