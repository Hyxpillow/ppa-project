structure Louvain: 
sig
  val louvain: Graph.t -> int Seq.t
end =
struct
  fun louvain (g: Graph.t) : int Seq.t = 
    Seq.tabulate (fn i => i) 5
end