structure Myprint: 
sig
  val print_seq: int Seq.t -> unit
end =
struct
  fun print_seq (s: int Seq.t) =
    let
        val len = Seq.length s
        fun loop i =
        if i >= len then ""
        else
            Int.toString (Seq.nth s i) ^
            (if i < len - 1 then "," else "") ^
            loop (i + 1)
    in
        print ("[" ^ loop 0 ^ "]\n")
    end
end