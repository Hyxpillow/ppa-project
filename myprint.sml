structure Myprint: 
sig
  val print_int_seq: int Seq.t -> unit
  val print_real_seq: real Seq.t -> unit
end =
struct
  fun print_int_seq (s: int Seq.t) =
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
  fun print_real_seq (s: real Seq.t) =
    let
        val len = Seq.length s
        fun loop i =
        if i >= len then ""
        else
            Real.toString (Seq.nth s i) ^
            (if i < len - 1 then "," else "") ^
            loop (i + 1)
    in
        print ("[" ^ loop 0 ^ "]\n")
    end
end