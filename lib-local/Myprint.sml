structure Myprint: 
sig
 
  val print_int_seq: int Seq.t -> unit
  val print_real_seq: real Seq.t -> unit
  val print_int_array: int array -> unit
  val f_print_int_array: int array * string -> unit
end =
struct
  fun print_int_array (s: int array) =
    let
        val len = Array.length s
        fun loop i =
        if i >= len then ""
        else
            Int.toString (Array.sub (s,i)) ^
            (if i < len - 1 then "," else "") ^
            loop (i + 1)
    in
        print ("[" ^ loop 0 ^ "]\n")
    end
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
  fun f_print_int_array (arr: int array, path: string) =
    let
      val out = TextIO.openOut path
      val _ = TextIO.output (out, "[")
      val last = Array.length arr - 1
      fun write i =
        if i = last then
          TextIO.output (out, Int.toString (Array.sub (arr, i)))
        else (
          TextIO.output (out, Int.toString (Array.sub (arr, i)) ^ ", ");
          write (i + 1)
        )
      val _ = write 0
      val _ = TextIO.output (out, "]\n")
      val _ = TextIO.closeOut out
    in
      ()
    end
end