signature BLASTER = sig
  type t
  val blastOut : (string * t) -> unit
  val blastIn  : string -> t
end

functor mkBlast(type t):BLASTER = struct
  type t = t

  fun blastOut (filename, (x:t)) = let
    val out = BinIO.openOut filename
    val _ = BinIO.output(out, (System.Unsafe.blastWrite x))
    val _ = BinIO.closeOut out
  in
    ()
  end

  fun blastIn filename = let
    val i = BinIO.openIn filename
    val v = BinIO.input i
  in
    (System.Unsafe.blastRead v) : t
  end
end
