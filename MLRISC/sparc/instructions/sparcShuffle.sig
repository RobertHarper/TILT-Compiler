(* sparcShuffle.sig -- shuffle src registers into destination registers *)

signature SPARCSHUFFLE = sig
  structure I : SPARCINSTR
 
  type t = {regMap:int->int, temp:I.ea option, dst:int list, src:int list}

  val shuffle : t -> I.instruction list
  val shufflefp : t -> I.instruction list
end
