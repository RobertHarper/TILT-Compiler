(* sparcShuffle.sig -- shuffle src registers into destination registers *)

signature SPARCSHUFFLE = sig
  structure I : SPARCINSTR
 
  type t = {regMap:int->int, temp:I.ea option, dst:int list, src:int list}

  val shuffle : t -> I.instruction list
  val shufflefp : t -> I.instruction list
end

(*
 * $Log$
# Revision 1.1  99/02/17  21:17:45  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:09:44  pscheng
# *** empty log message ***
#
 * Revision 1.1.1.1  1998/08/05 19:38:49  george
 *   Release 110.7.4
 *
 *)
