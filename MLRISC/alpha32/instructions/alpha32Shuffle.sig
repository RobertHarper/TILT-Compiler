signature ALPHA32SHUFFLE = sig
  structure I : ALPHA32INSTR

  type t = {regMap:int -> int, temp:I.ea option, dst:int list, src:int list}

  val shuffle : t -> I.instruction list
  val shufflefp : t -> I.instruction list
end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:31:51  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:15:17  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:48  pscheng
# *** empty log message ***
#
 * Revision 1.1.1.1  1998/04/08 18:39:01  george
 * Version 110.5
 *
 *)
