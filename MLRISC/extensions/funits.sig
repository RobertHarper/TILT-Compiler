(*
 * Functional units
 *)

signature FUNITS =
sig

   type fu

   val numberOfFUs : int
   val toString    : fu -> string
   val toInt       : fu -> int
   val fromInt     : int -> fu

end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:31:57  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:15:39  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:07:08  pscheng
# *** empty log message ***
#
 *)
