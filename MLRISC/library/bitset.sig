(*
 * Expandable set in bitvector format
 *)

signature BITSET =
sig

   type bitset 

   val create        : int -> bitset
   val size          : bitset -> int
   val contains      : bitset * int -> bool
   val set           : bitset * int -> unit
   val reset         : bitset * int -> unit
   val clear         : bitset -> unit
   val markAndTest   : bitset * int -> bool
   val unmarkAndTest : bitset * int -> bool
   val toString      : bitset -> string

end

(*
 * $Log$
# Revision 1.1  99/02/17  21:16:53  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:08:36  pscheng
# *** empty log message ***
#
 *)
