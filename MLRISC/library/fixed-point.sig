signature FIXED_POINT =
sig
   eqtype fixed_point

   val fixed_point  : int * int -> fixed_point

   val zero     : fixed_point
   val one      : fixed_point

   val <        : fixed_point * fixed_point -> bool
   val >        : fixed_point * fixed_point -> bool
   val >=       : fixed_point * fixed_point -> bool
   val <=       : fixed_point * fixed_point -> bool
   val !=       : fixed_point * fixed_point -> bool
   val ==       : fixed_point * fixed_point -> bool
   val compare  : fixed_point * fixed_point -> order

   val +        : fixed_point * fixed_point -> fixed_point
   val -        : fixed_point * fixed_point -> fixed_point
   val *        : fixed_point * fixed_point -> fixed_point
   val /        : fixed_point * fixed_point -> fixed_point
   val scale    : fixed_point * int -> fixed_point
   val div      : fixed_point * int -> fixed_point
   val min      : fixed_point * fixed_point -> fixed_point
   val max      : fixed_point * fixed_point -> fixed_point

   val toString : fixed_point -> string
   val toReal   : fixed_point -> real
   val toWord   : fixed_point -> word
   val fromReal : real -> fixed_point
   val fromInt  : int -> fixed_point
end

(*
 * $Log$
# Revision 1.1  99/02/17  21:16:56  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:08:44  pscheng
# *** empty log message ***
#
 *)
