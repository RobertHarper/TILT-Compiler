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
