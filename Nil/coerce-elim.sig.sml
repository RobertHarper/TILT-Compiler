(*$import Nil *)

(* Transformation to replace occurrences of coercion constructs in a module with *)
(* functions that use the primops roll and unroll.                               *)

signature COERCEELIM = 
  sig
    val transform : Nil.module -> Nil.module
  end