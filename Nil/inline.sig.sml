(*$import Nil *)

(* Inline functions that are non-recursive and either are called once
   or else are sufficiently small and called a sufficiently small number
   of times.
*)

signature INLINE =
 sig
     val debug : bool ref
     (* Print debug information *)

     val inline : {sizeThreshold : int,
		   occurThreshold : int} -> Nil.module -> Nil.module
     (* Inline functions that don't escape and either:
      *    1) Are not recursive and are called only once
      * or 2) Have body size no more than sizeThreshold and no more than occurThreshold calls
      *)
 end
