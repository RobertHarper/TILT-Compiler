(*$import Nil *)

(* Optimize Nil code by hoisting bindings to higher levels. Assumes no duplicate variables in program. *)

signature HOIST =
    sig 
	val optimize : Nil.module -> Nil.module
    end
