(* Optimize Nil code by hoisting bindings to higher levels. Assumes no duplicate variables in program. *)

signature HOIST =
    sig
	val HoistDiag : bool ref
	val optimize : Nil.module -> Nil.module
	val optimize_int : Nil.interface -> Nil.interface
    end
