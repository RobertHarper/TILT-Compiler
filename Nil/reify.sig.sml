(*$import Nil *)

(* Determines which constructors are used as data, creates proper traces, and possibly adds extra bindings for reified types *)

signature REIFY = 
    sig
	val debug : bool ref
	(* Print debugging information *)

	val reify_mod : Nil.module -> Nil.module
    end

