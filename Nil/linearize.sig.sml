(*$import Prelude Nil *)

signature LINEARIZE = 
    sig
	val debug : bool ref
	val linearize_mod : Nil.module -> Nil.module
	val linearize_exp : Nil.exp -> Nil.exp        (* Expression may be open *)

    end
