signature TOCLOSURE = 
    sig
	structure Nil : NIL

	val debug : bool ref
	val liftCode : bool ref

(*
	val close_exp : Nil.exp -> Nil.exp
	val close_con : Nil.con -> Nil.con
	val close_kind : Nil.kind -> Nil.kind
*)
	val close_mod : Nil.module -> Nil.module

    end
