signature TOCLOSURE = 
    sig
	structure Nil : NIL

val do_close_path : bool ref
val do_single_venv : bool ref

	val use_kind_at_bind : bool ref
	val debug : bool ref
	val debug_full : bool ref

val closure_print_free : bool ref
(*
	val liftCode : bool ref
	val close_exp : Nil.exp -> Nil.exp
	val close_con : Nil.con -> Nil.con
	val close_kind : Nil.kind -> Nil.kind
*)
	val close_mod : Nil.module -> Nil.module

    end
