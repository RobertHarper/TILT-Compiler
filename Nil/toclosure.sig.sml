signature TOCLOSURE = 
    sig
	structure Nil : NIL
	val close_exp : Nil.exp -> Nil.exp
	val close_con : Nil.con -> Nil.con
	val close_kind : Nil.kind -> Nil.kind

    end
