signature BETAREDUCE = 
sig
    structure Nil : NIL
	
(*
    val reduceExp : Nil.exp -> Nil.exp
    val reduceCon : Nil.con -> Nil.con
    val reduceKind : Nil.kind -> Nil.kind
*)
    val reduceModule : Nil.module -> Nil.module

end