signature NILUTIL =
sig
    structure Nil : NIL

    val substConInCon : (Nil.var -> Nil.con option) -> Nil.con -> Nil.con

end
