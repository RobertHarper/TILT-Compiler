signature SQUISH = 
sig
    structure Nil : NIL
    val squish : Nil.exp -> Nil.exp
    val squish_con : Nil.con -> Nil.con 
end
