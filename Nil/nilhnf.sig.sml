(*$import Nil NilContextPre *)

signature NILHNF = 
  sig
    type context = NilContextPre.context
    val reduce_hnf : context * Nil.con -> Nil.con
    val con_reduce : context * Nil.con -> Nil.con
  end