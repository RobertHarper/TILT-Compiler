signature NILEVAL = 
    sig 
	structure Nil : NIL
	type env
	datatype result = VALUE of Nil.exp
	              | CODE_VALUE of Nil.exp * env ref
	              | CLOSURE_VALUE of result * Nil.con ref * result ref
	              | EXCEPTION of result

	val exp_isval : Nil.exp -> bool
	val con_isval : Nil.con -> bool

	val eval_exp : Nil.exp -> result
	val eval_con : Nil.con -> Nil.con

	val eval_mod : Nil.con * Nil.exp -> result

    end

