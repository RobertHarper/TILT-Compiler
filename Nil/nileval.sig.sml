signature NILEVAL = 
    sig 
	structure Nil : NIL
	datatype result = VALUE of Nil.exp
	                | EXCEPTION of Nil.exp

	val exp_isval : Nil.exp -> bool
	val eval_exp : Nil.exp -> result

    end

