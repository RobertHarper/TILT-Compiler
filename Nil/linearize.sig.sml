signature LINEARIZE = 
    sig

	structure Nil : NIL
	val linearize_mod : Nil.module -> Nil.module
	
    end