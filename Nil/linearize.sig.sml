(*$import NIL *)
signature LINEARIZE = 
    sig

	structure Nil : NIL
	val debug : bool ref
	val linearize_mod : Nil.module -> Nil.module
	
    end
