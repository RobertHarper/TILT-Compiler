signature PASS =
    sig 
	structure Nil :NIL
	val doModule : bool -> Nil.module -> Nil.module
    end
