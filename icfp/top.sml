
structure Top :>

    sig
	val runFile : string -> Base.stack
    end

    = 
    struct
	
	fun runFile file = 
	    let val str = ParseString.file2string file
		val SOME ast = ParseString.parse str

		val res = Eval.eval ast 
		    handle e as Base.Eval s => (print "Evaluation Error: "; print s; print "\n"; raise e)
	    in  res
	    end
    end
