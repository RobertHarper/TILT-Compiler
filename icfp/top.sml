
structure Top :>

    sig
				val runFile : string -> 'a
				val run     : unit   -> 'a

    end = 
    struct

	fun goahead s =
	    let 
					val SOME ast = ParseString.parse s
				
					val _ = Eval.eval ast 
							handle e as Base.Eval s => (print "Evaluation Error: "; print s; 
																					print "\n"; raise e)
	    in  Base.exit_ok ()
	    end handle _ => Base.exit_error ()

	(* from a file *)	
	fun runFile file = 
			goahead (ParseString.file2string file)

	(* from standard in *)
	fun run () =
			goahead (Stdin.stdintostring ())

end
