(*
	The elaborator used to generate Match and Bind exceptions
	rather than looking them up.  They were impossible to catch.
*)
val _ = (case true of false => ()) handle Match => print "Match\n"
val _ = let val true = false in () end handle Bind => print "Bind\n"
