(* Removes some inefficient HIL idiosyncrasies 
   that the phjase-splitter did not get rid of.
   Globally unused datatype structues and
   monomorphic recursive functions *)
signature CLEANUP = 
sig
    structure Nil : NIL
	
    val cleanModule : Nil.module -> Nil.module

end