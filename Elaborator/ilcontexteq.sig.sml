(*$import Il BinIO *)
(* Equality and addition of contexts *)

signature ILCONTEXTEQ =
    sig

	val showUnequal : bool ref	(* Print contexts when unequal  *)
	val debug : bool ref		(* Print trace info when unequal *)
	val blast_debug : bool ref	(* Print trace info when reading partial context *)
	    
	val blastOutContext : BinIO.outstream -> Il.context -> unit
	val blastInContext : BinIO.instream -> Il.context

	val blastOutPartialContext : BinIO.outstream -> Il.partial_context -> unit
	val blastInPartialContext : BinIO.instream -> Il.partial_context

	val eq_con : Il.con * Il.con -> bool
	val eq_context : Il.context * Il.context -> bool
	val eq_partial_context : Il.partial_context * Il.partial_context -> bool

    end
