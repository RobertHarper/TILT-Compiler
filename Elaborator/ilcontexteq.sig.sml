(*$import Il BinIO *)
(* Equality and addition of contexts *)

signature ILCONTEXTEQ =
    sig

	val blastOutContext : BinIO.outstream -> Il.context -> unit
	val blastInContext : BinIO.instream -> Il.context

	val blastOutPartialContext : BinIO.outstream -> Il.partial_context -> unit
	val blastInPartialContext : BinIO.instream -> Il.partial_context

	val eq_context : Il.context * Il.context -> bool
	val eq_partial_context : Il.partial_context * Il.partial_context -> bool

    end
