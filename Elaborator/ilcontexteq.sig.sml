(*$import IL BinIO *)
(* Equality and addition of contexts *)

signature ILCONTEXTEQ =
    sig
	structure Il : IL

	val blastOutContext : BinIO.outstream -> Il.context -> unit
	val blastInContext : BinIO.instream -> Il.context
	val eq_context : Il.context * Il.context -> bool

    end
