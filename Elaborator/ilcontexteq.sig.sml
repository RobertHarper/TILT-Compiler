(* Equality of contexts *)

signature ILCONTEXTEQ =
    sig
	structure Il : IL
	val eq_context : Il.context * Il.context -> bool
    end
