signature HASHABLE =
    sig

	type key
	type hashcode = word		(* necessary, for the time being *)

	val hash : key -> hashcode
	val eq : key * key -> bool

    end
