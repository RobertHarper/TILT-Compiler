signature PRIMUTIL =
    sig
	structure Prim : PRIM
	type con
	type exp (* this corresponds to the exp of some intermediate language *)
	type value = exp Prim.value

	val value_type : value -> con
	val get_type : Prim.prim -> con list -> con
	val get_iltype : Prim.ilprim -> con
	val apply : Prim.prim -> con list -> exp list -> exp
    end