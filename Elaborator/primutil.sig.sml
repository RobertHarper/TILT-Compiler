signature PRIMUTIL =
    sig
	structure Prim : PRIM
	type con
	type exp (* this corresponds to the exp of some intermediate language *)
	type value = (con,exp) Prim.value

	val value_type : (exp -> con) -> value -> con
	val get_type : Prim.prim -> con list -> con
	val get_iltype : Prim.ilprim -> con list -> con
	val apply : Prim.prim -> con list -> exp list -> exp

	val same_intsize : Prim.intsize * Prim.intsize -> bool
	val same_float_size : Prim.floatsize * Prim.floatsize -> bool
    end
