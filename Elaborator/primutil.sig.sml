(*$import PRIM Prim *)
signature PRIMUTIL =
    sig

	type con			(* parameter *)
	type exp			(* parameter *)
	type context			(* parameter *)
	    
	type value = (con,exp) Prim.value

	val value_type : (exp -> con) -> value -> con
	val get_type : context -> Prim.prim -> con list -> bool * con list * con
	val get_iltype : context -> Prim.ilprim -> con list -> bool * con list * con
	val get_type' : context -> Prim.prim -> con list -> con
	val get_iltype' : context -> Prim.ilprim -> con list -> con

	(* Try to apply the prim.  An exception should be raised if the application
	 * fails in any way.  Should never just return the original expression.
	 *)
	val apply : context -> Prim.prim -> con list -> exp list -> exp

    end
