(*$import Prelude *)
signature PRIMUTILPARAM = 
    sig
	type con
	type exp


	val partial_arrow : con list * con -> con
	val total_arrow : con list * con -> con
	val con_tuple : con list -> con
	val con_bool : con
	val con_unit : con
	val con_int : Prim.intsize -> con
	val con_uint : Prim.intsize -> con
	val con_float : Prim.floatsize -> con
	val con_array : con -> con
	val con_ref : con -> con
	val con_vector : con -> con
	val con_tag : con -> con
	val unit_value : exp
	val exp2value : exp -> (con,exp) Prim.value option
	val value2exp : (con,exp) Prim.value -> exp
	val bool2exp : bool -> exp
    end
