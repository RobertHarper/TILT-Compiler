functor IlPrimUtilParam(structure IlUtil : ILUTIL) 
    : PRIMUTILPARAM =
    struct

	open IlUtil 
	open Il
	open Util
	open Prim

	type con = con
	type exp = exp
	type 'a value = 'a Prim.value
	val error = fn s => error "ilprimutilparam.sml" s

	fun partial_arrow (c1,c2) = CON_ARROW(c1,c2,oneshot_init PARTIAL)
	fun total_arrow (c1,c2) = CON_ARROW(c1,c2,oneshot_init PARTIAL)
	val con_int = CON_INT
	val con_uint = CON_UINT
	val con_float = CON_FLOAT
	val con_array = CON_ARRAY
	val con_ref = CON_REF
	val con_vector = CON_VECTOR
	val unit_value = unit_exp

	fun exp2value (SCON v) = SOME v
	  | exp2value _ = NONE

	val value2exp = SCON

	fun bool2exp false = false_exp
	  | bool2exp true = true_exp

    end
