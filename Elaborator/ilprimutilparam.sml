functor IlPrimUtilParam(structure Il : IL)
    : PRIMUTILPARAM =
    struct

(*	open IlUtil  *)
	open Il
	open Util
	open Prim

	type con = con
	type exp = exp
	type ('con,'exp) value = ('con,'exp) Prim.value
	val error = fn s => error "ilprimutilparam.sml" s

	fun partial_arrow (cons,c2) = CON_ARROW(cons,c2,false,oneshot_init PARTIAL)
	fun total_arrow (cons,c2) = CON_ARROW(cons,c2,false,oneshot_init TOTAL)
	fun generate_tuple_symbol (i : int) = Symbol.labSymbol(Int.toString i)
	fun generate_tuple_label (i : int) = Name.symbol_label(generate_tuple_symbol i)
	val unit_exp : exp = RECORD[]
	val con_unit = CON_RECORD[]
	val con_bool = CON_SUM{noncarriers = 2,
			       carriers = [],
			       special = NONE}
	val false_exp = INJ{noncarriers=2,carriers=[],special=0,inject=NONE}
	val true_exp = INJ{noncarriers=2,carriers=[],special=1,inject=NONE}
	    
	fun con_tuple conlist = CON_RECORD(Listops.mapcount (fn (i,c) => 
							     (generate_tuple_label (i+1),c)) conlist)

	val con_int = CON_INT
	val con_uint = CON_UINT
	val con_float = CON_FLOAT
	val con_array = CON_ARRAY
	val con_ref = CON_REF
	val con_vector = CON_VECTOR
	val unit_value = unit_exp
	val con_tag = CON_TAG

	fun exp2value (SCON v) = SOME v
	  | exp2value _ = NONE

	val value2exp = SCON

	fun bool2exp false = false_exp
	  | bool2exp true = true_exp

    end
