(*$import IL PRIMUTILPARAM Il Name *)
structure IlPrimUtilParam
    :> PRIMUTILPARAM where type con = Il.con 
                     where type exp = Il.exp
    =
    struct

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
	fun bool_help special = CON_SUM{names = [Name.symbol_label(Symbol.varSymbol "false"), 
						 Name.symbol_label(Symbol.varSymbol "true")],
					noncarriers = 2,
					carrier = CON_TUPLE_INJECT[],
					special = special}
	val con_bool = bool_help NONE
	val con_false = bool_help (SOME 0)
	val con_true = bool_help (SOME 1)
	val false_exp = INJ{sumtype=con_bool,field=0,inject=NONE}
	val true_exp = INJ{sumtype=con_bool,field=1,inject=NONE}
	    
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
