(*$import NIL PRIMUTILPARAM Int32 Nil *)
structure NilPrimUtilParam
    :> PRIMUTILPARAM where type con = Nil.con 
		       and type exp = Nil.exp =
    struct

      open Curtain
      open Nil 
	open Prim

	type con = con
	type exp = exp
	type ('con,'exp) value = ('con,'exp) Prim.value

	fun simple_arrow (effect,cons,c2) = C.AllArrow_c{openness = Code,
							 effect = effect,
							 isDependent = false,
							 tFormals = [],
							 eFormals = map (fn c => (NONE,c)) cons,
							 fFormals = 0w0,
							 body_type = c2}
	fun partial_arrow (cons,c2) = simple_arrow(Partial,cons,c2)
	fun total_arrow (cons,c2) =  simple_arrow(Total,cons,c2)

	fun con_int is = C.Prim_c(Int_c is,[])
	fun con_uint is = C.Prim_c(Int_c is,[])
	fun con_float fs = C.Prim_c(Float_c fs,[])
	fun con_array c = C.Prim_c(Array_c,[c])
	fun con_ref c = C.Prim_c(Array_c,[c])
	fun con_vector c = C.Prim_c(Vector_c,[c])
	fun con_tag c = C.Prim_c(Exntag_c,[c])
	val con_bool = C.Prim_c(Sum_c{tagcount=0w2,totalcount=0w2,known=NONE},[C.Crecord_c[]])
	val con_unit = C.Prim_c(Record_c ([],NONE), [])
	val unit_value = E.Prim_e(NilPrimOp(record []),[],[])

	fun exp2value e = (case E.expose e
			     of (Const_e v) => SOME v
			      | _ => NONE)

	val value2exp = E.hide o Const_e
	    
	fun generate_tuple_symbol (i : int) = Symbol.labSymbol(Int.toString i)
	fun generate_tuple_label (i : int) = Name.symbol_label(generate_tuple_symbol i)
	fun con_tuple clist = 
	    let fun mapper(i,_) = generate_tuple_label(i+1)
		val labs = Listops.mapcount mapper clist
	    in C.Prim_c(Record_c (labs,NONE),clist)
	    end
	val false_con = C.Prim_c(Sum_c{tagcount=0w2,totalcount=0w2,known=SOME 0w0},[C.Crecord_c[]])
	val true_con = C.Prim_c(Sum_c{tagcount=0w2,totalcount=0w2,known=SOME 0w1},[C.Crecord_c[]])
	val false_con' = C.Prim_c(Sum_c{tagcount=0w2,totalcount=0w2,known=NONE},[C.Crecord_c[]])
	val true_con' = C.Prim_c(Sum_c{tagcount=0w2,totalcount=0w2,known=NONE},[C.Crecord_c[]])
	val false_exp = E.Prim_e(NilPrimOp (inject_known 0w0),[false_con'],[])
	val true_exp = E.Prim_e(NilPrimOp (inject_known 0w1),[true_con'],[])
	fun bool2exp false = false_exp
	  | bool2exp true = true_exp

    end
