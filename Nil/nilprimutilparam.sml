(*$import Prelude TopLevel Sequence Name Symbol Listops NIL PRIMUTILPARAM Prim Int Nil IlUtil *)

structure NilPrimUtilParam
    :> PRIMUTILPARAM where type con = Nil.con 
		       and type exp = Nil.exp =
    struct

	open Nil
	open Prim

	type con = con
	type exp = exp
	type ('con,'exp) value = ('con,'exp) Prim.value

	fun simple_arrow (effect,cons,c2) = AllArrow_c{openness = Code,
						       effect = effect,
						       isDependent = false,
						       tFormals = [],
						       eFormals = map (fn c => (NONE,c)) cons,
						       fFormals = 0w0,
						       body_type = c2}
	fun partial_arrow (cons,c2) = simple_arrow(Partial,cons,c2)
	fun total_arrow (cons,c2) =  simple_arrow(Total,cons,c2)

	fun con_int is = Prim_c(Int_c is,[])
	fun con_uint is = Prim_c(Int_c is,[])
	fun con_float fs = Prim_c(Float_c fs,[])
	fun con_array c = Prim_c(Array_c,[c])
	fun con_ref c = Prim_c(Array_c,[c])
	fun con_vector c = Prim_c(Vector_c,[c])
	fun con_tag c = Prim_c(Exntag_c,[c])
	val con_sumbool = Prim_c(Sum_c{tagcount=0w2,totalcount=0w2,known=NONE},[Crecord_c[]])
 	val con_bool = 
 	    Proj_c(Mu_c(false,
 			Sequence.fromList
 			[(Name.fresh_named_var "nil_con_bool",
 			  con_sumbool)]),
 		   IlUtil.generate_tuple_label 1)
	val con_unit = Prim_c(Record_c ([],NONE), [])
	val unit_value = Prim_e(NilPrimOp(record []),[],[])

	fun exp2value (Const_e v) = SOME v
	  | exp2value _ = NONE

	val value2exp = Const_e
	    
	fun generate_tuple_symbol (i : int) = Symbol.labSymbol(Int.toString i)
	fun generate_tuple_label (i : int) = Name.symbol_label(generate_tuple_symbol i)
	fun con_tuple clist = 
	    let fun mapper(i,_) = generate_tuple_label(i+1)
		val labs = Listops.mapcount mapper clist
	    in Prim_c(Record_c (labs,NONE),clist)
	    end

	(* We may only apply ROLL to values (hence the LET).  To help
	   maintain the invariant that bound variables are unique, we
	   require that no two applications of bool2exp use the same
	   bound variable name. *)
	fun bool_help w =
	    let val var = Name.fresh_var()
	    in  Let_e(Sequential,
		      [Exp_b (var, TraceUnknown,
			      Prim_e(NilPrimOp (inject_known w), [con_sumbool], []))],
		      Prim_e(NilPrimOp roll, [con_bool], [Var_e var]))
	    end
	fun bool2exp false = bool_help 0w0
	  | bool2exp true = bool_help 0w1

    end
