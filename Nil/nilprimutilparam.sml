(*$import Sequence Name Symbol Listops NIL PRIMUTILPARAM Prim Int Nil IlUtil NilContextPre TilWord32 TraceInfo *)

structure NilPrimUtilParam
    :> PRIMUTILPARAM where type con = Nil.con 
		       and type exp = Nil.exp
		       and type context = NilContextPre.context =
    struct

	open Nil
	open Prim

	type context = NilContextPre.context
	type con = con
	type exp = exp
	type ('con,'exp) value = ('con,'exp) Prim.value

	fun simple_arrow (effect,cons,c2) = AllArrow_c{openness = Code,
						       effect = effect,
						       tFormals = [],
						       eFormals = cons,
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
	local
	    val bool_import = Name.to_open (Name.internal_label "_bool")
	    val (bool_import_c,bool_import_r) = Name.make_cr_labels bool_import


		
	    val bool_lab = Name.symbol_label (Symbol.tycSymbol "bool")
	    val bool_sum_lab = Name.internal_label "bool_sum"

	    val bool_in_lab = Name.to_coercion (Name.internal_label ("bool_in"))
	in
	    fun con_bool context =
		let val v = NilContextPre.find_labelled_var (context,bool_import_c)
		in  Proj_c (Var_c v, bool_lab)
		end
	    fun bool2exp context b =
		let val rv = NilContextPre.find_labelled_var (context,bool_import_r)
		    val cv = NilContextPre.find_labelled_var (context,bool_import_c)

		    val sum_var = Name.fresh_named_var "bool_sum"
		    val coercion_var = Name.fresh_named_var "bool_in"
		    val inject_var = Name.fresh_named_var "inject"
		    val bool_var = Name.fresh_named_var (if b then "true" else "false")

		    val coercion_exp = Prim_e (NilPrimOp (select bool_in_lab),[],[],[Var_e rv])
		    val sum = Proj_c (Var_c cv, bool_sum_lab)

		    val arm = TilWord32.fromInt (case b of false => 0 | true => 1)

		    val inject_exp = Prim_e (NilPrimOp (inject_known arm), [],[Var_c sum_var], [])

		    val bool_exp = Coerce_e (Var_e coercion_var, [], Var_e inject_var)
		      
		in  Let_e (Sequential,
			   [
			    Con_b(Runtime,Con_cb(sum_var,sum)),
			    Exp_b(coercion_var,TraceKnown TraceInfo.Notrace_Int,coercion_exp),
			    Exp_b(inject_var,TraceKnown TraceInfo.Trace,inject_exp),
			    Exp_b(bool_var,TraceCompute cv,bool_exp)
			    ],
			   Var_e bool_var)
		end
	end
	val con_unit = Prim_c(Record_c [], [])
	val unit_value = Prim_e(NilPrimOp(record []),[],[],[])

	fun exp2value (Const_e v) = SOME v
	  | exp2value _ = NONE

	val value2exp = Const_e
	    
	fun generate_tuple_symbol (i : int) = Symbol.labSymbol(Int.toString i)
	fun generate_tuple_label (i : int) = Name.symbol_label(generate_tuple_symbol i)
	fun con_tuple clist = 
	    let fun mapper(i,_) = generate_tuple_label(i+1)
		val labs = Listops.mapcount mapper clist
	    in Prim_c(Record_c labs, clist)
	    end
    end
