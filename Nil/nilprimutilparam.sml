structure NilPrimUtilParam
  :> PRIMUTILPARAM where type con = Nil.con
	             and type exp = Nil.exp
		     and type context = NilContextPre.context =
  struct

    open Nil
    open Prim

    val RefIsArray = CompilerControl.RefIsArray

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
    fun con_vector c = Prim_c(Vector_c,[c])
    fun con_intarray sz = Prim_c(IntArray_c sz,[])
    fun con_intvector sz = Prim_c(IntVector_c sz,[])
    fun con_floatarray sz = Prim_c(FloatArray_c sz,[])
    fun con_floatvector sz = Prim_c(FloatVector_c sz,[])

    fun con_ref c = if !RefIsArray then Prim_c(Array_c,[c]) else Prim_c(Ref_c,[c])
    fun con_tag c = Prim_c(Exntag_c,[c])

    fun con_bool context = Prim_c(Sum_c{tagcount = 0w2,
					totalcount = 0w2,
					known = NONE},[Crecord_c[]])
    fun bool2exp context b = 
      let
	val inject_var = Name.fresh_named_var "inject"
	val forget_var = Name.fresh_named_var "forget_bool"
	val bool_var = Name.fresh_named_var (if b then "true" else "false")
	val sum_var = Name.fresh_named_var "bool_sum"	  

	val arm = TilWord32.fromInt (case b of false => 0 | true => 1)
	  
	val inject_exp = Prim_e (NilPrimOp (inject_known arm), [],[Var_c sum_var], [])
	val forget_exp = ForgetKnown_e (Var_c sum_var,arm)
	    
	val bool_exp  = Coerce_e(Var_e forget_var,[],Var_e inject_var)
      in  Let_e (Sequential,
		   [
		    Con_b(Runtime,Con_cb(sum_var,con_bool context)),
		    Exp_b(inject_var,TraceKnown TraceInfo.Trace,inject_exp),
		    Exp_b(forget_var,TraceKnown TraceInfo.Notrace_Int,forget_exp),
		    Exp_b(bool_var,TraceKnown TraceInfo.Trace,bool_exp)
		    ],
		   Var_e bool_var)
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
