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
      val unitlab = Name.to_unit(Name.internal_label "Firstlude")
      val (unit_c, unit_r) = Name.make_cr_labels unitlab
      val bool_mod = Name.to_open (Name.internal_label "_bool")
      val bool_lab = Name.symbol_label (Symbol.tycSymbol "bool")
      val bool_sum_lab = Name.internal_label "bool_sum"
      val bool_in_lab = Name.to_coercion (Name.internal_label ("bool_in"))
      fun path2flatvar (context,p) = 
	let
	  val flat_l = Name.join_labels p
	  val v = NilContextPre.find_labelled_var (context,flat_l)
	in v
	end
    in
      fun con_bool context = 
	if !(Stats.bool "flatten_modules") then
	  let 
	    val v = path2flatvar (context,[unit_c,bool_mod,bool_lab])
	  in  Var_c v
	  end
	else
	  let 
	    val v = NilContextPre.find_labelled_var (context,unit_c)
	  in  Proj_c (Proj_c(Var_c v, bool_mod), bool_lab)
	  end
      fun bool2exp context b =
        let
	  fun help () = 
	    if !(Stats.bool "flatten_modules") then
	      let
		val mod_var = path2flatvar (context,[unit_r,bool_mod])
		val sum_var = path2flatvar (context,[unit_c,bool_mod,bool_sum_lab])
		val coercion_var = path2flatvar (context,[unit_r,bool_in_lab])
		val bool_var = path2flatvar (context,[unit_c,bool_mod,bool_lab])
		val bool_tr = TraceCompute bool_var
	      in ([],mod_var,sum_var,coercion_var,bool_tr)
	      end
	    else
	      let
		val rv = NilContextPre.find_labelled_var (context,unit_r)
		val cv = NilContextPre.find_labelled_var (context,unit_c)
		  
		val sum_var = Name.fresh_named_var "bool_sum"
		val mod_var = Name.fresh_named_var "bool_mod"
		val coercion_var = Name.fresh_named_var "bool_in"
		  
		val mod_exp = Prim_e (NilPrimOp (select bool_mod),[],[],[Var_e rv])
		val coercion_exp = Prim_e (NilPrimOp (select bool_in_lab),[],[],[Var_e mod_var])
		val sum_type = Proj_c (Proj_c(Var_c cv, bool_mod), bool_sum_lab)
		val bool_tr = TraceKnown (TraceInfo.Compute (cv,[bool_mod,bool_lab]))
		  
		val bnds = [Con_b(Runtime,Con_cb(sum_var,sum_type)),
			    Exp_b(mod_var,TraceKnown TraceInfo.Trace,mod_exp),
			    Exp_b(coercion_var,TraceKnown TraceInfo.Notrace_Int,coercion_exp)
			    ]
		  
	      in (bnds,mod_var,sum_var,coercion_var,bool_tr)
	      end
	  val (bnds,mod_var,sum_var,coercion_var,bool_tr) = help ()
	    
	  val inject_var = Name.fresh_named_var "inject"
	  val forget_var = Name.fresh_named_var "forget_bool"
	  val forgotten_var = Name.fresh_named_var "forgotten_bool"
	  val bool_var = Name.fresh_named_var (if b then "true" else "false")
	    
	  val arm = TilWord32.fromInt (case b of false => 0 | true => 1)
	    
	  val inject_exp = Prim_e (NilPrimOp (inject_known arm), [],[Var_c sum_var], [])
	  val forget_exp = ForgetKnown_e (Var_c sum_var,arm)
	    
	  val forgotten_exp  = Coerce_e(Var_e forget_var,[],Var_e inject_var)
	    
	  val bool_exp = Coerce_e (Var_e coercion_var, [], Var_e forgotten_var)
	    
	in  Let_e (Sequential,
		   bnds@
		   [
		    Exp_b(inject_var,TraceKnown TraceInfo.Trace,inject_exp),
		    Exp_b(forget_var,TraceKnown TraceInfo.Notrace_Int,forget_exp),
		    Exp_b(forgotten_var,TraceKnown TraceInfo.Trace,forgotten_exp),
		    Exp_b(bool_var,bool_tr,bool_exp)
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
