(*$import TORTL TORTLVARARG RTL PPRTL TORTLBASE RTLTAGS NIL NILUTIL PPNIL Stats *)

(* ASSUMPTIONS and GUARANTEES:
*)
	


functor TortlVararg(val number_flatten : int
		    structure Pprtl : PPRTL
		    structure TortlBase : TORTL_BASE 
		    structure Rtltags : RTLTAGS 
		    structure NilUtil : NILUTIL
		    structure Ppnil : PPNIL
		    sharing Pprtl.Rtltags = Rtltags)
    :> TORTL_VARARG where TortlBase = TortlBase
=
struct

structure TortlBase = TortlBase
open Nil
open TortlBase


val diag = ref true
val debug = ref false
val debug_full = ref false

   (* Module-level declarations *)

    structure Rtl = Rtl
    structure Nil = Nil
    open Util Listops
    open Nil
    open NilUtil
    open Rtl
    open Name
    open Rtltags 
    open Pprtl 
    open TortlBase


    val error = fn s => (Util.error "tortl-vararg.sml" s)

    val vararg_support = map0count (fn n => Name.fresh_named_var 
				    ("vararg_support_" ^ (Int.toString n))) (number_flatten + 1)
    val onearg_support = map0count (fn n => Name.fresh_named_var 
				    ("onearg_support_" ^ (Int.toString n))) (number_flatten + 1)
	
    fun xmake_vararg_support() = 
	let fun mapper(n,supportvar) = 
	    let val cenv_var = Name.fresh_named_var "vararg_support_cenv"
		val cenv_kind = kind_tuple(map0count (fn n => Type_k) (n+1))
		fun proj k = Proj_c(Var_c cenv_var, generate_tuple_label (k+1))
		val argc = con_tuple(map0count proj n)
		val resc = proj n
		val venv_var = Name.fresh_named_var "vararg_support_venv"
		val venv_type = AllArrow_c(Closure,Partial,[],[argc],0w0,resc)
		val expvars = map0count (fn n => Name.fresh_named_var 
					 ("vararg_support_expvar_" ^ (Int.toString n))) n
		val vclist = mapcount (fn (n,ev) => (ev,proj n)) expvars
		val arg = exp_tuple(map Var_e expvars)
		val body = App_e(Closure,Var_e venv_var,[],[arg],[])
		val call_function = Function(Partial,Arbitrary,
					      [(cenv_var,cenv_kind)],
					      vclist@[(venv_var,venv_type)],[],body,resc)

		val convars = map0count (fn n => Name.fresh_named_var 
					 ("vararg_support_convar_" ^ (Int.toString n))) (n+1)
		val vklist = map (fn v => (v,Type_k)) convars
		    
		val funvar = Name.fresh_named_var "vararg_support_funvar"
		val resvar = Name.fresh_named_var "vararg_support_resvar"
		val codevar = Name.fresh_named_var "vararg_support_codevar"
		val funcon = AllArrow_c(Closure,Partial,[],
					[con_tuple(map (Var_c o #1) (Listops.butlast vklist))], 0w0,
					Var_c(#1(List.last vklist)))
		val flat_funcon = AllArrow_c(Closure,Partial,[],
					     map (Var_c o #1) (Listops.butlast vklist), 0w0,
					     Var_c(#1(List.last vklist)))
		val vcl = (resvar,{code=codevar,venv=Var_e funvar,
				   cenv=con_tuple_inject(map Var_c convars),
				   tipe=flat_funcon})

		val body = Let_e(Sequential,
				 [Fixcode_b(Sequence.fromList[(codevar,call_function)]),
				  Fixclosure_b(false,Sequence.fromList[vcl])],
				 Var_e resvar)

		val function = Function(Partial,Arbitrary,vklist,[(funvar,funcon)],[],body,flat_funcon)
	    in  Fixcode_b(Sequence.fromList[(supportvar,function)])
	    end
	in  mapcount mapper vararg_support
	end

    fun xmake_onearg_support() = 
	let fun mapper(n,supportvar) = 
	    let val cenv_var = Name.fresh_named_var "onearg_support_cenv"
		val cenv_kind = kind_tuple(map0count (fn n => Type_k) (n+1))
		fun proj k = Proj_c(Var_c cenv_var, generate_tuple_label (k+1))
		val argcs = map0count proj n
		val argc = con_tuple argcs
		val resc = proj n
		val venv_var = Name.fresh_named_var "onearg_support_venv"
		val venv_type = AllArrow_c(Closure,Partial,[],argcs,0w0,resc)
		val expvar = Name.fresh_named_var "onearg_support_expvar"
		val args = map0count (fn n => Prim_e(NilPrimOp(select(generate_tuple_label (n+1))),
						     [], [Var_e expvar])) n
		val body = App_e(Closure,Var_e venv_var,[],args,[])
		val call_function = Function(Partial,Arbitrary,
					      [(cenv_var,cenv_kind)],
					      [(expvar,argc),
					       (venv_var,venv_type)],[],body,resc)

		val convars = map0count (fn n => Name.fresh_named_var 
					 ("onearg_support_convar_" ^ (Int.toString n))) (n+1)
		val vklist = map (fn v => (v,Type_k)) convars
		    
		val funvar = Name.fresh_named_var "onearg_support_funvar"
		val resvar = Name.fresh_named_var "onearg_support_resvar"
		val codevar = Name.fresh_named_var "onearg_support_codevar"
		val funcon = AllArrow_c(Closure,Partial,[],
					[con_tuple(map (Var_c o #1) (Listops.butlast vklist))], 0w0,
					Var_c(#1(List.last vklist)))
		val flat_funcon = AllArrow_c(Closure,Partial,[],
					     map (Var_c o #1) (Listops.butlast vklist), 0w0,
					     Var_c(#1(List.last vklist)))
		val vcl = (resvar,{code=codevar,venv=Var_e funvar,
				   cenv=con_tuple_inject(map Var_c convars),
				   tipe=funcon})

		val body = Let_e(Sequential,
				 [Fixcode_b(Sequence.fromList[(codevar,call_function)]),
				  Fixclosure_b(false,Sequence.fromList[vcl])],
				 Var_e resvar)

		val function = Function(Partial,Arbitrary,vklist,[(funvar,flat_funcon)],[],body,funcon)
	    in  Fixcode_b(Sequence.fromList[(supportvar,function)])
	    end
	in  mapcount mapper onearg_support
	end

    fun xmake_vararg xexp (state,argc,resc,function) = 
	let val noflattenl = alloc_code_label "noflatten"
	    val afterl = alloc_code_label "vararg_after"
	    val flattenl = alloc_code_label "flatten"
	    val recordlabs = map0count (fn n => alloc_code_label ("vararg_record" ^ (Int.toString n)))
					(number_flatten + 1)
	    val tmp = alloc_regi NOTRACE_INT
	    val desti = alloc_regi TRACE
	    val tagi = alloc_regi TRACE
	    val numfieldi = alloc_regi NOTRACE_INT
	    val _ = (add_instr(CMPUI(LE, argc, IMM 255, tmp)); (* check for small types *)
		     add_instr(BCNDI(NE, tmp, noflattenl, false));
		     add_instr(LOAD32I(EA(argc,0),tagi));      (* load tag of the type *)
		     add_instr(CMPUI(NE, tagi, IMM 5, tmp)); (* check for record *)
		     add_instr(BCNDI(NE, tmp, noflattenl, false)))

            (* dispatch to the right record case *)
	    val _ = add_instr(LOAD32I(EA(argc,4),numfieldi))      (* load record field number *)
	    fun mapper (n,recordlab) = (add_instr(CMPUI(EQ, numfieldi, IMM n, tmp)); (* check for record *)
					add_instr(BCNDI(NE, tmp, recordlab, false)))
	    val _ = mapcount mapper recordlabs
	    val _ = add_instr(BR noflattenl)
	    fun mapper (n,recordlab,supportvar) = 
		let val _ = add_instr(ILABEL recordlab)
		    val convars = map0count (fn n => Name.fresh_named_var "vararg_convar") n
		    val resconvar = Name.fresh_named_var "vararg_resconvar"
		    val funvar = Name.fresh_named_var "vararg_funvar"
		    fun folder(convar,(n,state)) = 
			let val tempi = alloc_regi TRACE
			    val _ = add_instr(LOAD32I(EA(argc,4*(2+n)),tempi))
			in  (n+1,
			     add_convar "vararg" (state,convar,Type_k,NONE,NONE,
						  SOME((VREGISTER (false,I tempi))), NONE))
			end
		    val (_,state) = foldl folder (0,state) convars
		    val state = add_convar "vararg" (state,resconvar,Type_k,NONE,NONE,
						     SOME((VREGISTER (false,I resc))), NONE)
		    val funcon = AllArrow_c(Closure,Partial,[],[con_tuple(map Var_c convars)],
					    0w0,Var_c resconvar)
		    val state = add_var  (state,funvar,funcon,
					  SOME((VREGISTER (false,I function))), NONE)
		    val e = App_e(Code,Var_e supportvar,(map Var_c convars) @ [Var_c resconvar],
				  [Var_e funvar],[])
		    val resulti = xexp(state,e)
		    val _ = add_instr(MV(resulti,desti))
		    val _ = add_instr(BR afterl)
		in  state
		end
	    val states = map2count mapper (recordlabs, vararg_support) 

            (* non-flattening case *)
	    val _ = (add_instr(ILABEL noflattenl);
		     add_instr(MV(function,desti)))
		
	    val _ = add_instr(ILABEL afterl)
	in  (join_states states, desti)
	end

    fun xmake_onearg xexp (state,argc,resc,function) = 
	let val noflattenl = alloc_code_label "noflatten"
	    val afterl = alloc_code_label "onearg_after"
	    val flattenl = alloc_code_label "flatten"
	    val recordlabs = map0count (fn n => alloc_code_label ("onearg_record" ^ (Int.toString n)))
					(number_flatten + 1)
	    val tmp = alloc_regi NOTRACE_INT
	    val desti = alloc_regi TRACE
	    val tagi = alloc_regi TRACE
	    val numfieldi = alloc_regi NOTRACE_INT
	    val _ = (add_instr(CMPUI(LE, argc, IMM 255, tmp)); (* check for small types *)
		     add_instr(BCNDI(NE, tmp, noflattenl, false));
		     add_instr(LOAD32I(EA(argc,0),tagi));      (* load tag of the type *)
		     add_instr(CMPUI(NE, tagi, IMM 5, tmp)); (* check for record *)
		     add_instr(BCNDI(NE, tmp, noflattenl, false)))

            (* dispatch to the right record case *)
	    val _ = add_instr(LOAD32I(EA(argc,4),numfieldi))      (* load record field number *)
	    fun mapper (n,recordlab) = (add_instr(CMPUI(EQ, numfieldi, IMM n, tmp)); (* check for record *)
					add_instr(BCNDI(NE, tmp, recordlab, false)))
	    val _ = mapcount mapper recordlabs
	    val _ = add_instr(BR noflattenl)
	    fun mapper (n,recordlab,supportvar) = 
		let val _ = add_instr(ILABEL recordlab)
		    val convars = map0count (fn n => Name.fresh_named_var "onearg_convar") n
		    val resconvar = Name.fresh_named_var "onearg_resconvar"
		    val funvar = Name.fresh_named_var "onearg_funvar"
		    fun folder(convar,(n,state)) = 
			let val tempi = alloc_regi TRACE
			    val _ = add_instr(LOAD32I(EA(argc,4*(2+n)),tempi))
			in  (n+1,
			     add_convar "onearg" (state,convar,Type_k,NONE,NONE,
						  SOME((VREGISTER (false,I tempi))), NONE))
			end
		    val (_,state) = foldl folder (0,state) convars
		    val state = add_convar "onearg" (state,resconvar,Type_k,NONE,NONE,
						     SOME((VREGISTER (false,I resc))), NONE)
		    val funcon = AllArrow_c(Closure,Partial,[],[con_tuple(map Var_c convars)],
					    0w0,Var_c resconvar)
		    val state = add_var  (state,funvar,funcon,
					  SOME((VREGISTER (false,I function))), NONE)
		    val e = App_e(Code,Var_e supportvar,(map Var_c convars) @ [Var_c resconvar],
				  [Var_e funvar],[])
		    val resulti = xexp(state,e)
		    val _ = add_instr(MV(resulti,desti))
		    val _ = add_instr(BR afterl)
		in  state
		end
	    val states = map2count mapper (recordlabs, onearg_support) 

            (* non-flattening case *)
	    val _ = (add_instr(ILABEL noflattenl);
		     add_instr(MV(function,desti)))
		
	    val _ = add_instr(ILABEL afterl)
	in  (join_states states, desti)
	end
end