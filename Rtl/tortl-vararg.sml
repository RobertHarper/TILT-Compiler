(*$import TORTLVARARG Rtl Pprtl TortlBase TortlRecord Rtltags Nil NilUtil Ppnil Stats *)

(* ASSUMPTIONS and GUARANTEES:
*)
	

structure TortlVararg :> TORTL_VARARG =

struct

structure TortlBase = TortlBase
open Nil
open TortlBase TortlRecord

val number_flatten = 6
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
		val venv_type = AllArrow_c{openness=Closure,effect=Partial,isDependent=false,
					   tFormals=[],eFormals=[(NONE,argc)],fFormals=0w0,
					   body_type=resc}
		val expvars = map0count (fn n => Name.fresh_named_var 
					 ("vararg_support_expvar_" ^ (Int.toString n))) n
		val vclist = mapcount (fn (n,ev) => (ev,proj n)) expvars
		val arg = exp_tuple(map Var_e expvars)
		val body = App_e(Closure,Var_e venv_var,[],[arg],[])
		val call_function = Function{effect=Partial, recursive=Arbitrary,isDependent=false,
					     tFormals=[(cenv_var,cenv_kind)],
					     eFormals=map (fn (v,c) => (v,TraceUnknown,c)) 
					                (vclist@[(venv_var,venv_type)]),
					     fFormals=[],
					     body=body, body_type=resc}

		val convars = map0count (fn n => Name.fresh_named_var 
					 ("vararg_support_convar_" ^ (Int.toString n))) (n+1)
		val vklist = map (fn v => (v,Type_k)) convars
		    
		val funvar = Name.fresh_named_var "vararg_support_funvar"
		val resvar = Name.fresh_named_var "vararg_support_resvar"
		val codevar = Name.fresh_named_var "vararg_support_codevar"
		val funcon = AllArrow_c{openness=Closure,effect=Partial,isDependent=false,
					tFormals=[],
					eFormals=[(NONE,
						   con_tuple(map (Var_c o #1) (Listops.butlast vklist)))],
					fFormals=0w0,
					body_type=Var_c(#1(List.last vklist))}
		val flat_funcon = AllArrow_c{openness=Closure,effect=Partial,isDependent=false,
					     tFormals=[],
					     eFormals=map (fn (v,_) => (NONE, Var_c v))
					                 (Listops.butlast vklist), 
					     fFormals=0w0,
					     body_type=Var_c(#1(List.last vklist))}
		val vcl = (resvar,{code=codevar,venv=Var_e funvar,
				   cenv=con_tuple_inject(map Var_c convars),
				   tipe=flat_funcon})

		val body = Let_e(Sequential,
				 [Fixcode_b(Sequence.fromList[(codevar,call_function)]),
				  Fixclosure_b(false,Sequence.fromList[vcl])],
				 Var_e resvar)

		val function = Function{effect=Partial,recursive=Arbitrary,isDependent=false,
					tFormals=vklist,
					eFormals=[(funvar,TraceKnown TraceInfo.Trace, funcon)],
					fFormals=[],
					body=body,
					body_type=flat_funcon}
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
		val venv_type = AllArrow_c{openness=Closure,effect=Partial,isDependent=false,
					   tFormals=[], 
					   eFormals=map (fn c => (NONE,c)) argcs, 
					   fFormals=0w0,
					   body_type=resc}
		val expvar = Name.fresh_named_var "onearg_support_expvar"
		val args = map0count (fn n => Prim_e(NilPrimOp(select(generate_tuple_label (n+1))),
						     [], [Var_e expvar])) n
		val body = App_e(Closure,Var_e venv_var,[],args,[])
		val call_function = Function{effect=Partial,recursive=Arbitrary,isDependent=false,
					     tFormals=[(cenv_var,cenv_kind)],
					     eFormals=[(expvar,TraceUnknown,argc),
						       (venv_var,TraceUnknown,venv_type)],
					     fFormals=[],
					     body=body,
					     body_type=resc}

		val convars = map0count (fn n => Name.fresh_named_var 
					 ("onearg_support_convar_" ^ (Int.toString n))) (n+1)
		val vklist = map (fn v => (v,Type_k)) convars
		    
		val funvar = Name.fresh_named_var "onearg_support_funvar"
		val resvar = Name.fresh_named_var "onearg_support_resvar"
		val codevar = Name.fresh_named_var "onearg_support_codevar"
		val funcon = AllArrow_c{openness=Closure, effect=Partial, isDependent=false,
					tFormals=[], 
					eFormals=[(NONE,
						   con_tuple(map (Var_c o #1) (Listops.butlast vklist)))], 
					fFormals=0w0,
					body_type=Var_c(#1(List.last vklist))}
		val flat_funcon = AllArrow_c{openness=Closure,effect=Partial, isDependent=false,
					     tFormals=[],
					     eFormals=map (fn (v,_) => (NONE, Var_c v)) 
					                  (Listops.butlast vklist), 
					     fFormals=0w0,
					     body_type=Var_c(#1(List.last vklist))}
		val vcl = (resvar,{code=codevar,venv=Var_e funvar,
				   cenv=con_tuple_inject(map Var_c convars),
				   tipe=funcon})

		val body = Let_e(Sequential,
				 [Fixcode_b(Sequence.fromList[(codevar,call_function)]),
				  Fixclosure_b(false,Sequence.fromList[vcl])],
				 Var_e resvar)

		val function = Function{effect=Partial,recursive=Arbitrary,isDependent=false,
					tFormals=vklist,
					eFormals=[(funvar,TraceKnown TraceInfo.Trace,flat_funcon)],
					fFormals=[],
					body=body,
					body_type=funcon}
	    in  Fixcode_b(Sequence.fromList[(supportvar,function)])
	    end
	in  mapcount mapper onearg_support
	end

    fun xmake_vararg xexp (state,argc,resc,function) = 
	let val noflattenl = fresh_code_label "noflatten"
	    val afterl = fresh_code_label "vararg_after"
	    val flattenl = fresh_code_label "flatten"
	    val recordlabs = map0count (fn n => fresh_code_label ("vararg_record" ^ (Int.toString n)))
					(number_flatten + 1)
	    val tmp = alloc_regi NOTRACE_INT
	    val desti = alloc_regi TRACE
	    val tagi = alloc_regi TRACE
	    val numfieldi = alloc_regi NOTRACE_INT
	    val _ = (add_instr(BCNDI(LE, argc, IMM 255, noflattenl, false)); (* check for small types *)
		     record_project(argc,0,tagi);                            (* load tag of the type *)
		     add_instr(BCNDI(NE, tagi, IMM 5, noflattenl, false)))   (* check for record *)

            (* dispatch to the right record case *)
	    val _ = record_project(argc,1,numfieldi)                     (* load record field number *)
	    fun mapper (n,recordlab) = 
		add_instr(BCNDI(EQ, numfieldi, IMM n, recordlab, false)) (* check for records *)
	    val _ = mapcount mapper recordlabs
	    val _ = add_instr(BR noflattenl)
	    fun mapper (n,recordlab,supportvar) = 
		let val _ = add_instr(ILABEL recordlab)
		    val convars = map0count (fn n => Name.fresh_named_var "vararg_convar") n
		    val resconvar = Name.fresh_named_var "vararg_resconvar"
		    val funvar = Name.fresh_named_var "vararg_funvar"
		    fun folder(convar,(n,state)) = 
			let val tempi = alloc_regi TRACE
			    val _ = record_project(argc,2+n,tempi)
			in  (n+1,
			     add_conterm (state,convar,Type_k,
					  SOME(LOCATION(REGISTER (false,I tempi)))))
			end
		    val (_,state) = foldl folder (0,state) convars
		    val state = add_conterm (state,resconvar,Type_k,
					     SOME(LOCATION(REGISTER (false,I resc))))
		    val funcon = AllArrow_c{openness=Closure, effect=Partial, isDependent=false,
					    tFormals=[], 
					    eFormals=[(NONE,con_tuple(map Var_c convars))],
					    fFormals=0w0, body_type=Var_c resconvar}
		    val state = add_term  (state,funvar,funcon, 
					   LOCATION(REGISTER (false,I function)), NONE)
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
	let val noflattenl = fresh_code_label "noflatten"
	    val afterl = fresh_code_label "onearg_after"
	    val flattenl = fresh_code_label "flatten"
	    val recordlabs = map0count (fn n => fresh_code_label ("onearg_record" ^ (Int.toString n)))
					(number_flatten + 1)
	    val tmp = alloc_regi NOTRACE_INT
	    val desti = alloc_regi TRACE
	    val tagi = alloc_regi TRACE
	    val numfieldi = alloc_regi NOTRACE_INT
	    val _ = (add_instr(BCNDI(LE, argc, IMM 255, noflattenl, false)); (* check for small types *)
		     record_project(argc,0,tagi);                           (* load tag of the type *)
		     add_instr(BCNDI(NE, tagi, IMM 5, noflattenl, false))) (* check for record *)

            (* dispatch to the right record case *)
	    val _ = record_project(argc,1,numfieldi)      (* load record field number *)
	    fun mapper (n,recordlab) = 
		add_instr(BCNDI(EQ, numfieldi, IMM n, recordlab, false)) (* check for record *)

	    val _ = mapcount mapper recordlabs
	    val _ = add_instr(BR noflattenl)
	    fun mapper (n,recordlab,supportvar) = 
		let val _ = add_instr(ILABEL recordlab)
		    val convars = map0count (fn n => Name.fresh_named_var "onearg_convar") n
		    val resconvar = Name.fresh_named_var "onearg_resconvar"
		    val funvar = Name.fresh_named_var "onearg_funvar"
		    fun folder(convar,(n,state)) = 
			let val tempi = alloc_regi TRACE
			    val _ = record_project(argc,2+n,tempi)
			in  (n+1,
			     add_conterm (state,convar,Type_k,
					  SOME(LOCATION(REGISTER (false,I tempi)))))
			end
		    val (_,state) = foldl folder (0,state) convars
		    val state = add_conterm (state,resconvar,Type_k,
					     SOME(LOCATION(REGISTER (false,I resc))))
		    val funcon = AllArrow_c{openness=Closure, effect=Partial, isDependent=false,
					    tFormals=[], 
					    eFormals=[(NONE,con_tuple(map Var_c convars))],
					    fFormals=0w0, body_type=Var_c resconvar}
		    val newfuncon = AllArrow_c{openness=Closure, effect=Partial, isDependent=false,
					       tFormals=[], 
					       eFormals=map (fn v => (NONE,Var_c v)) convars,
					       fFormals=0w0, body_type=Var_c resconvar}
		    val targs = (map Var_c convars) @ [Var_c resconvar]
		    val supportcon = AllArrow_c{openness=Code, effect=Total, isDependent=false,
						tFormals = map (fn _ => (fresh_var(), Type_k)) targs,
						fFormals = 0w0, 
						eFormals = [(NONE, funcon)],
						body_type = newfuncon}
		    val state = add_term (state,funvar,funcon,
					  LOCATION(REGISTER (false,I function)), NONE)
		    val e = App_e(Code,Var_e supportvar, targs, [Var_e funvar],[])
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