(*$import Prelude TopLevel String Symbol Int List Util Listops Tyvar Il IlStatic IlUtil IlContext Ppil Name Ast GraphUtil ListMergeSort AstHelp DATATYPE Stats *)

(* Datatype compiler and destructures of datatype signatures. *)
structure Datatype
    :> DATATYPE =
  struct

    structure IlContext = IlContext
    open Il IlStatic IlUtil Ppil 
    open Util Listops Name Tyvar
    open IlContext

    val do_inline = Stats.tt("DatatypeInline")
    val debug = Stats.ff("DatatypeDebug")

    val error = fn s => error "datatype.sml" s
    val error_sig = fn signat => fn s => error_sig "datatype.sml" signat s 
    fun debugdo t = if (!debug) then (t(); ()) else ()

	
    fun geq_string (s1,s2) = 
	(case (String.compare(s1,s2)) of
	     GREATER => true
	   | EQUAL => true
	   | LESS => false)
    fun geq_sym(sym1,sym2) = 
	geq_string(Symbol.symbolToString sym1, Symbol.symbolToString sym2)

	     
    fun con_fun(args,body) = 
	let val args' = map Name.derived_var args
	    fun folder ((v,v'),s) = subst_add_convar(s,v,CON_VAR v')
	    val subst = foldl folder empty_subst (zip args args')
	in  CON_FUN(args', con_subst(body,subst))
	end

    (* ------------------------------------------------------------------
      The datatype compiler for compiling a single strongly-connected type.
      ------------------------------------------------------------------ *)
    fun driver (xty : Il.context * Ast.ty -> Il.con,
		context : context, 
		std_list : (Symbol.symbol * Ast.tyvar list * (Symbol.symbol * Ast.ty option) list) list,
		eqcomp : Il.context * Il.con -> (Il.exp * Il.con) option) : (sbnd * sdec) list = 
      let 
        (* ---- tyvar_vars are the polymorphic type arguments for constructor functions
	   ---- tyvar_labs name the type components when they are strucutre components
	 *)
	val tyvar_labs = flatten(map (fn (_,tyvars,_) => 
				      map (symbol_label o AstHelp.tyvar_strip) tyvars) std_list)
	val tyvar_labs = foldr (fn (l,acc) => if (Listops.member_eq(eq_label,l,acc))
						  then acc else l::acc) [] tyvar_labs
	val num_tyvar = length tyvar_labs
	val tyvar_vars = map (fn lab => fresh_named_var "poly") tyvar_labs
	val tyvar_cons = map CON_VAR tyvar_vars
	val mpoly_var = fresh_named_var "mpoly_var"
	val is_monomorphic = num_tyvar = 0
	val num_datatype = length std_list

	(* ----- create names for overall recursive type, datatype types,
	         argument to sum types, sum types, special sums, and modules *)
	val type_syms = map #1 std_list
	val type_labs = map symbol_label type_syms
	val type_vars = map (fn s => fresh_named_var (Symbol.name s)) type_syms
	val eq_labs = map (fn s => symbol_label(Symbol.varSymbol(Symbol.name s ^ "_eq"))) type_syms
	val eq_vars = map (fn s => fresh_named_var (Symbol.name s ^ "_eq")) type_syms
	val inner_type_labvars = map (fn s => 
				   let val str = Symbol.name s
				   in  (internal_label str,
					fresh_named_var ("copy_" ^ str))
				   end) type_syms
	val top_type_string = foldl (fn (s,acc) => acc ^ "_" ^ (Symbol.name s)) "" type_syms
	val top_eq_string = top_type_string ^ "_eq"
	val top_type_var = if (num_datatype = 1) then (hd type_vars) else fresh_named_var top_type_string
	val top_type_lab = to_questionable (internal_label top_type_string)
	val top_eq_var = fresh_named_var top_eq_string
	val top_eq_lab = symbol_label (Symbol.tycSymbol top_eq_string)
	val module_labvars = map (fn s => let val str = Symbol.name s
					  in  (IlUtil.to_dt (symbol_label s),
					       fresh_named_var str)
					  end) type_syms
	val constr_sumarg_strings = map (fn s => (Symbol.name s) ^ "_sumarg") type_syms
	val constr_sumarg_vars = map fresh_named_var constr_sumarg_strings
	val constr_sumarg_labs = map internal_label constr_sumarg_strings
	val constr_sum_strings = map (fn s => (Symbol.name s) ^ "_sum") type_syms
	val constr_sum_vars = map fresh_named_var constr_sum_strings
	val constr_sum_labs = map internal_label constr_sum_strings
	val constr_ssum_strings = 
	    let fun mapper type_sym (n,_) = ((Symbol.name type_sym) ^ "_sum" ^ (Int.toString n))
	    in  map2 (fn (type_sym, (_,_,tys)) => mapcount (mapper type_sym) tys) (type_syms, std_list)
	    end
	val constr_ssum_vars = mapmap fresh_named_var constr_ssum_strings
	val constr_ssum_labs = mapmap internal_label constr_ssum_strings
	val constr_tys : Ast.ty option list list = map (fn (_,_,def) => map #2 def) std_list
        val constr_syms = map (fn (_,_,def) => map #1 def) std_list
	val constr_labs = mapmap symbol_label constr_syms
	val constr_con_strings = map3 (fn (ty_sym,con_syms,con_tys) => 
				       map2 (fn (s,NONE) => NONE
					      | (s,SOME _) => SOME((Symbol.name ty_sym) ^ "_" ^ 
							       (Symbol.name s)))
				       (con_syms, con_tys))
	                         (type_syms,constr_syms,constr_tys)
	val constr_con_vars = mapmap (Util.mapopt fresh_named_var) constr_con_strings
	val constr_con_labs = mapmap (Util.mapopt internal_label) constr_con_strings


        (* -- we name the variables representing the instantited fixpoints in CON_MU *)
	val vardt_list = map (fn s => (fresh_named_var ("vdt" ^ "_" ^ (Symbol.name s)))) type_syms


	(* ---- compute sig_poly, sig_poly+, and a context context' -----------
	   ---- that has the polymorphic type variables and datatypes bound --- *)
	local
	    fun folder ((tv,v), (sdecs, sdecs_eq, ctxt)) = 
		let val eq_label = to_eq tv
		    val eq_con = con_eqfun(CON_VAR v)
		    val sdec = SDEC(tv,DEC_CON(v,KIND,NONE,false))
		    val sdec_eq = SDEC(eq_label,DEC_EXP(fresh_var(),eq_con,NONE,false))
		    val ctxt = add_context_con(ctxt,tv,v,KIND,NONE)
		in  (sdec :: sdecs, sdec :: sdec_eq :: sdecs_eq, ctxt)
		end
	    val (sdecs, sdecs_eq, ctxt) = foldr folder ([],[],context) (Listops.zip tyvar_labs tyvar_vars)
	    fun folder ((tc,vty),ctxt) = 
		let val k = if (num_tyvar = 0) then KIND else KIND_ARROW(num_tyvar,KIND)
		in  add_context_con(ctxt,tc,vty,k,NONE)
		end
	in
	    val sdecs_eq = sdecs_eq
	    val sigpoly = SIGNAT_STRUCTURE sdecs
	    val sigpoly_eq = SIGNAT_STRUCTURE sdecs_eq
	    val context' = foldl folder ctxt (Listops.zip type_labs type_vars)
	end

	(* ------ compute 3 versions of all the types carried by the constructors 
	 *)
	local
	    fun tyvar_type_mapper ty = xty(context',ty)

	    fun folder ((v,c),s) = subst_add_convar(s,v,c)
	    fun vclist2subst vclist = foldl folder empty_subst vclist

	    val list_tyvar2vdt = zip type_vars (map CON_VAR vardt_list)
	    val subst_tyvar2vdt = vclist2subst list_tyvar2vdt
	    val subst_tyvar2mproj = vclist2subst (map2 (fn (l,v) => (v, CON_MODULE_PROJECT(MOD_VAR mpoly_var,l)))
						  (tyvar_labs, tyvar_vars))

	    fun conapper(CON_VAR tarv,cargs) : con option = assoc_eq(eq_var,tarv,list_tyvar2vdt)
	      | conapper _ = NONE

	in
	    fun vdt_mapper c = 
		if is_monomorphic
		    then con_subst(c,subst_tyvar2vdt)
		else con_subst_conapps(c,conapper)

	    fun mproj_type_mapper c = con_subst(c,subst_tyvar2mproj)
	    val constr_tyvar_type = mapmap (Util.mapopt tyvar_type_mapper) constr_tys
	    val constr_mproj_type = mapmap (Util.mapopt mproj_type_mapper) constr_tyvar_type
	    val constr_vdt  = mapmap (Util.mapopt vdt_mapper)  constr_tyvar_type
	end

        (* ------------ now create the sum arg and sum types which use type and tyvar *)
	local
	    fun conopts_split (nca,ca) ([] : 'a option list) = (nca,rev ca)
	      | conopts_split (nca,ca) (NONE::rest) = conopts_split (nca+1,ca) rest
	      | conopts_split (nca,ca) ((SOME c)::rest) = conopts_split (nca,c::ca) rest
	    val conopts_split = fn arg => conopts_split (0,[]) arg
	    fun mapper (conopts,conopts_vdt,constr_sumarg_var,names) = 
		let val (nca,ca) = conopts_split conopts
		    val (_,ca_vdt) = conopts_split conopts_vdt
		    val (sumarg, sumarg_kind) = 
			(case ca of
			     [ca1] => (ca1, KIND)
			   | _ => (CON_TUPLE_INJECT ca, KIND_TUPLE (length ca)))
		    val sumarg_vdt = (case ca_vdt of
					  [ca_vdt1] => ca_vdt1
					| _ => CON_TUPLE_INJECT ca_vdt)
		    val carrier = if is_monomorphic
				      then CON_VAR constr_sumarg_var
				  else CON_APP(CON_VAR constr_sumarg_var, tyvar_cons)
		in (sumarg, sumarg_kind,
		    CON_SUM{names=names,
			    noncarriers=nca,
			    carrier=sumarg_vdt,
			    special=NONE},
		    CON_SUM{names=names,
			    noncarriers=nca,
			    carrier=carrier,
			    special=NONE})
		end
	in
	    val conopts_split = conopts_split
	    val (constr_sumarg,constr_sumarg_kind,constr_fullsum_vdt,constr_sum) = 
		unzip4 (map4 mapper (constr_tyvar_type,constr_vdt,
				     constr_sumarg_vars,constr_labs))
	end

        (* ---------- now create the top datatype tuple using var_poly ------ *)
	val top_type_tyvar = 
	    CON_MU(CON_FUN(vardt_list,con_tuple_inject constr_fullsum_vdt))

        (* ------------ create the polymorphic type arguments --------------------- 
	   ------------- and instanitated versions of datatypes ------------------- *)
	val tyvar_mprojs = map (fn l => CON_MODULE_PROJECT (MOD_VAR mpoly_var, l)) tyvar_labs
	val type_cinsts = map (fn tv => if (is_monomorphic) 
					    then (CON_VAR tv)
					else CON_APP(CON_VAR tv, tyvar_cons)) type_vars
	val type_minsts = map (fn tv => if (is_monomorphic) 
					    then (CON_VAR tv)
					else CON_APP(CON_VAR tv, tyvar_mprojs)) type_vars



	(* ----------------- compute the constructors ------------- *)
	local 
	    fun mk_help (type_var_i : var,
			 constr_sum_var_i : var,
			 constr_mproj_type_i : con option list) =
		let 
		    val mutype = if is_monomorphic
				     then CON_VAR type_var_i
				 else ConApply(true, CON_VAR type_var_i, tyvar_mprojs)
		    val sumtype = if (is_monomorphic)
				      then CON_VAR constr_sum_var_i
				  else ConApply(true, CON_VAR constr_sum_var_i, tyvar_mprojs)
		    fun help (j, constr_mproj_type_ij_opt) =
			let val var = fresh_named_var "injectee"
			in  case constr_mproj_type_ij_opt of
			    NONE => (ROLL(mutype, INJ{sumtype = sumtype,
						      field = j,
						      inject = NONE}), mutype)
			  | SOME constr_mproj_type_ij =>
				(make_total_lambda
				 (var, constr_mproj_type_ij, mutype,
				  ROLL(mutype,
				       INJ{sumtype = sumtype,
					   field = j,
					   inject = SOME (VAR var)})))
			end
		in mapcount help constr_mproj_type_i
		end
	in val exp_con_mk = map3 mk_help (type_vars, constr_sum_vars, constr_mproj_type)
	end

	
	(* ----------------- compute the exposes ------------------- *)
	local 
	    fun expose_help (type_minst,constr_sum_var_i) =
		let val expose_var = fresh_named_var "exposee"
		    val sumtype = CON_VAR constr_sum_var_i
		    val sumtype = if is_monomorphic
				      then sumtype
				  else ConApply(true ,sumtype, tyvar_mprojs)
		in  make_total_lambda(expose_var,type_minst,sumtype,
				      UNROLL(type_minst,sumtype,VAR expose_var))
		end
	in val exp_con_expose = map2 expose_help (type_minsts, constr_sum_vars)
	end

	(* ----------------- compute the type bindings ------------------- *)		
	val top_type_sbnd_sdec = 
		let val (c,base_kind) = 
		    if is_monomorphic
			then (top_type_tyvar, KIND_TUPLE num_datatype)
		    else (con_fun(tyvar_vars, top_type_tyvar), 
			  KIND_ARROW(num_tyvar, KIND_TUPLE num_datatype))
		in  if num_datatype = 1 
			then []
		     else [(SBND(top_type_lab, BND_CON(top_type_var, c)),
		            SDEC(top_type_lab, DEC_CON(top_type_var, base_kind, SOME c, false)))]
		end


	val type_sbnd_sdecs = 
		let val kind = KIND
		    val kind = if is_monomorphic 
				   then kind
			       else KIND_ARROW(num_tyvar,kind)
		    fun mapper(i,l,v) =
			let val c = if num_datatype = 1 
					then CON_TUPLE_PROJECT(0,top_type_tyvar)
				    else 
			              let val c = CON_VAR top_type_var
					  val c = if is_monomorphic then c
						  else CON_APP(c, tyvar_cons)
				      in  CON_TUPLE_PROJECT(i,c)
				      end
			    val c = if is_monomorphic then c 
				      else con_fun(tyvar_vars, c)
			in  (SBND(l,BND_CON(v,c)), 
			     SDEC(l,DEC_CON(v,kind,SOME c, false)))
			end
		in  map2count mapper (type_labs,type_vars)
		end

	(* ----------------- compute the equality function and reduced sdecs_eq and sigpoly_eq  ------------------- *)
	local
	    val var_poly_dec = DEC_MOD(mpoly_var,false,
				       SelfifySig context (PATH(mpoly_var,[]),sigpoly_eq))
	    val ctxt = add_context_dec(context, var_poly_dec)
	    val ctxt = add_context_entries(ctxt, map (CONTEXT_SDEC o #2) 
						(top_type_sbnd_sdec @ type_sbnd_sdecs))
	    val eq_con = if is_monomorphic 
			     then CON_VAR top_type_var
			 else CON_APP(CON_VAR top_type_var, tyvar_mprojs)
				       (*  top_type_tyvar *)
	in
	    val eq_exp_con = eqcomp(ctxt, eq_con)
	    val (sdecs_eq, sigpoly_eq) =
		if is_monomorphic orelse not (isSome eq_exp_con)
		    then (sdecs_eq, sigpoly_eq)
		else
		    let val (eq_exp,_) = valOf eq_exp_con
			val vpath = (mpoly_var, [])
			val sdecs_eq' = reduce_typearg_sdecs (eq_exp, vpath, sdecs_eq)
			val sigpoly_eq' = SIGNAT_STRUCTURE sdecs_eq'
		    in  (sdecs_eq', sigpoly_eq')
		    end
	end



	(* --------- create the inner modules ------------- *)
	fun help ((inner_type_lab_i, inner_type_var_i),type_var_i,
		  (module_lab, module_var),
		  (exp_expose_i,con_expose_i),
		  constr_labs_row,
		  expcon_mk_i) = 
	  let 
	    (* ----- do the type -------- *)
	    val k = KIND
	    val k = if is_monomorphic then k else KIND_ARROW(num_tyvar,k)
	    val type_sbnd = SBND(inner_type_lab_i, BND_CON(inner_type_var_i,CON_VAR type_var_i))
	    val type_sdec = SDEC(inner_type_lab_i, DEC_CON(inner_type_var_i,k,SOME(CON_VAR type_var_i),true))

	    (* ----- do the expose -------- *)
	    val expose_var = fresh_named_var "exposer"
	    val expose_modvar = fresh_named_var "exposer_mod"
	    val expose_expbnd = BND_EXP(expose_var,exp_expose_i)
	    val expose_expdec = DEC_EXP(expose_var,con_expose_i,SOME exp_expose_i, true)
	    val expose_inner_sig = SIGNAT_STRUCTURE [SDEC(it_lab,expose_expdec)]
	    val expose_modbnd = BND_MOD(expose_modvar, true,
					MOD_FUNCTOR(TOTAL, mpoly_var,sigpoly,
						    MOD_STRUCTURE[SBND(it_lab,expose_expbnd)],
						    expose_inner_sig))
	    val expose_moddec = DEC_MOD(expose_modvar, true,
					SIGNAT_FUNCTOR(mpoly_var,sigpoly,
						       expose_inner_sig,TOTAL))
	    val expose_sbnd = SBND(expose_lab,if is_monomorphic 
						  then expose_expbnd else expose_modbnd)
	    val expose_sdec = SDEC(expose_lab,if is_monomorphic
						  then expose_expdec else expose_moddec)


	    (* ----- do the mk  ---- *)
	    fun inner_help (constr_lab_ij,(exp_mk_ij,con_mk_ij)) =
	      let 
		  val mk_var = fresh_var()
		  val mkpoly_var = fresh_var()
		  val bnd = BND_EXP(mk_var, exp_mk_ij)
		  val dec = DEC_EXP(mk_var, con_mk_ij, SOME exp_mk_ij, true)
		  val inner_sig = SIGNAT_STRUCTURE [SDEC(it_lab,dec)]
		  val modbnd = BND_MOD(mkpoly_var, true,
					   MOD_FUNCTOR(TOTAL, mpoly_var,sigpoly,
						       MOD_STRUCTURE[SBND(it_lab,bnd)],
						       inner_sig))
		  val moddec = DEC_MOD(mkpoly_var, true,
				       SIGNAT_FUNCTOR(mpoly_var,sigpoly,
						      inner_sig,TOTAL))
	      in  (SBND(constr_lab_ij, if is_monomorphic then bnd else modbnd),
		   SDEC(constr_lab_ij, if is_monomorphic then dec else moddec))
	      end
	    val temp = map2 inner_help (constr_labs_row, expcon_mk_i)
	    val (sbnds,sdecs) = (map #1 temp, map #2 temp)

	    val sbnds = type_sbnd::expose_sbnd::sbnds
	    val sdecs = type_sdec::expose_sdec::sdecs

	    val inner_mod = MOD_STRUCTURE sbnds
	    val inner_sig = SIGNAT_STRUCTURE sdecs

	  in (SBND(module_lab,BND_MOD(module_var,false,inner_mod)),
	      SDEC(module_lab,DEC_MOD(module_var,false,inner_sig)))
	  end


	val components = map6 help (inner_type_labvars, 
				    type_vars,
				    module_labvars,
				    exp_con_expose,
				    constr_labs,
				    exp_con_mk)


	local
	    fun help (top_eq_exp,top_eq_con) (i,type_lab_i,type_var_i) =
		let 
		    val eq_lab = to_eq type_lab_i
		    val equal_var = fresh_named_var (label2string eq_lab)
		    val bnd_var = fresh_named_var ("poly" ^ (label2string eq_lab))
		    val short_top_eq_exp = 
			if (is_monomorphic)
			    then VAR top_eq_var
			else MODULE_PROJECT(MOD_APP(MOD_VAR top_eq_var, MOD_VAR mpoly_var), it_lab)
		    val exp_eq = if num_datatype = 1
				     then top_eq_exp
				 else RECORD_PROJECT(short_top_eq_exp,
						     generate_tuple_label(i+1), top_eq_con)
		    val con_eq = con_eqfun(if is_monomorphic 
					       then CON_VAR type_var_i
					   else CON_APP (CON_VAR type_var_i, tyvar_mprojs))
		    val eq_expbnd = BND_EXP(equal_var,exp_eq)
		    val eq_expdec = DEC_EXP(equal_var,con_eq,NONE,false)
		    val eq_inner_sig = SIGNAT_STRUCTURE [SDEC(it_lab,eq_expdec)]
		    val eq_sbnd = 
			SBND((eq_lab,
			      if (is_monomorphic)
				  then eq_expbnd
			      else
				  BND_MOD(bnd_var, true,
					  MOD_FUNCTOR(TOTAL, mpoly_var, sigpoly_eq,
						      MOD_STRUCTURE[SBND(it_lab,eq_expbnd)],
						      eq_inner_sig))))
		    val eq_sdec = 
			SDEC(eq_lab,
			     if (is_monomorphic)
				 then eq_expdec
			     else DEC_MOD(bnd_var, true,
					  SIGNAT_FUNCTOR(mpoly_var,sigpoly_eq,
							 eq_inner_sig,TOTAL)))
		in (eq_sbnd, eq_sdec)
		end
	in val eq_sbnd_sdecs = (case eq_exp_con of
				    NONE => []
				  | SOME ec => map2count (help ec) (type_labs,type_vars))
	end


	val top_eq_sbnd_sdec = 
	    (case (eq_exp_con, num_datatype) of
		(NONE,_) => []
	      | (_, 1) => []
	      | (SOME (eq_exp, eq_con),_) =>
		let val equal_var = if is_monomorphic then top_eq_var 
				    else fresh_named_var "top_eq"
		    val expbnd = BND_EXP(equal_var, eq_exp)
		    val expdec = DEC_EXP(equal_var, eq_con, NONE,false)
		    val inner_sig = SIGNAT_STRUCTURE [SDEC(it_lab,expdec)]
		    val modbnd = BND_MOD(top_eq_var, true, MOD_FUNCTOR(TOTAL, mpoly_var,sigpoly_eq,
							 MOD_STRUCTURE[SBND(it_lab,expbnd)],
							 inner_sig))
		    val moddec = DEC_MOD(top_eq_var, true, SIGNAT_FUNCTOR(mpoly_var,sigpoly_eq,
								    inner_sig, TOTAL))
		    val bnd = if is_monomorphic then expbnd else modbnd
		    val dec = if is_monomorphic then expdec else moddec
		in  [(SBND(top_eq_lab, bnd), SDEC(top_eq_lab, dec))]
		end)

	fun mapper(constr_sumarg_lab_i,
		   constr_sumarg_var_i, 
		   constr_sumarg_i,
		   constr_sumarg_kind_i) = 
	    let val k = constr_sumarg_kind_i
		val c = if is_monomorphic
			    then constr_sumarg_i
			else con_fun(tyvar_vars, constr_sumarg_i)
	    in  (SBND(constr_sumarg_lab_i,BND_CON(constr_sumarg_var_i, c)),
		 SDEC(constr_sumarg_lab_i,DEC_CON(constr_sumarg_var_i, k, SOME c, false)))
	    end

	val constr_sumarg_sbnd_sdecs = map4 mapper
	    (constr_sumarg_labs, constr_sumarg_vars, constr_sumarg, constr_sumarg_kind)

	fun mapper(constr_sum_lab_i, constr_sum_var_i, constr_rf_sum_i) = 
	    let val (c,k) = if is_monomorphic 
				then (constr_rf_sum_i, KIND)
			    else (con_fun(tyvar_vars,constr_rf_sum_i), 
				  KIND_ARROW(num_tyvar,KIND))
	    in  (SBND(constr_sum_lab_i,BND_CON(constr_sum_var_i, c)),
		 SDEC(constr_sum_lab_i,DEC_CON(constr_sum_var_i, k, SOME c, false)))
	    end

	val constr_sum_sbnd_sdecs = map3 mapper (constr_sum_labs,constr_sum_vars, constr_sum)

	val final_sbnd_sdecs = (top_type_sbnd_sdec
				@ top_eq_sbnd_sdec
				@ type_sbnd_sdecs 
				@ constr_sumarg_sbnd_sdecs
				@ constr_sum_sbnd_sdecs
				@ eq_sbnd_sdecs 
				@ components)
      in  final_sbnd_sdecs
      end

 
    (* ------------------------------------------------------------------
      The datatype compiler for compiling a single datatype statement.
      ------------------------------------------------------------------ *)
    type node = int * (Symbol.symbol * Ast.tyvar list * (Ast.symbol * Ast.ty option) list)
    fun compile' (context, typecompile,
		  nodes : node list, eq_compile) : (sbnd * sdec) list =
      let 
	(* ---- Find the strongly-connected components of datatypes. *)
	local
	  val syms = map (fn (_,(s,_,_)) => s) nodes
	  fun help (_,NONE) = []
	    | help (_,SOME ty) = let val s = AstHelp.free_tyc_ty(ty,fn _ => false)
				 in list_inter_eq(Symbol.eq,s,syms)
				 end
	  fun lookupint [] tari = error "lookupint should not fail"
	    | lookupint ((i,info)::rest) tari = if (i = tari) then info
						else lookupint rest tari
	  fun lookup [] tars = error "lookup should not fail"
	    | lookup ((i,(s,tv,def))::rest) tars = if (Symbol.eq(s,tars)) then i
						   else lookup rest tars
	in 
	  val numnodes = length nodes
	  fun get_edges (i,(s,tyvars,def)) = (map (fn x => (i,x))
					      (map (lookup nodes) (flatten (map help def))))
	  val edges = flatten (map get_edges nodes)
	  val comps = rev(GraphUtil.scc numnodes edges)
	  val sym_tyvar_def_listlist = mapmap (lookupint nodes) comps
	end (* local *)
        (* ---- call the main routine for each sorted list of datatypes 
	   and retain the accumulated context *)
	fun loop context acc [] = rev acc
	  | loop context acc (std_list::rest) = 
	    let fun geq_symrest((s1,_),(s2,_)) = geq_sym(s1,s2)
		val sort_symrest = ListMergeSort.sort geq_symrest
		fun sort_std' (ncs,cs) [] = (sort_symrest ncs) @ (sort_symrest cs)
		  | sort_std' (ncs,cs) ((nc as (_,NONE))::rest) = sort_std' (nc::ncs,cs) rest
		  | sort_std' (ncs,cs) ((c as (_,SOME _))::rest) = sort_std' (ncs,c::cs) rest
		fun sort_std (s,tvs,st_list) = (s,tvs,sort_std' ([],[]) st_list)
		val std_list = map sort_std std_list
		fun geq_std((s1,_,_),(s2,_,_)) = geq_sym(s1,s2)
		val std_list = ListMergeSort.sort geq_std std_list
		val sbnd_sdecs = driver(typecompile,context,
					std_list, eq_compile)
		val _ = if (!debug)
			    then (print "DRIVER returned SBNDS = ";
				  map (fn (sb,_) => (Ppil.pp_sbnd sb; print "\n")) sbnd_sdecs;
				  print "   and returned SDECS = ";
				  map (fn (_,sd) => (Ppil.pp_sdec sd; print "\n")) sbnd_sdecs;
				  print "\n\n")
			else ()
		val sdecs = map (fn (_,SDEC(l,dec)) => SDEC(l,SelfifyDec context dec)) sbnd_sdecs
		val context' = add_context_sdecs(context,sdecs)
		val acc' = (rev sbnd_sdecs) @ acc
	    in loop context' acc' rest
	    end
	val res = loop context [] sym_tyvar_def_listlist
      in res
      end

    fun copy_datatype(context,path,tyc) = 
	let val old_type_sym = List.last path
	    val type_sym = tyc
	    val type_lab = symbol_label tyc
	    val type_var = gen_var_from_symbol tyc
	    val type_string = (Symbol.name type_sym)
	    val constr_sum_string = type_string ^ "_sum"
	    val constr_sum_var = fresh_named_var constr_sum_string
	    val constr_sum_lab = internal_label constr_sum_string
	    val old_constr_sum_string = (Symbol.name old_type_sym) ^ "_sum"
	    val old_constr_sum_lab = internal_label old_constr_sum_string
	    val constr_sumarg_string = (Symbol.name type_sym) ^ "_sumarg"
	    val constr_sumarg_var = fresh_named_var constr_sumarg_string
	    val constr_sumarg_lab = internal_label constr_sumarg_string
	    val old_constr_sumarg_string = (Symbol.name old_type_sym) ^ "_sumarg"
	    val old_constr_sumarg_lab = internal_label old_constr_sumarg_string

	    val eq_lab = to_eq type_lab 
	    val eq_var = fresh_named_var "eqfun"
	    val dt_lab = to_dt type_lab
	    val dt_var = fresh_named_var "dt"
	    fun change_path [] _ = error "empty path"
	      | change_path [l] base = [base l]
	      | change_path (a::b) base = a :: change_path b base
	    val change_path = change_path (map symbol_label path)
	    val type_labs = change_path (fn l => l)
	    val eq_labs = change_path to_eq
	    val dt_labs = change_path to_dt
	    val constr_sum_labs = change_path (fn _ => old_constr_sum_lab)
	    val constr_sumarg_labs = change_path (fn _ => old_constr_sumarg_lab)

	    val eq_sbndsdec = 
		(case (Context_Lookup_Labels(context,eq_labs)) of
		     SOME(_,PHRASE_CLASS_EXP (e,c,_,_)) => 
			 let val bnd = BND_EXP(eq_var,e)
			     val dec = DEC_EXP(eq_var,c,NONE,false)
			 in  [(SBND(eq_lab,bnd),SDEC(eq_lab,dec))]
			 end
		   | SOME(_,PHRASE_CLASS_MOD (m,b,s)) => 
			 let val bnd = BND_MOD(eq_var,b,m)
			     val dec = DEC_MOD(eq_var,b,s)
			 in  [(SBND(eq_lab,bnd),SDEC(eq_lab,dec))]
			 end
		   | _ => [])
	    val (all_constr_labs,carrying_constr_labs,constr_sbndsdec) = 
		(case (Context_Lookup_Labels(context,dt_labs)) of
		     SOME(_,PHRASE_CLASS_MOD (m,b,SIGNAT_SELF(_,_,s))) => 
			 let val bnd = BND_MOD(dt_var,b,m)
			     val dec = DEC_MOD(dt_var,b,s)
			     val SIGNAT_STRUCTURE (_::_::sdecs) = s
			     (* need to keep only labs of value-carrying constructors *)
			     fun mapper (SDEC(l,dec)) = 
				 let fun is_arrow (CON_ARROW _) = SOME l
				       | is_arrow _ = NONE
				 in  case dec of
				     DEC_EXP(_,c,_,_) => is_arrow c
				   | DEC_MOD (_,true,SIGNAT_FUNCTOR(_,_,
						  SIGNAT_STRUCTURE([SDEC(_,DEC_EXP(_,c,_,_))]),_)) => is_arrow c
				   | _ => NONE
				 end
			     val all_labs = map (fn (SDEC(l,_)) => l) sdecs
			     val carrying_labs = List.mapPartial mapper sdecs
			 in  (all_labs,carrying_labs,[(SBND(dt_lab,bnd),SDEC(dt_lab,dec))])
			 end
		   | _ => (Ppil.pp_pathlist Ppil.pp_label' dt_labs; print "\n"; 
			   error "unbound datatype - constr labs"))


	    val constr_ssum_strings = 
		let fun mapper (n,_) = ((Symbol.name type_sym) ^ "_sum" ^ (Int.toString n))
		in  mapcount mapper all_constr_labs
		end
	    val constr_ssum_var = map fresh_named_var constr_ssum_strings
	    val constr_ssum_lab = map internal_label constr_ssum_strings
	    val oldconstr_ssum_strings = 
		let fun mapper (n,_) = ((Symbol.name old_type_sym) ^ "_sum" ^ (Int.toString n))
		in  mapcount mapper all_constr_labs
		end
	    val oldconstr_ssum_labs = map internal_label oldconstr_ssum_strings
	    val constr_ssum_labs = map change_path (map (fn l => fn _ => l) oldconstr_ssum_labs)

	    fun copy_type str (lookup_labs, lab, var) =
		case (Context_Lookup_Labels(context,lookup_labs)) of
		     SOME(_,PHRASE_CLASS_CON (c,k,_,i)) => 
			 let 
			     val bnd = BND_CON(var,c)
			     val dec = DEC_CON(var,k,SOME c,i)
			 in  (SBND(lab,bnd),SDEC(lab,dec))
			 end
		   | _ => (print "lookup_labs: "; app Ppil.pp_label lookup_labs; print "\n";
			   error ("unbound datatype - copy type" ^ str))

(*
	    val constr_con_strings = map (fn con_sym =>
				         ((Symbol.name type_sym) ^ "_" ^ 
					   (Name.label2name con_sym))) carrying_constr_labs
	    val old_constr_con_strings = map (fn con_sym =>
				         ((Symbol.name old_type_sym) ^ "_" ^ 
					   (Name.label2name con_sym))) carrying_constr_labs

	    val constr_con_vars = map fresh_named_var constr_con_strings
	    val constr_con_labs = map internal_label constr_con_strings
	    val old_constr_con_labs = map internal_label old_constr_con_strings
	    val constr_con_lookup_labs = map (fn l => change_path (fn _ => l)) old_constr_con_labs
	    val constr_con_sbnd_sdecs = map3 (copy_type "constr_con") 
		(constr_con_lookup_labs,constr_con_labs,constr_con_vars)
*)
	    val type_sbndsdec = copy_type "type" (type_labs,type_lab,type_var)
	    val constr_sum_sbndsdec = copy_type "constr_sum" 
		(constr_sum_labs,constr_sum_lab,constr_sum_var)
	    val constr_sumarg_sbndsdec = copy_type "constr_sumarg" 
		(constr_sumarg_labs,constr_sumarg_lab,constr_sumarg_var)
(*
	    val constr_ssum_sbndsdecs = map3 (copy_type "constr_ssum") 
		(constr_ssum_labs,constr_ssum_lab,constr_ssum_var)
*)
	in [type_sbndsdec] 
(* @ constr_con_sbnd_sdecs *)
	    @ [constr_sumarg_sbndsdec,constr_sum_sbndsdec]
(*  @ constr_ssum_sbndsdecs *)
	      @ eq_sbndsdec @ constr_sbndsdec
	end
    

    fun compile {context, typecompile,
		 datatycs : Ast.db list, eq_compile} : (sbnd * sdec) list =
	let
	    fun calldriver() = 
		let fun mapper (i,arg) = 
		    let val (tyc,tyv,rhs) = AstHelp.db_strip arg
			val def = (case rhs of
				       Ast.Constrs def => def
				     | _ => error "bad datbind")
		    in (i,(tyc,tyv,def))
		    end
		    val nodes = mapcount mapper datatycs
		in  compile'(context,typecompile,nodes,eq_compile)
		end
	in 
	    case datatycs of
		[db] => 
		    let val (tyc,tyv,rhs) = AstHelp.db_strip db
		    in  (case rhs of
			     Ast.Repl path => copy_datatype(context,path,tyc)
			   | _ => calldriver())
		    end
	      | _ => calldriver()
	end


    (* ----- is the phrase-class for a non value-carrying constructor *)
    fun pc_is_const pc = 
	let val innercon =
	    (case pc of
		 PHRASE_CLASS_MOD(_,true,SIGNAT_SELF(_, _, SIGNAT_FUNCTOR(_,_,s,_))) =>
		     (case s of
			  SIGNAT_STRUCTURE([SDEC(itlabel,
						   DEC_EXP(_,con,_,_))]) => con
			| _ => (Ppil.pp_phrase_class pc;
                              error "ill-formed constructor mod phrase_class"))
	       | PHRASE_CLASS_EXP(_,con,_,_) => con
	       | _ => (Ppil.pp_phrase_class pc;
                       error "ill-formed constructor phrase_class"))
	in  (case innercon of
		 CON_ARROW _ => false
	       | CON_APP _ => true
	       | CON_VAR _ => true
	       | CON_SUM _ => true
	       | CON_MODULE_PROJECT _ => true
	       | _ => (print "ill-formed constructor type: ";
		       Ppil.pp_con innercon; print "\n";
		       error "ill-formed constructor type"))
	end

    (* ---------------- constructor LOOKUP RULES --------------------------- 
     --------------------------------------------------------- *)
    type lookup = (Il.context * Il.label list -> (Il.mod * Il.signat) option) 
    exception NotConstructor
    datatype path_or_con = APATH of path | CON of con

    fun constr_lookup context (p : Ast.path) =
	(SOME
	 let 
	    val _ = (debugdo (fn () => (print "constr_lookup called with path = ";
					app pp_label (map symbol_label p);
					print "\nand with context = ";
					pp_context context;
					print "\n")))
	    val (v,ls,pc) = 
		(case (Context_Lookup_Labels(context,map symbol_label p)) of
		     NONE=> (debugdo (fn () => print "constr_lookup found no phrase class\n");
			     raise NotConstructor)
		   | SOME (constr_path, pc) =>
		      (case constr_path of
			   PATH (_,[]) => (debugdo (fn() => print "path was too short\n"); raise NotConstructor)
			 | PATH (v,ls) => (v,ls,pc)))

	    val _ = debugdo (fn () => (print "constr_lookup found v, ls, pc\n"))

	    val datatype_path = PATH(v,butlast ls)  
	    val SIGNAT_SELF(_, _, data_sig) = GetModSig(context,path2mod datatype_path)
	    val sdecs = (case data_sig of
			     SIGNAT_STRUCTURE sdecs => sdecs
			   | _ => error "shortened path did not lead to a structure signature")

	    val _ = if (length sdecs < 2) then raise NotConstructor else ()
	    val internal_type_sdec:: expose_sdec :: _ = sdecs

	    val _ = debugdo (fn() => print "constr_lookup found internal_type_sdec and expose_sdec\n")

	    val (internal_type_lab,type_path) = 
		(case internal_type_sdec of
		     (SDEC(internal_type_lab,DEC_CON(_,_,SOME c,_))) =>
			 (internal_type_lab, (case (con2path c) of
						  NONE => CON c
						| SOME p => APATH p))
		   | _ => raise NotConstructor)

	    val _ = debugdo (fn() => print "constr_lookup found internal_type_label and type_path\n")

	    val sum_path =
		(case expose_sdec of
		     SDEC(_,DEC_EXP(_,CON_ARROW(_,c,_,_),_,_)) => (case con2path c of
								   SOME p => APATH p
								 | NONE => CON c)
		   | SDEC(_,DEC_MOD(_,true,SIGNAT_SELF(_, _, SIGNAT_FUNCTOR(_,_,SIGNAT_STRUCTURE sdecs,_)))) =>
			 (case sdecs of
			      [SDEC(_,DEC_EXP(_,CON_ARROW(_,CON_APP(c,_),_,_),_,_))] => 
				  (case con2path c of
				       SOME p => APATH p
				     | NONE => CON c)
			    | _ => raise NotConstructor)
		   | _ => (debugdo (fn() => (print "constr_lookup got bad expose_sdec\n";
					     pp_sdec expose_sdec; print "\n")); raise NotConstructor))

	    val _ = debugdo (fn() => print "constr_lookup got sum_path\n")
	    val type_lab = symbol_label(Symbol.tycSymbol (Name.label2name internal_type_lab))
	    val num_constr = (length sdecs) - 2

	in (case sdecs of
		(SDEC(type_lab,_)) :: 
		(SDEC(maybe_expose,_)) :: _ =>
		    if (eq_label(maybe_expose,expose_lab))
			then {name = type_lab,
			      is_const = pc_is_const pc,
			      datatype_path = datatype_path,
			      sum_path = sum_path,
			      type_path = type_path,
			      datatype_sig = data_sig}
		    else raise NotConstructor
	      | _ => raise NotConstructor)
	end
    handle NotConstructor => NONE)


    fun is_constr ctxt path = (case constr_lookup ctxt path of
				   NONE => false
				 | SOME _ => true)

    fun is_nonconst_constr ctxt path = (case constr_lookup ctxt path of
					    NONE => false
					  | SOME {is_const,...} => not is_const)

     fun des_dec (d : dec) : ((Il.var * Il.sdecs) option * Il.con) = 
       (case d of
	  DEC_MOD(_,true,SIGNAT_SELF(_, _, SIGNAT_FUNCTOR(v,SIGNAT_STRUCTURE sdecs,
							  SIGNAT_STRUCTURE([SDEC(_,DEC_EXP(_,c,_,_))]),_))) => (SOME (v, sdecs), c)
	| DEC_EXP(_,c,_,_) => (NONE, c)
	| _ => error "des_dec")


     fun destructure_datatype_signature 
	 s : {name : Il.label,
	      var_sdecs_poly : (Il.var * Il.sdecs) option,
	      arm_types : {name : Il.label, arg_type : Il.con option} list} 
       = 
       let fun bad () = (Ppil.pp_signat s;
			 error "ill-formed datatype_signature")
	   fun good (type_name,arm_sdecs) = 
	       let fun helper (SDEC(constr_name,mkdec)) =
			    let 
				val (vso,mkc) = des_dec mkdec
				val argcon = case mkc of
				    CON_ARROW([c],_,_,_) => SOME c
				  | _ => NONE
			    in (vso,{name=constr_name,arg_type=argcon})
			    end
		   val arm_types = map (#2 o helper) arm_sdecs
		   val var_sdecs_poly = #1 (helper (hd arm_sdecs))
	       in {name = type_name,
		   var_sdecs_poly = var_sdecs_poly,
		   arm_types = arm_types}
	       end
	   val sdecs = 
	   (case s of 
		SIGNAT_STRUCTURE sdecs => sdecs
	      | _ => bad())
       in
		    (case sdecs of
			 ((SDEC(type_name,DEC_CON(_,_,_,_)))::
			  (SDEC(maybe_expose,_))::arm_sdecs) =>
			 (if (eq_label(maybe_expose,expose_lab))
			      then good(type_name,arm_sdecs)
			  else bad())
			| _ => bad())
       end

	       
 fun instantiate_datatype_signature (context : Il.context,
				     path : Ast.path,
				     polyinst : (Il.context * Il.sdecs -> 
						 Il.sbnd list * Il.sdecs * Il.con list)) 
     : {instantiated_type : Il.con,
	instantiated_sumtype : Il.con,
	arms : {name : Il.label, arg_type : Il.con option} list,
	expose_exp : Il.exp} =

   let 

       val SOME {sum_path, type_path, 
		 datatype_path, datatype_sig, ...} = constr_lookup context path
       val {name,var_sdecs_poly,arm_types} = destructure_datatype_signature datatype_sig


       val sbnds_sdecs_cons_opt =
	   (case (var_sdecs_poly) of
		SOME (vp,sp) => SOME(polyinst(context, sp))
	      | NONE => NONE)

       fun help path = 
	   (case (Context_Lookup_Path_Open(context,path)) of 
		(SOME(_,PHRASE_CLASS_CON(_,_,SOME c,inline))) => if inline then c else path2con path
	      | NONE => path2con path)
       
       val tycon = (case type_path of
			APATH p => help p
		      | CON c => c)
       val sumtycon = (case sum_path of
			   APATH p => help p
			 | CON c => c)

       val (sumcon,datacon) = (case sbnds_sdecs_cons_opt of
				   NONE => (sumtycon,tycon)
				 | SOME (_,_,cons) => 
				       (CON_APP(sumtycon, cons), 
					CON_APP(tycon, cons)))
	   
       val expose_path = join_path_labels(datatype_path,[expose_lab])
       val expose_exp = 
	   (case 
		(Context_Lookup_Path_Open(context,expose_path), sbnds_sdecs_cons_opt) of 
		(SOME(_,PHRASE_CLASS_EXP(_,_,SOME e,_)),_) => e
	      | (SOME(_,PHRASE_CLASS_MOD(_,true,SIGNAT_SELF(_, _, s))),SOME(sbnds,_,_)) =>
		    (case s of
			 SIGNAT_FUNCTOR(v,argsig,SIGNAT_STRUCTURE [SDEC(_,DEC_EXP(_,_,SOME e,_))],_) =>
			     exp_subst(e,subst_add_modvar(empty_subst, v, MOD_STRUCTURE sbnds))
		       | _ => error "cannot construct exposeExp - weird signature")
	      | _ => (Ppil.pp_path expose_path;
			 error "cannot construct exposeExp - could not find path"))



     fun getconstr_namepatconoption {name,arg_type} =
	 {name=name,
	  arg_type=(case (arg_type,var_sdecs_poly,sbnds_sdecs_cons_opt) of
			(NONE,_,_) => NONE
		      | (SOME x,SOME(var_poly,_),
			        SOME(_,sdecs,_)) => 
			    (SOME(remove_modvar_type(x,var_poly,sdecs))
			    handle e => error "remove_modvar_failed")
		      | (SOME x, _, _) => SOME x)}
	 
   in  {instantiated_type = datacon, 
	instantiated_sumtype = sumcon, 
	arms = map getconstr_namepatconoption arm_types,
	expose_exp = expose_exp}
   end



   fun exn_lookup context path : {stamp : Il.exp,
				  carried_type : Il.con option} option =
       (case (Context_Lookup_Labels(context,map symbol_label path)) of
	  NONE=> NONE
	| SOME (path_mod,PHRASE_CLASS_MOD(m,_,SIGNAT_SELF(_,_,exn_sig as 
					  SIGNAT_STRUCTURE ([SDEC(lab1,DEC_EXP(_,ctag,_,_)),
							     SDEC(lab2,DEC_EXP(_,cmk,_,_))])))) =>
	      if (eq_label(lab1,stamp_lab) andalso eq_label(lab2,mk_lab))
		  then 
		      (case (ctag,cmk) of 
			      (_, CON_ANY) => SOME {stamp=MODULE_PROJECT(path2mod path_mod,stamp_lab), 
						    carried_type = NONE}
			    | (CON_TAG c, _) => SOME {stamp=MODULE_PROJECT(path2mod path_mod,stamp_lab), 
						      carried_type = SOME c}
			    | _ => error_sig exn_sig "bad exn signature")
	      else NONE
	| _ => NONE)


  end
