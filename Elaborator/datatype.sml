(*$import IL ILSTATIC ILUTIL ILCONTEXT Name PPIL Ast GraphUtil ListMergeSort AstHelp DATATYPE *)

(* Datatype compiler and destructures of datatype signatures. *)
functor Datatype(structure IlStatic : ILSTATIC
		 structure IlUtil : ILUTIL
		 structure Ppil : PPIL
		 structure IlContext : ILCONTEXT)
    :> DATATYPE =
  struct

    structure IlContext = IlContext
    open Il IlStatic IlUtil Ppil 
    open Util Listops Name Tyvar
    open IlContext

    val do_inline = ref true
    val error = fn s => error "datatype.sml" s
    val error_sig = fn signat => fn s => error_sig "datatype.sml" signat s 
    val debug = ref false
    fun debugdo t = if (!debug) then (t(); ()) else ()



    (* ------------------------------------------------------------------
      The datatype compiler for compiling a single stronglg-connected type.
      ------------------------------------------------------------------ *)
    fun driver (transparent : bool,
		xty : Il.context * Ast.ty -> Il.con,
		context : context, 
		std_list : (Symbol.symbol * Ast.tyvar list * (Symbol.symbol * Ast.ty option) list) list,
		eqcomp : Il.context * Il.con -> Il.exp option,
		eqcomp_mu : Il.context * Il.con -> Il.exp option) : (sbnd * sdec) list = 
      let 
        (* ---- speculate it is an eq-permitting datatype ------ *)
	val is_eq = ref true

	fun fresh_named_var_inline s = fresh_named_var(s ^ "_inline")
	fun fresh_named_var_transparent s = if transparent then fresh_named_var_inline s
					    else fresh_named_var s

        (* ---- we name the type variables that the datatypes are polymorphic in 
	        and alse name the argument struct holding the type variables --- *)
	val tyvar_labs = flatten (map (fn (_,tyvars,_) => 
					map (symbol_label o AstHelp.tyvar_strip) tyvars) std_list)
	val tyvar_vars = map (fn _ => fresh_named_var "poly") tyvar_labs
	val tyvar_tuple = con_tuple_inject(map CON_VAR tyvar_vars)
	val mpoly_var = fresh_named_var "mpoly_var"
	val num_tyvar = length tyvar_labs
	val is_monomorphic = num_tyvar = 0
	val num_datatype = length std_list

	(* ----- create type, constr type, sum types, constr, and module names *)
	val type_syms = map #1 std_list
	val type_labs = map symbol_label type_syms
	val type_vars = map (fn s => fresh_named_var_transparent (Symbol.name s)) type_syms
	val eq_labs = map (fn s => symbol_label(Symbol.varSymbol(Symbol.name s ^ "_eq"))) type_syms
	val eq_vars = map (fn s => fresh_named_var_transparent (Symbol.name s ^ "_eq")) type_syms
	val inner_type_labvars = map (fn s => 
				   let val str = Symbol.name s
				   in  (internal_label str,
					fresh_named_var_transparent ("copy_" ^ str))
				   end) type_syms
	val top_type_string = foldl (fn (s,acc) => acc ^ "_" ^ (Symbol.name s)) "" type_syms
	val top_eq_string = top_type_string ^ "_eq"
	val top_type_var = fresh_named_var_transparent top_type_string
	val top_type_lab = internal_label top_type_string
	val top_eq_var = fresh_named_var_transparent top_eq_string
	val top_eq_lab = symbol_label (Symbol.tycSymbol top_eq_string)
	val module_labvars = map (fn s => (openlabel(IlUtil.to_datatype_lab(symbol_label s)),
					   fresh_named_var_inline (Symbol.name s))) type_syms
	val constr_sumarg_strings = map (fn s => (Symbol.name s) ^ "_sumarg") type_syms
	val constr_sumarg_vars = map fresh_named_var_transparent constr_sumarg_strings
	val constr_sumarg_labs = map internal_label constr_sumarg_strings
	val constr_sum_strings = map (fn s => (Symbol.name s) ^ "_sum") type_syms
	val constr_sum_vars = map fresh_named_var_transparent constr_sum_strings
	val constr_sum_labs = map internal_label constr_sum_strings
	val constr_ssum_strings = 
	    let fun mapper type_sym (n,_) = ((Symbol.name type_sym) ^ "_sum" ^ (Int.toString n))
	    in  map2 (fn (type_sym, (_,_,tys)) => mapcount (mapper type_sym) tys) (type_syms, std_list)
	    end
	val constr_ssum_vars = mapmap fresh_named_var_transparent constr_ssum_strings
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
	val constr_con_vars = mapmap (Util.mapopt fresh_named_var_transparent) constr_con_strings
	val constr_con_labs = mapmap (Util.mapopt internal_label) constr_con_strings

	(* ---- if all constructors are non value-carrying, we don't (un)roll -- *)
	val is_noncarrying = 
	      (case std_list of
		   [(_,_,sym_tyopts)] => (Listops.andfold (fn (_,NONE) => true 
		                                           | (_,SOME _) => false) sym_tyopts)
		 | _ => false)
	fun roll(x,y) = if (is_noncarrying) then y else ROLL(x,y)
	fun unroll(x,y,z) = if (is_noncarrying) then z else UNROLL(x,y,z)

        (* -- we name the datatypes with variables for use in CON_MU ----- *)
	val vardt_list = map (fn s => (fresh_named_var ("vdt" ^ "_" ^ (Symbol.name s)))) type_syms


	(* ---- compute sig_poly, sig_poly+, and a context context' -----------
	   ---- that has the polymorphic type variables and datatypes bound --- *)
	local
	  val sdecs = (map2 (fn (tv,v) => SDEC(tv,DEC_CON(v,KIND_TUPLE 1,NONE))) 
		       (tyvar_labs, tyvar_vars))
	  val sdecs_eq = (map2 (fn (tv,v) => 
				let val eq_label = to_eq_lab tv
				    val eq_con = con_eqfun(CON_VAR v)
				in SDEC(eq_label,DEC_EXP(fresh_var(),eq_con))
				end)
			  (tyvar_labs, tyvar_vars))
	  val temp = ((map2 (fn (tv,vp) => (tv,vp,KIND_TUPLE 1)) (tyvar_labs,tyvar_vars)) @
		      (map2 (fn (tc,vty) => (tc,vty,KIND_ARROW(num_tyvar,1))) (type_labs,vardt_list)))
	  fun folder ((l,v,k),context) = add_context_con(context,l,v,k,NONE)
	  fun merge [] [] = []
	    | merge (a::b) (c::d) = a::c::(merge b d)
	    | merge _ _ = error "merge failed"
	in
	  val sigpoly = SIGNAT_STRUCTURE (NONE, sdecs)
	  val sigpoly_eq = SIGNAT_STRUCTURE (NONE, merge sdecs sdecs_eq)
	  val context' = foldl folder context temp
	end

	(* ---- compute all the types carried by the constructors ------------ *)
	local
	    val subst_nr2r = zip vardt_list (map CON_VAR vardt_list)
	    fun conapper(CON_VAR tarv,c2) : con option = assoc_eq(eq_var,tarv,subst_nr2r)
	      | conapper _ = NONE
	    fun to_var_dt c = con_subst_conapps(c,conapper)
	in 
	    val constr_nrc = mapmap (fn NONE => NONE
				      | SOME ty => SOME(to_var_dt(xty(context',ty)))) constr_tys
	end

        (* ------------ now create the sum types ------------------------------- *)
	local
	    fun conopts_split (nca,ca) ([] : 'a option list) = (nca,rev ca)
	      | conopts_split (nca,ca) (NONE::rest) = conopts_split (nca+1,ca) rest
	      | conopts_split (nca,ca) ((SOME c)::rest) = conopts_split (nca,c::ca) rest
	in
	    val conopts_split = fn arg => conopts_split (0,[]) arg
	    val (constr_nrc_sumarg,constr_sumarg_kind,constr_nrc_sum) = unzip3
		(map2 (fn (sumarg_var,conopts) => 
		      let val (nca,ca) = conopts_split conopts
			  val (carrier,kind) = (case ca of
						    [] => (CON_TUPLE_INJECT [], KIND_TUPLE 0)
						  | [c] => (c, KIND_TUPLE 1)
						  | _ => (CON_TUPLE_INJECT ca, KIND_TUPLE (length ca)))
		      in (carrier, kind,
			  CON_SUM{noncarriers=nca,
				  carrier=carrier,
				  special=NONE})
		      end)
		 (constr_sumarg_vars,constr_nrc))
	end

        (* ---------- now create the top datatype tuple using var_poly ------ *)
	val top_type_tyvar = if is_noncarrying 
			       then hd(constr_nrc_sum)
		              else CON_MU(CON_FUN(vardt_list,con_tuple_inject(constr_nrc_sum)))


        (* ------------ create the polymorphic type arguments --------------------- 
	   ------------- and instanitated versions of datatypes ------------------- *)
	val tyvar_mprojs = map (fn l => CON_MODULE_PROJECT (MOD_VAR mpoly_var, l)) tyvar_labs
	val tyvar_mproj = con_tuple_inject tyvar_mprojs
	val type_cinsts = map (fn tv => if (is_monomorphic) 
					    then (CON_VAR tv)
					else CON_APP(CON_VAR tv, tyvar_tuple)) type_vars
	val type_minsts = map (fn tv => if (is_monomorphic) 
					    then (CON_VAR tv)
					else CON_APP(CON_VAR tv, tyvar_mproj)) type_vars


	(* ------------ create the version of constructors types and datatypes that 
	   ------------ use the type variable projections ------------------------ *)
	local
	    val subst_nr2r = zip vardt_list type_cinsts
	    val subst_c2m = zip tyvar_vars tyvar_mprojs
	    fun constr_mapper c = con_subst_convar(c,subst_nr2r)
	    fun constr_mapper' NONE = NONE
	      | constr_mapper' (SOME c) = SOME(con_subst_convar(c,subst_nr2r))
	    fun constr_sum_mapper (constr_sum_arg_var_i,constr_con_vars_i) =
		let val (nca,vars) = conopts_split constr_con_vars_i
		    val carrier = 
			(case (transparent,is_monomorphic) of
			     (false,true) => CON_VAR constr_sum_arg_var_i
			   | (false,false) => CON_APP(CON_VAR constr_sum_arg_var_i, tyvar_tuple)
			   | (true,true) => con_tuple_inject(map CON_VAR vars)
			   | (true,false) => con_tuple_inject(map (fn v => (CON_APP(CON_VAR v, tyvar_tuple))) vars))
		in CON_SUM{noncarriers=nca,carrier=carrier,special=NONE}
		end
	    fun specialize (CON_SUM{noncarriers,carrier,...}) (n,_) = 
		     (CON_SUM{noncarriers=noncarriers,
				  carrier=carrier,special=SOME n})
	      | specialize _ _ = error "specialize got non-sum"
	in  
(*	    val constr_rf_sumarg = map constr_mapper constr_nrc_sumarg *)
	    val constr_rf = mapmap constr_mapper' constr_nrc
	    val constr_rf_sum = 
		(map2 constr_sum_mapper (constr_sumarg_vars,constr_con_vars))
	    val constr_rf_ssum = map2 (fn (s,opts) => mapcount (specialize s) opts) 
		                   (constr_rf_sum, constr_nrc)
	    val top_type_mproj = con_subst_convar(top_type_tyvar,subst_c2m)
	end 



	  
        (* -----------  create the carried types -------------- *)
	fun mapper(constr_con_vars_i, constr_con_labs_i, constr_rf_i) = 
	    map (fn (NONE,_,_) => NONE
	          | (SOME v,SOME l,SOME c) => 
		 let val (c,k) = if is_monomorphic 
				     then (c, KIND_TUPLE 1)
				 else (CON_FUN(tyvar_vars,c), KIND_ARROW(num_tyvar,1))
		 in  SOME(c,SBND(l,BND_CON(v,c)),
			  SDEC(l,DEC_CON(v,k,SOME c)))
		 end) (zip3 constr_con_vars_i constr_con_labs_i constr_rf_i)
	val constr_con_con_sbnd_sdecs = map3 mapper (constr_con_vars, constr_con_labs, constr_rf)


	(* ----------------- compute the constructors ------------- *)
	local 
	    fun mk_help (type_minst, 
			 type_i : con, 
			 constr_con_i : con option list,
			 constr_sum_i : con) =
		let 
		    val var = fresh_named_var "injectee"
		    val (nca,cons) = conopts_split constr_con_i
		    fun mapper c = if (is_monomorphic)
				       then c
				   else ConApply(true,c,tyvar_mproj)
		    val ca = map mapper cons
		    val mutype = if is_monomorphic
				     then type_i
				 else ConApply(true,type_i, tyvar_mproj)
		    fun help (j, constr_con_ij_opt) =
			let val constr_sum = if (is_monomorphic)
						  then constr_sum_i
					      else ConApply(true,constr_sum_i, tyvar_mproj)
			in  case constr_con_ij_opt of
			    NONE =>
				(roll(type_minst,
				      INJ{sumtype = constr_sum,
					  field = j,
					  inject = NONE}), mutype)
			  | SOME constr_con_ij =>
				(make_total_lambda(var,
					   if (is_monomorphic)
					       then constr_con_ij
					   else ConApply(true,constr_con_ij, tyvar_mproj),
					   mutype,
					   roll(type_minst,
						INJ{sumtype = constr_sum,
						    field = j,
						    inject = SOME (VAR var)})))
			end
		in mapcount help constr_con_i
		end
	    val top_type_inline = top_type_tyvar
	    val type_inline = if num_datatype = 1
				  then [top_type_tyvar]
			      else map0count (fn i => CON_TUPLE_PROJECT(i,
						      top_type_tyvar)) num_datatype
	in val exp_con_mk = map4 mk_help (type_minsts,
					  if transparent
					      then type_inline
					  else map CON_VAR type_vars,
					  if transparent 
					      then constr_rf
(*	    else mapmap (Util.mapopt CON_VAR) constr_con_vars, *)
					  else mapmap (fn SOME(c,_,_) => SOME c
						        | NONE => NONE) constr_con_con_sbnd_sdecs,
					  if transparent 
					      then constr_rf_sum
					  else map CON_VAR constr_sum_vars)
	end

	
	(* ----------------- compute the exposes ------------------- *)
	local 
	    fun expose_help (type_minst,constr_sum_i,constr_con_i) =
		let
		    val (count,cons : con list) = conopts_split constr_con_i
		    fun mapper c = if (is_monomorphic)
				       then c
				   else ConApply(true,c,tyvar_mproj)
		    val sumtype = if is_monomorphic
				      then constr_sum_i
				  else ConApply(true,constr_sum_i, tyvar_mproj)
		    val var = fresh_named_var "exposee"
		in  make_total_lambda(var,type_minst,sumtype,
				      unroll(type_minst,sumtype,VAR var))
		end
	in val exp_con_expose = map3 expose_help (type_minsts,
						  if transparent 
						      then constr_rf_sum
						  else map CON_VAR constr_sum_vars,
						  if transparent 
						      then constr_rf
						  else mapmap (Util.mapopt CON_VAR) constr_con_vars)
	end
		
	(* ----------------- compute the equality function  ------------------- *)
	local
	    val var_poly_dec = DEC_MOD(mpoly_var,SelfifySig context (SIMPLE_PATH mpoly_var,sigpoly_eq))
	    val temp_ctxt = add_context_dec(context,var_poly_dec)
	    val eq_con = if (is_noncarrying)
				then con_eqfun top_type_mproj
			 else let fun mapper i = con_eqfun(CON_TUPLE_PROJECT(i,top_type_mproj))
			      in  con_tuple(map0count mapper num_datatype)
			      end
	    val eq_exp = if (is_noncarrying)
			   then eqcomp(temp_ctxt,top_type_mproj)
			 else let val CON_MU confun = top_type_mproj
			      in  eqcomp_mu(temp_ctxt,confun)
			      end
	in
	     val eq_exp_con = (case eq_exp of 
				(SOME e) => SOME(e,eq_con)
		      	      | NONE => (is_eq := false; NONE))
	end



	(* --------- create the inner modules ------------- *)
	fun help ((inner_type_lab_i, inner_type_var_i),type_var_i,
		  (module_lab, module_var),
		  (exp_expose_i,con_expose_i),
		  constr_labs_row,
		  expcon_mk_i) = 
	  let 
	    (* ----- do the type -------- *)
	    val k = if is_monomorphic then KIND_TUPLE 1 else KIND_ARROW(num_tyvar,1)
	    val type_sbnd = SBND(inner_type_lab_i, BND_CON(inner_type_var_i,CON_VAR type_var_i))
	    val type_sdec = SDEC(inner_type_lab_i, DEC_CON(inner_type_var_i,k,SOME(CON_VAR type_var_i)))

	    (* ----- do the expose -------- *)
	    val expose_var = fresh_named_var "exposer"
	    val expose_modvar = fresh_named_var "exposer_mod"
	    val expose_expbnd = BND_EXP(expose_var,exp_expose_i)
	    val expose_expdec = DEC_EXP(expose_var,con_expose_i)
	    val expose_inner_sig = SIGNAT_STRUCTURE(NONE,[SDEC(it_lab,expose_expdec)])
	    val expose_modbnd = BND_MOD(expose_modvar,
					MOD_FUNCTOR(mpoly_var,sigpoly,
						    MOD_STRUCTURE[SBND(it_lab,expose_expbnd)],
						    expose_inner_sig))
	    val expose_moddec = DEC_MOD(expose_modvar,
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
		  val dec = DEC_EXP(mk_var, con_mk_ij)
		  val inner_sig = SIGNAT_STRUCTURE(NONE,[SDEC(it_lab,dec)])
		  val modbnd = BND_MOD(mkpoly_var,
					   MOD_FUNCTOR(mpoly_var,sigpoly,
						       MOD_STRUCTURE[SBND(it_lab,bnd)],
						       inner_sig))
		  val moddec = DEC_MOD(mkpoly_var,
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
	    val inner_sig = if (!do_inline)
				then SIGNAT_INLINE_STRUCTURE{self = NONE,
							     abs_sig = sdecs,
							     code = sbnds}
			    else SIGNAT_STRUCTURE(NONE,sdecs)

	  in (SBND(module_lab,BND_MOD(module_var,inner_mod)),
	      SDEC(module_lab,DEC_MOD(module_var,inner_sig)))
	  end


	val components = map6 help (inner_type_labvars, 
				    type_vars,
				    module_labvars,
				    exp_con_expose,
				    constr_labs,
				    exp_con_mk)


	local
	    fun help (i,type_lab_i,type_var_i) =
		(case eq_exp_con of
		     NONE => error "must have eqfuntion at this point"
		   | SOME (top_eq_exp,top_eq_con) =>
			 let 
			     val eq_lab = to_eq_lab type_lab_i
			     val equal_var = fresh_named_var_transparent (label2string eq_lab)
			     val bnd_var = fresh_named_var ("poly" ^ (label2string eq_lab))
			     val exp_eq = if num_datatype = 1
						then top_eq_exp
						else RECORD_PROJECT(VAR top_eq_var, 
							generate_tuple_label(i+1), top_eq_con)
			     val con_eq = con_eqfun(if is_monomorphic 
							then CON_VAR type_var_i
						     else CON_APP (CON_VAR type_var_i, tyvar_mproj))
			     val eq_expbnd = BND_EXP(equal_var,exp_eq)
			     val eq_expdec = DEC_EXP(equal_var,con_eq)
			     val eq_inner_sig = SIGNAT_STRUCTURE(NONE,[SDEC(it_lab,
									    eq_expdec)])
			     val eq_sbnd = 
				 SBND((eq_lab,
				       if (is_monomorphic)
					   then eq_expbnd
				       else
					   BND_MOD(bnd_var,
						   MOD_FUNCTOR(mpoly_var,sigpoly_eq,
							   MOD_STRUCTURE[SBND(it_lab,eq_expbnd)],
							       eq_inner_sig))))
			     val eq_sdec = 
				 SDEC(eq_lab,
				      if (is_monomorphic)
					  then eq_expdec
				      else DEC_MOD(bnd_var,
					     SIGNAT_FUNCTOR(mpoly_var,sigpoly_eq,
							    eq_inner_sig,TOTAL)))
			 in (eq_sbnd, eq_sdec)
			 end)
	in val eq_sbnd_sdecs = if (!is_eq)
				   then map2count help (type_labs,type_vars)
			       else []
	end


	val top_eq_sbnd_sdec = 
	    (case (eq_exp_con, num_datatype) of
		(NONE,_) => []
	      | (_, 1) => []
	      | (SOME (eq_exp, eq_con),_) =>
		let val equal_var = if is_monomorphic then top_eq_var 
				    else fresh_named_var_transparent "top_eq"
		    val expbnd = BND_EXP(equal_var, eq_exp)
		    val expdec = DEC_EXP(equal_var, eq_con)
		    val inner_sig = SIGNAT_STRUCTURE(NONE,[SDEC(it_lab,expdec)])
		    val modbnd = BND_MOD(top_eq_var, MOD_FUNCTOR(mpoly_var,sigpoly_eq,
							 MOD_STRUCTURE[SBND(it_lab,expbnd)],
							 inner_sig))
		    val moddec = DEC_MOD(top_eq_var, SIGNAT_FUNCTOR(mpoly_var,sigpoly_eq,
								    inner_sig, TOTAL))
		    val bnd = if is_monomorphic then expbnd else modbnd
		    val dec = if is_monomorphic then expdec else moddec
		in  [(SBND(top_eq_lab, bnd), SDEC(top_eq_lab, dec))]
		end)


	val top_type_sbnd_sdec = 
		let val (c,base_kind) = if is_monomorphic
					then (top_type_tyvar, KIND_TUPLE num_datatype)
				else (CON_FUN(tyvar_vars, top_type_tyvar), KIND_ARROW(num_tyvar, num_datatype))
		    val k = if (!do_inline) then KIND_INLINE(base_kind,c) else base_kind
		in  if num_datatype = 1 
			then []
		     else [(SBND(top_type_lab, BND_CON(top_type_var, c)),
		            SDEC(top_type_lab, DEC_CON(top_type_var, k, 
						       if transparent then SOME c else NONE)))]
		end

	val type_sbnd_sdecs = 
		let val base_kind = if is_monomorphic then KIND_TUPLE 1 else KIND_ARROW(num_tyvar,1)
		    fun mapper(i,l,v) =
			let val c = if num_datatype = 1 then top_type_tyvar 
					else CON_TUPLE_PROJECT(i,CON_VAR top_type_var)
			    val c = if is_monomorphic then c else CON_FUN(tyvar_vars, c)
			    val k = if (!do_inline) then KIND_INLINE(base_kind,c) else base_kind
			in  (SBND(l,BND_CON(v,c)), 
			     (SDEC(l,DEC_CON(v,k,
					     if transparent then SOME c else NONE))))
			end
		in  map2count mapper (type_labs,type_vars)
		end


	fun mapper(constr_sumarg_lab_i,constr_sumarg_var_i, 
		   constr_con_con_sbnd_sdecs_i : (con * sbnd * sdec) option list, 
		   constr_sumarg_kind_i) = 
	    let val KIND_TUPLE num_carrier = constr_sumarg_kind_i
		val cons = List.mapPartial (Util.mapopt #1) constr_con_con_sbnd_sdecs_i
		val k = if is_monomorphic 
			    then KIND_TUPLE num_carrier
			else KIND_ARROW(num_tyvar,num_carrier)
		val tyvar_tuple = con_tuple_inject(map CON_VAR tyvar_vars)
		val c =
		    (case (length cons = 1, is_monomorphic) of
			 (true,true) => hd cons
		       | (false,true) => con_tuple_inject cons
		       | (true,false) => hd cons
		       | (false,false) => 
			     (print "case 4 \n"; 
			      CON_FUN(tyvar_vars,
				      con_tuple_inject
				      (map (fn c => ConApply(true,c, tyvar_tuple)) cons))))
	    in  (SBND(constr_sumarg_lab_i,BND_CON(constr_sumarg_var_i, c)),
		 SDEC(constr_sumarg_lab_i,DEC_CON(constr_sumarg_var_i, k, SOME c)))
	    end

	val constr_sumarg_sbnd_sdecs = map4 mapper
	    (constr_sumarg_labs,constr_sumarg_vars, constr_con_con_sbnd_sdecs,constr_sumarg_kind)

	fun mapper(constr_sum_lab_i,constr_sum_var_i, constr_rf_sum_i) = 
	    let val (c,k) = if is_monomorphic 
				then (constr_rf_sum_i, KIND_TUPLE 1)
			    else (CON_FUN(tyvar_vars,constr_rf_sum_i), KIND_ARROW(num_tyvar,1))
	    in  (SBND(constr_sum_lab_i,BND_CON(constr_sum_var_i, c)),
		 SDEC(constr_sum_lab_i,DEC_CON(constr_sum_var_i, k, SOME c)))
	    end

	val constr_sum_sbnd_sdecs = map3 mapper (constr_sum_labs,constr_sum_vars, constr_rf_sum)


	fun mapper(constr_ssum_lab_ij,constr_ssum_var_ij, constr_rf_ssum_ij) = 
	    let val (c,k) = if is_monomorphic 
				then (constr_rf_ssum_ij, KIND_TUPLE 1)
			    else (CON_FUN(tyvar_vars,constr_rf_ssum_ij), KIND_ARROW(num_tyvar,1))
	    in  (SBND(constr_ssum_lab_ij,BND_CON(constr_ssum_var_ij, c)),
		 SDEC(constr_ssum_lab_ij,DEC_CON(constr_ssum_var_ij, k, SOME c)))
	    end

	val constr_ssum_sbnd_sdecs = 
	    map3 (fn (x,y,z) => map3 mapper (x,y,z))
	       (constr_ssum_labs,constr_ssum_vars, constr_rf_ssum)

	val final_sbnd_sdecs = (top_type_sbnd_sdec
				@ top_eq_sbnd_sdec
				@ type_sbnd_sdecs 
(*				@ (flatten constr_con_sbnd_sdecs)  *)
				@ constr_sumarg_sbnd_sdecs
				@ constr_sum_sbnd_sdecs
(*				@ (flatten constr_ssum_sbnd_sdecs) *)
				@ eq_sbnd_sdecs 
				@ components)
      in  final_sbnd_sdecs
      end

 
    (* ------------------------------------------------------------------
      The datatype compiler for compiling a single datatype statement.
      ------------------------------------------------------------------ *)
    type node = int * (Symbol.symbol * Ast.tyvar list * (Ast.symbol * Ast.ty option) list)
    fun compile' (transparent,
		  context, typecompile,
		  nodes : node list, eq_compile, eq_compile_mu) : (sbnd * sdec) list =
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
	    let fun geq_sym((s1,_),(s2,_)) = 
		(case (String.compare(Symbol.symbolToString s1,Symbol.symbolToString s2)) of
		     GREATER => true
		   | EQUAL => true
		   | LESS => false)
		fun sort_std' (ncs,cs) [] = (ListMergeSort.sort geq_sym ncs) @ 
		                            (ListMergeSort.sort geq_sym cs)
		  | sort_std' (ncs,cs) ((nc as (_,NONE))::rest) = sort_std' (nc::ncs,cs) rest
		  | sort_std' (ncs,cs) ((c as (_,SOME _))::rest) = sort_std' (ncs,c::cs) rest
		fun sort_std (s,tvs,st_list) = (s,tvs,sort_std' ([],[]) st_list)
		val std_list = map sort_std std_list
		val sbnd_sdecs = driver(transparent,typecompile,context,
					std_list, eq_compile, eq_compile_mu)
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
	    val constr_sum_string = (Symbol.name type_sym) ^ "_sum"
	    val constr_sum_var = fresh_named_var constr_sum_string
	    val constr_sum_lab = internal_label constr_sum_string
	    val old_constr_sum_string = (Symbol.name old_type_sym) ^ "_sum"
	    val old_constr_sum_lab = internal_label old_constr_sum_string
	    val constr_sumarg_string = (Symbol.name type_sym) ^ "_sumarg"
	    val constr_sumarg_var = fresh_named_var constr_sumarg_string
	    val constr_sumarg_lab = internal_label constr_sumarg_string
	    val old_constr_sumarg_string = (Symbol.name old_type_sym) ^ "_sumarg"
	    val old_constr_sumarg_lab = internal_label old_constr_sumarg_string

	    val eq_lab = to_eq_lab type_lab 
	    val eq_var = fresh_named_var "eqfun"
	    val dt_lab = to_datatype_lab type_lab
	    val dt_var = fresh_named_var "dt"
	    fun change_path [] _ = error "empty path"
	      | change_path [l] base = [base l]
	      | change_path (a::b) base = a :: change_path b base
	    val change_path = change_path (map symbol_label path)
	    val type_labs = change_path (fn l => l)
	    val eq_labs = change_path to_eq_lab
	    val dt_labs = change_path to_datatype_lab
	    val constr_sum_labs = change_path (fn _ => old_constr_sum_lab)
	    val constr_sumarg_labs = change_path (fn _ => old_constr_sumarg_lab)

	    val eq_sbndsdec = 
		(case (Context_Lookup_Labels(context,eq_labs)) of
		     SOME(_,PHRASE_CLASS_EXP (e,c)) => 
			 let val bnd = BND_EXP(eq_var,e)
			     val dec = DEC_EXP(eq_var,c)
			 in  [(SBND(eq_lab,bnd),SDEC(eq_lab,dec))]
			 end
		   | SOME(_,PHRASE_CLASS_MOD (m,s)) => 
			 let val bnd = BND_MOD(eq_var,m)
			     val dec = DEC_MOD(eq_var,s)
			 in  [(SBND(eq_lab,bnd),SDEC(eq_lab,dec))]
			 end
		   | _ => [])
	    val (all_constr_labs,carrying_constr_labs,constr_sbndsdec) = 
		(case (Context_Lookup_Labels(context,dt_labs)) of
		     SOME(_,PHRASE_CLASS_MOD (m,s)) => 
			 let val bnd = BND_MOD(dt_var,m)
			     val dec = DEC_MOD(dt_var,s)
			     val (_::_::sdecs) = 
				 (case s of 
				      SIGNAT_STRUCTURE(_,sdecs) => sdecs
				    | SIGNAT_INLINE_STRUCTURE {abs_sig,...} => abs_sig)
			     (* need to keep only labs of value-carrying constructors *)
			     fun mapper (SDEC(l,dec)) = 
				 let fun is_arrow (CON_ARROW _) = SOME l
				       | is_arrow _ = NONE
				 in  case dec of
				     DEC_EXP(_,c) => is_arrow c
				   | DEC_MOD (_,SIGNAT_FUNCTOR(_,_,
						  SIGNAT_STRUCTURE(_,[SDEC(_,DEC_EXP(_,c))]),_)) => is_arrow c
				   | _ => NONE
				 end
			     val all_labs = map (fn (SDEC(l,_)) => l) sdecs
			     val carrying_labs = List.mapPartial mapper sdecs
			 in  (all_labs,carrying_labs,[(SBND(dt_lab,bnd),SDEC(dt_lab,dec))])
			 end
		   | _ => error "unbound datatype - constr labs")


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
		     SOME(_,PHRASE_CLASS_CON (c,k)) => 
			 let 
			     val bnd = BND_CON(var,c)
			     val dec = DEC_CON(var,k,SOME c)
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
    

    fun compile {transparent,
		 context, typecompile,
		 datatycs : Ast.db list, eq_compile, eq_compile_mu} : (sbnd * sdec) list =
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
		in  compile'(transparent,context,typecompile,nodes,eq_compile, eq_compile_mu)
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
		 PHRASE_CLASS_MOD(_,SIGNAT_FUNCTOR(_,_,s,_)) =>
		     (case s of
			  SIGNAT_STRUCTURE(_,[SDEC(itlabel,
						   DEC_EXP(_,con))]) => con
		       | _ => error "ill-formed constructor phrase_class")
	       | PHRASE_CLASS_EXP(_,con) => con
	       | _ => (error "ill-formed constructor phrase_class"))
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
    datatype path_or_con = PATH of path | CON of con

    fun constr_lookup context (p : Ast.path) =
	(SOME
	 let 
	    val _ = (debugdo (fn () => (print "constr_lookup called with path = ";
					app pp_label (map symbol_label p);
					print "\nand with context = ";
					pp_context context;
					print "\n")))
	    val (constr_path,pc) = 
		(case (Context_Lookup_Labels(context,map symbol_label p)) of
		     NONE=> (debugdo (fn () => print "constr_lookup modsig_lookup got NONE\n");
			     raise NotConstructor)
		   | SOME lookup_result => lookup_result)
		
	    val (v,ls) = (* well-formed compound path *)
		(case constr_path of
		     SIMPLE_PATH v => raise NotConstructor
		   | COMPOUND_PATH (v,[]) => error "constr_lookup: empty COMPOUND PATH"
		   | COMPOUND_PATH v_ls => v_ls)


	    val datatype_path = (case ls of 
				     [_] => SIMPLE_PATH v
				   | _ => COMPOUND_PATH(v,butlast ls))

	    val data_sig = GetModSig(context,path2mod datatype_path)
	    val sdecs = (case data_sig of
			     (SIGNAT_STRUCTURE(_,sdecs)) => sdecs
			   | SIGNAT_INLINE_STRUCTURE {abs_sig = sdecs,...} => sdecs
			   | _ => raise NotConstructor)


(*
	    val _ = case sdecs of
		(SDEC(internal_type_lab,DEC_CON(_,_,SOME c)))::_ => ()
	      | _ => (print "datatype_constr_lookup failed with sdecs = \n";
		      pp_sdecs sdecs; print "\n")
*)
	    val _ = if (length sdecs < 2) then raise NotConstructor else ()
	    val internal_type_sdec:: expose_sdec :: _ = sdecs
	    val (internal_type_lab,type_path) = 
		(case internal_type_sdec of
		     (SDEC(internal_type_lab,DEC_CON(_,_,SOME c))) =>
			 (internal_type_lab, (case (con2path c) of
						  NONE => CON c
						| SOME p => PATH p))
		   | _ => raise NotConstructor)
	    val sum_path =
		(case expose_sdec of
		     SDEC(_,DEC_EXP(_,CON_ARROW(_,c,_,_))) => (case con2path c of
								   SOME p => PATH p
								 | NONE => CON c)
		   | SDEC(_,DEC_MOD(_,SIGNAT_FUNCTOR(_,_,SIGNAT_STRUCTURE(_,sdecs),_))) =>
			 (case sdecs of
			      [SDEC(_,DEC_EXP(_,CON_ARROW(_,CON_APP(c,_),_,_)))] => 
				  (case con2path c of
				       SOME p => PATH p
				     | NONE => CON c)
			    | _ => raise NotConstructor)
		   | _ => raise NotConstructor)



	    val type_lab = symbol_label(Symbol.tycSymbol (Name.label2name internal_type_lab))
	    val num_constr = (length sdecs) - 2

	    local
		val sum_lab = internal_label((Name.label2name type_lab) ^ "_sum")
(*		val ssum_lab = map0count (fn n => 
					   internal_label(Name.label2name type_lab ^ "_sum"
							   ^ (Int.toString n))) num_constr
*)
		fun get_path lab = 
		    let val _ = if (!debug)
				    then (print "get_path working on lab = "; 
					  Ppil.pp_label lab; print "\n")
				else ()
			val p = (case ls of
			 [_] => 
			     (case (Context_Lookup(context,lab)) of
				  SOME (p,_) => p
				| NONE => ((* print "constr_lookup failed to find path: ";
					   pp_label lab; print "\n"; *)
					   raise NotConstructor))
		       | _ => COMPOUND_PATH(v,(butlast(butlast ls)) @ [lab]))
			val _ = if (!debug)
				    then (print "get_path retrurning: ";
					  pp_path p; print "\n")
				else ()
		    in  p
		    end
	    in  
(* this path could get shadowed so we can't get the type_path this way *)
(*		val type_path = get_path type_lab *)
(*		val sum_path = get_path sum_lab *)
(*		val ssum_path = map get_path ssum_lab *)
	    end



	in (case sdecs of
		(SDEC(type_lab,_)) :: 
		(SDEC(maybe_expose,_)) :: _ =>
		    if (eq_label(maybe_expose,expose_lab))
			then {name = type_lab,
			      is_const = pc_is_const pc,
			      datatype_path = datatype_path,
(*			      ssum_path = ssum_path, *)
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
	  DEC_MOD(_,SIGNAT_FUNCTOR(v,SIGNAT_STRUCTURE (_,sdecs),
		    SIGNAT_STRUCTURE(_,[SDEC(_,DEC_EXP(_,c))]),_)) => (SOME (v, sdecs), c)
	| DEC_EXP(_,c) => (NONE, c)
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
		SIGNAT_STRUCTURE(_,sdecs) => sdecs
	      | (SIGNAT_INLINE_STRUCTURE {abs_sig=sdecs,...}) => sdecs
	      | _ => bad())
       in
		    (case sdecs of
			 ((SDEC(type_name,DEC_CON(_,_,_)))::
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
(*		 ssum_path, *)
		 datatype_path, datatype_sig, ...} = constr_lookup context path
       val {name,var_sdecs_poly,arm_types} = destructure_datatype_signature datatype_sig



       val sbnds_sdecs_cons_opt =
	   (case (var_sdecs_poly) of
		SOME (vp,sp) => SOME(polyinst(context, sp))
	      | NONE => NONE)

       fun help path = 
	   let val is_inline = 
	       (case path of 
		    (SIMPLE_PATH v) => Util.substring("inline",Name.var2name v)
		  | (COMPOUND_PATH (v,ls)) => (Util.substring("inline",Name.var2name v) orelse
					       orfold IlUtil.is_datatype_lab ls))
	   in  if is_inline
		   then (case (Context_Lookup_Path(context,path)) of
			     (SOME(_,PHRASE_CLASS_CON(c,_))) => c
			   | NONE => error "help could not find path")
	       else path2con path
	   end
       
       val tycon = (case type_path of
			PATH p => help p
		      | CON c => c)
       val sumtycon = (case sum_path of
			   PATH p => help p
			 | CON c => c)
(*       val ssumtycon = map help ssum_path *)
       val ((* ssumcon, *)sumcon,datacon) = (case sbnds_sdecs_cons_opt of
					   NONE => ((* ssumtycon, *) sumtycon,tycon)
					 | SOME (_,_,cons) => 
					       let val arg = con_tuple_inject cons
					       in  ((* map (fn c => CON_APP(c,arg)) ssumtycon, *)
						    CON_APP(sumtycon,arg), 
						    CON_APP(tycon, arg))
					       end)
	   
       val expose_path = join_path_labels(datatype_path,[expose_lab])
       val expose_exp = 
	   (case (Context_Lookup_Path(context,expose_path), sbnds_sdecs_cons_opt) of
		(SOME(_,PHRASE_CLASS_EXP(e,_)),_) => e
	      | (SOME(_,PHRASE_CLASS_MOD(m,_)),SOME(sbnds,_,_)) => 
		    (case m of
			 MOD_FUNCTOR(v,_,MOD_STRUCTURE[SBND(_,BND_EXP(_,e))],_) =>
			     exp_subst_modvar(e,[(v,MOD_STRUCTURE sbnds)])
		       | _ => MODULE_PROJECT(MOD_APP(m,MOD_STRUCTURE sbnds),it_lab))
	      | _ => error "cannot construct expose_exp")



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
(*	instantiated_special_sumtype = ssumcon,  *)
	arms = map getconstr_namepatconoption arm_types,
	expose_exp = expose_exp}
   end



   fun exn_lookup context path : {stamp : Il.exp,
				  carried_type : Il.con option} option =
       (case (Context_Lookup_Labels(context,map symbol_label path)) of
	  NONE=> NONE
	| SOME (path_mod,PHRASE_CLASS_MOD(m,exn_sig as 
		SIGNAT_STRUCTURE (_,[SDEC(lab1,DEC_EXP(_,ctag)),SDEC(lab2,DEC_EXP(_,cmk))]))) =>
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
