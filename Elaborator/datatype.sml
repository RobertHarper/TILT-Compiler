(* Datatype compiler and destructures of datatype signatures. *)
functor Datatype(structure Il : IL
		 structure IlStatic : ILSTATIC
		 structure IlUtil : ILUTIL
		 structure Ppil : PPIL
		 structure AstHelp : ASTHELP
		 structure IlContext : ILCONTEXT
		 sharing IlContext.Il = Ppil.Il = IlUtil.Il = IlStatic.Il = Il)
    : DATATYPE  = 
  struct

    structure Il = Il
    open AstHelp Il IlStatic IlUtil Ppil 
    open Util Listops Name (* IlLookup *) Tyvar
    open IlContext
  
    val error = fn s => error "datatype.sml" s
    val error_sig = fn signat => fn s => error_sig "datatype.sml" signat s 
    val debug = ref false
    fun debugdo t = if (!debug) then (t(); ()) else ()

    (* ------------------------------------------------------------------
      The datatype compiler for compiling a single strong-connected type.
      ------------------------------------------------------------------ *)
    fun driver (xty : Il.context * Ast.ty -> Il.con,
		context : context, 
		std_list : (Symbol.symbol * Ast.tyvar list * (Symbol.symbol * Ast.ty option) list) list,
		withtycs : Ast.tb list,
		xeq : Il.context * Il.con -> Il.exp) : (mod * signat) =
      let 
	val _ = (case withtycs of 
		   [] => ()
		 | _ => error "xdatbind does not handle withtypes yet")
	(* --------- create labels and variables; compute indices --------------- *)
	val is_eq = ref true  (* speculate it is an eq-permitting datatype *)
	val p = length std_list
	val type_symbols = map #1 std_list
	val top_type_label = map (fn s => fresh_internal_label ("top_" ^ Symbol.name s)) type_symbols
	val top_type_var = map (fn s => fresh_named_var ("top_" ^ Symbol.name s)) type_symbols
	val type_label = map (fn s => symbol_label s) type_symbols
	val type_var = map (fn s => fresh_named_var ("unused_" ^ (Symbol.name s))) type_symbols
	val tyvar_label = flatten (map (fn (_,tyvars,_) => map (symbol_label o tyvar_strip) tyvars) std_list)
	val k = length tyvar_label
	val varpoly_list = map (fn _ => fresh_named_var "poly") tyvar_label
	val vardt_list = map (fn _ => fresh_named_var "vdt") type_label
	val var_poly = fresh_named_var "varpoly"
	val tys : Ast.ty option list list = map (fn (_,_,def) => map #2 def) std_list
        val ids = map (fn (_,_,def) => map #1 def) std_list

	(* --------- compute sig_poly, sig_poly+, and context' ------------ *)
	local
	  val sdecs = (map2 (fn (tv,v) => SDEC(tv,DEC_CON(v,KIND_TUPLE 1,NONE))) 
		       (tyvar_label, varpoly_list))
	  val sdecs_eq = (map2 (fn (tv,v) => let val eq_label = to_eq_lab tv
						 val eq_con = CON_ARROW(con_tuple[CON_VAR v, CON_VAR v],
									con_bool, oneshot_init PARTIAL)
					     in SDEC(eq_label,DEC_EXP(fresh_var(),eq_con))
					     end)
			  (tyvar_label, varpoly_list))
	  val temp = ((map2 (fn (tv,vp) => (tv,vp,KIND_TUPLE 1)) (tyvar_label,varpoly_list)) @
		      (map2 (fn (tc,vty) => (tc,vty,KIND_ARROW(k,1))) (type_label,vardt_list)))
	  fun folder ((l,v,k),context) = add_context_convar(context,l,v,k,NONE)
	  fun merge [] [] = []
	    | merge (a::b) (c::d) = a::c::(merge b d)
	    | merge _ _ = error "merge failed"
	in
	  val sigpoly = SIGNAT_STRUCTURE (NONE, sdecs)
	  val sigpoly_eq = SIGNAT_STRUCTURE (NONE, merge sdecs sdecs_eq)
	  val context' = foldl folder context temp
	  val is_monomorphic = (length sdecs = 0)
	  val is_onedatatype = (length std_list = 1)
	end

	(* ------------ compute all the cons -------------------------------------- *)
	local
	    val subst_nr2r = zip vardt_list (map CON_VAR vardt_list)
	    fun conapper(CON_VAR tarv,c2) : con option = assoc_eq(eq_var,tarv,subst_nr2r)
	      | conapper _ = NONE
	in fun to_var_dt c = con_subst_conapps(c,conapper)
	end
	val almost_conss_nrc = mapmap (fn NONE => con_unit | SOME ty => xty(context',ty)) tys
	val conss_nrc = mapmap to_var_dt almost_conss_nrc
	val con_nrc = con_tuple_inject(map (fn cons => CON_SUM (NONE,cons)) conss_nrc)
	val cons_rc_help = CON_FUN(vardt_list,con_nrc)
	val cons_rc = map0count (fn i => CON_MUPROJECT(i,cons_rc_help)) p
	val cons_rf_help = map (fn l => CON_MODULE_PROJECT (MOD_VAR var_poly, l)) tyvar_label
	val cons_rf_help' = con_tuple_inject cons_rf_help
	val cons_rf = map (fn v => if (is_monomorphic) then (CON_VAR v)
			   else CON_APP(CON_VAR v, cons_rf_help')) top_type_var
	local
	    val subst_c2f = zip varpoly_list cons_rf_help
	    val subst_nr2r = zip vardt_list cons_rf 
	    val subst_both = subst_c2f @ subst_nr2r
	    fun subst c = con_subst_convar(c,subst_both)
	in val conss_rf = mapmap subst conss_nrc
	end 
(*
	val _ = (print "\nconss_nrc are:\n";
		 mapmap (fn c => (pp_con c; print "\n")) conss_nrc;
		 print "\nconss_rf are:\n";
		 mapmap (fn c => (pp_con c; print "\n")) conss_rf)
*)
	(* ----------------- compute the type functions  ------------- *)
	local
	    fun help (i,cons_rc_i) = 
		if (is_monomorphic) 
		    then (cons_rc_i, KIND_TUPLE 1)
		else (CON_FUN(varpoly_list,cons_rc_i), 
		      KIND_ARROW(k,1))
	    val temp = mapcount help cons_rc
	in  val type_funs = map #1 temp
	    val type_kinds = map #2 temp
	end
	  
	(* ----------------- compute the constructors ------------- *)
	local 
	    fun mk_help (conss_rf_i, cons_rf_i, tys_i) = 
		let 
		    val var = fresh_var()
		    fun help (j, _, NONE) = (ROLL(cons_rf_i,
						  INJ(conss_rf_i,j,unit_exp)), cons_rf_i)
		      | help (j, conss_rf_ij, _) = (make_total_lambda(var,conss_rf_ij,cons_rf_i,
								      ROLL(cons_rf_i,
									   INJ(conss_rf_i,j,VAR var))))
		in map2count help (conss_rf_i, tys_i)
		end
	in val exp_con_mk = map3 mk_help (conss_rf,cons_rf,tys)
	end

	(* ----------------- compute the exposes ------------------- *)
	local 
	    fun expose_help (conss_rf_i,cons_rf_i) =
		let
		    val sumtype = CON_SUM(NONE, conss_rf_i)
		    val var' = fresh_var()
		in make_total_lambda(var',cons_rf_i,sumtype,UNROLL(cons_rf_i,VAR var'))
		end
	in val exp_con_expose = map2 expose_help (conss_rf,cons_rf)
	end
		
	(* ----------------- compute the equality functions ------------------- *)
	local
	    val var_poly_dec = DEC_MOD(var_poly,SelfifySig(SIMPLE_PATH var_poly,sigpoly_eq))
	    val temp_ctxt = add_context_dec(context,var_poly_dec)
(*
	    val labs = ref ([] : label list)
	    fun eqfun_search e = 
		let 
		    fun add l = if (member_eq(eq_label,l,!labs))
				    then ()
				else labs := l :: (!labs)
		    fun eproj (MOD_VAR v,l) = (if (eq_var(v,var_poly))
						  then add l
					       else (); 
						   NONE)
		      | eproj _ = NONE
		    fun cproj _ = NONE
		in (exp_subst_proj(e,eproj,cproj); ())
		end
*)
	    fun eq_help (i,type_fun_i,cons_rf_i) =
		let
		    val cons = if (is_monomorphic) 
				   then type_fun_i
			       else CON_APP(type_fun_i,cons_rf_help')
		    (* don't need to resolve later, have all necessary data now *)
(*
		    val _ = (print "datatype.sml calling xeq ENTER with cons = ";
			     pp_con cons; print "\n")
*)
		    val eq_exp = (SOME (xeq(temp_ctxt,cons))
				  handle _ => (is_eq := false; NONE))
(*
		    val _ = (print "datatype.sml calling xeq RETURNED with eq_exp = \n";
			     (case eq_exp of
				 SOME e => pp_exp e
			       | NONE => print "NONE"); print "\n")
		    val _ = print "datatype.sml calling GetExp ENTER\n"
*)
(*  val eq_con = GetExpCon(temp_ctxt,eq_exp)  <--- this is wrong since we must hold type abstract *)
		    val eq_con = CON_ARROW(con_tuple[cons_rf_i,cons_rf_i],
					   con_bool,oneshot_init PARTIAL)
(*
		    val _ = (print "datatype.sml calling GetExpCon RETURNED with eq_con = \n";
			     pp_con eq_con; print "\n")
*)
		in (case eq_exp of 
			SOME e => ((* eqfun_search e; *)
				   SOME(e, eq_con))
		      | NONE => NONE)
		end
(*
	    fun prune (sdec as (SDEC(l,DEC_EXP _))) = if (member_eq(eq_label,l,!labs))
							  then SOME sdec
						      else NONE
	      | prune sdec = SOME sdec
*)
	in
(*
  val sigpoly_eq = (case sigpoly_eq of
				  SIGNAT_STRUCTURE sdecs => SIGNAT_STRUCTURE(List.mapPartial prune sdecs)
				| _ => error "sigpoly_eq not a SIG_STRUCT")
*)
	    val exp_con_eq_opt = map2count eq_help (type_funs, cons_rf)
	end


	(* --------- put the result together now ------------- *)
	fun help (i,(type_label,top_type_var,type_var,type_kind),
		  id_i,expcon_mk_i,
		  (exp_expose_i,con_expose_i)) =
	  let 
	    (* ----- do the tycon -------- *)
	    val type_sbnd = SBND(type_label,BND_CON(type_var,CON_VAR top_type_var))
	    val type_sdec = SDEC(type_label,DEC_CON(type_var,type_kind,SOME(CON_VAR top_type_var)))

	    (* ----- do the expose -------- *)
	    val expose_expbnd = BND_EXP(fresh_var(),exp_expose_i)
	    val expose_expdec = DEC_EXP(fresh_var(),con_expose_i)
	    val expose_sbnd = SBND(expose_lab,
			     if (is_monomorphic)
				 then expose_expbnd
			     else
				 BND_MOD(fresh_var(),
					 MOD_FUNCTOR(var_poly,sigpoly,
						     MOD_STRUCTURE[SBND(it_lab,expose_expbnd)])))
	    val expose_sdec = 
		SDEC(expose_lab,
		     if (is_monomorphic)
			 then expose_expdec
		     else DEC_MOD(fresh_var(),
				  SIGNAT_FUNCTOR(var_poly,sigpoly,
						 SIGNAT_STRUCTURE(NONE,[SDEC(it_lab,expose_expdec)]),
						 oneshot_init TOTAL)))
		

	    (* ----- do the mk  ---- *)
	    fun inner_help (id_ij,(exp_mk_ij,con_mk_ij)) =
	      let 
		fun maker e s = 
		  let val exp_bnd = BND_EXP(fresh_var(), e)
		    val sig_dec = DEC_EXP(fresh_var(), s)
		  in if (is_monomorphic) then (exp_bnd,sig_dec)
		     else
		       (BND_MOD(fresh_var(),MOD_FUNCTOR(var_poly,sigpoly,
							MOD_STRUCTURE[SBND(it_lab,exp_bnd)])),
			DEC_MOD(fresh_var(),
				   SIGNAT_FUNCTOR(var_poly,sigpoly,
						  SIGNAT_STRUCTURE(NONE,[SDEC(it_lab,sig_dec)]),
						  oneshot_init TOTAL)))
		  end
		val (mk_bnd,mk_dec) = maker exp_mk_ij con_mk_ij 
		val module = MOD_STRUCTURE[SBND(mk_lab,mk_bnd)]
		val signat = SIGNAT_STRUCTURE(NONE,[SDEC(mk_lab,mk_dec)])
	      in (SBND(id_ij,BND_MOD(fresh_var(),module)),
		  SDEC(id_ij,DEC_MOD(fresh_var(),signat)))
	      end
	    val temp = map2 inner_help (map symbol_label id_i, expcon_mk_i)
	    val (sbnds,sdecs) = (map #1 temp, map #2 temp)

(*
	    val inner_mod = (case eq_sbnd_opt of
				 SOME eq_sbnd => MOD_STRUCTURE(type_sbnd::eq_sbnd::expose_sbnd::sbnds)
			       | _ => MOD_STRUCTURE(type_sbnd::expose_sbnd::sbnds))
	    val inner_sig = (case eq_sdec_opt of
				 SOME eq_sdec => SIGNAT_STRUCTURE(type_sdec::eq_sdec::expose_sdec::sdecs)
			       | _ => SIGNAT_STRUCTURE(type_sdec::expose_sdec::sdecs))
*)
	    val inner_mod = MOD_STRUCTURE(type_sbnd::expose_sbnd::sbnds)
	    val inner_sig = SIGNAT_STRUCTURE(NONE,type_sdec::expose_sdec::sdecs)

	    val tc_star = openlabel type_label
	  in (SBND(tc_star,BND_MOD(fresh_var(),inner_mod)),
	      SDEC(tc_star,DEC_MOD(fresh_var(),inner_sig)))
	  end


	val components = map4count help ((zip4 type_label top_type_var type_var type_kinds), 
					 ids, exp_con_mk,
					 exp_con_expose)


	local
	    fun help (type_label,exp_con_eq_i_opt) = 
		(case exp_con_eq_i_opt of
		     NONE => NONE
		   | SOME (exp_eq_i,con_eq_i) =>
			 let 
			     val eq_lab = to_eq_lab type_label
			     val eq_expbnd = BND_EXP(fresh_var(),exp_eq_i)
			     val eq_expdec = DEC_EXP(fresh_var(),con_eq_i)
			     val eq_sbnd = 
				 SBND((eq_lab,
				       if (is_monomorphic)
					   then eq_expbnd
				       else
					   BND_MOD(fresh_var(),
						   MOD_FUNCTOR(var_poly,sigpoly_eq,
							   MOD_STRUCTURE[SBND(it_lab,eq_expbnd)]))))
			     val eq_sdec = 
				 SDEC(eq_lab,
				      if (is_monomorphic)
					  then eq_expdec
				      else DEC_MOD(fresh_var(),
					     SIGNAT_FUNCTOR(var_poly,sigpoly_eq,
							    SIGNAT_STRUCTURE(NONE,[SDEC(it_lab,eq_expdec)]),
							    oneshot_init TOTAL)))
			 in SOME(eq_sbnd, eq_sdec)
			 end)
	in val eq_sbnd_sdec_opts = map2 help (top_type_label,exp_con_eq_opt)
	end
	val type_sbnd_sdecs = (map4 (fn (l,v,c,k) => (SBND(l,BND_CON(v,c)),
						      (SDEC(l,DEC_CON(v,k,NONE)))))
			       (top_type_label,top_type_var, type_funs,type_kinds))
	fun interleave [] [] = []
	  | interleave (a::b) (NONE::d) = a::(interleave b d)
	  | interleave (a::b) ((SOME c)::d) = a::c::(interleave b d)
	  | interleave _ _ = error "interleave given unequal length lists"
	val first_sbnd_sdecs = interleave type_sbnd_sdecs eq_sbnd_sdec_opts
	val first_sbnds = map #1 first_sbnd_sdecs
	val first_sdecs = map #2 first_sbnd_sdecs
	val final_mod = MOD_STRUCTURE(first_sbnds @ (map #1 components))
	val final_sig = SIGNAT_STRUCTURE(NONE, first_sdecs @ (map #2 components))

      in (* (MOD_SEAL(final_mod,final_sig),final_sig) *)
	 (final_mod,final_sig)  
      end

 
    (* ------------------------------------------------------------------
      The datatype compiler for compiling a single datatype statement.
      ------------------------------------------------------------------ *)
    fun compile {context, typecompile,
		 datatycs : Ast.db list, withtycs : Ast.tb list, eq_compile} : (sbnd * sdec) list =
      let 
	(* ---- Find the strongly-connected components of datatypes. *)
	local
	  type node = int * (Symbol.symbol * Ast.tyvar list * (Ast.symbol * Ast.ty option) list)
	  val nodes = mapcount (fn (i,arg) => (i,db_strip arg)) datatycs
	  val syms = map (fn (_,(s,_,_)) => s) nodes
	  fun help (_,NONE) = []
	    | help (_,SOME ty) = let val s = free_tyc_ty(ty,fn _ => false)
				 in list_inter(s,syms)
				 end
	  fun lookupint [] tari = error "lookupint should not fail"
	    | lookupint ((i,info)::rest) tari = if (i = tari) then info
						else lookupint rest tari
	  fun lookup [] tars = error "lookup should not fail"
	    | lookup ((i,(s,tv,def))::rest) tars = if (s = tars) then i
						   else lookup rest tars
	in 
	  val numnodes = length nodes
	  fun get_edges (i,(s,tyvars,def)) = (map (fn x => (i,x))
					      (map (lookup nodes) (flatten (map help def))))
	  val edges = flatten (map get_edges nodes)
	  val comps = rev(GraphUtil.scc numnodes edges)
	  val sym_tyvar_def_listlist = mapmap (lookupint nodes) comps
	end (* local *)
        (* ---- call the main routine for each list of datatypes retaining the accumulated context *)
	fun loop context acc [] = rev acc
	  | loop context acc (std_list::rest) = 
	    let val (m,s) = driver(typecompile,context,std_list,withtycs, eq_compile)
		val l = fresh_open_internal_label "lbl"
		val v = fresh_var()
		val sbnd = SBND(l,BND_MOD(v,m))
		val sdec = SDEC(l,DEC_MOD(v,s))
		val sdec' = SDEC(l,DEC_MOD(v,SelfifySig(SIMPLE_PATH v,s)))
		val context' = add_context_sdecs(context,[sdec'])
		val acc' = (sbnd,sdec)::acc
	    in loop context' acc' rest
	    end
	val res = loop context [] sym_tyvar_def_listlist
      in res
      end

    (* ---------------- constructor LOOKUP RULES --------------------------- 
     --------------------------------------------------------- *)
    type lookup = (Il.context * Il.label list -> (Il.mod * Il.signat) option) 
    fun constr_lookup context (p : Ast.path) =
      (debugdo (fn () => (print "constr_lookup called with path = ";
			  AstHelp.pp_path p;
			  print "\nand with context = ";
			  pp_context context;
			  print "\n"));
       (case (modsig_lookup(context,map symbol_label p)) of
	    NONE=> (debugdo (fn () => print "constr_lookup modsig_lookup returned NONE\n");
		     NONE)
	  | SOME (path_mod,m,constr_sig as
		  (SIGNAT_STRUCTURE(_,[SDEC(maybe_mk,_)]))) =>
	    (if (eq_label(maybe_mk,mk_lab))
		then 
		    (case path_mod of
			 SIMPLE_PATH v => NONE
		       | COMPOUND_PATH (v,[]) => error "constr_lookup got empty COMPOUND PATH"
		       | COMPOUND_PATH (v,ls) => 
			     let val (short_p,name) = (COMPOUND_PATH(v, butlast ls), List.last ls)
				 val _ = debugdo (fn () => (print "BEFORE GETMODSIG with short_p = "; 
							    pp_path short_p;
							    print " and ctxt = \n"; pp_context context;
							    print "\n\n"))
				 val data_sig = GetModSig(context,path2mod short_p)
				 val _ = debugdo(fn () => print "AFTER GETMODSIG\n")
			     in
				 (case data_sig of
				      SIGNAT_STRUCTURE(_,(SDEC(type_lab,_)) :: (SDEC(maybe_expose,_)) :: _) =>
					  if (eq_label(maybe_expose,expose_lab))
					      then SOME{name = name,
							constr_sig = constr_sig,
							datatype_path = short_p,
							datatype_sig = data_sig}
					  else NONE
				    | _ => NONE)
			     end)
	    else NONE)
	     | _ => (debugdo (fn () => print "constr_lookup modsig_lookup returned SOME(unk)\n");
		     NONE))
	    handle NOTFOUND _ => (debugdo (fn () => print "constr_lookup failed ...\n");
				  NONE))

     fun is_const_constr signat = 
       (case signat of
	  SIGNAT_STRUCTURE(_,[SDEC(maybe_mk,dec)]) =>
	  let 
	    val _ = debugdo (fn () => (print "is_const_constr got signat of:\n";
				       pp_signat signat; print "\n"))
	    val con = (case dec of
			 DEC_MOD(_,SIGNAT_FUNCTOR(_,_,
				   SIGNAT_STRUCTURE(_,[SDEC(itlabel,
							    DEC_EXP(_,con))]),_)) => con
		       | DEC_EXP(_,con) => con
		       | _ => (pp_signat signat;
			       error "ill-formed constructor signature"))
	  in (case con of
		  CON_ARROW(_,CON_RECORD[],_) => true
		| _ => false)
	  end
	| _ => (pp_signat signat; error "This is not a valid constr_sig"))


     fun des_dec (d : dec) : ((Il.var option * Il.sdecs option) * Il.con) = 
       (case d of
	  DEC_MOD(_,SIGNAT_FUNCTOR(v,SIGNAT_STRUCTURE (_,sdecs),
				   SIGNAT_STRUCTURE(_,[SDEC(_,DEC_EXP(_,c))]),_)) => ((SOME v, SOME sdecs), c)
	| DEC_EXP(_,c) => ((NONE, NONE), c)
	| _ => error "des_dec")


     fun destructure_datatype_signature s : {name : Il.label,
					     var_poly : Il.var option,
					     sdecs_poly : Il.sdecs option,
					     arm_types : {name : Il.label, arg_type : Il.con option} list} 
       = 
       let fun bad () = (Ppil.pp_signat s;
			 error "ill-formed datatype_signature")
	   fun good (name,arm_sdecs) = 
	       let fun helper (sdec as 
			       (SDEC(name,DEC_MOD(_,SIGNAT_STRUCTURE (_,sdecs))))) = 
		   (case sdecs of
			[SDEC(_,mkdec)] =>
			    let 
				val (vso,mkc) = des_dec mkdec
				val argcon = case mkc of
				    CON_ARROW(c,_,_) => SOME c
				  | _ => NONE
			    in (vso,{name=name,arg_type=argcon})
			    end
		      | _ => error "ill-formed arm signature")
		     | helper _ = error "ill-formed arm signature"
		   val arm_types = map (#2 o helper) arm_sdecs
		   val (var_poly,sdecs_poly) = #1 (helper (hd arm_sdecs))
	       in {name = name,
		   var_poly = var_poly,
		   sdecs_poly = sdecs_poly,
		   arm_types = arm_types}
	       end
       in
	   (case s of
		SIGNAT_STRUCTURE(_,(SDEC(name,DEC_CON(_,_,_)))::
				 (SDEC(maybe_expose,_))::arm_sdecs) =>
		   (if (eq_label(maybe_expose,expose_lab))
			then good(name,arm_sdecs)
		    else bad())
	      | _ => bad())
       end

 fun instantiate_datatype_signature (datatype_path : Il.path,
				     datatype_sig : Il.signat,
				     context : Il.context,
				     polyinst : (Il.context * Il.sdecs -> Il.sbnd list * Il.sdecs * Il.con list)) =
   let 

     (* --- given the instantiated datatype and an option pair of
       ---   the formal var_poly and instantiated sdecs
       ---   and an sdec corresponding to a particular datatype arm dec
       --- returns the name of the construtor and a list for the carried types *)
     fun getconstr_namepatconoption datacon var_sdecs_option {name,arg_type} =
	 {name=name,
	  arg_type=(case (arg_type,var_sdecs_option) of
			(NONE,_) => NONE
		      | (SOME x,NONE) => SOME x
		      | (SOME x,SOME(var_poly,sdecs)) => SOME(remove_modvar_type(x,var_poly,sdecs)))}
	 
     val {name,var_poly,
	  sdecs_poly,arm_types} = destructure_datatype_signature datatype_sig
     val abstract_type = CON_MODULE_PROJECT(path2mod datatype_path,name)
     val expose_path = join_path_labels(datatype_path,[expose_lab])

     val (expose_exp, datacon,var_sdec_opt) = 
       (case (var_poly,sdecs_poly) of
	  (SOME vp,SOME sp) => let val (sbnds,sdecs,cons) = polyinst(context, sp)
				   val inst_con = CON_APP(abstract_type,
							   con_tuple_inject cons)
			       in (MODULE_PROJECT(MOD_APP(path2mod expose_path,MOD_STRUCTURE sbnds),it_lab),
				   inst_con,SOME(vp,sdecs))
			       end
	| (NONE,NONE) => (path2exp expose_path, abstract_type, NONE)
	| _ => error "var_poly/sdecs_poly mismatch")
   in  {instantiated_type = datacon, 
	arms = map (getconstr_namepatconoption datacon var_sdec_opt) arm_types,
	expose_exp = expose_exp}
   end


   fun old_exn_lookup context path : {name : Il.label,
				  carried_type : Il.con option} option =
       (case (modsig_lookup(context,map symbol_label path)) of
	  NONE=> NONE
	| SOME (path_mod,m,exn_sig as 
		SIGNAT_STRUCTURE (_,[SDEC(lab1,DEC_EXP(_,ctag)),SDEC(lab2,DEC_EXP(_,cmk))])) =>
	      if (eq_label(lab1,it_lab) andalso eq_label(lab2,mk_lab))
		  then 
		      let val name = (case path_mod of
					  SIMPLE_PATH v => error "exn_lookup found SIMPLE"
					| COMPOUND_PATH (v,[]) => error "exn_lookup found degenerate COMPUOND_PATH"
					| COMPOUND_PATH (v,ls) => List.last ls)
		      in (case (ctag,cmk) of 
			      (_, CON_ANY) => SOME {name=name, carried_type = NONE}
			    | (CON_TAG c, _) => SOME {name=name, carried_type = SOME c}
			    | _ => error_sig exn_sig "bad exn signature")
		      end
	      else NONE
	| _ => NONE)

   fun exn_lookup context path : {stamp : Il.exp,
				  carried_type : Il.con option} option =
       (case (modsig_lookup(context,map symbol_label path)) of
	  NONE=> NONE
	| SOME (path_mod,m,exn_sig as 
		SIGNAT_STRUCTURE (_,[SDEC(lab1,DEC_EXP(_,ctag)),SDEC(lab2,DEC_EXP(_,cmk))])) =>
	      if (eq_label(lab1,it_lab) andalso eq_label(lab2,mk_lab))
		  then 
		      (case (ctag,cmk) of 
			      (_, CON_ANY) => SOME {stamp=MODULE_PROJECT(path2mod path_mod,it_lab), 
						    carried_type = NONE}
			    | (CON_TAG c, _) => SOME {stamp=MODULE_PROJECT(path2mod path_mod,it_lab), 
						      carried_type = SOME c}
			    | _ => error_sig exn_sig "bad exn signature")
	      else NONE
	| _ => NONE)


  end
