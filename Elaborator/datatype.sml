(* Datatype compiler and destructures of datatype signatures. *)
functor Datatype(structure Il : IL
		 structure IlStatic : ILSTATIC
		 structure IlUtil : ILUTIL
		 structure Ppil : PPIL
		 structure AstHelp : ASTHELP
		 structure IlLookup : ILLOOKUP
		 sharing IlLookup.Il = Ppil.Il = IlUtil.Il = IlStatic.Il = Il)
    : DATATYPE  = 
  struct

    structure Il = Il
    open AstHelp Il IlStatic IlUtil Ppil 
    open Util Listops Name IlLookup Tyvar

  
    val error = fn s => error "datatype.sml" s
    val error_sig = fn signat => fn s => error_sig "datatype.sml" signat s 
    val debug = ref false
    fun debugdo t = if (!debug) then (t(); ()) else ()

    fun con_tuple_inject [c] = c
      | con_tuple_inject clist = CON_TUPLE_INJECT clist

    (* ------------------------------------------------------------------
      The datatype compiler for compiling a single strong-connected type.
      ------------------------------------------------------------------ *)
    fun driver (xty : Il.context * Ast.ty -> Il.con,
		context : context, 
		std_list : (Symbol.symbol * Ast.tyvar list * (Symbol.symbol * Ast.ty option) list) list,
		withtycs : Ast.tb list) : (mod * signat) =
      let 
	val _ = (case withtycs of 
		   [] => ()
		 | _ => error "xdatbind does not handle withtypes yet")
	(* --------- create labels and variables; compute indices --------------- *)
	val p = length std_list
	val tc_list = map (fn (s,_,_) => symbol_label s) std_list
	val varty_list = map (fn _ => fresh_named_var "vty") std_list
	val vardt_list = map (fn _ => fresh_named_var "vdt") std_list
	val tv_list = flatten (map (fn (_,tyvars,_) => map (symbol_label o tyvar_strip) tyvars) std_list)
	val k = length tv_list
	val varpoly_list = map (fn _ => fresh_named_var "poly") tv_list
	val lbl_poly = fresh_open_internal_label "lbl_poly"
	val (var_poly,var_all) = (fresh_named_var "vpoly",fresh_named_var "vall")
	val tys : Ast.ty option list list = map (fn (_,_,def) => map #2 def) std_list
	val ids = map (fn (_,_,def) => map #1 def) std_list
	(* --------- compute sig_poly, sig_poly+, and new context ------------ *)
	local
	  val sdecs = (map2 (fn (tv,v) => SDEC(tv,DEC_CON(v,KIND_TUPLE 1,NONE))) 
		       (tv_list, varpoly_list))
	  val temp = ((map2 (fn (tv,vp) => (tv,vp,KIND_TUPLE 1)) (tv_list,varpoly_list)) @
		      (map2 (fn (tc,vty) => (tc,vty,KIND_ARROW(k,1))) (tc_list,varty_list)))
	  fun folder ((l,v,k),context) = add_context_convar(context,l,v,k,NONE)
	in
	  val sigpoly = SIGNAT_STRUCTURE sdecs
	  val context' = foldl folder context temp
	  val is_monomorphic = (length sdecs = 0)
	  val is_onedatatype = (length std_list = 1)
	end
	(* --------- compute all the types ----------------------------------- *)
	val cons = mapmap (fn NONE => con_unit | SOME ty => xty(context',ty)) tys
	local
	  val app_subst = (map2 (fn (vty,vdt) => (vty,CON_VAR vdt))
			   (varty_list, vardt_list))
	  fun conapper(CON_VAR tarv,c2) : con option = 
	    let 
	      fun loop ((vty,dt_con)::rest) = if eq_var(vty,tarv) then SOME dt_con else loop rest
		| loop [] = NONE
	    in loop app_subst
	    end
	    | conapper _ = NONE
	in val cons' = mapmap (fn con => con_subst_conapps(con,conapper)) cons
	end 
	val con_sum = map (fn x => CON_SUM(NONE,x)) cons'
	val con_all = CON_FUN(vardt_list,con_tuple_inject(con_sum))
	local 
	  val con = if (is_monomorphic) 
		      then CON_VAR(var_all)
		    else CON_APP(CON_VAR(var_all),
				 con_tuple_inject(map (fn tv => CON_MODULE_PROJECT(MOD_VAR var_poly,tv))
						  tv_list))
	in
	  val con_dt = if (is_onedatatype)
			   then [con]
		       else map0count (fn i => CON_TUPLE_PROJECT(i,con)) p
	end
	val varpoly_subst = (map2 (fn (vp,tv) => (vp,CON_MODULE_PROJECT(MOD_VAR var_poly,tv)))
			 (varpoly_list, tv_list))
	val vardt_subst = (zip vardt_list con_dt) 
	val cons'' = mapmap (fn con => con_subst_var(con,vardt_subst @ varpoly_subst)) cons'

	  
	(* ---- compute the constructors, destructors, and cases ------------- *)
	val exp_con_mk = 
	    map3count (fn (i,cons_i'' : con list, con_dt_i,tys_i) => 
		       let val sumcon = CON_SUM(NONE,cons_i'')
			   val var = fresh_var()
			   fun help (j, _, NONE) = (ROLL(con_dt_i,INJ(cons_i'',j,unit_exp)),con_dt_i)
			     | help (j, con_ij'', _) = make_lambda(var,con_ij'',con_dt_i,
								   ROLL(con_dt_i,
									INJ(cons_i'',j,VAR var)))
		       in map2count help (cons_i'', tys_i)
		       end) (cons'',con_dt,tys)
	val exp_con_km = 
	    map2count (fn (i,cons_i'',con_dt_i) =>
		       let val var = fresh_var()
			   val comp = oneshot_init(if (length cons_i'' = 1) then TOTAL else PARTIAL)
			   fun help (j,con_ij'') = make_lambda(var,con_dt_i,con_ij'',
							       PROJ(cons_i'',j,UNROLL(con_dt_i,VAR var)))
		       in mapcount help cons_i''
		       end) (cons'',con_dt)
	val exp_con_case = 
	    map2count (fn (i,cons_i'',con_dt_i) =>
		       let
			   val sumtype = CON_SUM(NONE, cons_i'')
			   val var' = fresh_var()
		       in make_lambda(var',con_dt_i,sumtype,UNROLL(con_dt_i,VAR var'))
		       end) (cons'',con_dt)
	val exp_con_expose = 
	    map2count (fn (i,cons_i'',con_dt_i) =>
		       let
			   val sumtype = CON_SUM(NONE, cons_i'')
			   val var' = fresh_var()
		       in make_lambda(var',con_dt_i,sumtype,UNROLL(con_dt_i,VAR var'))
		       end) (cons'',con_dt)
	(* --------- put the result together now ------------- *)
	fun help (i,tc,id_i,expcon_mk_i,expcon_km_i,(exp_case_i,con_case_i),(exp_expose_i,con_expose_i)) = 
	  let 
	    (* ----- do the tycon -------- *)
	    val (con1,kind1) = if (is_monomorphic)
				 then (if (is_onedatatype)
					   then CON_VAR(var_all)
				       else CON_TUPLE_PROJECT(i,CON_VAR(var_all)), 
				       KIND_TUPLE 1)
			       else (CON_FUN(varpoly_list,
					     let val temp = CON_APP(CON_VAR(var_all),
									 con_tuple_inject
									 (map CON_VAR varpoly_list))
					     in if (is_onedatatype)
						 then temp
						else CON_TUPLE_PROJECT(i,temp)
					     end),
				     KIND_ARROW(k,1))
	    val sbnd1 = SBND(tc,BND_CON(fresh_var(),con1))
	    val sdec1 = SDEC(tc,DEC_CON(fresh_var(),kind1,SOME con1))
	    (* ----- do the case -------- *)
	    val sbnd2 = SBND(case_lab,
			     BND_MOD(fresh_var(),
				     MOD_FUNCTOR(var_poly,sigpoly,
						 MOD_STRUCTURE[SBND(it_lab,BND_EXP(fresh_var(),
										     exp_case_i))])))
	    val sdec2 = SDEC(case_lab,
			     DEC_MOD(fresh_var(),
					SIGNAT_FUNCTOR(var_poly,sigpoly,
						       SIGNAT_STRUCTURE[SDEC(it_lab,DEC_EXP(fresh_var(),
											      con_case_i))],
						       oneshot_init TOTAL)))
	    (* ----- do the expose -------- *)
	    val expose_expbnd = BND_EXP(fresh_var(),exp_expose_i)
	    val expose_expdec = DEC_EXP(fresh_var(),con_expose_i)
	    val sbnd3 = SBND(expose_lab,
			     if (is_monomorphic)
				 then expose_expbnd
			     else
				 BND_MOD(fresh_var(),
					 MOD_FUNCTOR(var_poly,sigpoly,
						     MOD_STRUCTURE[SBND(it_lab,expose_expbnd)])))
	    val sdec3 = SDEC(expose_lab,
			     if (is_monomorphic)
				 then expose_expdec
			     else DEC_MOD(fresh_var(),
					  SIGNAT_FUNCTOR(var_poly,sigpoly,
							 SIGNAT_STRUCTURE[SDEC(it_lab,expose_expdec)],
							 oneshot_init TOTAL)))
	    (* ----- do the mk and km ---- *)
	    fun inner_help (id_ij,(exp_mk_ij,con_mk_ij),(exp_km_ij,con_km_ij)) = 
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
						  SIGNAT_STRUCTURE[SDEC(it_lab,sig_dec)],
						  oneshot_init TOTAL)))
		  end
		val (mk_bnd,mk_dec) = maker exp_mk_ij con_mk_ij 
		val (km_bnd,km_dec) = maker exp_km_ij  con_km_ij 
		val module = MOD_STRUCTURE[SBND(mk_lab,mk_bnd),
					   SBND(km_lab,km_bnd)]
		val signat = SIGNAT_STRUCTURE[SDEC(mk_lab,mk_dec),
					      SDEC(km_lab,km_dec)]
	      in (SBND(id_ij,BND_MOD(fresh_var(),module)),
		  SDEC(id_ij,DEC_MOD(fresh_var(),signat)))
	      end
	    val temp = map3 inner_help (map symbol_label id_i, expcon_mk_i, expcon_km_i)
	    val (sbnds,sdecs) = (map #1 temp, map #2 temp)
	    val inner_mod = MOD_STRUCTURE(sbnd1::sbnd2::sbnd3::sbnds)
	    val inner_sig = SIGNAT_STRUCTURE(sdec1::sdec2::sdec3::sdecs)
	    val tc_star = openlabel tc
	  in (SBND(tc_star,BND_MOD(fresh_var(),inner_mod)),
	      SDEC(tc_star,DEC_MOD(fresh_var(),inner_sig)))
	  end
	val temp = map6count help (tc_list, ids, exp_con_mk, exp_con_km, exp_con_case, exp_con_expose)
	local
	  val lbl = internal_label "lbl"
	in 
	  val help_con = if (is_onedatatype)
			     then CON_MUPROJECT(0,con_all)
			 else con_tuple_inject(mapcount (fn (i,_) => CON_MUPROJECT(i,con_all)) ids)
	  val first_sbnd = SBND(lbl,BND_CON(var_all,
					    if (is_monomorphic)
					      then help_con
					    else CON_FUN(varpoly_list,help_con)))
	  val first_sdec = SDEC(lbl,DEC_CON(var_all,if (is_monomorphic) 
							 then KIND_TUPLE p else KIND_ARROW(k,p), NONE))
	end
	val final_mod = MOD_STRUCTURE(first_sbnd :: (map #1 temp))
	val final_sig = SIGNAT_STRUCTURE(first_sdec :: (map #2 temp))
      in (* (MOD_SEAL(final_mod,final_sig),final_sig) *)
	 (final_mod,final_sig) 
      end

 
    (* ------------------------------------------------------------------
      The datatype compiler for compiling a single datatype statement.
      ------------------------------------------------------------------ *)
    fun compile {context, typecompile,
		 datatycs : Ast.db list, withtycs : Ast.tb list} : (sbnd * sdec) list =
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
	fun loop context acc [] = acc
	  | loop context acc (std_list::rest) = 
	    let val (m,s) = driver(typecompile,context,std_list,withtycs)
		val l = fresh_open_internal_label "lbl"
		val v = fresh_var()
		val sbnd = SBND(l,BND_MOD(v,m))
		val sdec = SDEC(l,DEC_MOD(v,s))
		val context' = add_context_sdecs(context,[sdec])
		val acc' = (sbnd,sdec)::acc
	    in loop context' acc' rest
	    end
      in loop context [] sym_tyvar_def_listlist
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
		  SIGNAT_STRUCTURE[SDEC(clab1,_),SDEC(clab2,_)]) =>
	    (case path_mod of
		 SIMPLE_PATH v => NONE
	       | COMPOUND_PATH (v,[]) => error "constr_lookup got empty COMPOUND PATH"
	       | COMPOUND_PATH (v,ls) => 
		     let val (short_p,name) = (COMPOUND_PATH(v, butlast ls), List.last ls)
			 val decs = context2decs context
			 val _ = debugdo (fn () => (print "BEFORE GETMODSIG with short_p = "; pp_path short_p;
						    print " and decs = \n"; pp_decs decs;
						    print "\n\n"))
			 val data_sig = GetModSig(decs,path2mod short_p)
			 val _ = debugdo(fn () => print "AFTER GETMODSIG\n")
		     in
			 (case data_sig of
			      SIGNAT_STRUCTURE(_ :: (SDEC(dlab2,_)) :: (SDEC(dlab3,_)) :: _) =>
				  if (eq_label(dlab2,case_lab) andalso (eq_label(dlab3,expose_lab)))
				      then SOME{name = name,
						constr_sig = constr_sig,
						datatype_path = short_p,
						datatype_sig = data_sig}
				  else NONE
			    | _ => NONE)
		     end)
	     | _ => (debugdo (fn () => print "constr_lookup modsig_lookup returned SOME(unk)\n");
		     NONE))
	    handle NOTFOUND _ => (debugdo (fn () => print "constr_lookup failed ...\n");
				  NONE))

     fun is_const_constr signat = 
       (case signat of
	  SIGNAT_STRUCTURE[_,SDEC(kmlabel,dec)] =>
	  let 
	    val _ = debugdo (fn () => (print "is_const_constr got signat of:\n";
				       pp_signat signat; print "\n"))
	    val con = (case dec of
			 DEC_MOD(_,SIGNAT_FUNCTOR(_,_,SIGNAT_STRUCTURE[SDEC(itlabel,
									    DEC_EXP(_,con))],_)) => con
		       | DEC_EXP(_,con) => con
		       | _ => (pp_signat signat;
			       error "ill-formed constructor signature"))
	  in (case con of
		CON_ARROW(_,res,_) => eq_con([],con_unit,res)
	      | _ => false)
	  end
	| _ => (pp_signat signat; error "This is not a valid constr_sig"))


     fun des_dec (d : dec) : ((Il.var option * Il.sdecs option) * Il.con) = 
       (case d of
	  DEC_MOD(_,SIGNAT_FUNCTOR(v,SIGNAT_STRUCTURE sdecs,
				   SIGNAT_STRUCTURE[SDEC(_,DEC_EXP(_,c))],_)) => ((SOME v, SOME sdecs), c)
	| DEC_EXP(_,c) => ((NONE, NONE), c)
	| _ => error "des_dec")


     fun destructure_datatype_signature s : {name : Il.label,
					     abstract_type : Il.con,
					     var_poly : Il.var option,
					     sdecs_poly : Il.sdecs option,
					     arm_types : {name : Il.label, arg_type : Il.con option} list} 
       = 
       (case s of
	  SIGNAT_STRUCTURE((SDEC(name,DEC_CON(_,_,SOME abstract_type)))::
			   (SDEC(caselabel,_)):: 			   
			   (SDEC(exposelabel,_))::
			   arm_sdecs) => let fun helper (sdec as 
							 (SDEC(name,DEC_MOD(_,SIGNAT_STRUCTURE sdecs)))) = 
						(case sdecs of
						 [SDEC(_,mkdec), SDEC(_,kmdec)] => 
						   let 
						       val (vso,mkc) = des_dec mkdec
						       val (_,kmc) = des_dec kmdec
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
					     abstract_type = abstract_type,
					     var_poly = var_poly,
					     sdecs_poly = sdecs_poly,
					     arm_types = arm_types}
					 end
	| _ => (Ppil.pp_signat s;
		error "ill-formed datatype_signature"))
      

 fun instantiate_datatype_signature (datatype_path : Il.path,
				     datatype_sig : Il.signat,
				     context : Il.context,
				     polyinst : (Il.decs * Il.sdecs -> Il.sbnd list * Il.sdecs * Il.con list)) =
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
	 

     val {name,abstract_type,var_poly,
	  sdecs_poly,arm_types} = destructure_datatype_signature datatype_sig
     val expose_path = join_path_labels(datatype_path,[expose_lab])
     val case_path = join_path_labels(datatype_path,[case_lab])
     val (expose_exp, case_exp, datacon,var_sdec_opt) = 
       (case (var_poly,sdecs_poly) of
	  (SOME vp,SOME sp) => let val (sbnds,sdecs,cons) = polyinst(context2decs context, sp)
				   val inst_con = ConApply(abstract_type,
							   con_tuple_inject 
							   cons)
			       in (MODULE_PROJECT(MOD_APP(path2mod expose_path,MOD_STRUCTURE sbnds),it_lab),
				   MODULE_PROJECT(MOD_APP(path2mod case_path,MOD_STRUCTURE sbnds),it_lab),
				   inst_con,SOME(vp,sdecs))
			       end
	| (NONE,NONE) => (path2exp expose_path, path2exp case_path, abstract_type, NONE)
	| _ => error "var_poly/sdecs_poly mismatch")
   in  {instantiated_type = datacon, 
	arms = map (getconstr_namepatconoption datacon var_sdec_opt) arm_types,
	case_exp = case_exp,
	expose_exp = expose_exp}
   end


   fun old_exn_lookup context path : {name : Il.label,
				  carried_type : Il.con option} option =
       (case (modsig_lookup(context,map symbol_label path)) of
	  NONE=> NONE
	| SOME (path_mod,m,exn_sig as 
		SIGNAT_STRUCTURE [SDEC(lab1,DEC_EXP(_,ctag)),SDEC(lab2,DEC_EXP(_,cmk))]) =>
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
		SIGNAT_STRUCTURE [SDEC(lab1,DEC_EXP(_,ctag)),SDEC(lab2,DEC_EXP(_,cmk))]) =>
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
