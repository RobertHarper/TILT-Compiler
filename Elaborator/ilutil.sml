(* Il Utility *)
functor IlUtil(structure Ppil : PPIL
	       structure Il : IL
	       structure IlContext : ILCONTEXT
	       sharing Ppil.Il = IlContext.Il = Il)

  : ILUTIL = 
  struct
    structure Il = Il
    open Il IlContext Ppil 
    open Util Listops Name 
    open Prim Tyvar

    type tyvar = (context,con) Il.Tyvar.tyvar
    exception BUG
    exception UNIMP
    val debug = ref false
    fun debugdo t = if (!debug) then (t(); ()) else ()
    val error = fn s => error "ilutil.sml" s

    exception FAILURE of string
    exception NOTFOUND of string
    
    (* -------------------------------------------------------- *)
    (* --------------------- Misc helper functions ------------ *)
    fun fresh_named_con (ctxt,s) = CON_TYVAR (fresh_named_tyvar (ctxt,s))
    fun fresh_con ctxt = fresh_named_con (ctxt,"con")
    fun generate_tuple_symbol (i : int) = Symbol.labSymbol(Int.toString i)
    fun generate_tuple_label (i : int) = symbol_label(generate_tuple_symbol i)
      
    val mk_lab = internal_label "mk"
    val km_lab = internal_label "km"
    val it_lab = internal_label "it"
    val case_lab = internal_label "case"
    val expose_lab = internal_label "expose"
    val eq_lab = internal_label "eq"
    val functor_arg_lab = internal_label "functor_arg"


    local 
      fun geq_label (l1,l2) = (case (Name.compare_label(l1,l2)) of
				 GREATER => true
			       | EQUAL => true
			       | LESS => false)
      fun geq_labelpair ((l1,_),(l2,_)) = geq_label(l1,l2)
    in 
      fun sort_label (arg : label list) : label list = ListMergeSort.sort geq_label arg
      fun sort_labelpair (arg : (label * 'a) list) : (label * 'a) list = ListMergeSort.sort geq_labelpair arg
    end


    val unit_exp : exp = RECORD[]
    val fail_tag = fresh_named_tag "fail"
    val bind_tag = fresh_named_tag "bind"
    val match_tag = fresh_named_tag "match"
    val con_unit = CON_RECORD[]
    val fail_exp = SCON(tag(fail_tag,con_unit))
    val bind_exp = SCON(tag(bind_tag,con_unit))
    val match_exp = SCON(tag(match_tag,con_unit))
    val failexn_exp = EXN_INJECT(fail_exp,unit_exp)
    val bindexn_exp = EXN_INJECT(bind_exp,unit_exp)
    val matchexn_exp = EXN_INJECT(match_exp,unit_exp)
    fun con_tuple conlist = CON_RECORD(mapcount (fn (i,c) => 
						 (generate_tuple_label (i+1),c)) conlist)
    fun con_record symconlist = let fun help(s,c) = (symbol_label s,c)
				in CON_RECORD(sort_labelpair(map help  symconlist))
				end
    fun con_tuple_inject [c] = c
      | con_tuple_inject clist = CON_TUPLE_INJECT clist
    fun exp_tuple explist = let fun help(i,e) = (generate_tuple_label (i+1),e)
			    in  RECORD(mapcount help explist)
			    end
    val con_string = CON_VECTOR (CON_UINT W8)
    val con_bool = CON_MUPROJECT(0,CON_FUN([fresh_named_var "dummy"],
					   CON_SUM(NONE,[con_unit,con_unit])))
    val false_exp = ROLL(con_bool,INJ([con_unit,con_unit], 0, unit_exp))
    val true_exp = ROLL(con_bool,INJ([con_unit,con_unit], 1, unit_exp))
    fun make_lambda_help (a,var,con,rescon,e) 
      : exp * con = let val var' = fresh_var()
			val fbnd = FBND(var',var,con,rescon,e)
		    in (FIX(a,[fbnd]), CON_ARROW(con,rescon,oneshot_init a))
		    end
    fun make_total_lambda (var,con,rescon,e) = make_lambda_help(TOTAL,var,con,rescon,e)
    fun make_lambda (var,con,rescon,e) = make_lambda_help(PARTIAL,var,con,rescon,e)
    fun make_ifthenelse(e1,e2,e3,c) : exp = 
	let val con_dummy_false = CON_SUM(SOME 0,[con_unit,con_unit])
	    val con_dummy_true = CON_SUM(SOME 1,[con_unit,con_unit])
	    val e2' = #1(make_lambda(fresh_named_var "dummy",con_dummy_true,c,e2))
	    val e3' = #1(make_lambda(fresh_named_var "dummy",con_dummy_false,c,e3))
	in CASE([con_unit,con_unit],UNROLL(con_bool,e1),
		[SOME e3',SOME e2'],NONE)
	end

    fun make_let (ve_list : (var * exp) list, body) = LET (map BND_EXP ve_list,body)
    fun make_catch (e,con,efail) : exp =
       let 
	   val v = fresh_var()
	   val efail' = #1(make_lambda(fresh_named_var "dummy",con_unit,con,efail))
	   val outer = EXN_CASE(VAR v,[(fail_exp, con_unit, efail')],NONE)
       in HANDLE(e,#1 (make_lambda(v,CON_ANY,con,outer)))
       end

    fun con_deref (CON_TYVAR tyvar) = (case (tyvar_deref tyvar) of
					   NONE => CON_TYVAR tyvar
					 | SOME con => con)
      | con_deref c = c



    (* -------------------------------------------------------- *)
    (* ------------ Path manipulation functions ------------ *)
    fun join_path_labels (SIMPLE_PATH v, l) = COMPOUND_PATH(v,l)
      | join_path_labels (COMPOUND_PATH (v,ls), l) = COMPOUND_PATH(v,ls @ l)
    fun path2obj (var_maker : var -> 'a, mod_maker : mod * label -> 'a) p = 
      (case p of
	 (SIMPLE_PATH v) => var_maker v
       | (COMPOUND_PATH (v,ls)) => let fun loop [] _ = var_maker v
					 | loop [l] acc = mod_maker(acc,l)
					 | loop (l::rest) acc = loop rest (MOD_PROJECT(acc,l))
				   in loop ls (MOD_VAR v)
				   end)
    val path2mod = path2obj (MOD_VAR,MOD_PROJECT)
    val path2con = path2obj (CON_VAR, CON_MODULE_PROJECT)
    val path2exp = path2obj (VAR, MODULE_PROJECT)

    fun mod2path m = 
      let fun loop (MOD_VAR v) [] = SIMPLE_PATH v
	    | loop (MOD_VAR v) acc = COMPOUND_PATH(v,acc)
	    | loop (MOD_PROJECT (m,l)) acc = loop m (l::acc)
	    | loop m _ = (print "mod was: "; pp_mod m;
			  print "\n";
			  error "mod2path called on non-projection")
      in loop m []
      end
      fun eq_path(SIMPLE_PATH v1, SIMPLE_PATH v2) = eq_var(v1,v2)
        | eq_path(COMPOUND_PATH (v1,l1), COMPOUND_PATH(v2,l2)) = 
		eq_var(v1,v2) andalso eq_list(eq_label,l1,l2)
        | eq_path _ = false

      (* -------------------------------------------------------- *)
      (* ------------ Context manipulation functions ------------ *)



    local
      datatype state = STATE of ({bound_convar : var list,
				  bound_var : var list,
				  bound_modvar : var list} *
				 {exp_handler : exp * var list -> exp option,
				  con_handler : con * var list -> con option,
				  mod_handler : mod * var list -> mod option,
				  sdec_handler : state * sdec -> sdec option,
				  sig_handler : signat -> signat option})

      fun add_convars (STATE({bound_convar,bound_modvar,bound_var},handlers),tv) =
	  STATE({bound_convar = tv @ bound_convar,
		 bound_var = bound_var,
		 bound_modvar = bound_modvar},handlers)
      fun add_convar (h,v) = add_convars(h,[v])
      fun add_var (STATE({bound_convar,bound_modvar,bound_var},handlers),v) =
	  STATE({bound_convar = bound_convar,
		 bound_var = v :: bound_var,
		 bound_modvar = bound_modvar},handlers)

	
      fun add_modvar (STATE({bound_convar,bound_modvar,bound_var},handlers),v) = 
	  STATE({bound_convar = bound_convar,
		 bound_var = bound_var,
		 bound_modvar = v :: bound_modvar},handlers)

      fun f_exp (state as STATE({bound_var,...},{exp_handler,...})) (exp : exp) : exp =
	let val self = f_exp state
	in
	  (case (exp_handler (exp,bound_var)) of
	       SOME e => e
	     | NONE =>
	  (case exp of
	     (SCON _ | OVEREXP _) => exp
	   | VAR v => exp
	   | PRIM (p,cs) => PRIM(p, map (f_con state) cs)
	   | ILPRIM ilp => ILPRIM ilp
	   | APP (e1,e2) => APP(self e1, self e2)
	   | FIX (a,fbnds) => FIX(a,map (f_fbnd state) fbnds)
	   | RECORD (rbnds) => RECORD(map (f_rbnd state) rbnds)
	   | RECORD_PROJECT (e,l,c) => RECORD_PROJECT(self e,l,f_con state c)
	   | SUM_TAIL (c,e) => SUM_TAIL(f_con state c, self e)
	   | HANDLE (e1,e2) => HANDLE(self e1, self e2)
	   | RAISE (c,e) =>  RAISE(f_con state c, self e)
	   | LET (bnds,e) => let fun loop [] h = h
				   | loop ((BND_EXP(v,_))::rest) h = loop rest (add_var(h,v))
				   | loop ((BND_CON(v,_))::rest) h = loop rest (add_convar(h,v))
				   | loop ((BND_MOD(v,_))::rest) h = loop rest (add_modvar(h,v))
				 val state' = loop bnds state
			     in LET(map (f_bnd state) bnds, 
				    f_exp state' e)
			     end
	   | NEW_STAMP con => NEW_STAMP(f_con state con)
	   | EXN_INJECT (e1,e2) => EXN_INJECT(self e1, self e2)
	   | ROLL (c,e) => ROLL(f_con state c, self e)
	   | UNROLL (c,e) => UNROLL(f_con state c, self e)
	   | INJ (c,i,e) => INJ(map (f_con state) c, i, self e)
	   | CASE(c,earg,elist,edef) => let fun help NONE = NONE
					      | help (SOME e) = SOME(self e)
					in CASE(map (f_con state) c, 
						self earg, map help elist, help edef)
					end
	   | EXN_CASE(earg,ecelist,eopt) => let fun help (e1,c,e2) = (self e1, f_con state c, self e2)
						val eopt' = (case eopt of 
								 NONE => NONE
							       | SOME e => SOME (self e))
					    in EXN_CASE(self earg, map help ecelist, eopt')
					    end
	   | MODULE_PROJECT (m,l) => MODULE_PROJECT(f_mod state m,l)
	   | SEAL (e,c) =>  SEAL (self e, f_con state c)))
	end

      and f_con (state : state) (con : con) : con = 
	let 
	    val self = f_con state
	    val STATE({bound_convar,...},
		      {con_handler,...}) = state
	in
	  (case (con_handler (con,bound_convar)) of
	       SOME c => c
	     | NONE =>
		   (case con of
		       CON_TYVAR tyvar => (case (tyvar_deref tyvar) of
					       NONE => con
					     | SOME c => self c)
		     | CON_VAR _ => con
		     | (CON_OVAR ocon) => (self (CON_TYVAR (ocon_deref ocon)); con)
		     | (CON_INT _ | CON_FLOAT _ | CON_UINT _ | CON_ANY) => con
		     | CON_ARRAY c => CON_ARRAY (self c)
		     | CON_VECTOR c => CON_VECTOR (self c)
		     | CON_REF c => CON_REF (self c)
		     | CON_TAG c => CON_TAG (self c)
		     | CON_ARROW (c1,c2,complete) => CON_ARROW (self c1, self c2, complete)
		     | CON_APP (c1,c2) => CON_APP (self c1, self c2)
		     | CON_MUPROJECT (i,c) =>  CON_MUPROJECT (i, self c)
		     | CON_RECORD rdecs => CON_RECORD (map (f_rdec state) rdecs)
		     | CON_FLEXRECORD r => (* don't dereference until flex_handler called *)
			       (case (!r) of  
				    (FLEXINFO (stamp,true,rdecs)) => CON_RECORD (map (f_rdec state) rdecs)
				  | (FLEXINFO (stamp,false,rdecs)) => 
					let val res = map (f_rdec state) rdecs
				 val _ = r := FLEXINFO(stamp,false,res)
					in con
					end
				  | (INDIRECT_FLEXINFO rf) => self (CON_FLEXRECORD rf))
		     | CON_FUN (vars,c) => let val state' = add_convars(state,vars)
					   in CON_FUN(vars,f_con state' c)
					   end
		     | CON_SUM (iopt,clist) => CON_SUM (iopt,map self clist)
		     | CON_TUPLE_INJECT clist => CON_TUPLE_INJECT (map self clist)
		     | CON_TUPLE_PROJECT (i,c) =>  CON_TUPLE_PROJECT (i, self c)
		     | CON_MODULE_PROJECT (m,l) => CON_MODULE_PROJECT(f_mod state m, l)))
	end

      and f_mod (state as STATE({bound_modvar,...},
				{mod_handler,...})) (m : mod) : mod = 
      (case (mod_handler (m,bound_modvar)) of
	       SOME m => m
	     | NONE =>
	(case m of
	   MOD_VAR _ => m
	 | MOD_STRUCTURE sbnds => MOD_STRUCTURE (map (f_sbnd state) sbnds)
	 | MOD_FUNCTOR (v,s,m) => MOD_FUNCTOR (v, f_signat state s, f_mod state m)
	 | MOD_APP (m1,m2) => MOD_APP (f_mod state m1, f_mod state m2)
	 | MOD_PROJECT (m,l) => MOD_PROJECT(f_mod state m,l)
	 | MOD_LET (v,m1,m2) => MOD_LET (v, f_mod state m1, f_mod state m2)
	 | MOD_SEAL (m,s) => MOD_SEAL (f_mod state m, f_signat state s)))

      and f_signat (state as STATE(_,{sig_handler,...})) (s : signat) : signat = 
      (case (sig_handler s) of
	       SOME s => s
	     | NONE =>
	(case s of
	   SIGNAT_STRUCTURE (popt,sdecs) => SIGNAT_STRUCTURE (popt,map (f_sdec state) sdecs)
	 | SIGNAT_FUNCTOR (v,s1,s2,a) => SIGNAT_FUNCTOR(v, f_signat state s1, 
							   f_signat state s2, a)))

      and f_rbnd state (l,e) = (l, f_exp state e)
      and f_fbnd state (FBND(vname,varg,carg,cres,e)) : fbnd =
	   let val state' = add_var(state,varg)
	   in FBND(vname,varg,f_con state' carg,f_con state' cres,f_exp state' e)
	   end
      and f_rdec (state) (l,c) = (l, f_con state c)
      and f_sbnd (state) (SBND(l,bnd)) : sbnd = SBND(l, f_bnd state bnd)
      and f_sdec (state as STATE(_,{sdec_handler,...})) (sdec as (SDEC(l,dec))) : sdec = 
	  (case (sdec_handler (state,sdec)) of
	       NONE => SDEC(l, f_dec state dec)
	     | SOME x => x)
	       
      and f_bnd (state : state) (bnd : bnd) : bnd =
	(case bnd of
	   BND_EXP(v,e) => BND_EXP(v, f_exp state e)
	 | BND_MOD(v,m) => BND_MOD(v, f_mod state m)
	 | BND_CON(v,c) => BND_CON(v, f_con state c))

      and f_dec (state : state) (dec : dec) : dec =
	(case dec of
	   DEC_EXP(v,c) => DEC_EXP(v, f_con state c)
	 | DEC_MOD(v,s) => DEC_MOD(v, f_signat state s)
	 | DEC_CON(v,k,NONE) => dec
	 | DEC_CON(v,k,SOME c) => DEC_CON(v, k, SOME (f_con state c))
	 | DEC_EXCEPTION(n,c) => DEC_EXCEPTION(n, f_con state c))


      val default_bound_convar = []
      val default_bound_var = []
      val default_bound_modvar = []
      val default_bound = {bound_convar = default_bound_convar,
			   bound_var = default_bound_var,
			   bound_modvar = default_bound_modvar}
      fun default_flex_handler _ = false
      fun default_sdec_handler _ = NONE
      fun default_exp_handler _ = NONE
      fun default_con_handler _ = NONE
      fun default_mod_handler _ = NONE
      fun default_sig_handler _ = NONE

    in

      fun con_occurs(argcon : con, origtv : tyvar) : bool =
	let 
	  val occurs = ref false
	  fun con_handler (CON_TYVAR tv,_) = (if eq_tyvar(tv,origtv) 
						  then occurs := true else (); 
						      NONE)
	    | con_handler _ = NONE
	  val handlers = STATE(default_bound,
			       {
				sdec_handler = default_sdec_handler,
				exp_handler = default_exp_handler,
				con_handler = con_handler,
				mod_handler = default_mod_handler,
				sig_handler = default_sig_handler})
	in (f_con handlers argcon; !occurs)
	end

      fun con_free_convar (argcon : con) : var list = 
	let 
	  val free = ref []
	  fun con_handler (CON_VAR v,bound) = (if (member_eq(eq_var,v,bound) orelse
						      member_eq(eq_var,v,!free))
						      then ()
						  else free := (v::(!free));
						      NONE)
	    | con_handler _ = NONE
	  val handlers = STATE(default_bound,
			       {sdec_handler = default_sdec_handler,
				exp_handler = default_exp_handler,
				con_handler = con_handler,
				mod_handler = default_mod_handler,
				sig_handler = default_sig_handler})
	  val _ = f_con handlers argcon
	in !free
	end

      fun sig_free_conmodvar (argsig : signat) : var list * var list = 
	let 
	  val freecon = ref []
	  val freemod = ref []
	  fun con_handler (CON_VAR v,bound) = (if (member_eq(eq_var,v,bound) orelse
						      member_eq(eq_var,v,!freecon))
						      then ()
						  else freecon := (v::(!freecon));
						      NONE)
	    | con_handler _ = NONE
	  fun mod_handler (MOD_VAR v,bound) = (if (member_eq(eq_var,v,bound) orelse
						      member_eq(eq_var,v,!freemod))
						      then ()
						  else freemod := (v::(!freemod));
						      NONE)
	    | mod_handler _ = NONE
	  val handlers = STATE(default_bound,
			       {sdec_handler = default_sdec_handler,
				exp_handler = default_exp_handler,
				con_handler = con_handler,
				mod_handler = mod_handler,
				sig_handler = default_sig_handler})
	  val _ = f_signat handlers argsig
	in (!freecon, !freemod)
	end


      fun rebind_free_type_var(tv_stamp : stamp,
			       argcon : con, context, targetv : var) 
	  : ((context,con)Tyvar.tyvar * label * bool) list = 
	let 
	    val _ = debugdo (fn () => (print "rebind_free_type_var called on argcon = ";
				       pp_con argcon;
				       print "\n"))
	  val free_tyvar = ref ([] : (context,con) Tyvar.tyvar list)
	  fun con_handler (CON_TYVAR tv,_) = 
	      ((case (tyvar_deref tv) of
		    SOME _ => ()
		  | NONE => (if (not (member_eq(eq_tyvar,tv,!free_tyvar))
				 andalso not (tyvar_isconstrained tv)
				 andalso (tyvar_after tv_stamp tv))
				 then free_tyvar := (tv::(!free_tyvar))
			     else ()));
		    NONE)
	    | con_handler (CON_FLEXRECORD r,_) = (flex_handler r;
						NONE)
	    | con_handler _ = NONE

	  and flex_handler (r as (ref (FLEXINFO (stamp,resolved,rdecs)))) =
	      r := FLEXINFO(stamp,true,rdecs)
	    | flex_handler (ref (INDIRECT_FLEXINFO rf)) = flex_handler rf

	  val handlers = STATE(default_bound,
			       {sdec_handler = default_sdec_handler,
				exp_handler = default_exp_handler,
				con_handler = con_handler,
				mod_handler = default_mod_handler,
				sig_handler = default_sig_handler})
	  val _ = f_con handlers argcon
	  val res = map (fn tv => (tv, internal_label (tyvar2string tv),
				   tyvar_is_use_equal tv)) (!free_tyvar)
	  val _ = (map (fn (tv,lbl,useeq) => 
			 let val proj = CON_MODULE_PROJECT(MOD_VAR targetv, lbl)
			 in tyvar_set(tv,proj)
			 end)
		   res)
	in res
	end

      exception DEPENDENT
      fun remove_modvar_handlers(target_var : var, arg_sdecs : sdecs) : state =
	let 
	  fun con_handler (CON_TYVAR tyvar,_) =
	    (case (tyvar_deref tyvar) of
	       (SOME _) => NONE
	     | NONE => NONE
(*		   let 
		       val v = tyvar_getvar tyvar
		       fun loop [] = CON_TYVAR tyvar
			 | loop ((SDEC(cur_label,DEC_CON(curv,k,SOME c)))::rest) = 
			   if eq_var(v,curv) then c else loop rest
			 | loop (_::rest) = loop rest
		   in SOME(loop arg_sdecs)
		   end
*)
)
	    | con_handler (CON_MODULE_PROJECT(m,l),_) = con_proj_handler(m,l)
	    | con_handler _ = NONE
	  and con_proj_handler (m,argl) : con option = 
	    let 
		fun help (MOD_VAR v) acc = SOME(v,rev(argl::acc))
		  | help (MOD_PROJECT(m,l)) acc = help m (l::acc)
		  | help _ _ = NONE
	      fun search labs sdecs = 
		  (case (labs,sdecs) of
		       ([],_) => error "search in remove_modvar_* got no labels"
		     | ([l],[]) => raise DEPENDENT
		     | ([l],(SDEC(curl,DEC_CON(v,k,SOME c)))::rest) =>
			   if (eq_label(l,curl))
			       then c
			   else search [l] rest
		     | ([l],_::rest) => search [l] rest
		     | (l::ls,[]) => raise DEPENDENT
		     | (l::ls,(SDEC(curl,DEC_MOD(v,(SIGNAT_STRUCTURE (_,sdecs)))))::rest) =>
			   if (eq_label(l,curl))
			       then search ls sdecs
			   else search (l::ls) rest
		     | (l::ls,_::rest) => search (l::ls) rest)
	    in (case help m [] of
		    NONE => NONE
		| SOME(v,lbls) =>
		       if eq_var(v,target_var) 
			   then SOME(search lbls arg_sdecs)
		       else NONE)
	    end
	  fun sdec_handler (state,SDEC(l,DEC_CON(v,k,SOME c))) =
	      let val copt = (SOME(f_con state c)
			      handle DEPENDENT => NONE)
	      in SOME(SDEC(l,DEC_CON(v,k,copt)))
	      end
	    | sdec_handler (state,sdec) = NONE
	  val handlers = STATE(default_bound,
			       {sdec_handler = sdec_handler,
				exp_handler = default_exp_handler,
				con_handler = con_handler,
				mod_handler = default_mod_handler,
				sig_handler = default_sig_handler})
	in handlers
	end

      fun remove_modvar_type(c : con, target_var : var, sdecs : sdecs) : con = 
	  let val handlers = remove_modvar_handlers(target_var,sdecs)
	  in f_con handlers c
	      handle DEPENDENT => error "remove_modvar_type failed"
	  end
      fun remove_modvar_signat(s : signat, target_var : var, sdecs : sdecs) : signat = 
	  let val handlers = remove_modvar_handlers(target_var,sdecs)
	  in f_signat handlers s
	      handle DEPENDENT => error "remove_modvar_signat failed"
	  end
      fun remove_modvar_sdec(s : sdec, target_var : var, sdecs : sdecs) : sdec = 
	  let val handlers = remove_modvar_handlers(target_var,sdecs)
	  in f_sdec handlers s
	      handle DEPENDENT => error "remove_modvar_sdec failed"
	  end

      fun add_modvar_handlers(m : mod, sdecs : sdecs) =
	let 
	    fun loop (ea,ca,ma) [] = (ea,ca,ma)
	      | loop (ea,ca,ma) ((cur as (SDEC(l,dec)))::rest) =
		(case dec of
		     DEC_EXP (v,c) => loop ((v,MODULE_PROJECT(m,l))::ea,ca,ma) rest
		   | DEC_CON(v,k,copt) => loop (ea,(v,CON_MODULE_PROJECT(m,l))::ca,ma) rest
		   | DEC_MOD(v,s) => loop (ea,ca,(v,MOD_PROJECT(m,l))::ma) rest
		   | _ => loop (ea,ca,ma) rest)
	    val (exptable,contable,modtable) = loop ([],[],[]) sdecs
	    fun exp_handler (VAR var,bound) = 
		(case (assoc_eq(eq_var,var,exptable)) of
		     SOME e => SOME e
		   | NONE => NONE)
	      | exp_handler _ = NONE
	    fun con_handler (CON_VAR var,bound) = 
		(case (assoc_eq(eq_var,var,contable)) of
		     SOME c => SOME c
		   | NONE => NONE)
	      | con_handler _ = NONE
	    fun mod_handler (MOD_VAR modvar,bound) = 
		(case (assoc_eq(eq_var,modvar,modtable)) of
		     SOME m => SOME m
		   | NONE => NONE)
	      | mod_handler _ = NONE
	    val handlers = STATE(default_bound,
				 {sdec_handler = default_sdec_handler,
				  exp_handler = exp_handler,
				  con_handler = con_handler,
				  mod_handler = mod_handler,
				  sig_handler = default_sig_handler})
	in handlers
	end

      fun add_modvar_sig(s : signat, m : mod, sdecs : sdecs) : signat =
	let val handlers = add_modvar_handlers(m,sdecs)
	in f_signat handlers s
	end
      fun add_modvar_type(c : con, m : mod, sdecs : sdecs) : con = 
	let val handlers = add_modvar_handlers(m,sdecs)
	in f_con handlers c
	end

      fun con_subst_var_withproj(c : con, prev_sdecs : sdec list, m : mod) : con = 
	let fun strip [] = []
	      | strip ((SDEC (l, DEC_CON(v,_,_)))::rest) = (v,l) :: (strip rest)
	      | strip (_::rest) = strip rest
	    val table : (var * label) list= strip prev_sdecs
	    fun con_handler (CON_VAR var,bound) = 
	      (case (assoc_eq(eq_var,var,table)) of
		 SOME l => SOME(CON_MODULE_PROJECT(m,l))
	       | NONE => NONE)
	      | con_handler _ = NONE
	    val handlers = STATE(default_bound,
				 {sdec_handler = default_sdec_handler,
				  exp_handler = default_exp_handler,
				  con_handler = con_handler,
				  mod_handler = default_mod_handler,
				  sig_handler = default_sig_handler})
	in f_con handlers c
	end



      local
	  fun exp_handler table (VAR var,bound) = if (member_eq(eq_var,var,bound)) 
						      then NONE
						  else assoc_eq(eq_var,var,table)
	    | exp_handler _ _ = NONE
	  fun con_handler table (CON_VAR var,bound) = if (member_eq(eq_var,var,bound)) 
							  then NONE
						      else assoc_eq(eq_var,var,table)
	    | con_handler _ _ = NONE
	  fun ehandlers table = STATE(default_bound,
				     {sdec_handler = default_sdec_handler,
				      exp_handler = exp_handler table,
				      con_handler = default_con_handler,
				      mod_handler = default_mod_handler,
				      sig_handler = default_sig_handler})
	  fun chandlers table = STATE(default_bound,
				      {sdec_handler = default_sdec_handler,
				       exp_handler = default_exp_handler,
				       con_handler = con_handler table,
				       mod_handler = default_mod_handler,
				       sig_handler = default_sig_handler})
	  fun mod_handler table (MOD_VAR var,bound) = if (member_eq(eq_var,var,bound)) 
					       then NONE
					   else assoc_eq(eq_var,var,table)
	    | mod_handler _ _ = NONE
	  fun sig_handler handler_thunk table (SIGNAT_STRUCTURE (SOME p,sdecs)) = 
	      let val v = (case p of
			       SIMPLE_PATH v => v
			     | COMPOUND_PATH (v,_) => v)
		  val popt = (case (assoc_eq(eq_var,v,table)) of
				  NONE => SOME p
				| SOME _ => NONE)
	      in case (f_signat (handler_thunk()) (SIGNAT_STRUCTURE (NONE,sdecs))) of
		  (SIGNAT_STRUCTURE (_,sdecs)) => SOME(SIGNAT_STRUCTURE (popt,sdecs))
		| _ => error "f_signat changed shape"
	      end
	    | sig_handler _ _ _ = NONE
	  fun mhandlers table = 
	      let val self = fn () => mhandlers table
	      in STATE(default_bound,
		       {sdec_handler = default_sdec_handler,
			exp_handler = default_exp_handler,
			con_handler = default_con_handler,
			mod_handler = mod_handler table,
			sig_handler = sig_handler self table})
	      end
	  fun cmhandlers ctable mtable =
	      let val self = fn () => cmhandlers ctable mtable
	      in STATE(default_bound,
		       {sdec_handler = default_sdec_handler,
			exp_handler = default_exp_handler,
			con_handler = con_handler ctable,
			mod_handler = mod_handler mtable,
			sig_handler = sig_handler self mtable})
	      end
	  fun echandlers etable ctable =
	      let val self = fn () => echandlers etable ctable
	      in STATE(default_bound,
		       {sdec_handler = default_sdec_handler,
			exp_handler = exp_handler etable,
			con_handler = con_handler ctable,
			mod_handler = default_mod_handler,
			sig_handler = default_sig_handler})
	      end
      in 
	  fun exp_subst_expvar(arg,table) = f_exp (ehandlers table) arg
	  fun con_subst_expvar(arg,table) = f_con (ehandlers table) arg
	  fun mod_subst_expvar(arg,table) = f_mod (ehandlers table) arg
	  fun sig_subst_expvar(arg,table) = f_signat (ehandlers table) arg
	  fun exp_subst_convar(arg,table) = f_exp (chandlers table) arg
	  fun con_subst_convar(arg,table) = f_con (chandlers table) arg
	  fun mod_subst_convar(arg,table) = f_mod (chandlers table) arg
	  fun sig_subst_convar(arg,table) = f_signat (chandlers table) arg
	  fun exp_subst_modvar(arg,table) = f_exp (mhandlers table) arg
	  fun con_subst_modvar(arg,table) = f_con (mhandlers table) arg
	  fun mod_subst_modvar(arg,table) = f_mod (mhandlers table) arg
	  fun sig_subst_modvar(arg,table) = f_signat (mhandlers table) arg
	  fun con_subst_conmodvar(arg,ctable,mtable) = f_con (cmhandlers ctable mtable) arg
	  fun exp_subst_expconvar(arg,etable,ctable) = f_exp (echandlers etable ctable) arg
      end

      local
	  fun mod_handler table (MOD_VAR var,bound) = if (member_eq(eq_var,var,bound)) 
					       then NONE
					   else assoc_eq(eq_var,var,table)
	    | mod_handler _ _ = NONE
	  fun con_handler table (CON_VAR var,bound) = if (member_eq(eq_var,var,bound)) 
							  then NONE
						      else assoc_eq(eq_var,var,table)
	    | con_handler _ _ = NONE
	  fun sig_handler ctable mtable (SIGNAT_STRUCTURE (SOME p,sdecs)) = 
	      let val v = (case p of
			       SIMPLE_PATH v => v
			     | COMPOUND_PATH (v,_) => v)
		  val popt = (case (assoc_eq(eq_var,v,mtable)) of
				  NONE => SOME p
				| SOME _ => NONE)
	      in case (f_signat (handlers ctable mtable) (SIGNAT_STRUCTURE (NONE,sdecs))) of
		  (SIGNAT_STRUCTURE (_,sdecs)) => SOME(SIGNAT_STRUCTURE (popt,sdecs))
		| _ => error "f_signat changed shape"
	      end
	    | sig_handler _ _ _ = NONE
	  and handlers ctable mtable = 
	      STATE(default_bound,
		    {sdec_handler = default_sdec_handler,
		     exp_handler = default_exp_handler,
		     con_handler = con_handler ctable,
		     mod_handler = mod_handler mtable,
		     sig_handler = sig_handler ctable mtable})
      in 
	  fun exp_subst(arg,ctable,mtable) = f_exp (handlers ctable mtable) arg
	  fun con_subst(arg,ctable,mtable) = f_con (handlers ctable mtable) arg
	  fun mod_subst(arg,ctable,mtable) = f_mod (handlers ctable mtable) arg
	  fun sig_subst(arg,ctable,mtable) = f_signat (handlers ctable mtable) arg
      end

      fun con_subst_conapps (argcon, con_app_handler : (con * con -> con option)) : con = 
	let 
	  fun con_handler (CON_APP(c1,c2),_) = con_app_handler(c1,c2)
	    | con_handler _ = NONE
	  val handlers = STATE(default_bound,
			       {sdec_handler = default_sdec_handler,
				exp_handler = default_exp_handler,
				con_handler = con_handler,
				mod_handler = default_mod_handler,
				sig_handler = default_sig_handler})
	in f_con handlers argcon
	end

      fun sig_subst_allproj(argsig : signat, eproj, cproj, mproj, sdecer) : signat = 
	let 
	  fun sdec_handler(_,sdec) = sdecer sdec
	  fun exp_handler (MODULE_PROJECT(m,l),_) = eproj(m,l)
	    | exp_handler _ = NONE
	  fun con_handler (CON_MODULE_PROJECT(m,l),_) = cproj(m,l)
	    | con_handler _ = NONE
	  fun mod_handler (MOD_PROJECT(m,l),_) = mproj(m,l)
	    | mod_handler _ = NONE
	  val handlers = STATE(default_bound,
			       {sdec_handler = sdec_handler,
				exp_handler = exp_handler,
				con_handler = con_handler,
				mod_handler = mod_handler,
				sig_handler = default_sig_handler})
	in f_signat handlers argsig
	end

      fun subst_proj_handlers(eproj, cproj) =
	let 
	  fun exp_handler (MODULE_PROJECT(m,l),_) = eproj(m,l)
	    | exp_handler _ = NONE
	  fun con_handler (CON_MODULE_PROJECT(m,l),_) = cproj(m,l)
	    | con_handler _ = NONE
	  val handlers = STATE(default_bound,
			       {sdec_handler = default_sdec_handler,
				exp_handler = exp_handler,
				con_handler = con_handler,
				mod_handler = default_mod_handler,
				sig_handler = default_sig_handler})
	in handlers
	end

      fun exp_subst_proj(argexp : exp, eproj, cproj) : exp =
	  f_exp (subst_proj_handlers(eproj,cproj)) argexp

      fun con_constrain  (argcon, 
			  orig_con_handler,
			  param as {constrain : bool, 
				    stamp = stampopt : Il.Tyvar.stamp option,
				    eq_constrain : bool},
			  constrain_ctxts : context list) = 
	  let 
	      val tyvars = ref []
	      fun con_handler (c : con,bound : var list) = 
		  (case orig_con_handler c of
		      NONE => 
			  (case c of
			      CON_TYVAR tyvar =>
				  if (member_eq(eq_tyvar,tyvar,!tyvars))
				      then NONE
				  else (tyvars := tyvar :: (!tyvars);
					if constrain then tyvar_constrain tyvar else (); 
					debugdo (fn () => 
						 if (eq_constrain) 
						     then
							 (print "!!!eq_constraining tyvar ";
							  print (tyvar2string tyvar); print "\n")
						 else ());
					if eq_constrain then tyvar_use_equal tyvar else (); 
					    tyvar_addctxts(tyvar,constrain_ctxts);
					    (case stampopt of
						 SOME stamp => update_stamp(tyvar,stamp)
					       | NONE => ());
						 NONE)
			    | CON_FLEXRECORD r => (flex_handler r; SOME c)
			    | _ => NONE)
		    | someopt => someopt)
	      and flex_handler (r as (ref (FLEXINFO (stamp,resolved,rdecs)))) =
		  (app (fn (_,c) => con_constrain(c,orig_con_handler,param,constrain_ctxts)) rdecs;
		   (case stampopt of
			SOME s => r := FLEXINFO(stamp_join(s,stamp),resolved,rdecs)
		      | NONE => ()))
		| flex_handler (ref (INDIRECT_FLEXINFO rf)) = flex_handler rf
	      val handlers = STATE(default_bound,
				   {sdec_handler = default_sdec_handler,
				      exp_handler = default_exp_handler,
				      con_handler = con_handler,
				      mod_handler = default_mod_handler,
				      sig_handler = default_sig_handler})
	  in (f_con handlers argcon; ())
	  end


      fun ConApply (c1,c2) = 
	(case (c1,c2) of
	     (CON_FUN([var],c),CON_TUPLE_INJECT[arg_con]) => con_subst_convar(c, [(var,arg_con)])
	   | (CON_FUN([var],c),arg_con) => con_subst_convar(c, [(var,arg_con)])
	   | (CON_FUN(vars,c),CON_TUPLE_INJECT(arg_cons)) => con_subst_convar(c, zip vars arg_cons)
	   | _ => (print "ConApply got bad arguments\nc1 = ";
		   pp_con c1; print "\nc2 = "; 
		   pp_con c2; print "\n";
		   error "ConApply got bad arguments"))

      fun mod_free_expvar module : var list = 
	  let
	      val free = ref ([] : var list)
	      fun exp_handler (VAR var,bound) = (if (not (member_eq(eq_var,var,bound))
						     andalso (not (member_eq(eq_var,var,!free))))
						     then free := (var :: (!free)) else ();
							 NONE)
		| exp_handler _ = NONE
	      val handlers = STATE(default_bound,
				   {sdec_handler = default_sdec_handler,
				    exp_handler = exp_handler,
				    con_handler = default_con_handler,
				    mod_handler = default_mod_handler,
				    sig_handler = default_sig_handler})
	      val _ = f_mod handlers module
	  in !free
	  end

       fun sig_mod_handler (argsig : signat, mod_handler) : signat =
	  let
	      val handlers = STATE(default_bound,
				   {sdec_handler = default_sdec_handler,
				    exp_handler = default_exp_handler,
				    con_handler = default_con_handler,
				    sig_handler = default_sig_handler,
				    mod_handler = fn (m,_) => mod_handler m})
	  in f_signat handlers argsig
	  end

       fun sig_all_handle (argsig : signat, exp_handler, con_handler, mod_handler) : signat =
	  let
	      val handlers = STATE(default_bound,
				   {sdec_handler = default_sdec_handler,
				    exp_handler = fn (e,_) => exp_handler e,
				    con_handler = fn (c,_) => con_handler c,
				    mod_handler = fn (m,_) => mod_handler m,
				    sig_handler = default_sig_handler})
	  in f_signat handlers argsig
	  end

       fun con_all_handle (argcon : con, exp_handler, con_handler, mod_handler) : con =
	  let
	      val handlers = STATE(default_bound,
				   {sdec_handler = default_sdec_handler,
				    exp_handler = fn (e,_) => exp_handler e,
				    con_handler = fn (c,_) => con_handler c,
				    mod_handler = fn (m,_) => mod_handler m,
				    sig_handler = default_sig_handler})
	  in f_con handlers argcon
	  end


    fun con_free_modvar con : var list = 
	  let
	      val free = ref ([] : var list)
	      fun mod_handler (MOD_VAR var,bound) = (print "modvar_handler got var = "; pp_var var;
						print "\n  and bound = ";
						pp_list pp_var' bound ("",", ","",false);
						print "\n";
						if (member_eq(eq_var,var,bound) orelse
						    member_eq(eq_var,var,!free))
						    then () else free := (var :: (!free));
							NONE)
		| mod_handler _ = NONE
	      val handlers = STATE(default_bound,
				   {sdec_handler = default_sdec_handler,
				    exp_handler = default_exp_handler,
				    con_handler = default_con_handler,
				    mod_handler = mod_handler,
				    sig_handler = default_sig_handler})
	      val _ = f_con handlers con
	      val _ = (print "!free is "; pp_list pp_var' (!free) ("",", ","",false);
		       print "\n");
	  in !free
	  end

      fun subst_var (target_bnd, bnds) =
	  let 
	      fun loop [] e c m = (e,c,m)
		| loop ((BND_EXP(v,e))::rest) ee cc mm = loop rest ((v,e)::ee) cc mm
		| loop ((BND_CON(v,c))::rest) ee cc mm = loop rest ee ((v,c)::cc) mm
		| loop ((BND_MOD(v,m))::rest) ee cc mm = loop rest ee cc ((v,m)::mm)
	      val (etable,ctable,mtable) = loop bnds [] [] [] 
	      fun con_handler (CON_VAR var,bound) = if (member_eq(eq_var,var,bound))
							then NONE else assoc_eq(eq_var,var,ctable)
		| con_handler _ = NONE
	      fun exp_handler (VAR var,bound) = if (member_eq(eq_var,var,bound))
						then NONE else assoc_eq(eq_var,var,etable)
		| exp_handler _ = NONE
	      fun mod_handler (MOD_VAR var,bound) = if (member_eq(eq_var,var,bound))
						then NONE else assoc_eq(eq_var,var,mtable)
		| mod_handler _ = NONE
	      fun sig_handler (SIGNAT_STRUCTURE (SOME p,sdecs)) = 
		  let val v = (case p of
				   SIMPLE_PATH v => v
				 | COMPOUND_PATH (v,_) => v)
		      val popt = (case (assoc_eq(eq_var,v,mtable)) of
				      NONE => SOME p
				    | SOME _ => NONE)
		  in case (f_signat (handlers()) (SIGNAT_STRUCTURE (NONE,sdecs))) of
		      (SIGNAT_STRUCTURE (_,sdecs)) => SOME(SIGNAT_STRUCTURE (popt,sdecs))
		    | _ => error "f_signat changed shape"
		  end
		| sig_handler _ = NONE
	      and handlers () = STATE(default_bound,
				   {sdec_handler = default_sdec_handler,
				    exp_handler = exp_handler,
				    con_handler = con_handler,
				    mod_handler = mod_handler,
				    sig_handler = sig_handler})
	      val handlers = handlers()
	  in (case target_bnd of
		  BND_EXP(v,e) => BND_EXP(v,f_exp handlers e)
		| BND_CON(v,c) => BND_CON(v,f_con handlers c)
		| BND_MOD(v,m) => BND_MOD(v,f_mod handlers m))
	  end

      exception UNRESOLVED
      fun make_resolved_handlers () =
	  let 
	      fun exp_handler (OVEREXP (_,_,eshot),_) = (case (oneshot_deref eshot) of
							   SOME _ => NONE
							 | NONE => raise UNRESOLVED)
		| exp_handler _ = NONE
	      fun con_handler (CON_TYVAR tv,_) = (case (tyvar_deref tv) of
						    SOME _ => NONE
						  | NONE => raise UNRESOLVED)
		| con_handler _ = NONE
	      val handlers = STATE(default_bound,
				   {sdec_handler = default_sdec_handler,
				    exp_handler = exp_handler,
				    con_handler = con_handler,
				    mod_handler = default_mod_handler,
				    sig_handler = default_sig_handler})
	  in handlers
	  end

      fun mod_resolved m = (f_mod (make_resolved_handlers()) m; true) 
	  handle UNRESOLVED => false

      fun sig_resolved s = (f_signat (make_resolved_handlers()) s; true) 
	  handle UNRESOLVED => false
			    

      fun make_size_handlers () =
	  let 
	      val count = ref 0
	      fun inc() = (count := (!count) + 1; NONE)
	      fun exp_handler _ = inc()
	      fun con_handler _ = inc()
	      fun mod_handler _ = inc()
	      val handlers = STATE(default_bound,
				   {sdec_handler = default_sdec_handler,
				    exp_handler = exp_handler,
				    con_handler = con_handler,
				    mod_handler = mod_handler,
				    sig_handler = default_sig_handler})
	  in (count,handlers)
	  end
      
      fun mod_size m = 
	let val (count,handlers) = make_size_handlers()
	    val _=  f_mod handlers m
	in !count
	end
    
      fun sig_size s = 
	  let val (count,handlers) = make_size_handlers()
	      val _=  f_signat handlers s
	  in !count
	  end

    end   (* local *)


    fun error_obj pp_obj (obj_str : string) filename obj (str : string) = 
	(print filename; ": Error while evaluating "; print obj_str; print ": ";
	 print str; print "\n";
	 pp_obj obj; print "\n\n";
	 Util.error filename str)

    val error_exp = fn e => fn s => error_obj pp_exp "expression" e s
    val error_con = fn c => fn s => error_obj pp_con "constructor" c s
    val error_mod = fn m => fn s => error_obj pp_mod  "module" m s
    val error_sig = fn sg => fn s => error_obj pp_signat "signature" sg s

    fun to_eq_lab type_lab =
	  let
	      val type_str = label2string type_lab
	      val eq_str = type_str ^ "_eq"
	      val eq_lab = internal_label eq_str
	  in
	      eq_lab
	  end


     fun make_inline_module (context,m) : mod option = 
	 (case m of
	     MOD_STRUCTURE sbnds => 
		 let 
		     exception FAIL
		     val freevars = mod_free_expvar m
		     fun doit (ee,cc,mm) (SBND(l,bnd)) = 
			 SBND(l,case bnd of
			      BND_EXP(v,e) => BND_EXP(v, ee e)
			    | BND_CON(v,c) => BND_CON(v, cc c)
			    | BND_MOD(v,m) => BND_MOD(v, mm m))
		     fun loop [] : sbnd list = []
		       | loop ((sbnd as SBND(l,bnd))::rest) =
			 case bnd of
			     BND_EXP(v,e) => 
				 if member_eq(eq_var,v,freevars)
				     then (print "cannot inline module due to v = ";
					   pp_var v; print "\n";
					   raise FAIL)
				 else sbnd::(loop rest)
			   | BND_CON (v,c) => 
				 let 
				     fun ee earg = exp_subst_convar(earg, [(v,c)])
				     fun cc carg = con_subst_convar(carg, [(v,c)])
				     fun mm marg = mod_subst_convar(marg, [(v,c)])
				     val rest' = map (doit (ee,cc,mm)) rest
				 in sbnd::(loop rest')
				 end
			   | BND_MOD (v,m) =>
				 let 
				     fun ee earg = exp_subst_modvar(earg, [(v,m)])
				     fun cc carg = con_subst_modvar(carg, [(v,m)])
				     fun mm marg = mod_subst_modvar(marg, [(v,m)])
				     val rest' = map (doit (ee,cc,mm)) rest
				     val sbnd = SBND(l,BND_MOD(v,case make_inline_module (context,m) of
							       SOME m => m
							     | NONE => raise FAIL))
				 in sbnd::(loop rest')
				 end
		 in
		     SOME (MOD_STRUCTURE(loop sbnds))
		     handle FAIL => NONE
		 end
	   | MOD_FUNCTOR _ => SOME m
	   | _ => NONE)





  end;
