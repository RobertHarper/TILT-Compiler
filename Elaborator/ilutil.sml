(* Il Utility. *)
functor IlUtil(structure Ppil : PPIL
	       structure Il : IL
	       sharing Ppil.Il = Il)

  : ILUTIL = 
  struct
    structure Il = Il
    open Il Ppil 
    open Util Listops Name 
    open Prim Tyvar

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


    val unit_exp : exp = RECORD[]
    val fail_ttag = fresh_named_tag "fail"
    val bind_ttag = fresh_named_tag "bind"
    val match_ttag = fresh_named_tag "match"
    val con_unit = CON_RECORD[]
    val fail_tag = TAG(fail_ttag,con_unit)
    val bind_tag = TAG(bind_ttag,con_unit)
    val match_tag = TAG(match_ttag,con_unit)
    val failexn_exp = EXN_INJECT(fail_tag,unit_exp)
    val bindexn_exp = EXN_INJECT(bind_tag,unit_exp)
    val matchexn_exp = EXN_INJECT(match_tag,unit_exp)
    fun con_tuple conlist = CON_RECORD(mapcount (fn (i,c) => (generate_tuple_label (i+1),c)) conlist)
    fun con_record symconlist = CON_RECORD(map (fn (s,c) => (symbol_label s,c))  symconlist)
    fun exp_tuple explist = RECORD(mapcount (fn (i,e) => (generate_tuple_label (i+1),e)) explist)
    val con_string = CON_VECTOR (CON_UINT W8)
    val con_bool = CON_MUPROJECT(0,CON_FUN([fresh_named_var "dummy"],CON_SUM(NONE,[con_unit,con_unit])))
    val false_exp = ROLL(con_bool,INJ([con_unit,con_unit], 0, unit_exp))
    val true_exp = ROLL(con_bool,INJ([con_unit,con_unit], 1, unit_exp))
    fun make_ifthenelse(e1,e2,e3) : exp = CASE([con_unit,con_unit],UNROLL(con_bool,e1),
					       [SOME e3,SOME e2],NONE)
    fun make_lambda (var,con,rescon,e) 
      : exp * con = let val var' = fresh_var()
			val fbnd = FBND(var',var,con,rescon,e)
		    in (FIX([fbnd],var'), CON_ARROW(con,rescon,oneshot_init PARTIAL))
		    end

    fun make_let (ve_list : (var * exp) list, body : exp) : exp = LET (map BND_EXP ve_list,body)
    fun make_catch (e,con,e') : exp =
       let 
	   val v = fresh_var()
	   val outer = EXN_CASE(VAR v,[(fail_tag, con_unit, e')],NONE)
       in HANDLE(e,#1 (make_lambda(v,CON_ANY,con,outer)))
       end

    fun con_deref (CON_TYVAR tyvar) = (case (tyvar_deref tyvar) of
					   NONE => CON_TYVAR tyvar
					 | SOME con => con)
      | con_deref c = c
    local 
      fun geq_label (l1,l2) = (case (is_label_barred l1, is_label_barred l2) of
				 (true,false) => true
			       | (false,true) => false
			       | (_,_) => String.>(label2string l1, label2string l2))
      fun geq_labelpair ((l1,_),(l2,_)) = geq_label(l1,l2)
    in 
      fun sort_label (arg : label list) : label list = ListMergeSort.sort geq_label arg
      fun sort_labelpair (arg : (label * 'a) list) : (label * 'a) list = ListMergeSort.sort geq_labelpair arg
    end



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


      (* -------------------------------------------------------- *)
      (* ------------ Context manipulation functions ------------ *)
      fun Context_Get_FixityTable (CONTEXT c) : fixity_table = 
	let 
	    fun help (CONTEXT_SDEC(SDEC(_, DEC_FIXITY vflist))) = vflist
	      | help _ = []
	    val res = List.concat (map help c)
	    val _ = debugdo (fn () => (print "Context_Get_FixityTable called.  result is\n";
				       pp_dec (DEC_FIXITY res);
				       print "called with context = \n";
				       pp_context (CONTEXT c);
				       print "\n"))
	in  res 
	end
      fun Context_Get_BoundConvars (CONTEXT c) : var list = 
	let 
	  fun sdechelp (SDEC(l,d)) = 
	   (case (is_label_open l, d) of
	      (true,DEC_MOD(v,s)) => sighelp s
	    | (_,DEC_CON (v,_,_)) => [v]
	      | (_,(DEC_FIXITY _ | 
		    DEC_EXCEPTION _ | DEC_EXP _ | DEC_MOD _)) => [])
	  and sighelp (SIGNAT_STRUCTURE sdecs) = flatten(map sdechelp sdecs)
	    | sighelp (SIGNAT_FUNCTOR _) = []
	  fun loop [] = []
	    | loop (CONTEXT_SDEC(SDEC(l,dec))::rest) = 
	      (case dec of 
		   (DEC_CON(v,k,co)) => v :: (loop rest)
		 | (DEC_MOD(v, s)) => if (is_label_open l) then (sighelp s) @ (loop rest) else loop rest
		 | _ => loop rest)
	    | loop ((CONTEXT_INLINE _ | CONTEXT_SCOPED_TYVAR _ |
		     CONTEXT_SIGNAT _)::rest) = loop rest
	in loop c
	end
      
      fun Context_Get_ScopedConvars (CONTEXT c) : Symbol.symbol list = 
       let 
	 fun sdechelp (SDEC(l,d)) = 
	   (case (is_label_open l, d) of
	      (true,DEC_MOD(v,s)) => sighelp s
	    | (_,(DEC_CON _ | DEC_FIXITY _ | 
		  DEC_EXCEPTION _ | DEC_EXP _ | DEC_MOD _)) => [])
	 and sighelp (SIGNAT_STRUCTURE sdecs) = flatten(map sdechelp sdecs)
	   | sighelp (SIGNAT_FUNCTOR _) = []
	 fun loop [] = []
	   | loop (CONTEXT_SDEC(SDEC(l,(DEC_MOD(v, s))))::rest) = 
	     if (is_label_open l) then (sighelp s) @ (loop rest)
	     else loop rest
	   | loop ((CONTEXT_SDEC _)::rest) = loop rest
	   | loop ((CONTEXT_SCOPED_TYVAR s)::rest) = s @ (loop rest)
	   | loop (((CONTEXT_INLINE _) | (CONTEXT_SIGNAT _))::rest) = loop rest
       in loop c
       end

     fun add_context_dec(CONTEXT c, l, dec) = CONTEXT((CONTEXT_SDEC(SDEC(l,dec)))::c)
     fun add_context_var(c, l, v, con) = add_context_dec(c,l,DEC_EXP(v,con))
     fun add_context_module(c, l, v, signat) = add_context_dec(c,l,DEC_MOD(v,signat))
     fun add_context_convar(c, l, v, kind, conopt) = add_context_dec(c,l,DEC_CON(v,kind,conopt))
     fun add_context_inline(CONTEXT c, l, v, inline) = CONTEXT((CONTEXT_INLINE(l,v,inline))::c)
     fun add_context_scoped_tyvars(CONTEXT c, syms) = CONTEXT((CONTEXT_SCOPED_TYVAR syms):: c)
     fun add_context_entries(CONTEXT c, e : context_entry list) : context = CONTEXT(e @ c)
     fun add_context_sdecs(CONTEXT c, sdecs : sdecs) : context = CONTEXT((map CONTEXT_SDEC sdecs) @ c)
     fun add_context_signat(c, l, v, signat) = add_context_entries(c,[CONTEXT_SIGNAT(l,v,signat)])

  

    local
      (* 
       1st arg: bound tyvars; 
       2nd arg: function accepts (var * var list) and 
                  returns a con (typically, returns a var.fresh_label);
       3rg arg: function takes term level var and returns a expression
       4th arg: function takes mod level var and returns a module
       5th arg: func accepts (mod * label) from CON_MPROJ and returns a con
       6th arg: function accepts (con * con) returns con option *)
      datatype state = STATE of ({bound_convar : var list,
				  bound_var : var list,
				  bound_modvar : var list} *
				 {sdec_handler : state * sdec -> sdec option,
				  tyvar_handler : (decs,con) tyvar -> con option,
				  flex_handler : flexinfo ref -> bool,
				  convar_handler : (var * var list) -> con option,
				  var_handler : (var * var list) -> exp option,
				  modvar_handler : (var * var list) -> mod option,
				  con_proj_handler : (mod * label) -> con option,
				  con_app_handler : (con * con) -> con option,
				  exp_proj_handler : (mod * label) -> exp option})

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

      fun f_exp (state as STATE({bound_var,...},{exp_proj_handler,var_handler,...})) (exp : exp) : exp =
	let val self = f_exp state
	in
	  (case exp of
	     (SCON _ | OVEREXP _) => exp
	   | VAR v => (case (var_handler(v,bound_var)) of
			   NONE => exp
			 | SOME e => e)
	   | PRIM (p,cs) => PRIM(p, map (f_con state) cs)
	   | APP (e1,e2) => APP(self e1, self e2)
(*	   | SEQ elist => SEQ(map self elist) *)
	   | FIX (fbnds,var) => FIX(map (f_fbnd state) fbnds,var)
	   | RECORD (rbnds) => RECORD(map (f_rbnd state) rbnds)
	   | RECORD_PROJECT (e,l,c) => RECORD_PROJECT(self e,l,f_con state c)
	   | SUM_TAIL (c,e) => SUM_TAIL(f_con state c, self e)
	   | HANDLE (e1,e2) => HANDLE(self e1, self e2)
	   | RAISE (c,e) =>  RAISE(f_con state c, self e)
	   | LET (bnds,e) => let fun loop [] h = h
				   | loop ((BND_EXP(v,_))::rest) h = loop rest (add_var(h,v))
				   | loop ((BND_CON(v,_))::rest) h = loop rest (add_convar(h,v))
				   | loop ((BND_MOD(v,_))::rest) h = loop rest (add_modvar(h,v))
				   | loop (_::rest) h = loop rest h
				 val state' = loop bnds state
			     in LET(map (f_bnd state) bnds, 
				    f_exp state' e)
			     end
	   | NEW_STAMP con => NEW_STAMP(f_con state con)
	   | EXN_INJECT (e1,e2) => EXN_INJECT(self e1, self e2)
	   | MK_REF (exp) => MK_REF(self exp )
	   | GET (e) => GET(self e)
	   | SET (e1,e2) => SET(self e1, self e2)
	   | ROLL (c,e) => ROLL(f_con state c, self e)
	   | UNROLL (c,e) => UNROLL(f_con state c, self e)
	   | INJ (c,i,e) => INJ(map (f_con state) c, i, self e)
	   | PROJ (c,i,e) => PROJ (map (f_con state) c, i, self e)
	   | TAG (n,c) => TAG (n, f_con state c)
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
	   | MODULE_PROJECT (m,l) => (case (exp_proj_handler(m,l)) of
					NONE => MODULE_PROJECT(f_mod state m,l)
				      | SOME e => e)
	   | SEAL (e,c) =>  SEAL (self e, f_con state c))
	end

      and f_con (state : state) (con : con) : con = 
	let 
	    val self = f_con state
	    val STATE({bound_convar,...},
		      {convar_handler,con_app_handler,con_proj_handler,
		       tyvar_handler,flex_handler,...}) = state
	in
	  (case con of
	     CON_TYVAR tyvar => (case (tyvar_handler tyvar) of
				       (SOME c) => self c
				     | NONE => (case (tyvar_deref tyvar) of
						    NONE => con
						  | SOME c => self c))
	   | (CON_VAR var) => (case convar_handler(var,bound_convar) of
				   NONE => con
				 | SOME c => c)
	   | (CON_OVAR ocon) => (self (CON_TYVAR (ocon_deref ocon)); con)
	   | (CON_INT _ | CON_FLOAT _ | CON_UINT _ | CON_ANY) => con
	   | CON_ARRAY c => CON_ARRAY (self c)
	   | CON_VECTOR c => CON_VECTOR (self c)
	   | CON_REF c => CON_REF (self c)
	   | CON_TAG c => CON_TAG (self c)
	   | CON_ARROW (c1,c2,complete) => CON_ARROW (self c1, self c2, complete)
	   | CON_APP (c1,c2) => (case con_app_handler(c1,c2) of
				   NONE => CON_APP (self c1, self c2)
				 | SOME c => c)
	   | CON_MUPROJECT (i,c) =>  CON_MUPROJECT (i, self c)
	   | CON_RECORD rdecs => CON_RECORD (map (f_rdec state) rdecs)
	   | CON_FLEXRECORD r => (* don't dereference until flex_handler called *)
		 if (flex_handler r)
		     then con
		 else
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
	   | CON_MODULE_PROJECT (m,l) => case con_proj_handler(m,l) of
					    NONE => CON_MODULE_PROJECT(f_mod state m, l)
					  | SOME c => c)
	end

      and f_mod (state as STATE({bound_modvar,...},{modvar_handler,...})) (m : mod) : mod = 
	(case m of
	   MOD_VAR v => (case modvar_handler (v,bound_modvar) of
			     NONE => m
			   | SOME m' => m')
	 | MOD_STRUCTURE sbnds => MOD_STRUCTURE (map (f_sbnd state) sbnds)
	 | MOD_FUNCTOR (v,s,m) => MOD_FUNCTOR (v, f_signat state s, f_mod state m)
	 | MOD_APP (m1,m2) => MOD_APP (f_mod state m1, f_mod state m2)
	 | MOD_PROJECT (m,l) => MOD_PROJECT (f_mod state m, l)
	 | MOD_SEAL (m,s) => MOD_SEAL (f_mod state m, f_signat state s))

      and f_signat (state : state) (s : signat) : signat = 
	(case s of
	   SIGNAT_STRUCTURE sdecs => SIGNAT_STRUCTURE (map (f_sdec state) sdecs)
	 | SIGNAT_FUNCTOR (v,s1,s2,comp) => SIGNAT_FUNCTOR(v, f_signat state s1, f_signat state s2, comp))

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
	 | BND_CON(v,c) => BND_CON(v, f_con state c)
	 | BND_FIXITY _ => bnd)

      and f_dec (state : state) (dec : dec) : dec =
	(case dec of
	   DEC_EXP(v,c) => DEC_EXP(v, f_con state c)
	 | DEC_MOD(v,s) => DEC_MOD(v, f_signat state s)
	 | DEC_CON(v,k,NONE) => dec
	 | DEC_CON(v,k,SOME c) => DEC_CON(v, k, SOME (f_con state c))
	 | DEC_EXCEPTION(n,c) => DEC_EXCEPTION(n, f_con state c) 
	 | DEC_FIXITY _ => dec)

      val default_bound_convar = []
      val default_bound_var = []
      val default_bound_modvar = []
      val default_bound = {bound_convar = default_bound_convar,
			   bound_var = default_bound_var,
			   bound_modvar = default_bound_modvar}
      fun default_convar_handler _ = NONE
      fun default_tyvar_handler _ = NONE
      fun default_flex_handler _ = false
      fun default_var_handler _ = NONE
      fun default_sdec_handler _ = NONE
      fun default_modvar_handler _ = NONE
      fun default_con_proj_handler _ = NONE
      fun default_con_app_handler _ = NONE
      fun default_exp_proj_handler _ = NONE
      val default_handler = {convar_handler = default_convar_handler,
			     tyvar_handler = default_tyvar_handler,
			     flex_handler = default_flex_handler,
			     var_handler = default_var_handler,
			     sdec_handler = default_sdec_handler,
			     modvar_handler = default_modvar_handler,
			     con_proj_handler = default_con_proj_handler,
			     con_app_handler = default_con_app_handler,
			     exp_proj_handler = default_exp_proj_handler}
	  

    in

      fun con_occurs(argcon : con, origtv : (decs,con) tyvar) : bool =
	let 
	  val occurs = ref false
	  fun tyvar_handler tv = (if eq_tyvar(tv,origtv) 
				      then occurs := true else (); 
					  NONE)
	  val handlers = STATE(default_bound,
			       {convar_handler = default_convar_handler,
				tyvar_handler = tyvar_handler,
				flex_handler = default_flex_handler,
				var_handler = default_var_handler,
				sdec_handler = default_sdec_handler,
				modvar_handler = default_modvar_handler,
				con_proj_handler = default_con_proj_handler,
				con_app_handler = default_con_app_handler,
				exp_proj_handler = default_exp_proj_handler})
	in (f_con handlers argcon; !occurs)
	end

      fun con_free_convar (argcon : con) : var list = 
	let 
	  val free = ref []
	  fun convar_handler (v,bound) = (if (member_eq(eq_var,v,bound) orelse
					      member_eq(eq_var,v,!free))
					      then ()
					  else free := (v::(!free));
					      NONE)
	  val handlers = STATE(default_bound,
			       {convar_handler = convar_handler,
				tyvar_handler = default_tyvar_handler,
				flex_handler = default_flex_handler,
				var_handler = default_var_handler,
				sdec_handler = default_sdec_handler,
				modvar_handler = default_modvar_handler,
				con_proj_handler = default_con_proj_handler,
				con_app_handler = default_con_app_handler,
				exp_proj_handler = default_exp_proj_handler})
	  val _ = f_con handlers argcon
	in !free
	end


      fun rebind_free_type_var(tv_stamp : stamp,
			       argcon : con, context, targetv : var) 
	  : ((decs,con)Tyvar.tyvar * label * bool) list = 
	let 
	    val _ = debugdo (fn () => (print "rebind_free_type_var called on argcon = ";
				       pp_con argcon;
				       print "\n"))
	  val bound_convar = Context_Get_BoundConvars(context)
	  val free_tyvar = ref ([] : (decs,con) Tyvar.tyvar list)
	  fun tyvar_handler tv = ((case (tyvar_deref tv) of
					    SOME _ => ()
					  | NONE => (if (not (member_eq(eq_tyvar,tv,!free_tyvar))
							 andalso not (tyvar_isconstrained tv)
							 andalso (tyvar_after tv_stamp tv))
						       then free_tyvar := (tv::(!free_tyvar))
						     else ()));
					 NONE)
	  fun flex_handler (r as (ref (FLEXINFO (stamp,resolved,rdecs)))) =
	      (r := FLEXINFO(stamp,true,rdecs);
	       false)
	  val handlers = STATE(default_bound,
			       {var_handler = default_var_handler,
				sdec_handler = default_sdec_handler,
				modvar_handler = default_modvar_handler,
				tyvar_handler = tyvar_handler,
				flex_handler = flex_handler,
				convar_handler = default_convar_handler,
				con_proj_handler = default_con_proj_handler,
				con_app_handler = default_con_app_handler,
				exp_proj_handler = default_exp_proj_handler})
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
	  fun tyvar_handler tyvar =
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
	  fun con_proj_handler (m,argl) : con option = 
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
		     | (l::ls,(SDEC(curl,DEC_MOD(v,SIGNAT_STRUCTURE sdecs)))::rest) =>
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
			       {tyvar_handler = tyvar_handler,
				flex_handler = default_flex_handler,
				var_handler = default_var_handler,
				convar_handler = default_convar_handler,
				sdec_handler = sdec_handler,
				modvar_handler = default_modvar_handler,
				con_proj_handler = con_proj_handler,
				con_app_handler = default_con_app_handler,
				exp_proj_handler = default_exp_proj_handler})
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
	    fun var_handler (var,bound) = 
		(case (assoc_eq(eq_var,var,exptable)) of
		     SOME e => SOME e
		   | NONE => NONE)
	    fun convar_handler (convar,bound) = 
		(case (assoc_eq(eq_var,convar,contable)) of
		     SOME c => SOME c
		   | NONE => NONE)
	    fun modvar_handler (modvar,bound) = 
		(case (assoc_eq(eq_var,modvar,modtable)) of
		     SOME m => SOME m
		   | NONE => NONE)
	    val handlers = STATE(default_bound,
				 {tyvar_handler = default_tyvar_handler,
				  flex_handler = default_flex_handler,
				  var_handler = var_handler,
				  convar_handler = convar_handler,
				  sdec_handler = default_sdec_handler,
				  modvar_handler = modvar_handler,
				  con_proj_handler = default_con_proj_handler,
				  con_app_handler = default_con_app_handler,
				  exp_proj_handler = default_exp_proj_handler})
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
	    fun convar_handler (convar,bound) = 
	      (case (assoc_eq(eq_var,convar,table)) of
		 SOME l => SOME(CON_MODULE_PROJECT(m,l))
	       | NONE => NONE)
	    val handlers = STATE(default_bound,
				 {tyvar_handler = default_tyvar_handler,
				  flex_handler = default_flex_handler,
				  var_handler = default_var_handler,
				  convar_handler = convar_handler,
				  sdec_handler = default_sdec_handler,
				  modvar_handler = default_modvar_handler,
				  con_proj_handler = default_con_proj_handler,
				  con_app_handler = default_con_app_handler,
				  exp_proj_handler = default_exp_proj_handler})
	in f_con handlers c
	end

      fun con_subst_var(argcon : con, table : (var * con) list) : con = 
	let
	  fun convar_handler (var,bound) = assoc_eq(eq_var,var,table)
	  val handlers = STATE(default_bound,
			       {tyvar_handler = default_tyvar_handler,
				flex_handler = default_flex_handler,
				convar_handler = convar_handler,
				var_handler = default_var_handler,
				sdec_handler = default_sdec_handler,
				modvar_handler = default_modvar_handler,
				con_proj_handler = default_con_proj_handler,
				con_app_handler = default_con_app_handler,
				exp_proj_handler = default_exp_proj_handler})
	in f_con handlers argcon
	end


      local
	  fun var_handler table (var,bound) = if (member_eq(eq_var,var,bound)) 
						  then NONE
					      else assoc_eq(eq_var,var,table)
	  fun handlers table = STATE(default_bound,
			       {tyvar_handler = default_tyvar_handler,
				flex_handler = default_flex_handler,
				var_handler = var_handler table,
				convar_handler = default_convar_handler,
				sdec_handler = default_sdec_handler,
				modvar_handler = default_modvar_handler,
				con_proj_handler = default_con_proj_handler,
				con_app_handler = default_con_app_handler,
				exp_proj_handler = default_exp_proj_handler})
      in 
	  fun exp_subst_expvar(arg,table) = f_exp (handlers table) arg
	  fun exp_subst_var(arg,table) = f_exp (handlers table) arg
	  fun con_subst_expvar(arg,table) = f_con (handlers table) arg
	  fun mod_subst_expvar(arg,table) = f_mod (handlers table) arg
	  fun sig_subst_expvar(arg,table) = f_signat (handlers table) arg
      end


      local
	  fun convar_handler table (var,bound) = if (member_eq(eq_var,var,bound)) 
					       then NONE
					   else assoc_eq(eq_var,var,table)
	  fun handlers table = STATE(default_bound,
			       {tyvar_handler = default_tyvar_handler,
				flex_handler = default_flex_handler,
				var_handler = default_var_handler,
				convar_handler = convar_handler table,
				sdec_handler = default_sdec_handler,
				modvar_handler = default_modvar_handler,
				con_proj_handler = default_con_proj_handler,
				con_app_handler = default_con_app_handler,
				exp_proj_handler = default_exp_proj_handler})
      in 
	  fun exp_subst_convar(arg,table) = f_exp (handlers table) arg
	  fun con_subst_convar(arg,table) = f_con (handlers table) arg
	  fun mod_subst_convar(arg,table) = f_mod (handlers table) arg
	  fun sig_subst_convar(arg,table) = f_signat (handlers table) arg
      end

      local
	  fun modvar_handler table (var,bound) = if (member_eq(eq_var,var,bound)) 
					       then NONE
					   else assoc_eq(eq_var,var,table)
	  fun handlers table = STATE(default_bound,
			       {tyvar_handler = default_tyvar_handler,
				flex_handler = default_flex_handler,
				var_handler = default_var_handler,
				convar_handler = default_convar_handler,
				sdec_handler = default_sdec_handler,
				modvar_handler = modvar_handler table,
				con_proj_handler = default_con_proj_handler,
				con_app_handler = default_con_app_handler,
				exp_proj_handler = default_exp_proj_handler})
      in 
	  fun exp_subst_modvar(arg,table) = f_exp (handlers table) arg
	  fun con_subst_modvar(arg,table) = f_con (handlers table) arg
	  fun mod_subst_modvar(arg,table) = f_mod (handlers table) arg
	  fun sig_subst_modvar(arg,table) = f_signat (handlers table) arg
      end

      fun con_subst_conapps (argcon, con_app_handler : (con * con -> con option)) : con = 
	let 
	  val handlers = STATE(default_bound,
			       {tyvar_handler = default_tyvar_handler,
				flex_handler = default_flex_handler,
				var_handler = default_var_handler,
				convar_handler = default_convar_handler,
				sdec_handler = default_sdec_handler,
				modvar_handler = default_modvar_handler,
				con_proj_handler = default_con_proj_handler,
				con_app_handler = con_app_handler,
				exp_proj_handler = default_exp_proj_handler})
	in f_con handlers argcon
	end

      fun exp_subst_proj(argexp : exp, exp_proj_handler, con_proj_handler) : exp = 
	let 
	  val handlers = STATE(default_bound,
			       {tyvar_handler = default_tyvar_handler,
				flex_handler = default_flex_handler,
				convar_handler = default_convar_handler,
				var_handler = default_var_handler,
				sdec_handler = default_sdec_handler,
				modvar_handler = default_modvar_handler,
				con_proj_handler = con_proj_handler,
				con_app_handler = default_con_app_handler,
				exp_proj_handler = exp_proj_handler})
	in f_exp handlers argexp
	end

      fun con_constrain  (argcon, param as {constrain : bool, 
					    stamp = stampopt : Il.Tyvar.stamp option,
					    eq_constrain : bool}) = 
	  let 
	      fun tyvar_handler tyvar = (if constrain then tyvar_constrain tyvar else (); 
					 if eq_constrain then tyvar_use_equal tyvar else (); 
					 (case stampopt of
					      SOME stamp => update_stamp(tyvar,stamp)
					    | NONE => ());
					 NONE)
	      fun flex_handler (r as (ref (FLEXINFO (stamp,resolved,rdecs)))) =
		  (app (fn (_,c) => con_constrain(c,param)) rdecs;
		   (case stampopt of
			SOME s => r := FLEXINFO(stamp_join(s,stamp),resolved,rdecs)
		      | NONE => ());
		   true)
	      val handlers = STATE(default_bound,
				     {tyvar_handler = tyvar_handler,
				      flex_handler = flex_handler,
				      var_handler = default_var_handler,
				      convar_handler = default_convar_handler,
				      sdec_handler = default_sdec_handler,
				      modvar_handler = default_modvar_handler,
				      con_proj_handler = default_con_proj_handler,
				      con_app_handler = default_con_app_handler,
				      exp_proj_handler = default_exp_proj_handler})
	  in (f_con handlers argcon; ())
	  end

      fun con_useeq argcon =
	let 
	  fun tyvar_handler tyvar = (tyvar_use_equal tyvar; NONE)
	  val handlers = STATE(default_bound,
			       {tyvar_handler = tyvar_handler,
				flex_handler = default_flex_handler,
				var_handler = default_var_handler,
				convar_handler = default_convar_handler,
				sdec_handler = default_sdec_handler,
				modvar_handler = default_modvar_handler,
				con_proj_handler = default_con_proj_handler,
				con_app_handler = default_con_app_handler,
				exp_proj_handler = default_exp_proj_handler})
	in (f_con handlers argcon; ())
	end

      fun ConApply (c1,c2) = 
	(case (c1,c2) of
	     (CON_FUN([var],c),CON_TUPLE_INJECT[arg_con]) => con_subst_var(c, [(var,arg_con)])
	   | (CON_FUN([var],c),arg_con) => con_subst_var(c, [(var,arg_con)])
	   | (CON_FUN(vars,c),CON_TUPLE_INJECT(arg_cons)) => con_subst_var(c, zip vars arg_cons)
	   | _ => (print "ConApply got bad arguments\nc1 = ";
		   pp_con c1; print "\nc2 = "; 
		   pp_con c2; print "\n";
		   error "ConApply got bad arguments"))

      fun mod_free_expvar module : var list = 
	  let
	      val free = ref ([] : var list)
	      fun expvar_handler (var,bound) = (if (not (member_eq(eq_var,var,bound))
						    andalso (not (member_eq(eq_var,var,!free))))
						    then free := (var :: (!free)) else ();
							NONE)
	      val handlers = STATE(default_bound,
				   {tyvar_handler = default_tyvar_handler,
				    flex_handler = default_flex_handler,
				    var_handler = expvar_handler,
				    convar_handler = default_convar_handler,
				    sdec_handler = default_sdec_handler,
				    modvar_handler = default_modvar_handler,
				    con_proj_handler = default_con_proj_handler,
				    con_app_handler = default_con_app_handler,
				    exp_proj_handler = default_exp_proj_handler})
	      val _ = f_mod handlers module
	  in !free
	  end

     fun con_free_modvar con : var list = 
	  let
	      val free = ref ([] : var list)
	      fun modvar_handler (var,bound) = (print "modvar_handler got var = "; pp_var var;
						print "\n  and bound = ";
						pp_list pp_var' bound ("",", ","",false);
						print "\n";
						if (member_eq(eq_var,var,bound) orelse
						    member_eq(eq_var,var,!free))
						    then () else free := (var :: (!free));
							NONE)
	      val handlers = STATE(default_bound,
				   {tyvar_handler = default_tyvar_handler,
				    flex_handler = default_flex_handler,
				    var_handler = default_var_handler,
				    convar_handler = default_convar_handler,
				    sdec_handler = default_sdec_handler,
				    modvar_handler = modvar_handler,
				    con_proj_handler = default_con_proj_handler,
				    con_app_handler = default_con_app_handler,
				    exp_proj_handler = default_exp_proj_handler})
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
		| loop (_::rest) ee cc mm = loop rest ee cc mm
	      val (etable,ctable,mtable) = loop bnds [] [] [] 
	      fun convar_handler (var,bound) = if (member_eq(eq_var,var,bound))
						then NONE else assoc_eq(eq_var,var,ctable)
	      fun var_handler (var,bound) = if (member_eq(eq_var,var,bound))
						then NONE else assoc_eq(eq_var,var,etable)
	      fun modvar_handler (var,bound) = if (member_eq(eq_var,var,bound))
						then NONE else assoc_eq(eq_var,var,mtable)
	      val handlers = STATE(default_bound,
				   {tyvar_handler = default_tyvar_handler,
				    flex_handler = default_flex_handler,
				    var_handler = var_handler,
				    convar_handler = convar_handler,
				    sdec_handler = default_sdec_handler,
				    modvar_handler = modvar_handler,
				    con_proj_handler = default_con_proj_handler,
				    con_app_handler = default_con_app_handler,
				    exp_proj_handler = default_exp_proj_handler})
	  in (case target_bnd of
		  BND_EXP(v,e) => BND_EXP(v,f_exp handlers e)
		| BND_CON(v,c) => BND_CON(v,f_con handlers c)
		| BND_MOD(v,m) => BND_MOD(v,f_mod handlers m)
		| BND_FIXITY _ => target_bnd)
	  end

    end  

    fun error_obj pp_obj (obj_str : string) filename obj (str : string) = 
	(print filename; ": Error while evaluating "; print obj_str; print ": ";
	 print str; print "\n";
	 pp_obj obj; print "\n\n";
	 Util.error filename str)

    val error_exp = fn e => fn s => error_obj pp_exp "expression" e s
    val error_con = fn c => fn s => error_obj pp_con "constructor" c s
    val error_mod = fn m => fn s => error_obj pp_mod  "module" m s
    val error_sig = fn sg => fn s => error_obj pp_signat "signature" sg s


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
			    | BND_MOD(v,m) => BND_MOD(v, mm m)
			    | _ => bnd)
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
			   | _ => sbnd :: (loop rest)
		 in
		     SOME (MOD_STRUCTURE(loop sbnds))
		     handle FAIL => NONE
		 end
	   | MOD_FUNCTOR _ => SOME m
	   | _ => NONE)


(*
     fun Module_Lookup (module, labs) : (phrase * labels) = 
	let fun lookup(module,lbl) : (phrase * labels) =
	    (case module of
	         MOD_STRUCTURE sdecs => 
		     let
			 fun loop [] = raise (NOTFOUND "Module_Lookup reached []")
			   | loop (SBND(l,b)::r) = 
			     (case b of
				  (BND_EXP (_,e)) => if (eq_label(l,lbl)) then (PHRASE_EXP e,[l]) else loop r
				| (BND_CON (_,c)) => if (eq_label(l,lbl)) then (PHRASE_CON c,[l]) else loop r
				| (BND_MOD (_,m)) => if (eq_label(l,lbl)) then (PHRASE_MOD m,[l]) 
						     else if (is_label_open l)
							      then (let val (phrase,lbls') = lookup(m,lbl)
								    in (phrase,l::lbls')
								    end handle (NOTFOUND _) => loop r)
							  else loop r
			        | _ => loop r)
		     in  loop sdecs
		     end
	       | _ => error "Module_Lookup encountered a non-module")
	in
	    (case labs of
		 [] => error "Module_Lookup got []"
	       | [lbl] => lookup(module,lbl)
	       | (lbl :: lbls) =>
		     let val (phrase,labs) = lookup(module,lbl)
		     in
			 (case phrase of
			      PHRASE_MOD m => let val (phrase2,labs2) = Module_Lookup(m,lbls)
					     in (phrase2,labs @ labs2)
					     end 
			    | _ => error "Module_Lookup did not find a mod")
		     end)
	end

    fun Signat_Lookup (signat, labs) : (class * labels) = 
	let fun lookup(signat,lbl) : (class * labels) =
	    (case signat of
		 SIGNAT_FUNCTOR _ => raise (NOTFOUND "Signat_Lookup reached a functor signature")
	       | SIGNAT_STRUCTURE sdecs => 
		     let
			 fun loop [] = raise (NOTFOUND "Signat_Lookup reached []")
			   | loop (SDEC(l,d)::r) = 
			     (case d of
				  (DEC_EXP (_,c)) => if (eq_label(l,lbl)) then (CLASS_EXP c,[l]) else loop r
				| (DEC_CON (_,k,_)) => if (eq_label(l,lbl)) then (CLASS_CON k,[l]) else loop r
				| (DEC_MOD (_,s)) => if (eq_label(l,lbl)) then (CLASS_MOD s,[l]) 
						     else if (is_label_open l)
							      then (let val (class,lbls') = lookup(s,lbl)
								    in (class,l::lbls')
								    end handle (NOTFOUND _) => loop r)
							  else loop r
			        | _ => loop r)
		     in  loop sdecs
		     end)
	in
	    (case labs of
		 [] => error "Signat_Lookup got []"
	       | [lbl] => lookup(signat,lbl)
	       | (lbl :: lbls) =>
		     let val (class,labs) = lookup(signat,lbl)
		     in
			 (case class of
			      CLASS_MOD s => let val (class2,labs2) = Signat_Lookup(s,lbls)
					     in (class2,labs @ labs2)
					     end 
			    | _ => error "Signat_Lookup did not find a sig")
		     end)
	end

*)
  end;
