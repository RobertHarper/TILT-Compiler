(* Il Utility. *)
functor IlUtil(structure Ppil : PPIL
	       structure Il : IL
	       sharing Ppil.Il = Il)

  : ILUTIL = 
  struct
    structure Il = Il
    open Il Ppil 
    open Util Name Prim Tyvar

    exception BUG
    exception UNIMP
    val debug = ref false
    fun debugdo t = if (!debug) then (t(); ()) else ()
    val error = error "ilutil.sml"

    exception FAILURE of string
    
    (* -------------------------------------------------------- *)
    (* --------------------- Misc helper functions ------------ *)
    fun fresh_named_con s = CON_TYVAR (fresh_tyvar s)
    fun fresh_con () = fresh_named_con "c"
    fun generate_tuple_symbol (i : int) = Symbol.labSymbol(makestring i)
    fun generate_tuple_label (i : int) = symbol2label(generate_tuple_symbol i)
      
    val mk_lab = fresh_named_int_label "mk"
    val km_lab = fresh_named_int_label "km"
    val it_lab = fresh_named_int_label "it"
    val case_lab = fresh_named_int_label "case"
    val expose_lab = fresh_named_int_label "expose"
    val eq_lab = fresh_named_int_label "eq"
    val functor_arg_lab = fresh_named_int_label "functor_arg"


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
    fun con_tuple conlist = CON_RECORD(mapcount (fn (i,c) => RDEC(generate_tuple_label i,c)) conlist)
    fun con_record symconlist = CON_RECORD(map (fn (s,c) => RDEC(symbol2label s,c))  symconlist)
    fun exp_tuple explist = RECORD(mapcount (fn (i,e) => RBND(generate_tuple_label i,e)) explist)
    val con_string = CON_VECTOR CON_CHAR
    val con_bool = CON_SUM(NONE,[con_unit,con_unit])
    val false_exp = INJ([con_unit,con_unit], 0, unit_exp)
    val true_exp = INJ([con_unit,con_unit], 1, unit_exp)
    fun make_ifthenelse(e1,e2,e3) : exp = CASE([con_unit,con_unit],e1,[SOME e3,SOME e2],NONE)
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
	    | loop _ _ = error "mod2path called on non-projection"
      in loop m []
      end


      (* -------------------------------------------------------- *)
      (* ------------ Context manipulation functions ------------ *)
      fun Context_Get_FixityTable (CONTEXT c) : fixity_table = 
	let fun sdec_help (SDEC(_,DEC_FIXITY vflist)) = vflist
	      | sdec_help _ = []
	    fun help (CONTEXT_MODULE(l, v, SIGNAT_STRUCTURE sdecs)) = 
	      if (is_label_open l) then List.concat(map sdec_help sdecs) else []
	      | help _ = []
	in  List.concat (map help c)
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
	    | sighelp (SIGNAT_DATATYPE (_,_,sdecs)) = flatten(map sdechelp sdecs)
	    | sighelp (SIGNAT_FUNCTOR _) = []
	  fun loop [] = []
	    | loop ((CONTEXT_CONVAR (_,v,k,co))::rest) = v :: (loop rest)
	    | loop ((CONTEXT_MODULE (l, v, s))::rest) = if (is_label_open l) then (sighelp s) @ (loop rest)
							else loop rest
	    | loop ((CONTEXT_INLINE _ | CONTEXT_VAR _ | CONTEXT_SCOPED_TYVAR _ |
		     CONTEXT_SIGNAT _ | CONTEXT_FIXITY _)::rest) = loop rest
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
	   | sighelp (SIGNAT_DATATYPE (_,_,sdecs)) = flatten(map sdechelp sdecs)
	   | sighelp (SIGNAT_FUNCTOR _) = []
	 fun loop [] = []
	   | loop ((CONTEXT_MODULE (l, v, s))::rest) = if (is_label_open l) then (sighelp s) @ (loop rest)
						       else loop rest
	   | loop ((CONTEXT_SCOPED_TYVAR s)::rest) = s @ (loop rest)
	   | loop ((CONTEXT_INLINE _ | CONTEXT_VAR _ | CONTEXT_CONVAR _
	           | CONTEXT_SIGNAT _ | CONTEXT_FIXITY _)::rest) = loop rest
       in loop c
       end

     fun add_context_var(CONTEXT c, l, v, con) = CONTEXT((CONTEXT_VAR(l,v,con))::c)
     fun add_context_inline(CONTEXT c, l, inline) = CONTEXT((CONTEXT_INLINE(l,inline))::c)
     fun add_context_module(CONTEXT c, l, v, signat) = CONTEXT((CONTEXT_MODULE(l,v,signat))::c)
     fun add_context_convar(CONTEXT c, l, v, kind, conopt) = CONTEXT((CONTEXT_CONVAR(l,v,kind,conopt))::c)
     fun add_context_scoped_tyvars(CONTEXT c, syms) = CONTEXT((CONTEXT_SCOPED_TYVAR syms):: c)
     fun add_context_entries(CONTEXT c, e : context_entry list) : context = CONTEXT(e @ c)
     fun add_context_sdecs(CONTEXT c, sdecs : sdecs) : context = 
       let
	 fun sdec2entry (SDEC(l,DEC_EXP(v,c))) = CONTEXT_VAR(l,v,c)
	   | sdec2entry (SDEC(l,DEC_MOD(v,s))) = CONTEXT_SIGNAT(l,v,s)
	   | sdec2entry (SDEC(l,DEC_CON(v,k,co))) = CONTEXT_CONVAR(l,v,k,co)
	   | sdec2entry (SDEC(l,DEC_EXCEPTION _)) = error "sdec2entry on exception"
	   | sdec2entry (SDEC(l,DEC_FIXITY _)) = error "sdec2entry on fixity"
       in CONTEXT(foldl (fn (sdec,c) => (sdec2entry sdec)::c) c sdecs)
       end
  

    local
      (* 
       1st arg: bound tyvars; 
       2nd arg: function accepts (var * var list) and 
                  returns a con (typically, returns a var.fresh_label);
       3rg arg: function takes term level var and returns a expression
       4th arg: function takes mod level var and returns a module
       5th arg: func accepts (mod * label) from CON_MPROJ and returns a con
       6th arg: function accepts (con * con) returns con option *)
      type state = {bound_convar : var list,
		    bound_var : var list,
		    bound_modvar : var list,
                    tyvar_handler : con tyvar -> con option,
		    convar_handler : (var * var list) -> con option,
		    var_handler : (var * var list) -> exp option,
		    modvar_handler : (var * var list) -> mod option,
		    con_proj_handler : (mod * label) -> con option,
		    con_app_handler : (con * con) -> con option,
		    exp_proj_handler : (mod * label) -> exp option}

      fun add_convars ({bound_convar,bound_modvar,bound_var,convar_handler, tyvar_handler,
		       var_handler,modvar_handler,con_proj_handler,
		      con_app_handler,exp_proj_handler}, tv) : state = 
	                                                       {bound_convar = tv @ bound_convar,
								bound_var = bound_var,
								bound_modvar = bound_modvar,
								convar_handler = convar_handler,
								var_handler = var_handler,
								tyvar_handler = tyvar_handler,
								modvar_handler = modvar_handler,
								con_proj_handler = con_proj_handler,
								con_app_handler = con_app_handler,
								exp_proj_handler = exp_proj_handler}
      fun add_convar (h,v) = add_convars(h,[v])
      fun add_var ({bound_convar,bound_modvar,bound_var,convar_handler,tyvar_handler,
		    var_handler,modvar_handler,con_proj_handler,
		    con_app_handler,exp_proj_handler}, v) : state = 
                                                            {bound_convar = bound_convar,
							     bound_var = v :: bound_var,
							     bound_modvar = bound_modvar,
							     convar_handler = convar_handler,
							     tyvar_handler = tyvar_handler,
							     var_handler = var_handler,
							     modvar_handler = modvar_handler,
							     con_proj_handler = con_proj_handler,
							     con_app_handler = con_app_handler,
							     exp_proj_handler = exp_proj_handler}
	
      fun add_modvar ({bound_convar,bound_modvar,bound_var,convar_handler,tyvar_handler,
		       var_handler,modvar_handler,con_proj_handler,
		       con_app_handler,exp_proj_handler}, v) : state = 
                                                            {bound_convar = bound_convar,
							     bound_var = bound_var,
							     bound_modvar = v :: bound_modvar,
							     convar_handler = convar_handler,
							     tyvar_handler = tyvar_handler,
							     var_handler = var_handler,
							     modvar_handler = modvar_handler,
							     con_proj_handler = con_proj_handler,
							     con_app_handler = con_app_handler,
							     exp_proj_handler = exp_proj_handler}

      fun f_exp (handlers as {exp_proj_handler,var_handler,bound_var,...} : state) (exp : exp) : exp =
	let val self = f_exp handlers
	in
	  (case exp of
	     (SCON _ | OVEREXP _) => exp
	   | VAR v => (case (var_handler(v,bound_var)) of
			   NONE => exp
			 | SOME e => e)
	   | PRIM p => PRIM(f_prim handlers p)
	   | APP (e1,e2) => APP(self e1, self e2)
	   | SEQ elist => SEQ(map self elist)
	   | LOC (c,e) => (e := (self (!e));
			   LOC(f_con handlers c, e))
	   | FIX (fbnds,var) => FIX(map (f_fbnd handlers) fbnds,var)
	   | RECORD (rbnds) => RECORD(map (f_rbnd handlers) rbnds)
	   | RECORD_PROJECT (e,l,c) => RECORD_PROJECT(self e,l,f_con handlers c)
	   | SUM_TAIL (c,e) => SUM_TAIL(f_con handlers c, self e)
	   | HANDLE (e1,e2) => HANDLE(self e1, self e2)
	   | RAISE e =>  RAISE(self e)
	   | LET (bnds,e) => let fun loop [] h = h
				   | loop ((BND_EXP(v,_))::rest) h = loop rest (add_var(h,v))
				   | loop ((BND_CON(v,_))::rest) h = loop rest (add_convar(h,v))
				   | loop ((BND_MOD(v,_))::rest) h = loop rest (add_modvar(h,v))
				   | loop (_::rest) h = loop rest h
				 val handlers' = loop bnds handlers
			     in LET(map (f_bnd handlers) bnds, 
				    f_exp handlers' e)
			     end
	   | NEW_STAMP con => NEW_STAMP(f_con handlers con)
	   | EXN_INJECT (e1,e2) => EXN_INJECT(self e1, self e2)
	   | REF (con,exp) => REF(f_con handlers con, self exp )
	   | GET (c,e) => GET(f_con handlers c, self e)
	   | SET (c,e1,e2) => SET(f_con handlers c, self e1, self e2)
	   | ROLL (c,e) => ROLL(f_con handlers c, self e)
	   | UNROLL (c,e) => UNROLL(f_con handlers c, self e)
	   | INJ (c,i,e) => INJ(map (f_con handlers) c, i, self e)
	   | PROJ (c,i,e) => PROJ (map (f_con handlers) c, i, self e)
	   | TAG (n,c) => TAG (n, f_con handlers c)
	   | CASE(c,earg,elist,edef) => let fun help NONE = NONE
					      | help (SOME e) = SOME(self e)
					in CASE(map (f_con handlers) c, 
						self earg, map help elist, help edef)
					end
	   | EXN_CASE(earg,ecelist,eopt) => let fun help (e1,c,e2) = (self e1, f_con handlers c, self e2)
						val eopt' = (case eopt of 
								 NONE => NONE
							       | SOME e => SOME (self e))
					    in EXN_CASE(self earg, map help ecelist, eopt')
					    end
	   | MODULE_PROJECT (m,l) => (case (exp_proj_handler(m,l)) of
					NONE => MODULE_PROJECT(f_mod handlers m,l)
				      | SOME e => e)
	   | SEAL (e,c) =>  SEAL (self e, f_con handlers c))
	end

      and f_prim handlers prim =
	let val do_con = f_con handlers
	in
	  (case prim of
	     PRIM0 (SOFT_VTRAPprim _ | SOFT_ZTRAPprim _ | HARD_VTRAPprim _ | HARD_ZTRAPprim _) => prim
	   | PRIM1((* NOTprim | 
		   SIZEprim | CHRprim | ORDprim | EXPLODEprim | IMPLODEprim | *)
		   NEG_FLOATprim | ABS_FLOATprim | 
		   (* SQRTprim | SINprim | COSprim | ARCTANprim | EXPprim | LNprim | *)
		   NOT_INTprim  | NEG_INTprim | ABS_INTprim |
		   NOT_UINTprim | 
		   FLOAT2INTprim | INT2FLOATprim | INT2UINTprim | UINT2INTprim 
		   (* OPEN_INprim | OPEN_OUTprim | INPUTprim | LOOKAHEADprim |
		   CLOSE_INprim | END_OF_STREAMprim | CLOSE_OUTprim | USEprim | FLUSH_OUTprim *)) => prim
	   | PRIM2((* ANDprim | ORprim | XORprim | EQ_BOOLprim | *)
		   (* STRING_CONCATprim | *) EQ_CHARprim | NEQ_CHARprim | (* EQ_STRINGprim | NEQ_STRINGprim | *)
		   PLUS_FLOATprim | MINUS_FLOATprim | MUL_FLOATprim | DIV_FLOATprim |
		   LESS_FLOATprim | GREATER_FLOATprim | LESSEQ_FLOATprim | GREATEREQ_FLOATprim |
		   EQ_FLOATprim | NEQ_FLOATprim | 
		   PLUS_INTprim | MINUS_INTprim | MUL_INTprim | DIV_INTprim | MOD_INTprim | 
		   QUOT_INTprim | REM_INTprim | LESS_INTprim | GREATER_INTprim |
		   LESSEQ_INTprim | GREATEREQ_INTprim | EQ_INTprim | NEQ_INTprim | 
		   LSHIFT_INTprim | RSHIFT_INTprim | AND_INTprim | OR_INTprim | 
		   PLUS_UINTprim | MINUS_UINTprim | MUL_UINTprim | DIV_UINTprim | MOD_UINTprim | 
		   LESS_UINTprim | GREATER_UINTprim | LESSEQ_UINTprim | GREATEREQ_UINTprim |
		   EQ_UINTprim | NEQ_UINTprim | LSHIFT_UINTprim | RSHIFT_UINTprim |
		   AND_UINTprim | OR_UINTprim  
(*		   OUTPUTprim *) ) => prim

	   | PRIM1(MK_REFprim {instance}) => PRIM1(MK_REFprim{instance = do_con instance})
	   | PRIM1(DEREFprim {instance}) => PRIM1(DEREFprim{instance = do_con instance})
	   | PRIM2(SETREFprim {instance}) => PRIM2(SETREFprim{instance = do_con instance})
	   | PRIM2(EQ_REFprim{instance})   => PRIM2(EQ_REFprim{instance = do_con instance})
(*	   | PRIM0(NILprim   {instance})   => PRIM0(NILprim{instance = do_con instance})
	   | PRIM2(CONSprim  {instance})   => PRIM2(CONSprim{instance = do_con instance})
	   | PRIM1(ISNILprim   {instance}) => PRIM1(ISNILprim{instance = do_con instance})
	   | PRIM1(CARprim  {instance})    => PRIM1(CARprim{instance = do_con instance})
	   | PRIM1(CDRprim  {instance})    => PRIM1(CDRprim{instance = do_con instance}) *)
	   | PRIM2(ARRAY1prim  {instance}) => PRIM2(ARRAY1prim{instance = do_con instance})
	   | PRIM2(SUB1prim    {instance}) => PRIM2(SUB1prim{instance = do_con instance})
	   | PRIM3(UPDATE1prim {instance}) => PRIM3(UPDATE1prim{instance = do_con instance})
	   | PRIM1(LENGTH1prim {instance}) => PRIM1(LENGTH1prim{instance = do_con instance})
(*	   | PRIM3(ARRAY2prim  {instance}) => PRIM3(ARRAY2prim{instance = do_con instance})
	   | PRIM3(SUB2prim    {instance}) => PRIM3(SUB2prim{instance = do_con instance})
	   | PRIM4(UPDATE2prim {instance}) => PRIM4(UPDATE2prim{instance = do_con instance}) 
	   | PRIM1(LENGTH2prim {instance}) => PRIM1(LENGTH2prim{instance = do_con instance}) *)
		 )
	end


      and f_con (handlers : state) (con : con) : con = 
	let 
	  val self = f_con handlers 
	  val {convar_handler,bound_convar,con_app_handler,con_proj_handler,tyvar_handler,...} = handlers
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
	   | (CON_OVAR ocon) => (self (ocon_deref ocon); con)
	   | (CON_INT | CON_FLOAT | CON_UINT | CON_CHAR | CON_ANY) => con
	   | CON_LIST c => CON_LIST (self c)
	   | CON_ARRAY c => CON_ARRAY (self c)
	   | CON_VECTOR c => CON_VECTOR (self c)
	   | CON_REF c => CON_REF (self c)
	   | CON_TAG c => CON_TAG (self c)
	   | CON_ARROW (c1,c2,complete) => CON_ARROW (self c1, self c2, complete)
	   | CON_APP (c1,c2) => (case con_app_handler(c1,c2) of
				   NONE => CON_APP (self c1, self c2)
				 | SOME c => c)
	   | CON_MUPROJECT (i,c) =>  CON_MUPROJECT (i, self c)
	   | CON_RECORD rdecs => CON_RECORD (map (f_rdec handlers) rdecs)
	   | CON_FUN (vars,c) => let val handlers' = add_convars(handlers,vars)
				 in CON_FUN(vars,f_con handlers' c)
				 end
	   | CON_SUM (iopt,clist) => CON_SUM (iopt,map self clist)
	   | CON_TUPLE_INJECT clist => CON_TUPLE_INJECT (map self clist)
	   | CON_TUPLE_PROJECT (i,c) =>  CON_TUPLE_PROJECT (i, self c)
	   | CON_MODULE_PROJECT (m,l) => case con_proj_handler(m,l) of
					    NONE => CON_MODULE_PROJECT(f_mod handlers m, l)
					  | SOME c => c)
	end

      and f_mod (handlers as {modvar_handler,bound_modvar,...} : state) (m : mod) : mod = 
	(case m of
	   MOD_VAR v => (case modvar_handler (v,bound_modvar) of
			     NONE => m
			   | SOME m' => m')
	 | MOD_STRUCTURE sbnds => MOD_STRUCTURE (map (f_sbnd handlers) sbnds)
(*	 | MOD_DATATYPE (dt,tb,sbnds) => MOD_DATATYPE(dt,tb,map (f_sbnd handlers) sbnds) *)
	 | MOD_FUNCTOR (v,s,m) => MOD_FUNCTOR (v, f_signat handlers s, f_mod handlers m)
	 | MOD_APP (m1,m2) => MOD_APP (f_mod handlers m1, f_mod handlers m2)
	 | MOD_PROJECT (m,l) => MOD_PROJECT (f_mod handlers m, l)
	 | MOD_SEAL (m,s) => MOD_SEAL (f_mod handlers m, f_signat handlers s))

      and f_signat (handlers : state) (s : signat) : signat = 
	(case s of
	   SIGNAT_STRUCTURE sdecs => SIGNAT_STRUCTURE (map (f_sdec handlers) sdecs)
	 | SIGNAT_DATATYPE(dt,tb,sdecs) => SIGNAT_DATATYPE(dt,tb, (map (f_sdec handlers) sdecs))
	 | SIGNAT_FUNCTOR (v,s1,s2,comp) => SIGNAT_FUNCTOR(v, f_signat handlers s1, f_signat handlers s2, comp))

      and f_rbnd (handlers) (RBND(l,e)) : rbnd = RBND(l, f_exp handlers e)
      and f_fbnd (handlers) (FBND(vname,varg,carg,cres,e)) : fbnd =
	   let val handlers' = add_var(handlers,varg)
	   in FBND(vname,varg,f_con handlers' carg,f_con handlers' cres,f_exp handlers' e)
	   end
      and f_rdec (handlers) (RDEC(l,c)) : rdec = RDEC(l, f_con handlers c)
      and f_sbnd (handlers) (SBND(l,bnd)) : sbnd = SBND(l, f_bnd handlers bnd)
      and f_sdec (handlers) (SDEC(l,dec)) : sdec = SDEC(l, f_dec handlers dec)

      and f_bnd (handlers : state) (bnd : bnd) : bnd =
	(case bnd of
	   BND_EXP(v,e) => BND_EXP(v, f_exp handlers e)
	 | BND_MOD(v,m) => BND_MOD(v, f_mod handlers m)
	 | BND_CON(v,c) => BND_CON(v, f_con handlers c)
	 | BND_FIXITY _ => bnd)

      and f_dec (handlers : state) (dec : dec) : dec =
	(case dec of
	   DEC_EXP(v,c) => DEC_EXP(v, f_con handlers c)
	 | DEC_MOD(v,s) => DEC_MOD(v, f_signat handlers s)
	 | DEC_CON(v,k,NONE) => dec
	 | DEC_CON(v,k,SOME c) => DEC_CON(v, k, SOME (f_con handlers c))
	 | DEC_EXCEPTION(n,c) => DEC_EXCEPTION(n, f_con handlers c) 
	 | DEC_FIXITY _ => dec)

      val default_bound_convar = []
      val default_bound_var = []
      val default_bound_modvar = []
      fun default_convar_handler _ = NONE
      fun default_tyvar_handler _ = NONE
      fun default_var_handler _ = NONE
      fun default_modvar_handler _ = NONE
      fun default_con_proj_handler _ = NONE
      fun default_con_app_handler _ = NONE
      fun default_exp_proj_handler _ = NONE

    in

      fun con_occurs(argcon : con, origv : var) : bool =
	let 
	  val occurs = ref false
	  fun convar_handler(v,bound) = (if eq_var(v,origv) 
					     then occurs := true else (); 
						 NONE)
	  val handlers = {bound_convar = default_bound_convar,
			  bound_var = default_bound_var,
			  bound_modvar = default_bound_modvar,
			  convar_handler = convar_handler,
			  tyvar_handler = default_tyvar_handler,
			  var_handler = default_var_handler,
			  modvar_handler = default_modvar_handler,
			  con_proj_handler = default_con_proj_handler,
			  con_app_handler = default_con_app_handler,
			  exp_proj_handler = default_exp_proj_handler}
	in (f_con handlers argcon; !occurs)
	end

      fun con_free_tyvar (argcon : con) : con tyvar list = 
	let 
	  val free = ref []
	  fun tyvar_handler tv = (if (tyvar_isconstrained tv)
				      then ()
				  else free := (tv::(!free));
				      NONE)
	  val handlers = {bound_convar = default_bound_convar,
			  bound_var = default_bound_var,
			  bound_modvar = default_bound_modvar,
			  convar_handler = default_convar_handler,
			  tyvar_handler = tyvar_handler,
			  var_handler = default_var_handler,
			  modvar_handler = default_modvar_handler,
			  con_proj_handler = default_con_proj_handler,
			  con_app_handler = default_con_app_handler,
			  exp_proj_handler = default_exp_proj_handler}
	  val _ = f_con handlers argcon
	in !free
	end


      fun rebind_free_type_var(argcon : con, context, targetv : var) : (label * bool) list = 
	let 
	  val bound_convar = Context_Get_BoundConvars(context)
	  val free_tyvar = ref ([] : con Tyvar.tyvar list)
	  fun tyvar_handler tv = (print "tyvar_handler";
				  (case (tyvar_deref tv) of
					    SOME _ => (print "SOME case\n"; ())
					  | NONE => (print "NONE case\n";
						     if (not (member_eq(eq_tyvar,tv,!free_tyvar))
							 andalso not (tyvar_isconstrained tv))
						       then free_tyvar := (tv::(!free_tyvar))
						     else ()));
					 NONE)
	  val handlers = {bound_convar = bound_convar,
			  bound_var = default_bound_var,
			  bound_modvar = default_bound_modvar,
			  var_handler = default_var_handler,
			  modvar_handler = default_modvar_handler,
			  tyvar_handler = tyvar_handler,
			  convar_handler = default_convar_handler,
			  con_proj_handler = default_con_proj_handler,
			  con_app_handler = default_con_app_handler,
			  exp_proj_handler = default_exp_proj_handler}
	  val _ = f_con handlers argcon
	  val res = map (fn tv => (fresh_int_label(),tyvar_is_use_equal tv)) (!free_tyvar)
	  val _ = (map2 (fn (tv,(lbl,useeq)) => 
			 let val proj = if useeq
					  then (CON_MODULE_PROJECT
						(MOD_PROJECT(MOD_VAR targetv, openlabel lbl),lbl))
					else CON_MODULE_PROJECT(MOD_VAR targetv, lbl)
			 in tyvar_set(tv,proj)
			 end)
		   (!free_tyvar,res))
	  val _ = (print "rebind_free_type_var called on con:\n    ";
		   pp_con argcon;
		   print "\nand returning:\n    "; 
		   map (fn (l,b : bool) => (pp_label l; print ", "; print b; print ";  ")) res; 
		   print "\n")
	in res
	end

      fun make_non_dependent_type(c : con, target_var : var, arg_sdecs : sdecs) : con = 
	let 
	  fun tyvar_handler tyvar =
	    (case (tyvar_deref tyvar) of
	       (SOME _) => NONE
	     | NONE =>
		   let 
		       val v = tyvar_getvar tyvar
		       fun loop [] = CON_TYVAR tyvar
			 | loop ((SDEC(cur_label,DEC_CON(curv,k,SOME c)))::rest) = 
			   if eq_var(v,curv) then c else loop rest
			 | loop (_::rest) = loop rest
		   in SOME(loop arg_sdecs)
		   end)
	  val _ = print "!!!!!! make_non_dependente_type called\n"
	  fun con_proj_handler (m,argl) : con option = 
	    let 
		val _ = (print "***** con_proj_handler called with m and argl of: ";
			 Ppil.pp_con (CON_MODULE_PROJECT(m,argl));
			 print "\n")
	      fun help (MOD_VAR v) acc = SOME(v,rev(argl::acc))
		| help (MOD_PROJECT(m,l)) acc = help m (l::acc)
		| help _ _ = NONE
	      fun search [] _ = error "search got no labels"
		| search [l] [] = NONE
		| search [l] ((SDEC(curl,DEC_CON(v,k,SOME c)))::rest) = if (eq_label(l,curl))
									  then SOME c
									else search [l] rest
		| search [l] (_::rest) = search [l] rest
		| search (l::ls) [] = NONE
		| search (l::ls) ((SDEC(curl,DEC_MOD(v,SIGNAT_STRUCTURE sdecs)))::rest) = 
									  if (eq_label(l,curl))
									    then search ls sdecs
									  else search (l::ls) rest
		| search (l::ls) (_::rest) = search (l::ls) rest
	    in (case help m [] of
		  NONE => NONE
		| SOME(v,lbls) =>
		    if eq_var(v,target_var) 
		      then search lbls arg_sdecs
		    else NONE)
	    end
	  val handlers = {bound_convar = default_bound_convar,
			  bound_var = default_bound_var,
			  bound_modvar = default_bound_modvar,
			  tyvar_handler = tyvar_handler,
			  var_handler = default_var_handler,
			  convar_handler = default_convar_handler,
			  modvar_handler = default_modvar_handler,
			  con_proj_handler = con_proj_handler,
			  con_app_handler = default_con_app_handler,
			  exp_proj_handler = default_exp_proj_handler}
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
	    val handlers = {bound_convar = default_bound_convar,
			    bound_var = default_bound_var,
			    bound_modvar = default_bound_modvar,
			    tyvar_handler = default_tyvar_handler,
			    var_handler = default_var_handler,
			    convar_handler = convar_handler,
			    modvar_handler = default_modvar_handler,
			    con_proj_handler = default_con_proj_handler,
			    con_app_handler = default_con_app_handler,
			    exp_proj_handler = default_exp_proj_handler}
	in f_con handlers c
	end

      fun con_subst_var(argcon : con, table : (var * con) list) : con = 
	let
	  fun convar_handler (var,bound) = assoc_eq(eq_var,var,table)
	  val handlers = {bound_convar = default_bound_convar,
			  bound_var = default_bound_var,
			  bound_modvar = default_bound_modvar,
			  tyvar_handler = default_tyvar_handler,
			  convar_handler = convar_handler,
			  var_handler = default_var_handler,
			  modvar_handler = default_modvar_handler,
			  con_proj_handler = default_con_proj_handler,
			  con_app_handler = default_con_app_handler,
			  exp_proj_handler = default_exp_proj_handler}
	in f_con handlers argcon
	end

      fun exp_subst_var(argexp : exp, table : (var * exp) list) : exp = 
	let
	  fun var_handler (var,bound) = if (member_eq(eq_var,var,bound)) 
					    then NONE
					else assoc_eq(eq_var,var,table)
	  val handlers = {bound_convar = default_bound_convar,
			  bound_var = default_bound_var,
			  bound_modvar = default_bound_modvar,
			  tyvar_handler = default_tyvar_handler,
			  var_handler = var_handler,
			  convar_handler = default_convar_handler,
			  modvar_handler = default_modvar_handler,
			  con_proj_handler = default_con_proj_handler,
			  con_app_handler = default_con_app_handler,
			  exp_proj_handler = default_exp_proj_handler}
	in f_exp handlers argexp
	end

      fun con_subst_conapps (argcon, con_app_handler : (con * con -> con option)) : con = 
	let 
	  val handlers = {bound_convar = default_bound_convar,
			  bound_var = default_bound_var,
			  bound_modvar = default_bound_modvar,
			  tyvar_handler = default_tyvar_handler,
			  var_handler = default_var_handler,
			  convar_handler = default_convar_handler,
			  modvar_handler = default_modvar_handler,
			  con_proj_handler = default_con_proj_handler,
			  con_app_handler = con_app_handler,
			  exp_proj_handler = default_exp_proj_handler}
	in f_con handlers argcon
	end

      fun exp_subst_proj(argexp : exp, exp_proj_handler) : exp = 
	let 
	  val handlers = {bound_convar = default_bound_convar,
			  bound_var = default_bound_var,
			  bound_modvar = default_bound_modvar,
			  tyvar_handler = default_tyvar_handler,
			  convar_handler = default_convar_handler,
			  var_handler = default_var_handler,
			  modvar_handler = default_modvar_handler,
			  con_proj_handler = default_con_proj_handler,
			  con_app_handler = default_con_app_handler,
			  exp_proj_handler = exp_proj_handler}
	in f_exp handlers argexp
	end

      fun con_constrain argcon =
	let 
	  fun tyvar_handler tyvar = (tyvar_constrain tyvar; NONE)
	  val handlers = {bound_convar = default_bound_convar,
			  bound_var = default_bound_var,
			  bound_modvar = default_bound_modvar,
			  tyvar_handler = tyvar_handler,
			  var_handler = default_var_handler,
			  convar_handler = default_convar_handler,
			  modvar_handler = default_modvar_handler,
			  con_proj_handler = default_con_proj_handler,
			  con_app_handler = default_con_app_handler,
			  exp_proj_handler = default_exp_proj_handler}
	in (f_con handlers argcon; ())
	end

      fun con_useeq argcon =
	let 
	  fun tyvar_handler tyvar = (tyvar_use_equal tyvar; NONE)
	  val handlers = {bound_convar = default_bound_convar,
			  bound_var = default_bound_var,
			  bound_modvar = default_bound_modvar,
			  tyvar_handler = tyvar_handler,
			  var_handler = default_var_handler,
			  convar_handler = default_convar_handler,
			  modvar_handler = default_modvar_handler,
			  con_proj_handler = default_con_proj_handler,
			  con_app_handler = default_con_app_handler,
			  exp_proj_handler = default_exp_proj_handler}
	in (f_con handlers argcon; ())
	end

      fun ConApply (c1,c2) = 
	(case (c1,c2) of
	   (CON_FUN(vars,c),CON_TUPLE_INJECT(arg_cons)) => con_subst_var(c, zip vars arg_cons)
	 | _ => error "ConApply got bad arguments")

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
	      val handlers = {bound_convar = default_bound_convar,
			      bound_var = default_bound_var,
			      bound_modvar = default_bound_modvar,
			      tyvar_handler = default_tyvar_handler,
			      var_handler = var_handler,
			      convar_handler = convar_handler,
			      modvar_handler = modvar_handler,
			      con_proj_handler = default_con_proj_handler,
			      con_app_handler = default_con_app_handler,
			      exp_proj_handler = default_exp_proj_handler}
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

    val error_exp = error_obj pp_exp "expression"
    val error_con = error_obj pp_con "constructor"
    val error_mod = error_obj pp_mod  "module"
    val error_sig = error_obj pp_signat "signature"


  end;
