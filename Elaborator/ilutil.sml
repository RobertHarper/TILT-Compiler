(*$import IL Ppil PrimUtil IlPrimUtilParam ILUTIL ListMergeSort Stats *)
(* Il Utility *)
structure IlPrimUtil :> PRIMUTIL where type con = Il.con
                                 where type exp = Il.exp = PrimUtil(structure PrimUtilParam = IlPrimUtilParam)

structure IlUtil
  :> ILUTIL =
  struct

    open Il Ppil 
    open Util Listops Name 
    open Prim Tyvar

    type tyvar = (context,con) Tyvar.tyvar
    exception BUG
    exception UNIMP
    val debug = Stats.ff("IlutilDebug")
    fun debugdo t = if (!debug) then (t(); ()) else ()
    val error = fn s => error "ilutil.sml" s

    exception FAILURE of string

    
    (* -------------------------------------------------------- *)
    (* --------------------- Misc helper functions ------------ *)
    fun fresh_named_con (ctxt,s) = CON_TYVAR (fresh_named_tyvar (ctxt,s))
    fun fresh_con ctxt = fresh_named_con (ctxt,"con")
    local
	val size = 20
	fun make i = let val s = Symbol.labSymbol(Int.toString i)
		     in  (s, symbol_label s)
		     end
	val table = Array.tabulate(size, make)
    in
	fun generate_tuple_symbol 0 = error "generate_tuple_symbol called with 0"
	  | generate_tuple_symbol i = #1(if (i<size)
					     then Array.sub(table,i)
					 else make i)
	fun generate_tuple_label 0 = error "generate_tuple_label called with 0"
	  | generate_tuple_label i = #2(if (i<size)
					    then Array.sub(table,i)
					else make i)
    end
      
    val mk_lab = internal_label "mk"
    val km_lab = internal_label "km"
    val them_lab = internal_label "them"
    val it_lab = internal_label "it"
    val stamp_lab = internal_label "stamp"
    val case_lab = internal_label "case"
    val expose_lab = internal_label "expose"
    val eq_lab = internal_label "eq"
    val functor_arg_lab = internal_label "functor_arg"


    (* We can use Name.compare_label since it does respect the ordering of
       numeric labels that arise from tuples.  *)
    local 
      fun geq_label (l1,l2) = 
	  (case (Name.compare_label(l1,l2)) of
	       GREATER => true
	     | EQUAL => true
	     | LESS => false)
      fun geq_labelpair ((l1,_),(l2,_)) = geq_label(l1,l2)
    in 
      fun sort_label (arg : label list) : label list = ListMergeSort.sort geq_label arg
      fun sort_labelpair (arg : (label * 'a) list) : (label * 'a) list = ListMergeSort.sort geq_labelpair arg
      fun label_issorted [] = true
	| label_issorted [_] = true
	| label_issorted (l1::(r as (l2::_))) = 
	  (case (Name.compare_label(l1,l2)) of
	       LESS => (label_issorted r)
	     | _ => false)
    end


    val unit_exp : exp = RECORD[]
    val fail_tag = fresh_named_tag "fail"
    val bind_tag = fresh_named_tag "bind"
    val match_tag = fresh_named_tag "match"
    val con_unit = CON_RECORD[]
    val fail_exp = SCON(tag(fail_tag,con_unit))
    val bind_exp = SCON(tag(bind_tag,con_unit))
    val match_exp = SCON(tag(match_tag,con_unit))
    val failexn_exp = EXN_INJECT("fail",fail_exp,unit_exp)
    val bindexn_exp = EXN_INJECT("bind",bind_exp,unit_exp)
    val matchexn_exp = EXN_INJECT("match",match_exp,unit_exp)
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
	fun bool_help special = CON_SUM{names = [Name.symbol_label(Symbol.varSymbol "false"), 
						 Name.symbol_label(Symbol.varSymbol "true")],
					noncarriers = 2,
					carrier = CON_TUPLE_INJECT[],
					special = special}
	val con_bool = bool_help NONE
	val con_false = bool_help (SOME 0)
	val con_true = bool_help (SOME 1)
    fun con_eqfun c = CON_ARROW([con_tuple[c,c]],
				con_bool,false, oneshot_init PARTIAL)

    val false_exp = INJ{sumtype=con_bool,field=0,inject=NONE}
    val true_exp = INJ{sumtype=con_bool,field=1,inject=NONE}
    fun make_lambda_help (funvar_opt,a,var,con,rescon,e) 
      : exp * con = let val funvar = (case funvar_opt of
					  NONE => fresh_named_var "anonfun"
					| SOME v => v)
			val fbnd = FBND(funvar,var,con,rescon,e)
		    in (FIX(false,a,[fbnd]), CON_ARROW([con],rescon,false,oneshot_init a))
		    end
    fun make_total_lambda (var,con,rescon,e) = make_lambda_help(NONE,TOTAL,var,con,rescon,e)
    fun make_lambda (var,con,rescon,e) = make_lambda_help(NONE,PARTIAL,var,con,rescon,e)
    fun make_ifthenelse(e1,e2,e3,c) : exp = 
	CASE{sumtype=con_bool,arg=e1,bound=fresh_named_var "unused",
	     arms=[SOME e3,SOME e2],default=NONE,tipe=c}
    fun make_seq eclist =
	let fun loop _ [] = error "make_seq given empty list"
	      | loop [] [ec] = ec
	      | loop bnds [(e,c)] = (LET (rev bnds, e), c)
	      | loop bnds ((e,c)::erest) = loop (BND_EXP(fresh_var(),e)::bnds) erest
	in  loop [] eclist
	end

    fun make_let ([], body) = body
      | make_let (bnds, LET(bnds',body)) = LET (bnds @ bnds', body)
      | make_let (bnds, body) = LET (bnds, body)

    fun make_catch (e,con,tag_exp,tag_con,efail) : exp =
       let 
	   val v = fresh_var()
	   val efail' = #1(make_lambda(fresh_named_var "dummy",tag_con,con,efail))
	   val outer = EXN_CASE{arg = VAR v, arms = [(tag_exp, tag_con,efail')],
				default = NONE, tipe = con}
       in HANDLE(e,#1 (make_lambda(v,CON_ANY,con,outer)))
       end

    (* etaprims takes a record of their argument
       but expanded primitives takes their multiple arguments "flattened" *)
    fun etaexpand_help (primer,typer) (prim,cargs) = 
	let val prim_tipe = typer prim cargs
	    val (res_tipe,args_tipes) = 
		(case prim_tipe of
		     CON_ARROW([c],res_tipe,false,_) => (res_tipe,[c])
		   | CON_ARROW(clist,res_tipe,false,_) => (res_tipe,clist)
		   | _ => (print "cannot expand unexpected non-arrow primitive\n";
			   pp_con prim_tipe; print "\n";
			   error "cannot expand unexpected non-arrow primitive"))
	    val vars = map (fn _ => fresh_var()) args_tipes
	    val arg_var = fresh_var()
	    val (eargs,arg_tipe) = (case args_tipes of 
					[c] => ([VAR arg_var], c)
				      | _ => let val arg_tipe = con_tuple args_tipes
						 fun mapper (n,_) = (RECORD_PROJECT(VAR arg_var, 
										    generate_tuple_label (n+1),
										    arg_tipe))
					     in  (mapcount mapper args_tipes, arg_tipe)
					     end)
	in  #1(make_lambda(arg_var,arg_tipe,res_tipe,
			   primer(prim,cargs,eargs)))
	end

    val prim_etaexpand = etaexpand_help (PRIM,IlPrimUtil.get_type')
    val ilprim_etaexpand = etaexpand_help (ILPRIM,IlPrimUtil.get_iltype')


    fun con_deref (CON_TYVAR tyvar) = (case (tyvar_deref tyvar) of
					   NONE => CON_TYVAR tyvar
					 | SOME con => con)
      | con_deref c = c



    (* -------------------------------------------------------- *)
    (* ------------ Path manipulation functions ------------ *)
    fun join_path_labels (PATH (v,ls), l) = PATH(v,ls @ l)
    fun path2obj (var_maker : var -> 'a, mod_maker : mod * label -> 'a) (PATH(v,ls)) = 
	 let fun loop [] _ = var_maker v
	       | loop [l] acc = mod_maker(acc,l)
	       | loop (l::rest) acc = loop rest (MOD_PROJECT(acc,l))
         in loop ls (MOD_VAR v)
	 end
    val path2mod = path2obj (MOD_VAR,MOD_PROJECT)
    val path2con = path2obj (CON_VAR, CON_MODULE_PROJECT)
    val path2exp = path2obj (VAR, MODULE_PROJECT)


    local fun loop (MOD_VAR v) acc = SOME(PATH(v,acc))
	    | loop (MOD_PROJECT (m,l)) acc = loop m (l::acc)
	    | loop m _ = NONE
    in    
	fun mod2path (m : mod) = loop m []
	fun exp2path (e : exp) = 
	    (case e of
		 VAR v => SOME(PATH (v,[]))
	       | MODULE_PROJECT (m,l) => mod2path (MOD_PROJECT(m,l))
	       | _ => NONE)
	fun con2path (c : con) =
	    (case c of
		CON_VAR v => SOME(PATH(v,[]))
	      | CON_MODULE_PROJECT (m,l) => mod2path(MOD_PROJECT(m,l))
	      | _ => NONE)
    end


   fun eq_path(PATH (v1,l1), PATH(v2,l2)) = eq_var(v1,v2) andalso eq_list(eq_label,l1,l2)

    fun eq_mpath (MOD_VAR v, MOD_VAR v') = eq_var (v,v')
      | eq_mpath (MOD_PROJECT (m,l), MOD_PROJECT (m',l')) = eq_label(l,l') andalso eq_mpath(m,m')
      | eq_mpath _ = false
    fun eq_epath (VAR v, VAR v') = eq_var (v,v')
      | eq_epath (MODULE_PROJECT (m,l), MODULE_PROJECT (m',l')) = eq_label(l,l') andalso eq_mpath(m,m')
      | eq_epath _ = false
    fun eq_cpath (CON_VAR v, CON_VAR v') = eq_var (v,v')
      | eq_cpath (CON_MODULE_PROJECT (m,l), 
		  CON_MODULE_PROJECT (m',l')) = eq_label(l,l') andalso eq_mpath(m,m')
      | eq_cpath _ = false


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
	     SCON _ => exp
	   | OVEREXP (c,b,oe) => (case (oneshot_deref oe) of
				      NONE => OVEREXP(f_con state c, b, oe)
				    | SOME e => OVEREXP(f_con state c, b, oneshot_init(self e)))
	   | VAR v => exp
	   | PRIM (p,cs,es) => PRIM(p, map (f_con state) cs, map self es)
	   | ILPRIM (ilp,cs,es) => ILPRIM(ilp, map (f_con state) cs, map self es)
	   | ETAPRIM (p,cs) => ETAPRIM(p, map (f_con state) cs)
	   | ETAILPRIM (ilp,cs) => ETAILPRIM (ilp, map (f_con state) cs)
	   | APP (e1,e2) => APP(self e1, self e2)
	   | EXTERN_APP (c,e1,elist) => EXTERN_APP(f_con state c, self e1, map self elist)
	   | FIX (r,a,fbnds) => FIX(r,a,map (f_fbnd state) fbnds)
	   | RECORD (rbnds) => RECORD(map (f_rbnd state) rbnds)
	   | RECORD_PROJECT (e,l,c) => RECORD_PROJECT(self e,l,f_con state c)
	   | SUM_TAIL (i,c,e) => SUM_TAIL(i,f_con state c, self e)
	   | HANDLE (e1,e2) => HANDLE(self e1, self e2)
	   | RAISE (c,e) =>  RAISE(f_con state c, self e)
	   | LET (bnds,e) => let fun loop [] h = h
				   | loop ((BND_EXP(v,_))::rest) h = loop rest (add_var(h,v))
				   | loop ((BND_CON(v,_))::rest) h = loop rest (add_convar(h,v))
				   | loop ((BND_MOD(v,_,_))::rest) h = loop rest (add_modvar(h,v))
				 val state' = loop bnds state
			     in LET(map (f_bnd state) bnds, 
				    f_exp state' e)
			     end
	   | NEW_STAMP con => NEW_STAMP(f_con state con)
	   | EXN_INJECT (s,e1,e2) => EXN_INJECT(s,self e1, self e2)
	   | ROLL (c,e) => ROLL(f_con state c, self e)
	   | UNROLL (c1,c2,e) => UNROLL(f_con state c1, f_con state c2, self e)
	   | INJ {sumtype,field,inject} => INJ{sumtype = f_con state sumtype,
					       field=field,
					       inject = Util.mapopt self inject}
	   | CASE{sumtype,bound,arg,arms,default,tipe} =>
		let val state' = add_var(state,bound)
		in  CASE{sumtype = f_con state sumtype,
			 arg = self arg,
			 bound = bound,
			 arms = map (Util.mapopt (f_exp state')) arms,
			 default = Util.mapopt (f_exp state') default,
			 tipe = f_con state tipe}
		end
	   | EXN_CASE{arg,arms,default,tipe} => 
		 let fun help (e1,c,e2) = (self e1, f_con state c, self e2)
		     val default' = Util.mapopt self default
		     val tipe' = f_con state tipe
		 in EXN_CASE{arg = self arg, arms = map help arms, 
			     default = default', tipe = tipe'}
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
		     | (CON_OVAR ocon) => self (CON_TYVAR (ocon_deref ocon))
		     | CON_INT _ => con
		     | CON_FLOAT _  => con
		     | CON_UINT _  => con
		     | CON_ANY => con
		     | CON_ARRAY c => CON_ARRAY (self c)
		     | CON_VECTOR c => CON_VECTOR (self c)
		     | CON_REF c => CON_REF (self c)
		     | CON_TAG c => CON_TAG (self c)
		     | CON_ARROW (cons,c2,closed,complete) => CON_ARROW (map self cons, self c2, closed, complete)
		     | CON_APP (c1,c2) => CON_APP (self c1, self c2)
		     | CON_MU c =>  CON_MU (self c)
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
		     | CON_SUM {names,noncarriers,special,carrier} =>
			   CON_SUM{names=names,special=special,noncarriers=noncarriers,
				   carrier = self carrier}
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
	 | MOD_FUNCTOR (a,v,s1,m,s2) => MOD_FUNCTOR (a,v, f_signat state s1, 
						   f_mod state m, f_signat state s2)
	 | MOD_APP (m1,m2) => MOD_APP (f_mod state m1, f_mod state m2)
	 | MOD_PROJECT (m,l) => MOD_PROJECT(f_mod state m,l)
	 | MOD_LET (v,m1,m2) => MOD_LET (v, f_mod state m1, f_mod state m2)
	 | MOD_SEAL (m,s) => MOD_SEAL (f_mod state m, f_signat state s)))

      and f_signat (state as STATE(_,{sig_handler,...})) (s : signat) : signat = 
      (case (sig_handler s) of
	       SOME s => s
	     | NONE =>
	(case s of
	     SIGNAT_VAR v => s
	   | SIGNAT_OF m => SIGNAT_OF(f_mod state m)
	   | SIGNAT_STRUCTURE (popt,sdecs) => SIGNAT_STRUCTURE (popt,map (f_sdec state) sdecs)
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
	 | BND_MOD(v,b,m) => BND_MOD(v, b,f_mod state m)
	 | BND_CON(v,c) => BND_CON(v, f_con state c))

      and f_kind (state : state) (kind : kind) : kind = kind

      and f_dec (state : state) (dec : dec) : dec =
	(case dec of
	   DEC_EXP(v,c,NONE, inline)   => DEC_EXP(v, f_con state c, NONE, inline)
	 | DEC_EXP(v,c,SOME e, inline) => DEC_EXP(v, f_con state c, SOME (f_exp state e), inline)
	 | DEC_CON(v,k,NONE, inline)   => DEC_CON(v, f_kind state k, NONE, inline)
	 | DEC_CON(v,k,SOME c, inline) => DEC_CON(v, f_kind state k, SOME (f_con state c), inline)
	 | DEC_MOD(v,b,s)      => DEC_MOD(v, b, f_signat state s))


      val default_bound_convar = []
      val default_bound_var = []
      val default_bound_modvar = []
      val default_bound = {bound_convar = default_bound_convar,
			   bound_var = default_bound_var,
			   bound_modvar = default_bound_modvar}
      fun default_flex_handler _ = false
      fun default_sig_handler _ = NONE

    in

      fun default_sdec_handler _ = NONE
      fun default_exp_handler _ = NONE
      fun default_con_handler _ = NONE
      fun default_mod_handler _ = NONE

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


    fun canonical_tyvar_label is_equal n = 
	if (n<0 orelse n>25) then error "canonical_tyvar_label given number out of range"
	else symbol_label(Symbol.tyvSymbol ((if is_equal then "''" else "'")
					    ^ (String.str (chr (ord #"a" + n)))))

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
	  val free_tyvar = rev(!free_tyvar)
	  fun mapper (n,tv) = 
	      let val is_equal = tyvar_is_use_equal tv
		  val lbl = canonical_tyvar_label is_equal n
		  val proj = CON_MODULE_PROJECT(MOD_VAR targetv, lbl)
		  val _ = tyvar_set(tv,proj)
	      in  (tv, lbl, is_equal)
	      end
	in mapcount mapper free_tyvar
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
		     | ([l],(SDEC(curl,DEC_CON(v,k,SOME c,_)))::rest) =>
			   if (eq_label(l,curl))
			       then c
			   else search [l] rest
		     | ([l],_::rest) => search [l] rest
		     | (l::ls,[]) => raise DEPENDENT
		     | (l::ls,(SDEC(curl,DEC_MOD(v,_,(SIGNAT_STRUCTURE (_,sdecs)))))::rest) =>
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
	  fun sdec_handler (state,SDEC(l,DEC_CON(v,k,SOME c,inline))) =
	      let val copt = (SOME(f_con state c)
			      handle DEPENDENT => NONE)
	      in SOME(SDEC(l,DEC_CON(v,k,copt,inline)))
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
		     DEC_EXP (v,c,eopt,_) => loop ((v,MODULE_PROJECT(m,l))::ea,ca,ma) rest
		   | DEC_CON(v,k,copt,_) => loop (ea,(v,CON_MODULE_PROJECT(m,l))::ca,ma) rest
		   | DEC_MOD(v,_,s) => loop (ea,ca,(v,MOD_PROJECT(m,l))::ma) rest)
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
	      | strip ((SDEC (l, DEC_CON(v,_,_,_)))::rest) = (v,l) :: (strip rest)
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
	  val assoc_eq = fn count => fn (eq,key,table) => let val res = (assoc_eq(eq,key,table)) 
							val _ = (case res of
								     NONE => ()
								   | SOME _ => (count := !count + 1))
						    in  res
						    end
	  fun exp_handler count table (VAR var,bound) = 
	       if (member_eq(eq_var,var,bound)) 
		  then NONE
	      else assoc_eq count (eq_var,var,table)
	    | exp_handler _ _ _ = NONE
	  fun con_handler count table (CON_VAR var,bound) = if (member_eq(eq_var,var,bound)) 
							  then NONE
						      else assoc_eq count (eq_var,var,table)
	    | con_handler _ _ _ = NONE
	  fun ehandlers count table = STATE(default_bound,
				     {sdec_handler = default_sdec_handler,
				      exp_handler = exp_handler count table,
				      con_handler = default_con_handler,
				      mod_handler = default_mod_handler,
				      sig_handler = default_sig_handler})
	  fun chandlers count table = STATE(default_bound,
				      {sdec_handler = default_sdec_handler,
				       exp_handler = default_exp_handler,
				       con_handler = con_handler count table,
				       mod_handler = default_mod_handler,
				       sig_handler = default_sig_handler})
	  fun mod_handler count table (MOD_VAR var,bound) = if (member_eq(eq_var,var,bound)) 
					       then NONE
					   else assoc_eq count (eq_var,var,table)
	    | mod_handler _ _ _ = NONE
	  fun sig_handler count handler_thunk table (SIGNAT_STRUCTURE (SOME p,sdecs)) = 
	      let val PATH(v,_) = p
		  val popt = (case (assoc_eq count (eq_var,v,table)) of
				  NONE => SOME p
				| SOME _ => NONE)
	      in case (f_signat (handler_thunk()) (SIGNAT_STRUCTURE (NONE,sdecs))) of
		  (SIGNAT_STRUCTURE (_,sdecs)) => SOME(SIGNAT_STRUCTURE (popt,sdecs))
		| _ => error "f_signat changed shape"
	      end
	    | sig_handler _ _ _ _ = NONE
	  fun mhandlers count table = 
	      let val self = fn () => mhandlers count table
	      in STATE(default_bound,
		       {sdec_handler = default_sdec_handler,
			exp_handler = default_exp_handler,
			con_handler = default_con_handler,
			mod_handler = mod_handler count table,
			sig_handler = sig_handler count self table})
	      end
	  fun cmhandlers count ctable mtable =
	      let val self = fn () => cmhandlers count ctable mtable
	      in STATE(default_bound,
		       {sdec_handler = default_sdec_handler,
			exp_handler = default_exp_handler,
			con_handler = con_handler count ctable,
			mod_handler = mod_handler count mtable,
			sig_handler = sig_handler count self mtable})
	      end
	  fun echandlers count etable ctable =
	      let val self = fn () => echandlers count etable ctable
	      in STATE(default_bound,
		       {sdec_handler = default_sdec_handler,
			exp_handler = exp_handler count etable,
			con_handler = con_handler count ctable,
			mod_handler = default_mod_handler,
			sig_handler = default_sig_handler})
	      end
	  fun ecmhandlers count etable ctable mtable =
	      let val self = fn () => echandlers count etable ctable
	      in STATE(default_bound,
		       {sdec_handler = default_sdec_handler,
			exp_handler = exp_handler count etable,
			con_handler = con_handler count ctable,
			mod_handler = mod_handler count mtable,
			sig_handler = sig_handler count self mtable})
	      end
      in 
	  fun exp_subst_expvar(arg,table) = f_exp (ehandlers (ref 0) table) arg
	  fun con_subst_expvar(arg,table) = f_con (ehandlers (ref 0) table) arg
	  fun mod_subst_expvar(arg,table) = f_mod (ehandlers (ref 0) table) arg
	  fun sig_subst_expvar(arg,table) = f_signat (ehandlers (ref 0) table) arg
	  fun exp_subst_convar(arg,table) = f_exp (chandlers (ref 0) table) arg
	  fun con_subst_convar(arg,table) = f_con (chandlers (ref 0) table) arg
	  fun con_subst_convar'(arg,table) = let val r = ref 0 
					     in (r, f_con (chandlers r table) arg)
					     end
	  fun mod_subst_convar(arg,table) = f_mod (chandlers (ref 0) table) arg
	  fun sig_subst_convar(arg,table) = f_signat (chandlers (ref 0) table) arg
	  fun exp_subst_modvar(arg,table) = f_exp (mhandlers (ref 0) table) arg
	  fun con_subst_modvar(arg,table) = f_con (mhandlers (ref 0) table) arg
	  fun mod_subst_modvar(arg,table) = f_mod (mhandlers (ref 0) table) arg
	  fun sig_subst_modvar(arg,table) = f_signat (mhandlers (ref 0) table) arg
	  fun con_subst_conmodvar(arg,ctable,mtable) = f_con (cmhandlers (ref 0) ctable mtable) arg
	  fun mod_subst_conmodvar(arg,ctable,mtable) = f_mod (cmhandlers (ref 0) ctable mtable) arg
	  fun exp_subst_expconmodvar(arg,etable,ctable,mtable) = f_exp (ecmhandlers (ref 0) etable ctable mtable) arg
	  fun con_subst_expconmodvar(arg,etable,ctable,mtable) = f_con (ecmhandlers (ref 0) etable ctable mtable) arg
	  fun kind_subst_expconmodvar(arg,etable,ctable,mtable) = f_kind (ecmhandlers (ref 0) etable ctable mtable) arg
	  fun sig_subst_expconmodvar(arg,etable,ctable,mtable) = f_signat (ecmhandlers (ref 0) etable ctable mtable) arg
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
	      let val PATH(v,_) = p
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

      type proj_handler = (mod * label -> exp option) * (mod * label -> con option) * 
	   (mod * label -> mod option) * (sdec -> sdec option) 
      fun default_sdec_proj_handler _ = NONE
      fun default_exp_proj_handler _ = NONE
      fun default_con_proj_handler _ = NONE
      fun default_mod_proj_handler _ = NONE

      local
	  fun allproj_handlers(eproj, cproj, mproj, sdecer) : state =
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
	      in handlers
	      end
      in
	  fun exp_subst_allproj handlers = f_exp (allproj_handlers handlers)
	  fun con_subst_allproj handlers = f_con (allproj_handlers handlers)
	  fun mod_subst_allproj handlers = f_mod (allproj_handlers handlers)
	  fun sig_subst_allproj handlers = f_signat (allproj_handlers handlers)
	  fun bnd_subst_allproj handlers = f_bnd (allproj_handlers handlers)
	  fun dec_subst_allproj handlers = f_dec (allproj_handlers handlers)
      end

      fun find_tyvars_flexes con = 
	  let val tyvars = ref []
	      val flexes = ref []
	      fun help in_array_ref argcon = 
		  let
		      fun con_handler (c : con,bound : var list) = 
			  (case c of
			       CON_TYVAR tyvar =>
				   if (member_eq(eq_tyvar,tyvar,map #2 (!tyvars))
				       orelse (case tyvar_deref tyvar of
						   NONE => false
						 | SOME _ => true))
					then NONE
				      else 
					  (tyvars := (in_array_ref,tyvar) :: (!tyvars);
					  NONE)
			      | CON_FLEXRECORD r => (flexes := r :: (!flexes); NONE)
			      | CON_ARRAY c => (help true c; SOME c)
			      | CON_REF c => (help true c; SOME c)
			      | _ => NONE)
		      val handlers = STATE(default_bound,
					   {sdec_handler = default_sdec_handler,
					    exp_handler = default_exp_handler,
					    con_handler = con_handler,
					    mod_handler = default_mod_handler,
					    sig_handler = default_sig_handler})
		      val _ = f_con handlers argcon
		  in  (!tyvars, !flexes)
		  end
	  in  help false con
	  end
      


      fun ConApply (reduce_small,c1,c2) = 
	  let fun help(vars,body,args) = 
		 let val (ref count,res) = con_subst_convar'(body, zip vars args)
		 in  if (count <= length vars)
			 then res
		     else CON_APP(c1,c2)
		 end
	  in 
	      (case (reduce_small,c1,c2) of
		   (true,CON_FUN([var],body),arg as CON_VAR _) => con_subst_convar(body, [(var,arg)])
		 | (true,CON_FUN([var],body),arg) => help([var],body,[arg])
		 | (true,CON_FUN(vars,body),CON_TUPLE_INJECT args) => help(vars,body,args)
		 | (true,_,_) => CON_APP(c1,c2)
		 | (_,CON_FUN([var],c),CON_TUPLE_INJECT[arg_con]) => con_subst_convar(c, [(var,arg_con)])
		 | (_,CON_FUN([var],c),arg_con) => con_subst_convar(c, [(var,arg_con)])
		 | (_,CON_FUN(vars,c),CON_TUPLE_INJECT(arg_cons)) => con_subst_convar(c, zip vars arg_cons)
		 | _ => (print "ConApply got bad arguments\nc1 = ";
			 pp_con c1; print "\nc2 = "; 
			 pp_con c2; print "\n";
			 error "ConApply got bad arguments"))
	  end


      fun find_sdec ([],_) = NONE
	| find_sdec((sdec as SDEC(l,_))::rest,l') = if (eq_label(l,l')) 
						      then SOME sdec else find_sdec(rest,l')
      fun find_sbnd ([],_) = NONE
	| find_sbnd((sbnd as SBND(l,_))::rest,l') = if (eq_label(l,l')) 
						      then SOME sbnd else find_sbnd(rest,l')

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

       type handler = (exp -> exp option) * (con -> con option) * 
	   (mod -> mod option) * (sdec -> sdec option) 
       fun empty_handler (_ : 'a) : 'a option = NONE

       fun all_handlers(exp_handler, con_handler, mod_handler, sdec_handler) =
	  STATE(default_bound,
		{sdec_handler = fn (_,sd) => sdec_handler sd,
		 exp_handler = fn (e,_) => exp_handler e,
		 con_handler = fn (c,_) => con_handler c,
		 mod_handler = fn (m,_) => mod_handler m,
		 sig_handler = default_sig_handler})

       fun exp_all_handle handlers = f_exp (all_handlers handlers)
       fun con_all_handle handlers = f_con (all_handlers handlers)
       fun mod_all_handle handlers = f_mod (all_handlers handlers)
       fun sig_all_handle handlers = f_signat (all_handlers handlers)
       fun bnd_all_handle handlers = f_bnd (all_handlers handlers)
       fun dec_all_handle handlers = f_dec (all_handlers handlers)


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
		| loop ((BND_MOD(v,_,m))::rest) ee cc mm = loop rest ee cc ((v,m)::mm)
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
		  let val PATH(v,_) = p
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
		| BND_MOD(v,b,m) => BND_MOD(v,b,f_mod handlers m))
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


      fun bnd_size m = 
	  let val (count,handlers) = make_size_handlers()
	      val _ = f_bnd handlers m
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

    (* Internal labels follow special conventions *)
    (* Some internal labels are opened for lookup *)
    (* Some internal labels are non-exported *)
    (* Eq labels are identifiable as eq labels *)
	 
    val open_str = "+O"
    val nonexport_str = "-X"
    val eq_str = "+E"
    val dt_str = "+O-X+D"

    fun is_open lab = 
	let val str = label2name lab
	    val c = String.sub(str,0)
	in  (c = #"+" orelse c = #"-") andalso substring (open_str,str)
	end
    fun is_nonexport lab =  substring (nonexport_str,label2name lab)
    fun is_eq lab = substring (eq_str,label2name lab)
    fun is_dt lab = substring (dt_str,label2name lab)

    local
	fun to_meta_lab meta_str lab =
	  let
	      val str = label2name lab
	      val final_str = meta_str ^ str
	  in  internal_label final_str
	  end
    in  fun to_open lab = to_meta_lab open_str lab
	fun to_nonexport lab = to_meta_lab nonexport_str lab 
	fun to_eq lab = to_meta_lab eq_str lab
	fun to_dt lab = to_meta_lab dt_str lab
    end

    fun label2name lab = 
      let
	val str = Name.label2name lab
	val len = size str
	fun loop n = if (n < len andalso 
			(String.sub(str,n) = #"+" orelse
			String.sub(str,n) = #"-"))
			then loop (n+2) else n
	val start = loop 0
      in String.substring(str,start,len - start)
      end



   fun eq_modval (MOD_VAR v1,MOD_VAR v2) = eq_var(v1,v2)
     | eq_modval (MOD_PROJECT (m,l), 
		  MOD_PROJECT(m',l')) = eq_modval(m,m') andalso eq_label(l,l')
     | eq_modval (MOD_SEAL(m,s),m') = eq_modval(m,m')
     | eq_modval (m',MOD_SEAL(m,s)) = eq_modval(m,m')
     | eq_modval (MOD_VAR _, MOD_PROJECT _) = false
     | eq_modval (MOD_PROJECT _, MOD_VAR _) = false
     | eq_modval (m1,m2) = (print "eq_modval for a non value\nm1 = \n";
			   pp_mod m1; print "\nm2 = \n";
			   pp_mod m2; print "\n";
			   error "eq_modval for a non value")
   fun eq_mod (MOD_VAR v1,MOD_VAR v2) = eq_var(v1,v2)
     | eq_mod (MOD_PROJECT (m,l), 
		  MOD_PROJECT(m',l')) = eq_mod(m,m') andalso eq_label(l,l')
     | eq_mod (MOD_SEAL(m,s),m') = eq_mod(m,m')
     | eq_mod (m',MOD_SEAL(m,s)) = eq_mod(m,m')
     | eq_mod _ = false


   fun rename_confun(is_bound,vars,con) = 
       	let
	    fun make_entry v = if (is_bound v)
				   then SOME(v,CON_VAR(Name.derived_var v))
			       else NONE
	    val table = List.mapPartial make_entry vars
	    val con' = (case table of
			    [] => con
			  | _ => con_subst_convar(con,table))
	    fun find_var v = (case Listops.assoc_eq(eq_var,v,table) of
				  SOME (CON_VAR v) => v 
				| SOME _ => error "table has only Var_c's"
				| NONE => v)
	    val vars' = map find_var vars
	in  CON_FUN(vars',con')
	end

    fun beta_reduce_mod(x : mod, y : mod) : mod option = 
	(case (x,y) of
	     (MOD_FUNCTOR(_,v,s,m,_), _) => SOME (MOD_LET(v,y,m))
	   | _ => NONE)


    fun beta_reduce(x : exp, y : exp) : exp =
	let fun red (exp as (OVEREXP (c,_,oe))) = 
		      (case (oneshot_deref oe) of
			   SOME exp => exp
			 | NONE => exp)
		   | red exp = exp
	    val def = APP(x,y)
	in (case (red x, red y) of
	     (ETAPRIM(p,cs),RECORD rbnds) => PRIM(p,cs,map #2 rbnds)
	   | (ETAILPRIM(ip,cs),RECORD rbnds) => ILPRIM(ip,cs,map #2 rbnds)
	   | (x as ETAPRIM(p,cs),y) =>
		     (case (IlPrimUtil.get_type' p cs) of
			  CON_ARROW([_],_,_,_) => PRIM(p,cs,[y])
			| _ => def)
	   | (x as ETAILPRIM(ip,cs),y) =>
		     (case (IlPrimUtil.get_iltype' ip cs) of
			 CON_ARROW([_],_,_,_) => ILPRIM(ip,cs,[y])
			| _ => def)
	   | (FIX (false,_,[FBND(name,arg,argtype,bodytype,body)]), VAR argvar) => 
			  exp_subst_expvar(body,[(arg,VAR argvar)])
	   | (FIX (false,_,[FBND(name,arg,argtype,bodytype,body)]), y) => 
			  LET([BND_EXP(arg,y)],body)
	   | _ => def)
	end

  end;
