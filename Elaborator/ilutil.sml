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
    fun fresh_con ctxt = fresh_named_con (ctxt,"metavar")
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

    fun canonical_tyvar_label is_equal n = 
	if (n<0 orelse n>25) then error "canonical_tyvar_label given number out of range"
	else symbol_label(Symbol.tyvSymbol ((if is_equal then "''" else "'")
					    ^ (String.str (chr (ord #"a" + n)))))


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
    fun con_tuple_inject clist = CON_TUPLE_INJECT clist
    fun exp_tuple explist = let fun help(i,e) = (generate_tuple_label (i+1),e)
			    in  RECORD(mapcount help explist)
			    end
    val con_string = CON_VECTOR (CON_UINT W8)
    local val names = [Name.symbol_label(Symbol.varSymbol "false"), 
		       Name.symbol_label(Symbol.varSymbol "true")]
    in  fun sumbool_help special = CON_SUM{names = names,
					   noncarriers = 2,
					   carrier = CON_TUPLE_INJECT[],
					   special = special}
	fun bool_help special = 
	    let val con_sum = sumbool_help special
	    in  CON_TUPLE_PROJECT(0,CON_MU(CON_FUN([fresh_var()],
						   CON_TUPLE_INJECT [con_sum])))
	    end
    end

    val con_sumbool = sumbool_help NONE
    val con_bool = bool_help NONE
    val con_false = bool_help (SOME 0)
    val con_true = bool_help (SOME 1)
    fun con_eqfun c = CON_ARROW([con_tuple[c,c]],
				con_bool,false, oneshot_init PARTIAL)

    val false_exp = ROLL(con_bool,INJ{sumtype=con_sumbool,field=0,inject=NONE})
    val true_exp = ROLL(con_bool,INJ{sumtype=con_sumbool,field=1,inject=NONE})
    fun make_lambda_help totality (var,con,rescon,e) 
      : exp * con = let val funvar = fresh_named_var "anonfun"
			val fbnd = FBND(funvar,var,con,rescon,e)
		    in (FIX(false,totality,[fbnd]), CON_ARROW([con],rescon,false,oneshot_init totality))
		    end
    val make_total_lambda = make_lambda_help TOTAL
    val make_lambda  = make_lambda_help PARTIAL
    fun make_ifthenelse(e1,e2,e3,c) : exp = 
	CASE{sumtype=con_sumbool,
	     arg=UNROLL(con_bool,con_sumbool,e1),
	     bound=fresh_named_var "unused",
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
       in HANDLE(con,e,#1 (make_lambda(v,CON_ANY,con,outer)))
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


      fun find_sdec ([],_) = NONE
	| find_sdec((sdec as SDEC(l,_))::rest,l') = if (eq_label(l,l')) 
						      then SOME sdec else find_sdec(rest,l')
      fun find_sbnd ([],_) = NONE
	| find_sbnd((sbnd as SBND(l,_))::rest,l') = if (eq_label(l,l')) 
						      then SOME sbnd else find_sbnd(rest,l')


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
	   | HANDLE (c,e1,e2) => HANDLE(f_con state c, self e1, self e2)
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
		     | CON_APP (cfun,cargs) => CON_APP (self cfun, map self cargs)
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
	   | SIGNAT_OF p => (case mod2path (f_mod state (path2mod p)) of
				 NONE => error "f_signat transformed path to non-path with SIGNAT_OF"
			       | SOME p => SIGNAT_OF p)
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

      fun f_entry(state : state) (entry : context_entry) : context_entry = 
	  (case entry of
	       CONTEXT_SDEC sdec => CONTEXT_SDEC (f_sdec state sdec)
	     | CONTEXT_SIGNAT (l, v, s) => CONTEXT_SIGNAT (l, v, f_signat state s)
	     | CONTEXT_FIXITY _ => entry
	     | CONTEXT_OVEREXP (l, celist) => CONTEXT_OVEREXP (l, map (fn (c,e) => (f_con state c,
										    f_exp state e)) celist))

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

      type handler = (exp -> exp option) * (con -> con option) * 
	              (mod -> mod option) * (sdec -> sdec option) 
       local
	   fun all_handlers(exp_handler, con_handler, mod_handler, sdec_handler) =
	       STATE(default_bound,
		     {sdec_handler = fn (_,sd) => sdec_handler sd,
		      exp_handler = fn (e,_) => exp_handler e,
		      con_handler = fn (c,_) => con_handler c,
		      mod_handler = fn (m,_) => mod_handler m,
		      sig_handler = default_sig_handler})
       in
	   fun exp_handle handlers = f_exp (all_handlers handlers)
	   fun con_handle handlers = f_con (all_handlers handlers)
	   fun mod_handle handlers = f_mod (all_handlers handlers)
	   fun sig_handle handlers = f_signat (all_handlers handlers)
	   fun bnd_handle handlers = f_bnd (all_handlers handlers)
	   fun dec_handle handlers = f_dec (all_handlers handlers)
       end


      fun rebind_free_type_var(skip : int, tv_stamp : stamp,
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
		  val lbl = canonical_tyvar_label is_equal (n + skip)
		  val proj = CON_MODULE_PROJECT(MOD_VAR targetv, lbl)
		  val _ = tyvar_set(tv,proj)
	      in  (tv, lbl, is_equal)
	      end
	in mapcount mapper free_tyvar
	end

      exception DEPENDENT
      fun remove_modvar_handlers(target_var : var, arg_sdecs : sdecs) : state =
	let 
	  fun con_handler (CON_MODULE_PROJECT(m,l),_) = con_proj_handler(m,l)
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


      type subst = exp Name.PathMap.map * con Name.PathMap.map * mod Name.PathMap.map * var Name.VarMap.map
      val empty_subst = (Name.PathMap.empty, Name.PathMap.empty, Name.PathMap.empty, Name.VarMap.empty)
      fun subst_is_empty(e,c,m,s) = (Name.PathMap.numItems e = 0) andalso
	                            (Name.PathMap.numItems c = 0) andalso
				    (Name.PathMap.numItems m = 0) andalso
				    (Name.VarMap.numItems s = 0)
      fun subst_add((e1,c1,m1,s1),(e2,c2,m2,s2)) = 
	  let fun joiner _ = error "subst_add: subst not disjoint"
	  in  (Name.PathMap.unionWith joiner (e1,e2),
	       Name.PathMap.unionWith joiner (c1,c2),
	       Name.PathMap.unionWith joiner (m1,m2),
	       Name.VarMap.unionWith joiner (s1,s2))
	  end

      fun subst_add_exp((e,c,m,s), p, exp) = (Name.PathMap.insert(e, p, exp), c, m,s)
      fun subst_add_con((e,c,m,s), p, con) = (e, Name.PathMap.insert(c, p, con), m,s)
      fun subst_add_mod((e,c,m,s), p, module) = (e, c, Name.PathMap.insert(m, p, module),s)

      fun subst_add_expvar(subst, v, e) = subst_add_exp(subst, (v,[]), e)
      fun subst_add_convar(subst, v, c) = subst_add_con(subst, (v,[]), c)
      fun subst_add_modvar(subst, v, m) = subst_add_mod(subst, (v,[]), m)
      fun subst_add_sigvar((e,c,m,s), v, v') = (e, c, m, Name.VarMap.insert(s, v, v'))

      fun subst_add_exppath(subst, PATH p, e) = subst_add_exp(subst, p, e)
      fun subst_add_conpath(subst, PATH p, c) = subst_add_con(subst, p, c)
      fun subst_add_modpath(subst, PATH p, m) = subst_add_mod(subst, p, m)

    local 
	fun efolder ((v,e),s) = subst_add_expvar(s,v,e)
	fun cfolder ((v,c),s) = subst_add_convar(s,v,c)
	fun mfolder ((v,m),s) = subst_add_modvar(s,v,m)
    in
	fun list2subst (velist,vclist,vmlist) = 
	    let val subst = foldl efolder empty_subst velist
		val subst = foldl cfolder subst vclist
		val subst = foldl mfolder subst vmlist
	    in  subst
	    end
    end

      local
	  fun exp_handler count (subst : subst) (e,bound) = 
	      (case exp2path e of
		   NONE => NONE
		 | SOME (PATH(p as (v,_))) => 
		       if (member_eq(eq_var,v,bound)) 
			   then NONE
		       else (case Name.PathMap.find(#1 subst,p) of
				 NONE => NONE
			       | SOME e => (count := (!count) + 1; SOME e)))
	  fun con_handler count (subst : subst) (c,bound) = 
	      (case con2path c of
		   NONE => NONE
		 | SOME (PATH(p as (v,_))) => 
		       if (member_eq(eq_var,v,bound)) 
			   then NONE
		       else (case Name.PathMap.find(#2 subst,p) of
				 NONE => NONE
			       | SOME c => (count := (!count) + 1; SOME c)))
	  fun mod_handler count (subst : subst) (m,bound) = 
	      (case mod2path m of
		   NONE => NONE
		 | SOME (PATH(p as (v,_))) => 
		       if (member_eq(eq_var,v,bound)) 
			   then NONE
		       else (case Name.PathMap.find(#3 subst,p) of
				 NONE => NONE
			       | SOME m => (count := (!count) + 1; SOME m)))
	  fun sig_handler count (subst : subst) s =
	      (case s of
		   SIGNAT_VAR v => (case Name.VarMap.find(#4 subst,v) of
					NONE => NONE
				      | SOME v' => (count := (!count) + 1; SOME (SIGNAT_VAR v')))
		 | _ => NONE)

	  fun handlers count subst =
	       STATE(default_bound,
		       {sdec_handler = default_sdec_handler,
			exp_handler = exp_handler count subst,
			con_handler = con_handler count subst,
			mod_handler = mod_handler count subst,
			sig_handler = sig_handler count subst})
	  fun wrap f_obj (obj,subst) = if (subst_is_empty subst)
					   then obj
				       else f_obj (handlers (ref 0) subst) obj
	  fun wrap' f_obj (obj,subst) = if (subst_is_empty subst)
					    then (0,obj)
					else let val r = ref 0 
						 val obj = f_obj (handlers r subst) obj
					     in  (!r, obj)
					     end
      in 
	  val exp_subst = wrap f_exp
	  val con_subst = wrap f_con
	  val mod_subst = wrap f_mod
	  val sig_subst = wrap f_signat
	  val con_subst' = wrap' f_con
	  val sig_subst' = wrap' f_signat
      end


      local 
	  fun makeHandler s obj2path (obj,_) = 
	      (case obj2path obj of
		   NONE => NONE
		 | SOME (PATH p) => (s := Name.PathSet.add(!s, p); SOME obj))
	  fun findPaths f_obj obj = 
	      let val set = ref Name.PathSet.empty
		  val st = STATE(default_bound,
				 {sdec_handler = default_sdec_handler,
				  exp_handler = makeHandler set exp2path,
				  con_handler = makeHandler set con2path,
				  mod_handler = makeHandler set mod2path,
				  sig_handler = default_sig_handler})
		  val _ = f_obj st obj
	      in  (!set)
	      end

      in
	  val findPathsInMod = findPaths f_mod
	  val findPathsInSig = findPaths f_signat
	  val findPathsInCon = findPaths f_con
      end

      local
	  fun exp_handler s (VAR v,bound) = 
	                 if (member_eq(eq_var,v,bound)) 
			     then NONE
			 else ((s := Name.VarSet.add(!s, v)); NONE)
	    | exp_handler _ _ = NONE
	  fun con_handler s (CON_VAR v,bound) = 
	                 if (member_eq(eq_var,v,bound)) 
			     then NONE
			 else ((s := Name.VarSet.add(!s, v)); NONE)
	    | con_handler _ _ = NONE
	  fun mod_handler s (MOD_VAR v,bound) = 
	                 if (member_eq(eq_var,v,bound)) 
			     then NONE
			 else ((s := Name.VarSet.add(!s, v)); NONE)
	    | mod_handler _ _ = NONE
	  fun sig_handler s (SIGNAT_VAR v) =
	               ((s := Name.VarSet.add(!s, v)); NONE)
	    | sig_handler _ _ = NONE

	  fun handlers free =
	       STATE(default_bound,
		       {sdec_handler = default_sdec_handler,
			exp_handler = exp_handler free,
			con_handler = con_handler free,
			mod_handler = mod_handler free,
			sig_handler = sig_handler free})
	  fun wrap f_obj obj = let val free = ref Name.VarSet.empty
				      val _ = f_obj (handlers free) obj
				  in  !free
				  end
      in 
	  val exp_free = wrap f_exp
	  val con_free = wrap f_con
	  val mod_free = wrap f_mod
	  val sig_free = wrap f_signat
	  val sbnd_free = wrap f_sbnd
	  val entry_free = wrap f_entry
      end


      fun con_subst_conapps (argcon, con_app_handler : (con * con list -> con option)) : con = 
	let 
	  fun con_handler (CON_APP(c1,cargs),_) = con_app_handler(c1,cargs)
	    | con_handler _ = NONE
	  val handlers = STATE(default_bound,
			       {sdec_handler = default_sdec_handler,
				exp_handler = default_exp_handler,
				con_handler = con_handler,
				mod_handler = default_mod_handler,
				sig_handler = default_sig_handler})
	in f_con handlers argcon
	end


      fun find_tyvars_flexes con = 
	  let val tyvars = ref []
	      val flexes = ref []
	      (* opt is not in acc *)
	      fun compress tv acc opt = 
		  (case tyvar_deref tv of
		       NONE => ((case opt of 
				     NONE => ()
				   | SOME c => app (fn tv => tyvar_reset(tv,c)) acc);
				false)
		     | SOME (c as CON_TYVAR tv2) => compress tv2 (tv::acc) (SOME c)
		     | SOME c => (app (fn tv => tyvar_reset(tv,c)) acc; true))
	      fun help in_array_ref argcon = 
		  let
		      fun con_handler (c : con,bound : var list) = 
			  (case c of
			       CON_TYVAR tyvar =>
				   let val seen = member_eq(eq_tyvar,tyvar,map #2 (!tyvars))
				       val set =  compress tyvar [] NONE
				       val _ = if (seen orelse set)
						   then ()
					       else (tyvars := (in_array_ref,tyvar) :: (!tyvars))
				   in  NONE
				   end
			      | CON_FLEXRECORD r => (flexes := r :: (!flexes); NONE)
			      | CON_ARRAY elemc => (help true elemc; SOME c)
			      | CON_REF elemc => (help true elemc; SOME c)
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
      


      fun ConApply (reduce_small,cfun,cargs) = 
	  let val default = CON_APP(cfun,cargs)
	  in  (case cfun of
		   CON_FUN (vars, body) =>
		       let val argsShort = andfold (fn CON_VAR _ => true | _ => false) cargs
			   val subst = foldl (fn ((v,c),s) => subst_add_convar(s,v,c)) 
			               empty_subst (zip vars cargs)
			   val (count,res) = con_subst'(body, subst)
		       in  if (argsShort orelse (not reduce_small) orelse count <= length vars)
			       then res
			   else default
		       end		       
		 | _ => default)
	  end


      local
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
      in
	  fun mod_resolved m = (f_mod (make_resolved_handlers()) m; true) 
	      handle UNRESOLVED => false
		  
	  fun sig_resolved s = (f_signat (make_resolved_handlers()) s; true) 
	      handle UNRESOLVED => false
      end  

      local
	  fun size_handler () =
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
	  fun wrap f_obj obj = let val (count,handlers) = size_handler()
				   val _ =  f_obj handlers obj
			       in  !count
			       end

      in
	  val con_size = wrap f_con
	  val mod_size = wrap f_mod
	  val bnd_size = wrap f_bnd
	  val sig_size = wrap f_signat
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
	 
    val questionable_str = "+Q"
    val open_str = "+O"
    val nonexport_str = "-X"
    val eq_str = "+E"
    val dt_str = "+O+D"
    val cluster_str = "+C"

    fun is_questionable lab =  isSome(substring (questionable_str,label2name lab))
    fun is_open lab = isSome(substring (open_str,label2name lab))
    fun is_nonexport lab =  isSome(substring (nonexport_str,label2name lab))
    fun is_eq lab = isSome(substring (eq_str,label2name lab))
    fun is_dt lab = isSome(substring (dt_str,label2name lab))
    fun is_cluster lab = isSome(substring (cluster_str,label2name lab))

    local
	fun to_meta_lab meta_str lab =
	  let val str = label2name lab
	      val final_str = meta_str ^ str
	  in  internal_label final_str
	  end
    in  val to_questionable = to_meta_lab questionable_str
	val to_open = to_meta_lab open_str 
	val to_nonexport = to_meta_lab nonexport_str
	val to_eq = to_meta_lab eq_str
	val to_dt = to_meta_lab dt_str
	val to_cluster = to_meta_lab cluster_str
    end

    local 
	fun split str = 
	    let val len = size str
		fun loop n = if ((n+1) < len andalso 
				 (String.sub(str,n) = #"+" orelse
				  String.sub(str,n) = #"-"))
				 then loop (n+2) else n
		val start = loop 0
	    in  (String.substring(str,0, start),
		 String.substring(str,start,len - start))
	    end

    in
	fun prependToInternalLabel (prefix, lab) = 
	    let val str = Name.label2name lab
		val (attributes, name) = split str
		val name = prefix ^ name
	    in  internal_label(attributes ^ name)
	    end
	
	fun label2name lab = 
	    let val str = Name.label2name lab
		val (attributes, name) = split str
	    in  name
	    end
    end

    fun exp_reduce e : exp option = 
	(case e of
	     OVEREXP(_,_,oe) =>
		 (case (oneshot_deref oe) of
		      SOME exp => (case exp_reduce exp of
				       NONE => SOME exp
				     | SOME e => SOME e)
		    | NONE => NONE)
	   | APP(e1, e2) => 
		 let val (ch1,e1) = (case exp_reduce e1 of
					 NONE => (false,e1)
				       | SOME e => (true,e))
		     val (ch2,e2) = (case exp_reduce e2 of
					 NONE => (false,e2)
				       | SOME e => (true,e))
		     val def = if (ch1 orelse ch2) then SOME(APP(e1,e2)) else NONE
		 in (case (e1,e2) of
			 (ETAPRIM(p,cs),RECORD rbnds) => SOME(PRIM(p,cs,map #2 rbnds))
		       | (ETAILPRIM(ip,cs),RECORD rbnds) => SOME(ILPRIM(ip,cs,map #2 rbnds))
		       | (ETAPRIM(p,cs),y) =>
			     (case (IlPrimUtil.get_type' p cs) of
				  CON_ARROW([_],_,_,_) => SOME(PRIM(p,cs,[y]))
				| _ => def)
		       | (ETAILPRIM(ip,cs),y) =>
			      (case (IlPrimUtil.get_iltype' ip cs) of
				   CON_ARROW([_],_,_,_) => SOME(ILPRIM(ip,cs,[y]))
				 | _ => def)
		       | (FIX (false,_,[FBND(name,arg,argtype,bodytype,body)]), VAR argvar) => 
				   SOME(exp_subst(body, subst_add_expvar(empty_subst, arg, VAR argvar)))
		       | (FIX (false,_,[FBND(name,arg,argtype,bodytype,body)]), y) => 
				   SOME(make_let([BND_EXP(arg,y)],body))
		       | _ => def)
		 end
	   | _ => NONE)


    fun ConUnroll con = 
	(case con of
	     CON_MU (CON_FUN ([v], body)) =>
	     let val subst = list2subst([],[(v,con)],[])
	     in  con_subst(body,subst)
	     end
	   | CON_TUPLE_PROJECT(i, con_mu as (CON_MU (CON_FUN (vars, body)))) =>
	     let val vars_cons = mapcount (fn (i,v) => (v,CON_TUPLE_PROJECT(i, con_mu))) vars
		 val subst = list2subst([],vars_cons,[])
	     in  CON_TUPLE_PROJECT(i, con_subst(body, subst))
	     end
	   | _ => (print "Cannot unroll this con: ";
		   pp_con con; print "\n";
		   error "Bad con to ConUnroll"))
  end
