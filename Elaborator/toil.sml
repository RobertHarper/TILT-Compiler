(* todo : optimize coercion functors to recognize when it is entirely unndeeded 
          optimize coercion functions of polymorphic values to be identity by normalizing type argument positions
*)

(* The translation from EL to IL *)
functor Toil(structure Il : IL
	     structure IlStatic : ILSTATIC
	     structure IlUtil : ILUTIL
	     structure Ppil : PPIL
	     structure IlLookup : ILLOOKUP
	     structure Basis : BASIS
	     structure Pat : PAT
	     structure AstHelp : ASTHELP
	     structure InfixParse : INFIXPARSE
	     structure Datatype : DATATYPE
	     sharing IlLookup.Il = InfixParse.Il = Pat.Il = Ppil.Il = Basis.Il 
	       = IlUtil.Il = IlStatic.Il = Datatype.Il = Il
	     sharing Datatype = Basis.Datatype)
   : TOIL =  
  struct

    structure Il = Il
    open AstHelp Il IlStatic IlUtil Ppil Basis Pat
    open Util Listops Name IlLookup Tyvar
    open Prim

    val error = fn s => error "toil.sml" s
    val debug = ref false
    fun debugdo t = if (!debug) then (t(); ()) else ()
    fun nada() = ()

    type tyvar = (decs,con) Tyvar.tyvar
    type ocon = (decs,con) Tyvar.ocon

    (*  ------- Table to hold overload, eq, and flex entries -------
      -------------------------------------------------------------- *)
    local 
      val overload_table = ref ([] : (ocon list))
      val eq_table = ref ([] : (tyvar * exp Util.oneshot) list)
      val flex_table = ref ([] : (label * flexinfo ref * con * exp Util.oneshot) list)
    in
	fun clear_tables () = (overload_table := [];
			       flex_table := [];
			       eq_table := [])
	    
	fun get_overload_table () = !overload_table
	fun add_overload_entry ocon = (overload_table := ocon::(!overload_table))

	fun get_flex_table () = !flex_table
	fun add_flex_entry (l,rc,fc,e) = (print "ADDING FLEX ENTRY\n";
					  flex_table := (l,rc,fc,e)::(!flex_table))
	    
	fun get_eq_table () = !eq_table
	fun add_eq_entry (tyvar,expos) = (eq_table := (tyvar,expos)::(!eq_table))
    end


    (* ----- Polymorphic (sdec) Instantiation ------------------------ *)
    (* given a list of sdecs, return a triple consisting of
      (1) sbnds with the fresh types
      (2) the same sdecs except replace each opaque type with a fresh type
      (3) the fresh types *)
     fun poly_inst (decs,sdecs) : sbnds * sdecs * con list = 
       let
	   fun help sdecs =
	       (case sdecs of
		    [] => ([],[],[])
		  | (SDEC(l1,DEC_CON(v1,k1,NONE)) :: SDEC(l2,DEC_EXP(v2,c2)) :: rest) =>
			let val tyvar = fresh_named_tyvar (decs,"eq_tyvar")
			    val _ = tyvar_use_equal tyvar
			    val con = CON_TYVAR tyvar
			    val eq_con = CON_ARROW(con_tuple[con,con],con_bool,oneshot_init PARTIAL)
			    val exp_os = oneshot()
			    val _ = add_eq_entry(tyvar,exp_os)
			    val e2 = OVEREXP(eq_con,true,exp_os)
			    val (sbnds,sdecs,cons) = help rest
			    val sbnds' = (SBND(l1,BND_CON(v1,con))) ::
					  (SBND(l2,BND_EXP(v2,e2))) :: sbnds
			    val sdecs' = (SDEC(l1,DEC_CON(v1,k1,SOME con)))::
					  (SDEC(l2,DEC_EXP(v2,c2))) :: sdecs
			    val cons' = con :: cons
			in (sbnds',sdecs',cons')
			end
		  | (SDEC(l,DEC_CON(v,k,NONE))::rest) =>
			let val c = fresh_con decs
			    val (sbnds,sdecs,cons) = help rest
			in ((SBND(l,BND_CON(v,c)))::sbnds,
			    (SDEC(l,DEC_CON(v,k,SOME c)))::sdecs,
			    c::cons)
			end
		  | _ => (print "poly_inst got strange sdecs:\n";
			  pp_sdecs sdecs;
			  error "poly_inst received strange sdecs"))
       in help sdecs
       end

val s1 = ref (SIGNAT_STRUCTURE[])
val s2 = ref (SIGNAT_STRUCTURE[])
val decs1 = ref ([] : Il.dec list)


    (* ---------- Polymorphic function Instantiation ------------------ *)
    fun polyfun_inst (context, module : mod, s : signat) : exp * con = 
       let 
	 val _ = debugdo (fn () => (print "polyfun_inst called with module:\n";
				     pp_mod module; print "\nand sig = \n";
				     pp_signat s; print "\n\n"))
	 val (e,c) = polyfun_inst' (context,module,s)
	 val _ = debugdo (fn () => (print "and returning e =\n";
				    pp_exp e; print "\nand c =\n";
				    pp_con c; print "\n"))
       in (e,c)
       end
    and polyfun_inst' (context, module : mod, s : signat) : exp * con = 
       (case s of 
	  (SIGNAT_FUNCTOR(v,SIGNAT_STRUCTURE arg_sdecs, 
			  SIGNAT_STRUCTURE [SDEC(_,DEC_EXP(resv,resc))],_)) =>
	  let 
	      val _ = debugdo (fn () => (print "polyfun_inst' got module of:\n";
					 Ppil.pp_mod module;
					 print "\n\n  and signature of:\n";
					 Ppil.pp_signat s;
					 print "\n\n"))
	      val decs = context2decs context
	    local 
		fun dotype l v = let 
				     val tv = fresh_tyvar decs
				     val c = CON_TYVAR tv
				 in (tv,(SBND(l,BND_CON(v,c)),
					 SDEC(l,DEC_CON(v,KIND_TUPLE 1, SOME c)))
(*					 SDEC(l,DEC_CON(v,KIND_TUPLE 1, NONE)), *)
				     )
				 end
		fun help ([] : sdecs) = []
	          | help [SDEC(l,DEC_CON(v,_,_))] = [#2(dotype l v)]
		  | help (SDEC(l,DEC_CON(v,_,_)) :: 
			  (rest as (SDEC(_,(DEC_CON _)) :: _))) = (#2(dotype l v))::(help rest)
		  | help (SDEC(l1,DEC_CON(v1,_,_)) :: SDEC(l2,DEC_EXP(v2,c2)) :: rest) =
		    let 
			val (tyvar,(sbnd1,sdec1)) = dotype l1 v1
			val _ = tyvar_use_equal tyvar
			val con = CON_TYVAR tyvar
			val eq_con = CON_ARROW(con_tuple[con,con],con_bool,oneshot_init PARTIAL)
			val exp_os = oneshot()
			val _ = add_eq_entry(tyvar,exp_os)
			val eqexp = OVEREXP(eq_con,true,exp_os)
			val sbnd2 = SBND(l2,BND_EXP(v2,eqexp))
			val sdec2 = SDEC(l2,DEC_EXP(v2,c2))
		    in (sbnd1,sdec1) :: (sbnd2,sdec2) :: (help rest)
		    end
		  | help _ = error "unexpected sig to arg struct of polymorphic fun"
		val temp = help arg_sdecs
	    in 
	      val decs = context2decs context
	      val mod_poly = MOD_STRUCTURE(map #1 temp)
	      val signat_poly_sdecs = map #2 temp
	      val signat_poly = SIGNAT_STRUCTURE signat_poly_sdecs
	    end
	    val new_rescon : con = remove_modvar_type(resc,v,signat_poly_sdecs)
	    val _ = debugdo (fn () => (print "\n*********\nabout to make_non_dependent_type with resc = \n";
				       pp_con resc;
				       print "\n"))
	    val signat_temp = SIGNAT_STRUCTURE[SDEC
					       (it_lab,
						DEC_EXP(resv,new_rescon))]
	    val _ = debugdo (fn () => (print "\n*********\ndone make_non_dependent_type with signat_temp:\n";
				       pp_signat signat_temp; print "\n\n"))
	    val signat'= SIGNAT_FUNCTOR(v,signat_poly,signat_temp,oneshot_init PARTIAL)
	    val _ = if (Sig_IsSub(decs,s,signat')) then ()
		    else (print "s is\n";
			  pp_signat s; print "\nsignat' is \n";
			  pp_signat signat'; print "\n";
			  s1 := s; s2 := signat'; decs1 := decs;
			  error "Failed rule 24 Sig_IsSub")
	    val exp = MODULE_PROJECT(MOD_APP(module,mod_poly), it_lab)
	    val _ = debugdo (fn () => (print "polyfun_inst returning exp :\n";
				       pp_exp exp; print "\n\n  and rescon:\n";
				       pp_con new_rescon; print "\n\n"))
	  in (exp,new_rescon)
	  end
       |  _ =>  (print "polyfun_inst received s of\n";
		 pp_signat s;
		 print "\n";
		 error "rule 224 or 226 not applicable in polyfun_inst"))


    (* ---------- Least Super Signature Computation ------------------ *)
    (* Given a functor signature (v : s1 -> s2), 
         return the least super-signature of s2 with no references to v *)
    fun least_super_sig(argv, SIGNAT_STRUCTURE sdecs1, SIGNAT_STRUCTURE sdecs2) = 
	let
	    fun getsubst [] = []
	      | getsubst ((SDEC(l,DEC_CON(v,k,SOME c)))::rest) = ((l,k),(v,c))::(getsubst rest)
	      | getsubst (_::rest) = getsubst rest
	    val subst = getsubst sdecs1
	    fun normalize acc1 acc2 quads =
		(case (map (fn ((l,k),(v,c)) => ((l,k),(v,con_subst_var(c,acc2)))) quads) of
		     [] => (acc1,acc2)
		   | ((a1,a2)::b) => normalize (a1::acc1) (a2::acc2) b)
            val (acc1,acc2) = normalize [] [] subst
	    val sdecs = map2 (fn ((l,k),(v,c)) => SDEC(l,DEC_CON(v,k,SOME c))) (acc1,acc2)
	    val res = SIGNAT_STRUCTURE(map (fn sdec => remove_modvar_sdec(sdec,argv,sdecs)) sdecs2)
(* should attempt to remove all occurrences while retaining more sharing *)
	    val _ = debugdo (fn () => 
			     (print "least_super_sig got argv = "; pp_var argv;
			      print "\n and sdecs1 = "; pp_sdecs sdecs1;
			      print "\n and sdecs2 = "; pp_sdecs sdecs2;
			      print "\n and intermediate sdecs = "; pp_sdecs sdecs;
			      print "\n and returned res = "; pp_signat res;
			      print "\n"))
	in  res
	end
      | least_super_sig(v, _, _) = error "least_super_sig not called with two structure signatures"

     (* ----------------- Helper Functions ----------------------- *)


     (* make inlineable by substituting all variables.  
      However, substituing an expression is not legal except in certain cases *)
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
				     then raise FAIL
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
	   | _ => NONE)


     fun add_inline_module (context,label,var,module,signat) =
	 case (make_inline_module (context,module)) of
	     SOME norm_mod =>
		 let
		     val _ = (print "original module is:\n";
			      pp_mod module;
			      print "result of inlinemodule is:\n";
			      pp_mod norm_mod;
			      print "\n")
		     val signat = GetModSig(context2decs context,norm_mod)
		     val inline = INLINE_MODSIG(norm_mod, signat)
		 in  add_context_inline(context,label,var,inline)
		 end
	   | NONE => add_context_module(context,label,var,signat)


     fun sbnd_ctxt_list2modsig (sbnd_ctxt_list : (sbnd option * context_entry) list) 
	 : (mod * signat) = 
	 let fun loop (acc1,acc2) arg = 
	     (case arg of 
		  [] => (acc1,acc2)
		| ((NONE,_)::rest) => loop (acc1,acc2) rest
		| ((SOME sbnd,CONTEXT_SDEC sdec)::rest) => loop (sbnd::acc1,sdec::acc2) rest
		| ((SOME sbnd,_)::_) => error "sbnd_ctxt_list2modsig: sbnd without sdec")
	     val (sbnds,sdecs) = loop ([],[]) sbnd_ctxt_list
	 in (MOD_STRUCTURE sbnds, SIGNAT_STRUCTURE sdecs)
	 end

     fun boolsbnd_ctxt_list2modsig (boolsbnd_ctxt_list : ((bool * sbnd) option * context_entry) list) 
	 : (mod * signat) = sbnd_ctxt_list2modsig(map (fn (SOME(_,sbnd),ctxt) => (SOME sbnd,ctxt)
                                                        | (NONE,ctxt) => (NONE,ctxt)) boolsbnd_ctxt_list)

     fun add_context_boolsbnd_ctxts 
	 (context : context,
	  boolsbnd_ctxt : ((bool * sbnd) option * context_entry) list) : sbnd list * context = 
	 let 
	     fun loop (sbnds,ctxt) arg = 
		 case arg of
		     [] => (sbnds,ctxt)
		   | ((NONE,CONTEXT_SIGNAT(l,v,s))::rest) => loop (sbnds,add_context_signat(ctxt,l,v,s)) rest
		   | ((NONE,_)::rest) => loop (sbnds,ctxt) rest
		   | ((SOME (flag,sbnd as SBND(l,bnd)), CONTEXT_SDEC (sdec as SDEC(l',dec)))::rest) => 
			 let 
			     val sbnds' = sbnd::sbnds
			     val ctxt' = 
				 (case (flag,bnd,dec) of
				      (true,BND_MOD(v,m),DEC_MOD(v',s)) => 
					  if (eq_label(l,l') andalso (eq_var(v,v')))
					      then add_inline_module(ctxt,l,v,m,s)
					  else error "add_context_boolsbnd_ctxts: inconsistent sbnd_sdeclist"
				    | _ => add_context_sdecs(ctxt,[sdec]))
			 in
			     loop (sbnds',ctxt') rest 
			 end
		   | ((SOME _,_)::rest) => error "add_context_boolsbnd_ctxts: cannot have sbnd without sdec"
	 in loop ([],context) boolsbnd_ctxt
	 end

     (* -- translate and package a list of objects using an object translator 
           inputs  : a translator that takes objects to a list of
	               (optional) bindings and context-entries
		     the bool in the binding pair indicates whether
		       it should be inlined
                     a context
                     a list of objects
           outputs : a list of (optional) bindings and context-entries
     *)
     fun packagedecs (xobj : context * 'a -> ((bool * sbnd) option * context_entry) list)
	 context (objs : 'a list) : (sbnd option * context_entry) list = 
	 let 
	     fun loop context [] = []
	       | loop context [obj] = xobj(context,obj)
	       | loop context (obj::rest) =
		 let 
		     val boolsbnd_ctxt_list = xobj(context,obj)
		     val (lbl1,lbl2) = (fresh_open_internal_label "lbl1", fresh_open_internal_label "lbl2")
		     val var1 = fresh_var()
		     val var2 = fresh_var()
		     val (_,context') = add_context_boolsbnd_ctxts(context,boolsbnd_ctxt_list)
		     val boolsbnd_ctxt_restlist = loop context' rest
		 in boolsbnd_ctxt_list @ boolsbnd_ctxt_restlist
		 end
	     fun help (NONE,ce) = (NONE,ce)
	       | help (SOME(_,sbnd),ce) = (SOME sbnd, ce)
	 in map help (loop context objs)
	 end

    (* --------------------------------------------------------- 
      ------------------ EXPRESSIONS --------------------------
      --------------------------------------------------------- *)
	  
     fun xexp (context : context, exp : Ast.exp) : (exp * con) = 
      (case exp of
	 Ast.IntExp is =>   (SCON(int(W32,TilWord64.fromDecimalString is)), CON_INT W32)
       | Ast.WordExp ws =>  (SCON(uint(W32,TilWord64.fromWordStringLiteral ws)), CON_UINT W32)
       | Ast.RealExp s =>   (SCON(float(F64,s)), CON_FLOAT F64)
       | Ast.StringExp s => (SCON(vector (Array.fromList
					  (map (fn c => SCON(uint(W8,TilWord64.fromInt (ord c))))
					   (explode s)))), con_string)
       | Ast.CharExp s =>   (SCON(uint(W8,
				       (case (explode s) of
					    [c] => TilWord64.fromInt (ord c)
					  | _ => error "Ast.CharExp carries bad string"))),
			     CON_UINT W8)
       | Ast.TupleExp (exp_list) => let val recexp = Ast.RecordExp(mapcount (fn(n,a) => 
									     (generate_tuple_symbol (n+1),a))
								   exp_list)
				    in xexp(context, recexp)
				    end
       | Ast.RecordExp (sym_exp_list) => 
	     (let val label_exp_list = map (fn (s,e) => (symbol_label s,e)) sym_exp_list
		  val label_exp_list = sort_labelpair label_exp_list
		  fun doer(label,expr) = let val (exp,con) = xexp(context,expr)
					 in ((label,exp),(label,con))
					 end
		  val bnd_dec_list = map doer label_exp_list
	      in (RECORD(map #1 bnd_dec_list), CON_RECORD(map #2 bnd_dec_list))
	      end)
       | Ast.ListExp args => let fun loop [] = AstHelp.nil_exp
				   | loop (a::b) = Ast.AppExp{function=AstHelp.cons_exp,
							      argument=Ast.TupleExp[a,loop b]}
			     in xexp(context,loop args)
			     end
       | Ast.SelectorExp s => 
	     (* for the record, i'd like to say that this construct is a pain in the butt - Perry *)
	     let 
		 val label = symbol_label s
		 val stamp = get_stamp()
		 val decs = context2decs context
		 val fieldcon = fresh_con decs
		 val the_ref = ref(FLEXINFO(stamp,false,[(label,fieldcon)]))
		 val rescon = CON_ARROW(CON_FLEXRECORD the_ref,
					fieldcon,oneshot_init TOTAL)
		 val eshot : exp Util.oneshot = oneshot()
		 val exp = OVEREXP(rescon,true,eshot)
		 val _ = add_flex_entry(label,the_ref,fieldcon,eshot)
	     in (exp,rescon)
	     end
       | Ast.VarExp path => 
	   if (path = [Symbol.varSymbol "="]) 
	     then let val exp_os = oneshot()
		      val decs = context2decs context
		      val tyvar = fresh_named_tyvar (decs,"teq")
		      val _ = tyvar_use_equal tyvar
		      val con = CON_TYVAR tyvar
		      val eq_con = CON_ARROW(con_tuple[con,con],con_bool,oneshot_init PARTIAL)
		      val _ = add_eq_entry(tyvar,exp_os)
		  in (OVEREXP(eq_con,true,exp_os),eq_con)
		  end
	   else 
	     (case (Context_Lookup(context,map symbol_label path)) of
	       PHRASE_CLASS_EXP ec => ec
	     | PHRASE_CLASS_OVEREXP thunk =>
			let 
			  val decs = context2decs context
			  val (exp,ocon) = thunk()
			  val _ = add_overload_entry ocon
			in (exp,CON_OVAR ocon)
			end
	     | PHRASE_CLASS_MOD (m,s as SIGNAT_FUNCTOR _) => polyfun_inst (context,m,s)
	     | PHRASE_CLASS_MOD (m,s) =>
		 let 
		   val decs = context2decs context
		   val mk_exp = MODULE_PROJECT(m,mk_lab)
		   val mk_mod = MOD_PROJECT(m,mk_lab)
		 in (mk_exp,GetExpCon(decs,mk_exp))
		   handle (FAILURE _) => (* Rule 226 *)
		     let val signat_mk = GetModSig(decs,mk_mod)
		     in  polyfun_inst(context,mk_mod,signat_mk)
		     end
		 end
	     | (PHRASE_CLASS_CON _) => error "Ast.VarExp path leads to CON"
	     | (PHRASE_CLASS_SIG _) => error "Ast.VarExp path leads to SIG")
       | Ast.LetExp {dec,expr} => 
	     let val boolsbnd_ctxt_list = xdec'(context, dec)
		 val (sbnds,context') = add_context_boolsbnd_ctxts(context,boolsbnd_ctxt_list)
		 val (e,c) = xexp(context',expr)
		 val bnds = map (fn (SBND(_,bnd)) => bnd) sbnds
	     in  (LET(bnds,e),c)
	     end
       | Ast.FlatAppExp _ => (* let val _ = debugdo (fn () => (print "FlatAppExp... context is:\n";
							    pp_context context;
							    print "\n"))
			     in ... end *)
	                     xexp(context,InfixParse.parse_exp(Context_Get_FixityTable context, exp))
       | Ast.AppExp {argument,function} => let val (e1',con1) = xexp(context,function)
					       val (e2',con2) = xexp(context,argument)
					       val decs = context2decs context
					       val spec_rescon = fresh_con decs
					       val spec_funcon = CON_ARROW(con2,spec_rescon, oneshot())
					       val _ =  con_unify'(context,"",
								   ("function type", con1),
								   ("argument -> ? type", spec_funcon),
								   fn () => (pp_exp e1';
									     print "\n\n";
									     pp_exp e2'))
					   in (APP(e1',e2'),con_deref spec_rescon)
					   end
       | Ast.AndalsoExp (e1,e2) => xexp(context,Ast.IfExp{test=e1,thenCase=e2,elseCase=AstHelp.false_exp})
       | Ast.OrelseExp (e1,e2) => xexp(context,Ast.IfExp{test=e1,thenCase=AstHelp.true_exp,elseCase=e2})
       | Ast.IfExp {test,thenCase,elseCase} => let val pr1 = {pat=true_pat,exp=thenCase}
						   val pr2 = {pat=false_pat,exp=elseCase}
					       in xexp(context,Ast.CaseExp {expr=test, 
									    rules=[Ast.Rule pr1, Ast.Rule pr2]})
					       end
       | Ast.ConstraintExp {expr, constraint} => let val (exp,con) = xexp(context,expr)
						     val (con') = xty(context,constraint)
						 in con_unify'(context,"Contrained expression",
							       ("expressionn type",con),
							       ("constraint type",con'), nada);
						    (exp,con)
						 end
       | Ast.VectorExp elist => error "VectorExp not handler"
       | Ast.WhileExp {test,expr} => error "WhileExp not handled"
       | Ast.HandleExp {expr,rules} => (* almost same as CaseExp except need to wrap with HANDLE *)
		       let 
			   val (exp',rescon) = xexp(context,expr)
			   val v = fresh_var()
			   val patarg = {context = context,
					 typecompile = xty,
					 expcompile = xexp,
					 polyinst = poly_inst}
			   val arms = map (fn (Ast.Rule{pat,exp})=>(pat,exp)) rules
			   val handler_body = caseCompile{patarg = patarg,
							  arms = arms,
							  arg = (VAR v,CON_ANY)}
			   val handler = make_lambda(v,CON_ANY,#2 handler_body,#1 handler_body)
			   val _ = con_unify'(context,"handle",
					      ("body type",rescon),
					      ("handler body type", #2 handler_body), nada)
		       in (HANDLE(exp',#1 handler),rescon)
		       end
       | Ast.RaiseExp e => let val (exp,con) = xexp(context,e)
			       val _ = con_unify'(context,"raise",("ANY",CON_ANY),("con",con),nada)
			       val decs = context2decs context
			       val c = fresh_con decs
			   in (RAISE (c,exp),c)
			   end
       | Ast.SeqExp elist => let fun loop [] = error "Ast.SeqExp with empty list"
				   | loop [e] = xexp(context,e)
				   | loop (e::erest) = let val (e',c') = xexp(context,e)
							   val (erest',erestcon) = loop erest
							   val (abs,_) = make_lambda(fresh_var(),c',
										     erestcon,erest')
						       in (APP(abs,e'),erestcon)
						       end
			     in  loop elist
			     end
       | Ast.FnExp [] => error "Ast.FnExp with empty list"
       | Ast.FnExp rules => let val patarg = {context = context, 
					      typecompile = xty, 
					      expcompile = xexp, 
					      polyinst = poly_inst}
				val arms = map (fn (Ast.Rule{pat,exp}) => ([pat],exp)) rules
				val {arg=(argvar,argcon),body=(bexp,bcon)} = funCompile{patarg = patarg,
											rules = arms,
											reraise = false}
				val ec = make_lambda(argvar,argcon,bcon,bexp)
			    in ec
			    end
       | Ast.CaseExp {expr,rules} =>  
			    let fun getarm (Ast.Rule{pat,exp}) = (pat,exp)
			        val arms = map getarm rules
				val (arge,argc) = xexp(context,expr)
				val patarg = {context = context,
					      typecompile = xty,
					      expcompile = xexp,
					      polyinst = poly_inst}
				val (e,c) = caseCompile{patarg = patarg,
							arms = arms,
							arg = (arge,argc)}
			    in (e,c)
			    end
       | Ast.MarkExp(exp,region) => xexp(context,exp))


    (* --------------------------------------------------------- 
      ------------------ DECLARATIONS --------------------------
      --------------------------------------------------------- *)
      and to_eq_lab type_lab =
	  let
	      val type_str = label2string type_lab
	      val eq_str = type_str ^ "_eq"
	      val eq_lab = internal_label eq_str
	  in
	      eq_lab
	  end

      and make_typearg_sdec [] = []
        | make_typearg_sdec ((type_lab,is_eq) :: more) = 
	 let 
	     val rest = make_typearg_sdec more
	     val type_str = label2string type_lab
	     val eq_str = type_str ^ "_eq"
	     val type_var = fresh_named_var type_str
	     val type_sdec = SDEC(type_lab,DEC_CON(type_var, KIND_TUPLE 1, NONE))
	     val eq_lab = internal_label eq_str
	     val eq_var = fresh_named_var eq_str
	     val eq_con =  CON_ARROW(con_tuple[CON_VAR type_var, CON_VAR type_var],
				     con_bool, oneshot_init PARTIAL)
	     val eq_sdec = SDEC(eq_lab,DEC_EXP(eq_var, eq_con))
	 in  if (is_eq) then (type_sdec :: eq_sdec :: rest) else type_sdec :: rest
	 end
	
     and xdec' (context : context, d : Ast.dec) : ((bool * sbnd) option * context_entry) list = 
       let 
	   fun strip (Ast.MarkDec(d,r)) = strip d
	     | strip d = d
	   val d = strip d
	   val sbndsdec_list = xdec(context,d)
	   val inlineflag = (case d of
				 (Ast.DatatypeDec {datatycs,withtycs}) => false
			       | _ => false)
	   fun help (SOME b,d) = (SOME(inlineflag,b),d)
	     | help (NONE, d) = (NONE,d)
       in  map help sbndsdec_list
       end


     and xdec (context : context, d : Ast.dec) : (sbnd option * context_entry) list =
       (case d of
          (* --- The tricky thing about this value declarations is figuring 
	     --- out what type variables need to be generalized.  There are two
             --- sources: those arising explicitly from the program text and those
             --- arising from the type inference process.  
	     --- (1) Source-level Type Variables:
             ---     In the former case, these explicit type variables must be put into
             ---     the translation context before encountering.  This can be done by
             ---     a prepass over the program to explicitly scope these user-level type varaibles.
             ---     In ML96, these there is a syntax for the programmer to give these type variables
             ---     explicit scope.  Nonetheless, since this is optional, we must create the
             ---     (narrowest) scope for those variables that the user did not explicitly scope.
             ---     As a matter of efficiency, it would be better to compute the scope of user-level
             ---     type variables in a prepass rather than repeatedly scan the expression for them.
             --- (2) Unresolved meta-variables(unless constrained) are also eligible for type generalization.
             ---     If a meta-variable is to be generalized, it is resolved to a type variables which is
             ---     then generalized.  Unresolved meta-variables can either appear in the type of the
             ---     expression being bound.  These must be generalized.  Meta-variables that are generated
             ---     and appear in the translated expression but not the translated type may be generalized
             ---     or not.  If it is not generalized however, it MUST be resolved at some point to some      
             ---     well-formed type. 
             --- (3) Note that until pattern matching is done, metavariables that seem to be generalizable
             ---     may not be yet. *)
	  Ast.ValDec vblist => (* non-recursive bindings *)
	    let local
		  val pe_list = map vb_strip vblist
		in
		  val (pat,expr) = (case pe_list of
				      [] => error "let with nothing"
				    | [(p,e)] => (p,e)
				    | _ => (Ast.TuplePat(map #1 pe_list),
					    Ast.TupleExp(map #2 pe_list)))
		end
	        (* it would be more efficient to figure this out ahead of time,
		   instead of recomputing this over and over again .... *)
		local fun is_sym_bound s = (case (Context_Lookup(context,[symbol_label s])) of
						PHRASE_CLASS_CON _ => true
					      | _ => error "tyvar symbol bound to a non-type")
		in val tyvars = free_tyvar_exp(expr, is_sym_bound)
		end
		val tyvar_stamp = get_stamp()
		val lbl = fresh_open_internal_label "lbl"
		val lbl' = fresh_internal_label "lbl'"
		val var_poly = fresh_var()
		val var' = fresh_var()
		val var_poly = fresh_named_var "var_poly"
		val decs = context2decs context
		val cons1 = map (fn _ => fresh_con decs) tyvars
		local
		    val labs1 = map (fn s => symbol_label s) tyvars
		    val vars1 = map (fn _ => fresh_named_var "user_gen") tyvars
		in val temp_sdecs = (map3 (fn (v,l,c) => SDEC(l,DEC_CON(v,KIND_TUPLE 1,SOME c))) 
				     (vars1,labs1,cons1))
		end
		val _ = (print "about to compile expression with temp_sdecs = ";
			 pp_sdecs temp_sdecs;
			 print "\n")
		val context' = add_context_module(context,lbl,var_poly,SIGNAT_STRUCTURE temp_sdecs)
		val (e,con) = xexp(context',expr)
		val patarg = {context = context', 
			      typecompile = xty, 
			      expcompile = xexp, 
			      polyinst = poly_inst}
		val sbnd_sdec_list = bindCompile{patarg = patarg,
						 bindpat = pat,
						 arg = (e,con)}
		val _ = debugdo (fn () => (print "about to call rebind_free_type_var:  var_poly = ";
					   pp_var var_poly; print "  stamp = ";
					   print (Int.toString (stamp2int tyvar_stamp));
					   print "\nand con = \n";
					   pp_con con; print"\n\n"))


		val tyvar_lbls'_useeq = rebind_free_type_var(tyvar_stamp,con,context,var_poly)
		val _ = debugdo (fn () => (print "done calling rebind_free_type_var:  var_poly = ";
					   pp_var var_poly; print "\nand con = \n";
					   pp_con con; print"\n\n"))
		val poly_sdecs = make_typearg_sdec(map (fn (_,l,f) => (l,f)) tyvar_lbls'_useeq)
		val (sbnds,sdecs) = (map #1 sbnd_sdec_list, map #2 sbnd_sdec_list)
	    in
		(case poly_sdecs of
		     [] => map2 (fn (sbnd,sdec) => (SOME sbnd, CONTEXT_SDEC sdec)) (sbnds,sdecs)
		   | _ =>
			 let 
			     val sig_poly = SIGNAT_STRUCTURE poly_sdecs
			     val labs = map (fn SDEC (l,_) => l) sdecs
			     val cons = map (fn SDEC(l,DEC_EXP(_,c)) => c | _ => error "Rule 237") sdecs
			     fun mod_help l =
				 let val modapp = MOD_APP(MOD_VAR var',MOD_VAR var_poly)
				     val temp =  MOD_STRUCTURE[SBND(it_lab,
								    BND_MOD(fresh_var(),
									    MOD_PROJECT(modapp,l)))] 
				 in SBND(l,BND_MOD(fresh_var(),MOD_FUNCTOR(var_poly,sig_poly,temp)))
				 end
			     fun sig_help (l,c) = 
				 SDEC(l,DEC_MOD(fresh_var(),
						SIGNAT_FUNCTOR(var_poly,sig_poly,
							       SIGNAT_STRUCTURE[SDEC(it_lab,
										     DEC_EXP(fresh_var(),c))],
							       oneshot_init TOTAL)))
			     val temp_mod = MOD_FUNCTOR(var_poly,sig_poly,MOD_STRUCTURE sbnds)
			     val temp_sig = SIGNAT_FUNCTOR(var_poly,sig_poly,SIGNAT_STRUCTURE sdecs,
							   oneshot_init TOTAL)
			     val final_sbnds = ((SBND(lbl',BND_MOD(var',temp_mod)))::
						(map mod_help labs))
			     val final_sdecs = ((SDEC(lbl',DEC_MOD(var',temp_sig))) ::
						(map2 sig_help (labs, cons)))
			 in map2 (fn (sbnd,sdec) => (SOME sbnd,CONTEXT_SDEC sdec)) (final_sbnds, final_sdecs)
			 end)
	    end
	| binddec as ((Ast.ValrecDec _) | (Ast.FunDec _)) => (* recursive value declarations: i.e. functions *)
	      let
                  (* We must discover all the variables to generalize over.  For the user-level variables,
		     we must also compile in a context with these user-level variables bound.  As a first
		     step, we find all the user-level variables in the program. 
		     At some future point when the syntax includes explicit scoping, 
		     we must include those too *)
		  val tyvar_stamp = get_stamp()
		  val tyvars = 
		      let fun sym_is_bound s = ((case (Context_Lookup(context,[symbol_label s])) of
						    PHRASE_CLASS_CON _ => true
						  | _ => error "tyvar symbol bound to a non-type")
						handle (NOTFOUND _) => false)
		      in free_tyvar_dec(binddec,sym_is_bound)
		      end
		  local
		      fun help tyvar = 
			  let val type_str = Symbol.name tyvar
			      val type_lab = symbol_label tyvar
			      val is_eq =  ((size type_str > 1) andalso 
					    (String.substring(type_str,0,2) = "''"))
			  in  (type_lab,is_eq)
			  end
		      val temp = map help tyvars
		  in  val sdecs1 = make_typearg_sdec temp
		  end

		  val decs = context2decs context
		  val var_poly = fresh_named_var "var_poly"
		  val open_lbl = fresh_open_internal_label "lbl"
		  val context' = add_context_module(context,open_lbl,var_poly,SIGNAT_STRUCTURE sdecs1)
		  val _ = (print "var_poly is "; pp_var var_poly; print "\nand sdecs1 is = \n";
			   pp_sdecs sdecs1;
			   print "\n")

		  local (* ----- we cannonicalize into one format by introducing meta-variables *)
		      fun fb_help clause_list =
			  let 
			      val arg_con = fresh_named_con (decs,"arg_con")
			      val res_con = fresh_named_con (decs,"res_con")
			      fun help (Ast.Clause{pats : Ast.pat Ast.fixitem list, resultty,exp}) =
				  let 
				      fun getitem ({item,...} : Ast.pat Ast.fixitem) = item
				      val (s,restpats) = 
					  (case pats of
					       ({item=Ast.VarPat[s],...}::rest) => (s, map getitem rest)
					 | [p1,{item=Ast.VarPat[s],...},p2] => (s, [Ast.TuplePat[getitem p1,
												 getitem p2]])
					 | _ => error "rule 238: can't find funid")
				      val _  = (case resultty of
						    NONE => ()
						  | SOME ty => 
							let val given_result_con = xty(context',ty)
							in con_unify'(context',"fb_help",
								      ("given_result_con",given_result_con),
								      ("res_con",res_con),nada)
							end)
				  in (symbol_label s, (restpats, exp))
				  end
			      fun getid [] = error "no ids"
				| getid [a] = a
				| getid (a::(rest as b::_)) = if eq_label(a,b) then getid rest 
							      else error "not all ids are same"
			      val temp = map help clause_list
			      val id = getid (map #1 temp)
			      val matches = map #2 temp
			  in (id, (arg_con,res_con), matches)
			  end
		      fun rvb_help {var:Ast.symbol, fixity: (Ast.symbol * 'a) option,
				    exp:Ast.exp, resultty: Ast.ty option} = 
			let 
			    val arg_con = fresh_named_con (decs,"arg_con")
			    val res_con = fresh_named_con (decs,"res_con")
			    val _ = (case resultty of
					 NONE => ()
				       | SOME ty => let val given_fun_con = xty(context',ty)
							val fun_con = CON_ARROW(arg_con,res_con,oneshot())
						    in con_unify'(context',"rvb_help",
								  ("given_fun_con",given_fun_con),
								  ("fun_con",fun_con),nada)
						    end)
			    fun help (Ast.Rule{pat,exp}) = 
				(case pat of
				     Ast.ConstraintPat{constraint=ty,pattern} =>  
					 let val given_res_con = xty(context',ty)
					 in con_unify'(context',"rvb_help",
						       ("given_res_con",given_res_con),
						       ("res_con",res_con),nada);
					     ([pattern],exp)
					 end
				   | _ => ([pat],exp))
			    val matches = (case exp of 
					       Ast.FnExp rules => map help rules
					     | _ => error "val rec requires an fn expression")
			in  (symbol_label var, (arg_con, res_con), matches)
			end
		  in
		      val dec_list : (label * 
				      (con * con) *
				      (Ast.pat list * Ast.exp) list) list =
			  (case d of
			       Ast.ValrecDec rvblist => map (rvb_help o rvb_strip) rvblist
			     | Ast.FunDec fblist => map (fb_help o fb_strip) fblist
			     | _ => error "must have ValrecDec or FunDec here")
		  end (* local ------ ValRecDec and FunDec are assimilated now ---- *)

		  val fun_ids = map #1 dec_list
		  val fun_cons = map #2 dec_list
		  val matches_list = map #3 dec_list
		  val fun_labs = map (fn l => internal_label (label2string l)) fun_ids
		  val fun_vars = map (fn _ => fresh_var()) fun_ids

                  (* --- create the context with all the fun_ids typed --- *)
		  val context_fun_ids = 
		      let fun help ((id,var',(argcon,rescon)),ctxt) = 
			  add_context_var(ctxt,id,var',CON_ARROW(argcon,rescon,oneshot_init PARTIAL))
		      in foldl help context (zip3 fun_ids fun_vars fun_cons)
		      end

		  val context'' = add_context_module(context_fun_ids,open_lbl,var_poly,
						     SIGNAT_STRUCTURE sdecs1)


		  val fbnd_con_list = 
		      (map3 (fn (matches,(argcon,rescon),var') => 
			     let 
				 val patarg = {context = context'', 
					       typecompile = xty, 
					       expcompile = xexp, 
					       polyinst = poly_inst}
				 val {body = (ue,uc), arg = (var1,con1)} = funCompile{patarg = patarg,
										      rules = matches,
										      reraise = false}
				 val _ = con_unify'(context'',"fundef",("argcon",argcon),("con1", con1),nada)
				 val _ = con_unify'(context'',"fundef",("rescon",rescon),("uc",uc),nada)
			     in (FBND(var',var1,con1,uc,ue),
				 CON_ARROW(con1,uc,oneshot_init PARTIAL))
			     end)
		      (matches_list, fun_cons, fun_vars))

		  val fbnds = map #1 fbnd_con_list
		  val exp_con_list = map2 (fn (v',c) => (FIX(fbnds,v'),c)) (fun_vars,map #2 fbnd_con_list)
		  val tyvar_lbls'_useeq = flatten(map (fn (_,c) => 
							(print "about to call rebind_free_type_var:  var_poly = ";
							 pp_var var_poly; print "\nand c = \n";
							 pp_con c; print"\n\n";
							 rebind_free_type_var(tyvar_stamp,c,
									      context_fun_ids,var_poly)))
		                           exp_con_list)
		  local 
		      fun help(_,tlab,iseq) = (tlab,iseq)
		      val temp = map help tyvar_lbls'_useeq
		  in
		      val sdecs2 = make_typearg_sdec temp
		  end
		  val sdecs = sdecs1 @ sdecs2

		  val _ = 
		      let
			  val context_temp = add_context_module(context_fun_ids,open_lbl,var_poly,
								SIGNAT_STRUCTURE sdecs)
			  val decs_temp = context2decs context_temp
		      in app (fn (tv,_,_) => (print "setting sdecs with decs_temp = \n";
					      pp_decs decs_temp;
					      print "\n";
					      tyvar_setdecs(tv,decs_temp))) tyvar_lbls'_useeq 
		      end

		  fun modsig_helper (id,(exp,con)) = 
		      let val v1 = fresh_named_var "unused1"
			  val v2 = fresh_named_var "unused2"
		      in
			  (case sdecs of
			       [] => (SBND(id,BND_EXP(v2,exp)),
				      SDEC(id,DEC_EXP(v2,con)))
			     | _ => let 
					val sig_poly = SIGNAT_STRUCTURE sdecs
					val functor_mod = MOD_FUNCTOR(var_poly,sig_poly,
								      MOD_STRUCTURE[SBND(it_lab,
											 BND_EXP(v2,exp))])
					val functor_sig = 
					    SIGNAT_FUNCTOR(var_poly,sig_poly,
							   SIGNAT_STRUCTURE[SDEC(it_lab,
										 DEC_EXP(v2,con))],
							   oneshot_init TOTAL)
				    in  (SBND(id,BND_MOD(v1,functor_mod)),
					 SDEC(id,DEC_MOD(v1,functor_sig)))
				    end)
		      end
		  val sbnds_sdecs = map2 modsig_helper (fun_ids,exp_con_list)
	    in
		map (fn (sbnd,sdec) => (SOME sbnd,CONTEXT_SDEC sdec)) sbnds_sdecs
	    end

	| Ast.SeqDec decs => packagedecs xdec' context decs
	| Ast.OpenDec pathlist => 
	      let fun help path = 
		  (case (Context_Lookup(context,map symbol_label path)) of
		       PHRASE_CLASS_MOD(m,s) => (m,s)
		     | _ => error "Can't open a non structure")
		  val modsig_list = map help pathlist
		  val temp = mapcount (fn (i,a) => (fresh_open_internal_label ("lbl" ^ (Int.toString i)),
						    fresh_var(),a)) modsig_list
		  fun help(l,v,(m,s)) = (SOME (SBND(l,BND_MOD(v,m))), CONTEXT_SDEC(SDEC(l,DEC_MOD(v,s))))
	      in map help temp
	      end
	| Ast.TypeDec tblist => xtybind(context,tblist) 
	| Ast.DatatypeDec {datatycs,withtycs} => 
	      let val sbnd_sdec = Datatype.compile{context=context,
						   typecompile=xty,
						   datatycs=datatycs,
						   withtycs=withtycs}
	      in map (fn (sbnd,sdec) => (SOME sbnd,CONTEXT_SDEC sdec)) sbnd_sdec
	      end
	| Ast.StrDec strblist => xstrbinds(context,strblist) 
 	| Ast.FctDec fctblist => xfctbind(context,fctblist) 

	| Ast.ExceptionDec [] => error "ExceptionDec []"
	| Ast.ExceptionDec [Ast.MarkEb (eb,r)] => xdec(context, Ast.ExceptionDec [eb])
	| Ast.ExceptionDec [Ast.EbGen {exn,etype}] =>
		let 
		  val id_bar = symbol_label exn
		  val var = fresh_var()
		  val mkvar = fresh_named_var "mk"
		  val exnmodvar = fresh_named_var "exnmod"
		  val v = fresh_var()
		  val con = (case etype of
			       NONE => con_unit
			     | SOME ty => xty(context,ty))
		  val varcon = CON_RECORD[(mk_lab,CON_ARROW(con,CON_ANY,oneshot_init TOTAL)),
					  (km_lab,CON_ARROW(CON_ANY,con,oneshot_init PARTIAL))]
		  val (mk_exp,mk_con) = 
		      (case etype of
			   NONE => (EXN_INJECT(VAR var,unit_exp), CON_ANY)
			 | SOME ty => (#1 (make_lambda(v,con,CON_ANY,
						       EXN_INJECT(VAR var,VAR v))),
				       CON_ARROW(con, CON_ANY, oneshot_init TOTAL)))
		  val inner_mod = MOD_STRUCTURE[SBND(it_lab, BND_EXP(var,NEW_STAMP con)),
						SBND(mk_lab, BND_EXP(mkvar,mk_exp))]
		  val inner_sig = SIGNAT_STRUCTURE
		      [SDEC(it_lab,DEC_EXP(var,CON_TAG con)),
		       SDEC(mk_lab,DEC_EXP(mkvar,mk_con))]
		in [(SOME(SBND(id_bar,BND_MOD(exnmodvar,inner_mod))),
		     CONTEXT_SDEC(SDEC(id_bar,DEC_MOD(exnmodvar,inner_sig))))]
		end
	| Ast.ExceptionDec [Ast.EbDef {exn: Symbol.symbol, edef: Ast.path}] => 
	      (case (Context_Lookup(context,map symbol_label edef)) of
		   PHRASE_CLASS_MOD(m,s) => 
		       let val id_bar = symbol_label exn
			   val path_mk_exp = MODULE_PROJECT(m,mk_lab)
			   val path_it_exp = MODULE_PROJECT(m,it_lab)
			   val decs = context2decs context
			   val path_mk_con = GetExpCon(decs,path_mk_exp)
			   val path_it_con = GetExpCon(decs,path_it_exp)
			   val itvar = fresh_var()
			   val mkvar = fresh_var()
			   val modvar = fresh_var()
			   val inner_mod = MOD_STRUCTURE[SBND(mk_lab, BND_EXP(mkvar,path_mk_exp)),
							 SBND(it_lab, BND_EXP(itvar,path_it_exp))]
			   val inner_sig = SIGNAT_STRUCTURE[SDEC(it_lab, DEC_EXP(itvar,path_mk_con)),
							    SDEC(mk_lab, DEC_EXP(mkvar,path_it_con))]
		       in [(SOME(SBND(id_bar,BND_MOD(modvar,inner_mod))),
			    CONTEXT_SDEC(SDEC(id_bar,DEC_MOD(modvar,inner_sig))))]
		       end
		 | _ => error "Rule 243 looup yielded non-module")
	| Ast.ExceptionDec eblist => xdec(context,Ast.SeqDec(map (fn eb => Ast.ExceptionDec [eb]) eblist))

        (* Rule 244 *)
	| Ast.LocalDec (dec1,dec2) => 
	      let val (var1,var2,var3) = (fresh_var(),fresh_var(),fresh_var())
		  val boolsbnd_ctxt_list1 = xdec'(context,dec1)
		  val (m1,s1) = boolsbnd_ctxt_list2modsig boolsbnd_ctxt_list1
		  val (lbl1,lbl2,lbl3) = (fresh_open_internal_label "lbl1",
					  fresh_open_internal_label "lbl2",
					  fresh_open_internal_label "lbl3")
		  val (_,context') = add_context_boolsbnd_ctxts(context,boolsbnd_ctxt_list1)
		  val boolsbnd_ctxt_list2 = xdec'(context',dec2)
		  val (m2,s2) = boolsbnd_ctxt_list2modsig boolsbnd_ctxt_list2
		  val final_sig = s2 (* XXX <--- *)
		  val final_mod = MOD_PROJECT(MOD_STRUCTURE[SBND(lbl1,BND_MOD(var1,m1)),
							    SBND(lbl2,BND_MOD(var2,m2))],
					      lbl2)
		  val context'' = add_context_module(context,fresh_internal_label "lbl",var1,s1)
	      in if (Sig_IsSub(context2decs context'',s2,final_sig))
		     then [(SOME(SBND(lbl3,BND_MOD(var3,final_mod))),
			    CONTEXT_SDEC(SDEC(lbl3,DEC_MOD(var3,final_sig))))]
		 else error "Sig_IsSub in rule 244 failed"
	      end

        (* Must augment translation context with fixity information *)
	| Ast.FixDec {fixity,ops} => let (* given symbol is in FixSymbol space *)
				       fun helper sym = 
					   let val sym' = Symbol.varSymbol(Symbol.name sym)
					   in (symbol_label sym', fixity)
					   end
				       val vf_list = map helper ops
				       val bnd = BND_FIXITY vf_list
				       val dec = DEC_FIXITY vf_list
				     in [(SOME(SBND(fresh_internal_label "lbl",bnd)),
					  CONTEXT_SDEC(SDEC(fresh_internal_label "lbl",dec)))]
				     end

	(* These cases are unhandled *)
	| Ast.SigDec [] => []
	| Ast.SigDec (sigb::rest) => let val ctxt : context_entry = xsigb(context,sigb)
					 val context' = add_context_entries(context,[ctxt])
					 val sbnd_ctxt_rest = xdec(context',Ast.SigDec rest)
				     in (NONE,ctxt)::sbnd_ctxt_rest
				     end
	| Ast.AbstypeDec {abstycs,withtycs,body} => error "abstype not handled"
	| Ast.AbsDec strblist => error "abstract structure not handled"
	| Ast.FsigDec fsiglist => error "funsig not handled"
	| Ast.OvldDec fctblist => error "overloading declaration not handled"
	| Ast.ImportDec strlist => error "ImportDec not handled"

        (* translate declaration by dropping region information *)
	| Ast.MarkDec (dec,region) => xdec(context,dec))

	    
    (* --------------------------------------------------------- 
      ------------------ TYPE EXPRESSIONS----------------------
      --------------------------------------------------------- *)
    and xty (context, ty) : con = 
      (case ty of
	 Ast.VarTy(Ast.Tyv sym) => (case (Context_Lookup(context,[symbol_label sym])) of
				      PHRASE_CLASS_CON (c,k) => c
				    | _ => error "tyvar lookup did not yield a PHRASE_CLASS_CON")
       | Ast.VarTy(Ast.MarkTyv (tv,r)) => xty(context,Ast.VarTy tv)
       | Ast.MarkTy (ty,r) => xty(context,ty)
       | Ast.TupleTy(tys) => let fun loop _ [] = []
				   | loop n (a::rest) = (generate_tuple_symbol n,a)::(loop (n+1) rest)
					  in xty(context, Ast.RecordTy(loop 1 tys))
			     end
       | Ast.RecordTy(sym_ty_list) => let val lab_ty_list = map (fn (s,t) => (symbol_label s,t)) sym_ty_list
					  val sorted_lab_ty_list = sort_labelpair lab_ty_list
					  fun doer (lab,ty) = (lab,xty(context,ty))
				      in CON_RECORD(map doer sorted_lab_ty_list)
				      end
       | Ast.ConTy (syms,ty_list) => 
	     let 
		 val _ = debugdo (fn () => (print "xty: Ast.Conty(["; 
					    pp_list AstHelp.pp_sym' syms ("","",".",false); print "])"))
		 val con_list = map (fn t => xty(context,t)) ty_list
		 fun general() = (case (con_list,Context_Lookup(context, map symbol_label syms)) of
				      ([],PHRASE_CLASS_CON(con,KIND_TUPLE 1)) => con
				    | (_,PHRASE_CLASS_CON(con,KIND_ARROW(n,1))) => 
					  if (n = length con_list) 
					      then CON_APP(con,(case con_list of
								    [c] => c
								  | _ => CON_TUPLE_INJECT(con_list)))
					  else error "constructor applied to wrong # of types"
				    | (_,PHRASE_CLASS_CON (c',k')) => (print "bad kindness: got args but k' =";
								       pp_kind k'; print "\nand c' = ";
								       pp_con c';
								       error "bad kindness")
				    | _ => error "xty found sym that looks up not to a convar")
	     in
		 (* XXX can they be overridden ? *)
		 case syms of
		     [sym] => (if (false andalso sym = (Symbol.tycSymbol "ref"))
				   then (case con_list of
					     [c] => CON_REF c 
					   | _ => error "ref expects one constructor")
			       else if (false andalso sym = (Symbol.tycSymbol "->"))
					then (case con_list of
						  [c1,c2] => CON_ARROW(c1,c2,oneshot_init PARTIAL)
						| _ => error "-> expects two constructors")
				    else general())
		   | _ => general()
	     end)

				  
    (* --------------------------------------------------------- 
      ------------------ TYPE DEFINITIONS ---------------------
      --------------------------------------------------------- *)
    and xtybind (context : context, tblist : Ast.tb list) : (sbnd option * context_entry) list =
       let 
	 fun doer tb = 
	   let 
	     val (tyc,tyvars,def) = tb_strip tb
	     val vars = map (fn _ => fresh_var()) tyvars
	     val tyvars_bar = map (fn s => symbol_label (tyvar_strip s)) tyvars
	     val context' = (foldl (fn ((v,tv),c) => 
				    add_context_convar(c,tv,v,KIND_TUPLE 1,NONE))
			     context (zip vars tyvars_bar))
	     val con' = xty(context',def)
	     val n = length tyvars
	     val (con,kind) = (case tyvars of
				 [] => (con',KIND_TUPLE 1)
			       | _ => (CON_FUN(vars,con'),KIND_ARROW(n,1)))
	     val var = fresh_var()
	     val tyc_bar = symbol_label tyc
	   in (SOME(SBND(tyc_bar,BND_CON(var,con))),
	       CONTEXT_SDEC(SDEC(tyc_bar,DEC_CON(var,kind,SOME con))))
	   end
       in map doer tblist
       end
     
    (* --------------------------------------------------------- 
      ------------------ TYPE DESCRIPTIONS ---------------------
      --------------------------------------------------------- *)
     and xtypedesc(context : context, []) : sdecs = []
       | xtypedesc(context : context, ((sym,tyvars : Ast.tyvar list, tyopt)::rest)) = 
       let 
	 val kind = (case tyvars of
		       [] => KIND_TUPLE 1
		     | _ => KIND_ARROW(length tyvars,1))
	 val conopt = (case tyopt of
			 NONE => NONE
		       | SOME ty => 
			   let 
			     val vars = map (fn _ => fresh_var()) tyvars
			     val tyvars_bar = map (fn s => symbol_label (tyvar_strip s)) tyvars
			     val context' = (foldl (fn ((v,tv),c) => 
						    add_context_convar(c,tv,v,KIND_TUPLE 1,NONE))
					     context (zip vars tyvars_bar))
			     val con' = xty(context',ty)
			   in SOME (case tyvars of
				      [] => con'
				    | _ => (CON_FUN(vars,con')))
			   end)
	 val sdec = SDEC(symbol_label sym, DEC_CON(fresh_var(),kind,conopt))
       in sdec :: (xtypedesc(context,rest))
       end

    (* --------------------------------------------------------- 
      ------------ Signature bindings and expressions ----------
      --------------------------------------------------------- *)
(* XXX AST does not support wheretypes *)
     and xsigexp(context,sigexp) : signat =
       (case sigexp of
	  Ast.VarSig s => (case (Context_Lookup(context,[symbol_label s])) of
					  PHRASE_CLASS_SIG(s) => s
					| _ => error "lookup of sigexp yielded wrong flavor")
	| Ast.SigSig speclist => SIGNAT_STRUCTURE(xspec(context,speclist))
	| Ast.MarkSig (s,r) => xsigexp(context,s))

     and xsigb(context,Ast.MarkSigb(sigb,_)) : context_entry = xsigb(context,sigb)
       | xsigb(context,Ast.Sigb{name,def}) = CONTEXT_SIGNAT(symbol_label name,
							    gen_var_from_symbol name,
							    xsigexp(context,def))

    (* --------------------------------------------------------- 
      ------------------ SIGNATURE SPECIFICTIONS --------------
      --------------------------------------------------------- *)
     and xspec(context, specs : Ast.spec list) : sdecs = 
       let 
	 fun xspec1 (Ast.ValSpec vtlist) = (* Rules 257 - 258 *)
	       let 
		 fun doer (sym,ty) = 
		   (case (free_tyvar_ty(ty,fn _ => false)) of
		      [] => SDEC(symbol_label sym, 
				 DEC_EXP(fresh_var(),xty(context,ty)))
		    | ftv_sym => 
			let 
			    val varpoly = fresh_named_var "var_poly"
			    fun help tv_sym = SDEC(symbol_label tv_sym,
						   DEC_CON(fresh_var(),KIND_TUPLE 1, NONE))
			    val sigpoly = SIGNAT_STRUCTURE(map help ftv_sym)
			    val context' = add_context_module(context,
							      fresh_open_internal_label "lbl",
							      varpoly, sigpoly)
			    val con = xty(context',ty)
			    val fsig = SIGNAT_FUNCTOR(varpoly,sigpoly,
						      SIGNAT_STRUCTURE[SDEC(it_lab,
									    DEC_EXP(fresh_var(),con))],
						      oneshot_init TOTAL)
			in SDEC(symbol_label sym, DEC_MOD(fresh_named_var "unused",fsig))
			end)
	       in map doer vtlist 
	       end
	   | xspec1 (Ast.StrSpec (symsigexp_list)) =
	       let fun doer(sym,sigexp) = let val s = xsigexp(context,sigexp)
					  in SDEC(symbol_label sym,DEC_MOD(fresh_var(),s))
					  end
	       in map doer symsigexp_list
	       end
	   | xspec1 (Ast.IncludeSpec sym) = 
	       (case (xsigexp(context,Ast.VarSig sym)) of
		  SIGNAT_STRUCTURE sdecs => sdecs
		| _ => error "Rule 265 led to signature of a non-structure")
	   | xspec1 (Ast.FctSpec sym_fsigexp_list) = 
		  let 
		    fun doer (funid,fsig) = 
		      let 
			val var = fresh_var()
			fun help (Ast.VarFsig _) = error "Ast.VarFsig encountered"
			  | help (Ast.MarkFsig (fs,_)) = help fs
			  | help (Ast.FsigFsig {param,def=sigexp'}) = 
			     let (* this is a derived form *)
			       val strid = functor_arg_lab
			       val sym_sigexp_list = (map (fn (SOME s,se) => (s,se)
			                                    | (NONE, _) => error "functor arg unnamed")
						      param)
			       val sigexp = Ast.SigSig[Ast.StrSpec sym_sigexp_list]
			       val signat = xsigexp(context,sigexp)
			       val context' = add_context_module(context,strid,var,signat)
			       val signat' = xsigexp(context',sigexp')
			     in SIGNAT_FUNCTOR(var,signat,signat',oneshot_init PARTIAL)
			     end
		      in SDEC(symbol_label funid, DEC_MOD(var,help fsig))
		      end
		  in map doer sym_fsigexp_list
		  end
	   (* Rule 259 *)
	   | xspec1 (Ast.TycSpec (typdesc_list,_)) = xtypedesc(context,typdesc_list)
	   | xspec1 (Ast.ExceSpec exlist) = (* Rules 260 - 261 *)
		      let fun doer (sym,tyopt) = 
			let val (mk_con,it_con) = 
			  (case tyopt of
			     NONE => (CON_ANY, CON_TAG con_unit)
			   | (SOME ty) => let val con = xty(context,ty)
					  in (CON_ARROW(con,CON_ANY,oneshot_init TOTAL),
					      CON_TAG con)
					  end)
			    val inner_sig = 
			      SIGNAT_STRUCTURE[SDEC(it_lab,
						    DEC_EXP(fresh_var(),it_con)),
					       SDEC(mk_lab,
						    DEC_EXP(fresh_var(),mk_con))]
			in SDEC(symbol_label sym, DEC_MOD(fresh_var(),inner_sig))
			end
		      in map doer exlist
		      end
	   | xspec1 (Ast.DataSpec {datatycs=datatycs, withtycs=withtycs}) =
	     let val sbnd_sdecs = Datatype.compile{context=context,
						   typecompile=xty,
						   datatycs=datatycs,
						   withtycs=withtycs}
	     in map #2 sbnd_sdecs
	     end
	   | xspec1 (Ast.ShatycSpec _) = error "Ast.ShatySpec(267): type sharing spec not implemented"
	   | xspec1 (Ast.FixSpec _) = error "Ast.FixitySpec not implemented"
	   | xspec1 (Ast.ShareSpec _) = error "Ast.ShareSpec (structure sharing) not implemented"
	   | xspec1 (Ast.LocalSpec _) = error "Ast.LocalSpec not implemented"
	   | xspec1 (Ast.OpenSpec _) = error "Ast.OpenSpec not implemented"
	   | xspec1 (Ast.MarkSpec (s,r)) = xspec1 s
       in (case specs of
	     [] => []
	   | (spec::specrest) => let val sdecs = xspec1 spec
				     val context' = add_context_sdecs(context,sdecs)
				     val sdecsrest = xspec(context',specrest)
				 in sdecs @ sdecsrest
				 end)
       end


    (* --------------------------------------------------------- 
      ------------------ FUNCTOR BINDINDS ---------------------
      --------------------------------------------------------- *)
     and xfctbind (context : context, fctbs : Ast.fctb list) : (sbnd option * context_entry) list =
	 let 
	     fun help (context,(name,def)) : ((bool * sbnd) option * context_entry) list = 
		 (case def of
		      (Ast.VarFct _) => raise UNIMP
		    | (Ast.FctFct {params=[(argnameopt,sigexp)],body,constraint}) =>
			  let 
			      val arglabel = (case argnameopt of
						  NONE => fresh_open_internal_label "FunctorArg"
						| SOME s => openlabel(symbol_label s))
			      val funid = symbol_label name
			      val argvar = fresh_var()
			      val signat = xsigexp(context,sigexp)
			      val context' = add_context_module(context,arglabel,argvar,signat)
			      val (m',s') = xstrexp(context',body,constraint)
			      val v = fresh_var()
			      val sbnd = SBND(funid,BND_MOD(v,MOD_FUNCTOR(argvar,signat,m')))
			      val sdec = SDEC(funid,DEC_MOD(v,SIGNAT_FUNCTOR(argvar,signat,s',
									 oneshot_init PARTIAL)))
			  in [(SOME(false,sbnd), CONTEXT_SDEC sdec)]
			  end
		    | (Ast.FctFct _) => raise UNIMP
		    | (Ast.LetFct _) => raise UNIMP
		    | (Ast.AppFct _) => raise UNIMP
		    | (Ast.MarkFct (f,r)) => help (context,(name,f)))
	 in  packagedecs help context (map fctb_strip fctbs)
	 end

    (* --------------------------------------------------------- 
      ------------------ STRUCTURE EXPRESSION -----------------
      --------------------------------------------------------- *)
     and selfrule(modv : mod, s : signat) =
	 let fun help (SDEC(l,DEC_CON(v,k,NONE))) = SDEC(l,DEC_CON(v,k,SOME(CON_MODULE_PROJECT(modv,l))))
	       | help (sdec as (SDEC(l,DEC_CON(v,k,SOME c)))) = sdec
	       | help sdec = sdec
	 in  (case s of
		  SIGNAT_STRUCTURE sdecs => SIGNAT_STRUCTURE (map help sdecs)
		| _ => s)
	 end
     and xstrexp (context : context, strb : Ast.strexp, constr : Ast.sigexp option) : (mod * signat) = 
       (case constr of
	  NONE => (case strb of
		     Ast.VarStr path => (* remember to use self-rule to maximize sharing *)
			 (case modsig_lookup(context,map symbol_label path) of
			      SOME (path,m,s) => (m,selfrule(path2mod path,s))
			    | NONE => error "module projection failed in lookup")
		   | Ast.AppStr (path,strexpbool_list) => 
			 (case strexpbool_list of
			    [] => error "AppStr with []"
			  | [(strexp,_)] => 
				(case (Context_Lookup(context,map symbol_label path)) of
				 PHRASE_CLASS_MOD(m,s) => 
				   (case s of
				      (SIGNAT_FUNCTOR(var1,sig1,sig2,_)) => 
					let val (module,signat) = xstrexp(context,strexp,NONE)
					    val (modc,sig1') = 
						(case (xcoerce(context2decs context,signat,sig1)) of
						     (m,SIGNAT_FUNCTOR(v,s1,s2,_)) => (m,least_super_sig(v,s1,s2))
						   | _ => error "xcoerce did not return a functor")
					    val sig2' = least_super_sig(var1,sig1',sig2)
					  val fsig = SIGNAT_FUNCTOR(var1,sig1,sig2,oneshot_init PARTIAL)
					  val fsig' = SIGNAT_FUNCTOR(var1,sig1',sig2',oneshot_init PARTIAL)
					in if (true orelse Sig_IsSub(context2decs context,fsig,fsig'))
					     then (MOD_APP(m,MOD_APP(modc,module)),sig2')
					   else (print "sig_issub failed: want fsig < fsig'\n  fsig = ";
						 pp_signat fsig; print "\n  fsig' = ";
						 pp_signat fsig';
						 error "rule 251-3 failed")
					end
				    | _ => error "rule 251-2 failed")
			       | PHRASE_CLASS_EXP (e,c) => (print "phrase_class_exp\n";error "rule 251-1 failed")
			       | PHRASE_CLASS_CON  (c,k) =>  (print "phrase_class_con\n";error "rule 251-1 failed")
			       | PHRASE_CLASS_SIG  s =>  (print "phrase_class_sig\n";error "rule 251-1 failed")
			       | PHRASE_CLASS_OVEREXP _ =>  (print "phrase_class_overexp\n";error "rule 251-1 failed"))
			  | _ => error "AppStr with list of length > 1")
		   | Ast.LetStr (dec,strexp) => (* rule 254 *) 
			    let val (var1,var2) = (fresh_var(), fresh_var())
			      val (lbl1,lbl2) = (fresh_open_internal_label "lbl1",fresh_open_internal_label "lbl2")
			      val boolsbnd_sdec_list = xdec'(context,dec)
			      val (mod1,sig1) = boolsbnd_ctxt_list2modsig boolsbnd_sdec_list
			      val context' = add_context_module(context,lbl1,var1,sig1) (* <-- inline ? *)
			      val (mod2,sig2) = xstrexp(context',strexp,NONE)
			      val sig2' = sig2
			      val final_mod = MOD_PROJECT(MOD_STRUCTURE [SBND(lbl1,BND_MOD(var1,mod1)),
									 SBND(lbl2,BND_MOD(var2,mod2))],
							  lbl2)
			      val final_sig = SIGNAT_STRUCTURE [SDEC(lbl2,DEC_MOD(var2,sig2'))]
			    in if (Sig_IsSub(context2decs context',sig2,sig2'))
				   then (final_mod, final_sig)
			       else error "Rule 254 failed"
			    end
		   | Ast.StructStr dec => 
			 let 
			     val sbnd_ctxt_list = xdec(context,dec)
			     val sbnds = List.mapPartial #1 sbnd_ctxt_list
			     val sdecs = List.mapPartial (fn (_,CONTEXT_SDEC sdec) => SOME sdec
							   | _ => NONE) sbnd_ctxt_list
			 in (MOD_STRUCTURE sbnds, SIGNAT_STRUCTURE sdecs)
			 end
		   | Ast.MarkStr (strexp,r) => xstrexp(context,strexp,NONE))


	| SOME sigexp => (* Rule 252 *)
	    let val (module,signat) = xstrexp(context,strb,NONE)
	      val sig' = xsigexp(context,sigexp)
	      val (mod',signat'') = xcoerce(context2decs context,signat,sig')
	    in 
	      (MOD_APP(mod',module),sig')
	    end)

    (* --------------------------------------------------------- 
      ------------------ STRUCTURE BINDINGS --------------------
      --------------------------------------------------------- *)
     and xstrbinds (context : context, strbs : Ast.strb list) : (sbnd option * context_entry) list =
       let val strbs = map strb_strip strbs
	   fun help (n,(d,c)) = let val (m,s) = xstrexp(context,d,c)
				    val v = fresh_var()
				    val l = symbol_label n
				in  (SOME(SBND(l,BND_MOD(v,m))),
				     CONTEXT_SDEC(SDEC(l,DEC_MOD(v,s))))
				end
       in map help strbs
       end

    (* --------------------------------------------------------- 
      ------------------ SIGNATURE PATCHING -------------------
      --------------------------------------------------------- *)
    and xsig_wheretype(signat : signat, lbls : label list, con : con, kind : kind) : signat = 
      let 
	val fv = con_free_convar con
	fun bound v = if (member_eq(eq_var,v,fv)) 
			then error "xsig_wheretype: FV(con) ^ BV(sdecs) != nil"
		      else ()
	fun docon curl sdecs =
	  (case sdecs of
	     [] => error "xsig_wheretype: docon got empty lbls"
	   | (sdec::rest) => 
	       (case sdec of
		  SDEC(l,DEC_CON(v,k,NONE)) => (bound v; 
						if eq_label(l,curl)
						  then if (k = kind) 
							 then (SDEC(l,DEC_CON(v,k,SOME con)))::rest
						       else error "xsig_wheretype kind mismatch"
						else sdec::(docon curl rest))
		| SDEC(l,(DEC_EXP(v,_) | DEC_MOD(v,_) | DEC_CON(v,_,_))) => (bound v; sdec::(docon curl rest))
		| SDEC(l,(DEC_FIXITY _ | DEC_EXCEPTION _)) => sdec::(docon curl rest)))
	fun dosig [] sdecs = error "xsig_wheretype got empty lbls"
	  | dosig [l] sdecs = docon l sdecs
	  | dosig (curl::restl) sdecs = 
	      let 
		fun loop [] = error "xsig_where could not find sig"
		  | loop (sdec::rest) = 
		  (case sdec of
		     (SDEC(l,DEC_MOD(v,SIGNAT_STRUCTURE sdecs))) => 
		                  (bound v; if eq_label(l, curl) 
					      then (SDEC(l,DEC_MOD(v,SIGNAT_STRUCTURE
								   (dosig restl sdecs))))::rest
					    else sdec::(loop rest))
		    | SDEC(l,(DEC_EXP(v,_) | DEC_MOD(v,_) | DEC_CON(v,_,_))) => (bound v; sdec::(loop rest))
		    | SDEC(l,(DEC_FIXITY _ | DEC_EXCEPTION _)) => sdec::(loop rest))
	      in loop sdecs
	      end		
      in (case signat of
	    SIGNAT_FUNCTOR _ => error "xsig_wheretype for fuctor sig"
	  | (SIGNAT_STRUCTURE sdecs) => SIGNAT_STRUCTURE(dosig lbls sdecs))
      end


  and xsig_sharing(signat : signat, lbls1 : label list, lbls2 : label list, kind : kind) : signat = 
      let
	fun lookup (cl,sdecs,t) = 
	  let fun loop [] acc = error "xsig_sharing: could not find label"
		| loop (SDEC(l,dec)::rest) acc = 
	              if eq_label(l,cl) then let val (dec',rest') = t(dec,rest)
				       in (rev acc) @ ((SDEC(l,dec')) :: rest')
				       end
		      else loop rest (SDEC(l,dec)::acc)
	  in loop sdecs []
	  end
        val sdecs = (case signat of
		       SIGNAT_FUNCTOR _ => error "xsig_sharing got a functor sig"
		     | SIGNAT_STRUCTURE sd => sd)
	val sdecs' = 
	  (case (lbls1,lbls2) of
	     ([lbl],[lbl']) => 
	       lookup (lbl',sdecs, fn (dec', sdecs_rest) =>
		       (dec',lookup (lbl, sdecs_rest, fn (dec, sdecs'') =>
				     (case (dec',dec) of
					(DEC_CON(v',k',NONE),DEC_CON(v,k,NONE)) =>
					  if (k = k') 
					    then (DEC_CON(v,k,SOME (CON_VAR v')),sdecs'')
					  else error "kind mismatch in xsig_sharing"
				      | _ => error "lookup did not get CON in xsig_sharing"))))
	   | (lbl::lbls,[lbl']) =>
	       lookup (lbl', sdecs, fn (dec',sdecs_rest) =>
		       (dec',lookup(lbl, sdecs_rest, fn (dec, sdecs'') =>
				    (case (dec',dec) of
				       (DEC_CON(v',k,NONE),DEC_MOD(v,s)) =>
					 if (k = kind) 
					   then let val sig' = xsig_wheretype(s,lbls,CON_VAR v',k)
						in (DEC_MOD(v,sig'), sdecs'')
						end
					 else error "xsig_sharing: kind mismatch"
				     | _ => error "lookup did not get CON in xsig_sharing"))))
	   | (lbl::lbls,lbl'::lbls') =>
	       if eq_label(lbl,lbl') 
		 then lookup (lbl, sdecs, fn(dec,sdecs') =>
			      (case dec of
				 DEC_MOD(v,sig') =>
				   let val sig'' = xsig_sharing(sig',lbls,lbls',kind)
				   in (DEC_MOD(v,sig''), sdecs')
				   end
			       | _ => error "xsig_sharing: can't share these decs"))
	       else lookup (lbl',sdecs,fn(dec',sdecs_rest) =>
			    (dec', lookup (lbl, sdecs_rest, fn (dec,sdecs'') =>
					   (case (dec',dec) of
					      (DEC_MOD(v',s'),DEC_MOD(v,s'')) =>
						let val p = COMPOUND_PATH(v',lbls')
						  val s''' = xsig_wheretype(s'',lbls', path2con p, kind)
						in  (DEC_MOD(v,s'''), sdecs'')
						end
					    | _ => error "xsig_sharing: can't share these decs"))))
           | _ => error "xsig_sharing: can't share this shape of labels")
      in SIGNAT_STRUCTURE sdecs'
      end

    (* --------------------------------------------------------- 
      ------------------ COERCION COMPILATION-------------------
      --------------------------------------------------------- *)
(* XXX CHECK *)
    and Signat_Lookup(decs,(path,SIGNAT_STRUCTURE sdecs),labels) = 
	Sdecs_Lookup(decs,(path2mod path,sdecs),labels)
      | Signat_Lookup _ = error "Signat_Lookup called with a non-structure sig"

    and xcoerce (orig_decs : decs,
		 signat0 : signat,
		 signat : signat) : Il.mod * Il.signat = 
      let 
	  val _ =  debugdo (fn () => (print "trying to xcoerce with signat0 = \n";
				      pp_signat signat0; print "\nand signat = \n";
				      pp_signat signat; print "\nand decs = \n";
				      pp_decs orig_decs; print "\n"))
	val v0 = fresh_var()
	val orig_decs = (DEC_MOD(v0,signat0))::orig_decs
(* Rules 249 and 250 *)
	fun polyval_case decs (lbl,v,con : con,varsig_option) = 
	     (case (Signat_Lookup(decs,(SIMPLE_PATH v0,signat0),[lbl])) of
		(lbls,CLASS_MOD s) => 
		  let val (lbls,s) = (case s of
					SIGNAT_FUNCTOR _ => (lbls,s)
					| (SIGNAT_STRUCTURE[SDEC(mk_lab,
								 DEC_MOD(v,s)),_]) => (lbls @ [mk_lab], s)
							       | _ => error "no way to coerce")
		    val path = COMPOUND_PATH(v0,lbls)
		  in (case s of
		     SIGNAT_FUNCTOR(var_poly,sig_poly as SIGNAT_STRUCTURE sig_poly_sdecs,
				    SIGNAT_STRUCTURE[SDEC(maybe_it_lab,con'')],_) =>
			let 
			    val itsig = SIGNAT_STRUCTURE[SDEC(it_lab,DEC_EXP(fresh_var(),con))]
			    val mtemp = MOD_APP(path2mod path,MOD_VAR var_poly)
			    val (bnd,dec,almost_sig_poly') = 
				(case varsig_option of
				     NONE => (BND_EXP(v,MODULE_PROJECT(mtemp,it_lab)),
					      DEC_EXP(v,con),
					      poly_inst(decs,sig_poly_sdecs))
				   | SOME (v1,s1) => (BND_MOD(v,MOD_FUNCTOR(v1,s1,mtemp)),
						      DEC_MOD(v,SIGNAT_FUNCTOR(v1,s1,itsig,oneshot())),
						      poly_inst(DEC_MOD(v1,s1)::decs,sig_poly_sdecs)))
			    val sig_poly' = SIGNAT_STRUCTURE(#2 almost_sig_poly')
			    val decs' = dec::decs
			    val s'' = SIGNAT_FUNCTOR(fresh_var(),sig_poly',itsig,oneshot())
			    val _ = if (Sig_IsSub(decs',s,s''))
				      then ()
				    else (print "varsig_option is:\n";
					  (case varsig_option of
					       NONE => print "NONE\n"
					     | SOME(v,s) => pp_signat s);
					  print "\n";
					  print "s is: \n";
					  pp_signat s;
					  print "s'' is: \n";
					  pp_signat s'';
					  print "decs' is: \n";
					  pp_decs decs';
					  error "cannot coerce : 1")
			in (bnd,dec)
			end
		   | _ => error "cannot coerce : 2")
		  end
	      | _ => error "cannot coerce : 3")
		    
	fun doit decs (lbl,dec) : (bnd * dec) = 
	  (case dec of
	     DEC_EXP(v,c) =>
	       (case (Signat_Lookup(decs,(SIMPLE_PATH v0,signat0),[lbl])) of
		    (lbls,CLASS_EXP con) => (con_unify(decs,"coerce structure components",
						       ("target type",c),
						       ("actual type",con),nada);
					     (BND_EXP(v,path2exp(COMPOUND_PATH(v0,lbls))),
					      DEC_EXP(v,con)))
		| (lbls,CLASS_MOD s) => polyval_case decs (lbl,v,c,NONE)  (* rule 250 *)
		| _ => error "cannot coerce : 4")
	    | DEC_MOD(v,SIGNAT_FUNCTOR(v1,s1,                        (* rule 248 *)
			      SIGNAT_STRUCTURE[SDEC(_,DEC_EXP(_,c))],_)) => polyval_case decs (lbl,v,c,SOME(v1,s1))
	    | DEC_MOD(v,s) => (* rule 254 *)
		  let fun general_mod() = 
		      (case (Signat_Lookup(decs,(SIMPLE_PATH v0,signat0),[lbl])) of
			   (lbls,CLASS_MOD s1) => let val (m,s'') = xcoerce(decs,s1,s) (* signat *)
						      val v1 = fresh_var()
						      val m' = path2mod(COMPOUND_PATH(v0,lbls))
						  in (BND_MOD(v,MOD_FUNCTOR(v1,s1,
									    MOD_APP(m,m'))),
						      DEC_MOD(v,s''))
						  end
			 | _ => error "cannot coerce : 5")
		  in case s of
			SIGNAT_FUNCTOR(v1,s1,SIGNAT_STRUCTURE sdecs, _) =>
			    (case sdecs of
				 [SDEC(maybe_it,DEC_EXP(_,c))] => if (eq_label(maybe_it,it_lab))
								      then polyval_case decs (lbl,v,c,SOME(v1,s1))
								  else general_mod()
			       | _ => general_mod())
		      | _ => general_mod()
		  end
	    | DEC_CON(v,k,copt) => (* rules 251 XXX should do 252-253 *) 
	     (case (Signat_Lookup(decs,(SIMPLE_PATH v0,signat0),[lbl])) of
		(lbls,CLASS_CON knd) => 
		  let val con' = path2con(COMPOUND_PATH(v0,lbls))
		    val _ = (case copt of 
				 NONE => ()
			       | SOME con => con_unify(decs,"xcoerce_eqcon",
						       ("target type",con),
						       ("actual type",con'),nada))
		  in (BND_CON(v,con'),DEC_CON(v,k,SOME con'))
		  end
	      | _ => error "cannot coerce : 6")
	    | _ => error "cannot coerce : 7")
		  
	fun loop decs [] : (sbnds * sdecs) = ([],[])
	  | loop decs ((SDEC(l,dec))::rest) = let val (bnd,dec) = doit decs (l,dec)
						  val (sbnds,sdecs) = loop (dec::decs) rest
					      in ((SBND(l,bnd))::sbnds, 
						  (SDEC(l,dec))::sdecs)
					      end
	val (m,s) = (case (signat0,signat) of
		       (SIGNAT_FUNCTOR(v1,s1,s1',a1), SIGNAT_FUNCTOR(v2,s2,s2',a2)) =>
			 let 
			   val _ = comp_unify(a1,a2)
			   val (m3,s3) = xcoerce(orig_decs,s2,s1)
			   val (m4,s4) = xcoerce(orig_decs,s1',s2')
			   val modexp = MOD_APP(m4,MOD_APP(MOD_VAR v0, MOD_APP(m3, MOD_VAR v2)))
			   val decs' = (DEC_MOD(v2,s2))::orig_decs
			   val s = GetModSig(decs',modexp)
			 in (MOD_FUNCTOR(v2,s2,modexp),
			     SIGNAT_FUNCTOR(v2,s2,s,a1))
			 end
		     | (_,SIGNAT_STRUCTURE sdecs) => let 
							 val (sbnds,sdecs) = loop orig_decs sdecs
						     in (MOD_STRUCTURE sbnds,
							 SIGNAT_STRUCTURE sdecs)
						     end
		     | _ => error "xcoerce got bad sigs")
      in (MOD_FUNCTOR(v0,signat0,m),
	  SIGNAT_FUNCTOR(v0,signat0,s,oneshot_init TOTAL))
      end

    fun xeq (decs : decs, con : con) : exp = 
      let
	  val _ = (print "CALLED xeq with con = ";
	      pp_con con; print "\nand decs = \n";
	      pp_decs decs)
	fun self c = xeq(decs,c)
	open Prim
      in (case con_normalize(decs,con) of
	    CON_TYVAR tyvar => (case (tyvar_deref tyvar) of
				NONE => error "resolved type does not permit equailty"
			      | SOME c => self c)
	  | CON_VAR _ => error "cannot compile equality on CON_VAR _"
	  | CON_OVAR ocon => self (CON_TYVAR (ocon_deref ocon))
	  | CON_INT is => PRIM(eq_int is,[])
	  | CON_UINT is => ILPRIM(eq_uint is)
	  | CON_FLOAT fs => PRIM(eq_float fs,[])
	  | CON_RECORD fields => let 
				   val v = fresh_var()
				   val v1 = fresh_var()
				   val v2 = fresh_var()
				   val paircon = con_tuple[con,con]
				   val e1 = RECORD_PROJECT(VAR v,generate_tuple_label 1,paircon)
				   val e2 = RECORD_PROJECT(VAR v,generate_tuple_label 2,paircon)
				   fun help (lbl,con) = 
				     let 
				       val eqexp = self con
				       val e1 = RECORD_PROJECT(VAR v1,lbl,con)
				       val e2 = RECORD_PROJECT(VAR v2,lbl,con)
				     in APP(eqexp,exp_tuple[e1,e2])
				     end
				   fun folder (rdec,exp) = 
				     let val exp' = help rdec
				     in make_ifthenelse(exp,exp',false_exp)
				     end
				   val body = (case fields of
						 [] => true_exp
					       | (fst::rest) => foldl folder (help fst) rest)
				 in #1(make_lambda(v,paircon,con_bool,
						   make_let([(v1,e1),(v2,e2)],body)))
				 end
	  | CON_SUM (iopt,conlist) => let 
				 val v = fresh_var()
				 val v1 = fresh_var()
				 val v2 = fresh_var()
				 val paircon = con_tuple[con,con]
				 val e1 = RECORD_PROJECT(VAR v,generate_tuple_label 1,paircon)
				 val e2 = RECORD_PROJECT(VAR v,generate_tuple_label 2,paircon)
				 fun help(i,cs) = let val eqexp = self cs
						      val var' = fresh_var()
						      val var'' = fresh_var()
						      val sumc = CON_SUM(SOME i,conlist)
						      val armbody = APP(eqexp,
									exp_tuple[SUM_TAIL(sumc,VAR var'),
										  SUM_TAIL(sumc,VAR var'')])
						      val arms2 = map0count 
							  (fn j =>
							   if (i=j) 
							       then SOME(#1(make_lambda(var'',
											sumc,
											con_bool,
											armbody)))
							   else NONE) (length conlist)
						  in SOME (#1(make_lambda(var', CON_SUM(SOME i,conlist),
									  con_bool,
									  CASE(conlist,VAR v2,arms2,NONE))))
						  end
				 val arms1 = mapcount help conlist
				 val inner_body = CASE(conlist,VAR v1,arms1,NONE)
				 val body = make_catch(inner_body,con_bool,false_exp)
			       in #1(make_lambda(v,paircon,con_bool,
						 make_let([(v1,e1),(v2,e2)],body)))
			       end
	  | CON_ARRAY c => PRIM(array_eq,[c])
	  | CON_VECTOR c => APP(PRIM(vector_eq,[c]),xeq(decs,c))
	  | CON_REF c => PRIM(eq_ref,[c])
	  | CON_MODULE_PROJECT(m,l) => MODULE_PROJECT(m,to_eq_lab l)
	  | CON_MUPROJECT (j,con') => 
		(case GetConKind(decs,con') of
		     KIND_TUPLE _ => error "cannot perform equality on con tuples"
		   | KIND_ARROW(n,m) => 
			 let
			     val _= if (m=n) then () else error "datatype constructor must have kind n=>n"
			     val mu_cons = map0count (fn i => CON_MUPROJECT(i,con')) n
			     val type_lbls = map0count (fn i => fresh_internal_label("lbl" ^ (Int.toString i))) n
			     val eq_lbls = map (fn l => internal_label ((label2string l) ^ "_eq")) type_lbls
			     val var_poly = fresh_named_var "var_poly"
			     val vars_eq = map0count (fn i => fresh_named_var ("vars_eq_" ^ (Int.toString i))) n
			     local
				 val temp = map (fn tlab => (tlab,true)) type_lbls
				 val poly_sdecs = make_typearg_sdec temp
			     in
				 val extradec = DEC_MOD(var_poly,SIGNAT_STRUCTURE poly_sdecs)
			     end
			     val decs = extradec :: decs

			     local
				 val temp = map (fn tlab => CON_MODULE_PROJECT(MOD_VAR var_poly,tlab)) type_lbls
				 val applied = ConApply(con',(case temp of
								  [c] => c
								| _ => CON_TUPLE_INJECT temp))
			     in
				 val reduced_cons = (case applied of
							 CON_TUPLE_INJECT conlist => conlist
						       | c => [c])
			     end
			     val exps_v = map (fn c => xeq(decs,c)) reduced_cons
			     local
				 val eq_table = zip eq_lbls (map VAR vars_eq)
				 val type_table = zip type_lbls mu_cons
			     in 
				 fun eproj_handler (MOD_VAR sv,eq_lab) = 
				     if (eq_var(sv,var_poly))
					 then assoc_eq(eq_label,eq_lab,eq_table)
				     else NONE
				   | eproj_handler _ = NONE
				 fun cproj_handler (MOD_VAR sv,type_lab) = 
				     if (eq_var(sv,var_poly))
					 then assoc_eq(eq_label,type_lab,type_table)
				     else NONE
				   | cproj_handler _ = NONE
			     end
			     fun make_expeq (expv) = 
				 let
				     val var = fresh_named_var "arg_pair"
				     val expv' = exp_subst_proj(expv,eproj_handler,cproj_handler)
				     val e1 = UNROLL(con',
						     MODULE_PROJECT(MOD_VAR var,generate_tuple_label 1))
				     val e2 = UNROLL(con',
						     MODULE_PROJECT(MOD_VAR var,generate_tuple_label 2))
				 in (var,APP(expv',exp_tuple[e1,e2]))
				 end
			     val exps_eq = map make_expeq exps_v
			     val fbnds = map2 (fn (vareq,(vararg,expeq)) =>
					       FBND(vareq,vararg,con_tuple[con,con],con_bool,expeq))
				 (vars_eq,exps_eq)
			 in FIX(fbnds,List.nth(vars_eq,j))
			 end)
	  | CON_TAG _ => error "cannot perform equality on tag type"
	  | CON_ANY => error "cannot perform equality on exception type"
	  | CON_ARROW _ => error "cannot perform equality on functions"
	  | _ => (print "no equality at this type:\n";
		  pp_con con;
		  error "no equality at this type"))
      end

    (* ------------ Exported interface to resolve overloading ------------ *)
    fun overload_wrap xobj arg = 
      let 
	val _ = clear_tables()
	val res = xobj arg
	val overload_table = get_overload_table()
	val eq_table = get_eq_table()
	val flex_table = get_flex_table()
	fun flex_help (l,ref(FLEXINFO(stamp,false,rdecs)),fieldc,eshot) = 
	    (print "Unresolved flex record with label: ";
	     pp_label l; print "\nrdecs are:";
	     pp_con (CON_RECORD rdecs); print "\nfieldc is:";
	     pp_con fieldc; print "\n";
	     ())
(* error "Unresolved flex record" *)
	  | flex_help (l,ref(FLEXINFO(_,true,rdecs)),fieldc,eshot) = 
	    let val v = fresh_named_var "flex_eta_var"
		val recc = CON_RECORD rdecs
		val body = RECORD_PROJECT(VAR v,l,recc)
		val (e,c) = make_lambda(v,fieldc,recc,body)
	    in oneshot_set(eshot,e)
	    end
	  | flex_help (l,ref(INDIRECT_FLEXINFO fr),fieldc,eshot) = flex_help (l,fr,fieldc,eshot)
	fun overload_help ocon =
		 let val helpers = {hard = fn (c1,c2) => eq_con([],c1,c2),
				    soft = fn (c1,c2) => soft_eq_con([],c1,c2)}
		 in (case (ocon_constrain(ocon,helpers)) of
			   [] => error "overloaded type: none of the constraints are satisfied"
			 | [pos] => ()
			 | pos::_ => error "Warning: more than one constraint satisfied by overloaded type")
		 end
	fun eq_help (tyvar,exp_oneshot) = 
	  let
	      val con = CON_TYVAR tyvar
	      val decs = tyvar_getdecs tyvar
	      val eq_exp = xeq(decs,con)
	      val _ = oneshot_set(exp_oneshot,eq_exp)
	  in ()
	  end
        val _ = app overload_help overload_table 
	val _ = app eq_help eq_table 
	val _ = app flex_help flex_table 
      in res
      end
    
    val xdec = overload_wrap xdec
    val xexp = overload_wrap xexp

  end;
