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
	     sharing Datatype = Basis.Datatype
	     sharing Ppil.Formatter = AstHelp.Formatter)
   : TOIL =  
  struct

    structure Il = Il
    open AstHelp Il IlStatic IlUtil Ppil Basis Pat
    open Util Name IlLookup Tyvar
    open Prim

    val error = error "toil.sml"
    val debug = ref false
    fun debugdo t = if (!debug) then (t(); ()) else ()

    (*  ------------------ OVERLOAD ENTRIES ---------------------
      --------------------------------------------------------- *)
    local 
      val overload_table = ref ([] : (con Tyvar.ocon * exp Util.oneshot * (int list -> exp)) list)
      val eq_table = ref ([] : (decs * con * exp Util.oneshot) list)
    in
      fun clear_overload_table () = overload_table := []
      fun get_overload_table () = !overload_table
      fun add_overload_entry (ocon,expos,expmaker) = (overload_table := 
						      (ocon,expos,expmaker)::(!overload_table))
      fun clear_eq_table () = eq_table := []
      fun get_eq_table () = !eq_table
      fun add_eq_entry (decs,con,expos) = (eq_table := (decs,con,expos)::(!eq_table))
    end


    (* ----- Polymorphic (sdec) Instantiation ------------------------ *)
    (* given a list of sdecs, return a triple consisting of
      (1) sbnds with the fresh types
      (2) the same sdecs except replace each opaque type with a fresh type
      (3) the fresh types *)
     fun poly_inst (decs,sdecs) = 
       let
	 fun help (SDEC(l,DEC_CON(v,k,NONE))) = let val c = fresh_con() 
						   in (SBND(l,BND_CON(v,c)),
						       SDEC(l,DEC_CON(v,k,SOME c)),
						       c)
						   end
	   | help _ = error "poly_inst received strange sdec"
	 val temp = map help sdecs
       in (map #1 temp, map #2 temp, map #3 temp)
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
	      val _ = (print "polyfun_inst' got module of:\n";
		       Ppil.pp_mod module;
		       print "\n\n  amd signmature of:\n";
		       Ppil.pp_signat s;
		       print "\n\n")
	    local 
	      fun help (SDEC(l,dec)) =
		let 
		  fun dotype l v = let 
				     val tv = fresh_tyvar "tv"
				     val c = CON_TYVAR tv
				 in (tv,(SBND(l,BND_CON(v,c)),
					 SDEC(l,DEC_CON(v,KIND_TUPLE 1, SOME c)),
					 DEC_CON(tyvar_getvar tv,KIND_TUPLE 1,NONE)))
				 end
		in
		  (case dec of
		       (DEC_CON(v,_,_)) => #2(dotype l v)
		     | (DEC_MOD(v,SIGNAT_STRUCTURE[SDEC(l1,DEC_CON(v1,_,_)),
						   SDEC(l2,DEC_EXP(v2,c2))])) => 
		       let 
			 val (tyvar,(sbnd1,sdec1,dec1)) = dotype l1 v1
			 val _ = tyvar_use_equal tyvar
			 val con = CON_TYVAR tyvar
			 val eq_con = CON_ARROW(con_tuple[con,con],con_bool,oneshot_init PARTIAL)
			 val exp_os = oneshot()
			 val _ = add_eq_entry(context2decs context,con,exp_os)
			 val eqexp = OVEREXP(eq_con,true,exp_os)
			 val sbnd2 = SBND(l2,BND_EXP(v2,eqexp))
			 val sdec2 = SDEC(l2,DEC_EXP(v2,c2))
		       in (SBND(l,BND_MOD(v,MOD_STRUCTURE[sbnd1,sbnd2])),
			   SDEC(l,DEC_MOD(v,SIGNAT_STRUCTURE[sdec1,sdec2])),
			   dec1)
		       end
		     | _ => error "unexpected sig to arg struct of polymorphic fun")
		end
	      val temp = map help arg_sdecs
	    in 
	      val decs = context2decs context
	      val mod_poly = MOD_STRUCTURE(map #1 temp)
	      val signat_poly_sdecs = map #2 temp
	      val signat_poly = SIGNAT_STRUCTURE signat_poly_sdecs
	      val decs = (map #3 temp) @ decs
	    end
	    val _ = (print "resc is:  "; pp_con resc; print "\n")
	    val _ = (print "var is:  "; pp_var v; print "\n")
	    val _ = (print "signat_poly is:\n"; pp_signat signat_poly; print "\n")
	    val new_rescon : con = make_non_dependent_type(resc,v,signat_poly_sdecs)


	    val signat_temp = SIGNAT_STRUCTURE[SDEC
					       (it_lab,
						DEC_EXP(resv,new_rescon))]
	    val _ = (print "\n*********\ndone make_non_dependent_type with signat_temp:\n";
		     pp_signat signat_temp; print "\n\n")
	    val signat'= SIGNAT_FUNCTOR(v,signat_poly,signat_temp,oneshot_init PARTIAL)
	    val _ = if (Sig_IsSub(decs,s,signat')) then ()
		    else (print "s is\n";
			  pp_signat s; print "\nsignat' is \n";
			  pp_signat signat'; print "\n";
			  s1 := s; s2 := signat'; decs1 := decs;
			  error "Failed rule 24 Sig_IsSub")
	    val _ = if (Module_IsValuable(decs, mod_poly)) then ()
		    else (print "mod_poly not valuable:\n";
			  pp_mod mod_poly;
			  print "\n";
			  error "Failed rule 24 valuability")
	    val exp = MODULE_PROJECT(MOD_APP(module,mod_poly), it_lab)
	    val _ = (print "polyfun_inst returning exp :\n";
		     pp_exp exp; print "\n\n  and rescon:\n";
		     pp_con new_rescon; print "\n\n")
	  in (exp,new_rescon)
	  end
       |  _ =>  (print "polyfun_inst received s of\n";
		 pp_signat s;
		 print "\n";
		 error "rule 224 or 226 not applicable in polyfun_inst"))


    (* --------------------------------------------------------- 
      ------------------ EXPRESSIONS --------------------------
      --------------------------------------------------------- *)
	  
     fun xexp (context : context, exp : Ast.exp) : (exp * con) = 
      (case exp of
	 Ast.IntExp is =>   (SCON(INT(IntStr2word is)), CON_INT)
       | Ast.WordExp ws =>  (SCON(INT(WordStr2word ws)), CON_INT)
       | Ast.RealExp s =>   (SCON(FLOAT s), CON_FLOAT)
       | Ast.StringExp s => (SCON(STRING s), CON_VECTOR CON_CHAR)
       | Ast.CharExp s =>   (SCON(CHAR(case (explode s) of
					 [c] => c
				       | _ => error "Ast.CharExp carries a string of length != 1")), 
			     CON_CHAR)
       | Ast.TupleExp (exp_list) => let val recexp = Ast.RecordExp(mapcount (fn(n,a) => 
									     (generate_tuple_symbol n,a))
								   exp_list)
				    in xexp(context, recexp)
				    end
(* XXX not sorted? *)
       | Ast.RecordExp (sym_exp_list) => (let fun doer(s,expr) = let val l = symbol2label s
								     val (exp,con) = xexp(context,expr)
								 in (RBND(l,exp),RDEC(l,con))
								 end
					      val bnd_dec_list = map doer sym_exp_list
					  in (RECORD(map #1 bnd_dec_list), CON_RECORD(map #2 bnd_dec_list))
					  end)
       | Ast.ListExp args => let fun loop [] = AstHelp.nil_exp
				   | loop (a::b) = Ast.AppExp{function=AstHelp.cons_exp,
							      argument=Ast.TupleExp[a,loop b]}
			     in xexp(context,loop args)
			     end
       (* XXX SelectorExp must be handled like overloading... *)
       | Ast.SelectorExp s => (let val var = fresh_var()
				   val (con1,con2) = (fresh_con(),fresh_con())
				   val decs = context2decs context
			       in raise UNIMP
	 (* make_lambda(var,con1,con2,RECORD_PROJECT(VAR var,symbol2label s)) *)
			       end)
       | Ast.VarExp path => 
(if (path = [Symbol.varSymbol "CREDIT"])
then (print "got CREDIT: context is:";
      pp_context context;
      print "\n\n")
else ();
	   if (path = [Symbol.varSymbol "="]) 
	     then let val exp_os = oneshot()
		      val tyvar = fresh_tyvar "teq"
		      val _ = tyvar_use_equal tyvar
		      val con = CON_TYVAR tyvar
		      val eq_con = CON_ARROW(con_tuple[con,con],con_bool,oneshot_init PARTIAL)
		      val _ = add_eq_entry(context2decs context,con,exp_os)
		  in (OVEREXP(eq_con,true,exp_os),eq_con)
		  end
	   else 
	     (case (Context_Lookup(context,map symbol2label path)) of
	       PHRASE_CLASS_EXP ec => ec
	     | PHRASE_CLASS_OVEREXP thunk =>
			(* XXX need to insert in table to resolve later *)
			let 
			  val decs = context2decs context
			  val (exp_maker,ocon) = thunk()
			  val expos = oneshot()
			  val _ = add_overload_entry(ocon,expos,exp_maker)
			in (OVEREXP(CON_OVAR ocon,false,expos),
			    CON_OVAR ocon)
			end
	     | PHRASE_CLASS_MOD (m,s as SIGNAT_FUNCTOR _) => polyfun_inst (context,m,s)
	     | PHRASE_CLASS_MOD (m,s) =>
		 let 
		   val path = mod2path m
		   val decs = context2decs context
		   val path_mk = join_path_labels(path,[mk_lab])
		   val path_mk_exp = path2exp path_mk
		   val path_mk_mod = path2mod path_mk
		 in (path_mk_exp,GetExpCon(decs,path_mk_exp))
		   handle (FAILURE _) => (* Rule 226 *)
		     let val signat_mk = GetModSig(decs,path_mk_mod)
		     in  polyfun_inst(context,path2mod path_mk,signat_mk)
		     end
		 end
	     | (PHRASE_CLASS_CON _) => error "Ast.VarExp path leads to CON"
	     | (PHRASE_CLASS_SIG _) => error "Ast.VarExp path leads to SIG")
)
       | Ast.LetExp {dec,expr} => let val (inflag,module,signat) = xdec'(context, dec)
				      val var = fresh_var()
				      val lab = fresh_open_label()
				      val context' = if inflag then add_context_inline(context,lab,
									       INLINE_MODSIG(module,signat))
						     else add_context_module(context,lab,var,signat)
				      val (e,c) = xexp(context',expr)
				  in  (LET([BND_MOD(var,module)],e),c)
				  end
       | Ast.FlatAppExp fexps => let val _ = debugdo (fn () => (print "FlatAppExp... context is:\n";
								pp_context context;
								print "\n"))
				     val exps = map (fn {item,...} => item) fexps
				 in xexp(context,InfixParse.parse_exp(Context_Get_FixityTable context, exps))
				 end
       | Ast.AppExp {argument,function} => let val (e1',con1) = xexp(context,function)
					       val (e2',con2) = xexp(context,argument)
					       val spec_rescon = fresh_con()
					       val spec_funcon = CON_ARROW(con2,spec_rescon, oneshot())
					       val _ =  con_unify'(context,"",
								   con1,"function type", fn () => pp_exp e1',
								   spec_funcon,"argument -> ? type", 
								   fn () => pp_exp e2')
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
						 in con_unify(context,"Contrained expression",
							      con,"expressionn type",
							      con',"constraint type");
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
			   val _ = con_unify(context,"handle",
					     rescon,"handle body type",
					     #2 handler,"handler type")
		       in (HANDLE(exp',#1 handler),rescon)
		       end
       | Ast.RaiseExp e => let val (exp,con) = xexp(context,e)
			       val _ = con_unify(context,"raise",CON_ANY,"ANY",con,"con")
			   in (RAISE exp,fresh_con())
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
     and make_typearg_sdec(l,useeq) = 
		    let 
		      val var = fresh_var()
		      val sdec = SDEC(l,DEC_CON(var,KIND_TUPLE 1,NONE))
		    in if useeq
			 then let val convar = CON_VAR var
				  val arrowcon = CON_ARROW(con_tuple[convar,convar],
							   con_bool, oneshot_init PARTIAL)
			      in SDEC(openlabel l,
				      DEC_MOD(fresh_var(),
					      SIGNAT_STRUCTURE[sdec, 
							       SDEC(eq_lab,DEC_EXP(fresh_var(),arrowcon))]))
			      end
		       else sdec
		    end
     and xdec'  (context : context, d : Ast.dec) : (bool * mod * signat) =
       let 
	 val _ = print "-----> xdec' called:";
	 val (m,s) = xdec(context,d)
       in (case d of
	     Ast.MarkDec(d,r) => xdec'(context,d)
	   | Ast.DatatypeDec {datatycs,withtycs} => (print " got a datatype"; (false,m,s))
	   | _ => (print "\n\n"; (false,m,s)))
       end


     and xdec (context : context, d : Ast.dec) : (mod * signat) =
       (case d of
	  Ast.ValDec vblist => (* Rules 236 - 237 *)
	    let local
		  val pe_list = map vb_strip vblist
		in
		  val (pat,expr) = (case pe_list of
				      [] => error "let with nothing"
				    | [(p,e)] => (p,e)
				    | _ => (Ast.TuplePat(map #1 pe_list),
					    Ast.TupleExp(map #2 pe_list)))
		end
		val tyvars = free_tyvar_exp(expr, Context_Get_ScopedConvars context)
	    in
	      case tyvars of
		[] => (* Rule 236 *)
		  let val (e,c) = xexp(context,expr)
		    val patarg = {context = context, 
				  typecompile = xty, 
				  expcompile = xexp, 
				  polyinst = poly_inst}
		  in  bindCompile{patarg = patarg,
				  bindpat = pat,
				  arg = (e,c)}
		  end
	      | _ => (* Rules 237 *)
		let val context' = add_context_scoped_tyvars(context,tyvars)
		  val lbl = fresh_open_label()
		  val lbl' = fresh_int_label()
		  val var = fresh_var()
		  val var' = fresh_var()
		  val var_poly = fresh_var()
		  val sdecs1 = map (fn sym => SDEC(symbol2label sym,
						 DEC_CON(fresh_var(),KIND_TUPLE 1,NONE))) tyvars
		  val (e,con) = xexp(add_context_module(context',lbl,var_poly,SIGNAT_STRUCTURE sdecs1),expr)
		  val lbls'_useeq = rebind_free_type_var(con,context,var_poly)
		  val sdecs2 = map make_typearg_sdec lbls'_useeq
		  val sig_poly = SIGNAT_STRUCTURE (sdecs1 @ sdecs2)
		  val context'' = add_context_module(context',lbl,var_poly,sig_poly)
		  val patarg = {context = context'', 
				typecompile = xty, 
				expcompile = xexp, 
				polyinst = poly_inst}
		  val (module,signat) = bindCompile{patarg = patarg,
						    bindpat = pat,
						    arg = (e,con)}
		in 
		  case signat of 
		    SIGNAT_STRUCTURE sdecs => 
		      let val labs = map (fn SDEC (l,_) => l) sdecs
			val cons = map (fn SDEC(l,DEC_EXP(_,c)) => c | _ => error "Rule 237") sdecs
			fun mod_help (l,(l',_)) = 
			   let val temp =  MOD_STRUCTURE[SBND(it_lab,
					     BND_MOD(fresh_var(),
						     MOD_PROJECT(MOD_APP(MOD_VAR var',MOD_VAR var),l')))]
			   in SBND(l,BND_MOD(fresh_var(),MOD_FUNCTOR(var,sig_poly,temp)))
			   end
			fun sig_help (l,c) = 
			  SDEC(l,DEC_MOD(fresh_var(),
			       SIGNAT_FUNCTOR(var,sig_poly,
					      SIGNAT_STRUCTURE[SDEC(it_lab,
								    DEC_EXP(fresh_var(),c))],oneshot_init TOTAL)))
			val final_mod = MOD_STRUCTURE((SBND(lbl',BND_MOD(var',
									 MOD_FUNCTOR(var,sig_poly,module))))::
						      (map2 mod_help (labs,lbls'_useeq)))
			val final_sig = SIGNAT_STRUCTURE
			  ((SDEC(lbl',DEC_MOD(var',SIGNAT_FUNCTOR(var,sig_poly,signat,oneshot_init TOTAL)))) ::
			   (map2 sig_help (labs, cons)))
		      in (final_mod,final_sig)
		      end
		| _ => error "Rule 237: sig is not SIGNAT_STRUCTURE"
		end
	    end
	| Ast.ValrecDec rvblist => error "Encountered a valrec, just use fun ... and ..."
	| Ast.FunDec fblist => (* Rule 238 *)
	    let 
	      local
		fun help (Ast.Clause{pats,resultty,exp}) =
		  let 
		    fun getitem ({item,...} : 'a Ast.fixitem) = item
		    val (s,restpats) = 
		      (case pats of
			 ({item=Ast.VarPat[s],...}::rest) => (s, map getitem rest)
		       | [p1,{item=Ast.VarPat[s],...},p2] => (s, [Ast.TuplePat[getitem p1,
									       getitem p2]])
		       | _ => error "rule 238: can't find funid")
		  in (symbol2label s,(restpats,resultty,exp))
		  end
		val temp = map (fn fb => map help (fb_strip fb)) fblist
		fun getid [] = error "no ids"
		  | getid [a] = a
		  | getid (a::(rest as b::_)) = if eq_label(a,b) then getid rest 
						else error "not all ids are same"
		val tempids = mapmap #1 temp
	      in 
		val ids = map getid tempids
		val triples_listlist = mapmap #2 temp
	      end
	      val labs = map (fn _ => fresh_int_label()) ids
	      val vars' = map (fn _ => fresh_var()) ids
	      val cons = map (fn _ => fresh_con()) ids
	      val cons' = map (fn _ => fresh_con()) ids
	      val tyvars = let val scoped = Context_Get_ScopedConvars context
			   in flatten (flatten (mapmap (fn (_,_,exp) =>
							free_tyvar_exp(exp,scoped))
						triples_listlist))
			   end
	      val var_poly = fresh_var()
	      val lbl = fresh_open_label()
	      val sdecs1 = map (fn sym => SDEC(symbol2label sym,
					     DEC_CON(fresh_var(),KIND_TUPLE 1,NONE))) tyvars
	      val context' = foldl 
		(fn ((id,var',con,con'),c) => add_context_var(c,id,var',CON_ARROW(con,con',oneshot_init PARTIAL)))
		(add_context_module(context,lbl,var_poly,SIGNAT_STRUCTURE sdecs1))
		(zip4 ids vars' cons cons')
	      val fbnd_con_list = 
		(map4 (fn (rule_triples,argcon,rescon,var') => 
		       let 
			 val arms = map (fn (pats,_,e) => (pats,e)) rule_triples
			 val patarg = {context = context', 
				       typecompile = xty, 
				       expcompile = xexp, 
				       polyinst = poly_inst}
			 val {body = (ue,uc), arg = (var1,con1)} = funCompile{patarg = patarg,
									      rules = arms,
									      reraise = false}
			 val _ = con_unify(context',"fundef",
					   argcon,"argcon", con1,"con1")
			 val _ = con_unify(context',"fundef",
					   rescon,"rescon",uc,"uc")
		       in (FBND(var',var1,con1,uc,ue),
			   CON_ARROW(con1,uc,oneshot_init PARTIAL))
		       end)
		(triples_listlist, cons, cons',vars'))
	      val fbnds = map #1 fbnd_con_list
	      val exp_con_list = map2 (fn (v',c) => (FIX(fbnds,v'),c)) (vars',map #2 fbnd_con_list)
	      val lbls'_useeq = flatten(map (fn (_,c) => rebind_free_type_var(c,context,var_poly)) 
					exp_con_list)
	      val sdecs2 = map make_typearg_sdec lbls'_useeq
	      local
		val sdecs = sdecs1 @ sdecs2
		val sig_poly = case sdecs of [] => NONE | _ => SOME(SIGNAT_STRUCTURE sdecs)
	      in
		fun mod_helper (id,exp) = 
		  (case sig_poly of
		     SOME sig_poly => SBND(id,BND_MOD(fresh_var(),
						      MOD_FUNCTOR(var_poly,sig_poly,
								  MOD_STRUCTURE[SBND(it_lab,
										     BND_EXP(fresh_var(),exp))])))
		   | NONE => SBND(id,BND_EXP(fresh_var(),exp)))
		fun sig_helper (id,con) = 
		  (case sig_poly of
		     SOME sig_poly => SDEC(id,DEC_MOD(fresh_var(),
							 SIGNAT_FUNCTOR(var_poly,sig_poly,
									SIGNAT_STRUCTURE[SDEC(it_lab,
											      DEC_EXP(fresh_var(),con))],
									oneshot_init TOTAL)))
		   | NONE => SDEC(id,DEC_EXP(fresh_var(),con)))
	      end
	    in
	      (MOD_STRUCTURE(map2 mod_helper (ids,map #1 exp_con_list)),
	       SIGNAT_STRUCTURE(map2 sig_helper (ids,map #2 exp_con_list)))
	    end
	(* Rule 239 *)
	| Ast.SeqDec [] => (MOD_STRUCTURE[],SIGNAT_STRUCTURE[])
	| Ast.SeqDec [dec] => xdec(context,dec)
	| Ast.SeqDec (dec::decrest) => 
	    let 
	      val (inflag,m1,s1) = xdec'(context,dec)
	      val (var1,var2) = (fresh_var(),fresh_var())
	      val (lbl1,lbl2) = (fresh_open_label(),fresh_open_label())
	      val inline1 = INLINE_MODSIG(m1,s1)
	      val context' = if inflag then add_context_inline(context,lbl1,inline1)
			     else add_context_module(context,lbl1,var1,s1)
	      val (m2,s2) = xdec(context',Ast.SeqDec decrest)
	      fun package (sbnds,sdecs) = (MOD_STRUCTURE((SBND(lbl1,BND_MOD(var1,m1)))::sbnds),
					   SIGNAT_STRUCTURE((SDEC(lbl1,DEC_MOD(var1,s1)))::sdecs))
	      fun general() = package([SBND(lbl2,BND_MOD(var2,m2))],[SDEC(lbl2,DEC_MOD(var2,s2))])
	    in
	      (case (m2,s2) of
		 (MOD_STRUCTURE sbnds,
		  SIGNAT_STRUCTURE sdecs) => let fun loop [] = package(sbnds,sdecs)
						   | loop (SBND(l,_)::rest) = if (is_label_open l) 
										then loop rest
									      else general()
					     in loop sbnds
					     end
	       | _ => general())
	    end
	(* Rule 240 *)
	| Ast.OpenDec pathlist => let fun help path = 
				       (case (Context_Lookup(context,map symbol2label path)) of
					  PHRASE_CLASS_MOD(m,s) => (m,s)
					| _ => error "Can't open a non structure")
				      val modsig_list = map help pathlist
				      val temp = map (fn a => (fresh_open_label(),fresh_var(),a)) modsig_list
				      val newmod = map (fn (l,v,(m,s)) => SBND(l,BND_MOD(v,m))) temp
				      val newsig = map (fn (l,v,(m,s)) => SDEC(l,DEC_MOD(v,s))) temp
				  in (MOD_STRUCTURE newmod, SIGNAT_STRUCTURE newsig)
				  end
        (* Rule 245 - 248 *)
	| Ast.TypeDec tblist => xtybind(context,tblist)
	| Ast.DatatypeDec {datatycs,withtycs} => Datatype.compile{context=context,
								  typecompile=xty,
								  datatycs=datatycs,
								  withtycs=withtycs}
	| Ast.StrDec strblist => xstrbinds(context,strblist)
	| Ast.FctDec fctblist => xfctbind(context,fctblist)

        (* Rules 241 - 243 *)
	| Ast.ExceptionDec [] => error "ExceptionDec []"
	| Ast.ExceptionDec [Ast.MarkEb (eb,r)] => xdec(context, Ast.ExceptionDec [eb])
	| Ast.ExceptionDec [Ast.EbGen {exn,etype}] =>
		let 
		  val id_bar = symbol2label exn
		  val var = fresh_var()
		  val v = fresh_var()
		  val con = (case etype of
			       NONE => con_unit
			     | SOME ty => xty(context,ty))
		  val varcon = CON_RECORD[RDEC(mk_lab,CON_ARROW(con,CON_ANY,oneshot_init TOTAL)),
					  RDEC(km_lab,CON_ARROW(CON_ANY,con,oneshot_init PARTIAL))]
		  val (mk_exp,mk_con) = 
		      (case etype of
			   NONE => (EXN_INJECT(VAR var,unit_exp), CON_ANY)
			 | SOME ty => (#1 (make_lambda(v,con,CON_ANY,
						       EXN_INJECT(VAR var,VAR v))),
				       CON_ARROW(con, CON_ANY, oneshot_init TOTAL)))
		  val inner_mod = MOD_STRUCTURE[SBND(it_lab, BND_EXP(var,NEW_STAMP con)),
						SBND(mk_lab, BND_EXP(fresh_var(),mk_exp))]
		  val inner_sig = SIGNAT_STRUCTURE
		      [SDEC(it_lab,DEC_EXP(var,CON_TAG con)),
		       SDEC(mk_lab,DEC_EXP(fresh_var(),mk_con))]
		in (MOD_STRUCTURE[SBND(id_bar,BND_MOD(fresh_var(),inner_mod))],
		    SIGNAT_STRUCTURE[SDEC(id_bar,DEC_MOD(fresh_var(),inner_sig))])
		end
	| Ast.ExceptionDec [Ast.EbDef {exn: Symbol.symbol, edef: Ast.path}] => 
				  (case (Context_Lookup(context,map symbol2label edef)) of
				     PHRASE_CLASS_MOD(m,s) => 
				       let val id_bar = symbol2label exn
					 val path_mk_exp = MODULE_PROJECT(m,mk_lab)
					 val path_it_exp = MODULE_PROJECT(m,it_lab)
					 val decs = context2decs context
					 val path_mk_con = GetExpCon(decs,path_mk_exp)
					 val path_it_con = GetExpCon(decs,path_it_exp)
					 val inner_mod = MOD_STRUCTURE[SBND(mk_lab,
									    BND_EXP(fresh_var(),path_mk_exp)),
								       SBND(it_lab,
									    BND_EXP(fresh_var(),path_it_exp))]
					 val inner_sig = SIGNAT_STRUCTURE[SDEC(it_lab,
									       DEC_EXP(fresh_var(),path_mk_con)),
									  SDEC(mk_lab,
									       DEC_EXP(fresh_var(),path_it_con))]
				       in
					 (MOD_STRUCTURE[SBND(id_bar,BND_MOD(fresh_var(),inner_mod))],
					  SIGNAT_STRUCTURE[SDEC(id_bar,DEC_MOD(fresh_var(),inner_sig))])
				       end
				   | _ => error "Rule 243 looup yielded non-module")
	| Ast.ExceptionDec eblist => xdec(context,Ast.SeqDec(map (fn eb => Ast.ExceptionDec [eb]) eblist))

        (* Rule 244 *)
	| Ast.LocalDec (dec1,dec2) => let val (var1,var2) = (fresh_var(),fresh_var())
					  val (inflag,m1,s1) = xdec'(context,dec1)
					  val (lbl1,lbl2) = (fresh_open_label(),fresh_int_label())
					  val context' = if inflag 
							   then add_context_inline(context,lbl1,
										   INLINE_MODSIG(m1,s1))
							 else add_context_module(context,lbl1,var1,s1)
					  val (m2,s2) = xdec(context',dec2)
					  val s = s2
					  val final_mod = MOD_STRUCTURE[SBND(lbl1,BND_MOD(var1,m1)),
									SBND(lbl2,BND_MOD(var2,m2))]
					  val context'' = add_context_module(context,fresh_int_label(),var1,s1)
				      in if (Sig_IsSub(context2decs context'',s2,s))
					then (MOD_PROJECT(final_mod,lbl2),s)
					 else error "Sig_IsSub in rule 244 failed"
				      end
					    
        (* Must augment translation context with fixity information *)
	| Ast.FixDec {fixity,ops} => let 
				       fun helper sym = (symbol2label sym, fixity)
				       val vf_list = map helper ops
				       val bnd = BND_FIXITY vf_list
				       val dec = DEC_FIXITY vf_list
				     in (MOD_STRUCTURE[SBND(fresh_int_label(),bnd)],
					 SIGNAT_STRUCTURE[SDEC(fresh_int_label(),dec)])
				     end

	(* These cases are unhandled *)
	| Ast.SigDec fctblist => error "Signature permitted only at top level"
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
	 Ast.VarTy(Ast.Tyv sym) => (case (Context_Lookup(context,[symbol2label sym])) of
				      PHRASE_CLASS_CON (c,k) => c
				    | _ => error "tyvar lookup did not yield a PHRASE_CLASS_CON")
       | Ast.VarTy(Ast.MarkTyv (tv,r)) => xty(context,Ast.VarTy tv)
       | Ast.MarkTy (ty,r) => xty(context,ty)
       | Ast.TupleTy(tys) => let fun loop _ [] = []
				   | loop n (a::rest) = (generate_tuple_symbol n,a)::(loop (n+1) rest)
					  in xty(context, Ast.RecordTy(loop 1 tys))
			     end
       | Ast.RecordTy(sym_ty_list) => let val lab_ty_list = map (fn (s,t) => (symbol2label s,t)) sym_ty_list
					  val sorted_lab_ty_list = sort_labelpair lab_ty_list
					  fun doer (lab,ty) = RDEC(lab,xty(context,ty))
				      in CON_RECORD(map doer sorted_lab_ty_list)
				      end
       | Ast.ConTy ([sym],ty_list) => 
	     let 
	       val _ = debugdo (fn () => (print "xty: Ast.Conty(["; AstHelp.pp_sym sym; print "])"))
	       val table' = [("int", CON_INT), 
			     ("uint", CON_UINT),
			     ("real", CON_FLOAT), 
			     ("char", CON_CHAR), 
			     ("string", CON_VECTOR CON_CHAR),
			     ("unit", con_unit)]
	       val table = map (fn (s,c) => (Symbol.tycSymbol s, c)) table'
	     in (case assoc(sym, table) of
		   SOME v => v
		 | NONE => 
		     let 
		       val con_list = map (fn t => xty(context,t)) ty_list
		     in
		       case (sym = (Symbol.tycSymbol "ref"), con_list) of
			 (true,[c]) => CON_REF c
		       | _ => (case (con_list,Context_Lookup(context,[symbol2label sym])) of
				 ([],PHRASE_CLASS_CON(con,KIND_TUPLE 1)) => con
			       | (_,PHRASE_CLASS_CON(con,KIND_ARROW(n,1))) => 
				    if (n = length con_list) 
				      then CON_APP(con,CON_TUPLE_INJECT(con_list))
				    else error "constructor applied to wrong # of types"
			       | (_,PHRASE_CLASS_CON _) => error "bad kindness"
			       | _ => error "xty found sym that looks up not to a convar")
		     end)
	     end
       | Ast.ConTy ([],ty_list) => error "Ast.ConTy has no symbols"
       | Ast.ConTy (_,ty_list) => error "Ast.ConTy has multiple symbols")

				  
    (* --------------------------------------------------------- 
      ------------------ TYPE DEFINITIONS ---------------------
      --------------------------------------------------------- *)
    and xtybind (context : context, tblist : Ast.tb list) : (mod * signat) = 
       let 
	 fun doer tb = 
	   let 
	     val (tyc,tyvars,def) = tb_strip tb
	     val vars = map (fn _ => fresh_var()) tyvars
	     val tyvars_bar = map (fn s => symbol2label (tyvar_strip s)) tyvars
	     val context' = (foldl (fn ((v,tv),c) => 
				    add_context_convar(c,tv,v,KIND_TUPLE 1,NONE))
			     context (zip vars tyvars_bar))
	     val con' = xty(context',def)
	     val n = length tyvars
	     val (con,kind) = (case tyvars of
				 [] => (con',KIND_TUPLE 1)
			       | _ => (CON_FUN(vars,con'),KIND_ARROW(n,1)))
	     val var = fresh_var()
	     val tyc_bar = symbol2label tyc
	   in (SBND(tyc_bar,BND_CON(var,con)),
	       SDEC(tyc_bar,DEC_CON(var,kind,SOME con)))
	   end
	 val temp = map doer tblist
       in (MOD_STRUCTURE(map #1 temp), SIGNAT_STRUCTURE(map #2 temp))
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
			     val tyvars_bar = map (fn s => symbol2label (tyvar_strip s)) tyvars
			     val context' = (foldl (fn ((v,tv),c) => 
						    add_context_convar(c,tv,v,KIND_TUPLE 1,NONE))
					     context (zip vars tyvars_bar))
			     val con' = xty(context',ty)
			   in SOME (case tyvars of
				      [] => con'
				    | _ => (CON_FUN(vars,con')))
			   end)
	 val sdec = SDEC(symbol2label sym, DEC_CON(fresh_var(),kind,conopt))
       in sdec :: (xtypedesc(context,rest))
       end

    (* --------------------------------------------------------- 
      ------------------ Signature EXPRESSIONS ----------------
      --------------------------------------------------------- *)
(* XXX AST does not support wheretypes *)
     and xsigexp(context,sigexp) : signat =
       (case sigexp of
	  Ast.VarSig s => (case (Context_Lookup(context,[symbol2label s])) of
					  PHRASE_CLASS_SIG(s) => s
					| _ => error "lookup of sigexp yielded wrong flavor")
	| Ast.SigSig speclist => SIGNAT_STRUCTURE(xspec(context,speclist))
	| Ast.MarkSig (s,r) => xsigexp(context,s))


    (* --------------------------------------------------------- 
      ------------------ SIGNATURE SPECIFICTIONS --------------
      --------------------------------------------------------- *)
     and xspec(context, specs : Ast.spec list) : sdecs = 
       let 
	 fun xspec1 (Ast.ValSpec vtlist) = (* Rules 257 - 258 *)
	       let 
		 fun doer (sym,ty) = 
		   (case (free_tyvar_ty(ty,[])) of
		      [] => SDEC(symbol2label sym, 
				 DEC_EXP(fresh_var(),xty(context,ty)))
		    | ftv_sym => 
			let val var = fresh_var()
			  fun help tv_sym = SDEC(symbol2label tv_sym,
						 DEC_CON(var,KIND_TUPLE 1, NONE))
			  val sigpoly = SIGNAT_STRUCTURE(map help ftv_sym)
			  val context' = add_context_module(context,
							    fresh_open_label(),
							    fresh_var(), sigpoly)
			  val con = xty(context',ty)
			  val fsig = SIGNAT_FUNCTOR(var,sigpoly,SIGNAT_STRUCTURE[SDEC(it_lab,
										      DEC_EXP(fresh_var(),con))],
						    oneshot_init TOTAL)
			in SDEC(symbol2label sym, DEC_MOD(fresh_var(),fsig))
			end)
	       in map doer vtlist 
	       end
	   | xspec1 (Ast.StrSpec (symsigexp_list)) =
	       let fun doer(sym,sigexp) = let val s = xsigexp(context,sigexp)
					  in SDEC(symbol2label sym,DEC_MOD(fresh_var(),s))
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
		      in SDEC(symbol2label funid, DEC_MOD(var,help fsig))
		      end
		  in map doer sym_fsigexp_list
		  end
	   (* Rule 259 *)
	   | xspec1 (Ast.TycSpec (typdesc_list,_)) = xtypedesc(context,typdesc_list)
	   | xspec1 (Ast.ExceSpec exlist) = (* Rules 260 - 261 *)
		      let fun doer (sym,tyopt) = 
			let val (mk_con,km_con) = 
			  (case tyopt of
			     NONE => (CON_ANY,
				      CON_ARROW(CON_ANY,con_unit,oneshot_init PARTIAL))
			   | (SOME ty) => let val con = xty(context,ty)
					  in (CON_ARROW(con,CON_ANY,oneshot_init TOTAL),
					      CON_ARROW(CON_ANY,con,oneshot_init PARTIAL))
					  end)
			    val inner_sig = 
			      SIGNAT_STRUCTURE[SDEC(mk_lab,
						    DEC_EXP(fresh_var(),mk_con)),
					       SDEC(km_lab,
						    DEC_EXP(fresh_var(),km_con))]
			in SDEC(symbol2label sym, DEC_MOD(fresh_var(),inner_sig))
			end
		      in map doer exlist
		      end
	   | xspec1 (Ast.DataSpec {datatycs=datatycs, withtycs=withtycs}) =
		      let val (module,signat) = Datatype.compile{context=context,
								 typecompile=xty,
								 datatycs=datatycs,
								 withtycs=withtycs}
		      in (case signat of
			    SIGNAT_STRUCTURE sdecs => sdecs
			  | _ => error "datatype sig not a SIGNAT_STRUCTURE")
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
     and xfctbind (context : context, fctbs : Ast.fctb list) : (mod * signat) = 
       let fun help (name,def) = 
	     (case def of
		(Ast.VarFct _) => raise UNIMP
	      | (Ast.FctFct {params=[(NONE,sigexp)],body,constraint}) =>
		  let 
		    val funid = symbol2label name
		    val argvar = fresh_var()
		    val signat = xsigexp(context,sigexp)
		    val context' = add_context_module(context,funid,argvar,signat)
		    val (m',s') = xstrexp(context',body,constraint)
		    val v = fresh_var()
		    val sbnd = (funid,BND_MOD(v,MOD_FUNCTOR(argvar,signat,m')))
		    val sdec = (funid,DEC_MOD(v,SIGNAT_FUNCTOR(argvar,signat,s',
							       oneshot_init PARTIAL)))
		  in (SBND sbnd, SDEC sdec)
		  end
	      | (Ast.FctFct _) => raise UNIMP
	      | (Ast.LetFct _) => raise UNIMP
	      | (Ast.AppFct _) => raise UNIMP
	      | (Ast.MarkFct (f,r)) => help (name,f))
	   val temp = map (help o fctb_strip) fctbs
       in (MOD_STRUCTURE (map #1 temp), SIGNAT_STRUCTURE (map #2 temp))
       end

    (* --------------------------------------------------------- 
      ------------------ STRUCTURE EXPRESSION -----------------
      --------------------------------------------------------- *)
     and xstrexp (context : context, strb : Ast.strexp, constr : Ast.sigexp option) : (mod * signat) = 
       (case constr of
	  NONE => (case strb of
		     Ast.VarStr path => 
		      (case (Context_Lookup(context,map symbol2label path)) of
					  PHRASE_CLASS_MOD(m,s) => (m,s)
					| _ => error "xstrexp failed")
		   | Ast.AppStr (path,strexpbool_list) => 
			 (case strexpbool_list of
			    [] => error "AppStr with []"
			  | [(strexp,_)] => 
			      (case (Context_Lookup(context,map symbol2label path)) of
				 PHRASE_CLASS_MOD(m,s) => 
				   (case s of
				      (SIGNAT_FUNCTOR(var1,sig1,sig2,_)) => 
					let val (module,signat) = xstrexp(context,strexp,NONE)
					  val var = fresh_var()
					  val (mod1,sig1'') = xcoerce(context2decs context,s,sig1)
					  val sig2' = sig2 
					in if (Sig_IsSub(context2decs context,
							 SIGNAT_FUNCTOR(var1,sig1,sig2,oneshot_init PARTIAL),
							 SIGNAT_FUNCTOR(fresh_var(),sig1,sig2',oneshot_init PARTIAL)))
					     then (MOD_APP(m,MOD_APP(MOD_FUNCTOR(var,signat,mod1),module)),
						   sig2')
					   else error "rule 251-3 failed"
					end
				    | _ => error "rule 251-2 failed")
			       | _ => error "rule 251-1 failed")
			  | _ => error "AppStr with list of length > 1")
		   | Ast.LetStr (dec,strexp) => (* rule 254 *) 
			    let val (var1,var2) = (fresh_var(), fresh_var())
			      val (lbl1,lbl2) = (fresh_open_label(),fresh_int_label())
			      val (inflag,mod1,sig1) = xdec'(context,dec)
			      val context' = if inflag 
					       then add_context_inline(context,lbl1,
								       INLINE_MODSIG(mod1,sig1))
					     else add_context_module(context,lbl1,var1,sig1)
			      val (mod2,sig2) = xstrexp(context',strexp,NONE)
			      val signat = sig2 (* XXX *)
			    in if (Sig_IsSub(context2decs(add_context_module(context,
									     fresh_int_label(),var1,sig1)),
					     sig2,signat))
				 then (MOD_PROJECT(MOD_STRUCTURE[SBND(lbl1,BND_MOD(var1,mod1)),
								 SBND(lbl2,BND_MOD(var2,MOD_SEAL(mod2,signat)))],
						   lbl2),
				       signat)
			       else error "Rule 254 failed"
			    end
		   | Ast.StructStr dec => xdec(context,dec)
		   | Ast.MarkStr (strexp,r) => xstrexp(context,strexp,NONE))


	| SOME sigexp => (* Rule 252 *)
	    let val (module,signat) = xstrexp(context,strb,NONE)
	      val var = fresh_var()
	      val sig' = xsigexp(context,sigexp)
	      val (mod',signat'') = xcoerce(context2decs context,signat,sig')
	    in 
	      (MOD_APP(MOD_FUNCTOR(var,signat,mod'),module),signat'')
	    end)

    (* --------------------------------------------------------- 
      ------------------ STRUCTURE BINDINGS --------------------
      --------------------------------------------------------- *)
     and xstrbinds (context : context, strbs : Ast.strb list) : (mod * signat) = 
       let val strbs = map strb_strip strbs
	   val temp = map (fn (name,(d,c)) => (name,xstrexp(context,d,c))) strbs
	   fun mod_help (n,(m,s)) = SBND(symbol2label n,BND_MOD(fresh_var(),m))
	   fun sig_help (n,(m,s)) = SDEC(symbol2label n,DEC_MOD(fresh_var(),s))
       in (MOD_STRUCTURE(map mod_help temp),SIGNAT_STRUCTURE(map sig_help temp))
       end

    (* --------------------------------------------------------- 
      ------------------ SIGNATURE PATCHING -------------------
      --------------------------------------------------------- *)
    and xsig_wheretype(signat : signat, lbls : label list, con : con, kind : kind) : signat = 
      let 
	val fv = map tyvar_getvar(con_free_tyvar con)
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
	  | (SIGNAT_DATATYPE (dt,tb,sdecs)) => SIGNAT_DATATYPE(dt,tb,dosig lbls sdecs)
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
		     | SIGNAT_DATATYPE (_,_,sd) => error "xsig_sharing got a datatype sig"
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
    and xcoerce (decs : decs,
		 signat0 : signat,
		 signat : signat) : Il.mod * Il.signat = 
      let 
	val v0 = fresh_var()
(* Rules 249 and 250 *)
	fun polyval_case (lbl,v,con : con,varsig_option) = 
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
				    SIGNAT_STRUCTURE[SDEC(_,con'')],_) =>
			let 
			    val itsig = SIGNAT_STRUCTURE[SDEC(it_lab,DEC_EXP(fresh_var(),con))]
			    val mtemp = MOD_APP(path2mod path,MOD_VAR var_poly)
			    val (bnd,dec,sig_poly') = 
			      (case varsig_option of
				 NONE => (BND_EXP(v,MODULE_PROJECT(mtemp,it_lab)),
					  DEC_EXP(v,con),
					  SIGNAT_STRUCTURE(#2(poly_inst(decs,sig_poly_sdecs))))
			       | SOME (v1,s1) => (BND_MOD(v,MOD_FUNCTOR(v1,s1,mtemp)),
						  DEC_MOD(v,SIGNAT_FUNCTOR(v1,s1,itsig,oneshot())),
						  SIGNAT_STRUCTURE
						  (#2(poly_inst(DEC_MOD(v1,s1)::decs,sig_poly_sdecs)))))
			    val s'' = SIGNAT_FUNCTOR(fresh_var(),sig_poly',itsig,oneshot())
			    val _ = if (Sig_IsSub(dec::decs,s,s''))
				      then ()
				    else error "cannot coerce"
			in (bnd,dec)
			end
		   | _ => error "cannot coerce")
		  end
	      | _ => error "cannot coerce")
		    
	fun doit (lbl,dec) : (bnd * dec) = 
	  (case dec of
	     DEC_EXP(v,c) =>
	       (case (Signat_Lookup(decs,(SIMPLE_PATH v0,signat0),[lbl])) of
		  (lbls,CLASS_EXP con) => (BND_EXP(v,path2exp(COMPOUND_PATH(v,lbls))), (* rule 248 *)
					   DEC_EXP(v,con))
		| (lbls,CLASS_MOD s) => polyval_case(lbl,v,c,NONE)  (* rule 250 *)
		| _ => error "cannot coerce")
	    | DEC_MOD(v,SIGNAT_FUNCTOR(v1,s1,                        (* rule 248 *)
			      SIGNAT_STRUCTURE[SDEC(_,DEC_EXP(_,c))],_)) => polyval_case(lbl,v,c,SOME(v1,s1))
	    | DEC_MOD(v,s) => (* rule 254 *)
	     (case (Signat_Lookup(decs,(SIMPLE_PATH v0,signat0),[lbl])) of
		(lbls,CLASS_MOD s1) => let val (m,s'') = xcoerce(decs,s1,signat)
					   val v1 = fresh_var()
				       in (BND_MOD(v,MOD_FUNCTOR(v1,s1,
								 MOD_APP(m,path2mod(COMPOUND_PATH(v0,lbls))))),
					   DEC_MOD(v,s''))
				       end
	      | _ => error "cannot coerce")
	    | DEC_CON(v,k,copt) => (* rules 251 XXX should do 252-253 *) 
	     (case (Signat_Lookup(decs,(SIMPLE_PATH v0,signat0),[lbl])) of
		(lbls,CLASS_CON knd) => 
		  let val con' = path2con(COMPOUND_PATH(v0,lbls))
		    val _ = (case copt of 
				 NONE => ()
			       | SOME con => if (eq_con(con,con',DEC_MOD(v0,signat0)::decs))
					       then () else error "xcoerce eq_con")
		  in (BND_CON(v,con'),DEC_CON(v,k,SOME con'))
		  end
	      | _ => error "cannot coerce")
	    | _ => error "cannot coerce")
		  
	fun loop decs [] : (sbnds * sdecs) = ([],[])
	  | loop decs ((SDEC(l,dec))::rest) = let val (bnd,dec) = doit (l,dec)
						  val (sbnds,sdecs) = loop (dec::decs) rest
					      in ((SBND(l,bnd))::sbnds, 
						  (SDEC(l,dec))::sdecs)
					      end
	val (m,s) = (case (signat0,signat) of
		       (SIGNAT_FUNCTOR(v1,s1,s1',a1), SIGNAT_FUNCTOR(v2,s2,s2',a2)) =>
			 let 
			   val _ = comp_unify(a1,a2)
			   val (m3,s3) = xcoerce(decs,s2,s1)
			   val (m4,s4) = xcoerce(decs,s1',s2')
			   val modexp = MOD_APP(m4,MOD_APP(MOD_VAR v0, MOD_APP(m3, MOD_VAR v2)))
			   val decs' = (DEC_MOD(v0,signat0))::(DEC_MOD(v2,s2))::decs
			   val s = GetModSig(decs',modexp)
			 in (MOD_FUNCTOR(v2,s2,modexp),
			     SIGNAT_FUNCTOR(v2,s2,s,a1))
			 end
		     | (_,SIGNAT_STRUCTURE sdecs) => let val (sbnds,sdecs) = loop decs sdecs
						     in (MOD_STRUCTURE sbnds,
							 SIGNAT_STRUCTURE sdecs)
						     end
		     | _ => error "xcoerce got bad sigs")
      in (MOD_FUNCTOR(v0,signat0,m),
	  SIGNAT_FUNCTOR(v0,signat0,s,oneshot_init TOTAL))
      end

    fun xeq (decs : decs, con : con) : exp = 
      let
	fun self c = xeq(decs,c)
	open Prim
      in (case con of
	    CON_TYVAR tyvar => (case (tyvar_deref tyvar) of
				NONE => error "resolved type does not permit equailty"
			      | SOME c => self c)
	  | CON_VAR _ => error "cannot compile equality on CON_VAR _"
	  | CON_OVAR ocon => self (ocon_deref ocon)
	  | CON_INT => PRIM(PRIM2(EQ_INTprim))
	  | CON_UINT => PRIM(PRIM2(EQ_UINTprim))
	  | CON_CHAR => PRIM(PRIM2(EQ_CHARprim))
	  | CON_FLOAT => PRIM(PRIM2(EQ_FLOATprim))
	  | CON_RECORD fields => let 
				   val v = fresh_var()
				   val v1 = fresh_var()
				   val v2 = fresh_var()
				   val paircon = con_tuple[con,con]
				   val e1 = RECORD_PROJECT(VAR v,generate_tuple_label 1,paircon)
				   val e2 = RECORD_PROJECT(VAR v,generate_tuple_label 2,paircon)
				   fun help (RDEC(lbl,con)) = 
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
						 [] => SCON(BOOL true)
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
				 val body = make_catch(inner_body,con_bool,SCON(BOOL false))
			       in #1(make_lambda(v,paircon,con_bool,
						 make_let([(v1,e1),(v2,e2)],body)))
			       end
	  | CON_LIST c => raise UNIMP
	  | CON_ARRAY c => raise UNIMP
	  | CON_VECTOR c => raise UNIMP
	  | CON_REF c => PRIM(PRIM2(EQ_REFprim {instance=c}))
	  | CON_MODULE_PROJECT(m,l) => MODULE_PROJECT(m,eq_lab)
	  | CON_MUPROJECT (j,con') => 
			       (case GetConKind(decs,con') of
				  KIND_TUPLE _ => error "cannot perform equality on con tuples"
				| KIND_ARROW(n,m) => 
				    let
				      val lbls = map0count (fn _ => fresh_int_label()) n
				      val vars' = map0count (fn _ => fresh_var()) n
				      val varseq = map0count (fn _ => fresh_var()) n
				      fun helper(v',l) = 
					(case (make_typearg_sdec(l,true)) of
					   (SDEC(_,DEC_MOD(_,s))) => DEC_MOD(v',s)
					 | _ => error "unexpected sig")
				      val augment = map2 helper (vars',lbls)
				      val decs = augment @ decs
				      local
					val temp = (map2 (fn (v',l) => CON_MODULE_PROJECT(MOD_VAR v',l))
						    (vars',lbls))
					val applied = ConApply(con,CON_TUPLE_INJECT temp)
				      in
					val reduced_cons = (case applied of
							      CON_TUPLE_INJECT conlist => conlist
							    | _ => error "unexpected reduced cons")
				      end
				      val expvs = map (fn c => xeq(decs,c)) reduced_cons
				      val table = zip vars' (map VAR varseq)
				      fun eproj_handler (MOD_VAR sv,lbl) = 
					if eq_label(lbl,eq_lab)
					  then assoc_eq(eq_var,sv,table)
					else NONE
					| eproj_handler _ = NONE
				      val var = fresh_var()
				      fun make_expeq (i,expv) = 
					let
					  val expv' = exp_subst_proj(expv,eproj_handler)
					  val e1 = UNROLL(con',
							  MODULE_PROJECT(MOD_VAR var,generate_tuple_label 1))
					  val e2 = UNROLL(con',
							  MODULE_PROJECT(MOD_VAR var,generate_tuple_label 2))
					in APP(expv',exp_tuple[e1,e2])
					end
				      val expseq = mapcount make_expeq expvs
				      val fbnds = map2 (fn (vareq,expeq) =>
							FBND(vareq,var,con_tuple[con,con],con_bool,expeq))
					          (varseq,expseq)
				    in FIX(fbnds,List.nth(varseq,j-1))
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
	val _ = clear_overload_table()
	val _ = clear_eq_table()
	val res = xobj arg
	val overload_table = get_overload_table()
	val eq_table = get_eq_table()
	fun overload_help (ocon,exp_oneshot,exp_maker) = 
	  (case (ocon_check(ocon,fn (c1,c2) => soft_eq_con(c1,c2,[]))) of
	     NONE => error "overloaded type: constraints not satisfied"
	   | SOME poslist => let val rexp = exp_maker poslist
			     in oneshot_set(exp_oneshot,rexp)
			     end)
	fun eq_help (decs,con,exp_oneshot) = 
	  let val _ = (print "eq_help: con = "; pp_con con; print "\n")
	    val eq_exp = xeq(decs,con)
	    val _ = oneshot_set(exp_oneshot,eq_exp)
	  in print "eq_help success\n"
	  end
        val _ = app overload_help overload_table 
	val _ = app eq_help eq_table 
      in res
      end
    
    val xdec = overload_wrap xdec
    val xexp = overload_wrap xexp

  end;