(*$import Stats AstHelp Il IlStatic IlUtil Ppil IlContext Pat InfixParse Datatype Equal Error Signature TOIL *)

(* todo : LetExp and CaseExp: valuability coputation too conservative
          optimize coercion functors to recognize when it is entirely unndeeded 
          optimize coercion functions of polymorphic values to be identity by normalizing type argument positions
*)

(* The translation from EL to IL *)
structure Toil
   :> TOIL =
  struct


    open AstHelp Il IlStatic IlUtil Ppil Pat
    open Util Listops Name IlContext Tyvar
    open Prim Error

    val parse_error = fn s => error "toil.sml: parse impossibility" s
    val pat_error = fn s => error "toil.sml: pattern impossibility" s
    val elab_error = fn s => error "toil.sml: elaborator impossibility" s
    val error = fn s => error "toil.sml" s

    val debug = Stats.ff("ToilDebug")

    fun debugdo t = if (!debug) then (t(); ()) else ()
    fun debugdo' t = (t(); ())
    fun nada() = ()


    type tyvar = (context,con) Tyvar.tyvar
    type ocon = (context,con) Tyvar.ocon
    type decresult = (sbnd option * context_entry) list
    type filepos = Ast.srcpos -> string * int * int

    fun dummy_exp' arg = let val (e,c) = Error.dummy_exp arg
			in  (e,c,true)
			end

    fun canonical_tyvar_label n = 
	if (n<0 orelse n>25) then error "canonical_tyvar_label given number out of range"
	else symbol_label(Symbol.tyvSymbol ("'" ^ (String.str (chr (ord #"a" + n)))))


    (*  --------- Elaboration state contains: -------------------------
        overload table, eq table, flex tables, 
	source position stack, error state
        -------------------------------------------------------------- *)

    local 
      val tyvar_table = ref([] : tyvar list)
      val overload_table = ref ([] : (region * ocon) list)
      val eq_table = ref ([] : (region * context * tyvar * exp Util.oneshot) list)
      val eq_stack = ref ([] : (region * dec list * context * tyvar * exp Util.oneshot) list list)
      val flex_table = ref ([] : (label * flexinfo ref * con * exp Util.oneshot) list)
    in
	fun reset_eq() = (eq_table := []; eq_stack := [])
	fun reset_elaboration fp = (Error.reset fp;
				    reset_eq();
				    tyvar_table := [];
				    overload_table := [];
				    flex_table := [])
	fun get_tyvar_table () = !tyvar_table
	fun add_tyvar_table tv = (tyvar_table := tv :: (!tyvar_table))

	fun get_overload_table () = !overload_table
	fun add_overload_entry ocon = (overload_table := (peek_region(),ocon)::(!overload_table))

	fun get_flex_table () = !flex_table
	fun add_flex_entry (l,rc,fc,e) = flex_table := (l,rc,fc,e)::(!flex_table)
	    
	fun get_eq_table () = (case !eq_stack of
				   [] => !eq_table
				 | _ => error "get_eq_table called when eq_stack non-empty")
	fun add_eq_entry (tyvar,expos) = 
	    (case (!eq_stack) of
		 [] => elab_error "cannot add entry: empty eq_stack"
	       | (first::rest) => let val ctxt = (case (tyvar_getctxts tyvar) of
						      [] => elab_error "tyvar must have some context"
						    | a::_ => a)
				      val first' = (peek_region(),[],ctxt,tyvar,expos)::first
				      val _ = debugdo (fn () => 
						       (print "add_eq_entry called with length first = ";
							print (Int.toString (length first));
							print "\n"))
				  in (eq_stack := first'::rest)
				  end)
	fun eq_table_push() = (debugdo (fn () => (print "EQ_PUSHING depth = "; 
						  print (Int.toString (length (!eq_stack)));
						  print "\n"));
			       eq_stack := [] :: (!eq_stack))
	fun eq_table_pop dec = 
	    let val _ = debugdo (fn () => (print "EQ_POPPING depth = "; 
					    print (Int.toString (length (!eq_stack)));
					    print " with dec = ";
					    pp_dec dec; print "\n"))
		val stack = !eq_stack
		fun help dec (reg, extra_decs,context,tyvar,expos) = 
		    (reg, dec :: extra_decs, context, tyvar,expos)
		val stack' = mapmap (help dec) stack
		fun help2 (reg,extra_decs,context,tyvar,expos) = 
		    (reg,foldr (fn (dec,ctxt) => add_context_dec(ctxt,SelfifyDec ctxt dec)) context extra_decs,
		     tyvar,expos)
	    in case stack' of
		[] => elab_error "cannot pop: empty eq_stack"
	      | (first :: rest) => 
		    let val _ = (debugdo (fn () => (print "EQ-There are "; 
						    print (Int.toString (length first));
						    print " items in first\n")))
			val first' = map help2 first
		    in  (case rest of
			     [] => (eq_stack := [];
				    eq_table := (map help2 first) @ (!eq_table))
			   | (second::rest') => (eq_stack := (first @ second) :: rest'))
		    end
	    end
    end

    val fresh_tyvar' = fresh_tyvar
    fun fresh_tyvar ctxt = let val tv = fresh_tyvar' ctxt
				val _ = add_tyvar_table tv
			    in  tv
			    end
    fun fresh_con ctxt = CON_TYVAR(fresh_tyvar ctxt)


     (* ----------------- overload_resolver ----------------------- *)
    fun overload_help warn (region,ocon) =
	let val _ = push_region region
	val res =  (
(*
	    print "overload_help: internal type is: ";
	    Ppil.pp_con (CON_TYVAR (ocon_deref ocon));
	    print "\n";
*)
	    case (ocon_constrain ocon) of
		0 => (error_region();
		       print "overloaded type: none of the constraints are satisfied\n";
		       true)
	      | 1 => true
	      | n => (if warn 
			       then 
				   (error_region(); 
				    print "Warning: more than one constraint satisfied by overloaded type")
			   else ();
			       false))
	    val _ = pop_region()
	in res
	end



     (* ----------------- Helper Functions ----------------------- *)
    fun is_non_const context (syms : Symbol.symbol list) = 
	(length syms = 1 andalso Symbol.eq(hd syms,Symbol.varSymbol "ref")) orelse
	(Datatype.is_nonconst_constr context syms) orelse
	(case Datatype.exn_lookup context syms of
	     NONE => false
	   | SOME {stamp,carried_type=NONE} => false
	   | SOME {stamp,carried_type=SOME _} => true)

    fun parse_pats context pats = 
	(case InfixParse.parse_pat(Context_Fixity context,is_non_const context, pats) of
	     SOME result => result
	   | NONE => (error_region();
		      print "cannot parse pattern\n";
		      error "cannot parse pattern\n"))
    fun parse_pat context pat = (case parse_pats context [pat] of
				     [pat] => pat
				   | _ => error "parse_pat getting back more than 1 pat")



    fun reduce_to_remove (context, c, sbnds) = 
	let 
	    val boundModVars = (List.mapPartial 
				(fn (SBND(_,BND_MOD(v,_,_))) => SOME v
			          | _ => NONE) sbnds)
	    val boundConVars = (List.mapPartial 
				(fn (SBND(_,BND_CON(v,_))) => SOME v
			          | _ => NONE) sbnds)

	    fun loop con = 
		let val occurVars = Name.VarSet.listItems(con_free con)
		in  if (null (Listops.list_inter_eq(eq_var,boundModVars,
						    occurVars)))
			then con
		    else (case (con_reduce_once(context,con)) of
			      NONE => error "reduce_to_remove failed to remove"
			    | SOME con => loop con)
		end
	in if (null boundModVars andalso null boundConVars)
	       then c
	   else loop c
	end

     fun sbnd_ctxt_list2modsig (sbnd_ctxt_list : (sbnd option * context_entry) list) 
	 : (mod * signat) = 
	 let fun loop (acc1,acc2) arg = 
	     (case arg of 
		  [] => (acc1,acc2)
		| ((NONE,_)::rest) => loop (acc1,acc2) rest
		| ((SOME sbnd,CONTEXT_SDEC sdec)::rest) => loop (sbnd::acc1,sdec::acc2) rest
		| ((SOME sbnd,_)::_) => elab_error "sbnd_ctxt_list2modsig: sbnd without sdec")
	     val (sbnds,sdecs) = loop ([],[]) sbnd_ctxt_list
	 in (MOD_STRUCTURE sbnds, SIGNAT_STRUCTURE (NONE,sdecs))
	 end


     and add_context_sbnd_ctxts 
	 (context : context,
	  sbnd_ctxt : (sbnd option * context_entry) list) : sbnd list * context = 
	 let 
	     val sbnds = List.mapPartial #1 sbnd_ctxt
	     fun folder((_,CONTEXT_SDEC (sdec as (SDEC(l,DEC_MOD(v,b,s))))),ctxt) = 
		 if (is_open l)
		     then let val s = reduce_signat ctxt s
			  in  add_context_sdec(ctxt,SelfifySdec ctxt (SDEC(l,DEC_MOD(v,b,s))))
			  end
		 else add_context_sdec(ctxt,SelfifySdec ctxt sdec)
	       | folder((_,CONTEXT_SDEC sdec),ctxt) = 
		 add_context_sdec(ctxt,SelfifySdec ctxt sdec)
	       | folder((_,ce),ctxt) = add_context_entries(ctxt,[ce])
	     val ctxt = foldl folder context sbnd_ctxt
	 in  (sbnds,ctxt)
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
     and packagedecs (xobj : context * 'a -> (sbnd option * context_entry) list)
         context (objs : 'a list) : (sbnd option * context_entry) list = 
         let 
             fun loop context [] = []
               | loop context [obj] = xobj(context,obj)
               | loop context (obj::rest) =
                 let 
                     val sbnd_ctxt_list = xobj(context,obj)
                     val (_,context') = add_context_sbnd_ctxts(context,sbnd_ctxt_list)
                     val sbnd_ctxt_restlist = loop context' rest
                 in sbnd_ctxt_list @ sbnd_ctxt_restlist
                 end
             val sbnd_ctxt_list = loop context objs
             fun maxmap_folder((NONE,_),map) = map
               | maxmap_folder((SOME(SBND(l,_)),_),map) = 
                 (case Name.LabelMap.find(map,l) of
                     SOME r => (r := 1 + (!r);
                                map)
                   | NONE => Name.LabelMap.insert(map,l,ref 1))
             val maxmap = foldl maxmap_folder Name.LabelMap.empty sbnd_ctxt_list 
             fun uniquify [] = []
               | uniquify (sbnd_ctxt::rest) = 
                 (case sbnd_ctxt of
                      (NONE,ce) => (NONE,ce) :: (uniquify rest)
                    | (SOME(sbnd as (SBND(l,bnd))),
                       ce as (CONTEXT_SDEC(SDEC(_,dec)))) => 
                          (case (Name.LabelMap.find(maxmap,l)) of
                              NONE => elab_error "maxmap must have entry at this point"
                            | SOME r =>
                                  (r := (!r) - 1;
                                   if (!r = 0)
                                       then (SOME sbnd, ce) :: (uniquify rest)
                                   else if (!r < 0)
                                            then elab_error "maxmap count inconsistency"
                                        else (* rename case *)
                                            let val name = Name.label2name l
						val l' = fresh_internal_label ("hidden_" ^ name)
                                            in (SOME (SBND(l',bnd)), 
                                                CONTEXT_SDEC(SDEC(l',dec)))
                                                :: (uniquify rest)
                                            end))
                    | (SOME _, _) => elab_error "packagedec: got sbnd without CONTEXT_SDEC")
         in uniquify sbnd_ctxt_list 
         end



    (* --------------------------------------------------------- 
      ------------------ EXPRESSIONS --------------------------
      --------------------------------------------------------- *)
    (* ----- Polymorphic (sdec) Instantiation ------------------------ 
       given a list of sdecs, return a triple consisting of
      (1) sbnds with the fresh types or already available types
      (2) the same sdecs except replace each opaque type with a fresh type
      (3) a list of the the fresh or already present types 
      ---------------------------------------------------------------- *)
     exception NoEqExp
     fun polyinst_opt (ctxt,sdecs) : (sbnds * sdecs * con list) option  = 
       (let
	   fun help sdecs =
	       (case sdecs of
		    [] => ([],[],[])
		  | (SDEC(l1,DEC_CON(v1,k1,copt,_)) :: SDEC(l2,DEC_EXP(v2,c2,_,_)) :: rest) =>
			let 
			    val (con,eq_con,e2) = 
				(case copt of
				     NONE => let 
						 val tyvar = fresh_named_tyvar (ctxt,"eq_tyvar")
						 val _ = tyvar_use_equal tyvar
						 val con = CON_TYVAR tyvar
						 val exp_os = oneshot()
						 val eq_con = con_eqfun con
						 val _ = add_eq_entry(tyvar,exp_os)
						 val e2 = OVEREXP(eq_con,true,exp_os)
					     in (con,eq_con,e2)
					     end
				   | SOME con => let val e2 = (case xeq(ctxt,con) of
								   SOME (e,c) => e
								 | NONE => raise NoEqExp)
						     val eq_con = con_eqfun con
						 in (con,eq_con,e2)
						 end)

			    val (sbnds,sdecs,cons) = help rest
			    val sbnds' = (SBND(l1,BND_CON(v1,con))) ::
					  (SBND(l2,BND_EXP(v2,e2))) :: sbnds
			    val sdecs' = (SDEC(l1,DEC_CON(v1,k1,SOME con,false)))::
					  (SDEC(l2,DEC_EXP(v2,c2,NONE,false))) :: sdecs
			    val cons' = con :: cons
			in (sbnds',sdecs',cons')
			end
		  | (SDEC(l,DEC_CON(v,k,NONE,_))::rest) =>
			let val c = fresh_con ctxt
			    val (sbnds,sdecs,cons) = help rest
			in ((SBND(l,BND_CON(v,c)))::sbnds,
			    (SDEC(l,DEC_CON(v,k,SOME c,false)))::sdecs,
			    c::cons)
			end
		  | (SDEC(l,DEC_CON(v,k,SOME c,i))::rest) =>
			let val (sbnds,sdecs,cons) = help rest
			in ((SBND(l,BND_CON(v,c)))::sbnds,
			    (SDEC(l,DEC_CON(v,k,SOME c,i)))::sdecs,
			    c::cons)
			end
		  | _ => (print "polyinst got strange sdecs:\n";
			  pp_sdecs sdecs;
			  elab_error "polyinst received strange sdecs"))
       in SOME(help sdecs)
       end
   handle NoEqExp => NONE)

    and polyinst arg = valOf(polyinst_opt arg)

    (* ---------- Polymorphic function Instantiation ------------------ *)
    and polyfun_inst (context, module : mod, s : signat) : exp * con = 
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
	  (SIGNAT_FUNCTOR(v,SIGNAT_STRUCTURE (_,arg_sdecs), 
			  SIGNAT_STRUCTURE (_,[SDEC(_,DEC_EXP(resv,resc,eopt,inline))]),_)) =>
	  let 
(*
	    val _ = (print "polyfun_inst' got module of:\n";
		     Ppil.pp_mod module;
		     print "\n\n  and signature of:\n";
		     Ppil.pp_signat s;
		     print "\n\n")
*)
	    local 
		fun dotype l v = let val tv = fresh_tyvar context
				     val c = CON_TYVAR tv
				 in (tv,(SBND(l,BND_CON(v,c)),
					 SDEC(l,DEC_CON(v,KIND_TUPLE 1, SOME c,false))))
				 end
		fun help ([] : sdecs) = []
		  | help (SDEC(l1,DEC_CON(v1,_,_,_)) :: SDEC(l2,DEC_EXP(v2,c2,_,_)) :: rest) =
		    let 
			val (tyvar,(sbnd1,sdec1)) = dotype l1 v1
			val _ = tyvar_use_equal tyvar
			val con = CON_TYVAR tyvar
			val eq_con = con_eqfun con
			val exp_os = oneshot()
			val _ = add_eq_entry(tyvar,exp_os)
			val eqexp = OVEREXP(eq_con,true,exp_os)
			val sbnd2 = SBND(l2,BND_EXP(v2,eqexp))
			val sdec2 = SDEC(l2,DEC_EXP(v2,c2,NONE,false))
		    in (sbnd1,sdec1) :: (sbnd2,sdec2) :: (help rest)
		    end
		  | help (SDEC(l,DEC_CON(v,_,_,_)) :: rest) = (#2(dotype l v))::(help rest)
		  | help _ = elab_error "unexpected sig to arg struct of polymorphic fun"
		val temp = help arg_sdecs
	    in 
	      val (signat_poly_sbnds, signat_poly_sdecs) = Listops.unzip temp
	      val mod_poly = MOD_STRUCTURE signat_poly_sbnds
	      val signat_poly = SIGNAT_STRUCTURE (NONE,signat_poly_sdecs)
	    end
	    val new_rescon : con = 
		remove_modvar_type(resc,v,signat_poly_sdecs)
		handle e => (print "remove_modvar failed: called from polyfun_inst";
			     print "target con: "; pp_con resc; print "\n";
			     print "variable to be removed  =  "; pp_var v; print "\n";
			     print "sdecs of var = \n"; pp_sdecs signat_poly_sdecs; print "\n";
			     error "remove_modvar failed: called from polyfun_inst")
	    val exp = (case (eopt,inline) of
			   (SOME e,inline) =>
			       let fun folder (SBND(l,BND_CON(_,c)),s) = subst_add_conpath(s,PATH(v,[l]),c)
				     | folder _ = error "bad functor application"
				   val subst = foldl folder empty_subst signat_poly_sbnds
			       in  exp_subst(e,subst)
			       end
			 | _ => MODULE_PROJECT(MOD_APP(module,mod_poly), it_lab))
(*
	    val _ = (print "polyfun_inst returning exp :\n";
		     pp_exp exp; print "\n\n  and rescon:\n";
		     pp_con new_rescon; print "\n\n")
*)
	  in (exp,new_rescon)
	  end
       |  _ =>  (print "polyfun_inst received s of\n";
		 pp_signat s;
		 print "\n";
		 elab_error "rule 224 or 226 not applicable in polyfun_inst"))


     and xrecordexp (context : context, sorted, sym_expr_list) : (exp * con * bool) =
	 let fun doer((sym,expr),(acc,va_acc)) = 
	     let val label = symbol_label sym
		 val (exp,con,va) = xexp(context,expr)
	     in ((label,(label,exp),(label,con))::acc,va andalso va_acc)
	     end
	     val (label_rbnd_rdec,va) = foldr doer ([],true) sym_expr_list 
	     val sorted = sorted orelse (label_issorted (map #1 label_rbnd_rdec))
	 in
	     if sorted 
		 then (RECORD(map #2 label_rbnd_rdec), CON_RECORD(map #3 label_rbnd_rdec),va)
	     else
		 let
		     fun make_var(l,rb,rd) = (l,(fresh_named_var (label2string l),rb,rd))
		     val label_var_rbnd_rdec = map make_var label_rbnd_rdec
		     val bnds = map (fn (_,(v,(_,e),_)) => BND_EXP(v,e)) label_var_rbnd_rdec
		     val label_var_rbnd_rdec = sort_labelpair label_var_rbnd_rdec
		     val con = CON_RECORD(map (fn (_,(_,_,rd)) => rd) label_var_rbnd_rdec)
		     val body = RECORD(map (fn (l,(v,_,_)) => (l,VAR v)) label_var_rbnd_rdec)
		 in (LET(bnds,body), con, va)
		 end
	 end
     
     and xexp (context : context, exp : Ast.exp) : (exp * con * bool) = (* returns valuablilty *)
      (
(*
       print "xexp called ";
       AstHelp.pp_exp exp;
       print "\n";
*)
       case exp of
	 Ast.IntExp lit => (SCON(int(W32,lit)), CON_INT W32, true)
       | Ast.WordExp lit => (SCON(uint(W32,lit)), CON_UINT W32, true)
       | Ast.RealExp s => (SCON(float(F64,s)), CON_FLOAT F64, true)
       | Ast.StringExp s => 
	     (SCON(vector (CON_UINT W8,
			   Array.fromList
			   (map (fn c => SCON(uint(W8,TilWord64.fromInt (ord c))))
			    (explode s)))), con_string, true)
       | Ast.CharExp s =>   
	     (SCON(uint(W8,
			(case (explode s) of
			     [c] => TilWord64.fromInt (ord c)
			   | _ => parse_error "Ast.CharExp carries non-charcter string"))),
	      CON_UINT W8, true)
       | (Ast.TupleExp exps) => xrecordexp(context,length exps < 10,
					   mapcount (fn(n,a) => (generate_tuple_symbol (n+1),a)) exps)
       | (Ast.RecordExp sym_exps) => xrecordexp(context,false, sym_exps)
       | Ast.ListExp args => 
	     let fun loop [] = AstHelp.nil_exp
		   | loop (a::b) = Ast.AppExp{function=AstHelp.cons_exp,
					      argument=Ast.TupleExp[a,loop b]}
	     in xexp(context,loop args)
	     end
       | Ast.SelectorExp s => 
     (* for the record, i'd like to say that this construct is a pain in the butt - Perry *)
	     let 
		 val label = symbol_label s
		 val stamp = get_stamp()
		 val fieldcon = fresh_con context
		 val the_ref = ref(FLEXINFO(stamp,false,[(label,fieldcon)]))
		 val rescon = CON_ARROW([CON_FLEXRECORD the_ref],
					fieldcon, false, oneshot_init PARTIAL)
		 val eshot : exp Util.oneshot = oneshot()
		 val exp = OVEREXP(rescon,true,eshot)
		 val _ = add_flex_entry(label,the_ref,fieldcon,eshot)
	     in (exp,rescon,true)
	     end
       | Ast.VarExp path => 
	     let fun unbound() = 
		 let val _ = (error_region(); 
			      print "unbound variable or constructor: ";
			      AstHelp.pp_path path;
			      print "\n")
		 in dummy_exp'(context,"unbound_var")
		 end
		 fun eqcase iseq = 
		     let val exp_os = oneshot()
			 val tyvar = fresh_named_tyvar (context,"teq")
			 val _ = tyvar_use_equal tyvar
			 val con = CON_TYVAR tyvar
			 val arg_con = con_tuple[con,con]
			 val eq_con = con_eqfun con
			 val _ = add_eq_entry(tyvar,exp_os)
			 val eqexp = OVEREXP(eq_con,true,exp_os)
			 val res = if iseq
				       then eqexp
				   else let val v = fresh_named_var "neq_arg"
					in  #1(make_lambda(v,arg_con, con_bool,
							   make_ifthenelse(APP(eqexp,VAR v),
									   false_exp,true_exp,con_bool)))
					end
		     in (res,eq_con,true)
		     end
	     in
		 (case (Context_Lookup_Labels(context,map symbol_label path)) of
		      SOME(_,PHRASE_CLASS_EXP (_,c,SOME e,true)) => (e,c,Exp_IsValuable(context,e))
		    | SOME(_,PHRASE_CLASS_EXP (e,c,_,_)) => (e,c,true)
		    | SOME (_, PHRASE_CLASS_OVEREXP cons_exps) =>
			  let 
			      val eshot = oneshot()
			      fun mk_constraint (con,exp) =
				  let fun check(tyvar,is_hard) = 
				      let val c' = CON_TYVAR tyvar
					  val match = (if is_hard then eq_con else soft_eq_con)
					      (context,con,c')
(*
					  val _ = (print "CONSTRAINT: "; pp_con con;
						   print "  with "; pp_con c'; print "  ";
						   if match 
						      then print "  satisfiable\n"
						  else  print "   failed\n")
*)
				      in  match
				      end
				      fun thunk() = 
					  (case (oneshot_deref eshot) of
					       SOME _ => ()
					     | NONE => oneshot_set(eshot,exp))
				  in  (thunk, check)
				  end
			      val constraints = map mk_constraint cons_exps
			      val ocon = Tyvar.fresh_ocon(context,constraints)
			      val con = CON_OVAR ocon
			      val exp = OVEREXP(con,true,eshot)
			      val _ = add_overload_entry ocon
			  in (exp,CON_OVAR ocon,Exp_IsValuable(context,exp))
			  end
		      
		      
		    | SOME(_,PHRASE_CLASS_MOD (m, _, s as SIGNAT_FUNCTOR _)) => 
			  let val (e,c) = polyfun_inst (context,m,s)
			  in  (e,c,true)
			  end
		    | SOME(_,PHRASE_CLASS_MOD (m, _,(SIGNAT_STRUCTURE(_,sdecs)))) =>
			  let fun dosdec (SDEC(l,DEC_EXP(_,c,_,_))) =
				   if (eq_label (l,mk_lab))
				       then (MODULE_PROJECT(m,mk_lab),c,true)
				   else unbound()
				| dosdec (SDEC(l,DEC_MOD(_,_,s))) =
				       if (eq_label(l,mk_lab))
					   then 
					       let val mk_mod = MOD_PROJECT(m,mk_lab)
						   val (e,c) = polyfun_inst(context,mk_mod,s)
					       in  (e,c,true)
					       end
				       else unbound()
				| dosdec _ = unbound()
			  in (case sdecs of
				  [sdec] => dosdec sdec
				| [_,sdec] => dosdec sdec
				| _ => unbound())
			  end
		    | SOME (_, PHRASE_CLASS_CON _) => unbound()
		    | NONE => if (length path = 1 andalso (Symbol.eq(hd path,Symbol.varSymbol "=")))
				  then eqcase true
			      else if (length path = 1 andalso 
				       (Symbol.eq(hd path,Symbol.varSymbol "<>")))
				       then eqcase false
				   else unbound())
	     end
       | Ast.DelayExp expr =>
	     (case (Context_Lookup_Label(context,symbol_label (Symbol.varSymbol "Susp")),
		    Context_Lookup_Label(context,symbol_label (Symbol.tycSymbol "susp"))) of
		 (SOME (_,PHRASE_CLASS_MOD(sm,_,SIGNAT_FUNCTOR(_,SIGNAT_STRUCTURE(_,[sdec]),_,_))), 
		  SOME(_,PHRASE_CLASS_CON(sc,sk,_,_))) =>  
		 (let 
		      val (e,c,va) = xexp(context,expr)
		      local
			  val (type_sbnds,_,new_cons) = polyinst(context,[sdec])
			  val _ = (case new_cons of
				       [new_con] => if (eq_con(context,new_con,c))
							then ()
						    else elab_error "new_con mus be unset"
				     | _ => elab_error "polyinst returned diff length list")
			  val type_mod = MOD_STRUCTURE type_sbnds
		      in
			  val wrapper_exp = MODULE_PROJECT(MOD_APP(sm,type_mod),it_lab)
		      end
		      fun make_thunk(c,e) = make_lambda(fresh_named_var "dummy_var",con_unit, c, e) 
		      val ref_arg = fresh_named_var "delay_ref"
		      val value_arg = fresh_named_var "delay_value"
		      val thunk_c = CON_ARROW([con_unit],c,false,oneshot_init PARTIAL)
		      val dummy_fun = #1(make_thunk(c, RAISE(c,bindexn_exp)))
		      val bnd = BND_EXP(ref_arg,ILPRIM(mk_ref,[thunk_c], [dummy_fun]))
		      val thunk_e = #1(make_thunk(c, APP(ILPRIM(deref,[thunk_c],[VAR ref_arg]),
							 unit_exp)))
		      val wrapped_exp = APP(wrapper_exp,thunk_e)
		      val final_c = CON_APP(sc, c)
		      val inner_body = #1(make_seq[(ILPRIM(setref,[thunk_c],
						    [VAR ref_arg,
						     #1(make_thunk(c, VAR value_arg))]), con_unit),
						   (VAR value_arg, c)])
		      val assign_exp = ILPRIM(setref,[thunk_c],
					    [VAR ref_arg,
					     #1(make_thunk(c, LET([BND_EXP(value_arg,e)], inner_body)))])
		      val body = #1(make_seq[(assign_exp, con_unit),(wrapped_exp,final_c)])
		  in  (LET([bnd],body), final_c, va)
		  end)
		| _ => elab_error "constructor #Susp or type #susp not defined in initial basis\n")
       | Ast.LetExp {dec,expr} => 
	     let val sbnd_ctxt_list = xdec true (context, dec)
		 val (sbnds,context') = add_context_sbnd_ctxts(context,sbnd_ctxt_list)
		 val (e,c,va) = xexp(context',expr)
		 val c = reduce_to_remove(context',c,sbnds)
		 val bnds = map (fn (SBND(_,bnd)) => bnd) sbnds
	     in  (LET(bnds,e),c,false) 
	     end
       (*  We compile plet's away as follows:

	plet val a = e1
	     val b = e2
        in   e3
	end

	---> extra primitives needed: Spawn and Yield
             We assume there are no user interrups; otherwise we must make sure the SetWaitState and Spawns
	     are atomic

	let val ra = ref NONE
            val rb = ref NONE
            fun ta() = ra := SOME e1 
            fun tb() = rb := SOME e2
            val _ = Ccall(Spawn ta)
            val _ = Ccall(Spawn,tb)
            val _ = Ccall(Yield,())
            val SOME a = !ra
            val SOME b = !rb
        in  e3
        end

	*)
       | Ast.PletExp {dec,expr} => 
	     let fun traverse (Ast.ValDec (vblist,_)) = 
		     let fun dovb (Ast.Vb{pat,exp}) = (pat,exp)
			   | dovb (Ast.MarkVb(vb,_)) = dovb vb
		     in  map dovb vblist
		     end
		   | traverse (Ast.MarkDec(dec,_)) = traverse dec
		   | traverse (Ast.SeqDec decs) = flatten(map traverse decs)
		   | traverse _ = error "bad declaration type in plet"

		 (* helpers *)
		 fun intexp n = Ast.IntExp(TilWord64.fromInt n)
		 fun call(f,args) = Ast.CcallExp(f,args)
		 fun callbind(sym,f,args) = Ast.ValDec([Ast.Vb{pat=Ast.VarPat[sym],exp=call(f,args)}],ref[])
		 fun app(f,arg) = Ast.AppExp{function=f,argument=arg}
		 fun appbind(sym,f,arg) = Ast.ValDec([Ast.Vb{pat=Ast.VarPat[sym],exp=app(f,arg)}],ref[])
		 fun sym str = Symbol.varSymbol str
		 fun var sym = Ast.VarExp[sym]
		 fun varsym str = var(sym str)
		 val tuple = Ast.TupleExp
		 val empty = tuple[]
		 val ref_expr = varsym "ref"
		 val deref_expr = varsym "!"
		 val set_expr = varsym ":="

		 (* symbols and expressions *)
val _ = print "plet0\n"
		 val pats_exps = traverse dec
		 val count = length pats_exps
		 val fresh_str = Int.toString(var2int(fresh_var()))
		 val refs = map0count (fn n => sym ("r" ^ fresh_str ^ "*" ^ (Int.toString n))) count
		 val thunks = map0count (fn n => sym ("t" ^ fresh_str ^ "*" ^ (Int.toString n))) count
		 val discard = sym "_"

		 (* decs *)
		 val ref_decs = map (fn refsym => appbind(refsym,ref_expr,varsym "NONE")) refs
		 fun thunker (thunk,ref_sym,(_,exp)) = 
		     let val body = app(set_expr,tuple[var ref_sym, app(varsym "SOME",exp)])
		     in  Ast.FunDec([Ast.Fb[Ast.Clause{pats=[{item=Ast.VarPat[thunk],fixity=NONE,region=(0,0)},
							     {item=Ast.TuplePat[],fixity=NONE,region=(0,0)}],
						       resultty=NONE,exp=body}]],
				     ref [])
		     end
		 val thunk_decs = map3 thunker (thunks,refs,pats_exps)
		 val spawn_decs = map (fn sym => callbind(discard,varsym "Spawn",
							 [var sym])) thunks
		 val yield_dec = callbind(discard,varsym "Yield",[empty])
		 val result_decs = 
		     map2 (fn ((pat,_),slot) => 
			   Ast.ValDec([Ast.Vb{pat=Ast.AppPat{constr=Ast.VarPat[Symbol.varSymbol "SOME"],
							     argument=pat},
					      exp=app(deref_expr,var slot)}],ref [])) (pats_exps,refs)
		 val decs = ref_decs @ thunk_decs @ spawn_decs @ [yield_dec] @ result_decs
		 val expr = Ast.LetExp {dec=Ast.SeqDec decs,expr=expr}
	     in  xexp(context,expr)
	     end
       | Ast.FlatAppExp _ => (case InfixParse.parse_exp(Context_Fixity context, exp) of
				  SOME exp' => xexp(context,exp')
				| NONE => (error_region();
					   print "cannot parse FlatAppExp\n";
					   dummy_exp'(context,"bad_application")))

       | Ast.CcallExp (function,arguments) => 
	     let val (e,con,va) = xexp(context,function)
		 val arrow =
		     (case con of
			  CON_ARROW(_,_,true,arrow) => arrow
			| _ => (case (con_normalize(context,con)) of
				    CON_ARROW(_,_,true,arrow) => arrow
				  | _ => oneshot()))
		 val e_con_va_args = map (fn e => xexp(context,e)) arguments
		 val va_args = Listops.orfold #3 e_con_va_args
		 val exp_args = map #1 e_con_va_args
		 val con_args = map #2 e_con_va_args
		 val spec_rescon = fresh_con context
		 val spec_funcon = CON_ARROW(con_args,spec_rescon,
					     true,arrow)
	     in
		 if (semi_sub_con(context,spec_funcon,con))
		     then (let val va3 = 
			       (case oneshot_deref arrow of
				      NONE => (oneshot_set(arrow,PARTIAL); false)
				    | SOME PARTIAL => false
				    | SOME TOTAL => true)
			   in  (EXTERN_APP(con,e,exp_args),
				con_deref spec_rescon,
				va andalso va_args andalso va3)
			   end)
		 else
		     (case (con_normalize(context,con)) of
			 CON_ARROW(param_cons,_,_,_) => 
			     (error_region(); print " external application is ill-typed.\n";
			      print "  Function domain: "; 
			      app (fn c => (pp_con c; print "; ")) param_cons;
			      print "\n  Argument type: "; 
			      app (fn c => (pp_con c; print "; ")) con_args;
			      print "\n";
			      dummy_exp'(context,"bad_application"))
		       | nonarrow => (error_region(); 
				      print " operator is not a function. Has type:\n";
				      pp_con nonarrow;
				      print "\n";
				      dummy_exp'(context,"bad_application")))
	     end

       | Ast.AppExp {argument,function} => 
	     let val (e1,con1,va1) = xexp(context,function)
		 val (e2,con2,va2) = xexp(context,argument)
		 val arrow = oneshot()
		 val rescon = fresh_con context
		 val funcon = CON_ARROW([con2],rescon,false,arrow)
		 fun constrain (OVEREXP (CON_OVAR ocon,_,_)) = 
			   (overload_help false (peek_region(),ocon); ())
		   | constrain _ = ()
	     in  if (semi_sub_con(context,funcon,con1))
		     then (let val va3 = (case oneshot_deref arrow of
					      NONE => (oneshot_set(arrow,PARTIAL); false)
					    | SOME PARTIAL => false
					    | SOME TOTAL => true)
			       val _ = constrain e1
			       val _ = constrain e2
			       val exp = APP(e1,e2)
			   in  ((case exp_reduce exp of
				     NONE => exp
				   | SOME e => e),
				con_deref rescon,
				va1 andalso va2 andalso va3)
			   end)
		 else
		     (case (con_normalize(context,con1)) of
			 CON_ARROW([param_con],rescon,_,_) => 
			     (error_region(); print " application is ill-typed.\n";
			      print "  Function domain: "; pp_con param_con;
			      print "\n  Argument type: "; pp_con con2;
			      print "\n  Expanded function domain: "; pp_con (con_normalize(context,param_con));
			      print "\n  Expanded argument type: "; pp_con (con_normalize(context,con2));
			      print "\n";
			      dummy_exp'(context,"bad_application"))
		       | nonarrow => (error_region(); 
				      print " operator is not a function. Has type:\n";
				      pp_con nonarrow;
				      print "\n";
				      dummy_exp'(context,"bad_application")))
	     end
       | Ast.AndalsoExp (e1,e2) => 
	     xexp(context,Ast.IfExp{test=e1,thenCase=e2,elseCase=AstHelp.false_exp})
       | Ast.OrelseExp (e1,e2) => 
	     xexp(context,Ast.IfExp{test=e1,thenCase=AstHelp.true_exp,elseCase=e2})
       | Ast.IfExp {test,thenCase,elseCase} => 
	     let val pr1 = {pat=true_pat,exp=thenCase}
		 val pr2 = {pat=false_pat,exp=elseCase}
	     in xexp(context,Ast.CaseExp {expr=test, 
					  rules=[Ast.Rule pr1, Ast.Rule pr2]})
	     end
       | Ast.ConstraintExp {expr, constraint} => 
	     let val (exp,con,va) = xexp(context,expr)
		 val con' = xty(context,constraint)
	     in if (sub_con(context,con,con'))
		    then (SEAL(exp,con'),con',va)
		else let val (e,c) = dummy_exp(context,"badseal")
		     in  error_region();
			 print "constraint does not match expression type\n";
			 print "  Expression type: "; pp_con con;
			 print "\n  Constraint type: "; pp_con con';
			 print "\n";
			 (SEAL(e,con'),con',va)
		     end
	     end		    
       | Ast.VectorExp expr_list => 
	     let val c = fresh_con context
		 val ecv_list = map (fn e => xexp(context,e)) expr_list
		 val elist = map #1 ecv_list
		 fun folder (_,c',_) = (eq_con(context,c,c') orelse
					(error_region();
					 print "ill-typed vector expression\n";
					 false))
	     in if (andfold folder ecv_list)
		    then (SCON(vector(c,Array.fromList elist)), CON_VECTOR c,
			  andfold (fn (_,_,va) => va) ecv_list)
		else dummy_exp'(context, "bad_vector")
	     end
       | Ast.WhileExp {test,expr} => 
	     let 
		 val (teste,testc,_) = xexp(context,test)
		 val (be,bc,_) = xexp(context,expr)
		 val body_ec = (be,bc)
	     in  if (eq_con(context,testc,con_bool)) 
		     then 
			 let val loop_var = fresh_named_var "loop"
			     val arg_var = fresh_named_var "loop_arg"
			     val (then_exp,_) = make_seq[body_ec, 
							 (APP(VAR loop_var, unit_exp),
							  con_unit)]
			     val loop_body = make_ifthenelse(teste,then_exp,unit_exp,con_unit)
			     val loop_fun = FIX(true,PARTIAL,[FBND(loop_var,arg_var,con_unit,con_unit,loop_body)])
			 in (LET([BND_EXP(loop_var,loop_fun)],APP(VAR loop_var, unit_exp)),
			     con_unit, false)
			 end
		 else (error_region();
		       print "while construct given a test clause not of boolean type\n";
		       dummy_exp'(context,"badwhile"))
	     end
       | Ast.HandleExp {expr,rules} => (* almost same as CaseExp except need to wrap with HANDLE *)
	     let 
		 val (exp',rescon,va) = xexp(context,expr)
		 val v = fresh_named_var "handle_exn"
		 val arms = map (fn (Ast.Rule{pat,exp})=>(parse_pat context pat,exp)) rules
		 val (hbe,hbc) = caseCompile{context = context,
					     arms = arms,
					     arg = (v,CON_ANY),
					     reraise = true}
		 val (he,hc) = make_lambda(v,CON_ANY,hbc,hbe)
	     in if (eq_con(context,rescon,hbc))
		    then (HANDLE(rescon,exp',he),rescon,va)
		else let val rescon = supertype rescon
		     in  if sub_con(context,hbc,rescon)
			     then (HANDLE(rescon,exp',he),rescon,va)
			 else (error_region();
			       print "mismatch between handle body and handler\n";
			       dummy_exp'(context,"bad_handle"))
		     end
	     end
       | Ast.RaiseExp e => 
	     let val (exp,con,_) = xexp(context,e)
		 val c = fresh_con context
	     in if (eq_con(context,con,CON_ANY)) 
		    then (RAISE (c,exp),c,false)
		else (error_region(); print "raise not given an expression of exn type\n";
		      dummy_exp'(context,"badraise"))
	     end
       | Ast.SeqExp elist => 
	     let val ecvlist = map (fn e => xexp(context,e)) elist
		 val eclist = map (fn (e,c,v) => (e,c)) ecvlist
		 val (e,c) = make_seq eclist
	     in  (e,c,andfold (fn (e,c,v) => v) ecvlist)
	     end
       | Ast.FnExp [] => parse_error "Ast.FnExp with empty list"
       | Ast.FnExp rules => 
	     let val arms = map (fn (Ast.Rule{pat,exp}) => (parse_pats context [pat],exp)) rules
		 val {arglist,body} = funCompile{context = context,
						 rules = arms}
		 fun help ((v,c),(e,resc)) = make_lambda(v,c,resc,e)
		 val (e,c) = foldr help body arglist
	     in  (e,c,true)
	     end
       | Ast.CaseExp {expr,rules} =>  
	     let fun getarm (Ast.Rule{pat,exp}) = (parse_pat context pat,exp)
		 val arms = map getarm rules
		 val (arge,argc,_) = xexp(context,expr)
		 val (context,wrap,v) = (case arge of 
					     VAR v => (context,fn e => e, v)
					   | _ => let val v = fresh_named_var "casearg"
						      fun wrap e = LET([BND_EXP(v,arge)],e)
						  in  (add_context_exp'(context,v,argc),wrap,v)
						  end)
		 val (e,c) = caseCompile{context = context,
					 arms = arms,
					 arg = (v,argc),
					 reraise = false}
	     in (wrap e,c,false)
	     end
       | Ast.MarkExp(exp,region) => 
	     let val _ = push_region region
		 val res = xexp(context,exp)
		 val _ = pop_region()
	     in res 
	     end)


    (* --------------------------------------------------------- 
      ------------------ DECLARATIONS --------------------------
      --------------------------------------------------------- *)
      and make_typearg_sdec [] = []
        | make_typearg_sdec ((type_lab,is_eq) :: more) = 
	 let 
	     val rest = make_typearg_sdec more
	     val type_str = label2string type_lab
	     val type_var = fresh_named_var type_str 
	     val type_sdec = SDEC(type_lab,DEC_CON(type_var, KIND_TUPLE 1, NONE, false))
	     val eq_lab = to_eq type_lab
	     val eq_str = label2string eq_lab
	     val eq_var = fresh_named_var eq_str
	     val eq_con =  con_eqfun (CON_VAR type_var)
	     val eq_sdec = SDEC(eq_lab,DEC_EXP(eq_var, eq_con,NONE,false))
	 in  if (is_eq) then (type_sdec :: eq_sdec :: rest) else type_sdec :: rest
	 end
	

   and xdatatype (context,datatycs) : (sbnd * sdec) list =
        Datatype.compile{context=context,
			 typecompile=xty,
			 datatycs=datatycs,
			 eq_compile=xeq}

     and xfundec islocal (context : context, dec_list, tyvar_stamp, sdecs1, var_poly, open_lbl) =
	 let
(* We must discover all the variables to generalize over.  
   For the user-level variables, we must also compile in a context 
   with these user-level variables bound.  As a first
   step, we find all the user-level variables in the program. 
	At some future point when the syntax includes explicit scoping, 
	  we must include those too *)
		 
	     val fun_ids = map #1 dec_list
	     val fun_vars = map #2 dec_list
		 
	     (* --- create the context with all the fun_ids typed --- *)
	     val context_fun_ids = 
		 let fun help ((l,v,c,_,_),ctxt) = add_context_exp(ctxt,l,v,c)
		 in  foldl help context dec_list
		 end
	     
	     val context'' = add_context_mod(context_fun_ids,open_lbl,var_poly,
					     SelfifySig context (PATH (var_poly,[]),
								 SIGNAT_STRUCTURE(NONE, sdecs1)))
		 
		 
	     val _ = eq_table_push()
	     val fbnd_con_list = 
		 (map (fn (_,var',fun_con,body_con,matches) => 
			let val {body = (bodye,bodyc), arglist} = 
			         funCompile {context = context'',
					     rules = matches}
			    fun con_folder ((_,c),acc) = CON_ARROW([c],acc,false,oneshot_init PARTIAL)
			    val func = foldr con_folder bodyc arglist
			    val _ = if eq_con(context'',body_con,bodyc)
					then ()
				    else (error_region();
					  print "function constraint does not match body type\n";
					  print "Actual type: "; pp_con bodyc; print "\n";
					  print "Constraint type: "; pp_con body_con; print "\n";
					  print "Actual reduced type: "; 
					  pp_con (con_normalize(context'',bodyc)); print "\n";
					  print "Constraint reduced type: "; 
					  pp_con (con_normalize(context'',body_con)); print "\n")
			    val _ = if eq_con(context'',fun_con,func)
					then ()
				    else (error_region();
					  print "function constraint does not match function type\n";
					  print "Actual type: "; pp_con func; print "\n";
					  print "Constraint type: "; pp_con fun_con; print "\n")
			    local 
				fun help ((v,c),(e,resc)) = make_lambda(v,c,resc,e)
			    in 
				val (var1,con1)::_ = arglist
				val (bigbodyc,bigbodye) = foldr help (bodye,bodyc) (tl arglist)
			    end 
			in (FBND(var',var1,con1,bigbodye,bigbodyc),func)
			end)
		 dec_list)
		 
		 
	     val fbnds = map #1 fbnd_con_list
	     val fbnd_cons : con list = map #2 fbnd_con_list
	     (* Use symbols cannot have a bang andalphnumeric characters *)
	     val top_name = foldl (fn (l,s) => (Name.label2name l) ^ "_" ^ s) "" fun_ids
	     val top_label = to_nonexport(to_cluster (internal_label top_name))
	     val top_var = fresh_named_var "cluster"
	     val top_exp_con = (FIX(true,PARTIAL,fbnds),
				case fbnd_cons of
				    [c] => c
				  | _ => con_tuple fbnd_cons)
		 
	     local 
		 val (_,top_con) = top_exp_con
		 val tyvar_lbls'_useeq = 
		     rebind_free_type_var(length sdecs1,
					  tyvar_stamp,top_con,
					  context_fun_ids,var_poly)
		 fun help(_,tlab,iseq) = (tlab,iseq)
		 val temp = map help tyvar_lbls'_useeq
	     in
		 val sdecs2 = make_typearg_sdec temp
		 val sdecs = sdecs1 @ sdecs2
	     end
	 
	 
	     val temp_exp = (case sdecs of
				 [] => VAR top_var
			       | _ => let val s = MOD_APP(MOD_VAR top_var,
							  MOD_VAR var_poly)
				      in MODULE_PROJECT(s,them_lab)
				      end)
		 
	     val exp_con_list = 
		 case fbnd_cons of
		     [c] => [(temp_exp,c)]
		   | _ => mapcount (fn (i,c) => (RECORD_PROJECT(temp_exp, 
								generate_tuple_label (i+1),
								#2 top_exp_con), c))
			 fbnd_cons


	     val _ = 
		 let val extra_dec = DEC_MOD(var_poly,false,SIGNAT_STRUCTURE(NONE, sdecs))
		 in  eq_table_pop extra_dec
		 end


	     fun modsig_helper inner_lab (name,id,(exp,con)) = 
		 let val inner = Name.fresh_named_var ((Name.var2name name) ^ "_inner")
		     fun poly_case () = 
			 let 
			     val sig_poly = SIGNAT_STRUCTURE (NONE,sdecs)
			     val sbnd = SBND(inner_lab, BND_EXP(inner,exp))
			     val sdec = SDEC(inner_lab, DEC_EXP(inner,con,NONE,false))
			     val inner_sig = SIGNAT_STRUCTURE(NONE, [sdec])
			     val functor_mod = MOD_FUNCTOR(TOTAL,var_poly,sig_poly,
							   MOD_STRUCTURE[sbnd],inner_sig)
			     val functor_sig = 
				 SIGNAT_FUNCTOR(var_poly,sig_poly,
						inner_sig,TOTAL)
			 in  (SBND(id,BND_MOD(name,true,functor_mod)),
			      SDEC(id,DEC_MOD(name,true,functor_sig)))
			 end
		 in
		     (case sdecs of
			  [] => (SBND(id,BND_EXP(name,exp)),
				 SDEC(id,DEC_EXP(name,con,NONE,false)))
			| _ => poly_case())
		 end
	     
	     val (top_sbnd,top_sdec) = modsig_helper them_lab (top_var,top_label,top_exp_con)
	     val top_sbnd_entry = (SOME top_sbnd, CONTEXT_SDEC top_sdec)
	     val sbnds_sdecs = map3 (modsig_helper it_lab) (fun_vars,fun_ids,exp_con_list)
	     val sbnds_entries = (map (fn (sbnd,sdec) => (SOME sbnd,CONTEXT_SDEC sdec)) 
				  sbnds_sdecs)
	 in  top_sbnd_entry :: sbnds_entries
	 end

     and xdec islocal (context : context, d : Ast.dec) : (sbnd option * context_entry) list =
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
	  Ast.ValDec ([],_) => []
	| Ast.ValDec (vblist,ref tyvars) => (* non-recursive bindings *)
	    let 
		local
		  val pe_list = map vb_strip vblist
		in
		  val (pat,expr) = (case pe_list of
				      [] => pat_error "let with nothing should not get here"
				    | [(p,e)] => (p,e)
				    | _ => (Ast.TuplePat(map #1 pe_list),
					    Ast.TupleExp(map #2 pe_list)))
		end

		val tyvar_stamp = get_stamp()
		val lbl = to_open(internal_label ("!varpoly"))
		val var_poly = fresh_named_var "varpoly"
		val lbl' = to_nonexport (internal_label "!valbind")
		val var' = fresh_named_var "valbind"
		val tyvars = map tyvar_strip tyvars
		local
		     fun help tyvar = 
			  let val type_str = Symbol.name tyvar
			      val type_lab = symbol_label tyvar
			      val is_eq =  ((size type_str >= 2) andalso 
					    (String.substring(type_str,0,2) = "''"))
			  in  (type_lab,is_eq)
			  end
		     val temp = map help tyvars
		in  val temp_sdecs = make_typearg_sdec temp
		end
		val context' = add_context_mod(context,lbl,var_poly,
						  SelfifySig context (PATH (var_poly,[]),
							     SIGNAT_STRUCTURE (NONE,temp_sdecs)))
		val _ = eq_table_push()
		val lbl = internal_label "!bindarg"
		val v = fresh_named_var "bindarg"
		val (e,con,va) = xexp(context',expr)
		val sbnd_sdec = (SBND(lbl,BND_EXP(v,e)),SDEC(lbl,DEC_EXP(v,con,NONE,false)))
		val context' = add_context_exp'(context',v,con)
                val parsed_pat = parse_pat context pat
		val bind_sbnd_sdec = (bindCompile{context = context',
						  bindpat = parsed_pat,
						  arg = (v,con)})
		val sbnd_sdec_list = 
		    (case bind_sbnd_sdec of
			 [(SBND(lbl',BND_EXP(v'',VAR v')),_)] =>
			     if (eq_var(v,v'))
				 then [(SBND(lbl',BND_EXP(v'',e)), SDEC(lbl',DEC_EXP(v'',con,NONE,false)))]
			     else sbnd_sdec::bind_sbnd_sdec
		       | _ => sbnd_sdec::bind_sbnd_sdec)
		val is_irrefutable = va andalso Sbnds_IsValuable(context', map #1 bind_sbnd_sdec)
		fun refutable_case () = 
		    let val _ = eq_table_pop (DEC_MOD(fresh_named_var "dummy", false, SIGNAT_STRUCTURE(NONE,[])))
		    in  map (fn (sbnd,sdec) => (SOME sbnd,CONTEXT_SDEC sdec)) sbnd_sdec_list
		    end
		and irrefutable_case () = 
		    let
			val _ = debugdo (fn () => (print "about to call rebind_free_type_var:  var_poly = ";
						   pp_var var_poly; print "  stamp = ";
						   print (Int.toString (stamp2int tyvar_stamp));
						   print "\nand con = \n";
						   pp_con con; print"\n\n"))
			    
			    
			val tyvar_lbls_useeq = rebind_free_type_var(length tyvars,
								    tyvar_stamp,con,
								    context,var_poly)
			val lbls_useeq = (map (fn (_,l,f) => (l,f)) tyvar_lbls_useeq)
			val _ = debugdo (fn () => (print "done calling rebind_free_type_var:  var_poly = ";
						   pp_var var_poly; print "\nand con = \n";
						   pp_con con; print"\n\n"))
			val poly_sdecs = temp_sdecs @ (make_typearg_sdec lbls_useeq)
			val (sbnds,sdecs) = (map #1 sbnd_sdec_list, map #2 sbnd_sdec_list)
			val _ = 
			    let val extra_dec = DEC_MOD(var_poly, false,SIGNAT_STRUCTURE (NONE,poly_sdecs))
			    in  eq_table_pop extra_dec
			    end
		        val a = if is_irrefutable then TOTAL else PARTIAL
		    in
			(case poly_sdecs of
			     [] => map2 (fn (sbnd,sdec) => (SOME sbnd, CONTEXT_SDEC sdec)) (sbnds,sdecs)
			   | _ =>
				 let 
				     val sig_poly = SIGNAT_STRUCTURE (NONE,poly_sdecs)
				     val labs = map (fn SDEC (l,_) => l) sdecs
				     val cons = map (fn SDEC(l,DEC_EXP(_,c,_,_)) => c | _ => elab_error "Rule 237") sdecs
				     fun mod_sig_help (l,c) =
					 let val modapp = MOD_APP(MOD_VAR var',MOD_VAR var_poly)
					     val inner_var = fresh_named_var "inner_valbind"
					     val outer_var = fresh_named_var "outer_valbind"
						 
					     val temp_mod = MOD_STRUCTURE[SBND(it_lab,
									       BND_EXP(inner_var,
										       MODULE_PROJECT(modapp,l)))]
					     val temp_sig = SIGNAT_STRUCTURE(NONE,
									     [SDEC(it_lab,
										   DEC_EXP(inner_var,c,NONE,false))])
					     val bnd = BND_MOD(outer_var,true,
								MOD_FUNCTOR(a,var_poly,sig_poly,
									    temp_mod,temp_sig))
					     val dec = DEC_MOD(outer_var,true,
							       SIGNAT_FUNCTOR(var_poly,sig_poly,
									      temp_sig, a))
					 in (SBND(l,bnd),SDEC(l,dec))
					 end
				     val temp_mod = MOD_FUNCTOR(a,var_poly,sig_poly,
								MOD_STRUCTURE sbnds,
								SIGNAT_STRUCTURE(NONE, sdecs))
				     val temp_sig = SIGNAT_FUNCTOR(var_poly,sig_poly,
								   SIGNAT_STRUCTURE(NONE, sdecs), a)
				     val rest_sbnds_sdecs = map2 mod_sig_help (labs,cons)
				     val final_sbnds = ((SBND(lbl',BND_MOD(var',true,temp_mod)))::
							(map #1 rest_sbnds_sdecs))
				     val final_sdecs = ((SDEC(lbl',DEC_MOD(var',true,temp_sig))) ::
							(map #2 rest_sbnds_sdecs))
				 in map2 (fn (sbnd,sdec) => (SOME sbnd,CONTEXT_SDEC sdec)) (final_sbnds, final_sdecs)
				 end)
		    end
	    in if (is_irrefutable)
		then irrefutable_case()
	       else refutable_case()
	    end
        (* recursive value dec: i.e. functions *)
	| Ast.ValrecDec (rvblist,ref tyvars) => 
	    let val tyvar_stamp = get_stamp()
		val tyvars = map tyvar_strip tyvars
		local fun help tyvar = 
		    let val type_str = Symbol.name tyvar
			val type_lab = symbol_label tyvar
			val is_eq =  ((size type_str >= 2) andalso 
				      (String.substring(type_str,0,2) = "''"))
		    in  (type_lab,is_eq)
		    end
		    val temp = map help tyvars
		in  val sdecs1 = make_typearg_sdec temp
		end
	 
		val var_poly = fresh_named_var "var_poly"
		val open_lbl = to_open(internal_label "!opened")
		val context' = add_context_mod(context,open_lbl,var_poly,
					       SelfifySig context (PATH (var_poly,[]),
								   SIGNAT_STRUCTURE(NONE, sdecs1)))

		fun rvb_help {var:Ast.symbol, fixity: (Ast.symbol * 'a) option,
			      exp:Ast.exp, resultty: Ast.ty option} = 
		let 
		    val body_con = fresh_named_con (context',"body_con")
		    val fun_con = (case resultty of
				       NONE => fresh_named_con (context',"fun_con")
				     | SOME ty => xty(context',ty))
		    fun help (Ast.Rule{pat,exp}) = 
			(case pat of
			     Ast.ConstraintPat{constraint=ty,pattern} =>  
				 let val given_res_con = xty(context',ty)
				 in  if (eq_con(context',given_res_con,body_con))
					 then ()
				     else (error_region();
					   print "fn matches have conflicting constraints,";
					   print " using the first one\n");
					 (parse_pats context' [pattern],exp)
				 end
			   | _ => (parse_pats context' [pat],exp))
		    val matches = 
			(case exp of 
			     Ast.FnExp rules => map help rules
			   | _ => parse_error "val rec requires an fn expression")
		    val fun_lab = symbol_label var
		    val fun_var = fresh_named_var (label2string fun_lab)
		in  (fun_lab, fun_var, fun_con, body_con, matches)
		end
		val dec_list : (label * var * con * con *
				(Ast.pat list * Ast.exp) list) list =
		    map (rvb_help o rvb_strip) rvblist
	    in  xfundec islocal (context,dec_list,tyvar_stamp,sdecs1,var_poly,open_lbl)
	    end

	(* We compile away parallel bindings like this:

	     pval p1 = e1
	     and  p2 = e2

	     ---> extra primitives needed: Spawn and Yield
                  We assume there are no user interrupts.
	          Otherwise we must make sure the SetWaitState and Spawns are atomic.

	    val r1 = ref NONE
            val r2 = ref NONE
            fun t1() = r1 := SOME e1 
            fun t1() = r2 := SOME e2
            val _ = Ccall(Spawn t1)
            val _ = Ccall(Spawn,t2)
            val _ = Ccall(Yield,())
            val SOME pa = !r1
            val SOME pb = !r2	     
	 *)
	| Ast.PvalDec (vblist,ref tyvars) => (* parallel bindings *)
	    let
		 (* helpers *)
		fun intexp n = Ast.IntExp(TilWord64.fromInt n)
		fun call(f,args) = Ast.CcallExp(f,args)
		fun callbind(sym,f,args) = Ast.ValDec([Ast.Vb{pat=Ast.VarPat[sym],exp=call(f,args)}],ref[])
		fun app(f,arg) = Ast.AppExp{function=f,argument=arg}
		fun appbind(sym,f,arg) = Ast.ValDec([Ast.Vb{pat=Ast.VarPat[sym],exp=app(f,arg)}],ref[])
		fun sym str = Symbol.varSymbol str
		fun var sym = Ast.VarExp[sym]
		fun varsym str = var(sym str)
		val tuple = Ast.TupleExp
		val empty = tuple[]
		val ref_expr = varsym "ref"
		val deref_expr = varsym "!"
		val set_expr = varsym ":="
		    
		(* symbols and expressions *)
		val pats_exps = map vb_strip vblist
		val count = length pats_exps
		val fresh_str = Int.toString(var2int(fresh_var()))
		val refs = map0count (fn n => sym ("*result" ^ (Int.toString n))) count
		val thunks = map0count (fn n => sym ("*thunk" ^ (Int.toString n))) count
		val discard = sym "_"
		    
		(* decs *)
		val ref_decs = map (fn refsym => appbind(refsym,ref_expr,varsym "NONE")) refs
		fun thunker (thunk,ref_sym,(_,exp)) = 
		    let val body = app(set_expr,tuple[var ref_sym, app(varsym "SOME",exp)])
		    in  Ast.FunDec([Ast.Fb[Ast.Clause{pats=[{item=Ast.VarPat[thunk],fixity=NONE,region=(0,0)},
							    {item=Ast.TuplePat[],fixity=NONE,region=(0,0)}],
						      resultty=NONE,exp=body}]],
				   ref [])
		    end
		val thunk_decs = map3 thunker (thunks,refs,pats_exps)
		val spawn_decs = map (fn sym => callbind(discard,varsym "Spawn",
							 [var sym])) thunks
		val yield_dec = callbind(discard,varsym "Yield",[empty])
		val result_decs = 
		    map2 (fn ((pat,_),slot) => 
			  Ast.ValDec([Ast.Vb{pat=Ast.AppPat{constr=Ast.VarPat[Symbol.varSymbol "SOME"],
							    argument=pat},
					     exp=app(deref_expr,var slot)}],ref [])) (pats_exps,refs)
		val decs = ref_decs @ thunk_decs @ spawn_decs @ [yield_dec] @ result_decs
	    in  xdec islocal (context, Ast.SeqDec decs)
	    end

	| Ast.FunDec (fblist,ref tyvars) => 
	    let val tyvar_stamp = get_stamp()
		val tyvars = map tyvar_strip tyvars
		local fun help tyvar = 
		    let val type_str = Symbol.name tyvar
			val type_lab = symbol_label tyvar
			val is_eq =  ((size type_str >= 2) andalso 
				      (String.substring(type_str,0,2) = "''"))
		    in  (type_lab,is_eq)
		    end
		    val temp = map help tyvars
		in  val sdecs1 = make_typearg_sdec temp
		end
	 
		val var_poly = fresh_named_var "var_poly"
		val open_lbl = to_open(internal_label "!opened")
		val context' = add_context_mod(context,open_lbl,var_poly,
					       SelfifySig context (PATH (var_poly,[]),
								   SIGNAT_STRUCTURE(NONE, sdecs1)))

		fun fb_help clause_list =
		    let 
			val fun_con = fresh_named_con (context',"fun_con")
			val body_con = fresh_named_con (context',"body_con")
			fun help (Ast.Clause{pats, resultty, exp}, nameopt) = 
			    let val _ = 
				(case resultty of
				     SOME ty => if (eq_con(context',xty(context',ty),body_con)) 
						    then ()
						else (error_region();
						      print "conflicting result type constraints\n")
				   | NONE => ())
				val (name, (p, e)) = 
				    (case pats of
					 {item=Ast.VarPat[s],fixity=NONE,...}::rest =>
					     (symbol_label s, 
					      (parse_pats context' (map #item rest),exp))
				       | _ => 
					 (case (parse_pats context' (map #item pats)) of
					      (Ast.VarPat[s])::rest => 
						  (symbol_label s, (rest, exp))
					    | (Ast.AppPat{constr = Ast.VarPat[s], argument}::rest) => 
						  (symbol_label s, (argument::rest,exp))
					    | _ => error "illegal pattern for function declaraion"))
				val _ = 
				    (case nameopt of
					 NONE => ()
				       | SOME curName => 
					     if eq_label(name,curName)
						 then ()
					     else (error_region();
						   print "clauses don't all have same name\n"))
			    in  ((p,e), SOME name)
			    end
			val (matches, SOME id) = foldl_acc help NONE clause_list
			val fun_var = fresh_named_var (label2string id)
		    in (id, fun_var, fun_con, body_con, matches)
		    end
		val dec_list : (label * var * con * con *
				(Ast.pat list * Ast.exp) list) list =
		    map (fb_help o fb_strip) fblist
	    in  xfundec islocal	(context,dec_list,tyvar_stamp,sdecs1,var_poly,open_lbl)
	    end


	| Ast.ExternDec (sym,ty) =>
	    let val var = gen_var_from_symbol sym
		val lab = symbol_label sym
		val con = xty(context,ty)
		val con = con_normalize(context,con)
	    in  [(NONE, CONTEXT_SDEC(SDEC(lab,DEC_EXP(var,con,NONE,false))))]
	    end
	| Ast.SeqDec decs => packagedecs (xdec islocal) context decs
	| Ast.OpenDec pathlist => 
	      let fun help (i,path) = 
		  (case (Context_Lookup_Labels(context,map symbol_label path)) of
		       SOME(_,PHRASE_CLASS_MOD(m,b,s)) => 
			   let 
			       val str = foldl (fn (s,acc) => acc ^ (Symbol.name s))
				            "openlbl" path
                               val l = to_open(internal_label str)
			       val v = fresh_named_var "openvar"
			       val s = (case mod2path m of
					    NONE => s
					  | SOME p => SIGNAT_OF p)
			   in  SOME(SOME (SBND(l,BND_MOD(v,false,m))), 
				    CONTEXT_SDEC(SDEC(l,DEC_MOD(v,false,s))))
			   end
		     | _ => (error_region(); print "unbound structure: ";
			     AstHelp.pp_path path; print "\n";
			     NONE))
	      in List.mapPartial (fn x => x) (mapcount help pathlist)
	      end
	| Ast.TypeDec tblist => 
	      let val typeresult = xtybind(context,tblist) 
		  fun make_eq_bnddec(l,c,k) = 
		      let val is_poly = (case k of
					     KIND_TUPLE _ => NONE
					   | KIND_ARROW(m,_) => SOME m)
			  val vp = fresh_named_var "varpoly"
			  val (ctxt',c',c'',sigpoly) = 
			      case is_poly of 
				  NONE => (context,c,c,SIGNAT_STRUCTURE(NONE,[]))
				| SOME m => 
				      let
					  val lbls = Listops.map0count canonical_tyvar_label m
					  fun mapper l = 
					      let val eql = to_eq l
						  val v = fresh_var()
						  val sdec1 = SDEC(l,DEC_CON(v,KIND_TUPLE 1, NONE, false))
						  val sdec2 = SDEC(eql,DEC_EXP(fresh_var(),
									       con_eqfun(CON_VAR v),
									       NONE, false))
					      in  [sdec1,sdec2]
					      end
					  val sdecs = List.concat (map mapper lbls)
					  val sp = SIGNAT_STRUCTURE(NONE,sdecs)
					  val ctxt' = add_context_dec(context,SelfifyDec context (DEC_MOD(vp,false,sp)))
					  val arg_cons = map (fn l => CON_MODULE_PROJECT(MOD_VAR vp,l)) lbls
					  val arg_con = con_tuple_inject arg_cons
					  val c' = CON_APP(c,arg_con)
					  val c'' = ConApply(true,c,arg_con)
				      in  (ctxt',c',c'',sp)
				      end
			  val eqlab = to_eq l
			  val eq_con = con_eqfun c''
		    in case (xeq(ctxt',c')) of
			SOME (eq_exp,_) =>
			    let
			  val v1 = fresh_var()
			  val (bnd,dec) = 
			      case is_poly of
				  NONE => (BND_EXP(v1, eq_exp), 
					   DEC_EXP(v1, eq_con, NONE, false))
				| SOME _ => 
				      let val v2 = fresh_var()
					  val inner_innersig = SIGNAT_STRUCTURE(NONE,
								    [SDEC(it_lab,
									  DEC_EXP(v2,eq_con,
										  NONE, false))])
					  val innermod =
					      MOD_FUNCTOR(TOTAL, vp,sigpoly,
							  MOD_STRUCTURE[SBND(it_lab,
									     BND_EXP(v2,eq_exp))],
							  inner_innersig)
					  val innersig = 
					      SIGNAT_FUNCTOR(vp,sigpoly,
							     inner_innersig,TOTAL)
					      
				      in  (BND_MOD(v1,true,innermod), DEC_MOD(v1,true, innersig))
				      end
			    in  [(SOME(SBND(eqlab,bnd)),CONTEXT_SDEC(SDEC(eqlab,dec)))]
			    end
			    | NONE => []
			end
		  val eqresult = 
		      case typeresult of
			  [(SOME(SBND(l,BND_CON(_,c))), CONTEXT_SDEC(SDEC(_,DEC_CON(_,k,_,_))))] =>
			      make_eq_bnddec(l,c,k)
			| _ => []
	      in  typeresult @ eqresult
	      end
	| Ast.DatatypeDec {datatycs,withtycs=[]} => 
	      let val sbnd_sdecs = xdatatype(context,datatycs)
	      in  map (fn (sb,sd) => (SOME sb, CONTEXT_SDEC sd)) sbnd_sdecs 
	      end
	| Ast.DatatypeDec {datatycs,withtycs} => 
	      let val (dt,wt) = (case InfixParse.parse_datbind(datatycs,withtycs) of
				     SOME result => result
				   | NONE => (error_region();
					      print "cannot parse datbind\n";
					      error "cannot parse datbind"))
		  val dec = Ast.SeqDec[Ast.DatatypeDec{datatycs=dt,withtycs=[]},
				       Ast.TypeDec wt]
	      in  xdec islocal (context,dec)
	      end
	| Ast.StrDec strblist => xstrbinds islocal (context,strblist) 
 	| Ast.FctDec fctblist => xfctbind(context,fctblist) 

	| Ast.ExceptionDec [] => parse_error "ExceptionDec []"
	| Ast.ExceptionDec [Ast.MarkEb (eb,r)] => 
	      let val _ = push_region r
		  val res = xdec islocal (context, Ast.ExceptionDec [eb])
		  val _ = pop_region()
	      in res
	      end
	| Ast.ExceptionDec [Ast.EbGen {exn,etype}] =>
		let 
		  val exn_str = Symbol.symbolToString exn
		  val id_bar = symbol_label exn
		  val var = fresh_named_var "exn_stamp"
		  val mkvar = fresh_named_var "mk"
		  val exnmodvar = fresh_named_var "exnmod"
		  val v = fresh_named_var "injectee"
		  val con = (case etype of
			       NONE => con_unit
			     | SOME ty => xty(context,ty))
		  val (mk_exp,mk_con) = 
		      (case etype of
			   NONE => (EXN_INJECT(exn_str,VAR var,unit_exp), CON_ANY)
			 | SOME ty => (#1 (make_total_lambda(v,con,CON_ANY,
							     EXN_INJECT(exn_str, VAR var,VAR v))),
				       CON_ARROW([con], CON_ANY, false, oneshot_init TOTAL)))
		  val inner_mod = MOD_STRUCTURE[SBND(stamp_lab, BND_EXP(var,NEW_STAMP con)),
						SBND(mk_lab, BND_EXP(mkvar,mk_exp))]
		  val inner_sig = SIGNAT_STRUCTURE(NONE,
						   [SDEC(stamp_lab,DEC_EXP(var,CON_TAG con,NONE,false)),
						    SDEC(mk_lab,DEC_EXP(mkvar,mk_con,NONE,false))])
		in [(SOME(SBND(id_bar,BND_MOD(exnmodvar,false,inner_mod))),
		     CONTEXT_SDEC(SDEC(id_bar,DEC_MOD(exnmodvar,false,inner_sig))))]
		end
	| Ast.ExceptionDec [Ast.EbDef {exn: Symbol.symbol, edef: Ast.path}] => 
	      (case (Context_Lookup_Labels(context,map symbol_label edef)) of
		   SOME(_,PHRASE_CLASS_MOD(m,_,s)) => 
		       let val id_bar = symbol_label exn
			   val path_mk_exp = MODULE_PROJECT(m,mk_lab)
			   val path_stamp_exp = MODULE_PROJECT(m,stamp_lab)
			   val path_mk_con = GetExpCon(context,path_mk_exp)
			   val path_stamp_con = GetExpCon(context,path_stamp_exp)
			   val itvar = fresh_named_var "exn_tag"
			   val mkvar = fresh_named_var "exn_injector"
			   val modvar = fresh_named_var "exn_structure"
			   val inner_mod = MOD_STRUCTURE[SBND(stamp_lab, BND_EXP(itvar,path_stamp_exp)),
							 SBND(mk_lab, BND_EXP(mkvar,path_mk_exp))]

			   val inner_sig = SIGNAT_STRUCTURE(NONE,
							    [SDEC(stamp_lab, DEC_EXP(itvar,path_stamp_con,NONE,false)),
							     SDEC(mk_lab, DEC_EXP(mkvar,path_mk_con,NONE,false))])
		       in [(SOME(SBND(id_bar,BND_MOD(modvar,false,inner_mod))),
			    CONTEXT_SDEC(SDEC(id_bar,DEC_MOD(modvar,false,inner_sig))))]
		       end
		 | _ => (error_region(); print "unbound exception: ???\n";
			 []))
	| Ast.ExceptionDec eblist => xdec islocal (context,Ast.SeqDec(map (fn eb => Ast.ExceptionDec [eb]) eblist))

        (* Rule 244 *)
	| Ast.LocalDec (dec1,dec2) => 
	      let 
		  val sbnd_ctxt_list1 = xdec true (context,dec1)
		  val (_,context') = add_context_sbnd_ctxts(context,sbnd_ctxt_list1)
		  val sbnd_ctxt_list2 = xdec false (context',dec2)
		  fun rename(opt,CONTEXT_SDEC(SDEC(l,dec))) = 
		      let val lbl = internal_label ("local_" ^ (IlUtil.label2name l))
			  val lbl = to_nonexport lbl
			  val ce' = CONTEXT_SDEC(SDEC(lbl,dec))
		      in case opt of
			  NONE => (NONE,ce')
			| SOME (SBND(_,bnd)) => (SOME (SBND(lbl,bnd)), ce')
		      end
		    | rename arg = arg
		  val sbnd_ctxt_list1 = map rename sbnd_ctxt_list1
	      in sbnd_ctxt_list1 @ sbnd_ctxt_list2
	      end

        (* Must augment translation context with fixity information *)
	| Ast.FixDec {fixity,ops} => let (* given symbol is in FixSymbol space *)
				       fun helper sym = 
					   let val sym' = Symbol.varSymbol(Symbol.name sym)
					       val lab = symbol_label sym'
					   in (NONE, CONTEXT_FIXITY (lab, fixity))
					   end
				     in map helper ops
				     end

	(* These cases are unhandled *)
	| Ast.SigDec [] => []
	| Ast.SigDec (sigb::rest) => 
	      let val ctxt : context_entry = xsigb(context,sigb)
		  (* CONTEXT_SIGNATs do not need to be selfified *)
		  val context' = add_context_entries(context,[ctxt])
		  val sbnd_ctxt_rest = xdec islocal (context',Ast.SigDec rest)
	      in (NONE,ctxt)::sbnd_ctxt_rest
	      end
	| Ast.AbstypeDec {abstycs,withtycs,body} => 
	      let val ldec = Ast.DatatypeDec{datatycs = abstycs, withtycs = withtycs}
		  fun get_dec(Ast.MarkDb(db,_)) = get_dec db
		    | get_dec(Ast.Db{tyc,tyvars,...}) = 
		      let val ty = Ast.ConTy([tyc],map Ast.VarTy tyvars)
		      in Ast.TypeDec[Ast.Tb{tyc=tyc,tyvars=tyvars,def=ty}]
		      end
		  val bdec = Ast.SeqDec((map get_dec abstycs) @ [body])
		  val desugared_dec = Ast.LocalDec(ldec,bdec)
	      in xdec islocal (context, desugared_dec)
	      end
	| Ast.FsigDec fsiglist => parse_error "functor signature declaration not handled"
	| Ast.AbsDec strblist => parse_error "abstract structure not handled"
	| Ast.OvldDec (sym,ignored_type,exp_list) =>
	      let val con_exp_list = map (fn e => let val (e,c,_) = xexp(context,e)
						  in (c,e)
						  end) exp_list
		  val l = symbol_label sym
		  val v = gen_var_from_symbol sym
		  val ce = CONTEXT_OVEREXP(l,v,con_exp_list)
	      in  [(NONE,ce)]
	      end
	| Ast.ImportDec strlist => parse_error "import declaration not handled"

        (* translate declaration by dropping region information *)
	| Ast.MarkDec (dec,region) => let val _ = push_region region
					  val res = xdec islocal (context,dec)
					  val _ = pop_region()
				      in res
				      end)

	    
    (* --------------------------------------------------------- 
      ------------------ TYPE EXPRESSIONS----------------------
      --------------------------------------------------------- *)
    and xty (context, ty) : con = 
      (case ty of
	 Ast.VarTy tyvar => 
	     let val sym = AstHelp.tyvar_strip tyvar
	     in (case (Context_Lookup_Label(context,symbol_label sym)) of
		     SOME(p,PHRASE_CLASS_CON (_,_,SOME inline_con,true)) => inline_con
		   | SOME(p,PHRASE_CLASS_CON (_,_,_,_)) => path2con p
		   | _ => (error_region(); print "unbound type constructor: ";
			   AstHelp.pp_sym sym; print "\n";
			   fresh_named_con(context,"unbound_type")))
	     end
       | Ast.MarkTy (ty,r) => 
	     let val _ = push_region r
		 val res = xty(context,ty)
		 val _ = pop_region()
	     in res
	     end
       | Ast.TupleTy(tys) => 
	     let fun loop _ [] = []
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
		 val con_list = map (fn t => xty(context,t)) ty_list
	     in
		 (case (Context_Lookup_Labels(context, map symbol_label syms)) of
		      SOME(path,PHRASE_CLASS_CON(_,k,copt,inline)) =>
			let val con = 
			    (case (copt,inline) of
			       (SOME c,true) => c
			     | _ => path2con path)
			in
			  (case (con_list,k) of
			      ([],KIND_TUPLE 1) => con
			    | (_,KIND_ARROW(n,1)) => 
				  if (n = length con_list) 
				      then ConApply(true,con,con_tuple_inject con_list)
				  else (error_region();
					tab_region();
					print "type constructor wants ";
					print (Int.toString n);
					print "arguments, given ";
					print (Int.toString (length con_list));
					fresh_named_con(context,"badarity_type"))
			     | _ => (pp_kind k; print "\nand c = "; 
				      pp_con con;
				      elab_error "external_label mapped to type with KIND_ARROW(_,!= 1)"))
			end
		    | _ => 
		      if (length syms = 1 andalso
			     Symbol.eq(hd syms, Symbol.tycSymbol "-->"))
		      then let fun split _ [] = error "need at least result type"
				 | split cons [c] = (rev cons,c)
			         | split cons (c::d) = split (c::cons) d
			       val (arg_cons,res_con) = split [] con_list
			   in  CON_ARROW(arg_cons,res_con,true,
					 oneshot_init PARTIAL)
			   end
		      else
			       (error_region();
			       print "unbound type constructor: ";
			       pp_pathlist AstHelp.pp_sym' syms; print "\n";
			       fresh_named_con(context,"unbound_type")))
	     end)

				  
    (* --------------------------------------------------------- 
      ------------------ TYPE DEFINITIONS ---------------------
      --------------------------------------------------------- *)
    and xtybind (context : context, tblist : Ast.tb list) : (sbnd option * context_entry) list =
       let 
	 fun doer tb = 
	   let 
	     val (tyc,tyvars,def) = tb_strip tb
	     val tyvars = map tyvar_strip tyvars
	     val vars = map (fn s => gen_var_from_symbol s) tyvars
	     val tyvars_bar = map (fn s => symbol_label s) tyvars
	     val context' = (foldl (fn ((v,tv),c) => 
				    add_context_con(c,tv,v,KIND_TUPLE 1,NONE))
			     context (zip vars tyvars_bar))
	     val con' = xty(context',def)
	     val n = length tyvars
	     val (con,kind) = (case tyvars of
				 [] => (con',KIND_TUPLE 1)
			       | _ => (CON_FUN(vars,con'),KIND_ARROW(n,1)))
	     val var = gen_var_from_symbol tyc
	     val tyc_bar = symbol_label tyc
	   in (SOME(SBND(tyc_bar,BND_CON(var,con))),
	       CONTEXT_SDEC(SDEC(tyc_bar,DEC_CON(var,kind,SOME con,false))))
	   end
       in map doer tblist
       end
     
    (* --------------------------------------------------------- 
      ------------------ TYPE DESCRIPTIONS ---------------------
      --------------------------------------------------------- *)
   and xtypedesc (context : context, sym_tyvar_tyopt_list, is_eq) : sdecs = 
       let 
	   fun loop [] = []
	     | loop ((sym,tyvars : Ast.tyvar list, tyopt)::rest) =
		   let 
		       val type_label = symbol_label sym
		       val type_var = gen_var_from_symbol sym
		       val eq_label = to_eq type_label
		       val eq_var = fresh_named_var (label2string eq_label)
		       val kind = 
			   (case tyvars of
				[] => KIND_TUPLE 1
			      | _ => KIND_ARROW(length tyvars,1))
		       fun doty ty =
			   let 
			       val vars = map (fn _ => fresh_var()) tyvars
			       val tyvars_bar = map (fn s => symbol_label (tyvar_strip s)) tyvars
			       val context' = (foldl (fn ((v,tv),c) => 
						      add_context_con(c,tv,v,KIND_TUPLE 1,NONE))
					       context (zip vars tyvars_bar))
			       val con' = xty(context',ty)
			   in  (case tyvars of
				    [] => con'
				  | _ => (CON_FUN(vars,con')))
			   end
		       val conopt = mapopt doty tyopt
		       val type_sdec = SDEC(type_label, DEC_CON(type_var,kind,conopt,false))
		       val eq_dec = 
			   (case tyvars of
				[] => DEC_EXP(eq_var,con_eqfun(CON_VAR type_var),NONE,false)
			      | _ => 
				let val vpoly = fresh_named_var "vpoly"
				    val lbls = Listops.map0count canonical_tyvar_label (length tyvars)
				    fun mapper l = 
					let val eql = to_eq l
					    val v = fresh_var()
					    val sdec1 = SDEC(l,DEC_CON(v,KIND_TUPLE 1, NONE,false))
					    val sdec2 = SDEC(eql,DEC_EXP(fresh_var(),
									 con_eqfun(CON_VAR v),
									 NONE, false))
					in  [sdec1,sdec2]
					end
				    val sdecs = List.concat (map mapper lbls)
				    val sigpoly = SIGNAT_STRUCTURE(NONE,sdecs)
				    val args = map (fn l => CON_MODULE_PROJECT(MOD_VAR vpoly,l)) lbls
				    val eq_con = con_eqfun(CON_APP(CON_VAR type_var, 
								   con_tuple_inject args))
				    val innersig = SIGNAT_STRUCTURE(NONE,
								    [SDEC(it_lab,
									  DEC_EXP(fresh_var(),
										  eq_con,NONE,false))])
				in  DEC_MOD(eq_var,true,SIGNAT_FUNCTOR(vpoly,sigpoly,innersig,TOTAL))
				end)
					    
		       val eq_sdec = SDEC(eq_label, eq_dec)
		   in if (is_eq) then type_sdec :: eq_sdec:: (loop rest)
		      else type_sdec :: (loop rest)
		   end
       in loop sym_tyvar_tyopt_list
       end

    (* --------------------------------------------------------- 
      ------------ Signature bindings and expressions ----------
      --------------------------------------------------------- *)
     and xsigexp(context,sigexp) : signat =
       (case sigexp of
	  Ast.VarSig s => (case (Context_Lookup_Label(context,symbol_label s)) of
			       SOME(_,PHRASE_CLASS_SIG (v,s)) => SIGNAT_VAR v
			     | _ => (error_region();
				     print "unbound signature: "; 
				     AstHelp.pp_sym s;
				     print "\n";
				     SIGNAT_STRUCTURE(NONE,[])))
	| Ast.SigSig speclist => SIGNAT_STRUCTURE(NONE,xspec(context,speclist))
	| Ast.MarkSig (s,r) => let val _ = push_region r
				   val res = xsigexp(context,s)
				   val _ = pop_region()
			       in res 
			       end
	| Ast.AugSig (s, []) => xsigexp(context,s)
	| Ast.AugSig (s, ((Ast.WhStruct (syms1,syms2))::rest)) => 
	      let val mjunk = MOD_VAR(fresh_named_var "mjunk")
		  val (_,m2,s2) = xstrexp(context,Ast.VarStr syms2, Ast.NoSig)
		  val s = xsigexp(context,Ast.AugSig(s,rest))
		  val s2_is_struct = (case reduce_signat context s2 of
					  SIGNAT_STRUCTURE _ => true
					| _ => false)
		  val popt_sdecs_opt = 
		      (case reduce_signat context s of
			   SIGNAT_STRUCTURE (popt,sdecs) => SOME(popt,sdecs)
			 | _ => NONE)
	      in  case (s2_is_struct,popt_sdecs_opt) of
		  (false, _) => (error_region();
				 print "rhs of where-structure is a non-structure\n";
				 s)
		| (_,NONE) => (error_region();
			       print "can't where a non-structure signature\n";
			       s)
		| (_, SOME(popt,sdecs)) =>
			   SIGNAT_STRUCTURE(popt,Signature.xsig_where_structure
					    (context,sdecs,map symbol_label syms1,m2,s2))
	      end
	| Ast.AugSig (s, ((Ast.WhType(syms, tyvars, ty))::rest)) =>
	      (case reduce_signat context (xsigexp(context,Ast.AugSig(s,rest))) of
		   s as SIGNAT_STRUCTURE (popt,sdecs) => 
		       let val mjunk = MOD_VAR(fresh_named_var "mjunk")
		       in (case (Sdecs_Lookup_Open context (mjunk,sdecs,map symbol_label syms)) of
			       SOME(labels,PHRASE_CLASS_CON(_,k,_,_)) => 
				   let val sym_vars = map (fn tv => 
							   let val sym = AstHelp.tyvar_strip tv
							   in  (sym, gen_var_from_symbol sym)
							   end) tyvars
				       fun folder ((sym,var),context) = 
				        add_context_sdec(context,SDEC(symbol_label sym, 
								      DEC_CON(var,
									      KIND_TUPLE 1, NONE,false)))
				       val context = foldl folder context sym_vars
				       val c = xty(context,ty)
				       val c = (case sym_vars of
						    [] => c
						  | _ => CON_FUN(map #2 sym_vars, c))
				   in SIGNAT_STRUCTURE(popt,Signature.xsig_where_type(context,sdecs,labels,c,k))
				   end
			     | _ => (error_region();
				     print "can't where type a non-type component";
				     s))
		       end
		 | s => (error_region();
			 print "can't where type a non-structure signature\n";
			 s)))


     and xsigb(context,Ast.MarkSigb(sigb,r)) : context_entry = 
	 let val _ = push_region r
	     val res = xsigb(context,sigb)
	     val _ = pop_region()
	 in res
	 end
       | xsigb(context,Ast.Sigb{name,def}) = 
	 let val v = gen_var_from_symbol name
	 in CONTEXT_SIGNAT(symbol_label name,v,
			   xsigexp(context,def))

	 end

    (* --------------------------------------------------------- 
      ------------------ SIGNATURE SPECIFICTIONS --------------
      --------------------------------------------------------- *)
     and xspec(orig_ctxt, specs : Ast.spec list) : sdecs = 
       let 
	 datatype sdecs_tag = ADDITIONAL of sdecs | ALL_NEW of sdecs
	 fun xspec1 context prev_sdecs spec : sdecs_tag =
	     case spec of
		 (Ast.ValSpec vtlist) => (* Rules 257 - 258 *)
	       let 
		 fun doer (sym,ty) = 
		   (case (free_tyvar_ty(ty,fn _ => false)) of
			[] => SDEC(symbol_label sym, 
				   DEC_EXP(gen_var_from_symbol sym,xty(context,ty),NONE,false))
		    | ftv_sym => 
			let 
			    val varpoly = fresh_named_var "var_poly"
			    fun help (n,tv_sym) = 
				let val type_lab = symbol_label tv_sym
				    val eq_lab = to_eq type_lab
				    val type_var = fresh_var()
				    val eq_var = fresh_var()
				    val type_str = Symbol.name tv_sym
				    val is_eq =  ((size type_str >= 2) andalso 
						  (String.substring(type_str,0,2) = "''"))
				    val eq_con = con_eqfun(CON_VAR type_var)
				    val type_sdec = SDEC(type_lab,DEC_CON(type_var,KIND_TUPLE 1, 
									  NONE,false))
				    val eq_sdec = SDEC(eq_lab, DEC_EXP(eq_var,eq_con,NONE,false))
				in if is_eq
				    then [type_sdec,eq_sdec]
				   else [type_sdec]
				end
			    val sigpoly = SIGNAT_STRUCTURE(NONE,flatten(mapcount help ftv_sym))
			    val context' = add_context_mod(context,
							      to_open(internal_label "opened"),
							      varpoly, 
							      SelfifySig context (PATH (varpoly,[]),sigpoly))
			    val con = xty(context',ty)
			    val fsig = SIGNAT_FUNCTOR(varpoly,sigpoly,
						      SIGNAT_STRUCTURE(NONE,
								       [SDEC(it_lab,
									     DEC_EXP(fresh_var(),
										     con,NONE,false))]),
						      TOTAL)
			in SDEC(symbol_label sym, DEC_MOD(fresh_named_var "unused",true,fsig))
			end)
	       in ADDITIONAL(map doer vtlist )
	       end
	   | (Ast.StrSpec (sym_sigexp_path_list)) =>
	       let 
		   fun doer(sym,SOME sigexp, _) = 
		       let val s = xsigexp(context,sigexp)
		       in SDEC(symbol_label sym,DEC_MOD(fresh_var(),false,s))
		       end
		     | doer(sym,_,_) = error "structure path spec not imped"
	       in ADDITIONAL(map doer sym_sigexp_path_list)
	       end
	   | (Ast.IncludeSpec sym) =>
	       (case (reduce_signat context (xsigexp(context,Ast.VarSig sym))) of
		  SIGNAT_STRUCTURE (NONE,sdecs) => ADDITIONAL sdecs
		| SIGNAT_STRUCTURE (SOME _, _) => elab_error "xsigexp compiled to selfified signature"
		| _ => elab_error "xsigexp compiled to non-structure signature")
	   | (Ast.FctSpec sym_fsigexp_list) =>
		  let 
		    fun doer (funid,fsig) = 
		      let 
			val var = fresh_var()
			fun help (Ast.VarFsig _) = parse_error "Ast.VarFsig encountered"
			  | help (Ast.MarkFsig (fs,r)) = let val _ = push_region r
							     val res = help fs
							     val _ = pop_region ()
							 in res
							 end
			  | help (Ast.FsigFsig {param,def=sigexp'}) = 
			     let (* this is a derived form *)
			       val strid = functor_arg_lab
			       val sym_sigexp_path_list = 
				   (map (fn (SOME s,se) => (s,SOME se,NONE)
				          | (NONE, _) => elab_error "functor arg unnamed")
				    param)
			       val sigexp = Ast.SigSig[Ast.StrSpec sym_sigexp_path_list]
			       val signat = xsigexp(context,sigexp)
			       val context' = add_context_mod(context,strid,var,
								 SelfifySig context (PATH (var,[]),signat))
			       val signat' = xsigexp(context',sigexp')
			     in SIGNAT_FUNCTOR(var,signat,signat',PARTIAL)
			     end
		      in SDEC(symbol_label funid, DEC_MOD(var,false,help fsig))
		      end
		  in ADDITIONAL(map doer sym_fsigexp_list)
		  end
	   | (Ast.TycSpec (typdesc_list,is_eq)) => ADDITIONAL(xtypedesc(context,typdesc_list,is_eq))
	   | (Ast.ExceSpec exlist) => (* Rules 260 - 261 *)
		      let fun doer (sym,tyopt) = 
			let val (mk_con,stamp_con) = 
			  (case tyopt of
			     NONE => (CON_ANY, CON_TAG con_unit)
			   | (SOME ty) => let val con = xty(context,ty)
					  in (CON_ARROW([con],CON_ANY,false,oneshot_init TOTAL),
					      CON_TAG con)
					  end)
			    val inner_sig = 
			      SIGNAT_STRUCTURE(NONE,
					       [SDEC(stamp_lab,
						    DEC_EXP(fresh_var(),stamp_con,NONE,false)),
					       SDEC(mk_lab,
						    DEC_EXP(fresh_var(),mk_con,NONE,false))])
			in SDEC(symbol_label sym, DEC_MOD(fresh_var(),false,inner_sig))
			end
		      in ADDITIONAL(map doer exlist)
		      end
	   | (Ast.DataSpec {datatycs, withtycs = []}) =>
	        let val sbnd_sdecs = xdatatype(context,datatycs)
		    val sdecs = map #2 sbnd_sdecs
		in  ADDITIONAL sdecs
		end
	   | (Ast.ShareTycSpec paths) => ALL_NEW(Signature.xsig_sharing_types
						 (context,prev_sdecs,mapmap symbol_label paths))
	   | (Ast.ShareStrSpec paths) => ALL_NEW(Signature.xsig_sharing_structures
						 (context,prev_sdecs,mapmap symbol_label paths))
	   | (Ast.FixSpec _) => parse_error "fixity specs not supported"
	   | (Ast.MarkSpec (s,r)) => let val _ = push_region r
					 val res = xspec1 context prev_sdecs s
					 val _ = pop_region ()
				     in res
				     end
         fun loop ctxt prev_sdecs [] = prev_sdecs
           | loop ctxt prev_sdecs ((Ast.DataSpec{datatycs,withtycs=wt as (_::_)})::specrest) =
		let val (dt,wt) = (case InfixParse.parse_datbind(datatycs,wt) of
				       SOME result => result
				     | NONE => (error_region();
						print "cannot parse datspec\n";
						error "cannot parse datspec"))
		    val dspec = Ast.DataSpec{datatycs=dt,withtycs=[]}
		    fun strip tb = let val (s,tvs,ty) = AstHelp.tb_strip tb
				   in  (s,tvs,SOME ty) 
				   end
		    val wspec = Ast.TycSpec (map strip wt,false)
		in  loop ctxt prev_sdecs (dspec::wspec::specrest)
		end
           | loop ctxt prev_sdecs (spec::specrest) =
	     (case (xspec1 ctxt prev_sdecs spec) of
		  ADDITIONAL sdecs' =>
		      let val sdecs'' = map (fn (SDEC(l,dec)) => SDEC(l,SelfifyDec ctxt dec)) sdecs'
			  val ctxt' = add_context_sdecs(ctxt,sdecs'')
		      in loop ctxt' (prev_sdecs @ sdecs') specrest
		      end
		| ALL_NEW sdecs' => 
		      let val sdecs'' = map (fn (SDEC(l,dec)) => SDEC(l,SelfifyDec orig_ctxt dec)) sdecs'
			  val ctxt' = add_context_sdecs(orig_ctxt,sdecs'')
		      in loop ctxt' sdecs' specrest
		      end)
       in loop orig_ctxt [] specs
       end


    (* --------------------------------------------------------- 
      ------------------ FUNCTOR BINDINDS ---------------------
      --------------------------------------------------------- *)
     and xfctbind (context : context, fctbs : Ast.fctb list) : (sbnd option * context_entry) list =
	 let 
	     fun help (context,(name,def)) : (sbnd option * context_entry) list = 
		 (case def of
		      (Ast.VarFct (path,Ast.NoSig)) => 
			  (case (Context_Lookup_Labels(context,map symbol_label path)) of
			       SOME(path,PHRASE_CLASS_MOD(m,_,s as (SIGNAT_FUNCTOR _))) => 
				   let val l = symbol_label name
				       val v = fresh_named_var "functor_var"
				   in [(SOME(SBND(l,BND_MOD(v,false,m))),
					CONTEXT_SDEC(SDEC(l,DEC_MOD(v,false,s))))]
				   end
			     | _ => (error_region();
				    print "unbound functor: ";
				    AstHelp.pp_path path;
				    print "\n"; []))
		    | (Ast.VarFct (path,_)) => parse_error "functor signatures not handled"
		    | (Ast.FctFct {params=[(argnameopt,sigexp)],body,constraint}) =>
			  let 
			      val arglabel = (case argnameopt of
						  NONE => to_open(internal_label "FunctorArg")
						| SOME s => symbol_label s)
			      val funid = symbol_label name
			      val argvar = fresh_named_var "funct_arg"
			      val signat = xsigexp(context,sigexp)
			      val self_signat = SelfifySig context (PATH (argvar,[]), signat)
			      val context' = add_context_mod(context,arglabel,argvar, self_signat)
			      val (sbnd_ce_list,m',s') = xstrexp(context',body,constraint)
			      val v = fresh_named_var "functor_var"
			      val sbnd = SBND(funid,BND_MOD(v,false,MOD_FUNCTOR(PARTIAL, argvar,signat,m',s')))
			      val sdec = SDEC(funid,DEC_MOD(v,false,SIGNAT_FUNCTOR(argvar,signat,s',
									     PARTIAL)))
			  in sbnd_ce_list @ [(SOME sbnd, CONTEXT_SDEC sdec)]
			  end
		    | (Ast.FctFct {params=[],body,constraint}) => parse_error "Functor of order 0"
		    | (Ast.FctFct _) => parse_error "No higher order functors"
		    | (Ast.LetFct _) => parse_error "No lets in functor bindings"
		    | (Ast.AppFct _) => parse_error "No higher order functors"
		    | (Ast.MarkFct (f,r)) => let val _ = push_region r
						 val res = help (context,(name,f))
						 val _ = pop_region()
					     in res
					     end)
	 in  packagedecs help context (map fctb_strip fctbs)
	 end

    (* --------------------------------------------------------- 
      ------------------ STRUCTURE EXPRESSION -----------------
      --------------------------------------------------------- *)

     and xstrexp (context : context, strb : Ast.strexp,  Ast.Opaque sigexp) 
	 : (decresult * mod * signat) = 
	 let 
	     val sig_target = xsigexp(context,sigexp)
	     val (sbnd_ce_list,module,sig_actual) = xstrexp(context,strb,Ast.NoSig)
	     val (_,context) = add_context_sbnd_ctxts(context,sbnd_ce_list)
	 in  if (Sig_IsSub(context, sig_actual, sig_target))
		 then (sbnd_ce_list,module, sig_target)
	     else let val mod_result = Signature.xcoerce_seal(polyinst,context,
							      module,sig_actual,sig_target)
		  in  (sbnd_ce_list, mod_result, sig_target)
		  end
	 end
      | xstrexp (context, strb, Ast.Transparent sigexp) = 
	let 
	    val (sbnd_ce_list,module,signat) = xstrexp(context,strb,Ast.NoSig)
	    val (_,context') = add_context_sbnd_ctxts(context,sbnd_ce_list)
	    val sig' = xsigexp(context,sigexp)
	    val (mod_result,sig_result) =
		Signature.xcoerce_transparent (polyinst,context',module,signat,sig')
	in  (sbnd_ce_list, mod_result, sig_result)
	end

     | xstrexp (context, strb, Ast.NoSig) =
	(case strb of
	     Ast.VarStr path => 
		 (case Context_Lookup_Labels(context,map symbol_label path) of
		      SOME (_,PHRASE_CLASS_MOD(m,_,s)) =>
			  (case reduce_signat context s of
			       (SIGNAT_STRUCTURE _) => ([],m,s)
			     | _ => (error_region();
				     print "binding a non-structure module: ";
				     AstHelp.pp_path path;
				     print "\n";
				     ([],MOD_STRUCTURE[],SIGNAT_STRUCTURE(NONE,[]))))
		    | _ => (error_region();
			    print "unbound structure: ";
			    AstHelp.pp_path path;
			    print "\n";
			    ([],MOD_STRUCTURE[],SIGNAT_STRUCTURE(NONE,[]))))
	   | Ast.AppStr (_,[]) => parse_error "AppStr with no arguments"
	   | Ast.AppStr (f,[(Ast.MarkStr (se,r),flag)]) =>
		 xstrexp(context, Ast.AppStr(f,[(se,flag)]), Ast.NoSig)
	   | Ast.AppStr (funpath,[(strexp as (Ast.VarStr [var]),_)]) =>
		 (case (Context_Lookup_Labels(context,map symbol_label funpath)) of
		      SOME(_,PHRASE_CLASS_MOD(m,_,s as (SIGNAT_FUNCTOR(var1,sig1,sig2,_)))) => 
			  let 
			      val (sbnd_ce_list,argmod,signat) = xstrexp(context,strexp,Ast.NoSig)
			      val argpath = 
				  (case (mod2path argmod) of
				       SOME p => p
				     | _ => elab_error "xstrexp: functor argument became non-variable")
			      val varName = Symbol.name var
			      val coerced_lbl = internal_label ("!coerced_" ^ varName)
			      val coerced_var = fresh_named_var ("coerced_" ^ varName)

			      val (modc_body,sig1') = 
				  Signature.xcoerce_functor(polyinst,context,argpath,signat,sig1)
			      val sealed = MOD_SEAL(modc_body,sig1')
			      val coerced_sbnd_ce = 
				  (SOME(SBND(coerced_lbl,BND_MOD(coerced_var,false,sealed))),
				   CONTEXT_SDEC(SDEC(coerced_lbl,DEC_MOD(coerced_var,false,sig1'))))
			      val sig2' = sig_subst(sig2,subst_add_modvar(empty_subst,
									  var1,MOD_VAR coerced_var))
			  in (sbnd_ce_list @ [coerced_sbnd_ce],
			      MOD_APP(m,MOD_VAR coerced_var),
			      sig2')
			  end
	            | SOME _ => (error_region();
			    print "cannot apply a non-functor\n";
			    ([],MOD_STRUCTURE[],SIGNAT_STRUCTURE(NONE,[])))
	            | NONE => (error_region();
			    print "identifier not bound";
			       AstHelp.pp_path funpath;
			       print "\n";
			    ([],MOD_STRUCTURE[],SIGNAT_STRUCTURE(NONE,[]))))
	   | Ast.AppStr (_,[(strexp,_)]) => parse_error "AppStr applied to a non-path: we should be in named form"
	   | Ast.AppStr (_,_) => (error_region();
				  print "higher order functors not supported\n";
				  ([],MOD_STRUCTURE[],SIGNAT_STRUCTURE(NONE,[])))
	   | Ast.LetStr (dec,strexp) => (* rule 254 *) 
		 let val var1 = fresh_var()
		     val var2 = fresh_var()
		     val lbl1 = to_open(internal_label "LetStr1")
		     val lbl2 = to_open(internal_label "LetStr2")
		     val sbnd_sdec_list = xdec true (context,dec)
		     val (mod1,sig1) = sbnd_ctxt_list2modsig sbnd_sdec_list
		     val context' = add_context_mod(context,lbl1,var1,
						    SelfifySig context (PATH (var1,[]), sig1)) (* <-- inline ? *)
		     val (sbnd_ce_list,mod2,sig2) = xstrexp(context',strexp,Ast.NoSig)
		     val final_mod = MOD_STRUCTURE [SBND(lbl1,BND_MOD(var1,false,mod1)),
						    SBND(lbl2,BND_MOD(var2,false,mod2))]
		     val final_sig = SIGNAT_STRUCTURE(NONE,[SDEC(lbl1,DEC_MOD(var1,false,sig1)),
							    SDEC(lbl2,DEC_MOD(var2,false,sig2))])

		 in (sbnd_ce_list,final_mod, final_sig)
		 end
	   | Ast.StructStr dec => 
		 let 
		     val sbnd_ctxt_list = xdec false (context,dec)
		     val sbnds = List.mapPartial #1 sbnd_ctxt_list
		     val sdecs = List.mapPartial (fn (_,CONTEXT_SDEC sdec) => SOME sdec
		   | _ => NONE) sbnd_ctxt_list
		 in ([], MOD_STRUCTURE sbnds, SIGNAT_STRUCTURE(NONE, sdecs))
		 end
	   | Ast.MarkStr (strexp,r) => let val _ = push_region r
					   val res = xstrexp(context,strexp,Ast.NoSig)
					   val _ = pop_region()
				       in res
				       end)
	     
	     

    (* --------------------------------------------------------- 
      ------------------ STRUCTURE BINDINGS --------------------
      --------------------------------------------------------- *)
     and xstrbinds (islocal : bool) (context : context, strbs : Ast.strb list) 
	 : (sbnd option * context_entry) list =
       let val strbs = map strb_strip strbs
	   fun help (n,(strexp,constraint)) = 
	       let val v = fresh_named_var "strbindvar"
		   val l = symbol_label n
		   val (sbnd_ce_list,m,s) = xstrexp(context,strexp,constraint)
		   val rest = [(SOME(SBND(l,BND_MOD(v,false,m))),
				CONTEXT_SDEC(SDEC(l,DEC_MOD(v,false,s))))]
	       in  sbnd_ce_list @ rest
	       end
       in flatten(map help strbs)
       end



    and xeq (ctxt : context, argcon : con) : (exp * con) option = 
	let 
	    fun vector_eq ctxt = 
	    let val (e,vc,_) = xexp(ctxt,Ast.VarExp[Symbol.varSymbol "vector_eq"])
	    in  (e,vc)
	    end
	in  Equal.compile{polyinst_opt = polyinst_opt,
			  vector_eq = vector_eq,
			  context = ctxt,
			  con = argcon}
	end



    (* ------------ Exported interface to resolve overloading ------------ *)
    fun overload_wrap fp xobj arg = 
      let 
	fun flex_help (l,ref(FLEXINFO(stamp,false,rdecs)),fieldc,eshot) = 
	    (error_region();
	     print "Unresolved flex record with label: ";
	     pp_label l; print "\nrdecs are:";
	     pp_con (CON_RECORD rdecs); print "\nfieldc is:";
	     pp_con fieldc; print "\n")
	  | flex_help (l,ref(FLEXINFO(_,true,rdecs)),fieldc,eshot) = 
	    let val v = fresh_named_var "flex_eta_var"
		val recc = CON_RECORD rdecs
		val body = RECORD_PROJECT(VAR v,l,recc)
		val (e,c) = make_lambda(v,recc,fieldc,body)
	    in oneshot_set(eshot,e)
	    end
	  | flex_help (l,ref(INDIRECT_FLEXINFO fr),fieldc,eshot) = flex_help (l,fr,fieldc,eshot)
	fun eq_help (reg,decs,tyvar,exp_oneshot) = 
	  let val _ = push_region reg
	      val con = CON_TYVAR tyvar
	      val res = (case xeq(decs,con) of
			     SOME (e,_) => oneshot_set(exp_oneshot,e)
			   | NONE => (error_region();
				      print "no equality at type: ";
				      pp_con con; print "\n"))
	      val _ = pop_region()
	  in res
	  end

	fun tyvar_help tv = 
	    (case (Tyvar.tyvar_deref tv) of
		 SOME _ => ()
	       | NONE => (print "Warning: top-level unresolved tyvar -- setting to type unit\n"; 
			  Stats.counter "toil.unresolved_tyvar" ();
			  Tyvar.tyvar_set(tv,con_unit)))
			
	fun overload_loop warn overload_table = 
	    let fun folder (entry,(rest,change)) = 
		if (overload_help false entry)
		    then (rest,true)
		else (entry::rest, change)
		val (rest,change) = foldl folder ([],warn) overload_table
	    in  if warn
		    then ()
		else (if change 
			  then overload_loop warn rest
		      else overload_loop true rest)
	    end

	val _ = reset_elaboration fp
	val _ = push_region(0,1000000)
	val _ = eq_table_push()
	val res = xobj arg
	val _ = eq_table_pop (DEC_MOD(fresh_named_var "dummy", false, SIGNAT_STRUCTURE(NONE,[])))
        val tyvar_table = get_tyvar_table()
	val overload_table = get_overload_table()
	val flex_table = get_flex_table()
        val _ = overload_loop false overload_table 
	val _ = app flex_help flex_table 
        val _ = app tyvar_help tyvar_table

	fun eq_loop 10 = elab_error "eq_loop reached 10"
	  | eq_loop n = 
	    let val eq_table = get_eq_table()
	    in case eq_table of
		[] => ()
	      | _ => let val _ = debugdo (fn() =>
					  (print "eq_table had "; print (Int.toString (length eq_table));
					   print " things\n"))
			 val _ = reset_eq()
			 val _ = eq_table_push()
			 val _ = app eq_help eq_table 
			 val _ = eq_table_pop (DEC_MOD(fresh_named_var "dummy", false,
						       SIGNAT_STRUCTURE(NONE,[])))
		     in eq_loop (n+1)
		     end
	    end
	val _ = eq_loop 1
	val result = (case (get_error()) of
			  NoError => SOME res
			| Warn => SOME res
			| Error => NONE)
	val _ = reset_elaboration (Error.nofilepos)
      in result
      end


    val expcompile = xexp
    val typecompile = xty

    val xdec = fn (ctxt,fp,dec) => overload_wrap fp (xdec false) (ctxt,dec)
    val xexp = fn (ctxt,fp,exp) => overload_wrap fp xexp (ctxt,exp)
    val xstrexp = fn (ctxt,fp,strexp,sigc) => overload_wrap fp xstrexp (ctxt,strexp,sigc)
    val xspec = fn (ctxt,fp,specs) => overload_wrap fp xspec (ctxt,specs)
    val xsigexp = fn (ctxt,fp,se) => overload_wrap fp xsigexp (ctxt,se)
    val xty = fn (ctxt,fp,ty) => overload_wrap fp xty (ctxt,ty)
    val xtybind = fn (ctxt,fp,tyb) => overload_wrap fp xtybind (ctxt,tyb)


  end;



