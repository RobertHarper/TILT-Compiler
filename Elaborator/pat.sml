functor Pat(structure Il : IL
	    structure IlStatic : ILSTATIC
	    structure IlUtil : ILUTIL
	    structure Ppil : PPIL
	    structure InfixParse : INFIXPARSE
	    structure AstHelp : ASTHELP
	    structure Datatype : DATATYPE
	    structure IlLookup : ILLOOKUP
	    sharing IlLookup.Il = InfixParse.Il = Ppil.Il = IlUtil.Il = IlStatic.Il = Datatype.Il = Il)
  : PAT =
  struct
    
    type modsig_lookup = Il.context * Il.label list -> (Il.path * Il.mod * Il.signat) option
    type polyinst = Il.decs * Il.sdecs -> Il.sbnd list * Il.sdecs * Il.con list 
    type typecompile = Il.context * Ast.ty -> Il.con
    type expcompile = Il.context * Ast.exp -> Il.exp * Il.con
    type patarg = {context : Il.context,
		   typecompile : typecompile,
		   expcompile : expcompile,
		   polyinst : polyinst}


    open Il IlStatic IlUtil Ppil
    open Util Name IlLookup Prim

    val error = error "pat.sml"
    val debug = ref false
    fun debugdo t = if (!debug) then (t(); ()) else ()

    (* ---------- auxilliary types, datatypes, and functions -------------- 
     A case_exp is the argument to the case statement.  It distinguishes
       symbols/paths so that they do not have to rebound and so we can use
       nice names.  The difference between symbol and a path is that
       paths are "in context" already since they project from surrounding
       modules.  Thus, context does not need to be extended for paths.
     A clause is a list of patterns, corresponding to curried arguments.
     A subst is a symbol to path substition on an AST.
     A delayed expression is a triple.  It can be compiled
        by taking the AST expression, performaing the substituion,
	 and then compiled in a context augemented by the context entries.
     A def is the default argument taken if a match is non-exhaustive.
        If the def is NONE, then a bind exception is raised.
     An arm represents a rule to be compiled.  It contains the patterns
        that must be matched, the list of variables/types that are already
        bound, and the body itself.
     --------------------------------------------------------------------- *)
    datatype case_exp = CASE_NONVAR of Il.var * Il.exp * Il.con
                      | CASE_VAR    of Il.var * Il.con
    fun get_case_exp_con(CASE_VAR(_,c)) = c
      | get_case_exp_con(CASE_NONVAR(_,_,c)) = c
    fun get_case_exp_exp(CASE_VAR(v,_)) = VAR v
      | get_case_exp_exp(CASE_NONVAR(_,e,_)) = e



    type clause = Ast.pat list
    type def =  (Il.exp * Il.con) option
    type delay_exp = Il.context_entry list * Ast.exp
    type bound = (Symbol.symbol * var * con) list 
    type arm = clause * bound * Ast.exp option

    fun arm2context_entries(_,svc_list,_) = map (fn (s,v,c) => CONTEXT_VAR(symbol2label s,v,c)) svc_list
    fun arm_addbind(s,v,c,(cl,svc_list,expopt) : arm) = (cl,(s,v,c)::svc_list,expopt)
    fun is_constr (modsig_lookup, context) (p : Ast.path) = 
      (case Datatype.constr_lookup context p of
	 SOME _ => true
       | NONE => false)

    (* ----- creates a let binding construct ----------------------------------
     It returns 
     (1) a bnd option that should(if present) should wrap the exp to be bound
     (2) a var/type pair (with externally available symbol)
           to augment the arm with so that eventual
           compilation will occur in the right context.
	   If the let was created for case_exps, then the augmentation is unneeded.
     ------------------------------------------------------------------------ *)

    fun lethelp (str : string, arg : case_exp)
      : Il.bnd option * (Il.var * Il.con) = 
       (case arg of
	  CASE_VAR(var,con) => (NONE,(var,con))
	| CASE_NONVAR(var,exp,con) => (SOME(BND_EXP(var,exp)),(var,con)))
    fun wrapbnd (NONE : bnd option, (e : exp, c : con)) = (e,c)
      | wrapbnd (SOME b, (e,c)) = (LET ([b], e),c)
    fun wrapbnds ([] : bnds, (e : exp, c : con)) = (e,c)
      | wrapbnds (bnds, (e,c)) = (LET (bnds, e),c)


    fun letprojecthelp (rv : Il.var, recsyms : Symbol.symbol list)
      : Il.bnds * Il.var list * Il.con list = 
      let 
	val labels = map symbol2label recsyms
	val vars = map (fn _ => fresh_var()) labels
	val cons = map (fn _ => fresh_con()) labels
	val rcon = CON_RECORD(map2 (fn (l,c) => RDEC(l,c)) (labels, cons))
	val bnds = map2 (fn (v,l) => BND_EXP(v,RECORD_PROJECT(VAR rv,l,rcon))) (vars,labels)
      in (bnds,vars,cons)
      end


    (* ------------ file specific pretty-printed stuff  ------------------ *)
    fun pp_path_patopt (path,patopt) = (print "   path = "; AstHelp.pp_path path;
					(case patopt of 
					   NONE => ()
					 | SOME p => (print "   pat = "; AstHelp.pp_pat p; ())))
    fun pp_case_exp (CASE_VAR (v,c)) = (print "CASE_VAR("; pp_var v; print ", "; pp_con c;
					print ")")
      | pp_case_exp (CASE_NONVAR (v,e,c)) = (print "CASE_NONVAR("; pp_var v; print ", "; 
					     pp_exp e; print ", "; pp_con c;print ")")
    fun pp_def NONE = print "NONE"
      | pp_def (SOME (e,c)) = (print "SOME = "; pp_exp e)
    fun pp_arm ((cl, bound, body) : arm) = (print "arm has clause: "; 
					    map (fn p => (AstHelp.pp_pat p; print "   ")) cl;
					    print "\n    and bound: ";
					    map (fn (s,v,c) => (print "("; AstHelp.pp_sym s; print ", ";
								pp_var v; print ","; 
								pp_con c; print ")  ")) bound;
					    print "\n    and body: ";
					    (case body of 
					       NONE => ()
					     | SOME e => AstHelp.pp_exp e);
					     print "\n")

  
  (* ------------- MAIN ROUTINE --------------------------------------
   compile takes a list of arguments the entire match is applied to
   and a list match-rules, each of which consists of
   a list of (pattern list * bound var * Ast.exp) that represents pattern 
   sequence for that arm. The option denotes the to-be-compiled body 
   of the rule if the match succeeds.
     If the option is NONE, the return value should be a tuple of the 
     variables bound by that pattern.  If there is only one variable,
     then it is NOT tupled up.  To be well-typed, 
     the option can be NONE only if there is one match-rule.
     
     The result is a case expression with its type.  
     Also returned is a list, one for each match-rule supplied, 
     of the variables and their types bound in each pattern.
    -------------------------------------------------------------- *)
 fun compile ({context, typecompile = xty, expcompile = xexp, polyinst} : patarg,
	       compile_args : case_exp list, 
	       compile_arms : arm list,
	       compile_default : (Il.exp * Il.con) option)
     : (exp * con * (Symbol.symbol * con) list list) = 
    let

      val _ = debugdo (fn () => print "pat.sml: main compile called\n")
 (* ------------------------------------------------------------------------------
    various cases to handle certain collected pattern types 
    --------------------------------------------------------------------------- *)
(*
  fun nil_case(arg1,args,
	       acc : (unit * arm) list,
	       def : def) : bound list * (exp * con) = 
    let
      val elemcon = fresh_con()
      val _ = con_unify(context,"nil_case",
			get_case_exp_con arg1, "argcon",
			CON_LIST elemcon, "elemcon")
      val test = APP(PRIM (PRIM1(ISNILprim{instance=elemcon})), get_case_exp_exp arg1)
      val (vc_ll, (thencase,tc)) = match(args,map #2 acc,def)
      val (elsecase,ec) = (case def of
			     SOME (e,c) => (e,c)
			   | NONE => (Il.RAISE bindexn_exp, fresh_con()))
      val _ = con_unify(context,"nil_case",tc,"tc",ec,"ec")
    in (vc_ll, (make_ifthenelse(test,thencase,elsecase), ec))
    end

  and cons_case(arg1,args,
		acc : ((Ast.pat * Ast.pat) * arm) list,
		def : def) : bound list * (exp * con) = 
    let
      val elemcon = fresh_con()
      val (bnd,(var,con)) = lethelp("consvar",arg1)
      val _ = con_unify(context,"cons_case",
			con, "argcon",
			CON_LIST elemcon, "elemcon")
      val test = APP(PRIM(PRIM1 (ISNILprim{instance=elemcon})), VAR var)
      val (thencase,tc) = (case def of
			     SOME (e,c) => (e,c)
			   | NONE => (Il.RAISE bindexn_exp, fresh_con()))
      val (v1,v2) = (fresh_var(), fresh_var())
      val newargs = ((CASE_NONVAR(v1, APP(PRIM (PRIM1 (CARprim{instance=elemcon})), VAR var), elemcon))
		     :: (CASE_NONVAR(v2, APP(PRIM (PRIM1 (CDRprim{instance=elemcon})), VAR var), 
				     CON_LIST elemcon)) ::
		     args)
      val newarms = map (fn ((p1,p2),(c,b,eopt)) => (p1::p2::c,b,eopt)) acc
      val (vc_ll, (elsecase,ec)) = match(newargs,newarms,def)
      val _ = con_unify(context,"nil_case",tc,"tc",ec,"ec")
    in (vc_ll, (make_ifthenelse(test,thencase,elsecase), ec))
    end
*)
  fun ref_case(arg1,args,
	       acc : (Ast.pat * arm) list,
	       def : def) : bound list * (exp * con) = 
    let
      val elemcon = fresh_con()
      val (bnd,(var,con)) = lethelp("refvar",arg1)
      val _ = con_unify(context,"ref_case",
			con, "refcon",
			CON_REF elemcon, "elemcon")
      val newargs = (CASE_NONVAR(fresh_var(),GET (elemcon,VAR var), elemcon))::args
      val newarms = map (fn (p,(c,b,eopt)) => (p::c,b,eopt)) acc
    in match(newargs,newarms,def)
    end

  and var_case(arg1,
	       args,
	       accs : (Ast.symbol * arm) list,
	       def : def) : bound list * (exp * con) = 
    (case (args,accs) of
           (* if no variable is yet bound, this is the last pattern,
            and this is a binding construct, then the symbol
	      matches the expression, and the bound list will be a 
	      singleton of that expression. *)
       ([],[(s,([],[],NONE))]) => (case arg1 of
				     CASE_NONVAR (v,e,c) => ([[(s,v,c)]],(e,c))
				   | CASE_VAR(v,c) => ([[(s,v,c)]],(VAR v, c)))
     | _ =>
	 let 
	   val con1 = get_case_exp_con arg1
	   val (bnd,(var,con)) = lethelp ("casevar",arg1)
	   fun extender (s, arm) = arm_addbind(s,var,con,arm)
	   val newarms = map extender accs
	   val (vc_ll : bound list, ec) = match(args, newarms, def)
	 in (vc_ll, wrapbnd(bnd,ec))
	 end)

  and tuprec_case (arg1,
		   args,
		   syms : Symbol.symbol list,
		   accs : (Ast.pat list * arm) list,
		   def : def) : bound list * (exp * con) = 
    let
      val (bnd1,(var1,con1)) = lethelp("recordcase",arg1)
      val (rbnds,rvars,rcons) = letprojecthelp(var1,syms)
      val _ = con_unify(context,"tuprec_case",
			con1,"con1",
			con_record(zip syms rcons),"(rcons)")
      val newargs = map2 (fn (v,c) => CASE_VAR(v,c)) (rvars,rcons)
      fun extender (pats,(cl,bound,body)) = (pats @ cl, bound, body)
      val newarms = map extender accs
      val (vc_ll : bound list, ec) = match(newargs @ args,newarms,def)
    in (vc_ll,wrapbnd(bnd1,wrapbnds(rbnds,ec)))
    end



  and constr_case (arg1,
		   args,
		   accs : ((Ast.path * Ast.pat option) * arm) list,
		   def : def) : bound list * (exp * con) = 
    let 
      val _ = debugdo (fn () => (print "\n\nCONSTRUCTOR_CASE CALLED with clauses:\n";
				 mapcount (fn (i,(path_patopt,arm)) =>  
					   (print "  clause #"; print (makestring i); 
					    pp_path_patopt path_patopt;
					    print "\nand arm is:"; pp_arm arm; print "\n")) accs))
      val (casearg, casecon) = (case arg1 of
				  CASE_VAR(v,con) => (VAR v, con)
				| CASE_NONVAR (_,binde,bindc) => (binde,bindc))
      val rescon = fresh_con()
      fun getarm datacon sumcon (i,{name=cur_constr,arg_type}) : bound list * exp option = 
	let 
	  fun armhelp ((path,patopt), (clause,bound,body)) : arm option = 
	    (case (eq_label(cur_constr, symbol2label (List.last path)), patopt) of
	       (false,_) => NONE
	     | (true,NONE) => SOME(clause,bound,body)
	     | (true,SOME argument) => SOME(argument::clause,bound,body))
	  val relevants : arm list = List.mapPartial armhelp accs
	  val _ = con_unify (context,"constructor with argument con",
			     datacon,"datacon",casecon,"casecon")
	in (case (relevants : arm list, arg_type) of
	      ([],_) => ([],NONE)
	    | (_,NONE) => let 
			    val (vf_ll : bound list, (me,mc)) = match(args, relevants, def)
			    val _ = con_unify(context,"rescon with arms's result type",
					      rescon,"rescon",mc,"mc")
			  in (vf_ll,SOME (#1 (make_lambda(fresh_var(),con_unit,mc,me))))
			  end
	    | (_,SOME at) => let 
			   val var = fresh_var()
			   val rsvar = fresh_var()
			   val rscon = CON_SUM(SOME i,sumcon)
			   val rcon = List.nth(sumcon,i-1)
			   val (vf_ll,(me,mc)) = match((CASE_VAR (var,rcon))::args, relevants, def)
			   val _ = con_unify(context,"rescon with arm result type",
					     rescon,"rescon",mc,"mc")
		       in (vf_ll,SOME(#1 (make_lambda(rsvar,rscon,mc,
						      LET([BND_EXP(var,SUM_TAIL(rscon,VAR rsvar))],me)))))
		       end)
	end

      val {instantiated_type = datacon,
	   arms = constr_patconopt_list,
	   case_exp, expose_exp} =
	(case (Datatype.constr_lookup context (#1(#1(hd accs)))) of
	   NONE => error "constructor_lookup got path not to constructor"
	 | (SOME {name,datatype_path,constr_sig,datatype_sig}) => 
	     Datatype.instantiate_datatype_signature(datatype_path,datatype_sig,context,polyinst))
      val sumcon = (map (fn {name,arg_type} => case arg_type of 
			 NONE => con_unit | SOME c => c) constr_patconopt_list)
      val vfll_expopt_list : (bound list * (exp option)) list = 
	                                    mapcount (getarm datacon sumcon) constr_patconopt_list
      val _ = debugdo (fn () => (print "Got these arms:";
				 mapcount (fn (i,(_,eopt)) => (print "Arm #"; print (makestring i); print ": ";
							       (case eopt of 
								  NONE => print "NO ARM"
								| SOME e => (pp_exp e; ()));
								  print "\n")) vfll_expopt_list))
      val sumtypes = map (fn {arg_type=NONE,...} => con_unit | {arg_type=SOME c,...} => c)  constr_patconopt_list
      val expopt_list = map #2 vfll_expopt_list
      val vfll_list = map #1 vfll_expopt_list
    in  (flatten vfll_list,(CASE(sumtypes,APP(expose_exp,casearg),expopt_list,NONE), con_deref rescon))
    end

  and match (args : case_exp list, 
	     arms : arm list,
	     def : (Il.exp * Il.con) option) : bound list * (Il.exp * Il.con) = 
    (debugdo 
     (fn () => 
      (print "match called with "; print (length args); print " args:\n";
       mapcount (fn(i,a) => (print "  #"; print i; print ": "; pp_case_exp a; print "\n")) args;
       print "\nand with "; print (length arms); print " arms:\n";
       mapcount (fn(i,a) => (print "  #"; print i; print ": "; pp_arm a; print "\n")) arms;
       print "      and with def = "; pp_def def; print "\n\n"));
     case (arms,args) of
       ([],_) => (case def of
		    SOME ec => ([] : bound list, ec)
		  | NONE => ([], (Il.RAISE(bindexn_exp),fresh_con())))
     | ((_,[bound1 as (_,v,c)],NONE)::_,[]) => ([[bound1]],(VAR v,c))
     | ((_,bound,NONE)::_,[]) => (let val (vars,cons) = (map #2 bound, map #3 bound)
				  in ([bound],(exp_tuple (map VAR vars), con_tuple cons))
				  end)
     | ((arm as (_,bound : bound,SOME exp))::_, []) =>
	 let
	   val ce = arm2context_entries arm
	   val context' = add_context_entries(context,ce)
	   val _ = debugdo (fn () => (print "calling xexp with ce = \n";
				      pp_context (CONTEXT ce);
				      print "\n"))
	   val ec = xexp(context',exp)
	 in ([bound],ec)
	 end
     | (_,arg1::argrest) => 
	 let
	   (* this gets rid of MarkPat, ConstraintPat, ListPat of the first pattern *)
	   local
	     fun dopat (Ast.MarkPat(p,r)) = dopat p
	       | dopat (Ast.ConstraintPat{pattern,constraint}) = 
	       let val c = xty(context,constraint)
		 val con = get_case_exp_con arg1
		 val _ = con_unify(context,"Constraint Pattern",c,"c",con,"con")
	       in dopat pattern
	       end
	       | dopat (Ast.ListPat []) = Ast.VarPat [Symbol.varSymbol "nil"]
	       | dopat (Ast.ListPat (p::rest)) = Ast.AppPat{constr=Ast.VarPat[Symbol.varSymbol "::"],
							    argument=Ast.TuplePat[p,dopat (Ast.ListPat rest)]}
	       | dopat p = p
	     fun do_mark_and_constraint ([],bound,body) = error "no pattern but have arguments"
	       | do_mark_and_constraint (hdpat::tlpat,bound,b) = ((dopat hdpat)::tlpat,bound,b)
	   in
	     val _ = debugdo (fn () => (print "got "; print (makestring (length args)); 
					print " args\n";
					mapcount (fn (i,a) => (print "arm #"; print i; 
							       print " has ";
							       print (length (#1 a)); 
							       print "pats in clause\n")) arms))
	     val arms = map do_mark_and_constraint arms
	     val (arm1,armrest) = (hd arms, tl arms)
	     val (clauses, bound, bodies) = (map #1 arms, map #2 arms, map #3 arms)
	     val (clause1,clauserest) = (#1 arm1, map #1 armrest)
	     val (bound1,boundrest) = (#2 arm1, map #2 armrest)
	     val (body1,bodyrest) = (#3 arm1, map #3 armrest) 
	     val (headpat1, headpat_rest, tlpat1, tlpat_rest) = (hd clause1, map hd clauserest, 
								 tl clause1, map tl clauserest) 
	       handle List.Empty => error "some clause was empty while arglist was non-empty"
	   end
	 
	   (* -----------------------------------------------------------------
	    Given a 'a value-returning predicate that matches certains patterns 
           and a list of arms, returns a triple consisteing of
	    (1) the largest initial sequence of matching arms
           (2) the bound variable/constructors of the compiled non-matching arms
           (3) the compiled result of the non-matching arms
	     ---------------------------------------------------------------- *)
	   local
	     fun find_maxseq' patpred (arms : arm list) : (('a * arm) list * arm list) = 
	       let fun loop [] acc = (acc,[])
		     | loop (arms as ((hdpat::tlpat,bound,body)::armrest)) acc = 
		 (case (patpred hdpat) of
		    SOME obj => loop armrest ((obj,(tlpat,bound,body))::acc)
		  | NONE => (acc,arms))
		     | loop _ _ = error "find_maxseq' got an arm with no patterns"
	       in loop arms []
	       end
	   in
	     fun find_maxseq patpred (arms : arm list) : (('a * arm) list * 
							  bound list * def) = 
	       let 
		 val (accs,arms) = find_maxseq' patpred arms
		 val (vc_ll2, def) = (case arms of 
					[] => ([],def)
				      | _ => let val (vc_ll,ec) = match(args,arms,def)
					     in (vc_ll,SOME ec)
					     end)
		   in (accs,vc_ll2,def)
		   end
	       end

	     (* -----------------------------------------------------------------
               Here are the dispatchers of various pattern types.
		  Dispatchers look for maximal sequences of certain pattern types,
		  gather some information, compile the rest of the patterns into a def,
		  and then call the appropriate compiler for those patterns. 
              ----------------------------------------------------------------- *)
	       val is_constr = is_constr(modsig_lookup, context)
	       fun var_dispatch() = 
		 let 
		   fun varpred (Ast.VarPat [v]) = if (is_constr [v]) then NONE else SOME v
		     | varpred _ = NONE
		   val (accs,vc_ll2,def) = find_maxseq varpred arms
		   val (vc_ll,ec) = var_case(arg1,argrest,accs,def)
		 in (vc_ll @ vc_ll2, ec)
		 end
	       fun layer_dispatch() = 
		 let 
		   fun layerpred (Ast.LayeredPat{varPat=Ast.VarPat[s],expPat}) = SOME(s,expPat)
		     | layerpred (Ast.LayeredPat{varPat,expPat}) = error "Funny varPat in LayeredPat"
		     | layerpred _ = NONE
		   val (accs,vc_ll2,def) = find_maxseq layerpred arms
		   fun layer2var ((s,expPat),(cl,bound,body)) = (s, (expPat :: cl,bound,body))
		   val accs = map layer2var accs
		   val (vc_ll,ec) = var_case(arg1,(case arg1 of
						     CASE_NONVAR (s,_,c) => CASE_VAR (s,c)
						   | cv => cv) :: argrest,accs,def)
		 in (vc_ll @ vc_ll2, ec)
		 end


	       fun ref_dispatch() = 
		 let
		   fun refpred (Ast.AppPat {constr=Ast.VarPat [s],argument}) =
		     if (s = Symbol.varSymbol "ref") then SOME argument else NONE
		     | refpred _ = NONE
		   val (accs,vc_ll2,def) = find_maxseq refpred arms
		   val (vc_ll,ec) = ref_case(arg1,argrest,accs,def)
		 in (vc_ll @ vc_ll2, ec)
		 end

(*
	       fun nil_dispatch() = 
		 let 
		   fun nilpred (Ast.ListPat []) = SOME ()
		     | nilpred _ = NONE
		   val (accs,vc_ll2,def) = find_maxseq nilpred arms
		   val (vc_ll,ec) = nil_case(arg1,argrest,accs,def)
		 in (vc_ll @ vc_ll2, ec)
		 end

	       fun cons_dispatch() = 
		 let 
		   fun conspred (Ast.ListPat (p1::prest)) = SOME (p1,Ast.ListPat prest)
		     | conspred (Ast.AppPat{constr=Ast.VarPat[s],
					    argument=Ast.TuplePat[p1,p2]}) = 
		       if (s = Symbol.varSymbol "::") 
			 then SOME(p1,p2)
		       else NONE
		     | conspred _ = NONE
		   val (accs,vc_ll2,def) = find_maxseq conspred arms
		   val (vc_ll,ec) = cons_case(arg1,argrest,accs,def)
		 in (vc_ll @ vc_ll2, ec)
		 end

	       fun list_dispatch() = 
		 (case (hd (#1 (hd arms))) of
		    (Ast.ListPat []) => nil_dispatch()
		  | (Ast.ListPat _) => cons_dispatch()
		  | _ => error "not a ListPat in list_dispatch")
*)
	       fun constructor_dispatch() = 
		 let 
		   fun getpath (Ast.VarPat p) = p
		     | getpath (Ast.MarkPat (p,r)) = getpath p
		     | getpath _ = error "getpath failed to fetch path"
		   fun constrpred (Ast.VarPat p) = if (is_constr p) then SOME (p,NONE) else NONE
		     | constrpred (Ast.AppPat {constr,argument}) = SOME(getpath constr,SOME argument)
		     | constrpred _ = NONE
		   fun constr_dispatch() = 
		     let
		       val (accs,vc_ll2,def) = find_maxseq constrpred arms
		       val (vc_ll,ec) = constr_case(arg1,argrest,accs,def)
		     in (vc_ll @ vc_ll2, ec)
		     end
		 in (case (hd(#1 (hd arms))) of
			   Ast.AppPat{constr=Ast.VarPat[s],...} => if (s = Symbol.varSymbol "ref") 
									then ref_dispatch()
								      else constr_dispatch()
			 | _ => constr_dispatch())
		 end

	       fun tuple_record_dispatch() =
		 let 
		   fun makesym n = mapcount (fn (i,_) => generate_tuple_symbol i) (count n)
		   val syms = (case headpat1 of
				 Ast.TuplePat pats => makesym (length pats)
			       | Ast.RecordPat {def,flexibility} => map #1 def
			       | _ => error "weird bug")
		   fun tuprecpred (Ast.TuplePat pats) = if (makesym (length pats) = syms)
							  then SOME pats
							else error "tuple arity mismatch in pattern"
		     | tuprecpred (Ast.RecordPat {def,flexibility=false}) = if ((map #1 def) = syms)
									      then SOME(map #2 def)
						       else error "record pattern label/arity mismatch"
		     | tuprecpred (Ast.RecordPat {def,flexibility=true}) = error "flex records not handled"
		     | tuprecpred _ = NONE
		   val (accs,vc_ll2,def) = find_maxseq tuprecpred arms
		   val (vc_ll,ec) = tuprec_case(arg1,argrest,syms,accs,def)
		 in (vc_ll @ vc_ll2, ec)
		 end

	       fun constant_dispatch(eqcon,eq,sc) : bound list * (exp * con) = 
		   let
		     fun conszip a b = map (op ::) (zip a b)
		     val (bndopt,(var,con)) = lethelp("casevar_scon",arg1)
		     val _ = con_unify(context,"Base type Pattern",eqcon,"",con,"con")
		     val (vc_ll1,(e1,c1)) = match(argrest, [(tlpat1, bound1, body1)], def)
		     val (vc_ll2,(e2,c2)) = match(args, zip3 (conszip headpat_rest tlpat_rest) 
						  boundrest bodyrest, def)
		     val _ = con_unify(context,"Base type Pattern",c1,"c1",c2,"c2")
		     val ite_exp = make_ifthenelse(Il.APP(eq,exp_tuple[VAR var,Il.SCON sc]),e1,e2)
		     val ec = (case bndopt of
				 NONE => (ite_exp,c1)
			       | SOME bnd => (LET([bnd],ite_exp),c1))
		   in (vc_ll1 @ vc_ll2,ec)
		   end

		 fun wild_dispatch () = 
		   let fun wildpred(Ast.WildPat) = SOME ()
			 | wildpred _ = NONE
		       val (accs,vc_ll2,def) = find_maxseq wildpred arms
		       val (vc_ll,ec) = match(argrest,map #2 accs,def)
		   in (vc_ll @ vc_ll2, ec)
		   end

	       in
		 (case headpat1 of
		    (Ast.FlatAppPat _)    => error "no FlatAppPat should be here"
		  | (Ast.MarkPat (p,_))   => error "no MarkPat should be here"
		  | (Ast.ConstraintPat _) => error "no ConstaintPat should be here"
		  | (Ast.WordPat ws)   => constant_dispatch(CON_UINT,   
							    PRIM (PRIM2 EQ_UINTprim), 
							    UINT (WordStr2word ws))
		  | (Ast.IntPat is)    => constant_dispatch(CON_INT,   
							    PRIM (PRIM2 EQ_INTprim), 
							    INT (IntStr2word is))
		  | (Ast.RealPat rs)   => constant_dispatch(CON_FLOAT, 
							    PRIM (PRIM2 EQ_FLOATprim), 
							    FLOAT rs)
		  | (Ast.StringPat ss) => constant_dispatch(CON_VECTOR CON_CHAR, 
							    PRIM (PRIM2 (raise UNIMP)),
							    STRING ss)
		  | (Ast.CharPat cs)   => constant_dispatch(CON_CHAR,  
							    PRIM (PRIM2 EQ_CHARprim), 
							    CHAR (CharStr2char cs))
		  | (Ast.RecordPat _ | Ast.TuplePat _) => tuple_record_dispatch()
		  | (Ast.WildPat) => wild_dispatch()
		  | (Ast.VarPat v) => if (is_constr v) then constructor_dispatch() else var_dispatch()
		  | (Ast.AppPat _) => constructor_dispatch()
		  | (Ast.LayeredPat _) => layer_dispatch()
		  | (Ast.ListPat _) => error "should not get ListPat here"
		  | (Ast.VectorPat _) => raise UNIMP
		  | (Ast.OrPat _) => raise UNIMP)
	     end)

	val (bound : bound list, (final_e,final_c)) = match(compile_args,compile_arms,compile_default)
    in (final_e,final_c,mapmap (fn (s,v,c) => (s,c)) bound)
    end


    (* ============ parse possibly infix patterns =========================== *)
    fun parse_pats ({context,...} : patarg, pats) : Ast.pat list = 
      let 
	fun is_non_const (s : Symbol.symbol) = 
	  (s = Symbol.varSymbol "ref") orelse
	  (case (Datatype.constr_lookup context [s]) of
	      NONE => false
	    | (SOME {name,datatype_path,constr_sig,datatype_sig}) => 
		 not (Datatype.is_const_constr constr_sig))
	val fixtable = Context_Get_FixityTable context
      in InfixParse.parse_pat(fixtable, is_non_const, pats)
      end

    fun parse_pat (patarg,pat) : Ast.pat = (case (parse_pats(patarg,[pat])) of
							[p] => p
						      | _ => error "parse_pat got pats")



    (* ============ client interfaces =========================== *)
    (* ---- bindCompile creates bindings ----------------------- *)
    fun bindCompile {patarg = patarg,
		     bindpat : Ast.pat, 
		     arg = (arge : Il.exp, argc : Il.con)}
                    : Il.mod * Il.signat = 
      let 
	val pat = parse_pat(patarg,bindpat)
	val args = [CASE_NONVAR (fresh_named_var "bindComp",arge,argc)]
	val arms = [([pat],[],NONE)]
	val def = SOME (Il.RAISE(bindexn_exp),fresh_con())
	val (e,c,bindings_list) = compile(patarg,args,arms,def)
	val (m,s) = (case bindings_list of
	   (* if vc_list is of length 1, result was not tupled up *)
	    [[(s,c)]] => let val v = gen_var_from_symbol s
			 in (MOD_STRUCTURE[SBND (symbol2label s, BND_EXP(v,e))],
			     SIGNAT_STRUCTURE[SDEC(symbol2label s, DEC_EXP(v,c))])
			 end
	  | [sc_list] => (* otherwise, result was tupled up and we must project *)
	      let
		val lbl = fresh_int_label()
		val var = fresh_var()
		val tuplecon = con_tuple(map #2 sc_list)
		val temp = (mapcount (fn (i,(s,c)) =>
				      let 
					val l = symbol2label s
					val v = gen_var_from_symbol s
				      in (SBND(l,BND_EXP(v,RECORD_PROJECT(VAR var,
									  generate_tuple_label i,
									  tuplecon))),
					  SDEC(l,DEC_EXP(v,c)))
				      end)
			    sc_list)
		val final_mod = MOD_STRUCTURE((SBND(lbl,BND_EXP(var,e))) :: (map #1 temp))
		val final_sig = SIGNAT_STRUCTURE((SDEC(lbl,DEC_EXP(var,c))) :: (map #2 temp))
	      in  (final_mod,final_sig)
	      end
	  | [] => error "bindCompile: compile returned no bindings"
	  | _ => error "bindCompile: compile returned more than one bindings")
	val _ = debugdo (fn () => (print "bindCompile returning m = \n";
				   pp_mod m;
				   print "\n and s = \n";
				   pp_signat s;
				   print "\n"))
      in (m,s)
      end

    (* ---- caseCompile compiles a case expression in to an Il.exp/Il.con --- *)
    fun caseCompile {patarg = patarg,
		     arms = cases : (Ast.pat * Ast.exp) list, 
		     arg = (arge : Il.exp, argc : Il.con)}
      : Il.exp * Il.con = 
      let 
	val args = [CASE_NONVAR (fresh_named_var "caseComp", arge,argc)]
	val arms : arm list = map (fn (pat,body) => ([parse_pat(patarg,pat)],[], SOME body)) cases
	val def = SOME (Il.RAISE(matchexn_exp),fresh_con())
	val (e,c,_) = compile (patarg,args,arms,def)
      in (e,c)
      end


    (* ---- funCompile creates a curried function ----------------------- *)
    fun funCompile {patarg = patarg as {context, ...},
		    rules = cases : (clause * Ast.exp) list,
		    reraise}
      : {arg : Il.var * Il.con, body : Il.exp * Il.con} =
      let 
	val _ = debugdo (fn () => print "\n\n****MATCH COMPILE ENTER*********\n")
	(* -------- we begin by creating arguments of the same name as the 
	   -------- as the variable pattern when possible; just for legibility *)
	local
	  fun getname [] = "mvar"
	    | getname ((Ast.VarPat[s])::rest) = (case (Datatype.constr_lookup context [s]) of
						   NONE => Symbol.name s
						 | _ => getname rest)
	    | getname (_::rest) = getname rest
	  fun getnames ([]::_) = []
	    | getnames clauses = (getname (map hd clauses)) :: (getnames (map tl clauses))
	  val clauses = map #1 cases
	  val names = getnames clauses
	in 
	  val bound = map (fn s => (Symbol.varSymbol s, 
				    fresh_named_var s,
				    fresh_named_con s)) names
	  val argsyms = map #1 bound
	  val argvars = map #2 bound
	  val argcons = map #3 bound
	end
	(* ---- call main routine to get compiled body; creating arms by
	   ---- adding context entries to reflect these arguments ----- *)
	val arms : arm list = map (fn (cl,body) => (parse_pats(patarg,cl),bound, SOME body)) cases
	val args = map2 (fn (v,c) => CASE_VAR(v,c)) (argvars,argcons)
	val default = if (reraise) 
			then let val (v,c) = (hd argvars, hd argcons)
				 val _ = con_unify(context,"funCompile",CON_ANY,"ANY",c,"c")
			     in SOME(RAISE (VAR v),fresh_con())
			     end
		      else NONE
	val (e,c,_) = compile (patarg,args,arms,default)


	(* ---- wrap up the body almost completely --------------- *)
	local 
	  fun help ((s,v,c),(e,resc)) = make_lambda(v,c,resc,e)
	in 
	  val body = foldr help (e,c) (tl bound)
	  val arg =  (hd argvars, hd argcons)
	end

	val _ = debugdo (fn () => (print "\n\n***************\n";
				   pp_exp e;
				   print "\n\n*******MATCH COMPILE EXIT********\n"))

      in {arg = arg, body = body }
      end


  end

