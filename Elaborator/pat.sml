functor Pat(structure Il : IL
	    structure IlStatic : ILSTATIC
	    structure IlUtil : ILUTIL
	    structure Ppil : PPIL
	    structure InfixParse : INFIXPARSE
	    structure AstHelp : ASTHELP
	    structure Datatype : DATATYPE
	    structure IlContext : ILCONTEXT
	    sharing Datatype.IlContext = IlContext
	    sharing IlContext.Il = InfixParse.Il = Ppil.Il = IlUtil.Il = IlStatic.Il = Il)
  : PAT =
  struct
    
    type polyinst = Il.context * Il.sdecs -> Il.sbnd list * Il.sdecs * Il.con list 
    type typecompile = Il.context * Ast.ty -> Il.con
    type expcompile = Il.context * Ast.exp -> Il.exp * Il.con
    type patarg = {context : Il.context,
		   typecompile : typecompile,
		   expcompile : expcompile,
		   polyinst : polyinst,
		   error_region : unit -> unit}


    open Il IlStatic IlUtil Ppil
    open Util Listops Name (* IlLookup *) Prim
    open IlContext

    val error = fn s => error "pat.sml" s
    val debug = ref false
    fun printint i = print (Int.toString i)
    fun debugdo t = if (!debug) then (t(); ()) else ()
    fun nada() = ()

    fun supertype (arg_con : con) : con = 
	let fun exp_handler (e : exp) : exp option = NONE
	    fun mod_handler (m : mod) : mod option = NONE
	    fun con_handler (c : con) : con option = 
		(case c of
		   CON_SUM {noncarriers,carriers,special} =>
			SOME(CON_SUM{noncarriers = noncarriers,
					special = NONE,
					carriers = map supertype carriers})
		| _ => NONE)
	in  con_all_handle(arg_con,exp_handler,con_handler,mod_handler)
	end

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
    type delay_exp = sdec list * Ast.exp
    type bound = (Symbol.symbol * var * con) list 
    type arm = clause * bound * Ast.exp option

    fun arm2sdecs(_,svc_list,_) = map (fn (s,v,c) => (SDEC(symbol_label s,
							   DEC_EXP(v,c)))) svc_list
    fun arm_addbind(s,v,c,(cl,svc_list,expopt) : arm) = (cl,(s,v,c)::svc_list,expopt)
    fun is_constr context (p : Ast.path) = 
      let val res = (case Datatype.constr_lookup context p of
			 SOME _ => true
		       | NONE => false)
      in res
      end
    fun is_exn context (p : Ast.path) = 
	 case Datatype.exn_lookup context p of
		NONE => false
	      | SOME _ => true


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


    fun letprojecthelp' (context : context, rv : Il.var, labels)
      : Il.bnds * Il.var list * Il.con list = 
      let 
	  val vars = map (fn _ => fresh_var()) labels
	  val cons = map (fn _ => fresh_con context) labels
	  val rcon = CON_RECORD(sort_labelpair(zip labels cons))
	  val bnds = map2 (fn (v,l) => BND_EXP(v,RECORD_PROJECT(VAR rv,l,rcon))) (vars,labels)
      in (bnds,vars,cons)
      end

    fun letprojecthelp (context : context, rv : Il.var, recsyms : Symbol.symbol list)
      : Il.bnds * Il.var list * Il.con list = 
      let 
	  val labels = map symbol_label recsyms
      in letprojecthelp'(context,rv,labels)
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
 fun compile ({context, typecompile = xty, expcompile = xexp, polyinst, error_region} : patarg,
	       compile_args : case_exp list, 
	       compile_arms : arm list,
	       compile_default : (Il.exp * Il.con) option)
     : (exp * con * (Symbol.symbol * con) list list) = 
    let

      val _ = debugdo (fn () => print "pat.sml: main compile called\n")
 (* ------------------------------------------------------------------------------
    various cases to handle certain collected pattern types 
    --------------------------------------------------------------------------- *)
  fun ref_case(arg1,args,
	       acc : (Ast.pat * arm) list,
	       def : def) : bound list * (exp * con) = 
    let
	val elemcon = fresh_con context
	val (bnd,(var,con)) = lethelp("refvar",arg1)
	val _ = if (eq_con(context,con,CON_REF elemcon))
		    then ()
		else (error_region();
		      print "ref pattern used on a non-ref argument\n")
	val newargs = (CASE_NONVAR(fresh_var(),
				   PRIM(deref,[elemcon],[VAR var]),
				   elemcon))::args
	val newarms = map (fn (p,(c,b,eopt)) => (p::c,b,eopt)) acc
    in match(newargs,newarms,def)
    end

  and var_case(arg1,
	       args,
	       accs : (Ast.symbol * arm) list,
	       def : def) : bound list * (exp * con) = 
    (case (args,accs) of
(*           (* if no variable is yet bound, this is the last pattern,
            and this is a binding construct, then the symbol
	      matches the expression, and the bound list will be a 
	      singleton of that expression. *)
       ([],[(s,([],[],NONE))]) => (case arg1 of
				     CASE_NONVAR (v,e,c) => ([[(s,v,c)]],(e,c))
				   | CASE_VAR(v,c) => ([[(s,v,c)]],(VAR v, c)))
     | *) _ =>
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
		   accs : (((label * Ast.pat) list * bool) * arm) list,
		   def : def) : bound list * (exp * con) = 
    let
	fun is_subset ([],s2) = true
	  | is_subset (r1::s1,s2) = member_eq(eq_label,r1,s2) andalso is_subset(s1,s2)
	fun same(s1,s2) = if (is_subset(s1,s2) 
			      andalso is_subset(s2,s1))
			      then s2
			  else error "tuprec_case failed same"
	fun subset(s1,s2) = if (is_subset(s1,s2)) 
				then s2 
			    else error "tuprec_case failed subset"
	fun merge([],s2) = s2
	  | merge(s1::r1,s2) = if (member_eq(eq_label,s1,s2))
				   then merge(r1,s2)
			       else merge(r1,s1::s2)
	fun unique (splist : (label * Ast.pat) list) = 
	    let fun folder((l,_),acc) = if (member_eq(eq_label,l,acc))
					    then error "tuprec_case failed due to duplicate field names"
					else l::acc
	    in foldl folder [] splist
	    end
	fun folder (((splist : (label * Ast.pat) list,flag),_),(syms,flex)) = 
	    (case (flag,flex) of
		 (false,false) => (same(unique splist,syms),false)
	       | (true,false) => (subset(unique splist,syms),false)
	       | (false,true) => (subset(syms,unique splist),false)
	       | (true,true) => (merge(unique splist,syms),true))
	val (syms,flex) = foldl folder ([],true) accs
	fun fetch splist s = (case assoc_eq(eq_label,s,splist) of
				  SOME p => p
				| NONE => Ast.WildPat)
	fun acc_normalize((splist,flex),arm) = (map (fetch splist) syms, arm)
	val accs' = map acc_normalize accs
	val (bnd1,(var1,con1)) = lethelp("recordcase",arg1)
	val (rbnds,rvars,rcons) = letprojecthelp'(context,var1,syms)
	val lc = sort_labelpair (zip syms rcons)
	val argcon = if flex 
			 then CON_FLEXRECORD(ref (FLEXINFO(Tyvar.get_stamp(),false,lc)))
		     else CON_RECORD lc
	val _ = if (eq_con(context,con1,argcon))
		    then ()
		else (error_region();
		      print "tuple/record pattern used on a non-record argument\n")
	val newargs = map2 (fn (v,c) => CASE_VAR(v,c)) (rvars,rcons)
	fun extender (pats,(cl,bound,body)) = (pats @ cl, bound, body)
	val newarms = map extender accs'
	val (vc_ll : bound list, ec) = match(newargs @ args,newarms,def)
    in (vc_ll,wrapbnd(bnd1,wrapbnds(rbnds,ec)))
    end

  and exn_case(arg1,args,
	       accs: ((Ast.symbol list * Il.exp * Il.con option * Ast.pat option) * arm) list,
	       def : def) : bound list * (exp * con) = 
      let
	  val rescon = ref(fresh_con context)
	  fun check_rescon c = 
		(sub_con(context,c,!rescon)) orelse
		(rescon := supertype (!rescon);
		sub_con(context,c,!rescon))
	  fun helper ((path,stamp,carried_type,patopt),arm : arm) = 
	      let val con = (case carried_type of 
				 SOME c => c
			       | _ => con_unit)
		  val v = fresh_var()
		  val (bound',(e,c)) =
		      (case patopt of
			   NONE => match(args,[arm],def)
			 | SOME argpat =>
			       let val (cl,bound,body) = arm
				   val arm' = (argpat::cl,bound,body)
			       in  match((CASE_VAR (v,con))::args,[arm'],def)
			       end)
		  val _ =  if (check_rescon c)
			       then ()
			   else (error_region();
				 print "body and handler type mismatch\n")
		  val body = #1(make_lambda(v,con,c,e))
	      in (bound',(stamp,con,body))
	      end
	  val (exnarg, exncon) = (case arg1 of
				      CASE_VAR(v,con) => (VAR v, con)
				    | CASE_NONVAR (_,binde,bindc) => (binde,bindc))
	  val temp : (bound list * (exp * con * exp)) list = map helper accs
(*	  val (local_bnds,arms') = unzip(map (fn (_,(e1,c,e2)) => let val v = fresh_var()
								  in (BND_EXP(v,e1),(v,c,e2))
								  end) temp)
*)
	  val arms' = map #2 temp
	  val bound' = map #1 temp
      in (flatten bound', (* LET(local_bnds,... *)
	  (EXN_CASE(exnarg,arms',mapopt #1 def),!rescon))
      end


  and constr_case (arg1,
		   args,
		   accs : ((Ast.path * Ast.pat option) * arm) list,
		   def : def) : bound list * (exp * con) = 
    let 
      val _ = debugdo (fn () => (print "\n\nCONSTRUCTOR_CASE CALLED with clauses:\n";
				 mapcount (fn (i,(path_patopt,arm)) =>  
					   (print "  clause #"; printint i;
					    pp_path_patopt path_patopt;
					    print "\nand arm is:"; pp_arm arm; print "\n")) accs))
      val (casearg, casecon) = (case arg1 of
				  CASE_VAR(v,con) => (VAR v, con)
				| CASE_NONVAR (_,binde,bindc) => (binde,bindc))
      val rescon = ref(fresh_con context)
      fun check_rescon c = 
		(sub_con(context,c,!rescon)) orelse
		(rescon := supertype (!rescon);
		sub_con(context,c,!rescon))      
      fun getarm datacon (sumcon : int * con list) (i,{name=cur_constr,arg_type}) : bound list * exp option = 
	let 
	  fun armhelp ((path,patopt), (clause,bound,body)) : arm option = 
	      (case (eq_label(cur_constr, symbol_label (List.last path)), patopt) of
	       (false,_) => NONE
	     | (true,NONE) => SOME(clause,bound,body)
	     | (true,SOME argument) => SOME(argument::clause,bound,body))
	  val relevants : arm list = List.mapPartial armhelp accs
	  val _ = if (sub_con(context,casecon,datacon))
		      then ()
		  else (error_region();
			print "datacon is "; Ppil.pp_con datacon; print "\n";
			print "casecon is "; Ppil.pp_con casecon; print "\n";
			print "constructor pattern used on an argument of the wrong type\n")
	  val rsvar = fresh_var()
	  val rscon = CON_SUM{carriers = #2 sumcon,
			      noncarriers = #1 sumcon,
			      special = SOME i}
	in (case (relevants : arm list, arg_type) of
	      ([],_) => ([],NONE)
	    | (_,NONE) => let 
			    val (vf_ll : bound list, (me,mc)) = match(args, relevants, def)
			    val _ = if (check_rescon mc)
					 then ()
				     else (error_region();
					   print "mc is "; Ppil.pp_con mc; 
					   print "\n rescon is "; Ppil.pp_con (!rescon); print "\n";
					   print "results types of rules mismatch\n")
			  in (vf_ll,SOME me)
			  end
	    | (_,SOME rcon) => let 
			   val var = fresh_var()
			   val (vf_ll,(me,mc)) = match((CASE_VAR (var,rcon))::args, relevants, def)
			    val _ = if (check_rescon mc)
					 then ()
				     else (error_region();
					   print "results types of rules mismatch\n")
		       in (vf_ll,SOME(#1 (make_lambda(rsvar,rscon,mc,
						      LET([BND_EXP(var,SUM_TAIL(rscon,VAR rsvar))],me)))))
		       end)
	end

      val {instantiated_type = datacon,
	   arms = constr_patconopt_list,
	   expose_exp} =
	(case (Datatype.constr_lookup context (#1(#1(hd accs)))) of
	   NONE => error "constructor_lookup got path not to constructor"
	 | (SOME {name,datatype_path,is_const,datatype_sig}) => 
	     Datatype.instantiate_datatype_signature(datatype_path,datatype_sig,context,polyinst))
      fun loop (nca,ca) [] = (nca,rev ca)
	| loop (nca,ca) ({name,arg_type = NONE}::rest) = loop (nca+1,ca) rest
	| loop (nca,ca) ({name,arg_type = SOME c}::rest) = loop (nca,c::ca) rest
      val sumcon as (nca,ca) = loop (0,[]) constr_patconopt_list
      val vfll_expopt_list : (bound list * (exp option)) list = 
	                                    mapcount (getarm datacon sumcon) constr_patconopt_list
      val _ = debugdo (fn () => (print "Got these arms:";
				 mapcount (fn (i,(_,eopt)) => (print "Arm #"; printint i; print ": ";
							       (case eopt of 
								  NONE => print "NO ARM"
								| SOME e => (pp_exp e; ()));
								  print "\n")) vfll_expopt_list))
      val sumtypes = map (fn {arg_type=NONE,...} => con_unit | {arg_type=SOME c,...} => c)  constr_patconopt_list
      val expopt_list = map #2 vfll_expopt_list
      val vfll_list = map #1 vfll_expopt_list
    in  (flatten vfll_list,(CASE{noncarriers = nca,
				 carriers = ca,
				 arg = APP(expose_exp,casearg),
				 arms = expopt_list,
				 default = NONE,
				 tipe = con_deref (!rescon)}, con_deref (!rescon)))
    end

  and match (args : case_exp list, 
	     arms : arm list,
	     def : (Il.exp * Il.con) option) : bound list * (Il.exp * Il.con) = 
    (debugdo 
     (fn () => 
      (print "match called with "; printint (length args); print " args:\n";
       mapcount (fn(i,a) => (print "  #"; printint i; print ": "; pp_case_exp a; print "\n")) args;
       print "\nand with "; printint (length arms); print " arms:\n";
       mapcount (fn(i,a) => (print "  #"; printint i; print ": "; pp_arm a; print "\n")) arms;
       print "      and with def = "; pp_def def; print "\n\n"));
     case (arms,args) of
       ([],_) => (case def of
		    SOME ec => ([] : bound list, ec)
		  | NONE => let val c = fresh_con context
			    in ([], (Il.RAISE(c,bindexn_exp),c))
			    end)
(*     | ((_,[bound1 as (_,v,c)],NONE)::_,[]) => ([[bound1]],(VAR v,c)) *)
     | ((_,bound,NONE)::_,[]) => (let val (vars,cons) = (map #2 bound, map #3 bound)
				  in ([bound],(exp_tuple (map VAR vars), con_tuple cons))
				  end)
     | ((arm as (_,bound : bound,SOME exp))::_, []) =>
	 let
	   val sdecs = arm2sdecs arm
	   val context' = add_context_sdecs(context,sdecs)
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
		     val _ = if (eq_con(context,c,con))
				 then ()
			     else (error_region();
				   print "constraint pattern mismatches pattern type\n")
		 in dopat pattern
		 end
	       | dopat (Ast.ListPat []) = Ast.VarPat [Symbol.varSymbol "nil"]
	       | dopat (Ast.ListPat (p::rest)) = Ast.AppPat{constr=Ast.VarPat[Symbol.varSymbol "::"],
							    argument=Ast.TuplePat[p,dopat (Ast.ListPat rest)]}
	       | dopat p = p
	     fun do_mark_and_constraint ([],bound,body) = error "no pattern but have arguments"
	       | do_mark_and_constraint (hdpat::tlpat,bound,b) = ((dopat hdpat)::tlpat,bound,b)
	   in
	     val _ = debugdo (fn () => (print "got "; printint (length args); 
					print " args\n";
					mapcount (fn (i,a) => (print "arm #"; printint i; 
							       print " has ";
							       printint (length(#1 a));
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
	       val is_constr = is_constr context
	       val is_exn    = is_exn context
	       fun exn_dispatch() = 
		   let 
		       fun general(p,patopt) = 
			   case Datatype.exn_lookup context p of
			       NONE => NONE
			     | SOME {stamp,carried_type} => SOME(p,stamp,carried_type,patopt)
		       fun exnpred (Ast.VarPat p) = general(p,NONE)
			 | exnpred (Ast.AppPat{constr=Ast.VarPat p,argument}) = general(p,SOME argument)
			 | exnpred _ = NONE
		       val (accs,vc_ll2,def) = find_maxseq exnpred arms
		   val (vc_ll,ec) = exn_case(arg1,argrest,accs,def)
		   in (vc_ll @ vc_ll2, ec)
		   end
	       fun var_dispatch() = 
		 let 
		   fun varpred (Ast.VarPat [v]) = if (is_constr [v]) then NONE else SOME v
		     | varpred (Ast.VarPat _) = error "A variable pattern that is a path is illegal"
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

(*	       fun tuple_record_dispatch() =
		 let 
		   fun makesym n = mapcount (fn (i,_) => generate_tuple_symbol (i+1)) (count n)
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
*)
	       fun tuple_record_dispatch() =
		 let 
		   fun tuple_case _ [] = []
		     | tuple_case n (p::r) = (generate_tuple_label n,p)::(tuple_case (n+1) r)
		   fun rec_case (s,p) = (symbol_label s, p)
		   fun tuprecpred (Ast.TuplePat pats) = SOME(tuple_case 1 pats, false)
		     | tuprecpred (Ast.RecordPat {def,flexibility}) = SOME(map rec_case def, flexibility)
		     | tuprecpred _ = NONE
		   val (accs,vc_ll2,def) = find_maxseq tuprecpred arms
		   val (vc_ll,ec) = tuprec_case(arg1,argrest,accs,def)
		 in (vc_ll @ vc_ll2, ec)
		 end

	       fun constant_dispatch(eqcon,eq) : bound list * (exp * con) = 
		   let
		     fun conszip a b = map (op ::) (zip a b)
		     val (bndopt,(var,con)) = lethelp("casevar_scon",arg1)
		     val _ = if (eq_con(context,eqcon,con))
				 then ()
			     else (error_region();
				   print "base type mismatches argument type\n")
		     val (vc_ll1,(e1,c1)) = match(argrest, [(tlpat1, bound1, body1)], def)
		     val (vc_ll2,(e2,c2)) = match(args, zip3 (conszip headpat_rest tlpat_rest) 
						  boundrest bodyrest, def)
		     val _ = if (eq_con(context,c1,c2))
				 then ()
			     else (error_region();
				   print "base type mismatches argument type\n")
		     val ite_exp = make_ifthenelse(eq var,e1,e2,c1)
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
		  | (Ast.WordPat lit) => 
			let val ds = IntInf.toString lit
			    fun equaler v = ILPRIM (eq_uint W32,[],
						    [VAR v,SCON(uint (W32,TilWord64.fromDecimalString ds))])
			in constant_dispatch(CON_UINT W32, equaler)
			end
		  | (Ast.IntPat lit)  => 
			let val ds = IntInf.toString lit
			    fun equaler v = PRIM (eq_int W32,[],
						  [VAR v, SCON(int (W32,TilWord64.fromDecimalString ds))])
			in constant_dispatch(CON_INT W32, equaler)
			end
		  | (Ast.StringPat ss) => 
			let fun equaler v = 
			    let val len = size ss
				val lenbool = ILPRIM(eq_uint W32,[],
						   [PRIM(length1 false,[CON_UINT W8],[VAR v]),
						    SCON(uint (W32, TilWord64.fromInt len))])
				val z = chr 0
				val v' = fresh_var()
				fun loop _ [] = true_exp
				  | loop n [a] = loop n [a,z,z,z]
				  | loop n [a,b] = loop n [a,b,z,z]
				  | loop n [a,b,c] = loop n [a,b,c,z]
				  | loop n (a::b::c::d::rest) = 
				    let fun shift sh ch = TilWord64.lshift(TilWord64.fromInt(ord ch),sh)
					val (a,b,c,d) = (shift 0 a, shift 8 b, shift 16 c, shift 24 d)
					val e = TilWord64.orb(TilWord64.orb(a,b),TilWord64.orb(c,d))
					val match = ILPRIM(eq_uint W32,[],
							 [SCON(uint (W32, TilWord64.fromInt len)),
							  PRIM(sub1 false, [CON_UINT W32], 
							       [VAR v', 
								SCON(uint (W32, TilWord64.fromInt n))])])
				    in  (case rest of
					     [] => match
					   | _ => make_ifthenelse(match,loop (n+1) rest,
								  false_exp, con_bool))
				    end
				val content_bool = 
				    LET([BND_EXP(v',PRIM(uintv2uintv (W8,W32),[],[VAR v]))],
					loop 0 (explode ss))
			    in  make_ifthenelse(lenbool,content_bool,false_exp,con_bool)
			    end
			in  constant_dispatch(CON_VECTOR (CON_UINT W8),equaler)
			end
		 | (Ast.CharPat cs)   => 
		      let fun equaler v = ILPRIM (eq_uint W8,[],
						  [VAR v,SCON(uint(W8,TilWord64.fromInt
								   (ord (CharStr2char cs))))])
		      in  constant_dispatch(CON_UINT W8,equaler)
		      end
		  | (Ast.RecordPat _ | Ast.TuplePat _) => tuple_record_dispatch()
		  | (Ast.WildPat) => wild_dispatch()
		  | (Ast.VarPat p) => (if (is_constr p) then constructor_dispatch() 
				      else if (is_exn p) then exn_dispatch() 
					   else var_dispatch())
	          | (Ast.AppPat {constr,argument}) =>
			(case constr of
			     ((Ast.VarPat p) | (Ast.MarkPat (Ast.VarPat p,_))) => 
				 if (is_exn p) then exn_dispatch() 
				 else constructor_dispatch()
			   | _ => constructor_dispatch())
		  | (Ast.LayeredPat _) => layer_dispatch()
		  | (Ast.ListPat _) => error "should not get ListPat here"
		  | (Ast.VectorPat _) => raise UNIMP
		  | (Ast.OrPat _) => error "Sorry, Or-patterns not implemented")
	     end)

	val (bound : bound list, (final_e,final_c)) = match(compile_args,compile_arms,compile_default)
    in (final_e,final_c,mapmap (fn (s,v,c) => (s,c)) bound)
    end


    (* ============ parse possibly infix patterns =========================== *)
    fun parse_pats ({context,...} : patarg, pats) : Ast.pat list = 
      let 
	fun is_non_const (syms : Symbol.symbol list) = 
	     (syms = [Symbol.varSymbol "ref"]) orelse
	     (case (Datatype.constr_lookup context syms) of
	      NONE => (case Datatype.exn_lookup context syms of
			   NONE => false
			 | SOME {stamp,carried_type=NONE} => false
			 | SOME {stamp,carried_type=SOME _} => true)
	    | (SOME {name,datatype_path,is_const,datatype_sig}) => 
		 not is_const)
	val res = InfixParse.parse_pat(fixity context, is_non_const, pats)
      in res
      end

    fun parse_pat (patarg,pat) : Ast.pat = (case (parse_pats(patarg,[pat])) of
							[p] => p
						      | _ => error "parse_pat got pats")



    (* ============ client interfaces =========================== *)
    (* ---- bindCompile creates bindings ----------------------- *)
    fun bindCompile {patarg = patarg as ({context,...} : patarg),
		     bindpat : Ast.pat, 
		     arg = (arge : exp, argc : con)}
                    : (sbnd * sdec) list =
      let 
	val pat = parse_pat(patarg,bindpat)
	val args = [CASE_NONVAR (fresh_named_var "bindComp",arge,argc)]
	val arms = [([pat],[],NONE)]
	val res_con = fresh_con context
	val def = SOME (Il.RAISE(res_con,bindexn_exp),res_con)
	val (e,c,bindings_list) = compile(patarg,args,arms,def)
	val sc_list = (case bindings_list of
			   [sc_list] => sc_list
			 | [] => error "bindCompile: compile returned no bindings"
			 | _ => error "bindCompile: compile returned more than one bindings")
	fun default_case() = 
	    (case bindings_list of
		 (* if vc_list is of length 1, result was not tupled up *)
(*		 [[(s,c)]] => let val v = gen_var_from_symbol s
			      in [(SBND (symbol_label s, BND_EXP(v,e)),
				   SDEC(symbol_label s, DEC_EXP(v,c)))]
			      end 
	  | *) [sc_list] => (* otherwise, result was tupled up and we must project *)
	      let
		val lbl = internal_label "lbl"
		val var = fresh_var()
		val tuplecon = con_tuple(map #2 sc_list)
		val rest_sbnd_sdecs = 
		    (mapcount (fn (i,(s,c)) =>
			       let 
				   val l = symbol_label s
				   val v = gen_var_from_symbol s
			       in (SBND(l,BND_EXP(v,RECORD_PROJECT(VAR var,
									  generate_tuple_label (i+1),
									  tuplecon))),
				   SDEC(l,DEC_EXP(v,c)))
			       end)
		    sc_list)
		val first_sbnd_sdec = (SBND(lbl,BND_EXP(var,e)),
				       SDEC(lbl,DEC_EXP(var,c)))
	      in  first_sbnd_sdec :: rest_sbnd_sdecs
	      end
	  | [] => error "bindCompile: compile returned no bindings"
	  | _ => error "bindCompile: compile returned more than one bindings")

	  local
	    exception CannotFlatten
	    fun getvar(_,VAR v) = v
	      | getvar _ = raise CannotFlatten
	    fun flatten acc (RECORD le_list) = (acc,(map getvar le_list))
	      | flatten acc (VAR v) = (acc,[v])
	      | flatten acc (LET (bnds, rest)) = flatten (acc @ bnds) rest
	      | flatten _ _ = raise CannotFlatten
	    fun irrefutable_case (bnds,vlist) = 
		let val table = Listops.zip vlist sc_list
		    fun namer v = (case (assoc_eq(Name.eq_var, v, table)) of
				       SOME (s,_) => symbol_label s
				     | _ => internal_label "lblx")
		    fun make_sbnd(BND_EXP(v,e)) = SBND(namer v, BND_EXP(v,e))
		      | make_sbnd(BND_MOD(v,m)) = SBND(namer v, BND_MOD(v,m))
		      | make_sbnd(BND_CON(v,c)) = SBND(namer v, BND_CON(v,c))
		    val sbnds = map make_sbnd bnds
		    val sdecs = IlStatic.GetSbndsSdecs(context,sbnds)
		in  Listops.zip sbnds sdecs
		end
	in  val sbnd_sdecs = ((irrefutable_case (flatten [] e)) 
			       handle CannotFlatten => default_case())
	end
	val _ = debugdo (fn () => (print "bindCompile returning sbnds_sdecs = \n";
				   (pp_list (fn (sbnd,sdec) => Formatter.HOVbox[pp_sbnd' sbnd,
									       pp_sdec' sdec])
				    sbnd_sdecs ("(",", ",")",true));
				   print "\n"))
      in sbnd_sdecs
      end

    (* ---- caseCompile compiles a case expression in to an Il.exp/Il.con --- *)
    fun caseCompile {patarg = patarg as ({context,...} : patarg),
		     arms = cases : (Ast.pat * Ast.exp) list, 
		     arg = (arge : Il.exp, argc : Il.con)}
      : Il.exp * Il.con = 
      let 
	val args = [CASE_NONVAR (fresh_named_var "caseComp", arge,argc)]
	val arms : arm list = map (fn (pat,body) => ([parse_pat(patarg,pat)],[], SOME body)) cases
	val res_con = fresh_con context
	val def = SOME (Il.RAISE(res_con,matchexn_exp),res_con)
	val (e,c,_) = compile (patarg,args,arms,def)
      in (e,c)
      end


    (* ---- funCompile creates a curried function ----------------------- *)
    fun funCompile {patarg = patarg as {context, error_region, ...},
		    rules = cases : (clause * Ast.exp) list,
		    reraise}
      : {arglist : (Il.var * Il.con) list, body : Il.exp * Il.con} =
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
				    fresh_named_con (context,s))) names
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
				 val _ =  if (eq_con(context,c,CON_ANY))
					      then ()
					  else (error_region();
						print "default of pattern not an exn type\n")
				 val res_con = fresh_con context
			     in SOME(RAISE (res_con,VAR v),res_con)
			     end
		      else NONE
	val (e,c,_) = compile (patarg,args,arms,default)


	val _ = debugdo (fn () => (print "\n\n***************\n";
				   pp_exp e;
				   print "\n\n*******MATCH COMPILE EXIT********\n"))

      in {arglist = (zip argvars argcons), body = (e,c) }
      end


  end

