(* xxx should coalesce constants *)

functor Pat(structure Il : IL
	    structure IlStatic : ILSTATIC
	    structure IlUtil : ILUTIL
	    structure Ppil : PPIL
	    structure AstHelp : ASTHELP
	    structure Datatype : DATATYPE
	    structure IlContext : ILCONTEXT
	    sharing Datatype.IlContext = IlContext
	    sharing IlContext.Il = Ppil.Il = IlUtil.Il = IlStatic.Il = Il)
  : PAT =
  struct
    
    type polyinst = Il.context * Il.sdecs -> Il.sbnd list * Il.sdecs * Il.con list 
    type typecompile = Il.context * Ast.ty -> Il.con
    type expcompile = Il.context * Ast.exp -> Il.exp * Il.con * bool
    type patarg = {context : Il.context,
		   typecompile : typecompile,
		   expcompile : expcompile,
		   polyinst : polyinst,
		   error_region : unit -> unit,
		   fresh_con : Il.context -> Il.con}


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
	in  con_all_handle(arg_con,exp_handler,con_handler,mod_handler, fn _ => NONE)
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
    datatype case_exp = CASE_VAR    of Il.var * Il.con



    type clause = Ast.pat list
    type def =  (unit -> (Il.exp * Il.con)) option
    type delay_exp = sdec list * Ast.exp
    type bound = (Symbol.symbol * var * con) list 
    type arm' = clause * bound * Ast.exp option
    type arm = clause * bound * ((context * clause * bound) -> (exp * con)) option

    (* keep the members of b2 whose symbols are in b1 in the order of b1 *)
    fun bound_intersect (b1,b2) = 
	let fun teq (s,_,_) (s',_,_) = Symbol.eq(s,s')
	    fun mapper t = List.find (teq t) b2
	in  List.mapPartial mapper b1
	end

    fun arm2sdecs(_,svc_list,_) = map (fn (s,v,c) => (SDEC(symbol_label s,
							   DEC_EXP(v,c)))) svc_list
    fun arm_addbind(s,v,c,(cl,svc_list,expopt) : arm) = 
	let fun notshadow (s2,_,_) = not(Symbol.eq(s,s2))
	    val svc_list' = List.filter notshadow svc_list
	in  (cl,(s,v,c)::svc_list',expopt)
	end
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


    fun wrapbnds ([] : bnds, (e : exp, c : con)) = (e,c)
      | wrapbnds (bnds, (e,c)) = (LET (bnds, e),c)
    fun wrapbnd (b, ec) = wrapbnds([b],ec)


    fun letprojecthelp (rv : Il.var,
			vars : var list, cons : con list, 
			labels : label list)
      : Il.bnds =
      let val rcon = CON_RECORD(sort_labelpair(zip labels cons))
	  val bnds = map2 (fn (v,l) => BND_EXP(v,RECORD_PROJECT(VAR rv,l,rcon))) (vars,labels)
      in  bnds
      end


    (* ------------ file specific pretty-printed stuff  ------------------ *)
    fun pp_path_patopt (path,patopt) = (print "   path = "; AstHelp.pp_path path;
					(case patopt of 
					   NONE => ()
					 | SOME p => (print "   pat = "; AstHelp.pp_pat p; ())))
    fun pp_case_exp (CASE_VAR (v,c)) = (print "CASE_VAR("; pp_var v; print ", "; pp_con c;
					print ")")
    fun pp_def (NONE : def) = print "NONE"
      | pp_def (SOME _) = (print "SOME = " (* ; pp_exp e *))
    fun pp_arm ((cl, bound, body) : arm) = (print "arm has clause: "; 
					    map (fn p => (AstHelp.pp_pat p; print "   ")) cl;
					    print "\n      and bound: ";
					    map (fn (s,v,c) => (print "("; AstHelp.pp_sym s; print ", ";
								pp_var v; print ","; 
								pp_con c; print ")  ")) bound;
					    print "\n      and body: ";
(*					    (case body of 
					       NONE => ()
					     | SOME e => AstHelp.pp_exp e); *)
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
 fun compile ({context, typecompile = xty, polyinst, error_region, 
		fresh_con, expcompile} : patarg,
	       compile_args : case_exp list, 
	       compile_arms : arm' list,
	       compile_default : def)
     : (exp * con) =
    let

      local 
	    fun folder((clause,bound,NONE),(ls,arms)) = (ls, (clause,bound,NONE)::arms)
	      | folder((clause,bound,SOME e),(ls,arms)) = 
		  let val r = ref ([],NONE)
		      val ls = r :: ls
		  in  (ls, 
		       (clause,bound,SOME(fn (ctxt,clause,bound) =>
			let val one = oneshot()
			    val carried as (_,(e,c,va)) = 
				  (case #2(!r) of
					    NONE => (bound,expcompile(ctxt,e))
				          | SOME t => t)
			    val _ = r := ((one,bound)::(#1(!r)), SOME carried)
			    val e = OVEREXP(c,va,one)
			in  (e,c)
			end))::arms)
	    	  end
	    (* we must use foldr to preserve the order of the arms *)
	    val (list,compile_arms) = foldr folder ([],[]) compile_arms
      in    val compile_arms = compile_arms
	    fun wrapper ec =
		let fun folder(ref(one_bounds,SOME(bound,(e,c,va))),bnds) = 
			if (length one_bounds<2) 
			  then (oneshot_set(#1(hd one_bounds),e); bnds)
			else let val bounds_list = map #2 one_bounds
				 val bounds = foldl bound_intersect bound bounds_list
				 val bounds_list = map (fn b => bound_intersect(bound,b)) bounds_list
				 val bound_vars = map #2 bounds
				 val bound_cons = map #3 bounds
				 val funvar = Name.fresh_named_var "repeat_patbody"
				 val argvar = Name.fresh_named_var "repeat_casevar"
				 val argcon = con_tuple bound_cons
				 val labels = Listops.mapcount
				     (fn (n,_) => generate_tuple_label(n+1)) bounds
				 val lbnds = letprojecthelp(argvar,bound_vars,bound_cons,labels)
				 val (e,c) = wrapbnds(lbnds,(e,c))
				 val (lambda,funcon) = make_lambda(argvar,argcon,c,e)
				 val bnd = BND_EXP(funvar,lambda)
				 fun apper(one,bounds : bound) = 
				     let val bound_vars = map #2 bounds
					 val call = APP(VAR funvar,[exp_tuple(map VAR bound_vars)])
				     in  oneshot_set(one,call)
				     end
			         val _ = app apper one_bounds
			     in  bnd :: bnds
			     end
		      | folder(ref([],NONE),bnds) = bnds
		      | folder(ref(_,NONE),bnds) = error "non-empty oneshot list with NONE"
		    val bnds = foldl folder [] list
		in  wrapbnds(bnds,ec) 
		end
      end


      val _ = debugdo (fn () => print "pat.sml: main compile called\n")
 (* ------------------------------------------------------------------------------
    various cases to handle certain collected pattern types 
    --------------------------------------------------------------------------- *)
  fun ref_case(arg1,args,
	       acc : (Ast.pat * arm) list,
	       def : def) : (exp * con) = 
    let
	val elemcon = fresh_con context
	val CASE_VAR(var,con) = arg1
	val _ = if (eq_con(context,con,CON_REF elemcon))
		    then ()
		else (error_region();
		      print "ref pattern used on a non-ref argument\n")
	val v = fresh_var()
	val bnd' = BND_EXP(v,PRIM(deref,[elemcon],[VAR var]))
	val newargs = (CASE_VAR(v,elemcon))::args
	val newarms = map (fn (p,(c,b,eopt)) => (p::c,b,eopt)) acc
	val ec = match(newargs,newarms,def)
    in  wrapbnd(bnd', ec)
    end

  and var_case(arg1,
	       args,
	       accs : (Ast.symbol * arm) list,
	       def : def) : (exp * con) = 
	 let 
	   val CASE_VAR(var,con) = arg1
	   fun extender (s, arm) = arm_addbind(s,var,con,arm)
	   val newarms = map extender accs
	 in match(args, newarms, def)
	 end


  and tuprec_case (arg1,
		   args,
		   accs : (((label * Ast.pat) list * bool) * arm) list,
		   def : def) : (exp * con) = 
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
	    in foldr folder [] splist (* foldr preserves the order *)
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
	val CASE_VAR(var1,con1) = arg1
	val rvars = map (fn _ => fresh_var()) syms
	val rcons = map (fn _ => fresh_con context) syms
 	val rbnds = letprojecthelp(var1,rvars,rcons,syms)
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
	val ec = match(newargs @ args,newarms,def)
    in wrapbnds(rbnds,ec)
    end

  and exn_case(arg1,args,
	       accs: ((Ast.symbol list * Il.exp * Il.con option * Ast.pat option) * arm) list,
	       def : def) : (exp * con) = 
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
		  val (e,c) =
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
	      in (stamp,con,body)
	      end
	  val CASE_VAR(exnvar,exncon) = arg1
	  val exnarg = VAR exnvar
	  val arms' : (exp * con * exp) list = map helper accs
	  val default' = 
	      (case def of
		   NONE => NONE
		 | SOME ec_thunk => 
		       let val (e,c) = ec_thunk()
		       in  ((if (check_rescon c)
				 then ()
			     else (error_region();
				   print "exn: arm and default type mismatch\n")); SOME e)
		       end)
      in (EXN_CASE{arg=exnarg, arms=arms', default = default', tipe = (!rescon)},!rescon)
      end


  and constr_case (arg1,
		   args,
		   accs : ((Ast.path * Ast.pat option) * arm) list,
		   def : def) : (exp * con) = 
    let 
      val _ = debugdo (fn () => (print "\n\nCONSTRUCTOR_CASE CALLED with clauses:\n";
				 mapcount (fn (i,(path_patopt,arm)) =>  
					   (print "  clause #"; printint i;
					    pp_path_patopt path_patopt;
					    print "\nand arm is:"; pp_arm arm; print "\n")) accs))
      val CASE_VAR(casevar, casecon) = arg1
      val casearg = VAR casevar
      val rescon = ref(fresh_con context)
      fun check_rescon c = 
		(sub_con(context,c,!rescon)) orelse
		(rescon := supertype (!rescon);
		sub_con(context,c,!rescon))      
      fun getarm datacon (sumcon : int * con list) (i,{name=cur_constr,arg_type}) : exp option = 
	let 
	  fun armhelp ((path,patopt), (clause,bound,body)) : arm option = 
	      (case (eq_label(cur_constr, symbol_label (List.last path)), patopt) of
	       (false,_) => NONE
	     | (true,NONE) => SOME(clause,bound,body)
	     | (true,SOME argument) => SOME(argument::clause,bound,body))
	  val relevants : arm list = List.mapPartial armhelp accs
(*	val _ = (print "====getarm enter====casecon is: "; Ppil.pp_con casecon; print "\n") *)
	  val _ = if (sub_con(context,casecon,datacon))
		      then ()
		  else (error_region();
			print "datacon is "; Ppil.pp_con datacon; print "\n";
			print "casecon is "; Ppil.pp_con casecon; print "\n";
			print "constructor pattern used on an argument of the wrong type\n")
(*
	val _ = (print "====getarm past sub_con====casecon is: "; Ppil.pp_con casecon; print "\n\nand context = \n";
			Ppil.pp_context context; print "\n\n")
*)
	  val rsvar = fresh_var()
	  val rscon = CON_SUM{carriers = #2 sumcon,
			      noncarriers = #1 sumcon,
			      special = SOME i}
	in (case (relevants : arm list, arg_type) of
	      ([],_) => NONE
	    | (_,NONE) => let 
			    val (me,mc) = match(args, relevants, def)
			    val _ = if (check_rescon mc)
					 then ()
				     else (error_region();
					   print "mc is "; Ppil.pp_con mc; 
					   print "\n rescon is "; Ppil.pp_con (!rescon); print "\n";
					   print "results types of rules mismatch\n")
			  in SOME me
			  end
	    | (_,SOME rcon) => let 
			   val var = fresh_var()
			   val (me,mc) = match((CASE_VAR (var,rcon))::args, relevants, def)
			    val _ = if (check_rescon mc)
					 then ()
				     else (error_region();
					   print "results types of rules mismatch\n")
			       in SOME(#1 (make_lambda(rsvar,rscon,mc,
					   LET([BND_EXP(var,SUM_TAIL(rscon,VAR rsvar))],me))))
		       end)
	end

      val {instantiated_type = datacon,
	   arms = constr_patconopt_list,
	   expose_exp} =
	(case (Datatype.constr_lookup context (#1(#1(hd accs)))) of
	   NONE => error "constructor_lookup got path not to constructor"
	 | (SOME {name,datatype_path,is_const,datatype_sig}) => 
	     Datatype.instantiate_datatype_signature(datatype_path,datatype_sig,context,polyinst))
      val expose_exp = 
	  let
	      fun path2pc path = 
		  let val (v,lbls) = (case path of 
					  COMPOUND_PATH(v,lbls) => (v,lbls)
					| SIMPLE_PATH v => (v,[]))
		      val SOME(lbl,_) = Context_Lookup'(context,v)
		      val lbls = lbl :: lbls
		  in  (case Context_Lookup(context,lbls) of
			   SOME(_,pc) => pc
			 | _ => (print"expose_exp not found: lbls = ";
				 Ppil.pp_pathlist Ppil.pp_label' lbls;
				 print "context is\n";
				 Ppil.pp_context context;
				 print "\n";
				 error "expose_exp not found"))
		  end
	      fun monocase exp =
		  (case path2pc(exp2path exp) of
		       PHRASE_CLASS_EXP(e,_) => e
		     | _ => error "expose_exp is non expresion")
	      fun polycase module types l = 
		  (case path2pc(mod2path module) of
		       PHRASE_CLASS_MOD(m,_) =>
			   (* perform beta reduction since types are valuable *)
			   (case m of
				MOD_FUNCTOR(v,_,MOD_STRUCTURE[SBND(_,BND_EXP(_,e))]) =>
				    exp_subst_modvar(e,[(v,types)])
			      | _ => MODULE_PROJECT(MOD_APP(m,types),l))
		     | _ => error "expose_mod is non-module")
	  in  (case expose_exp of
		   MODULE_PROJECT (MOD_APP(m,types),l) => polycase m types l
		 | _ => monocase expose_exp)
	  end

      fun loop (nca,ca) [] = (nca,rev ca)
	| loop (nca,ca) ({name,arg_type = NONE}::rest) = loop (nca+1,ca) rest
	| loop (nca,ca) ({name,arg_type = SOME c}::rest) = loop (nca,c::ca) rest
      val sumcon as (nca,ca) = loop (0,[]) constr_patconopt_list
      val expopt_list : (exp option) list = 
	                                    mapcount (getarm datacon sumcon) constr_patconopt_list

      val _ = debugdo (fn () => (print "Got these arms:";
				 mapcount (fn (i,eopt) => (print "Arm #"; printint i; print ": ";
							       (case eopt of 
								  NONE => print "NO ARM"
								| SOME e => (pp_exp e; ()));
								  print "\n")) expopt_list))
      val sumtypes = map (fn {arg_type=NONE,...} => con_unit | {arg_type=SOME c,...} => c)  constr_patconopt_list
      val exhaustive = List.all (fn NONE => false | SOME _ => true) expopt_list
(*	val _ = (print "========casecon is: "; Ppil.pp_con casecon; print "\n") *)
      val arg = (case (IlUtil.beta_reduce(expose_exp,casearg)) of
			 NONE => APP(expose_exp,[casearg])
		       | SOME e => e)
    in  (CASE{noncarriers = nca,
	       carriers = ca,
	       arg = arg,
	       arms = expopt_list,
	       default = 
	       (case (exhaustive,def) of
		    (true,_) => NONE
		  | (_,NONE) => NONE
		  | (_,SOME ec_thunk) => 
			let val (e,c) = ec_thunk()
			in  (if (eq_con(context,c,(!rescon)))
				 then ()
			     else (error_region();
				   print "result type of constructor patterns mismatch";
				   print "result type is: ";
				   pp_con (!rescon);
				   print "\nfound type is: ";
				   pp_con c;
				   print "\n");
				 SOME e)
			end),
		tipe = con_deref (!rescon)}, con_deref (!rescon))
    end

  and match (args : case_exp list, 
	     arms : arm list,
	     def : def) : (Il.exp * Il.con) = 
    (
(*
debugdo 
     (fn () => 
      (print "match called with "; printint (length args); print " args:\n";
       mapcount (fn(i,a) => (print "  #"; printint i; print ": "; pp_case_exp a; print "\n")) args;
       print "\nand with "; printint (length arms); print " arms:\n";
       mapcount (fn(i,a) => (print "  #"; printint i; print ": "; pp_arm a; print "\n")) arms;
       print "      and with def = "; pp_def def; print "\n\n"));
*)
     case (arms,args) of
       ([],_) => (case def of
		    SOME ec_thunk => ec_thunk()
		  | NONE => let val c = fresh_con context
			    in (Il.RAISE(c,bindexn_exp),c)
			    end)
     | ((_,bound,NONE)::_,[]) => (let val (vars,cons) = (map #2 bound, map #3 bound)
				  in (exp_tuple (map VAR vars), con_tuple cons)
				  end)
     | ((arm as (clause,bound : bound,SOME ec_maker))::_, []) =>
	 let
	   val sdecs = arm2sdecs arm
	   val context' = add_context_sdecs(context,sdecs)
	 in ec_maker(context',clause,bound)
	 end
     | (_,arg1::argrest) => 
	 let
	   (* this gets rid of MarkPat, ConstraintPat, ListPat of the first pattern *)
	   local
	     fun listpat2pat [] = Ast.VarPat [Symbol.varSymbol "nil"]
	       | listpat2pat (p::rest) = Ast.AppPat{constr=Ast.VarPat[Symbol.varSymbol "::"],
						     argument=Ast.TuplePat[p,listpat2pat rest]}
	     fun do_arm ([],bound,body) = error "no pattern but have arguments"
	       | do_arm (hdpat::tlpat,bound,b) = 
		 case hdpat of
		     (Ast.MarkPat(p,r)) => do_arm (p::tlpat,bound,b)
		   | (Ast.LayeredPat{varPat=Ast.VarPat[s],expPat}) =>
			 let val CASE_VAR(v,c) = arg1
			 in  do_arm(arm_addbind(s,v,c,(expPat::tlpat,bound,b)))
			 end
		   | (Ast.LayeredPat _) => error "Funny varPat in LayeredPat"
		   | (Ast.ConstraintPat{pattern,constraint}) =>
			 let val c = xty(context,constraint)
			     val CASE_VAR(_,con) = arg1
			     val _ = if (eq_con(context,c,con))
					 then ()
				     else (error_region();
					   print "constraint pattern mismatches pattern type\n";
					   print "actual type:\n";
					   pp_con con;
					   print "\nconstraint type:\n";
					   pp_con c;
					   print "\n")
			 in do_arm (pattern::tlpat, bound, b)
			 end
		   | (Ast.ListPat pats) => ((listpat2pat pats)::tlpat, bound, b)
		   | _ => (hdpat::tlpat,bound,b)
	   in
	     val _ = debugdo (fn () => (print "got "; printint (length args); 
					print " args\n";
					mapcount (fn (i,a) => (print "arm #"; printint i; 
							       print " has ";
							       printint (length(#1 a));
							       print " pats in clause\n")) arms))
	     val arms = map do_arm arms
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
	       let fun loop [] acc = (rev acc,[])
		     | loop (arms as ((hdpat::tlpat,bound,body)::armrest)) acc = 
		 (case (patpred hdpat) of
		    SOME obj => loop armrest ((obj,(tlpat,bound,body))::acc)
		  | NONE => (rev acc,arms))
		     | loop _ _ = error "find_maxseq' got an arm with no patterns"
	       in loop arms []
	       end
	   in
	     fun find_maxseq patpred (arms : arm list) : (('a * arm) list * def) =
	       let 
		 val (accs,arms) = find_maxseq' patpred arms
		 val def = (case arms of 
				[] => def
			      | _ => SOME(fn () => match(args,arms,def)))
		   in (accs,def)
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
		       val (accs,def) = find_maxseq exnpred arms
		   in  exn_case(arg1,argrest,accs,def)
		   end
	       fun var_dispatch() = 
		 let 
		   fun varpred (Ast.VarPat [v]) = if (is_constr [v]) then NONE else SOME v
		     | varpred (Ast.WildPat) = SOME (Symbol.varSymbol "_")
		     | varpred (Ast.VarPat _) = NONE
		     | varpred _ = NONE
		   val (accs,def) = find_maxseq varpred arms
		 in  var_case(arg1,argrest,accs,def)
		 end
	       fun layer_dispatch() = 
		 let 
		   fun layerpred (Ast.LayeredPat{varPat=Ast.VarPat[s],expPat}) = SOME(s,expPat)
		     | layerpred (Ast.LayeredPat{varPat,expPat}) = error "Funny varPat in LayeredPat"
		     | layerpred _ = NONE
		   val (accs,def) = find_maxseq layerpred arms
		   fun layer2var ((s,expPat),(cl,bound,body)) = (s, (expPat :: cl,bound,body))
		   val accs = map layer2var accs
		 in  var_case(arg1,arg1 :: argrest,accs,def)
		 end

	       fun ref_dispatch() = 
		 let
		   fun refpred (Ast.AppPat {constr=Ast.VarPat [s],argument}) =
		     if (s = Symbol.varSymbol "ref") then SOME argument else NONE
		     | refpred _ = NONE
		   val (accs,def) = find_maxseq refpred arms
		 in  ref_case(arg1,argrest,accs,def)
		 end


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
		       val (accs,def) = find_maxseq constrpred arms
		     in  constr_case(arg1,argrest,accs,def)
		     end
		 in (case (hd(#1 (hd arms))) of
			   Ast.AppPat{constr=Ast.VarPat[s],...} => if (s = Symbol.varSymbol "ref") 
									then ref_dispatch()
								      else constr_dispatch()
			 | _ => constr_dispatch())
		 end


	       fun tuple_record_dispatch() =
		 let 
		   fun tuple_case _ [] = []
		     | tuple_case n (p::r) = (generate_tuple_label n,p)::(tuple_case (n+1) r)
		   fun rec_case (s,p) = (symbol_label s, p)
		   fun tuprecpred (Ast.TuplePat pats) = SOME(tuple_case 1 pats, false)
		     | tuprecpred (Ast.RecordPat {def,flexibility}) = SOME(map rec_case def, flexibility)
		     | tuprecpred _ = NONE
		   val (accs,def) = find_maxseq tuprecpred arms
		 in tuprec_case(arg1,argrest,accs,def)
		 end

	       fun constant_dispatch(eqcon,eq,eqpat) : (exp * con) = 
		   let
		     fun conszip a b = map (op ::) (zip a b)
		     val CASE_VAR(var,con) = arg1
		     val _ = if (eq_con(context,eqcon,con))
				 then ()
			     else (error_region();
				   print "base type mismatches argument type\n")
		     val (e2,c2) = match(args, zip3 (conszip headpat_rest tlpat_rest) 
					 boundrest bodyrest, def)
		     fun thunk() = 
			 let val temp = zip3 (conszip headpat_rest tlpat_rest) boundrest bodyrest
			     fun filter (p::_,_,_) = eqpat p
			       | filter _ = true
			 in  match(args, List.filter filter temp, def)
			 end
		     val (e1,c1) = match(argrest, [(tlpat1, bound1, body1)], SOME thunk)
		     val _ = if (eq_con(context,c1,c2))
				 then ()
			     else (error_region();
				   print "different result type in different arms\n")
		     val ite_exp = make_ifthenelse(eq var,e1,e2,c1)
		   in (ite_exp,c1)
		   end


	       in
		 (case headpat1 of
		    (Ast.FlatAppPat _)    => error "no FlatAppPat should be here"
		  | (Ast.MarkPat (p,_))   => error "no MarkPat should be here"
		  | (Ast.ConstraintPat _) => error "no ConstaintPat should be here"
		  | (Ast.WordPat lit) => 
			let val const = uint (W32,lit)
			    fun equaler v = ILPRIM (eq_uint W32,[],
						    [VAR v,SCON const])
			    fun eqpat (Ast.WordPat lit') = TilWord64.equal(lit, lit')
			      | eqpat _ = true
			in constant_dispatch(CON_UINT W32, equaler, eqpat)
			end
		  | (Ast.IntPat lit)  => 
			let val const = int (W32,lit)
			    fun equaler v = PRIM (eq_int W32,[],
						  [VAR v, SCON const])
			    fun eqpat (Ast.IntPat lit') = TilWord64.equal(lit, lit')
			      | eqpat _ = true
			in constant_dispatch(CON_INT W32, equaler, eqpat)
			end
		  | (Ast.StringPat ss) => 
			let fun eqpat (Ast.StringPat ss') = ss = ss'
			      | eqpat _ = true
			    fun equaler v = 
			    let val len = size ss
				val lenbool = ILPRIM(eq_uint W32,[],
						   [PRIM(length_table (IntVector W8),[],[VAR v]),
						    SCON(uint (W32, TilWord64.fromInt len))])
				val z = chr 0
				val v' = fresh_var()
				fun loop _ [] = true_exp
				  | loop n [a] = loop n [a,z,z,z]
				  | loop n [a,b] = loop n [a,b,z,z]
				  | loop n [a,b,c] = loop n [a,b,c,z]
				  | loop n (a::b::c::d::rest) = 
				    let fun shift sh ch = TilWord64.lshift(TilWord64.fromInt(ord ch),sh)
					val (a,b,c,d) = if (!(Stats.bool "littleEndian"))
								then (a,b,c,d)
								else (d,c,b,a)
					val (a,b,c,d) = (shift 0 a, shift 8 b, shift 16 c, shift 24 d)
					val e = TilWord64.orb(TilWord64.orb(a,b),TilWord64.orb(c,d))
					val match = ILPRIM(eq_uint W32,[],
							 [SCON(uint (W32, e)),
							  PRIM(sub (IntVector W32),[],
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
			in  constant_dispatch(CON_VECTOR (CON_UINT W8),equaler,eqpat)
			end
		 | (Ast.CharPat cs)   => 
		      let fun equaler v = ILPRIM (eq_uint W8,[],
						  [VAR v,SCON(uint(W8,TilWord64.fromInt
								   (ord (CharStr2char cs))))])
			  fun eqpat (Ast.CharPat cs') = cs = cs'
			    | eqpat _ = true
		      in  constant_dispatch(CON_UINT W8,equaler,eqpat)
		      end
		  | (Ast.RecordPat _ | Ast.TuplePat _) => tuple_record_dispatch()
		  | (Ast.WildPat) => var_dispatch()
		  | (Ast.VarPat p) => (if (is_constr p) then constructor_dispatch() 
				      else if (is_exn p) then exn_dispatch() 
					   else (case p of 
						  [_] => var_dispatch()
						| _ => error "illegal variable pattern - path"))
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

	val final_ec = match(compile_args,compile_arms,compile_default)
	val (final_e,final_c) = wrapper final_ec
    in (final_e,final_c)
    end




    fun get_bound (pat : Ast.pat) : Ast.symbol list = 
	let open Ast
	in  (case pat of
	    WildPat => []
          | VarPat [s] => [s]
          | VarPat _ => []
          | IntPat _ => []
          | WordPat _ => []
          | StringPat _ => []
          | CharPat _ => []
          | RecordPat {def:(symbol * pat) list, flexibility:bool} => 
		 Listops.flatten(map (fn (_,p) => get_bound p) def)
          | ListPat p => Listops.flatten(map get_bound p)
          | TuplePat p => Listops.flatten(map get_bound p)
	  | FlatAppPat patfixes => error "flatapppat should be parsed away"
          | AppPat {constr:pat,argument:pat} => get_bound argument
          | ConstraintPat {pattern:pat,constraint:ty} => get_bound pattern
          | LayeredPat {varPat:pat,expPat:pat} => (get_bound varPat) @ (get_bound expPat)
          | VectorPat p => Listops.flatten(map get_bound p)
          | MarkPat (p,r) => get_bound p
          | OrPat _ => error "orpat not handler"
          | DelayPat _ => error "delaypat not handled")
	end

    (* ============ client interfaces =========================== *)
    (* ---- bindCompile creates bindings ----------------------- *)
    fun bindCompile {patarg = patarg as ({context,...} : patarg),
		     bindpat : Ast.pat, 
		     arg = (argvar : var, argc : con)}
                    : (sbnd * sdec) list =
      let 
(*
	val _ = (print "---------------------------------\n";
		 print "bindcompile called with context = \n";
		 Ppil.pp_context context; print "\n\n")
*)
	val boundsyms = get_bound bindpat

	val args = [CASE_VAR (argvar,argc)] 
	val arms = [([bindpat],[],SOME(Ast.TupleExp(map (fn s => Ast.VarExp [s]) boundsyms)))]
	val res_con = fresh_con context
	val def = SOME (fn () => (Il.RAISE(res_con,bindexn_exp),res_con))
	val (binde,bindc) = compile(patarg,args,arms,def)
(*
	val _ = (print "bindcompile returned from compile.\nGot e = ";
		 Ppil.pp_exp binde; print "\n\nand Got c = ";
		 Ppil.pp_con bindc; print "\n\n")
*)
	fun default_case() : (sbnd * sdec) list = 
	    let val l = fresh_internal_label "bind"
		val bv = fresh_named_var "bind"
		val sbnd_sdec = (SBND(l,(BND_EXP(bv,binde))),
				 SDEC(l,(DEC_EXP(bv,bindc))))
		fun mapper(n,s) = 
		    let val l = symbol_label s
			val v = gen_var_from_symbol s
			val c = (case bindc of
				     CON_RECORD lc_list => #2(List.nth(lc_list,n))
				   | _ => error "bindc not a tuple")
			val e = RECORD_PROJECT(VAR bv,generate_tuple_label (n+1) ,bindc)
		    in   (SBND(l,(BND_EXP(v,e))),
			  SDEC(l,(DEC_EXP(v,c))))
		    end
		val sbnd_sdec_proj = Listops.mapcount mapper boundsyms
	    in  sbnd_sdec :: sbnd_sdec_proj
	    end


	  local
	    exception CannotFlatten
	    fun getvar(_,VAR v) = v
	      | getvar(d,OVEREXP(_,_,one)) = getvar(d,valOf(oneshot_deref one))
	      | getvar _ = (print "getvar-cannot flatten\n";
			    raise CannotFlatten)
	    fun flatten acc (RECORD le_list) = (acc,(map getvar le_list))
	      | flatten acc (OVEREXP(_,_,one)) = flatten acc (valOf(oneshot_deref one))
	      | flatten acc (LET (bnds, rest)) = flatten (acc @ bnds) rest
	      | flatten _ _ = (print "flatten-cannot flatten\n";
			       raise CannotFlatten)
	    fun irrefutable_case (bnds,vlist) = 
		let (* val table = Listops.zip vlist boundsyms *)
		    fun mapper(v,s) = SBND(symbol_label s, BND_EXP(gen_var_from_symbol s, VAR v))
		    val sbnds_second = map2 mapper (vlist,boundsyms)
		    val sbnds_first = map (fn b => SBND(fresh_internal_label "lblx", b)) bnds
		    val sbnds = sbnds_first @ sbnds_second
		    val sdecs = IlStatic.GetSbndsSdecs(context,sbnds)
		in  Listops.zip sbnds sdecs
		end
	in  val sbnd_sdecs = (* default_case() *)
	     ((irrefutable_case (flatten [] binde)) 
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
		     arg = (argvar : Il.var, argc : Il.con)}
      : Il.exp * Il.con = 
      let 
	val args = [CASE_VAR (argvar,argc)]
	val arms : arm' list = map (fn (pat,body) => ([pat],[], SOME body)) cases
	val res_con = fresh_con context
(*	val def = SOME (Il.RAISE(res_con,matchexn_exp),res_con) *)
	val def = NONE
      in compile (patarg,args,arms,def)
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
	val arms : arm' list = map (fn (cl,body) => (cl,bound, SOME body)) cases
	val args = map2 (fn (v,c) => CASE_VAR(v,c)) (argvars,argcons)
	val default = if (reraise) 
			then let val (v,c) = (hd argvars, hd argcons)
				 val _ =  if (eq_con(context,c,CON_ANY))
					      then ()
					  else (error_region();
						print "default of pattern not an exn type\n")
				 val res_con = fresh_con context
			     in SOME(fn()=>(RAISE (res_con,VAR v),res_con))
			     end
		      else NONE
	val (e,c) = compile (patarg,args,arms,default)


	val _ = debugdo (fn () => (print "\n\n***************\n";
				   pp_exp e;
				   print "\n\n*******MATCH COMPILE EXIT********\n"))

      in {arglist = (zip argvars argcons), body = (e,c) }
      end


  end

