(*$import Il IlStatic Ppil IlUtil IlContext Datatype Error PAT AstHelp Stats *)

(* xxx should coalesce constants *)

structure Pat
   :> PAT =
  struct

    val do_result_type = Stats.tt("PatResultType")
    val debug = Stats.ff("Pattern_debug")
    val diag = Stats.ff("Patter_diag")

    type polyinst = Il.context * Il.sdecs -> Il.sbnd list * Il.sdecs * Il.con list 
    type typecompile = Il.context * Ast.ty -> Il.con
    type expcompile = Il.context * Ast.exp -> Il.exp * Il.con * bool
    type patarg = {context : Il.context,
		   typecompile : typecompile,
		   expcompile : expcompile,
		   polyinst : polyinst}


    open Il IlStatic IlUtil Ppil
    open Util Listops Name Prim
    open IlContext Error

    val error = fn s => error "pat.sml" s
    fun printint i = print (Int.toString i)
    fun debugdo t = if (!debug) then (t(); ()) else ()

    fun compress_expression (exp as (CASE{arms,default,...})) = 
	let fun do_arms (no_arms,acc,[]) = (no_arms, rev acc)
	      | do_arms (no_arms,acc,NONE::rest) = do_arms(no_arms,NONE::acc,rest)
	      | do_arms (no_arms,acc,(SOME e)::rest) = (case compress_expression e of
							    NONE => do_arms(no_arms,NONE::acc,rest)
							  | se => do_arms(false,se::acc,rest))
	    val (no_arms,arms) = do_arms (true,[],arms)
	in  (case default of
		 NONE => if no_arms then NONE else SOME exp
	       | SOME def => if no_arms then compress_expression def else SOME exp)
	end
      | compress_expression exp = SOME exp




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
    type def =  (context -> (Il.exp * Il.con)) option
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
							   DEC_EXP(v,c,NONE, false)))) svc_list
    fun arm_addbind(s,v,c,(cl,svc_list,expopt) : arm) = 
	let fun notshadow (s2,_,_) = not(Symbol.eq(s,s2))
	    val svc_list' = List.filter notshadow svc_list
	in  (cl,(s,v,c)::svc_list',expopt)
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

    fun wrapbnds' ([] : bnds, (e : exp, c : con)) = (e,c)
      | wrapbnds' (bnds, (e,c)) = 
	let fun folder(BND_EXP(v,e),subst) = 
	         let val e' = IlUtil.exp_subst(e,subst)
		     val v' = Name.fresh_named_var (Name.var2string v)
		 in  (BND_EXP(v',e'), subst_add_expvar(subst,v,VAR v'))
		 end
	      | folder(BND_CON(v,c),subst) = (BND_CON(v,IlUtil.con_subst(c,subst)),subst)
	      | folder(BND_MOD(v,b,m),subst) = (BND_MOD(v,b,IlUtil.mod_subst(m,subst)),subst)
	    val (bnds,subst) = foldl_acc folder empty_subst bnds
	    val e = exp_subst(e,subst)
	in  (make_let (bnds, e),c)
	end

    fun wrapbnds ([] : bnds, (e : exp, c : con)) = (e,c)
      | wrapbnds (bnds, (e,c)) = (make_let (bnds, e),c)
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
 fun compile ({context=compile_context, typecompile = xty, polyinst, expcompile} : patarg,
	       compile_args : case_exp list, 
	       compile_arms : arm' list,
	       compile_default : def,
	       result_type_var : var option)
     : (exp * con) =
    let

	val wildSymbol = (Symbol.varSymbol "_")


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
			else let
				         
				 val bound = foldl bound_intersect bound (map #2 one_bounds)
				 val bound = List.filter (fn (s,_,_) => not(Symbol.eq(s,wildSymbol))) bound

(*
				 val _ = (print "BEFORE: one_bounds:\n";
					  app (fn (one,b) =>
					       (app(fn (s,v,_) =>
						   (print(Symbol.name s); print " -> ";
						   pp_var v; print "    ")) b;
						print "\n")) one_bounds;
					  print "\n\n")
*)

				 val one_bounds = map (fn (one,b) => 
						       (one,bound_intersect(bound,b))) one_bounds

(*
				 val _ = (print "AFTER: one_bounds:\n";
					  app (fn (one,b) =>
					       (app(fn (s,v,_) =>
						   (print(Symbol.name s); print " -> ";
						   pp_var v; print "    ")) b;
						print "\n")) one_bounds;
					  print "\n\n")
*)

				 val bound_vars = map #2 bound
				 val bound_cons = map #3 bound
				 val funvar = Name.fresh_named_var "repeat_patbody"
				 val argvar = if (length bound_cons = 1)
						then Name.derived_var(hd bound_vars)
					      else Name.fresh_named_var "repeat_casevar"
				 val argcon = if (length bound_cons = 1)
						then hd bound_cons
						else con_tuple bound_cons
				 val labels = Listops.mapcount
				     (fn (n,_) => generate_tuple_label(n+1)) bound
				 val lbnds = letprojecthelp(argvar,bound_vars,
							    bound_cons,labels)
				 val (e,c) = 
				     if (length bound_cons = 1)
					 then (IlUtil.exp_subst
					       (e,subst_add_expvar(empty_subst,hd bound_vars,VAR argvar)),c)
				     else wrapbnds'(lbnds,(e,c))
				 val (lambda,funcon) = make_lambda(argvar,argcon,c,e)
				 val bnd = BND_EXP(funvar,lambda)
				 fun apper(one,bounds : bound) = 
				     let val bound_vars = map #2 bounds
					 val call = APP(VAR funvar,
							if (length bound_cons = 1)
							    then VAR (hd bound_vars)
							else
							    exp_tuple(map VAR bound_vars))
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
  fun ref_case(context,
	       arg1,args,
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
	val bnd' = BND_EXP(v,ILPRIM(deref,[elemcon],[VAR var]))
	val newargs = (CASE_VAR(v,elemcon))::args
	val context = add_context_dec(context,DEC_EXP(v,elemcon,NONE, false))
	val newarms = map (fn (p,(c,b,eopt)) => (p::c,b,eopt)) acc
	val ec = match(context,newargs,newarms,def)
    in  wrapbnd(bnd', ec)
    end

  and var_case(context,
	       arg1,
	       args,
	       accs : (Ast.symbol * arm) list,
	       def : def) : (exp * con) = 
	 let 
	   val CASE_VAR(var,con) = arg1
	   fun extender (s, arm) = arm_addbind(s,var,con,arm)
	   val newarms = map extender accs
	 in match(context,args, newarms, def)
	 end


  and tuprec_case (context,
		   arg1,
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
(* val _ = (print "FLEX is "; print (Bool.toString flex); print "\n") *)
	val argcon = if flex 
			 then CON_FLEXRECORD(ref (FLEXINFO(Tyvar.get_stamp(),false,lc)))
		     else CON_RECORD lc
	val _ = if (eq_con(context,con1,argcon))
		    then ()
		else (error_region();
		      print "tuple/record pattern used on a non-record argument\n";
		      print "Actual type: "; pp_con con1; print "\n";
		      print "Pattern type: "; pp_con argcon; print "\n")
	val newargs = map2 (fn (v,c) => CASE_VAR(v,c)) (rvars,rcons)
	val context = foldl (fn (CASE_VAR(v,c),ctxt) => add_context_dec(ctxt,DEC_EXP(v,c,NONE, false))) context newargs
	fun extender (pats,(cl,bound,body)) = (pats @ cl, bound, body)
	val newarms = map extender accs'
	val ec = match(context,newargs @ args,newarms,def)
    in wrapbnds(rbnds,ec)
    end

  and exn_case(context,arg1,args,
	       accs: ((Ast.symbol list * Il.exp * Il.con option * Ast.pat option) * arm) list,
	       def : def) : (exp * con) = 
      let
	  val rescon = ref(fresh_con context)
	  fun check_rescon c = 
		(sub_con(context,c,!rescon)) orelse
		(rescon := supertype (!rescon);
		sub_con(context,c,!rescon))
	  val v = fresh_named_var "exnarg_var"
	  fun helper ((path,stamp,carried_type,patopt),arm : arm) = 
	      let val con = (case carried_type of 
				 SOME c => c
			       | _ => con_unit)
		  val (e,c) =
		      (case patopt of
			   NONE => match(context,args,[arm],def)
			 | SOME argpat =>
			       let val (cl,bound,body) = arm
				   val arm' = (argpat::cl,bound,body)
				   val context = add_context_dec(context,DEC_EXP(v,con,NONE, false))
			       in  match(context,(CASE_VAR (v,con))::args,[arm'],def)
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
		       let val (e,c) = ec_thunk context
			   val _ = if (check_rescon c)
					then ()
				   else (error_region();
					 print "exn: arm and default type mismatch\n")
		       in  compress_expression e 
		       end)
      in (EXN_CASE{arg=exnarg, arms=arms', default = default', tipe = (!rescon)},!rescon)
      end


  and constr_case (context,arg1,
		   args,
		   accs : ((Ast.path * Ast.pat option) * arm) list,
		   def : def) : (exp * con) = 
    let 
(*
      val _ = (print "\n\nCONSTRUCTOR_CASE CALLED with clauses:\n";
	       mapcount (fn (i,(path_patopt,arm)) =>  
			 (print "  clause #"; printint i;
			  pp_path_patopt path_patopt;
			  print "\nand arm is:"; pp_arm arm)) accs)
*)
      val CASE_VAR(casevar, casecon) = arg1
      val casearg = VAR casevar
      val rescon_var = fresh_named_var "rescon_var"
      val rescon = ref(fresh_con context)
      val rsvar = fresh_named_var "sumswitch_arg"
      fun check_rescon c = 
(*	  print "check_rescon = "; pp_con c; print "\n"; *)
		(sub_con(context,c,!rescon)) orelse
		(rescon := supertype (!rescon);
		 sub_con(context,c,!rescon))

      val ast_path = #1(#1(hd accs))
      val {instantiated_type = datacon,
	   instantiated_sumtype = sumtype,
	   arms = constr_patconopt_list,
	   expose_exp} = Datatype.instantiate_datatype_signature(context,#1(#1(hd accs)),polyinst)

      local
	  val (names,noncarriers,carrier) = 
	      (case (IlStatic.con_head_normalize(context,sumtype)) of
		   CON_SUM{names,noncarriers,carrier,special} => (names,noncarriers,carrier)
		 | c => (print "sumcon not reducible to SUM_CON: ";
			 pp_con c; print "\n";
			 error "sumcon not reducible to SUM_CON"))
      in  fun mk_ssumcon i = CON_SUM{names=names,
				     noncarriers=noncarriers,
				     carrier=carrier,
				     special=SOME i}
      end


      local
	  val SOME(_,PHRASE_CLASS_EXP(_,actualtype,_,_)) = Context_Lookup_Var(context,casevar)
      in  val jopt = (case actualtype of
			  CON_SUM{names,noncarriers,carrier,special} => special
			| _ => NONE)
      end

(*
      val _ = (print "datacon is "; Ppil.pp_con datacon; print "\n";
	       print "casecon before is "; Ppil.pp_con casecon; print "\n")
*)
      val _ = if (sub_con(context,casecon,datacon))
		  then ()
	      else (error_region();
		    print "datacon is "; Ppil.pp_con datacon; print "\n";
		    print "casecon is "; Ppil.pp_con casecon; print "\n";
		    print "constructor pattern used on an argument of the wrong type\n")
(*
      val _ = (print "casecon after is "; Ppil.pp_con casecon; print "\n")
*)

      fun getarm (i,{name=cur_constr,arg_type}) : exp option = 
	let 
	  fun armhelp ((path,patopt), (clause,bound,body)) : arm option = 
	      (case (eq_label(cur_constr, symbol_label (List.last path)), patopt) of
	       (false,_) => NONE
	     | (true,NONE) => SOME(clause,bound,body)
	     | (true,SOME argument) => SOME(argument::clause,bound,body))
	  val relevants : arm list = List.mapPartial armhelp accs

         val context = IlContext.add_context_dec(context,DEC_EXP(casevar,mk_ssumcon i,NONE, false))

	in  if (case jopt of
		    NONE => false
		  | SOME j => i <> j) then NONE
	    else (case (relevants : arm list, arg_type) of
	      ([],_) => NONE
	    | (_,NONE) => let 
			    val (me,mc) = match(context,args, relevants, def)
			    val _ = if (check_rescon mc)
					 then ()
				     else (error_region();
					   print "mc is "; Ppil.pp_con mc; 
					   print "\n rescon is "; Ppil.pp_con (!rescon); print "\n";
					   print "results types of rules mismatch\n")
			  in compress_expression me 
			  end
	    | (_,SOME rcon) => let 
			   val var = fresh_var()
			   val context = add_context_dec(context,DEC_EXP(var,rcon,NONE,false))
			   val (me,mc) = match(context,(CASE_VAR (var,rcon))::args, relevants, def)
			    val _ = if (check_rescon mc)
					 then ()
				     else (error_region();
					   print "results types of rules mismatch\n")
			       in SOME(make_let([BND_EXP(var,SUM_TAIL(i,sumtype,VAR rsvar))],me))
		       end)
	end


      val expopt_list = mapcount getarm constr_patconopt_list

      val _ = debugdo (fn () => (print "Got these arms:";
				 mapcount (fn (i,eopt) => (print "Arm #"; printint i; print ": ";
							       (case eopt of 
								  NONE => print "NO ARM"
								| SOME e => (pp_exp e; ()));
								  print "\n")) expopt_list))

      val exhaustive = List.all (fn NONE => false | SOME _ => true) expopt_list

      val arg = APP(expose_exp,casearg)
      val arg = (case IlUtil.exp_reduce arg of
		     NONE => arg
		   | SOME e => e)

      val case_exp = 
	  (CASE{sumtype=sumtype,
	       arg = arg,
	       bound=rsvar,
		arms = expopt_list,
	       default = 
	       (case (exhaustive,def) of
		    (true,_) => NONE
		  | (_,NONE) => NONE
		  | (_,SOME ec_thunk) => 
			let val (e,c) = ec_thunk context
			    val _ = (if (check_rescon c)
					 then ()
				     else (error_region();
					   print "result type of constructor patterns mismatch";
					   print "result type is: ";
					   pp_con (!rescon);
					   print "\nfound type is: ";
					   pp_con c;
					   print "\n"))
			in  compress_expression e
			end),
		tipe =  (case result_type_var of
			     NONE => con_deref (!rescon)
			   | SOME v => CON_VAR v)})

    in   (case_exp, con_deref (!rescon))
    end

  and match (context,
	     args : case_exp list, 
	     arms : arm list,
	     def : def) : (Il.exp * Il.con) = 
    (
(*
      print "\nMATCH called with "; printint (length args); print " args:\n";
       mapcount (fn(i,a as (CASE_VAR (casevar,_))) => (print "    #"; printint i; print ": "; 
			     pp_case_exp a; 
			     let val actualtype = 
				     (case Context_Lookup'(context,casevar) of
					  SOME(_,PHRASE_CLASS_EXP(_,c,_)) => c
					| _ => error ("!!!casevar " ^ (Name.var2string casevar) 
						      ^ " not bound"))
			     in  pp_con  actualtype
			     end;
			     print "\n")) args;
       print "and with "; printint (length arms); print " arms:\n";
       mapcount (fn(i,a) => (print "  #"; printint i; print ": "; pp_arm a; print "\n")) arms;
       print "and with def = "; pp_def def; print "\n\n";
*)
     case (arms,args) of
       ([],_) => (case def of
		    SOME ec_thunk => ec_thunk context
		  | NONE => let val c = fresh_con context
			    val ec =  (Il.RAISE(case result_type_var of
					     NONE => c
					   | SOME v => CON_VAR v,bindexn_exp),c)
			    in ec
			    end)
     | ((_,bound,NONE)::_,[]) => (let val (vars,cons) = (map #2 bound, map #3 bound)
				  in (exp_tuple (map VAR vars), con_tuple cons)
				  end)
     | ((arm as (clause,bound : bound,SOME ec_maker))::_, []) =>
	 let
	   val sdecs = arm2sdecs arm
	   val context' = add_context_sdecs(context,sdecs)
           val (e,c) = ec_maker(context',clause,bound)
	 in (e,c)
	 end
     | (_,arg1::argrest) => 
	 let
	   (* this gets rid of MarkPat, ConstraintPat, ListPat of the first pattern *)
	   local
	     fun listpat2pat [] = Ast.VarPat [Symbol.varSymbol "nil"]
	       | listpat2pat (p::rest) = Ast.AppPat{constr=Ast.VarPat[Symbol.varSymbol "::"],
						     argument=Ast.TuplePat[p,listpat2pat rest]}
	     fun do_arm ([],bound,body) = (error_region();
					   error "no pattern but have arguments")
	       | do_arm (hdpat::tlpat,bound,b) = 
		 case hdpat of
		     (Ast.MarkPat(p,r)) => do_arm (p::tlpat,bound,b)
		   | (Ast.LayeredPat{varPat=Ast.VarPat[s],expPat}) =>
			 let val CASE_VAR(v,c) = arg1
			 in  do_arm(arm_addbind(s,v,c,(expPat::tlpat,bound,b)))
			 end
		  | (Ast.OrPat pats) => (Error.error_region();
					 print "Or-patterns not implemented: using first pat\n";
					 ((hd pats)::tlpat, bound, b))
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
			      | _ => SOME(fn context => match(context,args,arms,def)))
		   in (accs,def)
		   end
	       end

	     (* -----------------------------------------------------------------
               Here are the dispatchers of various pattern types.
		  Dispatchers look for maximal sequences of certain pattern types,
		  gather some information, compile the rest of the patterns into a def,
		  and then call the appropriate compiler for those patterns. 
              ----------------------------------------------------------------- *)
	       val is_constr = Datatype.is_constr context
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
		   in  exn_case(context,arg1,argrest,accs,def)
		   end
	       fun var_dispatch() = 
		 let 
		   fun varpred (Ast.VarPat [v]) = if (is_constr [v]) then NONE else SOME v
		     | varpred (Ast.WildPat) = SOME wildSymbol
		     | varpred (Ast.VarPat _) = NONE
		     | varpred _ = NONE
		   val (accs,def) = find_maxseq varpred arms
		 in  var_case(context,arg1,argrest,accs,def)
		 end
	       fun layer_dispatch() = 
		 let 
		   fun layerpred (Ast.LayeredPat{varPat=Ast.VarPat[s],expPat}) = SOME(s,expPat)
		     | layerpred (Ast.LayeredPat{varPat,expPat}) = error "Funny varPat in LayeredPat"
		     | layerpred _ = NONE
		   val (accs,def) = find_maxseq layerpred arms
		   fun layer2var ((s,expPat),(cl,bound,body)) = (s, (expPat :: cl,bound,body))
		   val accs = map layer2var accs
		 in  var_case(context,arg1,arg1 :: argrest,accs,def)
		 end

	       fun ref_dispatch() = 
		 let
		   fun refpred (Ast.AppPat {constr=Ast.VarPat [s],argument}) =
		     if (Symbol.eq(s,Symbol.varSymbol "ref")) then SOME argument else NONE
		     | refpred _ = NONE
		   val (accs,def) = find_maxseq refpred arms
		 in  ref_case(context,arg1,argrest,accs,def)
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
		     in  constr_case(context,arg1,argrest,accs,def)
		     end
		 in (case (hd(#1 (hd arms))) of
			   Ast.AppPat{constr=Ast.VarPat[s],...} => 
			       if (Symbol.eq(s,Symbol.varSymbol "ref"))
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
		 in tuprec_case(context,arg1,argrest,accs,def)
		 end


	       datatype match = MATCH | FAIL | UNKNOWN

	       fun constant_dispatch(eqcon,eq,matchpat) : (exp * con) = 
		   let
		     fun loop (yes,maybe,maybe_no) [] = (rev yes, rev maybe, rev maybe_no)
		       | loop _ (([],bounds,body)::rest) = error "no pattern"
		       | loop (yes,maybe,maybe_no) ((arm as (pat::patrest,bounds,body))::rest) = 
			 loop (case (matchpat pat) of
				   MATCH => ((patrest,bounds,body)::yes,maybe,maybe_no)
				 | FAIL => (yes,maybe,arm::maybe_no)
				 | UNKNOWN => (yes,arm::maybe,arm::maybe_no)) rest
		     val (yes,maybe,maybe_no) = loop ([],[],[]) arms

		     val CASE_VAR(var,con) = arg1
		     val _ = if (eq_con(context,eqcon,con))
				 then ()
			     else (error_region();
				   print "base type mismatches argument type\n")

		     fun thunk context = match(context, args, maybe, def)
		     val (e1,c1) = match(context, argrest, yes, SOME thunk)
		     val (e2,c2) = match(context, args, maybe_no, def)
			 
		     val c3 = if (eq_con(context,c1,c2))
				 then c1
			      else 
				  let val c3 = supertype c1
				  in  if (sub_con(context,c2,c3))
					  then c3
				      else (error_region(); print "different result type in different arms\n";
					    tab_region(); print "First type: "; pp_con c3; print "\n";
					    tab_region(); print "Second type: "; pp_con c2; print "\n"; c3)
				  end
		     val ite_exp = make_ifthenelse(eq var,e1,e2,
						   case result_type_var of
						       NONE => c3
						     | SOME v => CON_VAR v)
		   in (ite_exp,c3)
		   end


	       val ec = 
		 (case headpat1 of
		    (Ast.FlatAppPat _)    => error "no FlatAppPat should be here"
		  | (Ast.MarkPat (p,_))   => error "no MarkPat should be here"
		  | (Ast.ConstraintPat _) => error "no ConstaintPat should be here"
		  | (Ast.WordPat lit) => 
			let val const = uint (W32,lit)
			    fun equaler v = ILPRIM (eq_uint W32,[],
						    [VAR v,SCON const])
			    fun eqpat (Ast.WordPat lit') = if TilWord64.equal(lit, lit') then MATCH else FAIL
			      | eqpat _ = UNKNOWN
			in constant_dispatch(CON_UINT W32, equaler, eqpat)
			end
		  | (Ast.IntPat lit)  => 
			let val const = int (W32,lit)
			    fun equaler v = PRIM (eq_int W32,[],
						  [VAR v, SCON const])
			    fun eqpat (Ast.IntPat lit') = if TilWord64.equal(lit, lit')
							      then MATCH else FAIL
			      | eqpat _ = UNKNOWN
			in constant_dispatch(CON_INT W32, equaler, eqpat)
			end
		  | (Ast.StringPat ss) => 
			let fun eqpat (Ast.StringPat ss') = if ss = ss' then MATCH else FAIL
			      | eqpat _ = UNKNOWN
			    fun equaler v = 
				let fun mapper c = SCON(uint(W8,TilWord64.fromInt (ord c)))
				    val str = SCON(vector (CON_UINT W8,
							   Array.fromList(map mapper (explode ss))))
				    val string_eq_label = to_eq(symbol_label (Symbol.tycSymbol "string"))
				    val string_eq = (case (Context_Lookup_Label(context,string_eq_label)) of
							 SOME(_,PHRASE_CLASS_EXP(e,_,_,_)) => e
						       | _ => error "string-equality undefined")
				in  APP(string_eq, exp_tuple[VAR v, str])
				end
(*
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
				    make_let([BND_EXP(v',PRIM(uintv2uintv (W8,W32),[],[VAR v]))],
					loop 0 (explode ss))
			    in  make_ifthenelse(lenbool,content_bool,false_exp,con_bool)
			    end
*)
			in  constant_dispatch(CON_VECTOR (CON_UINT W8),equaler,eqpat)
			end
		 | (Ast.CharPat cs)   => 
		      let fun equaler v = ILPRIM (eq_uint W8,[],
						  [VAR v,SCON(uint(W8,TilWord64.fromInt
								   (ord (CharStr2char cs))))])
			  fun eqpat (Ast.CharPat cs') = if cs = cs' then MATCH else FAIL
			    | eqpat _ = UNKNOWN
		      in  constant_dispatch(CON_UINT W8,equaler,eqpat)
		      end
		  | Ast.RecordPat _ => tuple_record_dispatch()
		  | Ast.TuplePat _ => tuple_record_dispatch()
		  | Ast.WildPat => var_dispatch()
		  | (Ast.VarPat p) => 
		      (if (is_constr p) then constructor_dispatch() 
		       else if (is_exn p) then exn_dispatch() 
			    else (case p of 
				      [_] => var_dispatch()
				    | _ => (Error.error_region();
					    print "illegal variable pattern - path\n";
					    error "illegal variable pattern - path")))
	          | (Ast.AppPat {constr,argument}) =>
			(case (case constr of
				   Ast.MarkPat (p,_) => p
				 | p => p) of
			     (Ast.VarPat p) =>
				 if (is_exn p) then exn_dispatch() 
				 else constructor_dispatch()
			   | _ => constructor_dispatch())
		  | (Ast.LayeredPat _) => layer_dispatch()
		  | (Ast.ListPat _) => error "should not get ListPat here"
		  | (Ast.VectorPat _) => raise UNIMP
		  | (Ast.OrPat _) => (Error.error_region();
				      print "Or-patterns not implemented\n";
	       				      error "Sorry, Or-patterns not implemented"))

	       val _ = debugdo 
		   (fn () => 
		    (print "\nMATCH returning "; pp_exp (#1 ec); print "\n"))
  	     in ec
	     end)

	val final_ec = match(compile_context,compile_args,compile_arms,compile_default)
	val (final_e,final_c) = wrapper final_ec

    in (final_e,final_c)
    end



    (* find all the variables that are bound; constructors are NOT bound *)
    fun get_bound context (pat : Ast.pat) : Ast.symbol list = 
	let open Ast
	in  (case pat of
	    WildPat => []
          | VarPat [s] => if (Datatype.is_constr context [s]) then [] else [s]
          | VarPat _ => []
          | IntPat _ => []
          | WordPat _ => []
          | StringPat _ => []
          | CharPat _ => []
          | RecordPat {def:(symbol * pat) list, flexibility:bool} => 
		 Listops.flatten(map (fn (_,p) => get_bound context p) def)
          | ListPat p => Listops.flatten(map (get_bound context) p)
          | TuplePat p => Listops.flatten(map (get_bound context) p)
	  | FlatAppPat patfixes => error "flatapppat should be parsed away"
          | AppPat {constr:pat,argument:pat} => get_bound context argument
          | ConstraintPat {pattern:pat,constraint:ty} => get_bound context  pattern
          | LayeredPat {varPat:pat,expPat:pat} => (get_bound context varPat) @ (get_bound context expPat)
          | VectorPat p => Listops.flatten(map (get_bound context) p)
          | MarkPat (p,r) => get_bound context p
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
	val boundsyms = get_bound context bindpat

	val args = [CASE_VAR (argvar,argc)] 
	val context = add_context_dec(context,DEC_EXP(argvar,argc,NONE,false))
	val patarg = {context = context, typecompile = #typecompile patarg,
		      expcompile = #expcompile patarg, polyinst = #polyinst patarg}

	val arms = [([bindpat],[],SOME(Ast.TupleExp(map (fn s => Ast.VarExp [s]) boundsyms)))]
	val res_con = fresh_con context
	val def = SOME (fn _ => (Il.RAISE(res_con, bindexn_exp),res_con))
	val (binde,bindc) = compile(patarg,args,arms,def,NONE)
(*
	val _ = (print "bindcompile returned from compile.\nGot e = ";
		 Ppil.pp_exp binde; print "\n\nand Got c = ";
		 Ppil.pp_con bindc; print "\n\n")
*)
	fun default_case() : (sbnd * sdec) list = 
	    let val l = fresh_internal_label "bind"
		val bv = fresh_named_var "bind"
		val sbnd_sdec = (SBND(l,(BND_EXP(bv,binde))),
				 SDEC(l,(DEC_EXP(bv,bindc,NONE,false))))
		fun mapper(n,s) = 
		    let val l = symbol_label s
			val v = gen_var_from_symbol s
			val c = (case bindc of
				     CON_RECORD lc_list => #2(List.nth(lc_list,n))
				   | _ => error "bindc not a tuple")
			val e = RECORD_PROJECT(VAR bv,generate_tuple_label (n+1) ,bindc)
		    in   (SBND(l,(BND_EXP(v,e))),
			  SDEC(l,(DEC_EXP(v,c,NONE,false))))
		    end
		val sbnd_sdec_proj = Listops.mapcount mapper boundsyms
	    in  sbnd_sdec :: sbnd_sdec_proj
	    end


	  local
	    exception CannotFlatten
	    fun getvar(_,VAR v) = v
	      | getvar(d,OVEREXP(_,_,one)) = getvar(d,valOf(oneshot_deref one))
	      | getvar _ = (if (!diag)
				then print "pat.sml: getvar cannot flatten\n"
			    else ();
			    raise CannotFlatten)
	    fun flatten acc (RECORD le_list) = (acc,(map getvar le_list))
	      | flatten acc (OVEREXP(_,_,one)) = flatten acc (valOf(oneshot_deref one))
	      | flatten acc (LET (bnds, rest)) = flatten (acc @ bnds) rest
	      | flatten _ _ = (if (!diag)
				   then print "pat.sml: flatten cannot flatten\n"
			       else ();
			       raise CannotFlatten)
	    fun irrefutable_case (bnds,vlist) = 
		let (* val table = Listops.zip vlist boundsyms *)
		    fun mapper1(i,b) = SBND(internal_label 
					    ("!irref_" ^ (Int.toString i)), b)
		    fun mapper2(v,s) = SBND(symbol_label s, 
					    BND_EXP(gen_var_from_symbol s, VAR v))
		    val sbnds_first = mapcount mapper1 bnds
		    val sbnds_second = map2 mapper2 (vlist,boundsyms)
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
	val context = add_context_dec(context,DEC_EXP(argvar,argc,NONE,false))
	val patarg = {context = context, typecompile = #typecompile patarg,
		      expcompile = #expcompile patarg, polyinst = #polyinst patarg}
	val arms : arm' list = map (fn (pat,body) => ([pat],[], SOME body)) cases
	val res_con = fresh_con context
(*	val def = SOME (Il.RAISE(res_con,matchexn_exp),res_con) *)
	val def = NONE
	val result_type_var  = fresh_named_var "result_type"
	val (e,c) = compile (patarg,args,arms,def,
			     if !do_result_type
				 then SOME result_type_var
			     else NONE)
	val e = if (!do_result_type) 
		    then make_let([BND_CON(result_type_var,c)],e)
		else e
      in (e,c)
      end


    (* ---- funCompile creates a curried function ----------------------- *)
    fun funCompile {patarg = patarg as {context, ...} : patarg,
		    rules = cases : (clause * Ast.exp) list,
		    reraise}
      : {arglist : (Il.var * Il.con) list, body : Il.exp * Il.con} =
      let 
	val _ = debugdo (fn () => print "\n\n****MATCH COMPILE ENTER*********\n")
	(* -------- we begin by creating arguments of the same name as the 
	   -------- as the variable pattern when possible; just for legibility *)
	local
	  fun getname [] = "mvar"
	    | getname ((Ast.VarPat[s])::rest) = if Datatype.is_constr context [s]
						    then getname rest
						else Symbol.name s
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
	val context = foldl (fn (CASE_VAR(v,c),ctxt) => add_context_dec(ctxt,DEC_EXP(v,c,NONE,false))) context args
	val patarg = {context = context, typecompile = #typecompile patarg,
		      expcompile = #expcompile patarg, polyinst = #polyinst patarg}
	val result_type_var  = fresh_named_var "result_type"
	val default = if (reraise) 
			then let val (v,c) = (hd argvars, hd argcons)
				 val _ =  if (eq_con(context,c,CON_ANY))
					      then ()
					  else (error_region();
						print "default of pattern not an exn type\n")
				 val res_con = fresh_con context
			     in SOME(fn _ =>(RAISE (if (!do_result_type)
						       then CON_VAR result_type_var
						   else res_con,
						   VAR v),res_con))
			     end
		      else NONE
	val (e,c) = compile (patarg,args,arms,default,
			     if (!do_result_type)
				 then SOME result_type_var
			     else NONE)
	val e = if (!do_result_type)
		    then make_let([BND_CON(result_type_var,c)],e)
		else e
		    
	val _ = debugdo (fn () => (print "\n\n***************\n";
				   pp_exp e;
				   print "\n\n*******MATCH COMPILE EXIT********\n"))

      in {arglist = (zip argvars argcons), body = (e,c) }
      end


  end

