(* Equality compiler: Generate an equality function, if possible, for a constructor.
   This is basically just a recursive crawl of the constructor, though we may need to
   intersperse reductions in order to be able to proceed.
   *)

structure Equal :> EQUAL =
struct

    open Il

    structure C = IlContext
    structure N = Name
    structure L = Listops
    structure U = IlUtil

    fun elab_error s = Util.error "equal.sml: elaborator impossibility" s
    fun error s = Util.error "equal.sml" s
    val debug = Stats.ff("EqualDebug")
    val num = Stats.int("eqtimes")

    fun debugdo t = if (!debug) then ignore (t()) else ()

    fun con_head_normalize (arg as (_, con)) = IlStatic.con_head_normalize arg handle _ => con

    (* Raised if the equality compiler determines that this is not an equality type.
       compile handles this and returns NONE for that case. *)
    exception NoEqExp
    and ReallyNoEqExp

    local
	val cadd_eq_entry = ref (NONE : ((Il.context,Il.con,Il.exp) Tyvar.tyvar -> unit) option);
    in
	fun installHelpers { add_eq_entry : (Il.context,Il.con,Il.exp) Tyvar.tyvar -> unit } = cadd_eq_entry := SOME add_eq_entry;
	fun add_eq_entry arg =
	    case !cadd_eq_entry of
		NONE => error "add_eq_entry not installed"
	      | SOME f => f arg
    end

    (* used to create the "bind" function below for constructors and expressions.
       dontBind: predicate on objects; if true, then just pass the object directly.
       fromVar: how to get an object from a variable bound to it (will be VAR or CON_VAR)
       toBnd: how to make a binding of a variable to an object
       *)
    type 'a bindparm = {dontBind : 'a -> bool,
			fromVar  : var -> 'a,
			toBnd    : var * 'a -> bnd}

    (* bind {dontBind, fromVar, toBnd} name obj f

       Invokes f with a small version (maybe a bound variable, maybe the
       object itself if it is small) of the object passed in. The result
       of f is wrapped with any bindings needed to make that

       Invokes the function with a short version of the given object
       and rewrites the result to include any necessary bindings. *)
    fun bind (parm : 'a bindparm) (name : string) (obj : 'a) (f : 'a -> exp * con) : exp * con =
	if #dontBind parm obj then f obj
	else
	    let
		val v = N.fresh_named_var name
		val (e, c) = f (#fromVar parm v)
		val e' = if N.VarSet.member (U.exp_free e, v)
			     then U.make_let ([#toBnd parm (v, obj)], e)
			 else e
	    in  (e', c)
	    end

    (* test if a constructor is "small" *)
    fun simple_con (CON_VAR _) = true
      | simple_con (CON_INT _) = true
      | simple_con (CON_UINT _) = true
      | simple_con (CON_FLOAT _) = true
      | simple_con (CON_ANY) = true
      | simple_con _ = false

    fun bind_con x = bind {dontBind = simple_con,
			   fromVar = CON_VAR,
			   toBnd = BND_CON} x

    fun bind_exp x = bind {dontBind = fn _ => false,
			   fromVar = VAR,
			   toBnd = BND_EXP} x

    (* Invoke function with supplied or created short form of
       constructor, rewriting the result. *)
    fun maybe_bind_con (SOME con', _, f) = f con'
      | maybe_bind_con (NONE, con, f) = bind_con "name" con f

    (* Not really "state". Just carry around the local bindings for
       "true" "false" and "bool", as well as the supplied vector
       equality function and polymorphic instantiation function. *)
    type state = {polyinst_opt : context * sdecs -> (sbnd list * sdecs * con list) option,
		  vector_eq : context -> exp * con,
		  con_bool : con, true_exp : exp, false_exp : exp,
		  con_eqfun : con -> con}

    (* XXX - Tom
       I think this idiom used to have (possibly) exponentially-bad
       performance. Note that each recursive call has its own handler
       for NoEqExp, which will not ever leave until con_reduce_once of
       the con returns NONE (At that point, it would raise NoEqExp,
       raising to the enclosing handler, which would then try reducing
       its copy of the con). Thus for a deep recursive nesting of xeq,
       each handler tries reducing and then recursing again, beginning
       the whole process anew on the reduced constructor.

       So I've changed it to raise ReallyNoEqExp and exit all of the
       handlers as soon as it fails to make an equality function for
       a con that can't be reduced. I think this was the original
       intention.
       *)

    (* xeq is structured as a sequence of reduction steps. We
       see if we can find the equality function for the con
       (xeq_step); if not, reduce one step and try again. *)

    fun xeq (state : state) ctxt (nameopt, con) =
	(maybe_bind_con (nameopt, con, fn con' => xeq_step state ctxt (con', con))
	 handle NoEqExp =>
	     let in
		 debugdo(fn() => print "NoEqExp: xeq_step failed, try reduce\n");
		 num := !num + 1;
		 case IlStatic.con_reduce_once (ctxt, con)
		   of NONE => raise ReallyNoEqExp
		    | SOME c => xeq state ctxt (nameopt, c)
	     end)

    and xeq_step (state : state) (ctxt : C.context)
		 (name : con, con : con) : exp * con =
	let
	    val _ = debugdo (fn () => (print "---- XEQ step:\n";
				       print "name = "; Ppil.pp_con name; print "\n";
				       print "con  = "; Ppil.pp_con con; print "\n\n"))
	    val xeq = xeq state
	    val self = xeq ctxt
	    open Prim
	in  case con
	    (* for a tyvar, if it's resolved and has an equality function, return that.
	       if it doesn't have an equality function, try to find one by recursing. *)
	      of CON_TYVAR tyvar => (case (Tyvar.tyvar_deref tyvar, Tyvar.tyvar_eq_hole tyvar)
				       of (NONE, NONE) => raise ReallyNoEqExp
					| (NONE, SOME os) => (* hole is empty since tyvar is unset *)
					   let
					       val eqcon = #con_eqfun state con
					       val exp = OVEREXP(eqcon,true,os)
					   in  add_eq_entry tyvar;
					       (exp, eqcon)
					   end
					| (SOME c, SOME os) =>  (* tyvar has been unified so hole is full *)
					   let val exp = valOf (Util.oneshot_deref os)
					   in  (exp, #con_eqfun state c)
					   end
				        (* Here, we don't fill the hole since the side effect can't be
					   undone and isn't always appropriate. *)
					| (SOME c, NONE) => self(SOME name,c))
	       (* if a variable, just look up its equality function.
		  they're stored under a special label we can generate with Name.to_eq. *)
	       | CON_VAR v =>  let val SOME(type_label,pc) = C.Context_Lookup_Var(ctxt,v)
				   val eqlabel = N.to_eq type_label
			       in  case IlStatic.Context_Lookup_Labels(ctxt,[eqlabel]) of
				       SOME(_,PHRASE_CLASS_EXP(e,c,_,_)) => (e,c)
				     | _ => (case pc of
						 (* if it's not there, recurse *)
						 PHRASE_CLASS_CON(_,_,SOME c,_) => self(SOME name,c)
					       | _ => (debugdo (fn () =>
								(print "No eq expression available for CON_VAR ";
								         Ppil.pp_var v;
								 print " at label "; Ppil.pp_label eqlabel;
								 print ".  Perhaps due to shadowing\n"));
						       raise NoEqExp))
			       end
	       | CON_OVAR ocon => self (SOME name,CON_TYVAR (Tyvar.ocon_deref ocon))

	       (* for base types, emit the appropriate primitive *)
	       | CON_INT is => (ETAPRIM(eq_int is,[]), #con_eqfun state con)
	       | CON_UINT is => (ETAILPRIM(eq_uint is,[]), #con_eqfun state con)
	       (* no equality on floats *)
	       | CON_FLOAT fs => raise NoEqExp

		  (*
		     Simply do pair-wise comparison.
		     name = t1 * t2 * ... * tn

		     fn (v : name * name) : bool =>
		        let v1 = #1 v
			    v2 = #2 v
			in if equal(#1 v1, #1 v2)
			    then if equal(#2 v1, #2 v2)
				then ...
			      else false
                           else false
	                end

		     *)
	       | CON_RECORD fields =>
		  let val con_bool = #con_bool state
		      val v = N.fresh_named_var "eqrec"
		      val v1 = N.fresh_named_var "eqrecl"
		      val v2 = N.fresh_named_var "eqrecr"
		      val paircon = U.con_tuple[name,name]
		      val e1 = RECORD_PROJECT(VAR v,U.generate_tuple_label 1,paircon)
		      val e2 = RECORD_PROJECT(VAR v,U.generate_tuple_label 2,paircon)
		      fun help (lbl,fieldcon) =
			  let
			      val (eqexp,_) = self (NONE,fieldcon)
			      val e1 = RECORD_PROJECT(VAR v1,lbl,con)
			      val e2 = RECORD_PROJECT(VAR v2,lbl,con)
			      val exp = APP(eqexp,U.exp_tuple[e1,e2])
			  in U.exp_try_reduce (ctxt,exp)
			  end
		      fun folder (rdec,exp) =
			  let val exp' = help rdec
			  in U.make_ifthenelse ctxt (exp,exp',#false_exp state,con_bool)
			  end
		      val body = (case fields of
				      [] => #true_exp state
				    | (fst::rest) => foldl folder (help fst) rest)
		  in U.make_total_lambda(v,paircon,con_bool,
					 U.make_let([BND_EXP(v1,e1),BND_EXP(v2,e2)],body))
		  end
	       | CON_SUM {names,carrier,noncarriers,special = SOME _} =>
		  error "xeq called on special sum type"
	       (*
		  datatype t = A of int | C | B of string
		  t ~= CON_SUM(names=["C", "A", "B"],
		               carrier=CON_TUPLE_INJECT[int, string],
			       noncarriers=1,
			       special=NONE)

		  fn (v : t * t) : bool =>
		     let v1 = #1 v
			 v2 = #2 v
		     in
			 case v1 of
			     1 => case v2 of
				     1 => true
                                     _ => false
			     2 => case v2 of
				     2 => eq_int (sumtail(v1), sumtail(v2))
				     _ => false
			     3 => case v3 of
				     3 => eq_string (sumtail(v1), sumtail(v2))
				     _ => false
	             end

		  *)
	       | CON_SUM {names,carrier,noncarriers,special = NONE} =>
		  let val con_bool = #con_bool state
		      val v = N.fresh_named_var "eqsum"
		      val v1 = N.fresh_named_var "eqsuml"
		      val v2 = N.fresh_named_var "eqsumr"
		      val paircon = U.con_tuple[name,name]
		      val e1 = RECORD_PROJECT(VAR v,U.generate_tuple_label 1,paircon)
		      val e2 = RECORD_PROJECT(VAR v,U.generate_tuple_label 2,paircon)
		      val carriers =
			  (case (con_head_normalize(ctxt,carrier)) of
			       CON_TUPLE_INJECT [] => []
			     | CON_TUPLE_INJECT clist => clist
			     | c => [c])
		      val totalcount = (noncarriers + length carriers)
		      val var' = N.fresh_named_var "eqsumtl"
		      val var'' = N.fresh_named_var "eqsumtr"

		      (* generate a particular numbered arm.
			 case analyze v2, if a match, either
			 apply the appropriate equality function
			 to the tails of the sum values (when a
			 carrier) or return true (when not
			 value-carrying).
			 *)
		      fun help (i : int) =
			  let
			      val is_carrier = i >= noncarriers
			      val sumv = N.fresh_named_var "sumc"
			      val sumbnd = BND_CON(sumv, CON_SUM{names=names,
								 carrier=carrier,
								 noncarriers=noncarriers,
								 special = SOME i})
			      val sumc = CON_VAR sumv
			      val successbody =
				  if is_carrier
				      then
					  let val c = List.nth(carriers, i - noncarriers)
					      val (eqexp,_) = self(NONE, c)
					      val e' = SUM_TAIL(i, sumc, VAR var')
					      val e'' = SUM_TAIL(i, sumc, VAR var'')
					      val exp = U.make_let ([sumbnd],APP(eqexp,
										 U.exp_tuple[e',e'']))
					  in U.exp_try_reduce (ctxt,exp)
					  end
				  else #true_exp state
			      val arms2 = L.map0count
				  (fn j => if (i=j) then SOME successbody
					   else NONE) totalcount
			      val switch = CASE{sumtype = name,
						arg = VAR v2,
						bound = var'',
						arms = arms2,
						default = SOME (#false_exp state),
						tipe = con_bool}
			  in SOME switch
			  end
		      val arms1 = L.map0count help totalcount
		      val body = CASE{bound = var',
				      sumtype = name,
				      arg = VAR v1,
				      arms = arms1,
				      default = NONE,
				      tipe = con_bool}

		  in (U.make_total_lambda(v,paircon,con_bool,
					  U.make_let([BND_EXP(v1,e1),BND_EXP(v2,e2)],body)))
		  end
	       (* pointer equality *)
	       | CON_ARRAY c => (ETAPRIM(equal_table (OtherArray false),[c]),
				 #con_eqfun state con)
	       | CON_REF c => (ETAILPRIM(eq_ref,[c]), #con_eqfun state con)
	       (* 'state' has our higher-order vector_eq function.
		  Just generate the equality function for the contents of
		  the vector and pass it along. *)
	       | CON_VECTOR c =>
		  let val (e,vc) = #vector_eq state ctxt
		      val ac = CON_ARROW([#con_eqfun state c],
					 #con_eqfun state (CON_VECTOR c),
					 false, Util.oneshot())

		      val _ = if (IlStatic.eq_con(ctxt,vc,ac)) then ()
			      else (print "vc = "; Ppil.pp_con vc; print "\n";
				    print "ac = "; Ppil.pp_con ac; print "\n";
				    elab_error "Prelude vector_eq is bad")

		      val exp = APP(e, #1 (self (NONE,c)))

		  in  (U.exp_try_reduce (ctxt,exp),
		       #con_eqfun state con)
		  end
	       (* if it's from a module, look for the equality function in that module *)
	       | CON_MODULE_PROJECT(m,l) =>
		  let val e = MODULE_PROJECT(m,N.to_eq l)
		  in (UtilError.dontShow IlStatic.GetExpCon (ctxt,e)
		      handle _ => raise NoEqExp);
		      (e, #con_eqfun state con)
		  end
	       | CON_APP(c,types) =>
		  let
		      val meq =
		      (case c of
			   CON_MODULE_PROJECT(m,l) =>
			       let val s = IlStatic.GetModSig(ctxt,m)
				   val eql = N.to_eq l
			       in  case s of
				   SIGNAT_STRUCTURE sdecs =>
				       if (List.exists (fn (SDEC(l,_)) => N.eq_label(l,eql)) sdecs)
					   then MOD_PROJECT(m, N.to_eq l)
				       else raise NoEqExp
				     | _ => raise NoEqExp
			       end
			 | CON_VAR v =>
			       let val SOME (type_label,_) = C.Context_Lookup_Var(ctxt,v)
				   val eql = N.to_eq type_label
			       in (case (IlStatic.Context_Lookup_Labels(ctxt,[eql])) of
				       SOME(_,PHRASE_CLASS_MOD(m,_,_,_)) => m
				     | _ => raise NoEqExp)
			       end
			 | _ => raise NoEqExp)
		      val s = IlStatic.GetModSig(ctxt,meq)
		  in  case s
		      of SIGNAT_FUNCTOR(_,SIGNAT_STRUCTURE sdecs,
					SIGNAT_STRUCTURE [res_sdec], _) =>
			  let
			      fun translucentfy [] [] = []
				| translucentfy [] _ = elab_error "arity mismatch in eq compiler"
				| translucentfy ((SDEC(l,DEC_CON(v,k,NONE,_)))::
						 (sdec2 as (SDEC(_,DEC_EXP _))) :: rest) (c::crest) =
				  ((SDEC(l,DEC_CON(v,k,SOME c,false)))::sdec2::
				   (translucentfy rest crest))
				| translucentfy ((SDEC(l,DEC_CON(v,k,NONE,_)))::rest) (c::crest) =
				  ((SDEC(l,DEC_CON(v,k,SOME c,true)))::(translucentfy rest crest))
				| translucentfy _ _ = elab_error "got strange sdec in eq compiler"
			      val sdecs = translucentfy sdecs types
			      val (new_sbnds,new_sdecs,new_types) = (case #polyinst_opt state (ctxt,sdecs) of
									 NONE => raise NoEqExp
								       | SOME triple => triple)
			  in (MODULE_PROJECT(MOD_APP(meq,MOD_STRUCTURE new_sbnds),U.it_lab),
			      #con_eqfun state con)
			  end
		      | _ => raise NoEqExp
		  end
	       | CON_MU confun => xeq_mu state ctxt (SOME name,confun)
	       (* if a tuple of mutually recursive types, generate the tuple of equality functions,
	          then project out of that. *)
	       | CON_TUPLE_PROJECT (j, con_mu as CON_MU confun) =>
		  let val (fix_exp, fix_con) = xeq_mu state ctxt (NONE,confun)
		      val con_res = #con_eqfun state con
		      val exp_res =
			  (case confun of
			       CON_FUN ([_],_) => fix_exp
			     | _ => RECORD_PROJECT(fix_exp, U.generate_tuple_label (j+1), con_res))
		  in  (exp_res, con_res)
		  end
	       | _ => raise NoEqExp
	end

    and xeq_mu (state : state) ctxt (munameopt, confun) =
	maybe_bind_con (munameopt, CON_MU confun,
			fn muname => xeq_mu_step state ctxt (muname, confun))

    (* All this, just to make a mutually-recursive tuple of functions that unfold the
       recursive type and call the appropriate equality function on it. *)
    and xeq_mu_step (state : state) (ctxt : C.context)
		    (name : con, confun : con) : exp * con =
	let
	    val _ = debugdo (fn () => (print "---- XEQ_MU:\n";
				       print "name   = "; Ppil.pp_con name; print "\n";
				       print "confun = "; Ppil.pp_con confun; print "\n\n"))
	    val xeq = xeq state
	    val confunKind = IlStatic.GetConKind(ctxt,confun)
	    val arity =
		(case confunKind of
		     KIND_ARROW (m,KIND_TUPLE n) => if (m = n) then SOME m else NONE
		   | _ => NONE)
	    val arity = (case arity of
			     NONE =>
				 (print "confun of bad kind: "; Ppil.pp_kind confunKind; print "\n\n";
				  elab_error "xeq_mu given confun of bad kind")
			   | SOME arity => arity)

	    val name_cons = L.map0count (fn i => CON_TUPLE_PROJECT(i,name)) arity
	    val (mu_cons, expanded_cons) =
		(case confun of
		     CON_FUN(vdts,CON_TUPLE_INJECT cons) =>
			 (L.map0count (fn i => CON_TUPLE_PROJECT(i,CON_MU confun)) arity,
			  map (fn c => U.con_subst(c,U.list2subst([], L.zip vdts name_cons,[]))) cons)
		       | _ => error "xeq_mu given confun which is not CON_FUN returning CON_TUPLE")

	    fun mkvar s i = N.fresh_named_var (s ^ Int.toString i)

	    val expanded_cons_vars = L.map0count (mkvar "expanded_con") arity
	    val vars_eq = L.map0count (mkvar "vars_eq") arity
	    val evars = L.map0count (mkvar "evar") arity
	    val cvars = L.map0count (mkvar "cvar") arity
	    val type_lbls = L.map0count (fn i => N.fresh_internal_label ("type" ^ Int.toString i)) arity
	    val eq_lbls = map N.to_eq type_lbls
	    val subst = U.list2subst(L.zip evars (map VAR vars_eq),
				     L.zip cvars name_cons, [])
	    fun cfolder ((cvar,cl),ctxt) =
		let val dec = DEC_CON(cvar,KIND,NONE,false)
		in C.add_context_sdec(ctxt,SDEC(cl,dec))
		end
	    fun efolder ((evar,cvar,el),ctxt) =
		let val con = #con_eqfun state (CON_VAR cvar)
		    val dec = DEC_EXP(evar,con,NONE,false)
		in C.add_context_sdec(ctxt,SDEC(el,dec))
		end
	    val ctxt = foldl cfolder ctxt (L.zip cvars type_lbls)
	    val ctxt = foldl efolder ctxt (L.zip3 evars cvars eq_lbls)

	    val reduced_cons = (case (U.ConApply(false,confun,map CON_VAR cvars)) of
				    CON_TUPLE_INJECT conlist => conlist
				  | _ => error "body of confun not a CON_TUPLE")

	    fun make_fbnd (name_con,mu_con,expanded_con_var,expanded_con,expv,var_eq) =
		let
		    val var = N.fresh_named_var "arg_pair"
		    val var_con = U.con_tuple[name_con, name_con]
		    val expv' = U.exp_subst(expv,subst)
		    val e1 = RECORD_PROJECT(VAR var, U.generate_tuple_label 1, var_con)
		    val e2 = RECORD_PROJECT(VAR var, U.generate_tuple_label 2, var_con)
		    val e1' = COERCE(UNFOLD([], name_con, CON_VAR expanded_con_var), [], e1)
		    val e2' = COERCE(UNFOLD([], name_con, CON_VAR expanded_con_var), [], e2)
		    val exp = APP(expv', U.exp_tuple[e1',e2'])
		    val exp = U.exp_try_reduce (ctxt,exp)
		    val exp = U.make_let([BND_CON(expanded_con_var, expanded_con)],exp)
		    val fbnd = FBND(var_eq, var,var_con, #con_bool state,exp)
		in  (fbnd, #con_eqfun state mu_con)
		end

	    val exps_v = L.map2 (fn (v,c) => #1(xeq ctxt (SOME (CON_VAR v),c))) (expanded_cons_vars,reduced_cons)
	    val (fbnds,cons) = L.unzip(L.map6 make_fbnd (name_cons,mu_cons,expanded_cons_vars,
							 expanded_cons,exps_v,vars_eq))

	in  (FIX(true,TOTAL,fbnds),
	     (case cons of  (* FIX does not return a record when there is only one function *)
		  [c] => c
		| _ => U.con_tuple cons))
	end

    (* client interface:
       locally bind true, false, bool, and call xeq. Handle ReallyNoEqExp and return NONE
       when no equality function generation is possible. *)
    fun compile ({polyinst_opt : context * sdecs -> (sbnd list * sdecs * con list) option,
		  vector_eq : context -> exp * con,
		  bool : (Il.con * Il.exp * Il.exp * (Il.con -> Il.con)) option,
		  context : C.context,
		  con : con}) : (exp * con) option =
	let val _ = debugdo (fn () => (print "equality compile called with con = ";
				       Ppil.pp_con con; print "\n";
				       print "context = "; (* Ppil.pp_context context; *) print "\n"))

	    val _ = num := 0
	    (* We plan syntactic restrictions preventing the programmer from rebinding
	       bool, true, or false. *)
	    val (cbool, truee, falsee, ceqfun) =
		(case bool
		   of NONE => (U.con_bool context, U.true_exp context,
			       U.false_exp context, U.con_eqfun context)
		    | SOME x => x)

	    val res = SOME (bind_con "bool" cbool
			    (fn b => bind_exp "true" truee
			     (fn t => bind_exp "false" falsee
			      (fn f => let
					   val state : state = {polyinst_opt = polyinst_opt,
								vector_eq = vector_eq,
								con_bool = b,
								true_exp = t,
								false_exp = f,
								con_eqfun = ceqfun}
				       in  xeq state context (NONE, con)
				       end))))
		handle ReallyNoEqExp => NONE
	in
	    debugdo (fn () => print ("XXX it took me " ^ Int.toString (!num) ^ " tries.\n"));
	    res
	end

end
