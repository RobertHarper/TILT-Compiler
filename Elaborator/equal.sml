(*$import Prelude TopLevel List Prim Int Il IlStatic IlUtil Ppil Util Listops Name IlContext Tyvar EQUAL Stats *)
(* Equality compiler *)
structure Equal
    :> EQUAL =
struct

    open Il IlStatic IlUtil Ppil 
    open Util Listops Name IlContext Tyvar

    val elab_error = fn s => error "equal.sml: elaborator impossibility" s
    val error = fn s => error "equal.sml" s
    val debug = Stats.ff("EqualDebug")
    fun debugdo t = if (!debug) then (t(); ()) else ()

    fun con_normalize (arg as (ctxt,con)) = IlStatic.con_normalize arg handle e => con
    fun con_head_normalize (arg as (ctxt,con)) = IlStatic.con_head_normalize arg handle e => con

    exception NoEqExp

    type 'a bindparm = {dontBind : 'a -> bool,
			fromVar  : var -> 'a,
			toBnd    : var * 'a -> bnd}

    (* Invokes the function with a short version of the given object
       and rewrites the result to include any necessary bindings. *)
    fun bind (parm : 'a bindparm) (name : string) (obj : 'a) (f : 'a -> exp * con) : exp * con =
	if #dontBind parm obj then f obj
	else
	    let
		val v = fresh_named_var name
		val (e, c) = f (#fromVar parm v)
		val e' = if Name.VarSet.member (exp_free e, v)
			     then make_let ([#toBnd parm (v, obj)], e)
			 else e
	    in  (e', c)
	    end

    fun simple_con (CON_VAR _) = true
      | simple_con (CON_INT _) = true
      | simple_con (CON_UINT _) = true
      | simple_con (CON_FLOAT _) = true
      | simple_con (CON_ANY) = true
      | simple_con _ = false

    val bind_con = fn x => bind {dontBind = simple_con,
				 fromVar = CON_VAR,
				 toBnd = BND_CON} x
	
    val bind_exp = fn x => bind {dontBind = fn _ => false,
				 fromVar = VAR,
				 toBnd = BND_EXP} x

    (* Invoke function with supplied or created short form of
       constructor, rewriting the result. *)
    fun maybe_bind_con (SOME con', _, f) = f con'
      | maybe_bind_con (NONE, con, f) = bind_con "name" con f

    type state = {polyinst_opt : context * sdecs -> (sbnd list * sdecs * con list) option,
		  vector_eq : context -> exp * con,
		  con_bool : con, true_exp : exp, false_exp : exp}

    fun xeq state ctxt (nameopt, con) =
	(maybe_bind_con (nameopt, con, fn con' => xeq_step state ctxt (con', con))
	 handle NoEqExp =>
	     let val _ = debugdo(fn() => print "NoEqExp\n")
	     in
		 case con_reduce_once (ctxt, con)
		   of NONE => raise NoEqExp
		    | SOME c => xeq state ctxt (nameopt, c)
	     end)
	
    and xeq_step (state as {polyinst_opt, vector_eq, con_bool, true_exp, false_exp})
	         (ctxt : IlContext.context)
		 (name : con, con : con) : exp * con =
	let
	    val _ = debugdo (fn () => (print "---- XEQ:\n";
				       print "name = "; pp_con name; print "\n";
				       print "con  = "; pp_con con; print "\n\n"))
	    val xeq = xeq state
	    val self = xeq ctxt
	    open Prim
	in  case con
	      of CON_TYVAR tyvar => (case (tyvar_deref tyvar, tyvar_eq_hole tyvar)
				       of (NONE, NONE) => elab_error "unresolved type does not permit equailty"
					| (NONE, SOME os) => (* hole is empty since tyvar is unset *)
					   let val eq_con = con_eqfun con
					       val exp = OVEREXP(eq_con,true,os)
					   in  (exp,eq_con)
					   end
					| (SOME c, SOME os) => (valOf (oneshot_deref os), con_eqfun c)
					| (SOME c, NONE) => self(SOME name,c))
				        (* In the last case, we aren't filling the hole since the side
					 * effect can't be undone and isn't always appropriate. *)
	       | CON_VAR v => (let val SOME(type_label,pc) = Context_Lookup_Var(ctxt,v) 
				   val eq_label = to_eq type_label
			       in (case (Context_Lookup_Label(ctxt,eq_label)) of
				       SOME(_,PHRASE_CLASS_EXP(e,_,_,_)) => (e, con_eqfun con)
				     | _ => (case pc of
						 PHRASE_CLASS_CON(_,_,SOME c,_) => self(SOME name,c)
					       | _ => (debugdo (fn () =>
								(print "No eq expression available for CON_VAR "; pp_var v;
								 print " at label "; pp_label eq_label;
								 print ".  Perhaps due to shadowing\n"));
						       raise NoEqExp)))
			       end)
	       | CON_OVAR ocon => self (SOME name,CON_TYVAR (ocon_deref ocon))
	       | CON_INT is => (ETAPRIM(eq_int is,[]), con_eqfun con)
	       | CON_UINT is => (ETAILPRIM(eq_uint is,[]), con_eqfun con)
	       | CON_FLOAT fs => raise NoEqExp
	       | CON_RECORD fields => 
		  let 
		      val v = fresh_var()
		      val v1 = fresh_var()
		      val v2 = fresh_var()
		      val paircon = con_tuple[name,name]
		      val e1 = RECORD_PROJECT(VAR v,generate_tuple_label 1,paircon)
		      val e2 = RECORD_PROJECT(VAR v,generate_tuple_label 2,paircon)
		      fun help (lbl,fieldcon) = 
			  let 
			      val (eqexp,_) = self (NONE,fieldcon)
			      val e1 = RECORD_PROJECT(VAR v1,lbl,con)
			      val e2 = RECORD_PROJECT(VAR v2,lbl,con)
			      val exp = APP(eqexp,exp_tuple[e1,e2])
			  in  (case exp_reduce exp of
				   NONE => exp
				 | SOME e => e)
			  end
		      fun folder (rdec,exp) = 
			  let val exp' = help rdec
			  in make_ifthenelse(exp,exp',false_exp,con_bool)
			  end
		      val body = (case fields of
				      [] => true_exp
				    | (fst::rest) => foldl folder (help fst) rest)
		  in make_total_lambda(v,paircon,con_bool,
				       make_let([BND_EXP(v1,e1),BND_EXP(v2,e2)],body))
		  end
	       | CON_SUM {names,carrier,noncarriers,special = SOME _} => 
		  error "xeq called on special sum type"
	       | CON_SUM {names,carrier,noncarriers,special = NONE} =>
		  let
		      val v = fresh_named_var "eqargpair"
		      val v1 = fresh_named_var "eqarg1"
		      val v2 = fresh_named_var "eqarg2"
		      val paircon = con_tuple[name,name]
		      val e1 = RECORD_PROJECT(VAR v,generate_tuple_label 1,paircon)
		      val e2 = RECORD_PROJECT(VAR v,generate_tuple_label 2,paircon)
		      val carriers =  
			  (case (con_head_normalize(ctxt,carrier)) of 
			       CON_TUPLE_INJECT [] => []
			     | CON_TUPLE_INJECT clist => clist
			     | c => [c])
		      val totalcount = (noncarriers + length carriers)
		      val var' = fresh_named_var "eqarg1"
		      val var'' = fresh_named_var "eqarg2"
		      fun help i = let 
				       val is_carrier = i >= noncarriers
				       val sumv = fresh_named_var "sumc"
				       val sumbnd = BND_CON(sumv, CON_SUM{names=names,
									  carrier=carrier,
									  noncarriers=noncarriers,
									  special = SOME i})
				       val sumc = CON_VAR sumv
				       val armbody = if is_carrier
							 then 
							     let val c = List.nth(carriers,i-noncarriers)
								 val (eqexp,_) = self(NONE,c)
								 val e' = SUM_TAIL(i,sumc,VAR var')
								 val e'' = SUM_TAIL(i,sumc,VAR var'')
								 val exp = make_let ([sumbnd],APP(eqexp,exp_tuple[e',e'']))
							     in  (case exp_reduce exp of
								      SOME e => e
								    | NONE => exp)
							     end
						     else true_exp
				       val arms2 = map0count 
					   (fn j =>
					    if (i=j) 
						then 
						    SOME armbody
					    else NONE) totalcount
				       val switch = CASE{sumtype = name,
							 arg = VAR v2,
							 bound = var'',
							 arms = arms2,
							 default = SOME false_exp,
							 tipe = con_bool}
				   in SOME switch
				   end
		      val arms1 = map0count help totalcount
		      val body = CASE{bound = var',
				      sumtype = name,
				      arg = VAR v1,
				      arms = arms1,
				      default = NONE,
				      tipe = con_bool}
		  in (make_total_lambda(v,paircon,con_bool,
					make_let([BND_EXP(v1,e1),BND_EXP(v2,e2)],body)))
		  end
	       | CON_ARRAY c => (ETAPRIM(equal_table (OtherArray false),[c]),
				 con_eqfun con)
	       | CON_VECTOR c => 
		  let val (e,vc) = vector_eq ctxt
		      val ac = CON_ARROW([con_eqfun c], 
					 con_eqfun (CON_VECTOR c),
					 false, oneshot())
		      val _ = if (eq_con(ctxt,vc,ac))
				  then ()
			      else (elab_error "Prelude vector_eq is bad")
		      val exp = APP(e, #1 (self (NONE,c)))
		  in  ((case exp_reduce exp of
			    NONE => exp
			  | SOME e => e),
		       con_eqfun con)
		  end
	       | CON_REF c => (ETAILPRIM(eq_ref,[c]), con_eqfun con)
	       | CON_MODULE_PROJECT(m,l) => 
		  let val e = MODULE_PROJECT(m,to_eq l)
		  in (GetExpCon(ctxt,e) 
		      handle _ => raise NoEqExp);
		      (e, con_eqfun con)
		  end
	       | CON_APP(c,types) => 
		  let val meq = 
		      (case c of
			   CON_MODULE_PROJECT(m,l) => 
			       let val SIGNAT_SELF(_,_,s) = GetModSig(ctxt,m)
				   val eql = to_eq l
			       in  case s of
				   SIGNAT_STRUCTURE sdecs =>
				       if (List.exists (fn (SDEC(l,_)) => eq_label(l,eql)) sdecs)
					   then MOD_PROJECT(m,to_eq l)
				       else raise NoEqExp
				     | _ => raise NoEqExp
			       end
			 | CON_VAR v => 
			       (let val SOME (type_label,_) = Context_Lookup_Var(ctxt,v)
				    val eq_label = to_eq type_label
				in (case (Context_Lookup_Label(ctxt,eq_label)) of
					SOME(_,PHRASE_CLASS_MOD(m,_,_)) => m
				      | _ => raise NoEqExp)
				end)
			 | _ => raise NoEqExp)
		      val SIGNAT_SELF(_,_,s) = GetModSig(ctxt,meq)
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
			      val (new_sbnds,new_sdecs,new_types) = (case polyinst_opt(ctxt,sdecs) of
									 NONE => raise NoEqExp
								       | SOME triple => triple)
			  in (MODULE_PROJECT(MOD_APP(meq,MOD_STRUCTURE new_sbnds),it_lab),
			      con_eqfun con)
			  end
		      | _ => raise NoEqExp
		  end
	       | CON_MU confun => xeq_mu state ctxt (SOME name,confun)
	       | CON_TUPLE_PROJECT (j, con_mu as CON_MU confun) => 
		  let val (fix_exp,fix_con) = xeq_mu state ctxt (NONE,confun)
		      val con_res = con_eqfun con
		      val exp_res = 
			  (case confun of
			       CON_FUN ([_],_) => fix_exp
			     | _ => RECORD_PROJECT(fix_exp, generate_tuple_label (j+1), con_res))
		  in  (exp_res, con_res)
		  end
	       | _ => raise NoEqExp
	end
	
    and xeq_mu state ctxt (munameopt, confun) =
	maybe_bind_con (munameopt, CON_MU confun,
			fn muname => xeq_mu_step state ctxt (muname, confun))

    and xeq_mu_step (state as {polyinst_opt, vector_eq, con_bool, true_exp, false_exp})
	            (ctxt : IlContext.context)
		    (name : con, confun : con) : exp * con =
	let
	    val _ = debugdo (fn () => (print "---- XEQ_MU:\n";
				       print "name   = "; pp_con name; print "\n";
				       print "confun = "; pp_con confun; print "\n\n"))
	    val xeq = xeq state
	    val confunKind = GetConKind(ctxt,confun) 
	    val arity = 
		(case confunKind of
		     KIND_ARROW (m,KIND_TUPLE n) => if (m = n) then SOME m else NONE
		   | _ => NONE)
	    val arity = (case arity of
			     NONE =>
				 (print "confun of bad kind: "; pp_kind confunKind; print "\n\n";
				  elab_error "xeq_mu given confun of bad kind")
			   | SOME arity => arity)
	      
	    val name_cons = map0count (fn i => CON_TUPLE_PROJECT(i,name)) arity
	    val (mu_cons, expanded_cons) = 
		(case confun of
		     CON_FUN(vdts,CON_TUPLE_INJECT cons) =>
			 (map0count (fn i => CON_TUPLE_PROJECT(i,CON_MU confun)) arity,
			  map (fn c => con_subst(c,list2subst([], zip vdts name_cons,[]))) cons)
		       | _ => error "xeq_mu given confun which is not CON_FUN returning CON_TUPLE")
	    val expanded_cons_vars = map0count (fn i => fresh_named_var ("expanded_con_" ^ (Int.toString i))) arity
	    val vars_eq = map0count (fn i => fresh_named_var ("vars_eq_" ^ (Int.toString i))) arity
	    val type_lbls = map0count (fn i => fresh_internal_label("type" ^ (Int.toString i))) arity
	    val evars = map0count (fn i => fresh_named_var ("evar" ^ (Int.toString i))) arity
	    val cvars = map0count (fn i => fresh_named_var ("cvar" ^ (Int.toString i))) arity
	    val eq_lbls = map to_eq type_lbls
	    val subst = list2subst(zip evars (map VAR vars_eq),
				   zip cvars name_cons, [])
	    fun cfolder ((cvar,cl),ctxt) = 
		let val dec = DEC_CON(cvar,KIND,NONE,false)
		in add_context_sdec(ctxt,SDEC(cl,SelfifyDec ctxt dec))
		end
	    fun efolder ((evar,cvar,el),ctxt) = 
		let 
		    val con = con_eqfun (CON_VAR cvar)
		    val dec = DEC_EXP(evar,con,NONE,false)
		in add_context_sdec(ctxt,SDEC(el,SelfifyDec ctxt dec))
		end
	    val ctxt = foldl cfolder ctxt (zip cvars type_lbls)
	    val ctxt = foldl efolder ctxt (zip3 evars cvars eq_lbls)
	      
	    val reduced_cons = (case (ConApply(false,confun,map CON_VAR cvars)) of
				    CON_TUPLE_INJECT conlist => conlist
				  | _ => error "body of confun not a CON_TUPLE")
	      
	    fun make_fbnd (name_con,mu_con,expanded_con_var,expanded_con,expv,var_eq) = 
		let
		    val var = fresh_named_var "arg_pair"
		    val var_con = con_tuple[name_con,name_con]
		    val expv' = exp_subst(expv,subst)
		    val e1 = RECORD_PROJECT(VAR var,generate_tuple_label 1,var_con)
		    val e2 = RECORD_PROJECT(VAR var,generate_tuple_label 2,var_con)
		    val e1' = COERCE(UNFOLD([], name_con, CON_VAR expanded_con_var), [], e1)
		    val e2' = COERCE(UNFOLD([], name_con, CON_VAR expanded_con_var), [], e2)
		    val exp = APP(expv',exp_tuple[e1',e2'])
		    val exp = (case exp_reduce exp of
				   NONE => exp
				 | SOME e => e)
		    val exp = make_let([BND_CON(expanded_con_var,expanded_con)],exp)
		    val fbnd = FBND(var_eq,var,var_con,con_bool,exp)
		in  (fbnd, con_eqfun mu_con)
		end

	    val exps_v = map2 (fn (v,c) => #1(xeq ctxt (SOME (CON_VAR v),c))) (expanded_cons_vars,reduced_cons)
	    val (fbnds,cons) = Listops.unzip(map6 make_fbnd (name_cons,mu_cons,expanded_cons_vars,
							     expanded_cons,exps_v,vars_eq))
		
	in  (FIX(true,TOTAL,fbnds), 
	     (case cons of  (* FIX does not return a record when there is only one function *)
		  [c] => c
		| _ => con_tuple cons))
	end

    fun compile ({polyinst_opt : context * sdecs -> (sbnd list * sdecs * con list) option,
		  vector_eq : context -> exp * con,
		  context : IlContext.context,
		  con : con}) : (exp * con) option = 
	let val _ = debugdo (fn () => (print "equality compile called with con = ";
				       pp_con con; print "\n"))
	in
	    SOME (bind_con "bool" con_bool
		  (fn b => bind_exp "true" true_exp
		   (fn t => bind_exp "false" false_exp
		    (fn f => let
				 val state : state = {polyinst_opt = polyinst_opt,
						      vector_eq = vector_eq,
						      con_bool = b,
						      true_exp = t,
						      false_exp = f}
			     in  xeq state context (NONE, con)
			     end))))
	    handle NoEqExp => NONE
	end 

end
