(*$import Il IlStatic IlUtil Ppil IlContext EQUAL Stats *)
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




    fun xeq (polyinst_opt : context * sdecs -> 
			 (sbnd list * sdecs * con list) option,
	     vector_eq : context -> exp * con)
	    (ctxt : IlContext.context)
	    (name : con, con : con) : exp * con = 
      let
	  val _ = debugdo (fn () => (print "CALLED xeq with con = ";
				     pp_con con; print "\nand ctxt = \n";
				     pp_context ctxt))
(*
	  val con = con_normalize(ctxt,con) 
	  val _ = debugdo (fn () => (print "NORMALIZE to con = ";
				     pp_con con'; print "\n"))
*)
	  val xeq = xeq(polyinst_opt,vector_eq)
	  val self = xeq ctxt
	  open Prim
      in case con of
	    CON_TYVAR tyvar => (case (tyvar_deref tyvar) of
				NONE => elab_error "unresolved type does not permit equailty"
			      | SOME c => self (name,c))
	  | CON_VAR v => (let val (type_label,pc) = (case Context_Lookup'(ctxt,v) of
							 SOME p => p
						       | _ => raise NoEqExp)
			      val eq_label = to_eq type_label
			  in (case (Context_Lookup(ctxt,eq_label)) of
				  SOME(_,PHRASE_CLASS_EXP(e,_,_,_)) => (e, con_eqfun name)
				| _ => (case pc of
					    PHRASE_CLASS_CON(_,_,SOME c,_) => self(con,c)
					  | _ => raise NoEqExp))
			  end)
	  | CON_OVAR ocon => self (name,CON_TYVAR (ocon_deref ocon))
	  | CON_INT is => (ETAPRIM(eq_int is,[]), con_eqfun con)
	  | CON_UINT is => (ETAILPRIM(eq_uint is,[]), con_eqfun con)
	  | CON_FLOAT fs => raise NoEqExp
	  | CON_RECORD fields => 
		let 
		    val v = fresh_var()
		    val v1 = fresh_var()
		    val v2 = fresh_var()
		    val paircon = con_tuple[con,con]
		    val e1 = RECORD_PROJECT(VAR v,generate_tuple_label 1,paircon)
		    val e2 = RECORD_PROJECT(VAR v,generate_tuple_label 2,paircon)
		    fun help (lbl,fieldcon) = 
			let 
			    val (eqexp,_) = self (fieldcon,fieldcon)
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
				     val sumc = CON_SUM{names=names,
							carrier=carrier,
							noncarriers=noncarriers,
							special = SOME i}
				     val armbody = if is_carrier
						       then 
							   let val c = List.nth(carriers,i-noncarriers)
							       val (eqexp,_) = self(c,c)
							       val e = SUM_TAIL(i,sumc,VAR var')
							       val exp = APP(eqexp,exp_tuple[e,e])
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
		    val exp = APP(e, #1 (self (c,c)))
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
	  | CON_APP(c,tuple) => 
		let val meq = 
		    (case c of
			 CON_MODULE_PROJECT(m,l) => 
			     let val s = GetModSig(ctxt,m)
				 val eql = to_eq l
			     in  case s of
				 SIGNAT_STRUCTURE(_,sdecs) =>
				     if (List.exists (fn (SDEC(l,_)) => eq_label(l,eql)) sdecs)
					 then MOD_PROJECT(m,to_eq l)
				     else raise NoEqExp
				   | _ => raise NoEqExp
			     end
		       | CON_VAR v => 
			     (let val type_label = (case (Context_Lookup'(ctxt,v)) of
							SOME(l,_) => l
						      | _ => raise NoEqExp)
				  val eq_label = to_eq type_label
			      in (case (Context_Lookup(ctxt,eq_label)) of
				      SOME(_,PHRASE_CLASS_MOD(m,_,_)) => m
				    | _ => raise NoEqExp)
			      end)
		       | _ => raise NoEqExp)
		in case (GetModSig(ctxt,meq)) of
		    SIGNAT_FUNCTOR(_,SIGNAT_STRUCTURE (NONE, sdecs),
				   SIGNAT_STRUCTURE(NONE, [res_sdec]),_) => 
			let 
			    val types = (case tuple of
					     CON_TUPLE_INJECT cons => cons
					   | c => [c])
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
	  | CON_MU confun => xeq_mu(polyinst_opt,vector_eq) ctxt (name,confun)
	  | CON_TUPLE_PROJECT (j,CON_MU confun) => 
		let val (fix_exp,fix_con) = xeq_mu(polyinst_opt,vector_eq) ctxt (CON_MU confun,confun)
		in  (RECORD_PROJECT(fix_exp, generate_tuple_label (j+1), fix_con), con_eqfun con)
		end
	  | _ => raise NoEqExp
      end



      and xeq_mu (polyinst_opt : context * sdecs -> 
			      (sbnd list * sdecs * con list) option,
		  vector_eq : context -> exp * con)
	  (ctxt : IlContext.context)
	  (name : con, confun : con) : exp * con =
      let
	  val xeq = xeq (polyinst_opt,vector_eq)
	  val arity = 
	      (case GetConKind(ctxt,confun) of
		   KIND_TUPLE _ => elab_error "cannot perform equality on con tuples"
		 | KIND_ARROW (m,n) => if (m = n) then m
				       else elab_error "xeq_mu given confun not of kind n => n")
	  val name_cons = 
	      if (arity = 1)
		  then [name]
	      else map0count (fn i => CON_TUPLE_PROJECT(i,name)) arity
	  val mu_cons = 
	      (case confun of
		   CON_FUN(vdts,CON_TUPLE_INJECT cons) => 
		       map0count (fn i => CON_TUPLE_PROJECT(i,CON_MU confun)) arity
		 | CON_FUN([vdt], con) => [CON_MU confun]
		 | _ => error "xeq_mu given confun which is not CON_FUN")
	  val expanded_cons = 
	      (case confun of
		   CON_FUN(vdts,CON_TUPLE_INJECT cons) => 
		       map (fn c => con_subst(c,list2subst([], zip vdts mu_cons,[]))) cons
		 | CON_FUN([vdt], con) => [con_subst(con,list2subst([], zip [vdt] mu_cons, []))]
		 | _ => error "xeq_mu given confun which is not CON_FUN")
	  val vars_eq = map0count (fn i => fresh_named_var ("vars_eq_" ^ (Int.toString i))) arity
	  val type_lbls = map0count (fn i => fresh_internal_label("lbl" ^ (Int.toString i))) arity
	  val evars = map0count (fn i => fresh_named_var ("evar" ^ (Int.toString i))) arity
	  val cvars = map0count (fn i => fresh_named_var ("cvar" ^ (Int.toString i))) arity
	  val eq_lbls = map to_eq type_lbls
	  val subst = list2subst(zip evars (map VAR vars_eq),
				 zip cvars mu_cons, [])
	  fun cfolder ((cvar,cl),ctxt) = 
	      let val dec = DEC_CON(cvar,KIND_TUPLE 1,NONE,false)
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
	      
	  local
	      val applied = ConApply(false,confun,con_tuple_inject (map CON_VAR cvars))
	  in
	      val reduced_cons = (case applied of
				      CON_TUPLE_INJECT conlist => conlist
				    | c => [c])
	  end
	  val exps_v = map (fn c => #1(xeq ctxt (c,c))) reduced_cons
	      
	  fun make_fbnd (name_con,expanded_con,expv,var_eq) = 
	      let
		  val var = fresh_named_var "arg_pair"
		  val var_con = con_tuple[name_con,name_con]
		  val expv' = exp_subst(expv,subst)
		  val e1 = RECORD_PROJECT(VAR var,generate_tuple_label 1,var_con)
		  val e2 = RECORD_PROJECT(VAR var,generate_tuple_label 2,var_con)
		  val e1' = UNROLL(name_con,expanded_con,e1)
		  val e2' = UNROLL(name_con,expanded_con,e2)
		  val exp = APP(expv',exp_tuple[e1',e2'])
		  val exp = (case exp_reduce exp of
				 NONE => exp
			       | SOME e => e)
		  val fbnd = FBND(var_eq,var,var_con,con_bool,exp)
	      in  (fbnd, con_eqfun name_con)
	      end
	  val (fbnds,cons) = Listops.unzip(map4 make_fbnd (name_cons,expanded_cons,exps_v,vars_eq))
      in  (FIX(true,TOTAL,fbnds), con_tuple cons)
      end

    fun fbnd2type (FBND(_,_,con,rescon,_)) = CON_ARROW([con],rescon,false,oneshot_init PARTIAL)

    fun compile ({polyinst_opt : context * sdecs -> 
			     (sbnd list * sdecs * con list) option,
		  vector_eq : context -> exp * con,
		  context : IlContext.context,
		  con : con}) : (exp * con) option = 
	let val eqexp_con = xeq (polyinst_opt,vector_eq) context (con,con)
	in  SOME eqexp_con
	end
	handle NoEqExp => NONE


end