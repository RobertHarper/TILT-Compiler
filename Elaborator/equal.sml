(*$import Il ILSTATIC ILUTIL PPIL ILCONTEXT EQUAL *)
(* Equality compiler *)
functor Equal(structure IlStatic : ILSTATIC
	      structure IlUtil : ILUTIL
	      structure IlContext : ILCONTEXT 
	      structure Ppil : PPIL)
    :> EQUAL =
struct

    open Il IlStatic IlUtil Ppil 
    open Util Listops Name IlContext Tyvar

    val elab_error = fn s => error "equal.sml: elaborator impossibility" s
    val error = fn s => error "equal.sml" s
    val debug = ref false
    fun debugdo t = if (!debug) then (t(); ()) else ()

    fun con_normalize (arg as (ctxt,con)) = IlStatic.con_normalize arg handle e => con
    fun con_head_normalize (arg as (ctxt,con)) = IlStatic.con_head_normalize arg handle e => con

    exception NoEqExp

    fun app(x,y) = (case (IlUtil.beta_reduce(x,y)) of
			NONE => APP(x,[y])
		      | SOME e => e)

    fun xeq (polyinst_opt : context * sdecs -> 
			 (sbnd list * sdecs * con list) option,
	     vector_eq : context -> exp * con)
	    (ctxt : IlContext.context)
	    (con : con) : exp =
      let
	  val _ = debugdo (fn () => (print "CALLED xeq with con = ";
				     pp_con con; print "\nand ctxt = \n";
				     pp_context ctxt))
	  val con' = con_normalize(ctxt,con) 
	  val _ = debugdo (fn () => (print "NORMALIZE to con = ";
				     pp_con con'; print "\n"))
	  val xeq = xeq(polyinst_opt,vector_eq)
	  val self = xeq ctxt
	  open Prim
      in case con' of
	    CON_TYVAR tyvar => (case (tyvar_deref tyvar) of
				NONE => elab_error "unresolved type does not permit equailty"
			      | SOME c => self c)
	  | CON_VAR v => (let val type_label = (case (Context_Lookup'(ctxt,v)) of
						    SOME(l,_) => l
						  | _ => raise NoEqExp)
			      val eq_label = to_eq_lab type_label
			  in (case (Context_Lookup(ctxt,eq_label)) of
				  SOME(_,PHRASE_CLASS_EXP(e,_)) => e
				| _ => raise NoEqExp)
			  end)
	  | CON_OVAR ocon => self (CON_TYVAR (ocon_deref ocon))
	  | CON_INT is => ETAPRIM(eq_int is,[])
	  | CON_UINT is => ETAILPRIM(eq_uint is,[])
	  | CON_FLOAT fs => raise NoEqExp
	  | CON_RECORD fields => 
		let 
		    val v = fresh_var()
		    val v1 = fresh_var()
		    val v2 = fresh_var()
		    val paircon = con_tuple[con',con']
		    val e1 = RECORD_PROJECT(VAR v,generate_tuple_label 1,paircon)
		    val e2 = RECORD_PROJECT(VAR v,generate_tuple_label 2,paircon)
		    fun help (lbl,fieldcon) = 
			let 
			    val eqexp = self fieldcon
			    val e1 = RECORD_PROJECT(VAR v1,lbl,con')
			    val e2 = RECORD_PROJECT(VAR v2,lbl,con')
			in  app(eqexp,exp_tuple[e1,e2])
			end
		    fun folder (rdec,exp) = 
			let val exp' = help rdec
			in make_ifthenelse(exp,exp',false_exp,con_bool)
			end
		    val body = (case fields of
				    [] => true_exp
				  | (fst::rest) => foldl folder (help fst) rest)
		in #1(make_total_lambda(v,paircon,con_bool,
					make_let([BND_EXP(v1,e1),BND_EXP(v2,e2)],body)))
		end
	  | CON_SUM {carrier,noncarriers,special} =>
		let 
		    val v = fresh_named_var "eqargpair"
		    val v1 = fresh_named_var "eqarg1"
		    val v2 = fresh_named_var "eqarg2"
		    val paircon = con_tuple[con',con']
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
				     val sumc = CON_SUM{carrier=carrier,
							noncarriers=noncarriers,
							special = SOME i}
				     val armbody = if is_carrier
						       then app(self(List.nth(carriers,i-noncarriers)),
								exp_tuple[SUM_TAIL(sumc,VAR var'),
									  SUM_TAIL(sumc,VAR var'')])
						   else true_exp
				     val arms2 = map0count 
					 (fn j =>
					  if (i=j) 
					      then 
						  SOME armbody
					  else NONE) totalcount
				     val switch = CASE{sumtype = 
						       CON_SUM{noncarriers = noncarriers,
							       carrier = carrier,
							       special = NONE},
						       arg = VAR v2,
						       bound = var'',
						       arms = arms2,
						       default = SOME false_exp,
						       tipe = con_bool}
				 in SOME switch
				 end
		    val arms1 = map0count help totalcount
		    val body = CASE{bound = var',
				    sumtype = CON_SUM{noncarriers = noncarriers,
						      carrier = carrier,
						      special = NONE},
				    arg = VAR v1,
				    arms = arms1,
				    default = NONE,
				    tipe = con_bool}
(*  val body = make_catch(inner_body,con_bool,match_exp,con_unit,false_exp) *)
		in #1(make_total_lambda(v,paircon,con_bool,
				  make_let([BND_EXP(v1,e1),BND_EXP(v2,e2)],body)))
		end
	  | CON_ARRAY c => ETAPRIM(equal_table WordArray,[c])
(*	  | CON_VECTOR c => APP(ETAPRIM(equal_table WordVector,[c]),self c) *)
	  | CON_VECTOR c => 
		let val (e,vc) = vector_eq ctxt
		    val ac = CON_ARROW([CON_ARROW([con_tuple[c,c]],con_bool,false,oneshot())],
				       CON_ARROW([con_tuple[CON_VECTOR c,CON_VECTOR c]],con_bool,false,oneshot()),
				       false, oneshot())
		    val _ = if (eq_con(ctxt,vc,ac))
				then ()
			    else (elab_error "Prelude vector_eq is bad")
		in  app(e,self c) 
		end
	  | CON_REF c => ETAPRIM(eq_ref,[c])
	  | CON_MODULE_PROJECT(m,l) => 
		let val e = MODULE_PROJECT(m,to_eq_lab l)
		in (GetExpCon(ctxt,e) 
			handle _ => raise NoEqExp);
		  	e
	       end
	  | CON_APP(c,tuple) => 
		let val meq = 
		    (case c of
			 CON_MODULE_PROJECT(m,l) => 
			     let val s = GetModSig(ctxt,m)
				 val eql = to_eq_lab l
			     in  case s of
				 SIGNAT_STRUCTURE(_,sdecs) =>
				     if (List.exists (fn (SDEC(l,_)) => eq_label(l,eql)) sdecs)
					 then MOD_PROJECT(m,to_eq_lab l)
				     else raise NoEqExp
				   | _ => raise NoEqExp
			     end
		       | CON_VAR v => 
			     (let val type_label = (case (Context_Lookup'(ctxt,v)) of
							SOME(l,_) => l
						      | _ => raise NoEqExp)
				  val eq_label = to_eq_lab type_label
			      in (case (Context_Lookup(ctxt,eq_label)) of
				      SOME(_,PHRASE_CLASS_MOD(m,_)) => m
				    | _ => raise NoEqExp)
			      end))
		in case (GetModSig(ctxt,meq)) of
		    SIGNAT_FUNCTOR(_,SIGNAT_STRUCTURE (NONE, sdecs),
				   SIGNAT_STRUCTURE(NONE, [res_sdec]),_) => 
			let 
			    val types = (case tuple of
					     CON_TUPLE_INJECT cons => cons
					   | c => [c])
			    fun translucentfy [] [] = []
			      | translucentfy [] _ = elab_error "arity mismatch in eq compiler"
			      | translucentfy ((SDEC(l,DEC_CON(v,k,NONE)))::
					       (sdec2 as (SDEC(_,DEC_EXP _))) :: rest) (c::crest) =
				((SDEC(l,DEC_CON(v,k,SOME c)))::sdec2::
				 (translucentfy rest crest))
			      | translucentfy ((SDEC(l,DEC_CON(v,k,NONE)))::rest) (c::crest) = 
				((SDEC(l,DEC_CON(v,k,SOME c)))::(translucentfy rest crest))
			      | translucentfy _ _ = elab_error "got strange sdec in eq compiler"
			    val sdecs = translucentfy sdecs types
			    val (new_sbnds,new_sdecs,new_types) = (case polyinst_opt(ctxt,sdecs) of
								       NONE => raise NoEqExp
								     | SOME triple => triple)
			in MODULE_PROJECT(MOD_APP(meq,MOD_STRUCTURE new_sbnds),it_lab)
			end
		  | _ => raise NoEqExp
		end
	  | CON_MU confun => 
		let val fix_exp as FIX _ = xeq_mu(polyinst_opt,vector_eq) ctxt confun
		in  fix_exp
	 	end
	  | CON_TUPLE_PROJECT (j,CON_MU confun) => 
		let val fix_exp as FIX(_,_,fbnds) = xeq_mu(polyinst_opt,vector_eq) ctxt confun
		    fun mapper i = let val mu_con = CON_TUPLE_PROJECT(i,CON_MU confun)
				   in  CON_ARROW([con_tuple[mu_con,mu_con]],
						 con_bool,false,
						 oneshot_init PARTIAL)
				   end
		    val fbnd_types = Listops.map0count mapper (length fbnds)
		in  case fbnd_types of
		    [_] => error "CON_TUPLE_PROJECT of CON_MU of kind 1->1"
		  | _ => RECORD_PROJECT(fix_exp, generate_tuple_label (j+1),
					con_tuple fbnd_types)
		end
	  | _ => raise NoEqExp
      end



      and xeq_mu (polyinst_opt : context * sdecs -> 
			      (sbnd list * sdecs * con list) option,
		  vector_eq : context -> exp * con)
	  (ctxt : IlContext.context)
	  (confun : con) : exp =
      let
	  val xeq = xeq (polyinst_opt,vector_eq)
	  val (n,m) = 
	      (case GetConKind(ctxt,confun) of
		   KIND_TUPLE _ => elab_error "cannot perform equality on con tuples"
		 | KIND_ARROW nm => nm)
	  val _ = if (m=n) then () else elab_error "datatype constructor must have kind n=>n"
	  val mu_cons = 
	      (case confun of
		   CON_FUN(vdts,CON_TUPLE_INJECT cons) => 
		       map0count (fn i => CON_TUPLE_PROJECT(i,CON_MU confun)) n
		 | CON_FUN([vdt], con) => [CON_MU confun]
		 | _ => error "xeq_mu given confun which is not CON_FUN")
	  val expanded_cons = 
	      (case confun of
		   CON_FUN(vdts,CON_TUPLE_INJECT cons) => 
		       map (fn c => con_subst_convar(c,zip vdts mu_cons)) cons
		 | CON_FUN([vdt], con) => [con_subst_convar(con,zip [vdt] mu_cons)]
		 | _ => error "xeq_mu given confun which is not CON_FUN")
	  val vars_eq = map0count (fn i => fresh_named_var ("vars_eq_" ^ (Int.toString i))) n
	  val type_lbls = map0count (fn i => fresh_internal_label("lbl" ^ (Int.toString i))) n
	  val eq_lbls = map to_eq_lab type_lbls
	  val evars = map0count (fn i => fresh_named_var ("evar" ^ (Int.toString i))) n
	  val cvars = map0count (fn i => fresh_named_var ("cvar" ^ (Int.toString i))) n
	  val elist = zip evars (map VAR vars_eq)
	  val clist = zip cvars mu_cons
	  fun cfolder ((cvar,cl),ctxt) = 
	      let val dec = DEC_CON(cvar,KIND_TUPLE 1,NONE)
	      in add_context_sdec(ctxt,SDEC(cl,SelfifyDec ctxt dec))
	      end
	  fun efolder ((evar,cvar,el),ctxt) = 
	      let 
		  val con = CON_ARROW([con_tuple[CON_VAR cvar, CON_VAR cvar]],
				      con_bool,false,oneshot_init PARTIAL)
		  val dec = DEC_EXP(evar,con)
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
	  val exps_v = map (xeq ctxt) reduced_cons
	      
	  fun make_expeq (mu_con,expanded_con,expv) = 
	      let
		  val var = fresh_named_var "arg_pair"
		  val var_con = con_tuple[mu_con,mu_con]
		  val expv' = exp_subst_expconmodvar(expv,elist,clist,[])
		  val e1 = RECORD_PROJECT(VAR var,generate_tuple_label 1,var_con)
		  val e2 = RECORD_PROJECT(VAR var,generate_tuple_label 2,var_con)
		  val e1' = UNROLL(mu_con,expanded_con,e1)
		  val e2' = UNROLL(mu_con,expanded_con,e2)
		  val exp = app(expv',exp_tuple[e1',e2'])
	      in (var,exp)
	      end
	  val exps_eq = map3 make_expeq (mu_cons,expanded_cons,exps_v)
	  val fbnds = map3 (fn (mu_con,vareq,(vararg,expeq)) =>
			    FBND(vareq,vararg,con_tuple[mu_con,mu_con],
				 con_bool,expeq))
	      (mu_cons,vars_eq,exps_eq)
	  val fbnd_types = 
	      map (fn mu_con => CON_ARROW([con_tuple[mu_con,mu_con]],
					  con_bool,false,
					  oneshot_init PARTIAL))
	      mu_cons
      in FIX(true,TOTAL,fbnds)
      end

    fun compile ({polyinst_opt : context * sdecs -> 
			     (sbnd list * sdecs * con list) option,
		  vector_eq : context -> exp * con,
		  context : IlContext.context,
		  con : con}) : exp option = 
	(SOME (xeq (polyinst_opt,vector_eq) context con))
	handle NoEqExp => NONE

    fun compile_mu ({polyinst_opt : context * sdecs -> 
				 (sbnd list * sdecs * con list) option,
		     vector_eq : context -> exp * con,
		     context : IlContext.context,
		     confun : con}) : exp option = 
	(SOME (xeq_mu (polyinst_opt,vector_eq) context confun))
	handle NoEqExp => NONE
	



end