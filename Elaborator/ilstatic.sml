(*$import IL ILCONTEXT PRIMUTIL PPIL ILUTIL Util Listops ILSTATIC *)
(* Static semantics *)
functor IlStatic(structure Il : IL
		 structure IlContext : ILCONTEXT
		 structure PrimUtil : PRIMUTIL
		 structure Ppil : PPIL
		 structure IlUtil : ILUTIL
		 sharing Ppil.Il = IlUtil.Il = IlContext.Il = Il
		 sharing PrimUtil.Prim = Il.Prim
		 sharing type PrimUtil.con = Il.con
		 sharing type PrimUtil.exp = Il.exp)
  :> ILSTATIC where Il = Il =
  struct


    open Util Listops
    structure Il = Il
    structure PrimUtil = PrimUtil
    open Il Ppil IlUtil 
    open IlContext 
    open Prim Tyvar Name

    val error = fn s => error "ilstatic.sml" s
    val trace = ref false
    val debug = ref false
    fun debugdo t = if (!debug) then (t(); ()) else ()
    fun debugdo' t = t()
    fun fail s = raise (FAILURE s)


   fun mod_ispath (MOD_VAR _) = true
     | mod_ispath (MOD_PROJECT (m,_)) = mod_ispath m
     | mod_ispath _ = false


   fun reduce_sigvar(context,v) = 
	(case (Context_Lookup'(context,v)) of
	     SOME(_,PHRASE_CLASS_SIG(v,s)) => s
	   | NONE => (print "reduce_sigvar given unbound SIGVAR = "; pp_var v;
		      print "in ctxt: \n"; Ppil.pp_context context;
		      error "reduce_sigvar given unbound SIGVAR"))

   datatype phrase = PHRASE_EXP of exp
                    | PHRASE_CON of con
                    | PHRASE_MOD of mod
                    | PHRASE_SIG of var * signat
                    | PHRASE_OVEREXP of (con * exp) list

    datatype class = CLASS_EXP of con
                   | CLASS_CON of kind
                   | CLASS_MOD of signat
                   | CLASS_SIG
                   | CLASS_OVEREXP


    fun merge_phrase_pc (PHRASE_EXP e, PHRASE_CLASS_EXP (_,c)) = PHRASE_CLASS_EXP (e,c)
      | merge_phrase_pc (PHRASE_CON c, PHRASE_CLASS_CON (_,k)) = PHRASE_CLASS_CON (c,k)
      | merge_phrase_pc (PHRASE_MOD m, PHRASE_CLASS_MOD (_,s)) = PHRASE_CLASS_MOD (m,s)
      | merge_phrase_pc (PHRASE_SIG (v,s), PHRASE_CLASS_SIG _) = PHRASE_CLASS_SIG (v,s)

      | merge_phrase_pc (PHRASE_OVEREXP oe, PHRASE_CLASS_OVEREXP _) = PHRASE_CLASS_OVEREXP oe 
      | merge_phrase_pc  _ = error "merge_phrase_pc got a phrase and phraseclass of conflicting flavors"
       
       
   (* --- remove references to internal variables from signature with given module ---- *)
   local
       type state = {ctxt : context,
	             expself : (var * exp) list,
		     conself : (var * con) list,
		     modself : (var * mod) list,
		     expunself : (exp * exp) list,
		     conunself : (con * con) list,
		     modunself : (mod * mod) list}
       fun initial_state ctxt = {ctxt = ctxt,
			         expunself = [],
			         conunself  = [],
			         modunself  = [],
				 expself  = [],
				 conself  = [],
				 modself  = []}
       fun isempty_state ({expself,conself,modself,expunself,conunself,modunself,...}:state) = 
	   (null expself) andalso (null conself) andalso (null modself) andalso
	   (null expunself) andalso (null conunself) andalso (null modunself) 

fun show_state ({modunself,...}:state) = 
    (print "modunself:\n";
     app (fn (m1,m2) => (pp_mod m1; print " --> "; pp_mod m2; print "\n")) modunself;
     print "\n")

       fun add_exppath({ctxt,expunself,conunself,modunself,expself,conself,modself} : state,v,p) = 
	   {ctxt=ctxt,expunself = (path2exp p, VAR v)::expunself,conunself=conunself,modunself=modunself,
	    expself = (v,path2exp p)::expself,
	    conself=conself,modself=modself}
       fun add_conpath({ctxt,expunself,conunself,modunself,expself,conself,modself}:state,v,p) = 
	   {ctxt=ctxt,conunself = (path2con p, CON_VAR v)::conunself,expunself=expunself,modunself=modunself,
	    conself = (v,path2con p)::conself,
	    expself=expself,modself=modself}
       fun add_modpath({ctxt,expunself,conunself,modunself,expself,conself,modself}:state,v,p) = 
	   {ctxt=ctxt,modunself = (path2mod p, MOD_VAR v)::modunself,expunself=expunself,conunself=conunself,
	    modself = (v,path2mod p)::modself,
	    conself=conself,expself=expself}
       fun meta_add adder (state,v,NONE) = state
	 | meta_add adder (state,v,SOME p) = adder(state,v,p)
       val add_exppath = meta_add add_exppath
       val add_conpath = meta_add add_conpath
       val add_modpath = meta_add add_modpath

       fun eq_modproj (MOD_VAR v, MOD_VAR v') = eq_var (v,v')
	 | eq_modproj (MOD_PROJECT (m,l), MOD_PROJECT (m',l')) = eq_label(l,l') andalso eq_modproj(m,m')
	 | eq_modproj _ = false
       fun eq_expproj (VAR v, VAR v') = eq_var (v,v')
	 | eq_expproj (MODULE_PROJECT (m,l), MODULE_PROJECT (m',l')) = eq_label(l,l') andalso eq_modproj(m,m')
	 | eq_expproj _ = false

       fun eq_conproj (CON_VAR v, CON_VAR v') = eq_var (v,v')
	 | eq_conproj (CON_MODULE_PROJECT (m,l), 
		       CON_MODULE_PROJECT (m',l')) = eq_label(l,l') andalso eq_modproj(m,m')
	 | eq_conproj _ = false

       fun ehandler ({expunself,...} : state) (m,l) = assoc_eq(eq_expproj, MODULE_PROJECT(m,l), expunself)
       fun chandler ({conunself,...} : state) (m,l) = 
	   let val res = assoc_eq(eq_conproj, CON_MODULE_PROJECT(m,l), conunself)
(*
	       val _ = (print "(un)-selfify chandler called with\n";
			pp_con (CON_MODULE_PROJECT(m,l));
			print "\nand conunself = ";
			app (fn (c,c') => (pp_con c; print "   -->  "; pp_con c'; print "\n")) conunself;
			print "\nand returning res = ";
			case res of 
			    NONE => print "NONE\n"
			  | SOME c => (print "SOME "; pp_con c; print "\n"))
*)
	   in  res
	   end
       fun mhandler ({modunself,...} : state) (m,l) = assoc_eq(eq_modproj,MOD_PROJECT(m,l), modunself)


       fun selfify_exp(selfify,state as {expself,conself,modself,...}:state,e) = 
	   if selfify
	       then exp_subst_expconmodvar(e,expself,conself,modself)
	   else exp_subst_allproj(e,ehandler state, chandler state, 
				  mhandler state, fn _ => NONE)

       fun selfify_con(selfify,state as {conself,modself,...}:state,c) = 
	   if selfify
	       then con_subst_conmodvar(c,conself,modself)
	   else  con_subst_allproj(c,ehandler state, chandler state, 
				   mhandler state, fn _ => NONE)
       fun selfify_kind(selfify,state,KIND_INLINE(k,c)) = KIND_INLINE(selfify_kind(selfify,state,k),
								      selfify_con(selfify,state,c))
	 | selfify_kind(_,_,k) = k

       fun selfify_mod(selfify,state as {conself,modself,...}:state,m) = 
	   if selfify
	       then mod_subst_conmodvar(m,conself,modself)
	   else  mod_subst_allproj(m,ehandler state, chandler state, 
				   mhandler state, fn _ => NONE)
	       
       fun sdec_folder (popt,selfify) (sdec as (SDEC(l,dec)),(state:state,rev_sdecs)) = 
	   let val popt = mapopt (fn p => join_path_labels(p,[l])) popt
	   in 
	       (case dec of
		    DEC_EXP(v,c) => (state,(SDEC(l,DEC_EXP(v,selfify_con(selfify,state,c))))::rev_sdecs)
		  | DEC_MOD(v,s) => 
		     let val this_dec = DEC_MOD(v,SelfifySig(state,selfify) (popt,s))
			 val state = add_modpath(state,v,popt)
		     in (state,SDEC(l,this_dec)::rev_sdecs)
		     end
		  | DEC_CON(v,k,copt) => 
		     let 
			 val this_dec = 
			 (case copt of  (*  if we do this, Unselfify is hard to write. *)
			      NONE => DEC_CON(v,selfify_kind(selfify,state,k),
					      (case popt of
						   NONE => NONE
						 | SOME p => SOME(path2con p)))
			    | SOME c => 
				  let val c' = selfify_con(selfify,state,c)
				      val copt' = 
					  (if (not selfify andalso 
					       case (c',popt) of
						   (CON_MODULE_PROJECT _, SOME p) => eq_conproj(c', path2con p)
(*
						       (print "l = "; pp_label l; print "\n";
							print "l' = "; pp_label l'; print "\n";
							print "m = "; pp_mod m; print "\n";
							print "p = "; pp_mod (path2mod p); print "\n";
						       eq_label(l,l') andalso eq_modproj(m,path2mod p))
*)
						 | _ => false)
					       then NONE
					   else SOME c')
				      val k' = selfify_kind(selfify,state,k)
				  in  DEC_CON(v,k',copt')
				  end)
			 val state = add_conpath(state,v,popt)
		     in (state,(SDEC(l,this_dec))::rev_sdecs)
		     end
		  | (DEC_EXCEPTION _) => (state,sdec::rev_sdecs))
	   end
		    
       and sbnd_folder (popt,selfify) (SBND(l,bnd),(state:state,rev_sbnds)) = 
	   let val popt = mapopt (fn p => join_path_labels(p,[l])) popt
	   in  (case bnd of
		    BND_EXP(v,e) => 
			let val state = add_exppath(state,v,popt)
			in (state,(SBND(l,BND_EXP(v,selfify_exp(selfify,state,e))))::rev_sbnds)
			end
		  | BND_MOD(v,m) => 
			let val this_bnd = BND_MOD(v,SelfifyMod(state,selfify) (popt,m))
			    val state = add_modpath(state,v,popt)
			in (state,SBND(l,this_bnd)::rev_sbnds)
			end
		  | BND_CON(v,c) =>
			let val this_bnd = BND_CON(v,selfify_con(selfify,state,c))
			    val state = add_conpath(state,v,popt)
			in (state,(SBND(l,this_bnd))::rev_sbnds)
			end)
	   end
       and do_sdecs (popt,selfify) (state:state,sdecs) = 
	   let val (_,rev_sdecs') = foldl (sdec_folder (popt,selfify)) (state,[]) sdecs
	   in  rev rev_sdecs'
	   end
       and do_sbnds (popt,selfify) (state,sbnds) = 
	   let val (_,rev_sbnds') = foldl (sbnd_folder (popt,selfify)) (state,[]) sbnds
	   in  rev rev_sbnds'
	   end
       and SelfifyMod (state:state,selfify) (popt: path option, module : mod) : mod = 
	   let val x = 5
	   in (case module of
		   MOD_FUNCTOR (v,s,m) => 
		       let val s' = SelfifySig (state,selfify) (NONE,s)
			   val m' = SelfifyMod (state,selfify) (NONE,m)
		       in  MOD_FUNCTOR(v,s',m')
		       end
		 | (MOD_STRUCTURE sbnds) =>
		       let val sbnds = do_sbnds (popt,selfify) (state, sbnds)
		       in  MOD_STRUCTURE sbnds
		       end
		 | (MOD_PROJECT (m,l)) => 
		       if selfify
			   then MOD_PROJECT(SelfifyMod (state,selfify) (NONE,m), l)
		       else mod_subst_allproj(module,ehandler state, chandler state, 
					      mhandler state, fn _ => NONE)
		 | (MOD_VAR v) => 
		       if selfify
			   then mod_subst_conmodvar(module,#conself state, #modself state)
		       else mod_subst_allproj(module,ehandler state, chandler state, 
					      mhandler state, fn _ => NONE)
		 | _ => error "selfify_mod' given a mod which is not a functor or structure")
	   end
       and SelfifySig (state:state,selfify) (popt: path option, signat : signat) : signat = 
	   let val x = 5
	   in (case (selfify,signat) of
		   (_,SIGNAT_FUNCTOR (v,s1,s2,a)) => 
		       if (selfify andalso isempty_state state)
			   then signat
		       else let val s1' = SelfifySig (state,selfify) (NONE,s1)
				val s2' = SelfifySig (state,selfify) (NONE,s2)
			    in  SIGNAT_FUNCTOR(v,s1',s2',a)
			    end
		 | (true,SIGNAT_INLINE_STRUCTURE {self=_,code,abs_sig,imp_sig}) =>
		       let val abs_sig = do_sdecs (popt,selfify) (state, abs_sig)
			   val imp_sig = do_sdecs (popt,selfify) (state, imp_sig)
			   val code = do_sbnds (popt,selfify) (state, code)
			   val popt = if selfify then popt else NONE
		       in  SIGNAT_INLINE_STRUCTURE{self=popt,
						   code=code, abs_sig=abs_sig, imp_sig=imp_sig}
		       end
		 | (false,SIGNAT_INLINE_STRUCTURE {self=SOME _,code,abs_sig,imp_sig}) =>
		       let val abs_sig = do_sdecs (popt,selfify) (state, abs_sig)
			   val imp_sig = do_sdecs (popt,selfify) (state, imp_sig)
			   val code = do_sbnds (popt,selfify) (state, code)
			   val popt = if selfify then popt else NONE
		       in  SIGNAT_INLINE_STRUCTURE{self=popt,
						   code=code, abs_sig=abs_sig, imp_sig=imp_sig}
		       end
		 | (_,SIGNAT_STRUCTURE (NONE,sdecs)) =>
		       let val sdecs' = do_sdecs (popt,selfify) (state, sdecs)
			   val popt = if selfify then popt else NONE
		       in  SIGNAT_STRUCTURE(popt,sdecs')
		       end
		 | (false,SIGNAT_STRUCTURE (SOME _,sdecs)) =>
		       let val sdecs' = do_sdecs (popt,selfify) (state, sdecs)
			   val popt = if selfify then popt else NONE
		       in  SIGNAT_STRUCTURE(popt,sdecs')
		       end
(* re-selfifying old paths; but what about other components *)
		 | (true,s as SIGNAT_STRUCTURE (SOME p,sdecs)) =>
		       (case popt of
			   SOME p' => if (eq_path(p,p'))
					  then s
				      else SelfifySig (state,selfify) 
					  (popt, SIGNAT_STRUCTURE(NONE,sdecs))
			 | _ => s)
		 | (_, SIGNAT_VAR v) => SelfifySig (state,selfify) (popt,reduce_sigvar(#ctxt state,v))
		 | (_, SIGNAT_OF m) => 
			    let val m' = SelfifyMod(state,selfify) (NONE, m)
				val _ = if (not selfify)
					    then (print "UNSELFIFY SIGNAT_OF: m = ";
						  pp_mod m; print "\n         m' = ";
						  pp_mod m'; print "\nstate = \n";
						  show_state state; print "\n")
					else ()
			    in  SIGNAT_OF m'
			    end

		 | _ => signat)
	   end
   in
       fun SelfifySig' ctxt = SelfifySig (initial_state ctxt, true)
       fun UnselfifySig' ctxt = SelfifySig (initial_state ctxt, false)
   end

     (* Performs lookup in a structure signature, performing
        normalization as needed with the supplied module argument. *)
    local
	fun wrap vm NONE = NONE
	  | wrap vm (SOME pc) = 
	    SOME(case pc of
		     PHRASE_CLASS_EXP(e,c) => PHRASE_CLASS_EXP(exp_subst_modvar(e,[vm]),
							       con_subst_modvar(c,[vm]))
		   | PHRASE_CLASS_CON(c,k) => PHRASE_CLASS_CON(con_subst_modvar(c,[vm]),k)
		   | PHRASE_CLASS_MOD(m,s) => PHRASE_CLASS_MOD(mod_subst_modvar(m,[vm]),
							       sig_subst_modvar(s,[vm]))
		   | _ => pc)

	fun local_Sdecs_Project ctxt (mopt, sdecs, l) = 
	    let
		fun bad () = (print "local_Sdecs_Project failed on ";
			      pp_label l;
			      print "\n";
			      print "and sdecs = ";
			      pp_sdecs sdecs;
			      print "\n";
			      error "local_Sdecs_Project failed")
		fun exper l = (case mopt of
				   NONE => bad()
				 | SOME m => MODULE_PROJECT(m,l))
		fun coner l = (case mopt of
				   NONE => bad()
				 | SOME m => CON_MODULE_PROJECT(m,l))
		fun moder l = (case mopt of
				   NONE => bad()
				 | SOME m => MOD_PROJECT(m,l))
		fun eh et (VAR v) = mapopt exper (assoc_eq (eq_var,v,et))
		  | eh et _ = NONE
		fun ch ct (CON_VAR v) = mapopt coner (assoc_eq (eq_var,v,ct))
		  | ch ct _ = NONE
		fun mh mt (MOD_VAR v) = mapopt moder (assoc_eq (eq_var,v,mt))
		  | mh mt _ = NONE
		fun externalize (et,ct,mt) dec =
		    case dec of
			(DEC_MOD(_,s)) => SOME(CLASS_MOD(sig_all_handle(s,eh et,ch ct,mh mt, fn _ => NONE)))
		      | (DEC_EXP(_,c)) => SOME(CLASS_EXP(con_all_handle(c,eh et,ch ct,mh mt, fn _ => NONE)))
		      | (DEC_CON(_,k,SOME c)) => SOME(CLASS_CON k)
		      | (DEC_CON(_,k,NONE)) => SOME(CLASS_CON k)
		      | (DEC_EXCEPTION _) => NONE
		fun extend (et,ct,mt) (l,DEC_MOD(v,_)) = (et,ct,(v,l)::mt)
		  | extend (et,ct,mt) (l,DEC_EXP(v,_)) = ((v,l)::et,ct,mt)
		  | extend (et,ct,mt) (l,DEC_CON(v,_,_)) = (et,(v,l)::ct,mt)
		  | extend tables _ = tables
		fun loop t [] = NONE
		  | loop t ((SDEC(l',dec))::rest) = if (eq_label(l,l')) 
							then externalize t dec
						    else loop (extend t (l',dec)) rest
	    in loop ([],[],[]) sdecs
	    end
    in					     

	val local_Sdecs_Project = local_Sdecs_Project

	fun UnselfifySig ctxt (p : path, signat : signat) = 
	    UnselfifySig' ctxt (SOME p,signat)

	fun SelfifySig ctxt (p : path, signat : signat) = 
	    let val res = SelfifySig' ctxt (SOME p,signat)
		val _ = debugdo 
		    (fn () => (print "SelfifySig': p is "; pp_path p;
			       print "\nsignat is\n";
			       pp_signat signat; print "\n\nand returning res:\n";
			       pp_signat res; print "\n\n"))
	    in res
	    end
	fun SelfifyDec ctxt (DEC_MOD (v,s)) = DEC_MOD(v,SelfifySig ctxt (SIMPLE_PATH v,s))
	  | SelfifyDec ctxt dec = dec
	fun SelfifySdecs ctxt (p : path, sdecs : sdecs) =
	    case (SelfifySig ctxt (p,SIGNAT_STRUCTURE(NONE,sdecs))) of
		SIGNAT_STRUCTURE (SOME _,sdecs) => sdecs
	      | _ => error "SelfifySdecs: SelfifySig returned non-normal structure"
    end



   (* ------- checks for the syntactic valuability of ------- *)
   fun Exp_IsSyntacticValue exp = 
     (case exp of
	SCON _ => true
      | PRIM _ => true
      | ILPRIM _ => true
      | VAR _ => true
      | MODULE_PROJECT(m,_) => mod_ispath m
      | RECORD rbnds => foldr (fn (a,b) => a andalso b) true 
	                (map (fn (l,e) => Exp_IsSyntacticValue e) rbnds)
      | FIX _ => true
      | INJ {inject=NONE,...} => true
      | INJ {inject=SOME e,...} => Exp_IsSyntacticValue e
      | _ => false)
   and Module_IsSyntacticValue module = 
      (case module of
	 MOD_STRUCTURE sbnds => foldr (fn (a,b) => a andalso b) true 
	                        (map (fn SBND(l,b) => Bnd_IsSyntacticValue b) sbnds)
       | MOD_LET (v,m1,m2) => (Module_IsSyntacticValue m1) andalso (Module_IsSyntacticValue m2)
       | (MOD_FUNCTOR _) => true
       | _ => false)
   and Bnd_IsSyntacticValue bnd = 
     (case bnd of
       BND_EXP (v,e) => Exp_IsSyntacticValue e
     | BND_MOD (v,m) => Module_IsSyntacticValue m
     | BND_CON (v,c) => true)





   (* --------- structural equality on scons, kinds ------------ *)
   fun eq_scon (s1,s2) = s1 = s2
   fun eq_kind (KIND_TUPLE n1, KIND_TUPLE n2) = n1 = n2
     | eq_kind (KIND_ARROW (m1,n1), KIND_ARROW(m2,n2)) = (m1 = m2) andalso (n1 = n2)
     | eq_kind (KIND_INLINE(k1,_), k2) = eq_kind(k1,k2)
     | eq_kind (k1,KIND_INLINE(k2,_)) = eq_kind(k1,k2)
     | eq_kind _ = false


   (* ---------------------------------------------------------------
      oneshot arrow unifier: note that unset does NOT unify with unset
     ---------------------------------------------------------------- *)
   fun eq_arrow(ax,ay,is_sub) = (ax = ay) orelse (is_sub andalso ax = TOTAL)
   fun eq_comp (comp1,comp2,is_sub) = 
       (case (oneshot_deref comp1,oneshot_deref comp2) of
	    (SOME x, SOME y) => (x = y) orelse (is_sub andalso x = TOTAL)
	  | (SOME x, NONE) => (oneshot_set(comp2,x); true)
	  | (NONE, SOME x) => (oneshot_set(comp1,x); true)
	  | (NONE, NONE) => ((eq_oneshot(comp1,comp2)) orelse
			     (oneshot_set(comp1,PARTIAL); 
			      oneshot_set(comp2,PARTIAL); true)))
   fun eq_onearrow (a1,a2) = eq_comp(a1,a2,false)


   val hardset_targets = ref ([] : con list);
       
   (* ------------------------------------------------------------ 
      type (sub)-equality:
	 First, normalize argument types:
           (1) a module projection whose type is known to the known type
           (2) beta-reducing constructor function application
	   (3) overloaded type expression to the carried type
         Then, performs structural equality except when a type variable.
	 When a type variable is encountered, the unifier routine
	     is called with the type variable and the given type.
	     The unifier may be side-effecting or not.
    -------------------------------------------------------------- *)
   fun unify_maker is_hard (fetch : (context,con) tyvar -> con option) 
       set (constrained,tyvar,c,decs,is_sub) = 
     let 
       val self = unify_maker is_hard fetch set 
     in (case (fetch tyvar) of
	   NONE => let val tyvar_ctxts = tyvar_getctxts tyvar
		   in (case c of
			   CON_TYVAR tv =>
			       eq_tyvar(tv,tyvar) orelse 
			       (case (fetch tv) of
				    SOME c' => self(constrained,tyvar,c',decs,is_sub)
				  | NONE => (set(constrained,tyvar,c); true))
			 | _ => 
			       (
(*
				print "unifying tyvar with non-tyvar "; pp_con c;
				print "\ntyvar has "; print (Int.toString (length tyvar_ctxts));
				print " contexts and they are:\n";
				app (fn ctxt => pp_context ctxt) tyvar_ctxts;
				print "\n\n";
*)
			       not (con_occurs(c,tyvar))
			       andalso (map (fn ctxt => GetConKind(c,ctxt)) tyvar_ctxts; true
					handle _ => false)
			       andalso (set(constrained,tyvar,c); true)))
		   end
	 | (SOME c') => meta_eq_con (is_hard,self) constrained (c',c,decs,is_sub))
     end

   and local_con_constrain(ctxt,c,param as {constrain,stamp,...},ctxts) : con = 
       let
	   val _ = debugdo (fn () => (print "local_con_constrain called on c = ";
				      pp_con c; print "\n"))
	   val param' = {constrain = constrain,
			 stamp = stamp,
			 eq_constrain = false}
	   fun help (CON_REF c) = (con_constrain(c,help,param',ctxts);
				   SOME(CON_REF c))
	     | help (CON_ARRAY c) = (con_constrain(c,help,param',ctxts);
				     SOME(CON_ARRAY c))
	     | help _ = NONE
	   val c' = Normalize(c,ctxt) 
	   val _ = con_constrain(c',help,param,ctxts)
       in c'
       end

   and hard_unifier (arg as (_,_,_,ctxt,_)) =
     let 
       fun hard_fetch tv = tyvar_deref tv
       fun hard_set (constr,tv,c) = 
	   let val _ = (debugdo (fn () => (print "now hard-setting ";
					   pp_con (CON_TYVAR tv); print " to ";
					   pp_con c; print "\n")))
	       val tyvar_ctxts = tyvar_getctxts tv
	       val c = local_con_constrain(ctxt,c,
					   {constrain = constr,
					    eq_constrain = (tyvar_is_use_equal tv),
					    stamp = SOME(tyvar_stamp tv)},tyvar_ctxts);

	   in
	       (hardset_targets := c::(!hardset_targets);
		tyvar_set(tv,c))
	   end
     in  unify_maker true hard_fetch hard_set arg
     end
   and soft_unifier () =
     let 
       val table = ref ([] : ((context,con) tyvar * con) list)
       val constr_con = ref ([] : con list)
       val useeq_con = ref ([] : con list)
       fun soft_fetch (tyvar : (context,con) tyvar) = let 
(*				  val v = tyvar_getvar tyvar *)
				  fun loop [] = NONE
				    | loop ((tv,c)::rest) = 
(*				      if (eq_var(v,tyvar_getvar tv)) *)
				      if (eq_tyvar(tyvar,tv))
					  then SOME c
				      else NONE
			      in (case (tyvar_deref tyvar) of
				    SOME c' => SOME c'
				  | NONE => loop (!table))
			      end
       fun soft_set (constr,tv,c) = 
	   let val _ = debugdo (fn () => 
				(print "now soft-setting ";
				 print (tyvar2string tv); print " to ";
				 pp_con c; print "\n"))
	       val _ = if constr then constr_con := c::(!constr_con) else ()
	       val _ = if (tyvar_is_use_equal tv) 
			   then useeq_con := c::(!useeq_con) else ()
	   in table := (tv,c)::(!table)
	   end
       fun soft_unify arg = unify_maker false soft_fetch soft_set arg
     in (soft_unify,table,constr_con,useeq_con)
     end
   and eq_con (con1,con2,decs) = meta_eq_con (true,hard_unifier) false (con1,con2,decs,false)
   and sub_con (con1,con2,ctxt) = 
     let fun msg() = (print "subcon called with con1 = \n";
			 pp_con con1; print "\nand con2 = \n";
			 pp_con con2; print "\nand ctxt = \n";
			 pp_context ctxt; print "\n")
     in  ((meta_eq_con (false,hard_unifier) false (con1,con2,ctxt,true))
	  handle e => (if !trace then msg() else (); raise e))
     end

   and soft_eq_con (con1,con2,ctxt) = 
     let val (soft_unify,table,constr_con,useeq_con) = soft_unifier()
     in  if (meta_eq_con (false,soft_unify) false (con1,con2,ctxt,false))
	   then ((* app tyvar_set (!table);  *)
		 map (fn c => local_con_constrain(ctxt,c,{constrain = true,
					       stamp = NONE,
					       eq_constrain = false},
(* XXX *)					    [ctxt])) (!constr_con);
		 map (fn c => local_con_constrain(ctxt,c,{constrain=false,
					       stamp = NONE,
					       eq_constrain = true},
 (* XXX *)			[ctxt])) (!useeq_con);
		 true)
	 else false
     end
   and meta_eq_con (is_hard,unifier) constrained (con1,con2,ctxt,is_sub) = 
       (case (con1,con2) of
	    (CON_MODULE_PROJECT(m,l),CON_MODULE_PROJECT(m',l')) =>
		(eq_label(l,l') andalso eq_mod(m,m')) orelse
		(meta_eq_con_hidden (is_hard,unifier) constrained (con1,con2,ctxt,is_sub))
	  | _ => (meta_eq_con_hidden (is_hard,unifier) constrained (con1,con2,ctxt,is_sub)))

   and meta_eq_con_hidden (is_hard,unifier) constrained (con1,con2,ctxt,is_sub) = 
     let fun msg() = (print "meta_eq_con_hidden called with con1 = \n";
			 pp_con con1; print "\nand con2 = \n";
			 pp_con con2; print "\nand ctxt = \n";
			 pp_context ctxt; print "\n")
in (meta_eq_con_hidden' (is_hard,unifier) constrained (con1,con2,ctxt,is_sub))
	  handle e => (if !trace then msg() else (); raise e)
end

   and meta_eq_con_hidden' (is_hard,unifier) constrained (con1,con2,ctxt,is_sub) = 
     let 

       val _ = debugdo (fn () => (print "\nUnifying"; 
				  if constrained then
				      print " CONSTRAINED: "
				  else print " not constrained: ";
				  pp_con con1;
				  print "\nwith:     "; pp_con con2;
				  print "\nusing ctxt = \n"; pp_context ctxt;
				  print "\n"))
       val (isocon1, con1) = HeadNormalize(con1,ctxt)
       val (isocon2, con2) = HeadNormalize(con2,ctxt)
       val _ = debugdo (fn () => (print "\nHeadNormalized to: "; pp_con con1;
				  print "\nand:     "; pp_con con2;
				  if isocon1 then
				      print " isocon1 is CONSTRAINED: "
				  else print " isocon1 is not constrained: ";
				  if isocon2 then
				      print " isocon2 is CONSTRAINED: "
				  else print " isocon2 is not constrained: ";
				  print "\n"))
       val constrained = constrained orelse isocon1 orelse isocon2

       val self = meta_eq_con (is_hard,unifier) constrained

       (* the flex record considered as an entirety is not generalizeable
	  but its subparts are generalizable *)
       fun dorecord() = 
		let 
		    fun match rdecs1 rdecs2 = 
			let fun help ((l1,c1),(l2,c2)) = eq_label(l1,l2) 
			    andalso self(c1,c2,ctxt,is_sub)
			in  eq_list(help,rdecs1,rdecs2)
			end
		    local
			fun check_one addflag (l,c) rdecs =
			    let 
				fun loop [] = if addflag then SOME((l,c)::rdecs) else NONE
				  | loop ((l',c')::rest) = 
				    if (eq_label(l,l'))
					(* do we need to flip the sense of is_sub *)
					then if (self(c,c',ctxt,is_sub))
					     then SOME rdecs
					     else NONE
				    else loop rest
			    in loop rdecs
			    end
		    in
			fun union [] rdecs = SOME(sort_labelpair rdecs)
			  | union (rdec::rest) rdecs = 
			    (case (check_one true rdec rdecs) of
				 NONE => NONE
			   | SOME x => (union rest x))
			fun subset ([]) rdecs = true
			  | subset (rdec::rest) rdecs = 
			    (case (check_one false rdec rdecs) of
				 NONE => false
			       | SOME _ => subset rest rdecs)  (* _ should be same as rdecs here *)
		    end
		    fun stamp_constrain stamp rdecs = 
			map (fn (_,c) => 
			     local_con_constrain(ctxt,c,{constrain=false,
							 stamp = SOME stamp,
							 eq_constrain=false},[])) rdecs
		    fun follow (CON_FLEXRECORD (ref (INDIRECT_FLEXINFO rf))) = follow (CON_FLEXRECORD rf)
		      | follow (CON_FLEXRECORD (ref (FLEXINFO (_,true,rdecs)))) = CON_RECORD rdecs
		      | follow c = c
		in (case (follow con1, follow con2) of
			(CON_RECORD r1, CON_RECORD r2) => match r1 r2
		      | (CON_RECORD rdecs, CON_FLEXRECORD(r as ref(FLEXINFO(stamp,false,flex_rdecs)))) =>
			((subset flex_rdecs rdecs) andalso (stamp_constrain stamp rdecs;
							    r := FLEXINFO(stamp,true,rdecs);
							    true))
		      | (CON_FLEXRECORD(r as ref(FLEXINFO(stamp,false,flex_rdecs))),CON_RECORD rdecs) =>
			((subset flex_rdecs rdecs) andalso (stamp_constrain stamp rdecs;
							    r := FLEXINFO(stamp,true,rdecs);
							    true))
		      | (CON_FLEXRECORD(ref1 as (ref (FLEXINFO (stamp1,false,rdecs1)))),
			 CON_FLEXRECORD(ref2 as (ref (FLEXINFO (stamp2,false,rdecs2))))) => 
			(case (union rdecs1 rdecs2) of
			     NONE => false
			   | SOME rdecs => (let val stamp = stamp_join(stamp1,stamp2)
						val _ = stamp_constrain stamp rdecs
						val flex = FLEXINFO(stamp,false,rdecs)
						val indirect_flex = INDIRECT_FLEXINFO ref2
						val _ = ref1 := indirect_flex
						val _ = ref2 := flex
					    in true
					    end))
		       | _ => error "must have a CON_RECORD or CON_FLEXRECORD here")
		end

       fun doit() = 
	 (case (con1,con2) of
	    (CON_TYVAR tv1, CON_TYVAR tv2) => 
		(eq_tyvar(tv1,tv2) 
		 orelse (unifier(constrained,tv1,con2,ctxt,is_sub)))
	  | (CON_TYVAR tv1, CON_FLEXRECORD _) => unifier(constrained,tv1,con2,ctxt,is_sub)
	  | (CON_TYVAR tv1, _) => unifier(constrained,tv1,con2,ctxt,is_sub)
	  | (_, CON_TYVAR tv2) => unifier(constrained,tv2,con1,ctxt,is_sub)
	  | (CON_VAR v1, CON_VAR v2) => eq_var(v1,v2)
	  | (CON_APP(c1_a,c1_r), CON_APP(c2_a,c2_r)) => self(c1_a,c2_a,ctxt,is_sub) 
	                                                andalso self(c1_r,c2_r,ctxt,is_sub)
	  | (CON_MODULE_PROJECT (m1,l1), CON_MODULE_PROJECT (m2,l2)) => 
		eq_modval(m1,m2) andalso eq_label(l1,l2)

	  | (CON_INT is1, CON_INT is2) => is1 = is2
	  | (CON_UINT is1, CON_UINT is2) => is1 = is2
	  | (CON_FLOAT fs1, CON_FLOAT fs2) => fs1 = fs2
	  | (CON_ANY, CON_ANY) => true
	  | (CON_ARRAY c1, CON_ARRAY c2) => self(c1,c2,ctxt,is_sub)
	  | (CON_VECTOR c1, CON_VECTOR c2) => self(c1,c2,ctxt,is_sub)
	  | (CON_REF c1, CON_REF c2) => self(c1,c2,ctxt,is_sub)
	  | (CON_TAG c1, CON_TAG c2) => self(c1,c2,ctxt,is_sub)
	  | (CON_ARROW (c1_a,c1_r,flag1,comp1), CON_ARROW(c2_a,c2_r,flag2,comp2)) => 
		(flag1 = flag2 
		 andalso eq_comp(comp1,comp2,is_sub)
		 andalso (length c1_a = length c2_a andalso
			  Listops.andfold (fn (c2,c1) => self (c2,c1,ctxt,is_sub)) (zip c1_a c2_a))
		 andalso self(c1_r,c2_r,ctxt,is_sub))
	  | (CON_MU c1, CON_MU c2) => (self(c1,c2,ctxt,is_sub))
	  | (CON_RECORD _, CON_RECORD _) => dorecord()
	  | (CON_RECORD _, CON_FLEXRECORD _) => dorecord()
	  | (CON_FLEXRECORD _, CON_RECORD _) => dorecord()
	  | (CON_FLEXRECORD _, CON_FLEXRECORD _) => dorecord()
	  | (CON_FUN (vs1,c1), CON_FUN(vs2,c2)) => 
		(length vs1 = length vs2) andalso
		let fun folder (v,acc) = add_context_con'(acc,v,KIND_TUPLE 1, NONE)
		    val ctxt' = foldl folder ctxt vs1
		    val table = map2 (fn (v1,v2) => (v2,CON_VAR v1)) (vs1,vs2)
		    val c2' = con_subst_convar(c2,table)
		in self(c1,c2',ctxt',is_sub)
		end
	  | (CON_SUM {noncarriers=n1,carrier=c1,special=i1},
	     CON_SUM {noncarriers=n2,carrier=c2,special=i2}) =>
		(i1=i2 orelse (i2=NONE andalso is_sub)) andalso (n1=n2) andalso
		self(c1,c2,ctxt,is_sub)
	  | (CON_TUPLE_INJECT cs1, CON_TUPLE_INJECT cs2) => 
		eq_list (fn (a,b) => self(a,b,ctxt,is_sub), cs1, cs2)
	  | (CON_TUPLE_PROJECT (i1, c1), CON_TUPLE_PROJECT(i2,c2)) => 
		(i1 =i2) andalso (self(c1,c2,ctxt,is_sub))
	  | _ => false)
       val res = doit()
       val _ = debugdo (fn () => print (if res then "unified\n" else "NOT unified\n"))
     in res
     end




   and Exp_IsValuable(ctxt,exp) =
     (Exp_IsSyntacticValue exp) orelse
     (case exp of
	MODULE_PROJECT (m,l) => Module_IsValuable m ctxt
      | APP(e1,es2) => let val (va1,e1_con) = GetExpCon(e1,ctxt)
			  val _ = (print "exp_isvaluable: app case: e1_con is: \n";
				   Ppil.pp_con e1_con; print "\n")
			  val e1_con_istotal = 
			      (case e1_con of
				   CON_ARROW(_,_,_,comp) => eq_comp(comp,oneshot_init TOTAL,false)
				 | _ => false)
		      in va1 andalso e1_con_istotal
			  andalso (Listops.andfold (fn e2 => Exp_IsValuable (ctxt,e2)) es2)
		      end
     | RECORD rbnds => foldr (fn (a,b) => a andalso b) true 
                       (map (fn (_,e) => Exp_IsValuable(ctxt,e)) rbnds)
     | RECORD_PROJECT(e,_,_) => Exp_IsValuable(ctxt,e)
     | LET (bnds,e) => (case (Bnds_IsValuable' bnds ctxt) of 
			  NONE => false
			| SOME ctxt' => Exp_IsValuable(ctxt',e))
     | ROLL (c,e) => Exp_IsValuable(ctxt,e)
     | UNROLL (c1,c2,e) => Exp_IsValuable(ctxt,e)
     | OVEREXP (_,v,os) => v orelse (case (oneshot_deref os) of
				       NONE => false
				     | SOME e => Exp_IsValuable(ctxt,e))
     | NEW_STAMP _ => true
     | SUM_TAIL (_,e) => Exp_IsValuable(ctxt,e)
     | EXN_INJECT(_,e1,e2) => (Exp_IsValuable(ctxt,e1)) andalso (Exp_IsValuable(ctxt,e2))
     | _ => false)

   (* Rules 140 - 143 *)
   and Bnds_IsValuable' [] ctxt = SOME ctxt
     | Bnds_IsValuable' (bnd::rest) ctxt = 
       let val self = Bnds_IsValuable' rest
       in  (case bnd of
		BND_EXP (v,e) => if (Exp_IsValuable(ctxt,e))
				     then self (add_context_exp'(ctxt,v,#2 (GetExpCon(e,ctxt))))
				 else NONE
	      | BND_MOD (v,m) => let val (va,s) = GetModSig(m,ctxt)
				     val s' = SelfifySig ctxt (SIMPLE_PATH v,s)
				 in if va then self (add_context_mod'(ctxt,v,s'))
				    else NONE
				 end
	      | BND_CON (v,c) => self (add_context_con'(ctxt,v,GetConKind(c,ctxt),SOME c)))
       end

   and Bnds_IsValuable bnds ctxt = (case (Bnds_IsValuable' bnds ctxt) of
					NONE => false
				      | SOME decs => true)
   and Sbnds_IsValuable sbnds ctxt = Bnds_IsValuable (map (fn (SBND(_,b)) => b) sbnds) ctxt
     
   (* Rules 144 - 147 *)
   and Module_IsValuable m ctxt =
     (Module_IsSyntacticValue m) orelse
     (case m of
       MOD_VAR v => true
     | MOD_STRUCTURE sbnds => Sbnds_IsValuable sbnds ctxt
     | MOD_LET (v,m1,m2) => let val (va1,s1) = GetModSig(m1,ctxt)
			    in va1 andalso
				(Module_IsValuable m2 (add_context_mod'(ctxt,v,
									   SelfifySig ctxt (SIMPLE_PATH v,s1))))
			    end
     | MOD_PROJECT (m,l) => Module_IsValuable m ctxt
     | MOD_APP (m1,m2) => let val (va1,s1) = GetModSig(m1,ctxt)
			  in case s1 of
			      SIGNAT_FUNCTOR(_,_,_,TOTAL) =>
				  va1 andalso (Module_IsValuable m2 ctxt)
			    | _ => false
			  end
     | _ => false)
	
   and GetSconCon(ctxt,scon) : con = PrimUtil.value_type (fn e => #2(GetExpCon(e,ctxt))) scon


  (* Rules 35 - 48 *)
   and GetConKind (arg : con, ctxt : context) : kind = 
     let fun msg() = (print "GetConKind called with con = \n";
			 pp_con arg; print "\nand ctxt = \n";
			 pp_context ctxt; print "\n")
	val _ = debugdo msg
     in GetConKind'(arg,ctxt)
	  handle e => (if !trace then msg() else (); raise e)
     end

   and GetConKind' (arg : con, ctxt : context) : kind = 
     let fun ksimp (KIND_INLINE(k,_)) = ksimp k
	   | ksimp k = k
	 val con = arg (* Normalize(arg,ctxt) *)
     in case con of
       (CON_TYVAR tv) => KIND_TUPLE 1 
     | (CON_VAR v) => 
	   (case Context_Lookup'(ctxt,v) of
		SOME(_,PHRASE_CLASS_CON(_,k)) => k
	      | SOME _ => error ("CON_VAR " ^ (var2string v) ^ " not bound to a con")
	      | NONE => error ("CON_VAR " ^ (var2string v) ^ " not bound"))
     | (CON_OVAR ocon) => raise UNIMP
     | (CON_INT _) => KIND_TUPLE 1
     | (CON_FLOAT _) => KIND_TUPLE 1
     | (CON_UINT _) => KIND_TUPLE 1
     | (CON_ANY) => KIND_TUPLE 1
     | (CON_REF _) => KIND_TUPLE 1
     | (CON_ARRAY _) => KIND_TUPLE 1
     | (CON_VECTOR _) => KIND_TUPLE 1
     | (CON_TAG _) => KIND_TUPLE 1
     | (CON_ARROW _) => KIND_TUPLE 1
     | (CON_APP (c1,c2)) => 
	   let val (k1,k2) = (GetConKind(c1,ctxt),GetConKind(c2,ctxt))
	   in (case (ksimp k1, ksimp k2) of
		   (KIND_ARROW(a,b),KIND_TUPLE c) => 
		       if (a=c) then KIND_TUPLE b
		       else (print "GetConKind: kind mismatch in "; pp_con con;
			     print "\nDomain arity = "; print (Int.toString a);
			     print "\nArgument arity = "; print (Int.toString c); print "\n";
			     error "GetConKind: kind mismatch in CON_APP")
		 | (k1,k2) => 
			(print "GetConKind: kind mismatch in "; pp_con con;
			     print "\nFunction kind = "; pp_kind k1;
			     print "\nArgument kind = "; pp_kind k2; print "\n";
			     error "GetConKind: kind mismatch in CON_APP"))
	   end
     | (CON_MU c) => (case GetConKind(c,ctxt) of
			KIND_ARROW(m,n) => KIND_TUPLE n
			| _ => error "kind of CON_MU argument not KIND_ARROW")
     | (CON_FLEXRECORD _) => KIND_TUPLE 1
     | (CON_RECORD _) => KIND_TUPLE 1
     | (CON_FUN (vs,c)) => 
	   let fun folder(v,ctxt) = add_context_con'(ctxt,v,KIND_TUPLE 1,NONE)
	       val ctxt' = foldl folder ctxt vs
	   in (case GetConKind(c,ctxt') of
		   KIND_TUPLE n => KIND_ARROW(length vs,n)
		 | (KIND_ARROW _) => error "kind of constructor body not a KIND_TUPLE")
	   end
     | (CON_SUM _) => KIND_TUPLE 1
     | (CON_TUPLE_INJECT [_]) => error "Cannot CON_TUPLE_INJECT one con"
     | (CON_TUPLE_INJECT cs) => KIND_TUPLE (length cs)
     | (CON_TUPLE_PROJECT (i,c)) => (case (GetConKind(c,ctxt)) of
				       KIND_TUPLE n =>
					 (if (i > 0 andalso i <= n)
					    then KIND_TUPLE 1
					  else
					    (print "GetConKind got: ";
					     pp_con con;
					     print "\n";
					     error "got CON_TUPLE_PROJECT in GetConKind"))
				     | _ => error "got CON_TUPLE_PROJECT in GetConKind")
     | (CON_MODULE_PROJECT (m,l)) => 
	   let val (_,signat) = GetModSig(m,ctxt)
	       val (self,sdecs) = 
		   (case (reduce_signat ctxt signat) of
			SIGNAT_FUNCTOR _ => error "cannot project from functor"
		      | SIGNAT_VAR _ => error "cannot project from SIGNAT_VAR"
		      | SIGNAT_STRUCTURE (self,sdecs) => (self,sdecs)
		      | SIGNAT_INLINE_STRUCTURE {self,abs_sig=sdecs,...} => (self,sdecs))
	   in  (case Sdecs_Lookup ctxt (MOD_VAR (fresh_var()),sdecs,[l]) of
		    NONE => (print "no such label = ";
			     pp_label l; print " in sig \n";
			     Ppil.pp_signat (SIGNAT_STRUCTURE(self,sdecs));
			     print "\n";
			     error "no such label in sig")
		  | SOME(_,PHRASE_CLASS_CON(_,k)) => k
		  | _ => error "label in sig not a DEC_CON")
	   end
     end

   (* --------- Rules 69 to 96 ----------- *)
   and GetExpCon (exparg,ctxt) : bool * con = 
     let fun msg() = (print "GetExpCon called with exp = \n";
			 pp_exp exparg; print "\nand ctxt = \n";
			 pp_context ctxt; print "\n")
	val _ = debugdo msg
     in GetExpCon'(exparg,ctxt) 
	  handle e => (if !trace then msg() else (); raise e)
     end

  and etaize c = (case c of
		      CON_ARROW([c],rescon,b,a) => CON_ARROW([c],rescon,b,a)
		    | CON_ARROW(cons,rescon,b,a) => CON_ARROW([con_tuple cons],rescon,b,a)
		    | _ => c)

   and GetExpAppCon' (exparg,ctxt) : bool * con = 
	   let val ((va1,con1),es2) = 
	       (case exparg of
		    APP(e1,es2) => (GetExpCon(e1,ctxt),es2)
		  | PRIM(p,cs,es) => ((true, PrimUtil.get_type' p cs), es)
		  | ILPRIM(ip,cs,es) => ((true, PrimUtil.get_iltype' ip cs),es))
	       val con1 = Normalize(con1,ctxt)
	       val vacon2 = map (fn e => GetExpCon(e,ctxt)) es2
	       val va2 = Listops.andfold #1 vacon2
	       val cons2 = map #2 vacon2
	       val res_con = fresh_con ctxt
	       val (guesscon,arrow) = 
		   (case con1 of
			CON_ARROW(_,_,closed,arrow) => (CON_ARROW(cons2,res_con,closed,arrow),arrow)
		      | _ => (CON_ARROW (cons2,res_con,false,oneshot()), oneshot()))
	       val is_sub = sub_con(guesscon,con1,ctxt) 
	       val total = (case oneshot_deref arrow of
				NONE => false
			      | SOME PARTIAL => false
			      | SOME TOTAL => true)
	       val va = va1 andalso va2 andalso total
	   in  if is_sub
		   then (va,con_deref res_con)
	       else (print "\nfunction type is = "; pp_con con1;
		     (case cons2 of
			  [con2] => (print "\nargument type is = "; pp_con con2; print "\n")
			| _ => (print "\nargument types are = "; app pp_con cons2; print "\n"));
		     print "Type mismatch in expression application:\n";
		     pp_exp exparg;
		     error "Type mismatch in expression application")
	   end

   and GetExpRollCon' (ctxt,isroll,e,c) : bool * con = 
       let val cNorm = Normalize' "ROLL/UNROLL" (c,ctxt)
	   val (va,econ) = GetExpCon(e,ctxt)
	   val error = fn str => (print "typing roll/unroll expression:\n";
				  Ppil.pp_exp e;
				  print "\n";
				  error str)
	   val (i,cInner) = (case cNorm of
			       (CON_TUPLE_PROJECT(i,CON_MU cInner)) => (i, cInner)
	                     | CON_MU cInner => (0, cInner)
  	                     | _ => (print "\nUnnormalized Decoration of (UN)ROLL was ";
				   pp_con c; print "\n";
				   print "\nNormalized Decoration of (UN)ROLL was ";
				   pp_con cNorm; print "\n";
				   error "decoration of ROLL not a recursive type CON_MU or CON_TUPLE_PROJ(CON_MU)"))
       in (va,
	   (case GetConKind(cInner,ctxt) of
			KIND_ARROW(n,n') =>
			    (if ((n = n') andalso (0 <= i) andalso (i < n))
				 then 
				     let 
					 fun temp j = if n = 1 then cInner else CON_TUPLE_PROJECT(j,CON_MU cInner)
					 val contemp = CON_APP(cInner,con_tuple_inject(map0count temp n))
					 val con2 = if (n = 1) then contemp
						    else CON_TUPLE_PROJECT(i,contemp)
				     in
					 if isroll
					     then
						 (if (sub_con(econ,con2,ctxt))
						      then cNorm
						  else
						      (Ppil.pp_con econ; print "\n";
						       Ppil.pp_con con2; print "\n";
						      error "ROLL: expression type does not match decoration"))
					 else 
					     (if (eq_con_from_get_exp6(econ,cNorm,ctxt))
						  then Normalize' "ROLL/UNROLL 3" (con2,ctxt)
					      else (print "UNROLL: expression type does not match decoration";
						    print "\necon = "; pp_con econ;
						    print "\ncNorm = "; pp_con cNorm;
						    print "\nctxt = "; pp_context ctxt;
						    error "UNROLL: expression type does not match decoration"))
				     end
			     else error "projected decoration has the wrong KIND_ARROW")
		      | _ => error "projected decoration has the wrong kind"))
       end

   and GetExpCon' (exparg,ctxt) : bool * con = 
     (case exparg of
       SCON scon => (true,GetSconCon(ctxt,scon))
     | OVEREXP (con,va,eone) => (case oneshot_deref eone of
				     SOME e => if va then (va,con)
					       else GetExpCon(e,ctxt)
				   | NONE => (va,con))
     | ETAPRIM (p,cs) => (true, etaize (PrimUtil.get_type' p cs))
     | ETAILPRIM (ip,cs) => (true, etaize(PrimUtil.get_iltype' ip cs))
     | (VAR v) => (case Context_Lookup'(ctxt,v) of
		       SOME(_,PHRASE_CLASS_EXP(_,c)) => (true,c)
		     | SOME _ => error "VAR looked up to a non-value"
		     | NONE => error ("GetExpCon: (VAR " ^ (Name.var2string v) ^ "v) not in context"))
     | (PRIM _) => GetExpAppCon' (exparg,ctxt)
     | (ILPRIM _) => GetExpAppCon' (exparg,ctxt)
     | (APP _) => GetExpAppCon' (exparg,ctxt)
     | (FIX (r,a,fbnds)) => (* must check that there are no function calls for TOTAL *)
	   let fun get_arm_type(FBND(_,_,c,c',_)) = CON_ARROW([c],c',false,oneshot_init a)
	       val res_type = (case fbnds of
				   [fbnd] => get_arm_type fbnd
				 | _ => con_tuple(map get_arm_type fbnds))
	       fun folder (FBND(v',v,c,c',e), ctxt) = 
		   add_context_exp'(ctxt,v',CON_ARROW([c],c',false,oneshot_init PARTIAL))
	       val full_ctxt = foldl folder ctxt fbnds
	       fun ttest lctxt (FBND(v',v,c,c',e)) =
		   let val (va,bodyc) = GetExpCon(e,add_context_exp'(lctxt,v,c))
		   in va andalso sub_con(bodyc,c',lctxt)
		   end
	       fun ptest lctxt (FBND(v',v,c,c',e)) =
		   let val (_,bodyc) = GetExpCon(e,add_context_exp'(lctxt,v,c))
		   in sub_con(bodyc,c',lctxt)
		   end
	   in (true,
	       case a of
	       PARTIAL => if (andfold (ptest full_ctxt) fbnds)
			      then res_type
			  else (print "could not type-check FIX expression:\n";
				pp_exp exparg;
				error "could not type-check FIX expression")
	     | TOTAL =>  if ((andfold (ttest ctxt) fbnds))
			     then res_type
			 else (print "could not type-check TOTALFIX expression:\n";
			       pp_exp exparg;
			       error "could not type-check TOTALFIX expression"))
	   end
     | (RECORD (rbnds)) => 
	   let fun help (l,e) = let val (va,c) = GetExpCon(e,ctxt)
				in (va,(l,c))
				end
	       val temp = map help rbnds
	       val va = andfold #1 temp
	       val rdecs = sort_labelpair(map #2 temp)
	   in (va, CON_RECORD rdecs)
	   end
     | (RECORD_PROJECT (exp,l,c)) => 
	   let 
	       val (va,con) = GetExpCon(exp,ctxt)
	       val con' = #2(HeadNormalize(con,ctxt))
	       fun RdecLookup (label,[]) = (print "RdecLookup could not find label ";
					    pp_label label; print "in type ";
					    pp_con con';
					    error "RdecLookup could not find label")
		 | RdecLookup (label,(l,c)::rest) = if eq_label(label,l) then c
						    else RdecLookup (label,rest)
	       fun chase (ref (FLEXINFO(_,_,rdecs))) = rdecs
		 | chase (ref (INDIRECT_FLEXINFO fr)) = chase fr
	   in (case con' of
		   (CON_RECORD rdecs) => (va,RdecLookup(l,rdecs))
		 | (CON_FLEXRECORD fr) => (va,RdecLookup(l,chase fr))
		 | _ => (print "Record Proj on exp not of type CON_RECORD; type = ";
			 pp_con con; print "\n";
			 error "Record Proj on exp not of type CON_RECORD"))
	   end
     | (SUM_TAIL (c,e)) => 
	   let val (va,con) = GetExpCon(e,ctxt)
	   in if (eq_con_from_get_exp2(c,con,ctxt)) 
		  then (case c of
			    CON_SUM{noncarriers,carrier,special=SOME i} => 
				if (i<noncarriers) 
				    then error "SUM_TAIL projecting noncarrier"
				else (va,CON_TUPLE_PROJECT((i-noncarriers),carrier))
			  | _ => error "adornment of SUM_TAIL not a CON_SUM(SOME _,...)")
	      else error "SUM_TAIL: adornment mismatches type of expression"
	   end
     | (HANDLE (body,handler)) => 
	   let val (_,bcon) = GetExpCon(body,ctxt)
	       val (_,hcon) = GetExpCon(handler,ctxt)
	       val res_con = fresh_con ctxt
	       val hcon' = CON_ARROW ([CON_ANY],res_con,false,oneshot())
	   in  if (eq_con_from_get_exp3(hcon,hcon',ctxt)) 
		   then (false,bcon)
	       else error "Type mismatch between handler and body of HANDLE"
	   end
     | (RAISE (c,e)) => 
	   let val (_,econ) = GetExpCon(e,ctxt)
	       val _ = (GetConKind(c,ctxt) 
		   handle _ => error "RAISE: con decoration ill-formed")
	   in (case econ of
		   CON_ANY => (false,c)
		 | _ => error "type of expression raised is not ANY")
	   end
     | (LET (bnds,e)) => 
	   let 
	       fun folder ((_,dec),ctxt) = add_context_dec(ctxt,SelfifyDec ctxt dec)
	       val vdecs = GetBndsDecs(ctxt,bnds)
	       val va_decs = andfold #1 vdecs
	       val ctxt' = foldl folder ctxt vdecs
	       val (va,econ) = GetExpCon(e,ctxt')
	   in (va andalso va_decs,econ)
	   end
     | (NEW_STAMP con) => ((GetConKind(con,ctxt); (true,CON_TAG con))
			   handle _ => error "NEW_STAMP: type is ill-formed")
     | (EXN_INJECT (_,e1,e2)) => 
	   let 
	       val (va1,c1) = GetExpCon(e1,ctxt)
	       val (va2,c2) = GetExpCon(e2,ctxt)
	   in if (eq_con_from_get_exp4(c1,CON_TAG c2,ctxt))
		  then (va1 andalso va2, CON_ANY)
	      else error "EXN_INJECT tag type and value: type mismatch"
	   end
     | ROLL(c,e) => GetExpRollCon'(ctxt,true,e,c)
     | UNROLL(c,_,e) => GetExpRollCon'(ctxt,false,e,c)
    | (INJ {sumtype,field,inject}) =>
       let val sumtype = (case sumtype of
			      CON_SUM _ => sumtype
			    | _ => Normalize(sumtype,ctxt))
	   val (carrier,noncarriers) = 
	         (case sumtype of
		      CON_SUM {carrier,noncarriers,special} => (carrier,noncarriers)
		    | _ => error "INJ got type irreudcible to a special sumtype")
       in  
	(case inject of
	   NONE => 
	       if (field<noncarriers)
		   then (true,CON_SUM{noncarriers=noncarriers,
				      carrier= Normalize(carrier,ctxt),
				      special=NONE})
	       else (error "INJ: bad injection")
	| SOME e =>
	  if (field<noncarriers)
	      then (error "INJ: bad injection")
	  else let val (va,econ) = GetExpCon(e,ctxt)
		   val carrier = Normalize(carrier,ctxt)
		   val i = field - noncarriers (* i >= 0 *)
		   val (n,fieldcon_opt) = 
		       (case carrier of 
		             CON_TUPLE_INJECT [] => (0,NONE)
		           | CON_TUPLE_INJECT clist => (length clist, SOME(List.nth(clist,i)))
			   | _ => (1,SOME carrier))
	       in if (i >= n)
		      then
			  (print "INJ: injection field out of range in exp:";
			   Ppil.pp_exp exparg; print "\n";
			   error "INJ: injection field out of range")
		  else
		      if (eq_con_from_get_exp7(econ, valOf fieldcon_opt, ctxt))
			   then (va,CON_SUM{noncarriers=noncarriers,carrier=carrier,special=NONE})
		       else (print "INJ: injection does not type check eq_con failed on: ";
			     Ppil.pp_exp exparg; print "\n";
			     print "econ is "; Ppil.pp_con econ; print "\n";
			     print "nth clist is "; Ppil.pp_con (valOf fieldcon_opt); print "\n";
			     error "INJ: injection does not typecheck")
	       end)
       end
     | (EXN_CASE {arg,arms,default,tipe}) =>
	   let 
	       val (_,argcon) = GetExpCon(arg,ctxt)
	       val _ = if (eq_con_from_get_exp12(argcon,CON_ANY,ctxt))
			   then () 
		       else error "arg not a CON_ANY in EXN_CASE"
	       fun checkarm(e1,c,e2) = 
		   let val (_,c1) = GetExpCon(e1,ctxt)
		       val (_,c2) = GetExpCon(e2,ctxt)
		   in if ((eq_con_from_get_exp9(c1,CON_TAG c, ctxt))
			  andalso eq_con_from_get_exp10(c2,CON_ARROW([c],tipe,false,oneshot()),ctxt))
			  then ()
		      else error "rescon does not match in EXN_CASE"
		   end
	       val _ = app checkarm arms
	       val _ = 
		   (case default of 
			NONE => ()
		      | (SOME e) =>
			    let val (_,optcon) = GetExpCon(e,ctxt)
			    in if (eq_con_from_get_exp11(optcon,tipe,ctxt))
				   then ()
			       else (print "EXN_CASE: default case mismatches";
				     print "default con is:\n";
				     Ppil.pp_con optcon; print "\n";
				     print "tipe con is :\n";
				     Ppil.pp_con tipe; print "\n";
				     error "EXN_CASE: default case mismatches")
			    end)
	   in (false, tipe)
	   end
     | (CASE {sumtype,arg,bound,arms,tipe,default}) => 
	   let 
	       val sumtype = (case sumtype of
				  CON_SUM _ => sumtype
				| _ => Normalize(sumtype,ctxt))
	       val {carrier,noncarriers,special=_} = 
		   (case sumtype of
			CON_SUM triple => triple
		      | _ => error "CASE got type irreducible to a sumtype")
	       val n = length arms
	       val carrier = Normalize' "CASE" (carrier,ctxt)
	       val (va,eargCon) = GetExpCon(arg,ctxt)
	       val sumcon = CON_SUM {special = NONE,
				     carrier = carrier,
				     noncarriers = noncarriers}
	       fun loop _ va [] =  
		   (case default of 
			NONE => (va, tipe)
		      | SOME edef => 
			    let val (va',defcon) = GetExpCon(edef,ctxt)
			    in  if (sub_con(defcon,tipe,ctxt))
				    then (va andalso va', tipe)
				else error "default arm type mismatch"
			    end)
		 | loop n va (NONE::rest) = loop (n+1) va rest
		 | loop n va ((SOME exp)::rest) = 
			let val ctxt =  if (n < noncarriers) then ctxt
					else add_context_exp'(ctxt,bound,CON_SUM{special = SOME n,
										 carrier = carrier,
										 noncarriers = noncarriers})
			    val (va',c) = GetExpCon(exp,ctxt)
			    val va = va andalso va'
			in  
			    if (sub_con(c,tipe,ctxt))
				then loop (n+1) va rest
			    else (print "case arm type mismatch: checking exp = ";
				  pp_exp exparg; print "\nwith ctxt = ";
				  pp_context ctxt; print "\n";
				  print "exp = \n"; pp_exp exp;
				  print "c = \n"; pp_con c;
				  print "tipe = \n"; pp_con tipe;
				  error "case arm type mismatch")
			end
	   in if (eq_con_from_get_exp15(eargCon,sumcon,ctxt))
		  then (loop 0 va arms)
	      else 
		  error "CASE: expression type and decoration con mismatch"
	   end
     | (MODULE_PROJECT(m,l)) => 
	   let val (va,signat) = GetModSig(m,ctxt)
	       fun self_case(p,sdecs) = 
		   (case Sdecs_Lookup ctxt (path2mod p, sdecs,[l]) of
		       NONE => ((* print "Normalize: label "; pp_label l;
				 print " not in signature s = \n";
				 pp_signat signat; print "\n"; *)
				fail "MODULE_PROJECT: label not in modsig")
		     | (SOME (_,PHRASE_CLASS_EXP(_,con))) => (va,Normalize(con,ctxt))
		     | SOME _ => fail "MODULE_PROJECT: label not of exp")
	       fun notself_case(sdecs) = 
		   (case local_Sdecs_Project ctxt(if va then SOME m else NONE, sdecs, l) of
			NONE => error "MODULE_PROJECT: label not in modsig"
		      | SOME (CLASS_EXP con) => (va,Normalize(con,ctxt))
		      | SOME _ => fail "MODULE_PROJECT: label not of exp")
	   in case (reduce_signat ctxt signat) of
	       SIGNAT_FUNCTOR _ => error "cannot project from module with functor signature"
	     | SIGNAT_VAR _ => error "cannot project from module with signature variable"
	     | SIGNAT_STRUCTURE(SOME p,sdecs) => self_case(p,sdecs)
	     | SIGNAT_INLINE_STRUCTURE{self=SOME p,abs_sig=sdecs,...} => self_case(p,sdecs)
	     | SIGNAT_STRUCTURE (NONE,sdecs) => notself_case sdecs
	     | SIGNAT_INLINE_STRUCTURE{self=NONE,abs_sig=sdecs,...} => notself_case sdecs

(* 	(print "MODULE_PROJECT with m = ";
	pp_mod m; raise UNIMP) *)
	   end

     | (SEAL (e,c)) => let val (va,c') = GetExpCon(e,ctxt)
			   val c'' = Normalize(c,ctxt)
		       in if sub_con(c',c'',ctxt)
			      then (va,c'')
			  else error "SEAL: expression type does not match sealing type"
		       end)

   and eq_con_from_get_exp1(c1,c2,ctxt) = eq_con(c1,c2,ctxt)
   and eq_con_from_get_exp2(c1,c2,ctxt) = eq_con(c1,c2,ctxt)
   and eq_con_from_get_exp3(c1,c2,ctxt) = eq_con(c1,c2,ctxt)
   and eq_con_from_get_exp4(c1,c2,ctxt) = eq_con(c1,c2,ctxt)
   and eq_con_from_get_exp5(c1,c2,ctxt) = eq_con(c1,c2,ctxt)
   and eq_con_from_get_exp6(c1,c2,ctxt) = eq_con(c1,c2,ctxt)
   and eq_con_from_get_exp7(c1,c2,ctxt) = eq_con(c1,c2,ctxt)
   and eq_con_from_get_exp8(c1,c2,ctxt) = eq_con(c1,c2,ctxt)
   and eq_con_from_get_exp9(c1,c2,ctxt) = eq_con(c1,c2,ctxt)
   and eq_con_from_get_exp10(c1,c2,ctxt) = eq_con(c1,c2,ctxt)
   and eq_con_from_get_exp11(c1,c2,ctxt) = eq_con(c1,c2,ctxt)
   and eq_con_from_get_exp12(c1,c2,ctxt) = eq_con(c1,c2,ctxt)
   and eq_con_from_get_exp13(c1,c2,ctxt) = eq_con(c1,c2,ctxt)
   and eq_con_from_get_exp14(c1,c2,ctxt) = eq_con(c1,c2,ctxt)
   and eq_con_from_get_exp15(c1,c2,ctxt) = eq_con(c1,c2,ctxt)
   and eq_con_from_get_exp16(c1,c2,ctxt) = eq_con(c1,c2,ctxt)

   (* ----------- rules 22 - 25 ------------------------- *)
   and GetBndDec (ctxt,BND_EXP (v,e))  = let val (va,c) = GetExpCon (e,ctxt)
					 in (va,DEC_EXP(v,c))
					 end
     | GetBndDec (ctxt,BND_MOD (v,m))  = let val (va,s) = GetModSig(m,ctxt)
					 in (va,DEC_MOD(v,s))
					 end
     | GetBndDec (ctxt,BND_CON (v,c))  = (true,DEC_CON(v,GetConKind(c,ctxt),SOME c))
   and GetBndsDecs (ctxt,bnds) = GetBndsDecs'(ctxt,bnds,[])
   and GetBndsDecs' (ctxt,[],acc) = rev acc
     | GetBndsDecs' (ctxt,bnd::rest,acc) = 
       let val (va,d) = GetBndDec(ctxt,bnd)
	   val ctxt' = add_context_dec(ctxt,SelfifyDec ctxt d)
       in GetBndsDecs' (ctxt',rest, (va,d)::acc)
       end
   and GetSbndSdec (ctxt,SBND (l, bnd)) = let val (va,dec) = GetBndDec(ctxt,bnd)
					  in (va,SDEC(l,dec))
					  end

   and GetSbndsSdecs (ctxt, []) = []
     | GetSbndsSdecs (ctxt, (sbnd as (SBND(l,bnd))) :: rest) = 
       let val (va,dec) = GetBndDec(ctxt,bnd)
	   val sdec = SDEC(l,dec)
	   val ctxt' = add_context_dec(ctxt, SelfifyDec ctxt dec)
       in (sbnd,sdec)::(GetSbndsSdecs(ctxt',rest))
       end



    and Sbnds_Lookup ctxt (sbnds, labs) : (labels * phrase) option =
	let 
(*
	    val _ = (print "sbnds_lookup called with labs = ";
		     app (fn l => (print (Name.label2string l); print ".")) labs; 
		     print "\nand sbnds are:\n";
		     app (fn (SBND(l,_)) => (print (Name.label2string l); print " ...")) sbnds;
		     print "\n\n")
*)
	    fun loop lbl [] = NONE
	      | loop lbl ((sbnd as SBND(l,b))::r) = 
		let val self = loop lbl 
		in
		    (case b of
			 (BND_EXP (_,e)) => if (eq_label(l,lbl)) 
						then SOME([l],PHRASE_EXP e) else self r
		       | (BND_CON (_,c)) => if (eq_label(l,lbl)) 
						then let val c' = c (* XXX *)
						     in SOME([l],PHRASE_CON c') 
						     end
					    else self r
		       | (BND_MOD (_,m)) => (if (eq_label(l,lbl)) 
						 then SOME([l],PHRASE_MOD m) 
					     else if (is_label_open l)
						      then 
							  (case m of 
							       MOD_STRUCTURE sbnds =>
								   (case (self (rev sbnds)) of
									SOME(lbls',phrase) => SOME(l::lbls',phrase)
								      | NONE => self r)
							     | _ => self r)
						  else self r))
		end
	in
	    (case labs of
		 [] => error "Sbnds_Lookup got []"
	       | [lbl] => loop lbl (rev sbnds)
	       | (lbl :: lbls) =>
		     (case (loop lbl (rev sbnds)) of
			 SOME(labs,PHRASE_MOD (MOD_STRUCTURE sbnds)) => 
			     (case (Sbnds_Lookup ctxt (sbnds,lbls)) of
				  SOME(labs2,phrase2) => SOME(labs@labs2,phrase2)
				| _ => NONE)
		       | _ => NONE))
	end

    and Sdecs_Lookup_help ctxt (om, sdecs, labs) : (bool * (phrase_class * labels)) option = 
	let 
	    fun loop m lbl [] = NONE
	      | loop m lbl ((sdec as (SDEC(l,d)))::rest) =
		if (eq_label(l,lbl)) 
		    then (case d of
			      (DEC_EXP (_,c)) => 
				  SOME(false,(PHRASE_CLASS_EXP(MODULE_PROJECT(m,l), c),[l]))
			    | (DEC_CON (_,k,SOME c)) =>
				  SOME(true,(PHRASE_CLASS_CON(c,k),[l]))
			    | (DEC_CON (_,k,NONE)) => 
				  SOME(false,(PHRASE_CLASS_CON(CON_MODULE_PROJECT(m,l),
							   k),[l]))
			    | (DEC_MOD (_,s)) => 
				SOME(false,(PHRASE_CLASS_MOD(MOD_PROJECT(m,l),s),[l]))
			    | _ => loop m lbl rest)
		else if (is_label_open l)
		    then 
		     (case d of
		       (DEC_MOD(_,s)) =>
			   (case s of
			       SIGNAT_STRUCTURE (_,sdecs) =>
				  (case (loop (MOD_PROJECT(m,l)) lbl (rev sdecs)) of
				       SOME (flag,(class,lbls')) => SOME(flag,(class,l::lbls'))
				     | NONE => loop m lbl rest)
			    | SIGNAT_INLINE_STRUCTURE{code,abs_sig=sdecs,...} =>
				  (case (Sbnds_Lookup ctxt (code,[lbl]),
					 loop (MOD_PROJECT(m,l)) lbl (rev sdecs)) of
				       (SOME (lbl,p), SOME(_,(pc,lbls'))) => 
						SOME(true,(merge_phrase_pc(p,pc),l::lbls'))
				     | (SOME _, NONE) => error "sdecs_Lookup: open case:  SOME/NONE"
				     | (NONE, SOME _) => error "sdecs_Lookup: open case:  NONE/SOME"
				     | (NONE,NONE) => loop m lbl rest)
			    | _ => loop m lbl rest)
		        | _ => loop m lbl rest)
		     else loop m lbl rest

	in
	    (case labs of
		 [] => error "Sdecs_Lookup_help got []"
	       | [lbl] => loop om lbl (rev sdecs)
	       | (lbl :: lbls) =>
		     case (loop om lbl (rev sdecs)) of
		       SOME(_,(phrase_class,labs)) =>
			 let fun doit(m',s) =
			     (case s of
				  SIGNAT_STRUCTURE (_,sdecs') =>
				      (case (Sdecs_Lookup_help ctxt(m',sdecs',lbls)) of
					   SOME(nontrivial,(pc2,labs2)) => 
					       SOME(nontrivial,(pc2,labs @ labs2))
					 | NONE => NONE)
				| SIGNAT_INLINE_STRUCTURE {code,abs_sig,...} =>
				      (case (Sbnds_Lookup ctxt (code,lbls)) of
					SOME(labels,phrase) =>
					  (case (Sdecs_Lookup_help ctxt(m',abs_sig,lbls)) of
					       SOME(_,(pc,labs2)) => 
						   SOME(true,(merge_phrase_pc(phrase,pc),
							      labs@labs2))
					     | NONE => NONE))
				| _ => NONE)
			 in  (case phrase_class of
				  PHRASE_CLASS_MOD (m,s) => 
				      doit(m,reduce_signat ctxt s)
				| _ => NONE)
			 end
		     | _ => (print "Sdecs_Lookup_help could not find label";
			     pp_label lbl; print " in sdecs:\n";
			     pp_sdecs sdecs; print "\n";
			     error "Sdecs_Lookup_help could not find label"))
	end

    and Sdecs_Lookup ctxt (m, sdecs, labs) : (labels * phrase_class) option =
	let 
	    fun loop lbl [] = NONE
	      | loop lbl ((sdec as (SDEC(l,d)))::rest) =
		if (eq_label(l,lbl)) 
		    then (case d of
			      (DEC_EXP (_,c)) => 
				  SOME([l],PHRASE_CLASS_EXP(MODULE_PROJECT(m,l), c))
			    | (DEC_CON (_,k,SOME c)) =>
				  SOME([l],PHRASE_CLASS_CON(c,k))
			    | (DEC_CON (_,k,NONE)) => 
				  SOME([l],PHRASE_CLASS_CON(CON_MODULE_PROJECT(m,l),
							k))
			    | (DEC_MOD (_,s)) => 
				  SOME([l],PHRASE_CLASS_MOD(MOD_PROJECT(m,l),s))
			    | _ => loop lbl rest)
		else loop lbl rest

	in
	    (case labs of
		 [] => error "Sdecs_Lookup got []"
	       | [lbl] => loop lbl (rev sdecs)
	       | (lbl :: lbls) =>
		     case (loop lbl (rev sdecs)) of
			 SOME(labs,PHRASE_CLASS_MOD (m',((SIGNAT_STRUCTURE (_,sdecs'))))) =>
			     (case (Sdecs_Lookup ctxt (m',sdecs',lbls)) of
				  SOME(labs2, pc2) => SOME(labs @ labs2, pc2)
				| NONE => NONE)
		       | SOME _ => NONE
		       | NONE => NONE)
	end



    and Sdecs_Lookup' ctxt (m,sdecs,labels) = 
	  (case (Sdecs_Lookup_help ctxt (m,sdecs,labels)) of
	       SOME(_,(pc,labels)) => SOME(labels,pc)
	     | NONE => NONE)




   (* ------------ Return a module's signature    -------------- *)
   and reduce_signat context (SIGNAT_VAR v) = reduce_signat context (reduce_sigvar(context,v))
     | reduce_signat context (SIGNAT_OF m ) = reduce_signat context (#2(GetModSig(m,context)))
     | reduce_signat context s = s

   and GetModSig (module, ctxt : context) : bool * signat =
     let fun msg() = (print "GetModSig called with module = \n";
			 pp_mod module; print "\nand ctxt = \n";
			 pp_context ctxt; print "\n")
	 val _ = debugdo msg
     in GetModSig'(module,ctxt)
	  handle e => (if !trace then msg() else (); raise e)
     end

   and GetModSig' (module, ctxt : context) : bool * signat =
     (case module of
       (MOD_VAR v) => 
	   (case Context_Lookup'(ctxt,v) of
		SOME(_,PHRASE_CLASS_MOD(_,s)) => (true,s)
	      | SOME _ => error ("MOD_VAR " ^ (Name.var2string v) ^ " bound to a non-module")
	      | NONE => error ("MOD_VAR " ^ (Name.var2string v) ^ " not bound"))
     | MOD_STRUCTURE (sbnds) => 
	   let fun loop va [] acc ctxt = (va,rev acc)
		 | loop va (sb::sbs) acc ctxt = 
		   let 
		       val (lva,sdec) = GetSbndSdec(ctxt,sb)
		       val SDEC(_,dec) = sdec
		   in loop (va andalso lva) sbs (sdec::acc) (add_context_dec(ctxt,SelfifyDec ctxt dec))
		   end
	       val (va,sdecs) = (loop true sbnds [] ctxt)
	       val res = SIGNAT_STRUCTURE(NONE,sdecs)
	   in (va,res)
	   end
     | MOD_FUNCTOR (v,s,m) => 
	   let val ctxt' = add_context_dec(ctxt,DEC_MOD(v,SelfifySig ctxt (SIMPLE_PATH v, s)))
	       val (va,signat) = GetModSig(m,ctxt')
	   in  (true,SIGNAT_FUNCTOR(v,s,signat,
				    (if va then TOTAL else PARTIAL)))
	   end
     | MOD_APP (a,b) => 
	   let val _ = debugdo (fn () => (print "\n\nMOD_APP case in GetModSig\n";
					  print "a is\n"; pp_mod a; print "\n";
					  print "b is\n"; pp_mod b; print "\n"))
	       val (vaa,asignat) = GetModSig(a,ctxt)
	       val (vab,bsignat) = GetModSig(b,ctxt)
	       val _ = debugdo (fn () => (print "\n\nMOD_APP case in GetModSig got asignat and bsignat\n";
					  print "asignat is\n"; pp_signat asignat; print "\n";
					  print "bsignat is\n"; pp_signat bsignat; print "\n"))
	   in case (reduce_signat ctxt asignat) of
	       (SIGNAT_STRUCTURE _) => error "Can't apply a structure signature"
	     | (SIGNAT_VAR _) => error "Can't apply a structure with variable signature"
	     | (SIGNAT_INLINE_STRUCTURE _) => error "Can't apply a structure signature"
	     | SIGNAT_FUNCTOR (v,csignat,dsignat,ar) =>
		   if (Sig_IsSub(ctxt, bsignat, csignat))
		       then (vaa andalso vab andalso (ar = TOTAL),
			     sig_subst_modvar(dsignat,[(v,b)]))
			    else error ("Module Application where" ^ 
					" argument and parameter signature mismatch")
	   end
     | MOD_LET (v,m1,m2) => 
	   let val (va1,s1) = GetModSig(m1,ctxt)
	       val s1' = SelfifySig ctxt (SIMPLE_PATH v,s1)
	       val ctxt' = add_context_mod'(ctxt,v,s1')
	       val (va2,s2) = GetModSig(m2,ctxt')
	   in (va1 andalso va2,sig_subst_modvar(s2,[(v,m1)]))
	   end
     | MOD_PROJECT (m,l) => 
	   let 
	       val _ = debugdo (fn () => (print "GetModSig called with: "; 
					  pp_mod module; print "\n"))
	       val (va,signat) = GetModSig(m,ctxt)
	       val _ = debugdo (fn () => (print "retrieved signat of \n"; 
					  pp_signat signat; print "\n"))
	       fun self_case(p,sdecs) = 
		   (case Sdecs_Lookup ctxt (path2mod p, sdecs,[l]) of
			NONE =>  
			 (print "GetModSig: SignatLookup MOD_PROJECT failed with label ";
			     pp_label l;
			     print "\nand with signat = \n";  pp_signat signat; 
			     print "\n";
			     fail "MOD_PROJECT failed to find label ")
		      | (SOME (_,PHRASE_CLASS_MOD(_,s))) => (va,s)
		      | _ => (print "MOD_PROJECT at label "; pp_label l; 
			      print "did not find DEC_MOD.  \nsig was = ";
			      pp_signat signat; print "\n";
			      fail "MOD_PROJECT found label not of flavor DEC_MOD"))
	       fun notself_case(sdecs) = 
		   (case local_Sdecs_Project ctxt(if va then SOME m else NONE, sdecs, l) of
			NONE => error "MOD_PROJECT: label not in modsig"
		      | SOME (CLASS_MOD s) => (va,s)
		      | SOME _ => fail "MOD_PROJECT: label found wrong flavor")

	   in case (reduce_signat ctxt signat) of
	       SIGNAT_FUNCTOR _ => error "cannot project from functor"
	     | SIGNAT_VAR _ => error "cannot project from variable signature"
	     | SIGNAT_STRUCTURE (SOME p,sdecs) => self_case(p,sdecs)
	     | SIGNAT_INLINE_STRUCTURE {self=SOME p,abs_sig=sdecs,...} => self_case(p,sdecs)
	     | SIGNAT_STRUCTURE (NONE,sdecs) => notself_case sdecs
	     | SIGNAT_INLINE_STRUCTURE {self=NONE,abs_sig=sdecs,...} => notself_case sdecs
	   end

     | MOD_SEAL (m,s) => let val (va,ps) = GetModSig(m,ctxt)
			     val _ = if (Sig_IsSub(ctxt,ps,s)) then()
				     else error "MOD_SEAL: Sig_IsSub failed"
			 in (va,s)
			 end)

    and HeadNormalize (arg,ctxt) : (bool * con) = 
	let fun msg() = (print "HeadNormalize called with con =\n";
			 pp_con arg; print "\n and ctxt = \n";
			 pp_context ctxt; print "\n")
	in  HeadNormalize'(arg,ctxt)
	  handle e => (if !trace then msg() else (); raise e)
	end

    and HeadNormalize' (arg,ctxt) : (bool * con) = 
	 (case arg of
	      CON_OVAR ocon => let val tv = ocon_deref ocon
			    val (_,c') = HeadNormalize(CON_TYVAR tv,ctxt)
			       in (true,c')
			       end
	    | (CON_TYVAR tv) => (tyvar_isconstrained tv,
				 case tyvar_deref tv of
				     NONE => arg
				   | SOME c => #2(HeadNormalize(c,ctxt)))
	    | (CON_VAR v) => (case (Context_Lookup'(ctxt,v)) of
				  SOME(_,PHRASE_CLASS_CON (CON_VAR v',k)) => 
				      if (eq_var(v,v'))
					  then (case k of (* we consider KIND_INLINE abstrct *)
(*						    KIND_INLINE (k,c) => HeadNormalize(c,ctxt) 
						  | *) _ => (false, CON_VAR v))
				      else HeadNormalize(CON_VAR v',ctxt)
				| SOME(_,PHRASE_CLASS_CON (c,_)) => HeadNormalize(c,ctxt)
				| SOME _ => error ("Normalize: CON_VAR " ^ (var2string v) ^ " not bound to a con")
				| NONE => error ("Normalize: CON_VAR " ^ (var2string v) ^ " not bound"))
	    | CON_TUPLE_PROJECT (i,c) => 
		  let val (f,c) = HeadNormalize(c,ctxt)
		  in case c of
		      CON_TUPLE_INJECT cons => 
			  let val len = length cons
			  in if (i >= 0 andalso i < len)
				 then 
				     let val (f',c') = HeadNormalize(List.nth(cons,i),
								     ctxt)
				     in (f orelse f', c')
				     end
			     else
				 error "HeadNormalize: con tuple projection - index wrong"
					       end
		    | _ => (f,CON_TUPLE_PROJECT(i,c))
		  end
	    (* Eta contract functions *)
	    | c as CON_FUN([v],CON_APP(f,CON_VAR v')) => if (eq_var(v,v')) then HeadNormalize(f,ctxt)
							else (false,c)
	    | c as CON_FUN(vars,CON_APP(f,CON_TUPLE_INJECT args)) =>
		let fun match (v,(CON_VAR v')) = eq_var(v,v')
		      | match _ = false
		in  if (length vars = length args andalso (andfold match (zip vars args)))
			then HeadNormalize(f,ctxt)
		     else (false,c)
		end
	    | CON_APP(c1,c2) => 
		  let val (f1,c1') = HeadNormalize(c1,ctxt)
		      val (f2,c2') = HeadNormalize(c2,ctxt)
		      val (f,c) = (case (c1',c2') of
				       (CON_FUN(vars,cons), _) =>
					   HeadNormalize(ConApply(false,c1',c2'),ctxt)
				     | _ => (false,CON_APP(c1',c2')))
		  in (f1 orelse f2 orelse f, c)
		  end
	    | (CON_MODULE_PROJECT (m,l)) =>
		  (let 
		   val (_,s) = GetModSig(m,ctxt)
		   fun break_loop (c as (CON_MODULE_PROJECT(m',l'))) = 
		       if (eq_label(l,l') andalso eq_mod(m,m')) 
			   then (false,c)
		       else HeadNormalize(c,ctxt)
		     | break_loop c = HeadNormalize(c,ctxt)
		   fun loop _ [] = (false,arg)
		     | loop (tables as (ctable,mtable)) ((SDEC(curl,dec))::rest) = 
		       (case dec of
			    DEC_CON(v,_,SOME curc) =>
				let val tables' = ((v,CON_MODULE_PROJECT(m,curl))::ctable,mtable)
				in  if eq_label(curl,l)
					then let val curc' = con_subst_convar(curc,ctable)
						 val curc'' = con_subst_modvar(curc',mtable)
					     in break_loop curc''
					     end
				    else loop tables' rest
				end
			  | DEC_MOD(v,s) => let val tables' = (ctable, (v,MOD_PROJECT(m,curl))::mtable)
					    in loop tables' rest
					    end
			  | _ => loop tables rest)
		   fun self_case(p,sdecs) = 
		       (case Sdecs_Lookup ctxt (path2mod p, sdecs, [l]) of
			    SOME(_,PHRASE_CLASS_CON(c,_)) => break_loop c
			  | SOME _ => error "CON_MOD_PROJECT found signature with wrong flavor"
			  | NONE => (false,arg))
		   fun notself_case sdecs = 
		       (debugdo (fn () => 
				 (print "HeadNormalize: CON_MODULE_PROJECT case: l = "; 
				  pp_label l; print "\n and sdecs = ";
				  pp_sdecs sdecs;
				  print "\n"));
			loop ([],[]) sdecs)
	       in (case (reduce_signat ctxt s) of 
		       SIGNAT_STRUCTURE(NONE,sdecs) => notself_case sdecs
		     | SIGNAT_INLINE_STRUCTURE{self=NONE,abs_sig=sdecs,...} => notself_case sdecs
		     | SIGNAT_STRUCTURE (SOME p,sdecs) => self_case(p,sdecs)
		     | SIGNAT_INLINE_STRUCTURE{self=SOME p,abs_sig=sdecs,...} => self_case(p,sdecs)
		     | SIGNAT_VAR _ => (print "CON_MODULE_PROJECT from signat var \n";
					pp_mod m;
					error "CON_MODULE_PROJECT from signat var")
		     | SIGNAT_FUNCTOR _ => (print "CON_MODULE_PROJECT from functor = \n";
					    pp_mod m;
					    error "CON_MODULE_PROJECT from a functor"))
	       end)
	  | c => (false,c))

	
    and Normalize' str (arg,ctxt) = 
	(debugdo (fn () => (print "Normalize called with string = ";
			    print str; print "\n"));
	 Normalize (arg,ctxt))
	
 (* -------- performs CON application, tuple projection, and module projections ----- *)
   and Normalize (arg,ctxt) = 
       (debugdo (fn () => (print "Normalize called with con = ";
			   pp_con arg; print "\nand ctxt = \n";
			   pp_context ctxt;	print "\n"));
      case arg of
	CON_INT _  => arg
      | CON_FLOAT _  => arg
      | CON_UINT _  => arg
      | CON_ANY => arg
      | (CON_OVAR ocon)           => let val tv = ocon_deref ocon
				     in Normalize(CON_TYVAR tv,ctxt)
				     end
      | (CON_REF c)               => CON_REF (Normalize(c,ctxt))
      | (CON_ARRAY c)             => CON_ARRAY (Normalize(c,ctxt))
      | (CON_VECTOR c)            => CON_VECTOR (Normalize(c,ctxt))
      | (CON_TAG c)               => CON_TAG (Normalize(c,ctxt))
      | (CON_ARROW (cs1,c2,closed,comp))  => CON_ARROW (map (fn c => Normalize(c,ctxt)) cs1, 
						 Normalize(c2,ctxt),closed,comp)
      | (CON_MU c)     => CON_MU(Normalize(c,ctxt))
      | (CON_RECORD rdecs)        => let fun f (l,c)= (l,Normalize(c,ctxt))
	                             in CON_RECORD (map f rdecs)
                                     end
      | (CON_FLEXRECORD (ref (INDIRECT_FLEXINFO rf))) => Normalize(CON_FLEXRECORD rf,ctxt)
      | (CON_FLEXRECORD (r as ref (FLEXINFO (stamp,flag,rdecs)))) => 
	    let fun f (l,c)= (l,Normalize(c,ctxt))
		val _ = r := FLEXINFO(stamp,flag,map f rdecs)
	    in arg
	    end
      | (CON_FUN (vs,c))          => CON_FUN (vs,c)
      | (CON_SUM {noncarriers,carrier,special}) => 
	    CON_SUM{noncarriers = noncarriers,
		    carrier = Normalize(carrier,ctxt),
		    special = special}
      | (CON_TUPLE_INJECT cs)     => CON_TUPLE_INJECT (map (fn c => Normalize(c,ctxt)) cs)
      | (CON_TUPLE_PROJECT (i,c)) => let val c' = Normalize(c,ctxt)
					 val arg' = CON_TUPLE_PROJECT(i,c')
				     in #2(HeadNormalize(arg',ctxt))
				     end
      | (CON_MODULE_PROJECT (m as MOD_STRUCTURE sbnds,l)) => (* no need to normalize m *)
	    (case (Sbnds_Lookup ctxt (sbnds,[l])) of
		 SOME(_, PHRASE_CON c) => Normalize(c,ctxt)
	       | SOME _ => error "Sbnds_Lookup projected out a non-con component"
	       | NONE => error "Sbnds_Lookup could not find component")
      | (CON_MODULE_PROJECT (m,l)) => 
	    (let val _ = debugdo (fn () =>
				  (print "normalize about to call getmodsig of m = \n";
				   pp_mod m;
				   print "\nand ctxt = \n";
				   pp_context ctxt;
				   print "\n"))
		 val (_,signat) = GetModSig(m,ctxt)
		 val _ = debugdo (fn () => (print "normalize got back sig:\n";
					    pp_signat signat; print "\n"))
		 fun self_case(p,sdecs) = 
		     (case Sdecs_Lookup ctxt (path2mod p, sdecs, [l]) of
			  NONE => (print "CON_MOD_PROJECT failed to find label = ";
				       pp_label l; print " and signat = \n";
				       pp_signat signat; print "\n";
				       error "CON_MOD_PROJECT failed to find label")
			| (SOME (_,PHRASE_CLASS_CON(c,k))) => 
			      (case c of
				   CON_MODULE_PROJECT(m',l') => 
				       if (eq_label(l,l') andalso eq_mod(m,m'))
					   then c
				       else Normalize(c,ctxt)
				 | _ => Normalize(c,ctxt))
			| (SOME _) => error "CON_MOD_PROJECT found label not DEC_CON")
		 fun notself_case (sbnds,sdecs) = 
		     let val p = SIMPLE_PATH (fresh_named_var "badbadbad")
		     in case Sdecs_Lookup ctxt (path2mod p, sdecs, [l]) of
			 NONE => (print "CON_MOD_PROJECT failed to find label = ";
				  pp_label l; print " and signat = \n";
				  pp_signat signat; print "\n";
				  error "CON_MOD_PROJECT failed to find label")
		       | (SOME (_,PHRASE_CLASS_CON(c,k))) => 
			     (case c of
				  CON_MODULE_PROJECT(m',l') => 
				      if (eq_label(l,l') andalso eq_mod(m,m'))
					  then c
				      else Normalize(c,ctxt)
				| _ => Normalize(c,ctxt))
		       | (SOME _) => error "CON_MOD_PROJECT found label not DEC_CON"
		     end

	     in  
		 case (m,reduce_signat ctxt signat) of
		     (_,SIGNAT_FUNCTOR _) => error "cannot project for functor"
		   | (_,SIGNAT_VAR _) => error "cannot project from signat_var"
		   | (_,SIGNAT_STRUCTURE (SOME p,sdecs)) => self_case(p,sdecs)
		   | (_,SIGNAT_INLINE_STRUCTURE{self=SOME p,abs_sig=sdecs,...}) => self_case(p,sdecs)
		   | (MOD_STRUCTURE sbnds,SIGNAT_STRUCTURE (NONE,sdecs)) => notself_case(sbnds,sdecs)
		   | (MOD_STRUCTURE sbnds, SIGNAT_INLINE_STRUCTURE{self=NONE,abs_sig=sdecs,...}) => 
			 notself_case(sbnds,sdecs)
		   | _ => (print "Normalize: CON_MODULE_PROJECT with m = ";
			    pp_mod m;
			    print "and context = ";
			    pp_context ctxt;
			    raise UNIMP)
	     end)
      | (CON_VAR v) => (case #2(HeadNormalize(arg,ctxt)) of
			    CON_VAR v' => if (eq_var(v,v')) 
					      then arg else Normalize(CON_VAR v',ctxt)
			  | c => Normalize(c,ctxt))
      | (CON_TYVAR co) => (case tyvar_deref co of
			      SOME c => Normalize(c,ctxt)
			    | NONE => arg)
      | (CON_APP(a,b)) => let val a' = Normalize(a,ctxt)
			      val b' = Normalize(b,ctxt)
			      val c' = CON_APP(a',b')
			  in #2(HeadNormalize(c',ctxt))
			  end)


     and Kind_Valid (KIND_TUPLE n,_)     = n >= 0
       | Kind_Valid (KIND_ARROW (m,n),_) = (m >= 0) andalso (n >= 0)
       | Kind_Valid (KIND_INLINE (k,c),ctxt) = 
	 let val k' = GetConKind(c,ctxt)
	     fun eq_kind (KIND_TUPLE n1, KIND_TUPLE n2) = n1 = n2
	       | eq_kind (KIND_ARROW(m1,n1), KIND_ARROW(m2,n2)) = 
		 ((m1 = m2) andalso (n1 = n2))
	       | eq_kind _ = false
	 in  Kind_Valid(k,ctxt) andalso eq_kind(k,k')
	 end

     and Context_Valid ctxt = raise Util.UNIMP

     and Decs_Valid (ctxt,[]) = true
       | Decs_Valid (ctxt,a::rest) = Dec_Valid(ctxt,a) andalso 
	 Decs_Valid(add_context_dec(ctxt,SelfifyDec ctxt a),rest)

     and Dec_Valid (ctxt : context, dec) = 
	 let fun var_notin v  = (Context_Lookup'(ctxt,v); false) handle _ => true
	     fun name_notin n = (Context_Exn_Lookup(ctxt,n); false) handle _ => true
       in  (case dec of
	      DEC_EXP(v,c) => (var_notin v) andalso 
		              (case GetConKind(c,ctxt) of
				   KIND_TUPLE 1 => true
				 | _ => false)
	    | DEC_MOD(v,s) => (var_notin v) andalso (Sig_Valid(ctxt,s))
	    | DEC_CON (v,k,NONE) => (var_notin v) andalso (Kind_Valid(k,ctxt))
	    | DEC_EXCEPTION(name,CON_TAG c) => ((name_notin name) andalso 
						(case GetConKind(c,ctxt) of
						     KIND_TUPLE 1 => true
						   | _ => false))
	    | _ => false)
       end
				

     and Sdecs_Domain sdecs = map (fn SDEC(l,_) => l) sdecs
     and Sdecs_Valid (ctxt, []) = Context_Valid ctxt
       | Sdecs_Valid (ctxt, (SDEC(label,dec)::rest)) = 
	 (Dec_Valid(ctxt,dec) andalso 
	  Sdecs_Valid(add_context_dec(ctxt,SelfifyDec ctxt dec),rest) andalso 
	  (not (List.exists (fn l => eq_label(label,l))
		(Sdecs_Domain rest))))

     and Sig_Valid (ctxt : context, SIGNAT_STRUCTURE(NONE, sdecs)) = Sdecs_Valid(ctxt,sdecs)
       | Sig_Valid (ctxt : context, SIGNAT_STRUCTURE (SOME p,sdecs)) = Sdecs_Valid(ctxt,sdecs)
       | Sig_Valid (ctxt : context, SIGNAT_INLINE_STRUCTURE {abs_sig=sdecs,...}) = Sdecs_Valid(ctxt,sdecs)
       | Sig_Valid (ctxt, SIGNAT_FUNCTOR(v,s_arg,s_res,arrow)) = 
	 (Sig_Valid(ctxt,s_arg) andalso 
	  Sig_Valid(add_context_mod'(ctxt,v,SelfifySig ctxt (SIMPLE_PATH v,s_arg)),s_res))
       | Sig_Valid (ctxt : context, SIGNAT_VAR v) = Sig_Valid(ctxt,reduce_sigvar(ctxt,v))

     and Dec_IsSub (ctxt,d1,d2) = Dec_IsSub' true (ctxt,d1,d2) 
     and Dec_IsEqual (ctxt,d1,d2) = Dec_IsSub' false (ctxt,d1,d2) 

     and Dec_IsSub' isSub (ctxt,d1,d2) = 
	 (case (d1,d2) of
	      (DEC_MOD(v1,s1),DEC_MOD(v2,s2)) => 
		  eq_var(v1,v2) andalso (if isSub then Sig_IsSub(ctxt,s1,s2)
					     else Sig_IsEqual(ctxt,s1,s2))
	    | (DEC_CON(v1,k1,SOME c1),DEC_CON(v2,k2,NONE)) => 
		  isSub andalso eq_var(v1,v2) andalso eq_kind(k1,k2) 
		  andalso eq_kind(k1,GetConKind(c1,ctxt))
	    | (DEC_CON(v1,k1,NONE),DEC_CON(v2,k2,SOME _)) => false
	    | (DEC_CON(v1,k1,SOME c1),DEC_CON(v2,k2,SOME c2)) => 
		  eq_var(v1,v2) andalso eq_kind(k1,k2) 
		  andalso if isSub then sub_con(c1,c2,ctxt) else eq_con(c1,c2,ctxt)
	    | (DEC_EXP(v1,c1),DEC_EXP(v2,c2)) => 
		  eq_var(v1,v2) andalso (if isSub then sub_con(c1,c2,ctxt) else eq_con(c1,c2,ctxt))
	    | (DEC_EXCEPTION (n1,c1), DEC_EXCEPTION(n2,c2)) => 
		  (eq_tag(n1,n2)) andalso (if isSub then sub_con(c1,c2,ctxt) else eq_con(c1,c2,ctxt))
	    | _ => false)


     (* Rules 99 - 100 *)
     and Sdecs_IsSub (ctxt,sdecs1,sdecs2) = Sdecs_IsSub' true (ctxt,sdecs1,sdecs2)
     and Sdecs_IsEqual (ctxt,sdecs1,sdecs2) = Sdecs_IsSub' false (ctxt,sdecs1,sdecs2)

     and Sdecs_IsSub' isSub (ctxt,sdecs1,sdecs2) =
	 let 
	     exception NOPE
	     fun help subster (v1,v2,sdecs) =
		 (case (subster(SIGNAT_STRUCTURE (NONE,sdecs),[(v1,v2)])) of
		      SIGNAT_STRUCTURE (_,sdecs') => sdecs'
		    | _ => error "Sdecs_IsSub subst failed")
	     fun match_var [] [] = []
	       | match_var (SDEC(l1,dec1)::rest1) ((sdec2 as (SDEC(l2,dec2)))::rest2) : sdec list = 
		 if (eq_label (l1,l2))
		     then (case (dec1,dec2) of
			       (DEC_MOD(v1,s1),DEC_MOD(v2,s2)) =>
				   if (eq_var(v1,v2))
				       then sdec2::(match_var rest1 rest2)
				   else SDEC(l2,(DEC_MOD(v1,s2)))::
				       (match_var rest1 (help sig_subst_modvar (v2,MOD_VAR v1,rest2)))
			     | (DEC_EXP(v1,c1),DEC_EXP(v2,c2)) =>
				   if (eq_var(v1,v2))
				       then sdec2::(match_var rest1 rest2)
				   else SDEC(l2,(DEC_EXP(v1,c2)))
				       ::(match_var rest1 (help sig_subst_expvar (v2,VAR v1,rest2)))
			     | (DEC_CON(v1,k1,c1),DEC_CON(v2,k2,c2)) =>
				   if (eq_var(v1,v2))
				       then sdec2::(match_var rest1 rest2)
				   else SDEC(l2,(DEC_CON(v1,k2,c2)))
				       ::(match_var rest1 (help sig_subst_convar (v2,CON_VAR v1,rest2)))
			     | _ => SDEC(l2,dec2)::(match_var rest1 rest2))
		 else raise NOPE
	       | match_var _ _ = (print "Sdecs_IsSub: length mismatch\n";
				  raise NOPE)
	     fun loop ctxt [] [] = true
	       | loop ctxt (SDEC(_,dec1)::rest1) (SDEC(_,dec2)::rest2) = 
		 let val dec1 = SelfifyDec ctxt dec1
		 in (Dec_IsSub' isSub (ctxt,dec1,dec2)
		     andalso loop (add_context_dec(ctxt,dec1)) rest1 rest2)
		 end
	       | loop ctxt _ _ = false
	 in (loop ctxt sdecs1 (match_var sdecs1 sdecs2))
	     handle NOPE => false
	 end

     (* Rules 109 - 112 *)
     and Sig_IsSub (ctxt, sig1, sig2) = Sig_IsSub' true (ctxt, sig1, sig2)
     and Sig_IsEqual (ctxt, sig1, sig2) = Sig_IsSub' false (ctxt, sig1, sig2)

     and Sig_IsSub' isSub (ctxt, sig1, sig2) = 
	 let fun help(ctxt,sdecs1,sdecs2) = Sdecs_IsSub' isSub (ctxt,sdecs1,sdecs2)
	 in
	     (case (reduce_signat ctxt sig1,reduce_signat ctxt sig2) of
		  (SIGNAT_STRUCTURE (NONE,sdecs1), 
		   SIGNAT_STRUCTURE (NONE,sdecs2)) => 
		  let val v = fresh_named_var "selfvar"
		      val p = SIMPLE_PATH v
		      val sdecs1 = SelfifySdecs ctxt (p,sdecs1)
		      val sdecs2 = SelfifySdecs ctxt (p,sdecs2)
		      val ctxt = add_context_mod'(ctxt,v,SIGNAT_STRUCTURE(SOME p,sdecs1))
		  in help(ctxt,sdecs1,sdecs2)
		  end
		| (SIGNAT_INLINE_STRUCTURE {self,abs_sig,...},_) => 
		      Sig_IsSub' isSub (ctxt,SIGNAT_STRUCTURE(self,abs_sig),sig2)
		| (_,SIGNAT_INLINE_STRUCTURE {self,abs_sig,...}) => 
		      Sig_IsSub' isSub (ctxt,sig1,SIGNAT_STRUCTURE(self,abs_sig))
		| (SIGNAT_STRUCTURE (NONE,sdecs1), 
		   SIGNAT_STRUCTURE (SOME p,sdecs2)) => help(ctxt,SelfifySdecs ctxt (p,sdecs1),sdecs2)
		| (SIGNAT_STRUCTURE (SOME p,sdecs1), 
		   SIGNAT_STRUCTURE (NONE, sdecs2)) => help(ctxt,sdecs1,SelfifySdecs ctxt (p,sdecs2))
		| (SIGNAT_STRUCTURE (SOME p1,sdecs1), 
		   SIGNAT_STRUCTURE (SOME p2,sdecs2)) => (* eq_path(p1,p2) orelse *) help(ctxt,sdecs1,sdecs2)
		| (SIGNAT_FUNCTOR(v1,s1_arg,s1_res,a1), 
		   SIGNAT_FUNCTOR(v2,s2_arg,s2_res,a2)) =>
		  ((eq_arrow(a1,a2,true)) andalso 
		   let val s1_res = if (eq_var(v1,v2)) then s1_res
				    else sig_subst_modvar(s1_res,[(v1,MOD_VAR v2)])
		       val s2_arg = SelfifySig ctxt (SIMPLE_PATH v2, s2_arg)
		       val ctxt' = add_context_dec(ctxt,DEC_MOD(v2,s2_arg))
		   in  Sig_IsSub' isSub (ctxt',s2_arg,s1_arg) andalso 
		       Sig_IsSub' isSub (ctxt',s1_res,s2_res)
		   end)
		 | _ => (print "Warning: ill-formed call to Sig_IsSub' with sig1 = \n";
			 pp_signat sig1; print "\n and sig2 = \n";
			 pp_signat sig2; print "\n";
			 false))
	 end


  local
      fun LookupHelp (ctxt, labs, path, pc) = 
	  let fun sbnds_sdecs (sbnds,sdecs) = 
	      (case (Sbnds_Lookup ctxt (sbnds,labs)) of
		   SOME(labels,phrase) =>
		       (case (Sdecs_Lookup_help ctxt (path2mod path,sdecs,labs)) of
			    SOME(_,(pc,labels')) => 
				let val p = join_path_labels(path,labels)
				in  SOME(p,merge_phrase_pc(phrase,pc)) 
				end
			  | NONE => NONE)
		 | NONE => NONE)
	  in case pc of
	      (PHRASE_CLASS_MOD(m,s)) =>
		  (case (m,reduce_signat ctxt s) of
		       (MOD_STRUCTURE sbnds,SIGNAT_STRUCTURE (_,sdecs)) =>
			   sbnds_sdecs(sbnds,sdecs)
		     | (_,SIGNAT_INLINE_STRUCTURE{abs_sig=sdecs,code=sbnds,...}) =>
			   sbnds_sdecs(sbnds,sdecs)
		     | (_,SIGNAT_STRUCTURE (_,sdecs)) =>
			   (case (Sdecs_Lookup_help ctxt (path2mod path,sdecs,labs)) of
				SOME(_,(pc,labels)) =>
				    let val p = join_path_labels(path,labels)
				    in  SOME (p,pc)
				    end
			      | _ => NONE)
		     | _ => NONE)
	    | _ => NONE
	  end
  in
      fun Context_Lookup_Labels (ctxt, [] : label list) : (path * phrase_class) option = NONE
	| Context_Lookup_Labels (ctxt as CONTEXT{label_list, ...}, (lab::labrest)) = 
	(case (labrest,Name.LabelMap.find(label_list,lab)) of
	    (_,NONE) => NONE
	  | ([],SOME (path,pc)) => SOME(path,pc)
	  | (_,SOME (path,pc)) => LookupHelp(ctxt,labrest,path,pc))
      fun Context_Lookup_Path (CONTEXT{var_list,...}, p as SIMPLE_PATH v) = 
	  (case Name.VarMap.find(#1 var_list, v) of
	      NONE => NONE
	    | SOME (_,pc) => SOME(p,pc))
	| Context_Lookup_Path (_, COMPOUND_PATH(v,[])) = error "ill-formed compound path"
	| Context_Lookup_Path (ctxt as CONTEXT{var_list, ...}, COMPOUND_PATH(v,labs)) =
	(case (Name.VarMap.find(#1 var_list,v)) of
	    NONE => NONE
	  | SOME (_,pc) => LookupHelp(ctxt,labs,SIMPLE_PATH v,pc))
  end

    val eq_con = fn (ctxt,c1,c2) => eq_con(c1,c2,ctxt)
    val sub_con = fn (ctxt,c1,c2) => sub_con(c1,c2,ctxt)
    val soft_eq_con = fn (ctxt,c1,c2) => soft_eq_con(c1,c2,ctxt)
    val con_normalize = fn (context,c) => Normalize(c,context)
    val con_head_normalize = fn (context,c) => #2(HeadNormalize(c,context))

    val GetExpCon = fn (d,e) => #2(GetExpCon(e,d))
    val GetConKind = fn (d,c) => GetConKind(c,d)
    val GetModSig = fn (d,m) => ((* Stats.counter "ilstatic.externgetmodsig" (); *)
				#2(GetModSig(m,d)))
    val GetBndDec = fn arg => #2(GetBndDec arg)
    val GetBndsDecs = fn arg => map #2 (GetBndsDecs arg)
    val GetSbndsSdecs = fn arg => map #2(GetSbndsSdecs arg)
    val Module_IsValuable = fn (d,m) => Module_IsValuable m d
    val Bnds_IsValuable = fn (d,bs) => Bnds_IsValuable bs d
    val Sbnds_IsValuable = fn (d,ss) => Sbnds_IsValuable ss d

  end
