(* Static semantics *)
structure IlStatic
  :> ILSTATIC =
  struct

    open Util Listops
    open Il Ppil IlUtil
    open IlContext
    open Prim Tyvar Name

    val error = fn s => error "ilstatic.sml" s
    val trace = Stats.ff("IlstaticTrace") 		(* show debug messages while raising exception *)
    val debug = Stats.ff("IlstaticDebug")		(* show other debug messages *)
    val showing = Stats.ff("IlstaticShowing")		(* show unification progress *)

    fun debugdo t = if (!debug) then (t(); ()) else ()
    fun debugdo' t = t()

   local
       val Ceq_compile : (context * con -> (exp * con) option) ref =
	   ref (fn _ => error "eq_compile not installed")
   in
       fun installHelpers {eq_compile : context * con -> (exp * con) option} : unit =
	   Ceq_compile := eq_compile
       fun eq_compile arg = !Ceq_compile arg
   end

   fun reduce_sigvar(context,v) =
	(case (Context_Lookup_Var(context,v)) of
	     SOME(_,PHRASE_CLASS_SIG(v,s)) => s
	   | SOME _ => error "reduce_sigvar given non SIGVAR"
	   | NONE => error "reduce_sigvar given unbound SIGVAR")

   fun reduce_signat context (SIGNAT_VAR v) = reduce_signat context (reduce_sigvar(context,v))
     | reduce_signat context s = s

   fun deep_reduce_signat ctxt signat : signat =
       let val signat' = reduce_signat ctxt signat
       in  (case signat' of
	      SIGNAT_STRUCTURE sdecs =>
		 SIGNAT_STRUCTURE(map (deep_reduce_sdec ctxt) sdecs)
            | SIGNAT_FUNCTOR(v,s1,s2,arrow) =>
		 SIGNAT_FUNCTOR(v, deep_reduce_signat ctxt s1,
				deep_reduce_signat ctxt s2, arrow)
	    | s => s)
       end

   and deep_reduce_sdec ctxt sdec : sdec =
	let val SDEC(l,dec) = sdec
	    val sdec' = (case dec
			   of DEC_MOD(v,b,s) =>
			       let val s = deep_reduce_signat ctxt s
			       in  SDEC(l,DEC_MOD(v,b,s))
			       end
			    | _ => sdec)
	in  sdec'
	end

    datatype class = CLASS_EXP of con
                   | CLASS_CON of kind
                   | CLASS_MOD of signat
                   | CLASS_SIG
                   | CLASS_OVEREXP


   (* ------- checks for the syntactic valuability of ------- *)
   fun Exp_IsSyntacticValue exp =
     (case exp of
	SCON _ => true
      | PRIM _ => true
      | ILPRIM _ => true
      | VAR _ => true
      | MODULE_PROJECT(m,_) => Option.isSome(mod2path m)
      | RECORD rbnds => foldr (fn (a,b) => a andalso b) true
	                (map (fn (l,e) => Exp_IsSyntacticValue e) rbnds)
      | FIX _ => true
      | INJ {inject=NONE,...} => true
      | INJ {inject=SOME e,...} => Exp_IsSyntacticValue e
      | FOLD _ => true
      | UNFOLD _ => true
      | _ => false)
   and Module_IsSyntacticValue module =
      (case module of
         MOD_VAR _ => true
       | MOD_STRUCTURE sbnds => foldr (fn (a,b) => a andalso b) true
	                        (map (fn SBND(l,b) => Bnd_IsSyntacticValue b) sbnds)
       | MOD_LET (v,m1,m2) => (Module_IsSyntacticValue m1) andalso (Module_IsSyntacticValue m2)
       | (MOD_FUNCTOR _) => true
       | _ => false)
   and Bnd_IsSyntacticValue bnd =
     (case bnd of
       BND_EXP (_,e) => Exp_IsSyntacticValue e
     | BND_MOD (_,_,m) => Module_IsSyntacticValue m
     | BND_CON (_,c) => true)





   (* --------- structural equality on kinds ------------ *)

   fun eq_kind (KIND,KIND) = true
     | eq_kind (KIND_TUPLE n1, KIND_TUPLE n2) = n1 = n2
     | eq_kind (KIND_ARROW (n1,k1), KIND_ARROW(n2,k2)) = (n1 = n2) andalso eq_kind(k1,k2)
     | eq_kind _ = false


   (* ---------------------------------------------------------------
      oneshot arrow unifier: note that unset does NOT unify with unset
     ---------------------------------------------------------------- *)

   (* shallow use of equality types here *)
   fun eq_arrow(ax,ay,is_sub) = (ax = ay) orelse (is_sub andalso ax = TOTAL)
   fun eq_comp (comp1,comp2,is_sub) =
       (case (oneshot_deref comp1,oneshot_deref comp2) of
	    (SOME x, SOME y) => eq_arrow(x,y,is_sub)
	  | (SOME x, NONE) => (oneshot_set(comp2,x); true)
	  | (NONE, SOME x) => (oneshot_set(comp1,x); true)
	  | (NONE, NONE) => ((eq_oneshot(comp1,comp2)) orelse
			     (oneshot_set(comp1,PARTIAL);
			      oneshot_set(comp2,PARTIAL); true)))

   fun eq_sigarrow(a1,a2,is_sub) = (
       case (a1,a2) of
         (APPLICATIVE,APPLICATIVE) => true
       | (GENERATIVE,GENERATIVE) => true
       | (APPLICATIVE,GENERATIVE) => is_sub
       (* XXX Remove this case once functors are switched over to APPLICATIVE vs. GENERATIVE *)
       | _ => eq_arrow(a1,a2,is_sub)
   )

    datatype reduceType = NORM   (* Reduce to normal form *)
                        | HEAD   (* Reduce to head-normal-form *)
                        | ONCE   (* Take one reduction step *)


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

    (* find_tyvars_flexes : given a con, return a list of all unset tyvars with a flag indicating
                            whether it occurred inside a CON_REF, CON_ARRAY, or CON_APP which never
                            uses equality at that type and all flexinfo refs in con.  Also performs
                            path compression on chains of CON_TYVARs.  *)
    fun find_tyvars_flexes (con,ctxt) =
	let val tyvars = ref []
	    val flexes = ref []
	    val _ = debugdo (fn () =>
			     (print "find_tyvars_flexes con = ";
			      pp_con con; print "\n"))
	    fun reset c tv = (debugdo (fn () =>
				       (print "find_tyvars_flexes: path compression resetting ";
					print (tyvar2string tv); print " to "; pp_con c; print "\n"));
			      tyvar_reset(tv,c))
	    fun compress tv =
		let
		    fun scan (tv, tvs, copt) =
			(case tyvar_deref tv
			   of NONE => (tvs, copt)
			    | SOME (c as CON_TYVAR tv2) => scan (tv2, tv :: tvs, SOME c)
			    | somec => (tvs, somec))
		in
		    case scan (tv, nil, NONE)
		      of (tvs, SOME c) => app (reset c) tvs
		       | _ => ()
		end
	    fun funArgSdecs (SIGNAT_FUNCTOR (_, SIGNAT_STRUCTURE sdecs, _, _)) = sdecs
	      | funArgSdecs _ = error "find_tyvars_flexes: expected SIGNAT_FUNCTOR"
	    fun geteqmodsig (CON_MODULE_PROJECT (m,l)) =
		let val (_,s,_) = GetModSig (m,ctxt)
		in
		    (case ProjectModSig (ctxt,m,s,to_eq l)
		       of SOME (PHRASE_CLASS_MOD(_,_,signat,_)) => SOME signat
			| SOME _ => error ("find_tyvars_flexes: eq label not bound to structure")
			| NONE => NONE)
		end
	      | geteqmodsig (CON_VAR v) =
		let val l =
		    (case Context_Lookup_Var(ctxt,v)
		       of SOME(l,PHRASE_CLASS_CON _) => l
			| SOME _ => error ("find_tyvars_flexes: CON_VAR " ^ (var2string v) ^
					   " not bound to a con")
			| NONE => error ("find_tyvars_flexes: CON_VAR " ^ (var2string v) ^
					 " not bound"))
		    val eql = to_eq l
		in  case Context_Lookup_Labels(ctxt,[eql])
		      of SOME(_,PHRASE_CLASS_MOD(_,_,signat,_)) => SOME signat
		       | SOME _ => error ("find_tyvars_flexes: label " ^ (label2string eql) ^
					  " not bound to a module")
		       | NONE => NONE
		end
	      | geteqmodsig _ = error "find_tyvars_flexes: expected CON_MODULE_PROJECT or CON_VAR"
	    fun help in_array_ref argcon =
		let
		    fun con_handler (c : con) =
			(case c of
			     CON_TYVAR tyvar =>
				 let val seen = member_eq(eq_tyvar,tyvar,map #2 (!tyvars))
				     val set = isSome (tyvar_deref tyvar)
				     val _ = compress tyvar
				     val _ = if (seen orelse set)
						 then ()
					     else (tyvars := (in_array_ref,tyvar) :: (!tyvars))
				 in  NONE
				 end
			   | CON_FLEXRECORD r => (flexes := r :: (!flexes); NONE)
			   | CON_ARRAY elemc => (if in_array_ref then NONE else (help true elemc; SOME c))
			   | CON_INTARRAY is => (if in_array_ref then NONE else (help true (CON_UINT is); SOME c))
			   | CON_FLOATARRAY fs => (if in_array_ref then NONE else (help true (CON_FLOAT fs); SOME c))
			   | CON_REF elemc => (if in_array_ref then NONE else (help true elemc; SOME c))
			   | CON_APP (c',types) =>
				 (* For each a in types we recurse with in_ref true if the equality
				  * functor for c' does not require equality at a.  *)
				 if in_array_ref then NONE
				 else
				     (case geteqmodsig c'
					of NONE => NONE
					 | SOME eqsig =>
					    let
						val sdecs = funArgSdecs eqsig
						fun eqLabel (SDEC (l, DEC_EXP(_, CON_ARROW _, _, _))) =
						    if is_eq l then SOME l else NONE
						  | eqLabel _ = NONE
						val eqLabels = List.mapPartial eqLabel sdecs
						fun wants_eq tyl =
						    let val eql = to_eq tyl
						    in  List.exists (fn eql' => eq_label(eql,eql')) eqLabels
						    end
						fun tyLabel (SDEC (l, DEC_CON(_, KIND, _, _))) = SOME l
						  | tyLabel _ = NONE
						val tyLabels = List.mapPartial tyLabel sdecs
						val _ = if length tyLabels = length types then ()
							else error "find_tyvars_flexes: meq requires wrong number of types"
						fun help' (tyl,ty) = help (not (wants_eq tyl)) ty
					    in  help false c';
						app help' (Listops.zip tyLabels types);
						SOME c
					    end)
			   | _ => NONE)
		    val handlers = (default_exp_handler,
				    con_handler,
				    default_mod_handler,
				    default_sdec_handler,
				    default_sig_handler)

		    val _ = con_handle handlers argcon
		in  ()
		end
	in  help false con;
	    debugdo (fn () => print "find_tyvars_flexes finished\n");
	    (!tyvars, !flexes)
	end


   and unify_maker () =
     let
	 (* Commit establishes the invariant that a tyvar which wants equality is set
	  * iff its eq_hole is filled.
	  *)
       datatype undo_info =
	   UNDO_TYVAR of {saved : (context,con,exp) tyvar, active : (context,con,exp) tyvar}
	 | UNDO_FLEX of flexinfo ref * stamp

       datatype commit_info =
	   COMMIT_EQEXP of {hole : exp oneshot,
			    exp : exp,
			    tyvar : (context,con,exp) tyvar,
			    con : con}

       local
	   fun undo_flex (r as (ref (FLEXINFO (_,resolved,rdecs))), stamp) = r := FLEXINFO(stamp,resolved,rdecs)
	     | undo_flex (ref (INDIRECT_FLEXINFO rf), stamp) = undo_flex (rf,stamp)

	   fun undo' (UNDO_TYVAR {saved, active}) = tyvar_update(active,saved)
	     | undo' (UNDO_FLEX arg) = undo_flex arg

	   fun commit' (COMMIT_EQEXP {hole, exp, tyvar, con}) =
	       let val _ = debugdo (fn () =>
				    (print "unification committing to equality function\n";
				     print "  tyvar = "; print (tyvar2string tyvar); print "\n";
				     print "  con = "; pp_con con; print "\n";
				     print "  exp = "; pp_exp exp; print "\n"))
	       in  oneshot_set(hole, exp)
	       end

	   val undo_table = ref ([] : undo_info list)
	   val commit_table = ref ([] : commit_info list)
       in
	   fun add_undo entry = undo_table := entry :: (!undo_table)
	   fun add_commit entry = commit_table := entry :: (!commit_table)
	   fun undo() = app undo' (!undo_table)
	   fun commit() = app commit' (!commit_table)
	   fun marker () =
	       let val undo = !undo_table
		   val commit = !commit_table
		   fun restore () =
		       let val current_undo = !undo_table
			   val count = (length current_undo) - (length undo)
		       in  app undo' (List.take(current_undo,count));
			   undo_table := undo;
			   commit_table := commit
		       end
	       in  restore
	       end
       end

       fun constrain(bools_tyvars,flexes,
		     {gen_constrain,eq_constrain,constrain_ctxts,stamp}) =
	   let
	       fun do_tyvar (in_ref,tv) =
		   let val saved = tyvar_copy tv
		       val _ = if gen_constrain then tyvar_constrain tv else ()
		       val _ = if (eq_constrain andalso not in_ref)
				   then tyvar_use_equal tv else ()
		       val _ = tyvar_addctxts(tv,constrain_ctxts)
		       val _ = update_stamp(tv,stamp)
		   in
		       add_undo (UNDO_TYVAR{saved = saved, active = tv})
		   end
	       fun do_flex (r as (ref (FLEXINFO (st,resolved,rdecs)))) =
		   let val _ = r := FLEXINFO(stamp_join(stamp,st),resolved,rdecs)
		   in  add_undo (UNDO_FLEX (r,stamp))
		   end
		 | do_flex (ref (INDIRECT_FLEXINFO rf)) = do_flex rf
	   in  app do_tyvar bools_tyvars;
	       app do_flex flexes
	   end

       fun set (ctxt,tyvar,c) =
	   let
	       (* If tyvar is marked for equality its oneshot will be set UNLESS the equality
		* compiler fails for c.  In that case, unification fails.  *)
	       val local_undo = marker()
	       val _ = (case (tyvar_deref tyvar) of
			    NONE => ()
			  | (SOME c') => error "cannot set an already set tyvar")
	       fun follow_tyvar (c as (CON_TYVAR tv)) =
		         (case tyvar_deref tv of
			      NONE => c
			    | SOME c => follow_tyvar c)
		 | follow_tyvar c = c
	       val c = follow_tyvar c
	       val (tyvars,flexes) = find_tyvars_flexes (c,ctxt)
	       val _ =  if (!showing)
			    then (print "unification setting ";
				  print (tyvar2string tyvar); print " == ";
				  pp_con (CON_TYVAR tyvar); print "\n  to ";
				  pp_con c; print "\n";
				  print "  tyvars are ";
				  app (fn (_,tv) => (print (tyvar2string tv); print " == ";
						     pp_con (CON_TYVAR tv); print "   ")) tyvars;
				  print "\n\n")
			else ()
	       val tyvar_ctxts = tyvar_getctxts tyvar
	       val use_equal = tyvar_is_use_equal tyvar
	       val _ = constrain (tyvars,flexes,
				  {gen_constrain = tyvar_isconstrained tyvar,
				   eq_constrain = use_equal,
				   stamp = tyvar_stamp tyvar,
				   constrain_ctxts = tyvar_ctxts})
	       val eq_compile_failed =	(* Must follow constrain *)
		   (use_equal andalso
		    (case eq_compile (ctxt, c)
		       of SOME (exp,con) =>
			   (add_commit (COMMIT_EQEXP {hole=valOf (tyvar_eq_hole tyvar),
						      exp=exp, tyvar=tyvar, con=con});
			    false)
			| NONE => true))
	       val fv = Name.VarSet.listItems(con_free c)
	       fun var_bound ctxt v = (case Context_Lookup_Var_Raw(ctxt,v) of
					   SOME _ => true
					 | _ => false)
	       fun bounded ctxt = Listops.andfold (var_bound ctxt) fv
	       val well_formed = Listops.andfold bounded tyvar_ctxts

	       val occurs = Listops.member_eq(eq_tyvar,tyvar,map #2 tyvars) (* occurs check *)

	       fun fail why = (debugdo (fn () =>
					(print "Fails "; print why; print "\n"));
			       local_undo();
			       false)
	   in
	       case (occurs, eq_compile_failed, not well_formed)
		 of (true, _, _) => fail "occurs check"
		  | (_, true, _) => fail "equality compilation"
		  | (_, _, true) => fail "well-formed in tyvar contexts"
		  | _ => (add_undo (UNDO_TYVAR {saved = tyvar_copy tyvar, active = tyvar});
			  tyvar_set(tyvar,c);
			  true)
	   end

     in  (set, constrain, undo, commit)
     end


   and meta_eq_con (setter,constrain,is_sub) (con1,con2,ctxt) =
       let val self = meta_eq_con (setter, constrain, is_sub)
	   val _ = if (!showing)
		       then (print "meta_eq_con called on:-------------\n";
			     print "con1 = "; pp_con con1; print "\n";
			     print "con2 = "; pp_con con2; print "\n")
		   else ()
       in
	   case (con1,con2) of
	       (CON_TYVAR tv1, CON_TYVAR tv2) =>
		   (eq_tyvar(tv1,tv2) orelse
		    (case (tyvar_deref tv1, tyvar_deref tv2) of
			 (NONE, NONE) => setter(ctxt,tv1,con2)
		       | (NONE, SOME c2) => self(con1,c2,ctxt)
		       | (SOME c1,_) => self (c1,con2,ctxt)))
	     | (CON_TYVAR tv1, _) =>
		   (case tyvar_deref tv1 of
			NONE => setter(ctxt,tv1,con2)
		      | SOME c => self (c,con2,ctxt))
	     | (_, CON_TYVAR tv2) =>
		   (case tyvar_deref tv2 of
			NONE => setter(ctxt,tv2,con1)
		      | SOME c => self (con1,c,ctxt))
	     | _ =>
		       let
			   val con1 = HeadNormalize(con1,ctxt)
			   val con2 = HeadNormalize(con2,ctxt)
		       in  
			   meta_eq_con_hidden (setter,constrain,is_sub) (con1,con2,ctxt)
		       end
       end


   (* ------------ con1 and con2 are head-normalized *)
   and meta_eq_con_hidden (setter,constrain,is_sub) (con1,con2,ctxt) =
     let
	   val _ = if (!showing)
		       then (print "meta_eq_con_hidden called on:-------------\n";
			     print "con1 = "; pp_con con1; print "\n";
			     print "con2 = "; pp_con con2; print "\n")
		   else ()
	 val self = meta_eq_con (setter, constrain, is_sub)

       (* the flex record considered as an entirety is not generalizeable
	  but its subparts are generalizable *)
       fun dorecord () =
		let
		    fun match rdecs1 rdecs2 =
			let fun help ((l1,c1),(l2,c2)) = eq_label(l1,l2)
			    andalso self(c1,c2,ctxt)
			in  eq_list(help,rdecs1,rdecs2)
			end
		    local
			fun check_one addflag (l,c) rdecs =
			    let fun loop [] = if addflag then SOME((l,c)::rdecs) else NONE
				  | loop ((l',c')::rest) =
				    if (eq_label(l,l'))
					(* do we need to flip arguments for subbing *)
					then (if (self(c,c',ctxt))
						  then SOME rdecs
					      else NONE)
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
			let val temp = CON_RECORD rdecs
			    val (tyvars, flexes) = find_tyvars_flexes (temp,ctxt)
			in  constrain(tyvars,flexes,
				      {gen_constrain = false,
				       eq_constrain = false,
				       stamp = stamp,
				       constrain_ctxts = []})
			end
		    fun follow (CON_FLEXRECORD (ref (INDIRECT_FLEXINFO rf))) = follow (CON_FLEXRECORD rf)
		      | follow (CON_FLEXRECORD (ref (FLEXINFO (_,true,rdecs)))) = CON_RECORD rdecs
		      | follow c = c
		in (case (follow con1, follow con2) of
			(CON_RECORD r1, CON_RECORD r2) => match r1 r2
		      | (CON_RECORD rdecs,
			    CON_FLEXRECORD(r as ref(FLEXINFO(stamp,false,flex_rdecs)))) =>
			((subset flex_rdecs rdecs) andalso (stamp_constrain stamp rdecs;
							    r := FLEXINFO(stamp,true,rdecs);
							    true))
		      | (CON_FLEXRECORD(r as ref(FLEXINFO(stamp,false,flex_rdecs))),
			   CON_RECORD rdecs) =>
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
     in
	 (case (con1,con2) of
	    (CON_TYVAR _, _) => self(con1,con2,ctxt)
	  | (_, CON_TYVAR _) => self(con1,con2,ctxt)
	  | (CON_VAR v1, CON_VAR v2) => eq_var(v1,v2)
	  | (CON_APP(c1_f,c1_args), CON_APP(c2_f,c2_args)) =>
		self(c2_f,c1_f,ctxt)
		andalso (andfold (fn (c1,c2) => self(c1,c2,ctxt)) (zip c1_args c2_args))
	  | (CON_MODULE_PROJECT (m1,l1), CON_MODULE_PROJECT (m2,l2)) => 
		eq_label(l1,l2) andalso path_eq_mod(ctxt,m1,m2)
	  | (CON_INT is1, CON_INT is2) => is1 = is2
	  | (CON_UINT is1, CON_UINT is2) => is1 = is2
	  | (CON_FLOAT fs1, CON_FLOAT fs2) => fs1 = fs2
	  | (CON_ANY, CON_ANY) => true
	  | (CON_ARRAY c1, CON_ARRAY c2) => self(c1,c2,ctxt)
	  | (CON_VECTOR c1, CON_VECTOR c2) => self(c1,c2,ctxt)
	  | (CON_INTARRAY is1, CON_INTARRAY is2) => is1 = is2
	  | (CON_INTVECTOR is1, CON_INTVECTOR is2) => is1 = is2
	  | (CON_FLOATARRAY fs1, CON_FLOATARRAY fs2) => fs1 = fs2
	  | (CON_FLOATVECTOR fs1, CON_FLOATVECTOR fs2) => fs1 = fs2
	  | (CON_REF c1, CON_REF c2) => self(c1,c2,ctxt)
	  | (CON_TAG c1, CON_TAG c2) => self(c1,c2,ctxt)
	  | (CON_ARROW (c1_a,c1_r,flag1,comp1), CON_ARROW(c2_a,c2_r,flag2,comp2)) =>
		(flag1 = flag2
		 andalso eq_comp(comp1,comp2,is_sub)
		 andalso (length c1_a = length c2_a andalso
			  Listops.andfold (fn (c2,c1) => self (c2,c1,ctxt)) (zip c1_a c2_a))
		 andalso self(c1_r,c2_r,ctxt))
	  | (CON_RECORD _, CON_RECORD _) => dorecord()
	  | (CON_RECORD _, CON_FLEXRECORD _) => dorecord()
	  | (CON_FLEXRECORD _, CON_RECORD _) => dorecord()
	  | (CON_FLEXRECORD _, CON_FLEXRECORD _) => dorecord()
	  | (CON_FUN (vs1,c1), CON_FUN(vs2,c2)) =>
		(length vs1 = length vs2) andalso
		let fun folder ((v1,v2),(ctxt,subst)) = (add_context_con'(ctxt,v1,KIND, NONE),
							 subst_add_convar(subst,v2,CON_VAR v1))
		    val (ctxt',subst) = foldl folder (ctxt,empty_subst) (zip vs1 vs2)
		    val c2' = con_subst(c2,subst)
		in self(c1,c2',ctxt')
		end
	  | (CON_COERCION (tyvars,c1,c2), CON_COERCION(tyvars',c1',c2')) =>
		(length tyvars = length tyvars') andalso
		let fun folder ((v1,v2),(ctxt,subst)) = (add_context_con'(ctxt,v1,KIND, NONE),
							 subst_add_convar(subst,v2,CON_VAR v1))
		    val (ctxt',subst) = foldl folder (ctxt,empty_subst) (zip tyvars tyvars')
		    val c1'' = con_subst(c1',subst)
		    val c2'' = con_subst(c2',subst)
		in
		    self(c1,c1'',ctxt') andalso self(c2,c2'',ctxt')
		end
	  | (CON_SUM {names=n1,noncarriers=nc1,carrier=c1,special=i1},
	     CON_SUM {names=n2,noncarriers=nc2,carrier=c2,special=i2}) =>
		let val special = (i1=i2) 
		    val res = special andalso (eq_list(eq_label,n1,n2)) andalso
		              (nc1=nc2) andalso self(c1,c2,ctxt)
		in  res
		end
	  | (CON_TUPLE_INJECT cs1, CON_TUPLE_INJECT cs2) =>
		eq_list (fn (a,b) => self(a,b,ctxt), cs1, cs2)
	  | (CON_TUPLE_PROJECT (i1, c1), CON_TUPLE_PROJECT(i2,c2)) =>
		(i1 = i2) andalso (self(c1,c2,ctxt))
	  | (CON_MU c1, CON_MU c2) => self(c1,c2,ctxt)
	  | _ => false)
     end

   and eq_con (ctxt, con1, con2) =
       let val _ = if (!showing)
		       then print "eq_con calling meta_eq_con\n"
		   else ()
	   val (setter,constrain,undo,commit) = unify_maker()
	   val is_eq = (* Stats.subtimer("Elab-subeq_con", *) meta_eq_con (setter,constrain,false)
	               (con1, con2, ctxt)
	   val _ = commit()
       in  is_eq
       end

   and sub_con (arg as (ctxt, con1, con2)) =
       let val _ = if (!showing)
		       then print "sub_con calling meta_eq_con\n"
		   else ()
	   val (setter,constrain,undo,commit) = unify_maker()
	   val is_sub = (* Stats.subtimer("Elab-subeq_con", *) meta_eq_con (setter,constrain,true)
	                (con1, con2, ctxt)
	   val _ = commit()
       in  is_sub
       end

   and soft_eq_con (ctxt,con1,con2) =
       let val _ = if (!showing)
		       then print "soft_eq_con calling meta_eq_con\n"
		   else ()
	   val (setter,constrain,undo,commit) = unify_maker()
	   val is_eq = meta_eq_con (setter,constrain,false) (con1,con2,ctxt)
	   val _ = undo()
       in  is_eq
       end

   and semi_sub_con (ctxt,con1,con2) =
       let val _ = if (!showing)
		       then print "semi_sub_con calling meta_eq_con\n"
		   else ()
	   val (setter,constrain,undo,commit) = unify_maker()
	   val is_eq = meta_eq_con (setter,constrain,true) (con1,con2,ctxt)
	   val _ = if is_eq then commit() else undo()
       in  is_eq
       end

   and eq_mod (ctxt,mod1,mod2,signat) : bool = (
       case reduce_signat ctxt signat of
              SIGNAT_STRUCTURE sdecs =>
                eq_con(ctxt, CON_MODULE_PROJECT(mod1,ident_lab),
		       CON_MODULE_PROJECT(mod2,ident_lab))
	    | SIGNAT_FUNCTOR (v,s_arg,s_res,APPLICATIVE) => 
                let val ctxt' = add_context_mod'(ctxt,v,s_arg)
		in eq_mod(ctxt', MOD_APP(mod1, MOD_VAR v),
			  MOD_APP(mod2, MOD_VAR v), s_res)
		end
	    | _ => error "eq_mod invoked at non-applicative functor signature"
   )

   and path_eq_mod (ctxt,mod1,mod2) : bool = 
       eq_mpath(mod1,mod2) orelse #1(path_eq_mod' false (ctxt,mod1,mod2))

   and path_eq_mod' return_sig (ctxt,mod1,mod2) : bool * signat option = (
     let val self = path_eq_mod' return_sig
     in
       case (mod1,mod2) of
           (MOD_VAR v1, MOD_VAR v2) => 
             if eq_var(v1,v2)
		 then (true, if return_sig then SOME(#2(GetModSig(mod1,ctxt))) else NONE)
	     else (false,NONE)
	 | (MOD_PROJECT(m1,l1), MOD_PROJECT(m2,l2)) =>
             let val (b,sOpt) = self(ctxt,m1,m2)
		 val b = b andalso eq_label(l1,l2)
		 val sreturn = if b andalso return_sig 
			       then case ProjectModSig(ctxt,m1,valOf(sOpt),l1) of
                                        SOME(PHRASE_CLASS_MOD(_,_,s,_)) => SOME s
				      | _ => error "path_eq_mod': modpath ill-typed"
			       else NONE
	     in  (b,sreturn)
	     end
	 | (MOD_APP(m1,m1'), MOD_APP(m2,m2')) => 
             (case path_eq_mod' true (ctxt,m1,m2) of
                 (true,SOME(SIGNAT_FUNCTOR(v,s_arg,s_res,arrow))) =>
                      if eq_mod(ctxt,m1',m2',s_arg)
			  then (true, if return_sig 
					  then SOME(sig_subst(s_res,subst_modvar(v,m1')))
				      else NONE)
		      else (false,NONE)
	       | (true, _) => error "path_eq_mod': modpath ill-typed"
	       | _ => (false,NONE))
	 | _ => (false,NONE)
     end
   )


   and Exp_IsValuable(ctxt,exp) =
     (Exp_IsSyntacticValue exp) orelse
     (case exp of
	MODULE_PROJECT (m,l) => Module_IsValuable m ctxt
      | APP(e1,e2) => let val (va1,e1_con) = GetExpCon(e1,ctxt)
			  val (va2,e2_con) = GetExpCon(e2,ctxt)
			  val _ = debugdo (fn () =>
					   (print "exp_isvaluable: app case: e1_con is: \n";
					    Ppil.pp_con e1_con; print "\n"))
			  val e1_con_istotal =
			      (case e1_con of
				   CON_ARROW(_,_,_,comp) => eq_comp(comp,oneshot_init TOTAL,false)
				 | _ => false)
		      in va1 andalso e1_con_istotal andalso va2
		      end
      | EXTERN_APP(_,e1,es2) => let val (va1,e1_con) = GetExpCon(e1,ctxt)
			  val _ = debugdo (fn () =>
					   (print "exp_isvaluable: app case: e1_con is: \n";
					    Ppil.pp_con e1_con; print "\n"))
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
     | COERCE(coercion,cons,e) => Exp_IsValuable(ctxt,coercion) andalso Exp_IsValuable(ctxt,e)
     | ROLL (c,e) => Exp_IsValuable(ctxt,e)
     | UNROLL (c1,c2,e) => Exp_IsValuable(ctxt,e)
     | OVEREXP (_,v,os) => v orelse (case (oneshot_deref os) of
				       NONE => false
				     | SOME e => Exp_IsValuable(ctxt,e))
     | NEW_STAMP _ => true
     | SUM_TAIL (_,_,e) => Exp_IsValuable(ctxt,e)
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
	      | BND_MOD (v,_,m) => let val (va,s,_) = GetModSig(m,ctxt)
				   in if va then self (add_context_mod'(ctxt,v,s))
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
     | MOD_LET (v,m1,m2) => let val (va1,s1,_) = GetModSig(m1,ctxt)
			    in va1 andalso
				(Module_IsValuable m2 (add_context_mod'(ctxt,v,s1)))
			    end
     | MOD_PROJECT (m,l) => Module_IsValuable m ctxt
     | MOD_APP (m1,m2) => let val (va1,s1,_) = GetModSig(m1,ctxt)
			  in case s1 of
			      SIGNAT_FUNCTOR(_,_,_,TOTAL) =>
				  va1 andalso (Module_IsValuable m2 ctxt)
			    | _ => false
			  end
     | _ => false)

   and GetSconCon(ctxt,scon) : con = IlPrimUtil.value_type (fn e => #2(GetExpCon(e,ctxt))) scon


  (* Rules 35 - 48 *)
   and GetConKind (arg : con, ctxt : context) : kind =
     let fun msg() = (print "GetConKind called with con = \n";
			 pp_con arg; print "\n")
	val _ = debugdo msg
     in GetConKind'(arg,ctxt)
	  handle e => (if !trace then msg() else (); raise e)
     end

   and GetConKind' (con : con, ctxt : context) : kind =
      (case con of
       (CON_TYVAR tv) => KIND
     | (CON_VAR v) =>
	   (case Context_Lookup_Var(ctxt,v) of
		SOME(_,PHRASE_CLASS_CON(_,k,_,_)) => k
	      | SOME _ => error ("CON_VAR " ^ (var2string v) ^ " not bound to a con")
	      | NONE => error ("CON_VAR " ^ (var2string v) ^ " not bound"))
     | (CON_OVAR ocon) => KIND
     | (CON_INT _) => KIND
     | (CON_FLOAT _) => KIND
     | (CON_UINT _) => KIND
     | (CON_ANY) => KIND
     | (CON_REF _) => KIND
     | (CON_ARRAY _) => KIND
     | (CON_VECTOR _) => KIND
     | (CON_INTARRAY _) => KIND
     | (CON_INTVECTOR _) => KIND
     | (CON_FLOATARRAY _) => KIND
     | (CON_FLOATVECTOR _) => KIND
     | (CON_TAG _) => KIND
     | (CON_ARROW _) => KIND
     | (CON_COERCION _) => KIND             (* XXX Is this correct? XXX *)
     | (CON_APP (c1,cargs)) =>
	   let val k1 = GetConKind(c1,ctxt)
	       val kargs = map (fn c => GetConKind(c,ctxt)) cargs
	       val b = length kargs
	   in (case k1 of
		   KIND_ARROW(a,kres) =>
		       if (a = b)
			   then
			       if (andfold (fn KIND => true | _ => false) kargs)
				   then kres
			       else (debugdo (fn () =>
					      (print "GetConKind: arguments of app not of kind KIND in ";
					       pp_con con; print "Argument kinds = ";
					       app (fn k => (pp_kind k; print "  ")) kargs;
					       print "\n"));
				     error "GetConKind: arguments not of kind KIND")
		       else (debugdo (fn () =>
				      (print "GetConKind: kind mismatch in "; pp_con con;
				       print "\nDomain arity = "; print (Int.toString a);
				       print "\nNumber of arguments = ";
				       print (Int.toString b); print "\n"));
			     error "GetConKind: kind mismatch in CON_APP")
		 | _ => (debugdo (fn () =>
				  (print "GetConKind: bad function kind in "; pp_con con;
				   print "\nFunction kind = "; pp_kind k1; print "\n"));
			   error "GetConKind: wrong kind in CON_APP"))
	   end
     | (CON_MU c) => (case GetConKind(c,ctxt) of
			KIND_ARROW(m,kres) => KIND_TUPLE m
		      | _ => error "kind of CON_MU argument not KIND_ARROW")
     | (CON_FLEXRECORD _) => KIND
     | (CON_RECORD _) => KIND
     | (CON_FUN (vs,c)) =>
	   let fun folder(v,ctxt) = add_context_con'(ctxt,v,KIND,NONE)
	       val ctxt' = foldl folder ctxt vs
	       val kbody = GetConKind(c,ctxt')
	   in  KIND_ARROW(length vs,kbody)
	   end
     | (CON_SUM _) => KIND
     | (CON_TUPLE_INJECT cs) => KIND_TUPLE (length cs)
     | (CON_TUPLE_PROJECT (i,c)) => (case (GetConKind(c,ctxt)) of
				       KIND_TUPLE n =>
					 (if (i >= 0 andalso i < n)
					    then KIND
					  else
					    (debugdo (fn () =>
						      (print "GetConKind got: ";
						       pp_con con;
						       print "\n"));
					     error "got CON_TUPLE_PROJECT in GetConKind"))
				     | k => (debugdo (fn () =>
						      (print "GetConKind got: ";
						       pp_con con;
						       print "\nwith kind";
						       pp_kind k;
						       print "\n"));
					     error "got CON_TUPLE_PROJECT in GetConKind"))
     | (CON_MODULE_PROJECT (m,l)) =>
	   let val (_,signat,pure) = GetModSig (m,ctxt)
	   in
              if pure then
	       (case ProjectModSig (ctxt,m,signat,l)
		  of NONE => (debugdo (fn () =>
				       (print "no such label = ";
					pp_label l; print " in sig \n";
					Ppil.pp_signat signat; print "\n"));
			      error "no such label in sig")
		   | SOME(PHRASE_CLASS_CON(_,k,_,_)) => k
		   | _ => error "label in sig not a DEC_CON")
	      else error "GetConKind: attempted to project from impure module"
	   end)


   (* --------- Rules 69 to 96 ----------- *)
   and GetExpCon (exparg,ctxt) : bool * con =
     let fun msg() = (print "GetExpCon called with exp = \n";
		      pp_exp exparg; print "\n")
	val _ = debugdo msg
     in GetExpCon'(exparg,ctxt)
	  handle e => (if !trace then msg() else (); raise e)
     end

   and etaize c = (case c of
		      CON_ARROW([_],rescon,b,a) => c
		    | CON_ARROW(cons,rescon,b,a) => CON_ARROW([con_tuple cons],rescon,b,a)
		    | _ => c)

   and GetExpAppCon' (exparg,ctxt) : bool * con =
	   let val ((va1,con1),es2) =
	       (case exparg of
		    APP(e1,e2) => (GetExpCon(e1,ctxt),[e2])
		  | EXTERN_APP(_,e1,es2) => (GetExpCon(e1,ctxt),es2)
		  | PRIM(p,cs,es) => ((true, IlPrimUtil.get_type' ctxt p cs), es)
		  | ILPRIM(ip,cs,es) => ((true, IlPrimUtil.get_iltype' ctxt ip cs),es)
		  | _ => error "GetExpAppCon' got unexpected argument")
	       val con1 = HeadNormalize(con1,ctxt)
	       val vacon2 = map (fn e => GetExpCon(e,ctxt)) es2
	       val va2 = Listops.andfold #1 vacon2
	       val cons2 = map #2 vacon2
	       val res_con = fresh_con ctxt
	       val (guesscon,arrow) =
		   (case con1 of
			CON_ARROW(_,_,closed,arrow) =>
			    (CON_ARROW(cons2,res_con,closed,arrow),arrow)
		      | _ => (CON_ARROW (cons2,res_con,false,oneshot()), oneshot()))
	       val is_sub = sub_con(ctxt,guesscon,con1)
	       val total = (case oneshot_deref arrow of
				NONE => false
			      | SOME PARTIAL => false
			      | SOME TOTAL => true)
	       val va = va1 andalso va2 andalso total
	   in  if is_sub
		   then (va,con_deref res_con)
	       else (debugdo (fn () =>
			      (print "\nfunction type is = "; pp_con con1;
			       (case cons2 of
				    [con2] => (print "\nargument type is = "; pp_con con2; print "\n")
				  | _ => (print "\nargument types are = "; app pp_con cons2; print "\n"));
			       print "Type mismatch in expression application:\n";
			       pp_exp exparg; print "\n"));
		     error "Type mismatch in expression application")
	   end

   and GetExpRollCon' (ctxt,isroll,e,c) : bool * con =
       let val cNorm = HeadNormalize(c,ctxt)
	   val (va,econ) = GetExpCon(e,ctxt)
	   val error = fn str => (debugdo (fn () =>
					   (print "typing roll/unroll expression:\n";
					    Ppil.pp_exp e;
					    print "\n"));
				  error str)
	   val (i,cInner) =
	       (case cNorm of
		    CON_TUPLE_PROJECT(i,CON_MU cInner) => (i, cInner)
		  | _ => (debugdo (fn () =>
				   (print "\nUnnormalized Decoration of (UN)ROLL was ";
				    pp_con c; print "\n";
				    print "\nNormalized Decoration of (UN)ROLL was ";
				    pp_con cNorm; print "\n"));
			  error "decoration of ROLL not of the form CON_TUPLE_PROJ(CON_MU ...)"))
       in (va,
	   (case GetConKind(cInner,ctxt) of
		KIND_ARROW(n,KIND_TUPLE n') =>
		    (if ((n = n') andalso (0 <= i) andalso (i < n))
			 then
			     let
				 fun mapper j = CON_TUPLE_PROJECT(j,CON_MU cInner)
				 val cUnroll = CON_TUPLE_PROJECT(i,CON_APP(cInner, map0count mapper n))
				 val cUnroll = HeadNormalize(cUnroll,ctxt)
			     in
				 if isroll
				     then
					 (if (sub_con(ctxt,econ,cUnroll))
					      then cNorm
					  else
					      (debugdo (fn () =>
							(Ppil.pp_con econ; print "\n";
							 Ppil.pp_con cUnroll; print "\n"));
					       error "ROLL: expression type does not match decoration"))
				 else
				     (if (eq_con(ctxt,econ,cNorm))
					  then cUnroll
				      else (debugdo (fn () =>
						     (print "UNROLL: expression type does not match decoration";
						      print "\necon = "; pp_con econ;
						      print "\ncNorm = "; pp_con cNorm));
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
     | ETAPRIM (p,cs) => (true, etaize (IlPrimUtil.get_type' ctxt p cs))
     | ETAILPRIM (ip,cs) => (true, etaize(IlPrimUtil.get_iltype' ctxt ip cs))
     | VAR v => (case Context_Lookup_Var(ctxt,v) of
		     SOME(_,PHRASE_CLASS_EXP(_,c,_,_)) => (true,c)
		   | SOME(_,PHRASE_CLASS_EXT(_,_,c)) => (true,c)
		   | SOME _ => let val str = Name.var2string v
			       in  error ("VAR " ^ str ^ " looked up to a non-value")
			       end
		   | NONE => error ("GetExpCon: (VAR " ^ (Name.var2string v) ^ ") not in context"))
     | (PRIM _) => GetExpAppCon' (exparg,ctxt)
     | (ILPRIM _) => GetExpAppCon' (exparg,ctxt)
     | (APP _) => GetExpAppCon' (exparg,ctxt)
     | (EXTERN_APP _) => GetExpAppCon' (exparg,ctxt)
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
		   in va andalso sub_con(lctxt,bodyc,c')
		   end
	       fun ptest lctxt (FBND(v',v,c,c',e)) =
		   let val (_,bodyc) = GetExpCon(e,add_context_exp'(lctxt,v,c))
		       val res = sub_con(lctxt,bodyc,c')
		       val _ = if res
				   then ()
			       else debugdo (fn () =>
					     (print "ILSTATIC - ptest yielded false\n";
					      print "e = \n"; pp_exp e; print "\n";
					      print "c' = \n"; pp_con c'; print "\n";
					      print "bodyc = \n"; pp_con bodyc; print "\n"))

		   in res
		   end
	   in (true,
	       case a of
	       PARTIAL => if (andfold (ptest full_ctxt) fbnds)
			      then res_type
			  else (debugdo (fn () =>
					 (print "could not type-check FIX expression:\n";
					  pp_exp exparg; print "\n"));
				error "could not type-check FIX expression")
	     | TOTAL =>  if ((andfold (ttest ctxt) fbnds))
			     then res_type
			 else (debugdo (fn () =>
					(print "could not type-check TOTALFIX expression:\n";
					 pp_exp exparg; print "\n"));
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
	       val con' = HeadNormalize(con,ctxt)
	       fun RdecLookup (label,[]) = (debugdo (fn () =>
						     (print "RdecLookup could not find label ";
						      pp_label label; print "in type ";
						      pp_con con'));
					    error "RdecLookup could not find label")
		 | RdecLookup (label,(l,c)::rest) = if eq_label(label,l) then c
						    else RdecLookup (label,rest)
	       fun chase (ref (FLEXINFO(_,_,rdecs))) = rdecs
		 | chase (ref (INDIRECT_FLEXINFO fr)) = chase fr
	   in (case con' of
		   (CON_RECORD rdecs) => (va,RdecLookup(l,rdecs))
		 | (CON_FLEXRECORD fr) => (va,RdecLookup(l,chase fr))
		 | _ => (debugdo (fn () =>
				  (print "Record Proj on exp not of type CON_RECORD; type = ";
				   pp_con con; print "\n"));
			 error "Record Proj on exp not of type CON_RECORD"))
	   end
     | (SUM_TAIL (_,c,e)) =>
	   (case c
	      of CON_SUM{names,noncarriers,carrier,special=SOME i} =>
		  if (i<noncarriers)
		      then error "SUM_TAIL projecting noncarrier"
		  else
		      let val (va,con) = GetExpCon(e,ctxt)
		      in
			  if (eq_con(ctxt,c,con)) then
			      (va,CON_TUPLE_PROJECT((i-noncarriers),carrier))
			  else (debugdo(fn () =>
					(print "SUM_TAIL: adornment mismatches type of expression\n";
					 print "expression: "; pp_exp e; print "\n";
					 print "adornment type: "; pp_con c; print "\n";
					 print "expression type: "; pp_con con; print "\n"));
				error "SUM_TAIL: adornment mismatches type of expression")
		      end
	       | _ => (debugdo(fn () =>
			       (print "SUM_TAIL: adornment not a special sum\n";
				print "adornment type: "; pp_con c; print "\n"));
		       error "SUM_TAIL: adornment not a special sum"))
     | (HANDLE (con,body,handler)) =>
	   let val (_,bcon) = GetExpCon(body,ctxt)
	       val (_,hcon) = GetExpCon(handler,ctxt)
	       val con' = CON_ARROW ([CON_ANY],con,false,oneshot())
	   in  if (sub_con(ctxt,bcon,con) andalso sub_con(ctxt,hcon,con'))
		   then (false,con)
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
	       val (_,(va_decs,ctxt',_)) = GetBndsDecs(ctxt,bnds)
	       val (va,econ) = GetExpCon(e,ctxt')
	       val _ = (GetConKind(econ,ctxt) handle _ =>
		   error "GetExpCon: LET bnds in e -- type of e refers to bnds")
	   in (va andalso va_decs,econ)
	   end
     | (NEW_STAMP con) => ((GetConKind(con,ctxt); (true,CON_TAG con))
			   handle _ => error "NEW_STAMP: type is ill-formed")
     | (EXN_INJECT (_,e1,e2)) =>
	   let
	       val (va1,c1) = GetExpCon(e1,ctxt)
	       val (va2,c2) = GetExpCon(e2,ctxt)
	   in if (eq_con(ctxt,c1,CON_TAG c2))
		  then (va1 andalso va2, CON_ANY)
	      else error "EXN_INJECT tag type and value: type mismatch"
	   end
     | COERCE(coercion,cons,e) =>
	   let
	       val (va1,ctyp) = GetExpCon(coercion,ctxt)
	       val (tyvars,c1,c2) = (case ctyp of CON_COERCION info => info
	                             | _ => error "COERCE: coercion doesn't have coercion type")
	       val _ = if length cons = length tyvars then () else
		   error "COERCE: wrong number of type arguments"
	       val subst = foldl (fn ((v,c),subst) => subst_add_convar(subst,v,c))
		   empty_subst (zip tyvars cons)
	       val (va2,econ) = GetExpCon(e,ctxt)
	       val tau1 = con_subst(c1,subst)
	       val tau2 = HeadNormalize(con_subst(c2,subst),ctxt)
	   in
	       if sub_con(ctxt,econ,tau1) then (va1 andalso va2, tau2)
	       else error "COERCE: term argument has wrong type"
	   end
     | FOLD(tyvars,cUnroll,cRoll) =>
	   let
	       val ctxt' = foldl (fn (v,ctxt) => add_context_con'(ctxt,v,KIND,NONE)) ctxt tyvars
	       val _ = debugdo (fn () => print "Typechecking a FOLD:\n")
	       val cRollNorm = HeadNormalize(cRoll,ctxt)
	       val cRollUnrolled = ConUnroll cRollNorm
	   in
	       if eq_con(ctxt', cUnroll, cRollUnrolled) then (true,CON_COERCION(tyvars,cUnroll,cRoll))
	       else error "FOLD: unrolled type is not unrolling of rolled type"
	   end
     | UNFOLD(tyvars,cRoll,cUnroll) =>
	   let
	       val ctxt' = foldl (fn (v,ctxt) => add_context_con'(ctxt,v,KIND,NONE)) ctxt tyvars
	       val _ = debugdo (fn () => print "Typechecking an UNFOLD:\n")
	       val cRollNorm = HeadNormalize(cRoll,ctxt)
	       val cRollUnrolled = ConUnroll cRollNorm
	   in
	       if eq_con(ctxt', cUnroll, cRollUnrolled) then (true,CON_COERCION(tyvars,cRoll,cUnroll))
	       else error "UNFOLD: unrolled type is not unrolling of rolled type"
	   end
     | ROLL(c,e) => GetExpRollCon'(ctxt,true,e,c)
     | UNROLL(c,_,e) => GetExpRollCon'(ctxt,false,e,c)
     | (INJ {sumtype,field,inject}) =>
	   let val sumtype = HeadNormalize(sumtype,ctxt)
	       val {names,carrier,noncarriers,...} =
		   (case sumtype of
			CON_SUM info => info
		      | _ => error "INJ's decoration not reducible to a sumtype")
	   in
	       (case (field<noncarriers,inject) of
		    (true,NONE) =>
			(true,CON_SUM{names=names,
				      noncarriers=noncarriers,
				      carrier=carrier,
				      special=NONE})
		  | (false, NONE) => error "INJ: bad injection"
		  | (true,SOME e) => error "INJ: bad injection"
		  | (false, SOME e) =>
		      let val (va,econ) = GetExpCon(e,ctxt)
			  val carrier = HeadNormalize(carrier,ctxt)
			  val i = field - noncarriers (* i >= 0 *)
			  val (n,fieldcon_opt) =
			      (case carrier of
				   CON_TUPLE_INJECT [] => (0,NONE)
				 | CON_TUPLE_INJECT clist => (length clist, SOME(List.nth(clist,i)))
				 | _ => (1,SOME carrier))
		      in if (i >= n)
			     then
				 (debugdo (fn () =>
					   (print "INJ: injection field out of range in exp:";
					    Ppil.pp_exp exparg; print "\n"));
				  error "INJ: injection field out of range")
			 else
			     if (eq_con(ctxt, econ, valOf fieldcon_opt))
				 then (va,CON_SUM{names=names,noncarriers=noncarriers,
						  carrier=carrier,special=NONE})
			     else (debugdo (fn () =>
					    (print "INJ: injection does not type check eq_con failed on: ";
					     Ppil.pp_exp exparg; print "\n";
					     print "econ is "; Ppil.pp_con econ; print "\n";
					     print "nth clist is "; Ppil.pp_con (valOf fieldcon_opt); print "\n"));
				   error "INJ: injection does not typecheck")
		      end)
	   end
     | (EXN_CASE {arg,arms,default,tipe}) =>
	   let
	       val (_,argcon) = GetExpCon(arg,ctxt)
	       val _ = if (eq_con(ctxt,argcon,CON_ANY))
			   then ()
		       else error "arg not a CON_ANY in EXN_CASE"
	       fun checkarm(e1,c,e2) =
		   let val (_,c1) = GetExpCon(e1,ctxt)
		       val (_,c2) = GetExpCon(e2,ctxt)
		   in if ((eq_con(ctxt,c1,CON_TAG c))
			  andalso eq_con(ctxt,c2,CON_ARROW([c],tipe,false,oneshot())))
			  then ()
		      else error "rescon does not match in EXN_CASE"
		   end
	       val _ = app checkarm arms
	       val _ =
		   (case default of
			NONE => ()
		      | (SOME e) =>
			    let val (_,optcon) = GetExpCon(e,ctxt)
			    in if (eq_con(ctxt,optcon,tipe))
				   then ()
			       else (debugdo (fn () =>
					      (print "EXN_CASE: default case mismatches";
					       print "default con is:\n";
					       Ppil.pp_con optcon; print "\n";
					       print "tipe con is :\n";
					       Ppil.pp_con tipe; print "\n"));
				     error "EXN_CASE: default case mismatches")
			    end)
	   in (false, tipe)
	   end
     | (CASE {sumtype,arg,bound,arms,tipe,default}) =>
	   let
	       val sumtype = (case sumtype of
				  CON_SUM _ => sumtype
				| _ => HeadNormalize(sumtype,ctxt))
	       val {names,carrier,noncarriers,special=_} =
		   (case sumtype of
			CON_SUM info => info
		      | _ => (debugdo (fn () =>
				       (print "CASE statement is decorated with non-sumtype:\n";
					pp_exp exparg; print "\n"));
			      error "CASE statement is decorated with non-sumtype"))
	       val n = length arms
	       val carrier = HeadNormalize(carrier,ctxt)
	       val (va,eargCon) = GetExpCon(arg,ctxt)
	       val sumcon = CON_SUM {names = names,
				     special = NONE,
				     carrier = carrier,
				     noncarriers = noncarriers}
	       fun loop _ va [] =
		   (case default of
			NONE => (va, tipe)
		      | SOME edef =>
			    let val (va',defcon) = GetExpCon(edef,ctxt)
			    in  if (sub_con(ctxt,defcon,tipe))
				    then (va andalso va', tipe)
				else error "default arm type mismatch"
			    end)
		 | loop n va (NONE::rest) = loop (n+1) va rest
		 | loop n va ((SOME exp)::rest) =
			let val ctxt =  if (n < noncarriers) then ctxt
					else add_context_exp'(ctxt,bound,CON_SUM{names = names,
										 special = SOME n,
										 carrier = carrier,
										 noncarriers = noncarriers})
			    val (va',c) = GetExpCon(exp,ctxt)
			    val va = va andalso va'
			in
			    if (sub_con(ctxt,c,tipe))
				then loop (n+1) va rest
			    else (debugdo (fn () =>
					   (print "case arm type mismatch: checking exp = ";
					    pp_exp exparg; print "\n";
					    print "exp = \n"; pp_exp exp;
					    print "c = \n"; pp_con c;
					    print "tipe = \n"; pp_con tipe));
				  error "case arm type mismatch")
			end
	   in if (eq_con(ctxt,eargCon,sumcon))
		  then (loop 0 va arms)
	      else
		  (debugdo (fn () =>
			    (print "CASE: expression's type and decoration type are unequal\n";
			     print "  eargCon = "; pp_con eargCon; print "\n";
			     print "  sumcon = "; pp_con sumcon; print "\n";
			     print "  CASE exp = "; pp_exp exparg; print "\n"));
		   error "CASE: expression type and decoration type are unequal")
	   end
     | (MODULE_PROJECT(m,l)) =>
	   let val (va,signat,pure) = GetModSig(m,ctxt)
	       val c = (case ProjectModSig (ctxt,m,signat,l)
			  of (SOME (PHRASE_CLASS_EXP(_,c,_,_))) => c
			   | _ => error "MODULE_PROJECT lookup failed")
	   in  if pure andalso is_elimform m then (va,c)
	       else error "GetExpCon: attempted to project from impure module"
	   end

     | (SEAL (e,c)) => let val (va,c') = GetExpCon(e,ctxt)
		       in if sub_con(ctxt,c',c)
			      then (va,c)
			  else error "SEAL: expression type does not match sealing type"
		       end)



   (* ----------- rules 22 - 25 ------------------------- *)
   and GetBndDec (ctxt,BND_EXP (v,e))  = let val (va,c) = GetExpCon (e,ctxt)
					 in (va,DEC_EXP(v,c,NONE,false),true)
					 end
     | GetBndDec (ctxt,BND_MOD (v,b,m))  = let val (va,s,pure) = GetModSig(m,ctxt)
					 in (va,DEC_MOD(v,b,s),pure)
					 end
     | GetBndDec (ctxt,BND_CON (v,c))  = (true,DEC_CON(v,GetConKind(c,ctxt),SOME c,false),true)

   and GetBndDecFolder (bnd,(va,ctxt,pure)) =
       let val (va',dec,pure') = GetBndDec(ctxt,bnd)
       in (dec,(va andalso va', add_context_dec(ctxt,dec), pure andalso pure'))
       end

   and GetBndsDecs (ctxt,bnds) : decs * (bool * context * bool) =
       foldl_acc GetBndDecFolder (true,ctxt,true) bnds

   and GetSbndSdec (ctxt,SBND (l, bnd)) = let val (va,dec,pure) = GetBndDec(ctxt,bnd)
					  in (va,SDEC(l,dec),pure)
					  end

   and GetSbndsSdecs (ctxt,sbnds) : sdecs * (bool * context * bool) =
       foldl_acc (fn (SBND(l,bnd),stuff) =>
		  let val (dec,stuff) = GetBndDecFolder(bnd,stuff)
		  in (SDEC(l,dec),stuff)
		  end) (true,ctxt,true) sbnds



   (* ------------ Return a module's signature    -------------- *)

   and GetModSig(module, ctxt : context) : bool * signat * bool =
     let fun msg() = (print "GetModSig called with module = \n";
			 pp_mod module; print "\n")
	 val _ = debugdo msg
     in (case GetModSig'(module,ctxt)
	   of SOME r => r
	    | NONE => error "GetModSig failed")
	handle e => (if !trace then msg() else (); raise e)
     end

   and GetModSig' (module, ctxt : context) : (bool * signat * bool) option =
     (case module of
       (MOD_VAR v) =>
	   (case Context_Lookup_Var(ctxt,v) of
		SOME(_,PHRASE_CLASS_MOD(_,_,s,_)) => SOME (true,s,true)
	      | SOME _ => error ("MOD_VAR " ^ (Name.var2string v) ^ " bound to a non-module")
	      | NONE => NONE)
     | MOD_STRUCTURE (sbnds) =>
           let val (sdecs,(va,_,pure)) = GetSbndsSdecs(ctxt,sbnds)
	   in SOME (va,SIGNAT_STRUCTURE(sdecs),pure)
	   end
     | MOD_FUNCTOR (a,v,s,m,s2) =>
	   let val ctxt' = add_context_dec(ctxt,DEC_MOD(v,false,s))
	       val (va,signat,pure) = GetModSig(m,ctxt')
	       val _ =
		(case a of
		  TOTAL => va orelse error "TOTAL annotation on non-valuable functor"
		| APPLICATIVE => pure orelse error "APPLICATIVE annotation on generative functor"
		| _ => true)
(*
	       val _ = Sig_IsSub(ctxt',signat,s2) orelse
		   error "GetModSig': body of functor does not match result signature annotation"
*)
	   in  SOME (true,SIGNAT_FUNCTOR(v,s,s2,a),true)
	   end
     | MOD_APP (a,b) =>
	   let val _ = debugdo (fn () => (print "\n\nMOD_APP case in GetModSig\n";
					  print "a is\n"; pp_mod a; print "\n";
					  print "b is\n"; pp_mod b; print "\n"))
	       val (vaa,asignat,purea) = GetModSig(a,ctxt)
	       val (vab,bsignat,pureb) = GetModSig(b,ctxt)
	       val _ = debugdo (fn () => (print "\n\nMOD_APP case in GetModSig got asignat and bsignat\n";
					  print "asignat is\n"; pp_signat asignat; print "\n";
					  print "bsignat is\n"; pp_signat bsignat; print "\n"))
	       val _ = pureb orelse error "trying to apply functor to impure argument"
	   in case (reduce_signat ctxt asignat) of
	       SIGNAT_FUNCTOR (v,csignat,dsignat,ar) =>
(*
		   if (Sig_IsSub(ctxt, bsignat, csignat))
		       then 
*)
			    SOME (vaa andalso vab andalso (ar = TOTAL),
				  sig_subst(dsignat,subst_modvar(v,b)),
				  purea andalso pureb andalso ((ar = APPLICATIVE) orelse (ar = TOTAL)))
(*
			    else (print "Signat of actual arg:"; pp_signat bsignat;
				  print "\nSignat of formal arg:"; pp_signat csignat; print "\n";
				  error "MOD_APP argument and parameter signature mismatch")
*)
	     | SIGNAT_STRUCTURE _ => error "Can't apply a structure"
	     | _ => error "signat_var is not reduced"
	   end
     | MOD_LET (v,m1,m2) =>
	   let val (va1,s1,pure1) = GetModSig(m1,ctxt)
	       val ctxt' = add_context_mod'(ctxt,v,s1)
	       val (va2,s2,pure2) = GetModSig(m2,ctxt')
(*
	       val _ = Sig_Valid(ctxt,s2) orelse
		       error "result signature of MOD_LET refers to hidden bound variable"
*)
	   in  SOME (va1 andalso va2,s2,pure1 andalso pure2)
	   end
     | MOD_PROJECT (m,l) =>
	   (case GetModSig'(m,ctxt)
	      of NONE => NONE
	       | SOME (_,_,false) => error "trying to obtain signature of impure projection"
	       | SOME (va,signat,true) =>
		   (case ProjectModSig (ctxt,m,signat,l)
		      of SOME (PHRASE_CLASS_MOD(_,_,s,_)) => SOME (va,s,true)
		       | _ => NONE))
     | MOD_SEAL (m,s) => 
		let val (va,ps,pure) = GetModSig(m,ctxt)
(*
		    val _ = Sig_IsSub(ctxt,ps,s) orelse
			error "MOD_SEAL: Sig_IsSub failed"
*)
		in SOME (va,s,pure)
		end
    )


    and Normalize (con,ctxt) : con =
	let fun msg() = (print "Normalize called with con =\n";
			 pp_con con; print "\n")
	    val _ = debugdo msg
	in  (case Reduce NORM (con,ctxt) of
		 NONE => con
	       | SOME(c) => c)
	  handle e => (if !trace then msg() else (); raise e)
	end


    and NormOnce (con,ctxt) : con option =
	let fun msg() = (print "NormOnce called with con =\n";
			 pp_con con; print "\n")
	    val _ = debugdo msg
	in  Reduce ONCE (con,ctxt)
	  handle e => (if !trace then msg() else (); raise e)
	end


    and HeadNormalize (con,ctxt) : con =
	let fun msg() = (print "HeadNormalize called with con =\n";
			 pp_con con; print "\n")
	    val _ = debugdo msg
	in  (case Reduce HEAD (con,ctxt) of
		 NONE => con
	       | SOME(c) => c)
	  handle e => (if !trace then msg() else (); raise e)
	end


    and Reduce how (argcon,ctxt) : con option =
       let
	    fun msg() = (print "Reduce called with argcon =\n";
			 pp_con argcon; print "\n")
	    val _ = debugdo msg
	    fun ReduceAgain c =
		(case how of
		    ONCE => SOME(c)
		|   _ =>
			(case (Reduce how (c,ctxt)) of
			    NONE => SOME(c)
			 |  r => r))
	    fun reducelist clist : con list option =
		let fun folder (c,change) =
			(case Reduce how (c,ctxt) of
			    NONE => (c, change)
			 |  SOME(c) => (c, true))
		    val (clist,change) = foldl_acc folder false clist
		in  if change then SOME clist else NONE
		end
	    fun helplist constr clist =
		(case how of
		    HEAD => NONE
		 |  _ =>
			(case (reducelist clist) of
			    SOME clist => SOME(constr clist)
			 |  NONE => NONE))
	    fun help constr c = helplist (fn clist => constr (hd clist)) [c]
       in
	   (case argcon of
	      CON_INT _ => NONE
	    | CON_UINT _  => NONE
	    | CON_FLOAT _ => NONE
	    | CON_ANY => NONE
	    | CON_REF c => help CON_REF c
	    | CON_ARRAY c => help CON_ARRAY c
	    | CON_VECTOR c => help CON_VECTOR c
	    | CON_INTARRAY _ => NONE
	    | CON_INTVECTOR _ => NONE
	    | CON_FLOATARRAY _ => NONE
	    | CON_FLOATVECTOR _ => NONE
	    | CON_TAG c => help CON_TAG c
	    | CON_MU c => help CON_MU c
	    | CON_SUM{names,carrier,noncarriers,special} =>
		  let fun constr c = CON_SUM{names=names,
					     carrier=c,
					     noncarriers=noncarriers,
					     special=special}
		  in  help constr carrier
		  end
	    | CON_COERCION ([],c1,c2) =>
		  let fun constr [c1,c2] = CON_COERCION([],c1,c2)
		  in helplist constr [c1,c2]
		  end
	    | CON_COERCION (tyvars,c1,c2) =>
		  let fun folder (v,ctxt) = add_context_con'(ctxt,v,KIND,NONE)
		      val ctxt' = foldl folder ctxt tyvars
		  in (case Reduce how (CON_COERCION([],c1,c2),ctxt') of
		      NONE => NONE
	            | SOME (CON_COERCION([],c1,c2)) => SOME (CON_COERCION(tyvars,c1,c2))
		    | _ => error "CON_COERCION mishandled in Reduce function")
		  end
	    | CON_ARROW (cargs,cres,closed,comp) =>
		  let fun constr [] = error "CON_ARROW must have at least one domain type"
			| constr (cres :: cargs) = CON_ARROW(cargs, cres, closed, comp)
		  in  helplist constr (cres :: cargs)
		  end
	    | CON_RECORD rdecs =>
		  let val (labs,cons) = Listops.unzip rdecs
		      fun constr cons = CON_RECORD(Listops.zip labs cons)
		  in  helplist constr cons
		  end
	    | CON_TUPLE_INJECT cons => helplist CON_TUPLE_INJECT cons

	    | CON_FLEXRECORD (ref (INDIRECT_FLEXINFO rf)) =>
		  ReduceAgain (CON_FLEXRECORD rf)
	    | CON_FLEXRECORD (r as ref (FLEXINFO (stamp,flag,rdecs))) =>
		  let val (labs,cons) = Listops.unzip rdecs
		      fun constr cons =
			  let val rdecs = Listops.zip labs cons
			      val _ = r := FLEXINFO(stamp,flag,rdecs)
			  in  argcon
			  end
		  in  helplist constr cons
		  end
	    | CON_OVAR ocon => ReduceAgain (CON_TYVAR (ocon_deref ocon))
	    | CON_TYVAR tv => (case tyvar_deref tv of
				    NONE => NONE
				  | SOME c => ReduceAgain c)
	    | CON_VAR v =>
		   (case (Context_Lookup_Var(ctxt,v)) of
			SOME(_,PHRASE_CLASS_CON (_,_,SOME c,_)) => ReduceAgain c
		      | SOME(_,PHRASE_CLASS_CON (_,_,NONE,_)) => NONE
		      | SOME _ => error ("Normalize: CON_VAR " ^ (var2string v) ^ " not bound to a con")
		      | NONE => NONE)
	    | CON_TUPLE_PROJECT (i,CON_TUPLE_INJECT cons) =>
		  let val len = length cons
		      val _ = if (i >= 0 andalso i < len) then ()
			      else error "Reduce: con tuple projection - index wrong"
		      val c = List.nth(cons,i)
		  in  ReduceAgain c
		  end
	    | CON_TUPLE_PROJECT (i,c) =>
		  (case (Reduce how (c,ctxt)) of
		       NONE => NONE
		   |   SOME(c) =>
			   let val reduced = CON_TUPLE_PROJECT(i,c)
			   in  (case c of
				    CON_TUPLE_INJECT _ => ReduceAgain reduced
				|   _ => SOME reduced)
			   end)
	    | CON_FUN(formals,CON_APP(f,args)) =>
		let fun match (v,(CON_VAR v')) = eq_var(v,v')
		      | match _ = false
		in  (* --- Eta contract functions --- *)
		    if (eq_list(match,formals,args))
		      then ReduceAgain f
		    else NONE
		end
	    | CON_FUN _ => NONE
	    | CON_APP(f as CON_FUN _,args) =>
		  let val c =
			(case (reducelist args) of
			    SOME args => CON_APP(f,args)
			 |  NONE => ConApply(false,f,args))
		  in  ReduceAgain c
		  end
	    | CON_APP(f,args) =>
		  (case (Reduce how (f,ctxt)) of
		       SOME (f) => ReduceAgain(CON_APP(f,args))
		   |   NONE =>
			   (case (reducelist args) of
				SOME args => SOME(CON_APP(f,args))
			    |   NONE => NONE))
	    | CON_MODULE_PROJECT (m,l) =>
		(let
		   val (_,s,_) = GetModSig(m,ctxt)
		   val cOpt : con option =
			(case ProjectModSig (ctxt,m,s,l)
			   of SOME(PHRASE_CLASS_CON(_,_,cOpt,_)) => cOpt
			    | _ => (print "Module projection can't be normalized because label ";
				    pp_label l;
				    print " is not exported by module ";
				    pp_mod m;
				    print "\n";
				    error "Module projection can't be normalized"))
		   val reducedOpt : con option =
		       (case cOpt of SOME c => if eq_cpath(argcon,c) then NONE else ReduceAgain c
                                   | NONE => NONE)
		 in  reducedOpt
		 end))
       end

     and Kind_Valid (KIND) = true
       | Kind_Valid (KIND_TUPLE n)     = n >= 0
       | Kind_Valid (KIND_ARROW (m,kres)) = (m >= 0) andalso Kind_Valid(kres)

     and Decs_Valid (ctxt,[]) = true
       | Decs_Valid (ctxt,a::rest) = (Dec_Valid(ctxt,a) andalso
				      Decs_Valid(add_context_dec(ctxt,a),rest))

     and Dec_Valid (ctxt : context, dec) =
	 let fun var_notin v  = not (isSome (Context_Lookup_Var_Raw(ctxt,v)))
       in  (case dec of
	      DEC_EXP(v,c,eopt,_) =>
		  (var_notin v) andalso
		  (case GetConKind(c,ctxt) of
		       KIND => true
		     | _ => false) andalso
		  (case eopt of
		       SOME e => eq_con(ctxt,c,#2 (GetExpCon(e,ctxt)))
		     | NONE => true)
	    | DEC_CON (v,k,NONE, _) => (var_notin v) andalso (Kind_Valid(k))
	    | DEC_CON (v,k,SOME c, _) => (var_notin v) andalso (Kind_Valid(k))
		       andalso (GetConKind(c,ctxt); true)
	    | DEC_MOD(v,_,s) => (var_notin v) andalso (Sig_Valid(ctxt,s)))
       end


     and Sdecs_Domain sdecs = map (fn SDEC(l,_) => l) sdecs
     and Sdecs_Valid (ctxt, []) = true
       | Sdecs_Valid (ctxt, (SDEC(label,dec)::rest)) =
	 (Dec_Valid(ctxt,dec) andalso
	  Sdecs_Valid(add_context_dec(ctxt,dec),rest) andalso
	  (not (List.exists (fn l => eq_label(label,l))
		(Sdecs_Domain rest))))

     and Sig_Valid (ctxt : context, SIGNAT_STRUCTURE sdecs) = Sdecs_Valid(ctxt,sdecs)
       | Sig_Valid (ctxt, SIGNAT_FUNCTOR(v,s_arg,s_res,arrow)) =
	 (Sig_Valid(ctxt,s_arg) andalso
	  Sig_Valid(add_context_mod'(ctxt,v,s_arg),s_res))
       | Sig_Valid (ctxt : context, SIGNAT_VAR v) = Sig_Valid(ctxt,reduce_sigvar(ctxt,v))

     and Dec_IsSub (ctxt,d1,d2) = Dec_IsSub' true (ctxt,d1,d2)
     and Dec_IsEqual (ctxt,d1,d2) = Dec_IsSub' false (ctxt,d1,d2)

     and Dec_IsSub' isSub (ctxt,d1,d2) =
	 (case (d1,d2) of
	      (DEC_MOD(v1,b1,s1),DEC_MOD(v2,b2,s2)) =>
		  eq_var(v1,v2) andalso (b1 = b2) andalso
                  (if isSub then Sig_IsSub(ctxt,s1,s2) else Sig_IsEqual(ctxt,s1,s2))
	    | (DEC_CON(v1,k1,NONE,inline1),DEC_CON(v2,k2,NONE,inline2)) =>
		  eq_var(v1,v2) andalso eq_kind(k1,k2) andalso inline1=inline2
	    | (DEC_CON(v1,k1,SOME c1,inline1),DEC_CON(v2,k2,NONE,inline2)) =>
		  isSub andalso eq_var(v1,v2) andalso eq_kind(k1,k2)
		  andalso eq_kind(k1,GetConKind(c1,ctxt))
		  andalso inline1=inline2
	    | (DEC_CON(v1,k1,NONE,_),DEC_CON(v2,k2,SOME _,_)) => false
	    | (DEC_CON(v1,k1,SOME c1,inline1),DEC_CON(v2,k2,SOME c2,inline2)) =>
		  eq_var(v1,v2) andalso eq_kind(k1,k2)
		  andalso if isSub then sub_con(ctxt,c1,c2) else eq_con(ctxt,c1,c2)
		  andalso inline1=inline2
	    | (DEC_EXP(v1,c1,eopt1,inline1),DEC_EXP(v2,c2,eopt2,inline2)) =>
		  eq_var(v1,v2) andalso (if isSub then sub_con(ctxt,c1,c2) else eq_con(ctxt,c1,c2))
		  andalso (case (eopt1,eopt2) of
			       (SOME e1, SOME e2) => true
			     | (NONE, NONE) => true
			     | (NONE, SOME _) => false
			     | (SOME _, NONE) => isSub)
		  andalso inline1=inline2
	    | _ => false)

     (*
	We should get rid of eq_exp if Perry approves of the change to
	signature.sml to assume that inlined expressions match inlined
	specs.

	Eq_exp gets called as part of interface equality and the
	TiltPrim unit inlines all primitives.
    *)

    (* Warning: expression equality seems to only cover enough cases
     * to cover TiltPrim.  -leaf
     *)

     and eq_exp(ctxt,exp1,exp2) =
	 let fun find subst v = (case Listops.assoc_eq(eq_var,v,subst) of
				     NONE => v
				   | SOME v => v)
	     fun eq_cons (cs, cs') =
		 andfold (fn (c1,c2) => eq_con(ctxt,c1,c2)) (zip cs cs')
	     fun add subst (v1,v2) = (v1,v2)::subst
	     fun eq_exps (es, es', subst) =
		 andfold (fn (e1,e2) => eq(e1,e2,subst)) (zip es es')
	     and eq_expopt (e1opt, e2opt, subst) =
		  Util.eq_opt(fn (e1,e2) => eq(e1,e2,subst), e1opt, e2opt)
	     and eq (e1,e2,subst) =
	     (case (e1,e2) of
		 (PRIM (p1,cs1,es1), PRIM (p2,cs2,es2)) =>
		     Prim.same_prim (p1,p2) andalso
		     eq_cons (cs1,cs2) andalso
		     eq_exps (es1,es2,subst)
	       | (ILPRIM (p1,cs1,es1), ILPRIM (p2,cs2,es2)) =>
		     Prim.same_ilprim (p1,p2) andalso
		     eq_cons (cs1,cs2) andalso
		     eq_exps (es1,es2,subst)
	       | (ETAPRIM (p1,cs1), ETAPRIM (p2,cs2)) =>
		     Prim.same_prim (p1,p2) andalso eq_cons (cs1,cs2)
	       | (ETAILPRIM (p1,cs1), ETAILPRIM (p2,cs2)) =>
		     Prim.same_ilprim (p1,p2) andalso eq_cons (cs1,cs2)
	       | (VAR v1, VAR v2) => let val v2 = find subst v2
				      in  eq_var(v1,v2)
				      end
	       | (FIX(b1,a1,fbnds1), FIX(b2,a2,fbnds2)) =>
		     let fun loop subst [] [] = subst
			   | loop subst ((FBND(v1,_,_,_,_)) :: rest1)
			                ((FBND(v2,_,_,_,_)) :: rest2) = loop (add subst (v2,v1)) rest1 rest2
			   | loop subst _ _ = subst
			 val subst = loop subst fbnds1 fbnds2
			 fun eq_fbnd (FBND(_,v1,c1,_,b1),FBND(_,v2,c2,_,b2)) =
			     let val subst = add subst (v2,v1)
			     in  eq(b1,b2,subst) andalso eq_con(ctxt,c1,c2)
			     end
		     in  b1 = b2 andalso a1 = a2 andalso eq_list(eq_fbnd,fbnds1,fbnds2)
		     end
	       | (RECORD_PROJECT (e1,l1,c1), RECORD_PROJECT (e2,l2,c2)) =>
		     eq(e1,e2,subst) andalso eq_label(l1,l2) andalso
		     eq_con(ctxt,c1,c2)
	       | (RECORD  le1,RECORD le2) => 
		     eq_list (fn ((l1,e1),(l2,e2)) => (eq_label (l1,l2) andalso eq(e1,e2,subst)),le1,le2)
	       | (APP (e11,e12),APP (e21,e22)) => eq(e11,e21,subst) andalso eq(e12,e22,subst)
	       | (COERCE (coercion,cs,e), COERCE (coercion',cs',e')) =>
		     eq(coercion,coercion',subst) andalso eq(e,e',subst) andalso
		     eq_cons(cs,cs')
	       | (UNROLL (c1,d1,e1), UNROLL(c2,d2,e2)) =>
		     eq_con(ctxt,c1,c2) andalso  eq_con(ctxt,d1,d2) andalso eq(e1,e2,subst)
	       | (ROLL (c1,e1), ROLL(c2,e2)) =>
		     eq_con(ctxt,c1,c2) andalso eq(e1,e2,subst)
	       | (SUM_TAIL (i1,c1,e1), SUM_TAIL (i2,c2,e2)) =>
		     i1 = i2 andalso eq_con(ctxt,c1,c2) andalso eq(e1,e2,subst)
	       | (INJ {sumtype=s1,field=f1,inject=e1opt},
		  INJ {sumtype=s2,field=f2,inject=e2opt}) =>
		     eq_con(ctxt,s1,s2) andalso f1 = f2 andalso
		     eq_expopt(e1opt,e2opt,subst)
	       | (MODULE_PROJECT _, MODULE_PROJECT _) => eq_epath(e1,e2)
	       | (CASE {sumtype=s1, arg=arg1, bound=v1, arms=arms1, tipe=c1, default=e1opt},
		  CASE {sumtype=s2, arg=arg2, bound=v2, arms=arms2, tipe=c2, default=e2opt}) =>
		     let val subst' = add subst (v2,v1)
			 fun eqarm (e1opt,e2opt) = eq_expopt(e1opt,e2opt,subst')
		     in  eq_con(ctxt,s1,s2) andalso eq(arg1,arg2,subst) andalso
			 andfold eqarm (zip arms1 arms2) andalso
			 eq_con(ctxt,c1,c2) andalso eqarm(e1opt,e2opt)
		     end
	       | _ => (debugdo (fn () =>
				(print "eq_exp failed with \n    exp1 = ";
				 pp_exp e1; print "\n\nand exp2 = ";
				 pp_exp e2; print "\n\n"));
		       false))
	 in  eq(exp1,exp2,[])
	 end

     (* Rules 99 - 100 *)
     and Sdecs_IsSub (ctxt,sdecs1,sdecs2) = Sdecs_IsSub' true (ctxt,sdecs1,sdecs2)
     and Sdecs_IsEqual (ctxt,sdecs1,sdecs2) = Sdecs_IsSub' false (ctxt,sdecs1,sdecs2)

     and Sdecs_IsSub' isSub (ctxt,sdecs1,sdecs2) =
	 let
	     exception NOPE
	     fun match_var subst [] [] = []
	       | match_var subst (SDEC(l1,dec1)::rest1) ((sdec2 as (SDEC(l2,dec2)))::rest2) : sdec list =
		 if (eq_label (l1,l2))
		     then
		       (case (dec1,dec2) of
			       (DEC_MOD(v1,b1,s1),DEC_MOD(v2,b2,s2)) =>
				   let val s2 = sig_subst(s2,subst)
				       val sdec2 = SDEC(l1,DEC_MOD(v1,b2,s2))
				       val subst = if (eq_var(v1,v2))
						       then subst
						   else subst_add_modvar(subst, v2, MOD_VAR v1)
				   in  sdec2 :: (match_var subst rest1 rest2)
				   end
			     | (DEC_EXP(v1,c1,e1,i1),DEC_EXP(v2,c2,e2,i2)) =>
				   let val c2 = con_subst(c2,subst)
				       val e2 = (case e2 of
						     NONE => NONE
						   | SOME e2 => SOME(exp_subst(e2,subst)))
				       val sdec2 = SDEC(l1,DEC_EXP(v1,c2,e2,i2))
				       val subst = if (eq_var(v1,v2))
						       then subst
						   else subst_add_expvar(subst, v2, VAR v1)
				   in  sdec2 :: (match_var subst rest1 rest2)
				   end
			     | (DEC_CON(v1,k1,c1,i1),DEC_CON(v2,k2,c2,i2)) =>
				   let val c2 = (case c2 of
						     NONE => NONE
						   | SOME c2 => SOME(con_subst(c2,subst)))
				       val sdec2 = SDEC(l1,DEC_CON(v1,k2,c2,i2))
				       val subst = if (eq_var(v1,v2))
						       then subst
						   else subst_add_convar(subst, v2, CON_VAR v1)
				   in  sdec2 :: (match_var subst rest1 rest2)
				   end
			     | _ => sdec2::(match_var subst rest1 rest2))
		 else (debugdo (fn () => print "Sdecs_IsSub: label mismatch\n");
		       raise NOPE)
	       | match_var _ _ _ = (debugdo (fn () => print "Sdecs_IsSub: length mismatch\n");
				    raise NOPE)
	     fun loop ctxt [] [] = true
	       | loop ctxt (SDEC(_,dec1)::rest1) (SDEC(_,dec2)::rest2) =
		 (Dec_IsSub' isSub (ctxt,dec1,dec2)
		  andalso loop (add_context_dec(ctxt,dec1)) rest1 rest2)
	       | loop ctxt _ _ = false
	 in (loop ctxt sdecs1 (match_var empty_subst sdecs1 sdecs2))
	     handle NOPE => false
	 end

     (* Rules 109 - 112 *)
     and Sig_IsSub (ctxt, sig1, sig2) = Sig_IsSub' true (ctxt, sig1, sig2)
     and Sig_IsEqual (ctxt, sig1, sig2) = Sig_IsSub' false (ctxt, sig1, sig2)

     and Sig_IsSub' isSub (ctxt, sig1, sig2) =
	 (case (reduce_signat ctxt sig1,reduce_signat ctxt sig2)
	    of (SIGNAT_STRUCTURE sdecs1, SIGNAT_STRUCTURE sdecs2) =>
		Sdecs_IsSub' isSub (ctxt,sdecs1,sdecs2)
	      | (SIGNAT_FUNCTOR(v1,s1_arg,s1_res,a1),SIGNAT_FUNCTOR(v2,s2_arg,s2_res,a2)) =>
		((eq_sigarrow(a1,a2,isSub)) andalso
		 let val s1_res = if (eq_var(v1,v2)) then s1_res
				  else sig_subst(s1_res,subst_modvar(v1,MOD_VAR v2))
		     val b1 = Sig_IsEqual (ctxt,s2_arg,s1_arg)
		     val ctxt' = add_context_dec(ctxt,DEC_MOD(v2,false,s2_arg))
		     val b2 = Sig_IsSub' isSub (ctxt',s1_res,s2_res)
		     val _ = debugdo (fn () =>
				      (if b1 then () else print "argument sig mismatch\n";
				       if b2 then () else print "result sig mismatch\n" ))
		 in  b1 andalso b2
		 end)
	      | _ => (print "Warning: ill-formed call to Sig_IsSub'\n";
		      debugdo (fn () =>
			       (print " with sig1 = \n";
				pp_signat sig1; print "\n and sig2 = \n";
				pp_signat sig2; print "\n"));
		      false))

    (* Peel_Mod peels off any leading existentials, rds's or compsig's.
       Returns the corresponding labels, and the peeled module and its signature.
    *)
    and PeelModSig ctxt (pc as PHRASE_CLASS_MOD(m : mod, poly, s : signat, _)) : (labels * phrase_class) =
      let 
          fun peel (m,s,acc_labels) : labels * mod * signat =
            let val s = reduce_signat ctxt s in
              case is_existential_sig s of
                SOME(_,_,visible_sig) =>
                  peel(MOD_PROJECT(m,visible_lab),
		       visible_sig,
		       visible_lab::acc_labels)
	      | NONE =>
                  (* XXX Implement other cases for rds's and compsig's here. -Derek *)
                  (rev acc_labels,m,s)
            end
          val (labs,m,s) = peel(m,s,[])
      in
	  if null labs then ([],pc)
	  else (labs,PHRASE_CLASS_MOD(m,poly,s,fn () => s))
      end
      | PeelModSig _ pc = ([],pc)

    and Sig_Lookup_Label (x as (doOpen : bool, ctxt : context, lbl : label)) (m : mod, s : signat)
        : (labels * phrase_class) option =
        (case reduce_signat ctxt s of
            SIGNAT_STRUCTURE sdecs => Sdecs_Lookup_Label x (m,sdecs)
            (* XXX Implement other cases for rds's and compsig's here. -Derek *)
	  | _ => NONE)

    and Sdecs_Lookup_Label (x as (doOpen : bool, ctxt : context, lbl : label)) (m : mod, sdecs)
	: (labels * phrase_class) option =
	let
	    fun find [] = NONE
	      | find (SDEC(l,d)::rest) =
		if eq_label(l,lbl) then
		    let val p = (m,l)
			val pc =
			    (case d
			       of DEC_EXP (_,c,eopt,inline) =>
				   PHRASE_CLASS_EXP(MODULE_PROJECT p,c,eopt,inline)
				| DEC_CON (_,k,copt,inline) =>
				   PHRASE_CLASS_CON(CON_MODULE_PROJECT p,k,copt,inline)
				| DEC_MOD (_,poly,s) =>
				   PHRASE_CLASS_MOD(MOD_PROJECT p,poly,s,fn () => s))
		    in  SOME ([l],pc)
		    end
		else
		    (case (doOpen andalso is_open l, d)
		       of (true, DEC_MOD(_,_,s)) =>
                           (case Sig_Lookup_Label x (MOD_PROJECT(m,l),s) of
                                SOME (lbls,pc) => SOME(l::lbls,pc)
			      | NONE => find rest)
			| _ => find rest)
	in  
	    find (rev sdecs)
	end

    and Sig_Lookup_Labels (doOpen : bool, ctxt : context, labs : labels) (m : mod, s : signat)
	: (labels * phrase_class) option =
	(case labs
	   of [] => NONE
	    | [lbl] => (case Sig_Lookup_Label (doOpen,ctxt,lbl) (m,s) of
	                   (res as SOME (labs1,pc)) =>
			     if doOpen then
			       let val (labs2,pc) = PeelModSig ctxt pc
			       in SOME(labs1 @ labs2, pc)
			       end
			     else res
			 | NONE => NONE)
	    | (lbl :: lbls) =>
	       (case Sig_Lookup_Label (doOpen,ctxt,lbl) (m,s)
		  of SOME(labs1,PHRASE_CLASS_MOD (m',_,s',_)) =>
		      (case Sig_Lookup_Labels (doOpen,ctxt,lbls) (m',s') of
			   SOME(labs2,pc) => SOME(labs1 @ labs2, pc)
			 | NONE => NONE)
		   | _ => NONE))

    and ProjectModSig (ctxt, m, s : signat, l) : phrase_class option =
        Option.map #2 (Sig_Lookup_Labels (false,ctxt,[l]) (m,s))

    and Sig_Lookup ctxt (m : mod, s : signat, labs : labels) : (labels * phrase_class) option =
        Sig_Lookup_Labels (true,ctxt,labs) (m,s)

    and Sdecs_Lookup ctxt (m, sdecs, labs) : (labels * phrase_class) option = 
	Sig_Lookup ctxt (m, SIGNAT_STRUCTURE sdecs, labs)

    and Context_Lookup_Path (ctxt, p as PATH (v,labs)) : (path * phrase_class) option =
	(case (Context_Lookup_Var (ctxt,v),labs) of
            (* v is a module *)
	      (SOME (_,pc as PHRASE_CLASS_MOD (_,_,signat,_)),_) =>
                 Option.map (fn (labs',pc') => (PATH(v,labs'),pc'))
		   (if null labs then SOME(PeelModSig ctxt pc)
		    else Sig_Lookup ctxt (MOD_VAR v, signat, labs))
            (* v is not a module, which is fine so long as there is no attempt to project from it *)
	    | (SOME (_,pc),nil) => SOME(p,pc)
	    | _ => NONE)

    and Context_Lookup_Labels (ctxt : context, labs : labels) : (path * phrase_class) option =
	(case labs
	   of nil => NONE
	    | (lab :: labs1) =>
	       (case Context_Lookup_Label (ctxt,lab)
		  of NONE => NONE
		   | SOME path => Context_Lookup_Path (ctxt, join_path_labels(path,labs1))))


    fun con_reduce_once (context,c) = NormOnce(c,context)
    fun con_normalize (context,c) = Normalize(c,context)
    fun con_head_normalize (context,c) = HeadNormalize(c,context)

    val sub_sigarrow = fn (s1,s2) => eq_sigarrow(s1,s2,true)
    val eq_sigarrow = fn (s1,s2) => eq_sigarrow(s1,s2,false)

    val GetExpCon = fn (d,e) => #2(GetExpCon(e,d))
    val GetConKind = fn (d,c) => GetConKind(c,d)
    val GetModSigPurity = fn (d,m) => let val (_,s,p) = GetModSig(m,d) in (s,p) end
    val GetModSig = fn (d,m) => #2(GetModSig(m,d))
    val Module_IsValuable = fn (d,m) => Module_IsValuable m d
    val Bnds_IsValuable = fn (d,bs) => Bnds_IsValuable bs d
    val Sbnds_IsValuable = fn (d,ss) => Sbnds_IsValuable ss d

    val PeelModSig = fn ctxt => fn (m,s) => PeelModSig ctxt (PHRASE_CLASS_MOD(m,false,s,fn () => s))

  end
