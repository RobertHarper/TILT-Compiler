(*$import List Int ILCONTEXT Il Util Name Listops Ppil Stats IlUtil Fixity Option *)

structure IlContext :> ILCONTEXT =
struct

    open Il Util Name Listops Ppil IlUtil
    type fixity = Fixity.fixity
	
    val error = fn s => error "ilcontext.sml" s
	
    val debug = Stats.ff("IlcontextDebug")
    fun debugdo t = if (!debug) then (t(); ()) else ()

    val IlcontextChecks = Stats.ff "IlcontextChecks"
    fun sanitycheck t = if (!IlcontextChecks) then (t(); ()) else ()
	    
    (* Setting this to true currently does not work.  Once it does, we
       should kill off the "false" code and the flag. *)
    val NoOpenInternalPaths = Stats.ff "NoOpenInternalPaths" (* XXX *)
	
    (* ------ Type equivalence needed to ensure overloading resolvable by distinct types ------- *)
    local 
	val Ceq_con = ref (NONE : (context * con * con -> bool) option)
	fun eq_con arg = let val SOME eq_con = !Ceq_con
			 in  eq_con arg
			 end
	fun eq ctxt ((c1,e1,d1),(c2,e2,d2)) = eq_con(ctxt,c1,c2)
	fun sub ctxt (exp1, exp2) = Listops.list_diff_eq(eq ctxt, exp1, exp2)
	fun add ctxt (exp1, exp2) = Listops.list_sum_eq(eq ctxt, exp1, exp2)
	fun expanded f (o1, o2) = ovld_collapse (f (ovld_expand o1, ovld_expand o2))
    in
	fun installHelpers{eq_con} = 
	    ((case !Ceq_con of
		 NONE => ()
	       | SOME _ => (print "WARNING: IlContext.installHelpers called more than once.\n"));
	     Ceq_con := SOME eq_con)
	fun ovld_add (label, ctxt, ovld1 as OVLD (_, d1), ovld2 as OVLD (_, d2)) =
	    let val _ = case (d1, d2)
			  of (SOME _, SOME _) =>
			      (print "overloaded identifier with ambiguous default: ";
			       pp_label label; print "\n";
			       print "ovld1 = "; pp_ovld ovld1; print "\n";
			       print "ovld2 = "; pp_ovld ovld2; print "\n";
			       error "overloaded identifier with ambiguous default")
			   | _ => ()
	    in  expanded (add ctxt) (ovld1, ovld2)
	    end
	fun ovld_sub (ctxt, ovld1, ovld2) = expanded (sub ctxt) (ovld1, ovld2)
    end

    (* ----------- signature utilities ----------------------------  *)

    fun Context_Lookup_Var_Raw (ctxt : context, v : var) : (label * phrase_class) option =
	let val CONTEXT {varMap,...} = ctxt
	in  VarMap.find (varMap, v)
	end

    (*
	When a module variable is looked up, its signature is
	transformed and memoized.  The transformations are (deep)
	reduction, local variable elimination, and selfification.
	Reduction is also needed when a starred structure is bound.
    *)
    local

	(* An "lvs" is a substitution from local variables to bound
	   paths.  This is the book-keeping needed to project
	   componenets from a bound structure.*)

	type lvs = mod * subst

	fun lvs (m : mod) : lvs = (m, empty_subst)

	fun lvs_extend ((m,subst) : lvs, sdec) : lvs =
	    let val SDEC(l,dec) = sdec
		val p = (m,l)
		val subst = 
		    (case dec
		       of DEC_EXP (v,_,_,_) =>
			   (* The only local expression variables that
			      may appear in inlined expressions are
			      coercions.  *)
			   if is_coercion l
			       then subst_add_expvar (subst, v, MODULE_PROJECT p)
			   else subst
			| DEC_CON (v,_,_,_) => subst_add_convar (subst, v, CON_MODULE_PROJECT p)
			| DEC_MOD (v,_,_) => subst_add_modvar (subst, v, MOD_PROJECT p))
	    in  (m,subst)
	    end

	(* Rewrite SIGNAT_OF(localvar.labs) as SIGNAT_OF(v.labs')
	   where v is bound in the ambient context. *)
	fun lvs_rewrite ((_,subst) : lvs, s : signat) : signat =
	    (case s
	       of SIGNAT_OF _ => sig_subst(s, subst)
		| _ => s)

	fun lvs_recurse ((m,subst) : lvs, l : label, s : signat) : lvs =
	    let val subst = (case s
			       of SIGNAT_VAR _ => empty_subst
				| SIGNAT_OF _ => empty_subst
				| _ => subst)
		val m = MOD_PROJECT (m,l)
	    in  (m,subst)
	    end

	fun lvs_subst (lvs : lvs) : subst = #2 lvs

	fun lvs_mod (lvs : lvs) : mod = #1 lvs

	(* Recursively eliminates non-binding occurrences of local
	   variables in an sdecs, replacing them with paths.  This may
	   be a bad idea but it is currently assumed by other parts of
	   the elaborator.  *)
    
	fun eliminate (lvs, s : signat) : signat =
	    (case s
	       of SIGNAT_STRUCTURE sdecs =>
		   let val (sdecs,_) = foldl_acc eliminate' lvs sdecs
		   in  SIGNAT_STRUCTURE sdecs
		   end
		| _ => sig_subst(s,lvs_subst lvs))

	and eliminate' (sdec, lvs) : sdec * lvs =
	    let val SDEC (l,dec) = sdec
		val subst = lvs_subst lvs
		val EXP = fn e => exp_subst(e,subst)
		val CON = fn c => con_subst(c,subst)
		val OPT = Option.map
		val dec = (case dec
			     of DEC_EXP (v,c,eopt,inline) => DEC_EXP (v,CON c,OPT EXP eopt,inline)
			      | DEC_CON (v,k,copt,inline) => DEC_CON (v,k,OPT CON copt,inline)
			      | DEC_MOD (v,poly,s) =>
				 let val s = eliminate (lvs_recurse (lvs,l,s), s)
				 in  DEC_MOD (v,poly,s)
				 end)
	    in  (SDEC (l,dec), lvs_extend (lvs,sdec))
	    end

	(* Recursively apply the HS self rules.
	   If decs |- m : s and s' = selfify(m,s),
           then decs |- m : s' and s' is transparant.
	*)
    
	fun selfify (m : mod, s : signat) : signat =
	    (case s
	       of SIGNAT_STRUCTURE sdecs => SIGNAT_STRUCTURE (map (selfify' m) sdecs)
		| _ => s)
		 
	and selfify' (m : mod) (sdec : sdec) : sdec =
	    let val SDEC (l,dec) = sdec
		val dec = (case dec
			     of DEC_EXP _ => dec
			      | DEC_CON (v,k,copt,inline) =>
				 let val copt = (case copt
						   of NONE => SOME (CON_MODULE_PROJECT (m,l))
						    | SOME _ => copt)
				 in  DEC_CON(v,k,copt,inline)
				 end
			      | DEC_MOD (v,poly,s) =>
				 let val s = selfify (MOD_PROJECT(m,l), s)
				 in  DEC_MOD(v,poly,s)
				 end)
	    in  SDEC(l,dec)
	    end

	(*
           Reduce_signat reduces an arbitrary structure signature to
           an sdecs.  It uses the following helpers.

	   Reduce_sigvar reduces a structure signature of the form
	   SIGNAT_VAR(v) to an sdecs by looking up the signature v.
		 
	   Reduce_sigof reduces a structure signature of the form
	   SIGNAT_OF(X) to an sdecs by looking up and selfifying the
	   signature of X.

	   Reduce_sigof_fast reduces a structure signature of the form
	   SIGNAT_OF(v.labs) to an sdecs when v is in the context's
	   memo table.  That table stores deeply reduced, selfified
	   signatures that have no occurrences of local variables.
	   From such a signature, we simply have to project out the
	   right component.
        *)

	fun lookup (arg : context * var) : phrase_class =
	    (case Context_Lookup_Var_Raw arg
	       of SOME(_,pc) => pc
		| NONE => error ("unbound module variable " ^ var2string (#2 arg)))
	
	fun reduce_sigof_fast (ctxt : context, v : var, labs : labels) : signat option =
	    let val CONTEXT {memo, ...} = ctxt
		fun project (l : label, s : signat) : signat =
		    let val sdecs = (case s
				       of SIGNAT_STRUCTURE sdecs => sdecs
					| _ => error "reduce_sigof_fast projecting from non-structure component")
			val sdecopt = find_sdec (sdecs,l)
		    in  (case sdecopt
			   of SOME (SDEC(_,DEC_MOD(_,_,s))) => s
			    | SOME _ => error "projecting from non-module component in reduce_sigof_fast"
			    | NONE => error "cannot find label in reduce_sigof_fast")
		    end
	    in  (case VarMap.find(!memo, v)
		   of NONE => NONE
		    | SOME signat => SOME (foldl project signat labs))
	    end

	fun reduce_signat (ctxt : context, s : signat) : signat =
	    (case s
	       of SIGNAT_VAR v => reduce_sigvar (ctxt,v)
		| SIGNAT_OF (PATH(v,labs)) => (case reduce_sigof_fast (ctxt, v, labs)
						 of SOME s' => s'
						  | NONE => reduce_sigof (ctxt,v,labs))
		| _ => s)

	and reduce_sigvar (ctxt : context, v : var) : signat =
	    let val s = (case lookup (ctxt, v)
			   of PHRASE_CLASS_SIG (_,s) => s
			    | _ => error "SIGNAT_VAR with non-signature var")
	    in  reduce_signat (ctxt, s)
	    end

	and reduce_sigof (ctxt : context, v : var, labs : labels) : signat =
	    let val signat = (case lookup (ctxt,v)
				of PHRASE_CLASS_MOD (_,_,s) => s
				 | _ => error "SIGNAT_OF projecting from non-module")
		val signat = selfify (MOD_VAR v, signat)
		    
		(* We have to continue selfifying after reduction.
		   For SIGNAT_OF, this is taken care of by the
		   recursive call. *)
		fun reduce_and_selfify (lvs, s) =
		    let val s' = reduce_signat (ctxt, s)
			val selfified = (case s
					   of SIGNAT_VAR _ => false
					    | _ => true)
		    in  if selfified then s'
			else selfify (lvs_mod lvs, s')
		    end
		fun project (l : label, (lvs, s : signat)) : lvs * signat =
		    let val s = reduce_and_selfify (lvs,s)
			val sdecs = (case s
				       of SIGNAT_STRUCTURE sdecs => sdecs
					| _ => error "SIGNAT_OF projecting from non-structure component")
			fun find (nil,_) = error "cannot find label in SIGNAT_OF"
			  | find ((sdec as SDEC(l',dec)) :: rest,lvs) =
			    if eq_label (l,l') then
				let val s = (case dec
					       of DEC_MOD(_,_,s) => s
						| _ => error "SIGNAT_OF projecting from non-module component")
				    val s = lvs_rewrite (lvs, s)
				    val lvs = lvs_recurse (lvs,l,s)
				in  (lvs,s)
				end
			    else find (rest, lvs_extend (lvs,sdec))
		    in  find (sdecs,lvs)
		    end
		val lvs = lvs (MOD_VAR v)
		val (lvs,s) = foldl project (lvs,signat) labs
		val s = sig_subst (s, lvs_subst lvs)
	    in  reduce_and_selfify (lvs, s)
	    end

	(* Deep_reduce applies reduce_signat recursively so that any
           substructure signatures are also sdecs.  *)
	    
	fun deep_reduce (ctxt : context, lvs, s : signat) : signat =
	    (case reduce_signat (ctxt,s)
	       of SIGNAT_STRUCTURE sdecs =>
		   let val (sdecs,_) = foldl_acc (deep_reduce' ctxt) lvs sdecs
		   in  SIGNAT_STRUCTURE sdecs
		   end
		| s => s)

	and deep_reduce' (ctxt : context) (sdec, lvs) : sdec * lvs =
	    (case sdec
	       of SDEC (l,DEC_MOD(v,poly,s)) =>
		   let val s = lvs_rewrite (lvs, s)
		       val s = deep_reduce (ctxt, lvs_recurse (lvs,l,s), s)
		       val lvs = lvs_extend (lvs,sdec)
		       val sdec = SDEC (l,DEC_MOD (v,poly,s))
		   in  (sdec,lvs)
		   end
		| _ => (sdec,lvs))
		 
    in
	fun transform (ctxt : context, v : var, s : signat) : signat =
	    let val CONTEXT {memo, ...} = ctxt
	    in
		(case VarMap.find (!memo,v)
		   of SOME s' => s'
		    | NONE =>
		       let val m = MOD_VAR v
			   val lvs = lvs m
			   val s' = deep_reduce (ctxt,lvs,s)
			   val s' = eliminate (lvs,s')
			   val s' = selfify (m,s')
			   val _ = memo := VarMap.insert (!memo,v,s')
(*
			   val _ = debugdo(fn () =>
					   (print "\n\ntransformed "; pp_mod m;
					    print " : "; pp_signat s; print "\n";
					    print "to : "; pp_signat s'; print "\n\n"))
*)
		       in  s'
		       end)
	    end
	    
	val deep_reduce : context * mod * signat -> signat =
	    (fn (ctxt,m,s) => deep_reduce (ctxt, lvs m, s))
    end

    (* ----------- context extenders ----------------------------  *)
    val empty_context = CONTEXT{varMap = VarMap.empty,
				ordering = nil,
				labelMap = LabelMap.empty,
				memo = ref VarMap.empty,
				fixityMap = LabelMap.empty,
				overloadMap = LabelMap.empty}

    type varMap = (label * phrase_class) VarMap.map
    type labelMap = vpath LabelMap.map
    type overloadMap = ovld LabelMap.map
	
    (* Prepare for the possible shadowing of a label. *)
    fun shadow (vm : varMap, lm : labelMap, om : overloadMap, l : label)
	: varMap * labelMap * overloadMap =
	(case (LabelMap.find(om,l), LabelMap.find(lm,l))
	   of (SOME _, NONE) =>
	       let val (om,_) = LabelMap.remove(om,l)
	       in  (vm,lm,om)
	       end
	    | (NONE, SOME vpath) =>
	       let val (lm,_) = LabelMap.remove(lm,l)
		   val (vm,lm) =
		       (case vpath
			  of (v,nil) =>
			      let val newlab = fresh_internal_label(label2name' l)
				  val SOME (_,pc) = VarMap.find (vm,v)
				  val vm = VarMap.insert (vm,v,(newlab,pc))
				  val lm = LabelMap.insert (lm,newlab,vpath)
			      in  (vm,lm)
			      end
			   | _ => (vm,lm))
	       in  (vm,lm,om)
	       end
	    | (NONE, NONE) => (vm,lm,om)
	    | (SOME _, SOME _) => error "overloadMap and labelMap labels not disjoint")

    local
	fun print_binding (l, v, pc) = (pp_label l; print " > "; pp_var v;
					print " : "; pp_phrase_class pc)

	(* Check that the free variables of pc are all bound in ctxt. *)
	fun check_free (ctxt : context, l : label, v : var, pc : phrase_class) : unit =
	    let val CONTEXT {varMap, ...} = ctxt
		fun bound v' = isSome (VarMap.find (varMap, v'))
		fun ck v' = if bound v' then ()
			    else (debugdo (fn () =>
					   (print "unbound variable "; pp_var v';
					    print " detected while adding:\n";
					    print_binding (l,v,pc); print "\n"));
				  error "unbound variable detected")
	    in  VarSet.app ck (classifier_free pc)
	    end
	
	(* Check that v is not bound in context. *)
	fun check_shadow (ctxt : context, l : label, v : var, pc : phrase_class) : unit =
	    let val CONTEXT {varMap, ...} = ctxt
	    in  (case VarMap.find (varMap,v)
		   of NONE => ()
		    | SOME (l',pc') =>
		       (debugdo (fn () =>
				 (print "shadowing "; print_binding (l',v,pc');
				  print "\nwith "; print_binding (l,v,pc);
				  print "\n"));
			error "variable shadowing"))
	    end
	
    in
	(* Extend context with l > v:pc.  Ignores starred structures. *)
	fun add' (arg : context * label * var * phrase_class) : context =
	    let val _ = sanitycheck(fn () =>
				    (check_free arg;
				     check_shadow arg))
		val (ctxt,l,v,pc) = arg
		val CONTEXT {varMap, ordering, labelMap, memo,
			     fixityMap, overloadMap} = ctxt
		val (varMap,labelMap,overloadMap) = shadow (varMap,labelMap,overloadMap,l)
		val varMap = VarMap.insert (varMap,v,(l,pc))
		val ordering = v :: ordering
		val labelMap = LabelMap.insert (labelMap,l,(v,nil))
(*
		val _ = debugdo(fn () =>
				(print "\n\nextending context with ";
				 print_binding (l,v,pc); print "\n\n"))
*)
	    in  CONTEXT{varMap = varMap,
			ordering = ordering,
			labelMap = labelMap,
			memo = ref (!memo),
			fixityMap = fixityMap,
			overloadMap = overloadMap}
	    end
    end

    local
	(* Extend labelMap with top-level labels visible by the star
	   convention. *)
	fun add_labels (v : var, labs : labels, s : signat, labelMap) : labelMap =
	    (case s
	       of SIGNAT_STRUCTURE sdecs => foldl (add_labels' (v,labs)) labelMap sdecs
		| _ => labelMap)
	     
	and add_labels' (v : var, labs : labels) (sdec, labelMap) : labelMap =
	    let val SDEC(l,dec) = sdec
		val labs = labs @ [l]
		val labelMap = (if !NoOpenInternalPaths andalso is_label_internal l
				    then labelMap
				else LabelMap.insert (labelMap,l,(v,labs)))
	    in
		(case (is_open l,dec)
		   of (true,DEC_MOD (_,_,s)) => add_labels (v,labs,s,labelMap)
		    | _ => labelMap)
	    end
    in
	
	(* Extend context with l > v:pc.  When a structure with a
	   starred label is added, its componenets are added to the
	   labelMap.  *)
	
	fun add (ctxt : context, l : label, v : var, pc : phrase_class) : context =
	    let val ctxt = add' (ctxt,l,v,pc)
	    in  (case (is_open l, pc)
		  of (true,PHRASE_CLASS_MOD(m,_,s)) =>
		      let val CONTEXT {varMap, ordering, labelMap, memo,
				       fixityMap, overloadMap} = ctxt
			  val labelMap = add_labels (v,nil,deep_reduce (ctxt,m,s),labelMap)
		      in  CONTEXT{varMap = varMap,
				  ordering = ordering,
				  labelMap = labelMap,
				  memo = ref (!memo),
				  fixityMap = fixityMap,
				  overloadMap = overloadMap}
		      end
		   | _ => ctxt)
	    end
    end

    (* Add a top-level label that maps to the given bound variable.
       Ignores starred structures.  *)
   
    fun add_context_label (context, label, var) : context =
	let val CONTEXT {varMap, ordering, labelMap, memo,
			 fixityMap, overloadMap} = context
	    val _ = sanitycheck(fn () =>
				(case VarMap.find (varMap, var)
				   of SOME _ => ()
				    | NONE =>
				       (debugdo (fn () =>
						 (print "unbound variable "; pp_var var;
						  print " detected while adding label ";
						  pp_label label; print "\n"));
					error "unbound variable in add_context_label")))
	    val (varMap,labelMap,overloadMap) = shadow (varMap,labelMap,overloadMap,label)
	    val labelMap = LabelMap.insert (labelMap,label,(var,nil))
	in  CONTEXT{varMap = varMap,
		    ordering = ordering,
		    labelMap = labelMap,
		    memo = ref (!memo),
		    fixityMap = fixityMap,
		    overloadMap = overloadMap}
	end
	
    fun add_context_sdec (ctxt : context, sdec) : context =
	let val SDEC (l,dec) = sdec
	    val (v,pc) = (case dec
			    of DEC_EXP (v,c,eopt,inline) => (v,PHRASE_CLASS_EXP (VAR v,c,eopt,inline))
			     | DEC_CON (v,k,copt,inline) => (v,PHRASE_CLASS_CON (CON_VAR v,k,copt,inline))
			     | DEC_MOD (v,poly,s) => (v,PHRASE_CLASS_MOD (MOD_VAR v,poly,s)))
	in  add (ctxt,l,v,pc)
	end
    
    fun add_context_exp (ctxt,l,v,c) = add_context_sdec (ctxt,SDEC(l,DEC_EXP(v,c,NONE,false)))
    fun add_context_con (ctxt,l,v,k,copt) = add_context_sdec (ctxt,SDEC(l,DEC_CON(v,k,copt,false)))
    fun add_context_mod (ctxt,l,v,s) = add_context_sdec (ctxt,SDEC(l,DEC_MOD(v,false,s)))
	
    fun add_context_sdec' (sdec, ctxt) = add_context_sdec (ctxt, sdec)
    fun add_context_sdecs (ctxt, sdecs) = foldl add_context_sdec' ctxt sdecs
	
    fun anon_label () = fresh_internal_label "anon"
    fun dec2sdec dec = SDEC(anon_label(), dec)
	
    fun add_context_exp' (ctxt,v,c) = add_context_exp (ctxt,anon_label(),v,c)
    fun add_context_con' (ctxt,v,k,copt) = add_context_con (ctxt,anon_label(),v,k,copt)
    fun add_context_mod' (ctxt,v,s) = add_context_mod (ctxt,anon_label(),v,s)
    fun add_context_dec (ctxt,dec) = add_context_sdec (ctxt,dec2sdec dec)
    fun add_context_decs (ctxt,decs) = add_context_sdecs (ctxt,map dec2sdec decs)
	
    fun add_context_sig (ctxt,l,v,s) = add (ctxt,l,v,PHRASE_CLASS_SIG (v,s))
	
    fun add_context_fixity (ctxt : context, l : label, fixity) : context =
	let val CONTEXT {varMap, ordering, labelMap, memo,
			 fixityMap, overloadMap} = ctxt
	    val fixityMap = LabelMap.insert (fixityMap,l,fixity)
	in  CONTEXT {varMap = varMap,
		     ordering = ordering,
		     labelMap = labelMap,
		     memo = ref (!memo),
		     fixityMap = fixityMap,
		     overloadMap = overloadMap}
	end
    
    fun add_context_overexp (ctxt : context, l : label, ovld2 : ovld) : context =
	let val CONTEXT {varMap, ordering, labelMap, memo,
			 fixityMap, overloadMap} = ctxt
	    val ovld1 = (case Name.LabelMap.find(overloadMap, l) of
			     SOME ovld1 => ovld1
			   | NONE => OVLD ([], NONE))
	    val ovld3 = ovld_add(l,ctxt,ovld1,ovld2)
	    val (varMap,labelMap,overloadMap) = shadow (varMap,labelMap,overloadMap,l)
	    val overloadMap = LabelMap.insert(overloadMap, l, ovld3)
	in  CONTEXT {varMap = varMap,
		     ordering = ordering,
		     labelMap = labelMap,
		     memo = ref (!memo),
		     fixityMap = fixityMap,
		     overloadMap = overloadMap}
	end
    
    fun add_context_entry (ctxt : context, entry : context_entry) : context = 
	(case entry
	   of CONTEXT_SDEC sdec => add_context_sdec (ctxt,sdec)
	    | CONTEXT_SIGNAT (l,v,s) => add_context_sig (ctxt,l,v,s)
	    | CONTEXT_FIXITY (l,f) => add_context_fixity (ctxt,l,f)
	    | CONTEXT_OVEREXP (l,ovld) => add_context_overexp (ctxt,l,ovld))
	     
    fun add_context_entry' (entry,ctxt) = add_context_entry (ctxt,entry)
    fun add_context_entries (ctxt, entries) = foldl add_context_entry' ctxt entries
	
    (* ----------- context access ----------------------------  *)

    local
	fun varEntry (varMap) (v : var) : context_entry =
	    let val SOME (l,pc) = VarMap.find (varMap,v)
		val sdec = fn dec => CONTEXT_SDEC (SDEC(l,dec))
	    in
		(case pc
		   of PHRASE_CLASS_EXP (_,c,eopt,inline) => sdec (DEC_EXP(v,c,eopt,inline))
		    | PHRASE_CLASS_CON (_,k,copt,inline) => sdec (DEC_CON(v,k,copt,inline))
		    | PHRASE_CLASS_MOD (_,poly,s) => sdec (DEC_MOD(v,poly,s))
		    | PHRASE_CLASS_SIG (_,s) => CONTEXT_SIGNAT (l,v,s))
	    end
	     
	fun varEntries (varMap, ordering) : context_entry list =
	    map (varEntry varMap) (rev ordering)

	fun overloadEntries overloadMap : context_entry list =
	    map CONTEXT_OVEREXP (LabelMap.listItemsi overloadMap)

	fun fixityEntries fixityMap : context_entry list =
	    map CONTEXT_FIXITY (LabelMap.listItemsi fixityMap)
    in
	fun list_entries (ctxt : context) : context_entry list =
	    let val CONTEXT {varMap,ordering,fixityMap,overloadMap,...} = ctxt
	    in  List.concat [fixityEntries fixityMap,
			     varEntries (varMap,ordering),
			     overloadEntries overloadMap]
	    end
    end

    type fixityMap = fixity LabelMap.map
    fun Context_Fixity (ctxt : context) : fixityMap =
	let val CONTEXT {fixityMap,...} = ctxt
	in  fixityMap
	end

    fun Context_Lookup_Var (ctxt : context, v : var) : (label * phrase_class) option =
	(case Context_Lookup_Var_Raw (ctxt,v)
	   of SOME (l,PHRASE_CLASS_MOD(m,poly,s)) =>
	       SOME (l,PHRASE_CLASS_MOD(m,poly,transform(ctxt,v,s)))
	    | answer => answer)
	     
    fun Context_Lookup_Label (ctxt : context, l : label) : path option =
	let val CONTEXT {labelMap, ...} = ctxt
	in  Option.map PATH (LabelMap.find (labelMap, l))
	end

    fun Context_Lookup_Overload (ctxt : context, l : label) : ovld option =
	let val CONTEXT {overloadMap, ...} = ctxt
	in  LabelMap.find (overloadMap, l)
	end

    (* XXX: This will go away soon.  It is only needed by IlContextEq.  *)
    fun Context_Ordering (ctxt : context) : var list =
	let val CONTEXT {ordering, ...} = ctxt
	in  rev ordering
	end
	
    fun con_exp_subst subst (c,e) = (con_subst(c,subst), exp_subst(e,subst))
    fun ovld_subst subst (OVLD (celist, default)) = OVLD (map (con_exp_subst subst) celist, default)

    (* alphaVarying a partial context WRT a context so that
       (1) the partial context's unresolved list maps to variables as bound by the full context
       (2) the partial context does not bind variables (at top-level) 
           that are bound (at top-level) of the full context
       If either condition is not already true, a substitution is produced and
          the substitution is applied to partial context.
    *)
    fun alphaVaryPartialContext (ctxt1 : context, (ctxt2,unresolved) : partial_context) : partial_context option =
	let
	    val CONTEXT {varMap = vm1, labelMap = lm1, ...} = ctxt1
	    val CONTEXT {varMap = vm2, ordering = ord2, labelMap = lm2,
			 fixityMap = fm2, overloadMap = om2, ...} = ctxt2
            (* ---- Using the current unresolved of the partial context and the context,
	       ---- compute the substitution of the unresolved variables on the partial context
	       ---- to match the corresponding variables of the context and 
	       ---- generate a new unresolved list. *)
	    fun unresolvedFolder (v,l,(unresolved,vsubst)) =
		(case Name.LabelMap.find(lm1,l) of
		     SOME (v',[]) =>
			 (VarMap.insert(unresolved, v', l),
			 if (eq_var(v,v'))
			     then vsubst
			 else Name.VarMap.insert(vsubst,v,v'))
		    | _ => error ("add_context could not resolve " ^ (Name.label2name l)))
	    val (unresolved,unresolvedVsubst) = Name.VarMap.foldli unresolvedFolder
		                                (VarMap.empty, VarMap.empty) unresolved
            (* ---- Compute the substitution to avoid clashes of bound variables ------- *)
	    fun clashFolder (v, (l, pc), vsubst) = 
		(case VarMap.find(vm1, v) of
		     NONE => vsubst
		   | SOME _ => let val v' = fresh_named_var "clash" (* derived_var v *)
			       in  VarMap.insert(vsubst,v,v')
			       end)
	    val clashVsubst = Name.VarMap.foldli clashFolder VarMap.empty vm2
	    (* ----- Compute the final substitution by combining unresolved and clash ---- *)
	    local
		fun folder(v,v',subst) = 
		    if (eq_var (v,v'))
			then subst
		    else let val subst = subst_add_expvar(subst,v,VAR v')
			     val subst = subst_add_convar(subst,v,CON_VAR v')
			     val subst = subst_add_modvar(subst,v,MOD_VAR v')
			     val subst = subst_add_sigvar(subst,v,v')
			 in  subst
			 end
	    in  val final_vsubst = Name.VarMap.unionWithi (fn (v,_,_) => error "same var in vsubst")
		                   (unresolvedVsubst, clashVsubst)
		val final_subst = Name.VarMap.foldli folder empty_subst final_vsubst
	    end
	in  if (subst_is_empty final_subst)
		then NONE
	    else 
		let val vsubst = fn v => (case VarMap.find(final_vsubst,v)
					    of NONE => v
					     | SOME v' => v')
		    val pcsubst = fn pc => pc_subst(pc,final_subst)
		    val varMap = (VarMap.foldli
				  (fn (v,(lab,pc),vm) =>
				   let val v = vsubst v
				       val pc = pcsubst pc
				       val vm = VarMap.insert (vm,v,(lab,pc))
				   in  vm
				   end) VarMap.empty vm2)
		    val ordering = map vsubst ord2
		    val labelMap = LabelMap.map (fn (v,labs) => (vsubst v, labs)) lm2
		    val overloadMap = LabelMap.map (ovld_subst final_subst) om2
		in  SOME (CONTEXT{varMap = varMap,
				  ordering = ordering,
				  labelMap = labelMap,
				  memo = ref VarMap.empty,
				  fixityMap = fm2,
				  overloadMap = overloadMap},
			  unresolved)
		end 
	end

    fun pcAddFree (pc : phrase_class, set : VarSet.set) : VarSet.set =
	VarSet.union (set, pc_free pc)

    (* is_open_internal_path checks if a path consists of a sequence of open labels ending with
       an internal label. *)
    fun is_open_internal_path (_,(v,[])) = false
      | is_open_internal_path (varmap,(v,labs)) = 
	(case labs of
	     [lab] => (case VarMap.find(varmap,v) of
			   NONE => (print "Could not find "; pp_var v; print "\n";
				    error "is_open_internal_path failed")
			 | SOME (l,_) => is_open l andalso is_label_internal lab)
	   | _ => let val len = length labs
		  in is_open (List.nth(labs, len - 2)) andalso is_label_internal (List.nth(labs, len - 1))
		  end)

    (* adding the partial context to the context *)
    fun plus_context (ctxt1 : context, orig_pctxt : partial_context) : partial_context option * context = 
	let
(*
	    val _ = debugdo (fn () => (print "plus PARTIAL CONTEXT:\n";
				       pp_pcontext orig_pctxt;
				       print "\n\n"))
*)
	    val CONTEXT {varMap = vm1, ordering = ord1, labelMap = lm1,
			 memo = ref memo1, fixityMap = fm1, overloadMap = om1} = ctxt1
	    val pctxt_option = alphaVaryPartialContext(ctxt1, orig_pctxt)
	    val (ctxt2, unresolved) = (case pctxt_option
					 of NONE => orig_pctxt (* alpha-varying not needed *)
					  | SOME pctxt => pctxt)
	    val CONTEXT {varMap = vm2, ordering = ord2, labelMap = lm2,
			 fixityMap = fm2, overloadMap = om2, ...} = ctxt2
	    val fixityMap = LabelMap.unionWithi
		            (fn (l, _, _) => error ("fixityMap not disjoint at " ^ (label2name l)))
			    (fm1,fm2)
            (* Combine the labelMaps from both contexts.
               For any labels in the domain of both labelMaps, check that at least one corresponds to an
               open_internal_path.  Why is this the right thing?
             *)
	    val labelMap = LabelMap.unionWithi 
		            (fn (l,p1,p2) =>
			     if not (!NoOpenInternalPaths) andalso
				 (is_open_internal_path(vm1,p1) orelse
				  is_open_internal_path(vm2,p2))
				 then p2
			     else (print "p1 = "; pp_path (PATH p1); print "\n";
				   print "p2 = "; pp_path (PATH p2); print "\n";
				   error ("labelMap not disjoint at " ^ (label2name l))))
			     (lm1,lm2)
	    val varMap = VarMap.unionWithi
		            (fn (v,(l,_),(l',_)) => (print "varMap not disjoint at label ";
						     Ppil.pp_label l;
						     print " and label' ";
						     Ppil.pp_label l';
						     print " and var ";
						     Ppil.pp_var v;
						     print "\n";
						     error "varMap not disjoint"))
			     (vm1,vm2)
	    (* Ignore (empty) table in the partial context. *)
	    val memoMap = memo1
		
	    (* orderings are backwards *)
	    val ordering = ord2 @ ord1
	    val almostFinalContext = CONTEXT{varMap = varMap,
					     ordering = ordering,
					     labelMap = labelMap,
					     memo = ref memoMap,
					     fixityMap = fixityMap,
					     overloadMap = Name.LabelMap.empty}
	    val overloadMap = LabelMap.unionWithi
                     	      (fn (l, ovld1, ovld2) =>
			       (ovld_add(l,almostFinalContext,ovld1,ovld2)))
			      (om1,om2)
	in  (pctxt_option,
	     CONTEXT{varMap = varMap,
		     ordering = ordering,
		     labelMap = labelMap,
		     memo = ref memoMap,
		     fixityMap = fixityMap,
		     overloadMap = overloadMap})
	end 

    fun reachableVars(varMap, black, gray) = 
	if (VarSet.isEmpty gray)
	    then black
	else 
	    let 
		val black = VarSet.union(black, gray)
		fun folder (v,s) = 
		    (case VarMap.find(varMap,v) of
			 SOME (_, pc) => pcAddFree(pc,s)
		       | _ => s)
(* error ("reachableVars could not find free variable " ^ (Name.var2string v)) *)
		val freeVars = VarSet.foldl folder VarSet.empty gray  
		fun pred v = (case VarMap.find(varMap,v) of
				  SOME _ => true
				| _ => false)
		val temp = VarSet.filter pred freeVars         (* one step from old gray *)
		val gray = VarSet.difference(temp, black)      (* there might be loops *)
	    in  reachableVars(varMap,black,gray)
	    end

    fun sub_context (bigger : Il.context, smaller : Il.context) : Il.partial_context = 
	let 
	    val CONTEXT{varMap=v1, ordering=o1, labelMap=l1, fixityMap=f1, overloadMap=om1, ...} = bigger
	    val CONTEXT{varMap=v2, ordering=o2, labelMap=l2, fixityMap=f2, overloadMap=om2, ...} = smaller
	    val v3 = VarMap.filteri (fn (v,_) => 
				     (case VarMap.find(v2,v)
					of NONE => true
					 | SOME _ => false)) v1
	    val o3 = List.filter (fn v => (case VarMap.find(v2,v)
					     of NONE => true
					      | SOME _ => false)) o1
	    val l3 = LabelMap.filteri (fn (l,_) => 
				       (case LabelMap.find(l2,l)
					  of NONE => true
					   | SOME _ => false)) l1
	    val f3 = LabelMap.filteri (fn (l1,_) => (case LabelMap.find(f2,l1)
						       of NONE => true
							| SOME _ => false)) f1
	    val om3 = LabelMap.mapi (fn (l1, ovld1) => (case LabelMap.find(om2, l1)
							  of NONE => ovld1
							   | SOME ovld2 => ovld_sub(bigger,ovld1,ovld2))) om1
	    val om3 = LabelMap.filter (fn (OVLD ([],_)) => false
	                                | _ => true) om3
	    (* There is no reason to retain the memo table; it should not
	       be written to disk. *)
	    val diff = CONTEXT {varMap = v3, ordering = o3, labelMap = l3,
				fixityMap = f3, overloadMap = om3, memo = ref VarMap.empty}
	    val roots = (VarMap.foldli 
			 (fn (v,_,s) => VarSet.add(s,v))
			 VarSet.empty v3)
	    val reachable = LabelMap.foldl
		           (fn (OVLD (celist,_),reach) =>
			      foldl (fn ((c,e),reach) => 
				     let val cFree = con_free c
					 val eFree = exp_free e
				     in  VarSet.union(VarSet.union(reach,cFree),eFree)
				     end) reach celist)
			   VarSet.empty om3
	    val reachable = reachableVars(v1, reachable, roots)
	    val keepVars = VarSet.difference(reachable, roots)
	    val unresolved = VarSet.foldl
		                  (fn (v,vm) => 
				   (case VarMap.find(v1,v)
				      of SOME (l, _) => VarMap.insert(vm,v,l)
				       | NONE => error "Missing variable in sub_context"))
				  Name.VarMap.empty keepVars
	in  (diff, unresolved)
	end

    fun gc_context (module : Il.module) : Il.context = 
	let val (ctxt1, (ctxt2,_), sbndopt_entry) = module
	    val CONTEXT {varMap, ...} = ctxt2
	    val exclude = VarMap.foldli
		          (fn (v,_,s) => VarSet.add(s,v))
		          VarSet.empty varMap
	    val roots = VarMap.foldli
		          (fn (v,(_,pc),s) => pcAddFree(pc,s))
		          VarSet.empty varMap
	    fun folder ((sbndopt,entry),roots) = 
		let val roots = (case sbndopt of
				     NONE => roots
				   | SOME sbnd => VarSet.union(roots, sbnd_free sbnd))
		in  VarSet.union(roots, entry_free entry)
		end
	    val roots = foldl folder roots sbndopt_entry
	    val roots = VarSet.difference(roots, exclude)
	    val CONTEXT{varMap, ordering, labelMap, overloadMap, fixityMap, ...} = ctxt1
	    val roots = VarMap.foldli
		          (fn (v,(l,_),s) => if Name.keep_import l
						 then VarSet.add(s,v)
					     else s)
			  roots varMap
	    val reachable = reachableVars(varMap, VarSet.empty, roots)
	    fun isReachable v = Name.VarSet.member(reachable,v)
	    val varMap_orig = varMap (* used for the print statements below *)
	    val varMap = VarMap.filteri (fn (v,_) => isReachable v) varMap_orig
	    val labelMap = LabelMap.filteri (fn (_,(v,_)) => isReachable v) labelMap
	    val ordering = List.filter isReachable ordering
	    val _ = print ("gc_context: " ^
			   Int.toString(VarMap.numItems varMap_orig) ^
			   " items in original context.  " ^ 
			   (Int.toString (VarMap.numItems varMap))
			   ^ " items in reduced context.\n")
	in  CONTEXT{varMap = varMap,
		    ordering = ordering,
		    labelMap = labelMap,
		    memo = ref VarMap.empty, (* laziness *)
		    fixityMap = fixityMap,		    
		    overloadMap = overloadMap}
	end

    (* Invariant: If f : label_info then f is 1-1. *)
    type label_info = label Name.LabelMap.map

    fun inverse (li : label_info) : label_info =
	let fun folder (l, l', li') = Name.LabelMap.insert (li', l', l)
	in  Name.LabelMap.foldli folder Name.LabelMap.empty li
	end
    
    val empty_label_info : label_info = Name.LabelMap.empty
	
    fun get_labels ((CONTEXT{fixityMap, overloadMap, labelMap, ...}, _), li) =
	let
	    fun addLabel (li', l) =
		case Name.LabelMap.find (li', l)
		  of NONE => Name.LabelMap.insert (li', l, fresh_internal_label "")
		   | SOME _ => li'
	    fun addDomain (map,acc) = Name.LabelMap.foldli (fn (l, _, acc') => addLabel (acc', l)) acc map
	    val li = addDomain (fixityMap, li)
	    val li = addDomain (overloadMap, li)
	    val li = addDomain (labelMap, li)
	in  li
	end
    
    fun map_labels (li : label_info) (l : label) : label =
	let val l' = (case Name.LabelMap.find (li, l)
			of NONE => l
			 | SOME l' => l')
(*
	    val _ = debugdo (fn () =>
			     (print "map_labels: "; pp_label l;
			      print " -> "; pp_label l'; print "\n"))
*)
	in  l'
	end

    fun map_context_labels (CONTEXT{varMap, ordering, labelMap, memo=ref memoMap, fixityMap, overloadMap}, li) =
	let val lookup = map_labels li
	    fun folder (l, v, map) = Name.LabelMap.insert (map, lookup l, v)
	    fun changeDomain map = Name.LabelMap.foldli folder Name.LabelMap.empty map
	    val varMap = Name.VarMap.map (fn (l,pc) => (lookup l, pc)) varMap
	    val overloadMap = changeDomain overloadMap
	    val labelMap = changeDomain labelMap
	    val fixityMap = changeDomain fixityMap
	in  CONTEXT{varMap = varMap,
		    ordering = ordering,
		    labelMap = labelMap,
		    memo = ref memoMap,
		    fixityMap = fixityMap,
		    overloadMap = overloadMap}
	end
    fun map_partial_context_labels ((context, frees), li) =
	let val context = map_context_labels (context, li)
	    val frees = Name.VarMap.map (map_labels li) frees
	in  (context, frees)
	end

    fun obscure_labels (ctxt, li) = map_context_labels (ctxt, li)

    fun unobscure_labels ((ctxt, partial_ctxt, binds), li) =
	let val li' = inverse li
	in  (map_context_labels (ctxt, li'),
	     map_partial_context_labels (partial_ctxt, li'),
	     binds)
	end

    fun list_labels (CONTEXT{varMap, labelMap, ...}) : label list =
	let
	    (* We do not have to add the domain of overloadMap because
	       the elaborator resolves overloading. *)
	    fun addPath (l : label, p : vpath, acc) =
		if not (!NoOpenInternalPaths) andalso is_open_internal_path(varMap,p)
		    then acc
		else
		    let val (c,r) = Name.make_cr_labels l
		    in  l :: c :: r :: acc
		    end
	in  Name.LabelMap.foldli addPath nil labelMap
	end
    
    fun removeNonExport (CONTEXT{varMap, overloadMap, labelMap, ordering, fixityMap, ...}, frees) = 
	let val ordering = List.filter (fn v => let val SOME(l,_) = Name.VarMap.find(varMap,v)
						in  not (is_nonexport l)
						end) ordering
	    val varMap = Name.VarMap.filteri (fn (_,(l,_)) => not(is_nonexport l)) varMap
	    val labelMap = Name.LabelMap.filteri (fn (l,_) => not(is_nonexport l)) labelMap
	    val frees = Name.VarMap.filteri (fn (_, l) => not(is_nonexport l)) frees
	in  (CONTEXT{varMap = varMap,
		     ordering = ordering,
		     labelMap = labelMap,
		     memo = ref VarMap.empty, (* laziness *)
		     fixityMap = fixityMap,
		     overloadMap = overloadMap},
	     frees)
	end
end
