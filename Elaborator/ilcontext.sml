(*
	When a structure variable is looked up, its signature is
	transformed and memoized.  The transformations are (deep)
	reduction, local variable elimination, and selfification.
	These transformations do not touch functor signatures.

	Reduce reduces an arbitrary structure signature to an sdecs.
	Reduce_sigvar reduces a signature of the form SIGNAT_VAR v to
	an sdecs (which may contain signature variables) by looking up
	v and recursing on the definition.  Reduce_sigof reduces a
	signature of the form SIGNAT_OF(X) to an sdecs by looking up
	and selfifying the signature of X.  Deep_reduce applies reduce
	recursively so that any sub-structure signatures are also
	sdecs.

	Eliminate recursively eliminates non-binding occurrences of
	local variables in an sdecs, replacing them with paths.  This
	may be a bad idea but it is currently assumed by other parts
	of the elaborator.

	Selfify recursively applies the HS self rules.

	These transformations use local variable substitutions (lvs)
	to account for local variables; these are substitutions from
	local variables to bound paths.

	Several of these functions return a boolean to indicate
	whether the result comes from the memo table.  This enables
	some shortcuts and may be a premature optimization.  (I
	measured the real time needed to elaborate the Basis using
	time(1) with "tilt -fUptoElaborate -b" on a 650MHz Pentium
	III.  The following code averaged (four runs) 19.08 seconds.
	A simpler version of the code that did not use most of the
	shortcuts and did not pass and test the booleans averaged
	(three runs) 19.71 seconds.)
*)

structure IlContext :> ILCONTEXT =
struct

    open Il Util Name Listops Ppil IlUtil
    type fixity = Fixity.fixity

    val error = fn s => error "ilcontext.sml" s

    val IlcontextDiag = Stats.ff("IlcontextDiag")
    fun msg str = if (!IlcontextDiag) then print str else ()

    val debug = Stats.ff("IlcontextDebug")
    fun debugdo t = if (!debug) then (t(); ()) else ()

    val IlcontextChecks = Stats.tt "IlcontextChecks"
    fun sanitycheck t = if (!IlcontextChecks) then (t(); ()) else ()

    (* Setting this to true currently does not work.  Once it does, we
       should kill off the "false" code and the flag. *)
    val NoOpenInternalPaths = Stats.ff "NoOpenInternalPaths" (* XXX *)

    (*
	Type equivalence is needed to ensure overloading resolvable by
	distinct types.
    *)
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

    fun Context_Lookup_Var_Raw (ctxt : context,
				v : var) : (label * phrase_class) option =
	let val CONTEXT {varMap,...} = ctxt
	in  VarMap.find (varMap, v)
	end
    local

	type lvs = mod * subst

	fun lvs (m : mod) : lvs = (m, empty_subst)

	(*
		Note that the only local expression variables that may
		appear in inlined expressions are coercions.
	*)
	fun lvs_extend ((m,subst) : lvs, sdec) : lvs =
	    let val SDEC(l,dec) = sdec
		val p = (m,l)
		val subst =
		    (case dec
		       of DEC_EXP (v,_,_,_) =>
			   if is_coercion l
			       then subst_add_expvar (subst, v, MODULE_PROJECT p)
			   else subst
			| DEC_CON (v,_,_,_) =>
			   subst_add_convar (subst, v, CON_MODULE_PROJECT p)
			| DEC_MOD (v,_,_) =>
			   subst_add_modvar (subst, v, MOD_PROJECT p))
	    in  (m,subst)
	    end

	(*
		Rewrite SIGNAT_OF(localvar.labs) as SIGNAT_OF(v.labs')
		where v is bound in the ambient context.
	*)
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

	fun eliminate (lvs, sdecs : sdecs) : sdecs =
	    #1 (foldl_acc eliminate' lvs sdecs)

	and eliminate' (sdec, lvs) : sdec * lvs =
	    let val SDEC (l,dec) = sdec
		val subst = lvs_subst lvs
		val EXP = fn e => exp_subst(e,subst)
		val CON = fn c => con_subst(c,subst)
		val SIG = fn s => sig_subst(s,subst)
		val OPT = Option.map
		val dec =
		    (case dec
		       of DEC_EXP (v,c,eopt,inline) =>
			    DEC_EXP (v,CON c,OPT EXP eopt,inline)
			| DEC_CON (v,k,copt,inline) =>
			    DEC_CON (v,k,OPT CON copt,inline)
			| DEC_MOD (v,poly,s as SIGNAT_STRUCTURE sdecs) =>
			    let val lvs = lvs_recurse (lvs,l,s)
				val sdecs = eliminate (lvs,sdecs)
			    in  DEC_MOD (v,poly,SIGNAT_STRUCTURE sdecs)
			    end
			| DEC_MOD (v,poly,s) => DEC_MOD (v,poly,SIG s))
	    in  (SDEC (l,dec), lvs_extend (lvs,sdec))
	    end

	exception Memo

	datatype class =
	    MOD of signat * bool
	  | SIG of signat

	fun lookup (arg : context * var) : class =
	    (case Context_Lookup_Var_Raw arg
	       of SOME(_,pc) =>
		    (case pc
		       of PHRASE_CLASS_MOD (_,_,s,f) =>
			    (MOD (f(), true) handle Memo => MOD (s, false))
			| PHRASE_CLASS_SIG (_,s) => SIG s
			| _ =>
			    error "lookup saw non-module, non-signature\
				  \ variable")
		| NONE => error "lookup saw unbound variable")

	fun project_fast (s : signat, labs : labels) : sdecs * bool =
	    let
		fun sdecs (s : signat) : sdecs =
		    (case s
		       of SIGNAT_STRUCTURE sdecs => sdecs
			| _ => error "project_fast saw non-sdecs signature")
		fun project (l : label, s : signat) : signat =
		    let val sdecs = sdecs s
			val sdecopt = find_sdec (sdecs,l)
		    in  (case sdecopt
			   of SOME (SDEC(_,DEC_MOD(_,_,s))) => s
			    | SOME _ => error "projecting from non-module\
					      \ component in project_fast"
			    | NONE => error "cannot find label in project_fast")
		    end
	    in  (sdecs(foldl project s labs),true)
	    end

	fun reduce (ctxt : context, s : signat) : sdecs * bool =
	    (case s
	       of SIGNAT_STRUCTURE sdecs => (sdecs,false)
		| SIGNAT_VAR v => reduce_sigvar (ctxt, v)
		| SIGNAT_OF (PATH(v,labs)) => reduce_sigof (ctxt, v, labs)
		| SIGNAT_FUNCTOR _ => error "reduce saw functor signature")

	and reduce_sigvar (ctxt : context, v : var) : sdecs * bool =
	    (case lookup (ctxt, v)
	       of SIG s => reduce(ctxt,s)
		| _ => error "reduce_sigvar saw non-signature variable")

	and reduce_sigof (ctxt : context, v : var,
			  labs : labels) : sdecs * bool =
	    (case lookup (ctxt, v)
	       of MOD (s,false) =>
		    (case reduce (ctxt,s)
		       of (sdecs,true) =>
			    project_fast (SIGNAT_STRUCTURE sdecs, labs)
			| (sdecs,false) =>
			    let val m = MOD_VAR v
				val sdecs = selfify(ctxt,m,sdecs)
			    in  project(ctxt,m,sdecs,labs)
			    end)
		| MOD (s,true) => project_fast (s, labs)
		| _ => error "reduce_sigof saw non-module variable")

	and project (ctxt : context, m : mod, sdecs : sdecs,
		     labs : labels) : sdecs * bool =
	    let
		fun find (l : label, sdecs, lvs) : lvs * sdecs * bool =
		    (case sdecs
		       of nil => error "project cannot find label"
			| (sdec as SDEC(l',dec)) :: rest =>
			    if eq_label (l,l') then
				let val s = (case dec
					       of DEC_MOD(_,_,s) => s
						| _ => error "projecting from non-module component")
				    val s = lvs_rewrite (lvs, s)
				    val lvs = lvs_recurse (lvs,l,s)
				    val (sdecs,memoized) = reduce (ctxt, s)
				in  (lvs,sdecs,memoized)
				end
			    else find (l,rest, lvs_extend (lvs,sdec)))
		fun loop (labels, lvs, sdecs) : sdecs * bool =
		    (case labels
		       of nil => (sdecs_subst (sdecs, lvs_subst lvs),false)
			| (l :: rest) =>
			    (case find (l,sdecs,lvs)
			       of (_,sdecs,true) =>
				    project_fast (SIGNAT_STRUCTURE sdecs,rest)
				| (lvs,sdecs,false) => loop (rest,lvs,sdecs)))
	    in  loop (labs, lvs m, sdecs)
	    end

	and selfify (ctxt : context, m : mod, sdecs : sdecs) : sdecs =
	    map (selfify' (ctxt,m)) sdecs

	and selfify' (ctxt, m : mod) (sdec : sdec) : sdec =
	    (case sdec
	       of SDEC (_, DEC_EXP _) => sdec
	        | SDEC (_, DEC_CON (_,_,SOME _,_)) => sdec
	        | SDEC (l, DEC_CON (v,k,NONE,inline)) =>
		    let val c = CON_MODULE_PROJECT (m,l)
		        val dec = DEC_CON (v,k,SOME c, inline)
		    in  SDEC (l,dec)
		    end
		| SDEC (_, DEC_MOD (_,_,SIGNAT_FUNCTOR _)) => sdec
		| SDEC (_, DEC_MOD (_,_,SIGNAT_OF _)) => sdec
		| SDEC (l, DEC_MOD (v,poly,s)) =>
		    let val sdecs =
			    (case reduce (ctxt, s)
			       of (sdecs,true) => sdecs
				| (sdecs,false) =>
				    selfify (ctxt,MOD_PROJECT (m,l),sdecs))
			val dec = DEC_MOD (v,poly,SIGNAT_STRUCTURE sdecs)
		    in  SDEC (l,dec)
		    end)

	fun deep_reduce (ctxt : context, lvs, s : signat) : sdecs * bool =
	    (case reduce (ctxt, s)
	       of (sdecs,false) =>
		    (#1 (foldl_acc (deep_reduce' ctxt) lvs sdecs), false)
		| r => r)

	and deep_reduce' (ctxt : context) (arg as (sdec,lvs)) : sdec * lvs =
	    (case sdec
	       of SDEC (_,DEC_MOD(_,_,SIGNAT_FUNCTOR _)) => arg
		| SDEC (l,DEC_MOD(v,poly,s)) =>
		   let val s = lvs_rewrite (lvs, s)
		       val (sdecs,_) = deep_reduce (ctxt, lvs_recurse (lvs,l,s),
						    s)
		       val s = SIGNAT_STRUCTURE sdecs
		       val lvs = lvs_extend (lvs,sdec)
		       val sdec = SDEC (l,DEC_MOD (v,poly,s))
		   in  (sdec,lvs)
		   end
		| _ => arg)

	fun transform' (ctxt : context, m : mod, s : signat) : signat =
	    let val lvs = lvs m
		val sdecs =
		    (case deep_reduce (ctxt,lvs,s)
		       of (sdecs,true) => sdecs
			| (sdecs,false) =>
			    eliminate (lvs, selfify (ctxt,m,sdecs)))
		val s' = SIGNAT_STRUCTURE sdecs
(*
		val _ = debugdo(fn () =>
				(print "\n\ntransformed "; pp_mod m;
				 print " : "; pp_signat s; print "\nto : ";
				 pp_signat s'; print "\n\n"))
*)
	    in  s'
	    end
    in
	val noTransform : unit -> signat = fn () => raise Memo

	fun transform (ctxt : context, m : mod, s : signat) : unit -> signat =
	    (case s
	       of SIGNAT_FUNCTOR _ => (fn () => s)
		| _ => Util.memoize (fn () => transform' (ctxt,m,s)))
    end

    (* ----------- context extenders ----------------------------  *)
    val empty_context = CONTEXT{varMap = VarMap.empty,
				ordering = nil,
				labelMap = LabelMap.empty,
				fixityMap = LabelMap.empty,
				overloadMap = LabelMap.empty}

    type varMap = (label * phrase_class) VarMap.map
    type labelMap = vpath LabelMap.map
    type overloadMap = ovld LabelMap.map

    fun labelmap_remove x = #1(LabelMap.remove x)

    (*
	Prepare for the possible shadowing of a label.  When shadowing
	a convar's label, we also change its equality function's label.
    *)

    fun shadow' (vm : varMap, lm : labelMap, om : overloadMap, l : label,
		 labopt : label option) : varMap * labelMap * overloadMap =
	(case (LabelMap.find(om,l), LabelMap.find(lm,l))
	   of (SOME _, NONE) => (vm,lm,labelmap_remove(om,l))
	    | (NONE, SOME (v,nil)) =>
	       let val l' = (case labopt
			       of SOME l' => l'
				| NONE => fresh_internal_label(label2name' l))
		   val SOME (_,pc) = VarMap.find(vm,v)
		   val vm = VarMap.insert (vm,v,(l',pc))
		   val lm = labelmap_remove(lm,l)
		   val lm = LabelMap.insert (lm,l',(v,nil))
	       in  (case pc
		      of PHRASE_CLASS_CON _ =>
			    shadow' (vm,lm,om,to_eq l,SOME (to_eq l'))
		       | _ => (vm,lm,om))
	       end
	    | (NONE, SOME _) => (vm,labelmap_remove(lm,l),om)
	    | (NONE, NONE) => (vm,lm,om)
	    | (SOME _, SOME _) => error "overloadMap and labelMap labels\
					\ not disjoint")
    fun shadow (vm,lm,om,l) = shadow' (vm,lm,om,l,NONE)

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
	fun check_shadow (ctxt : context, l : label, v : var,
			  pc : phrase_class) : unit =
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
		val CONTEXT {varMap, ordering, labelMap,
			     fixityMap, overloadMap} = ctxt
		val (varMap,labelMap,overloadMap) =
		    shadow (varMap,labelMap,overloadMap,l)
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
			fixityMap = fixityMap,
			overloadMap = overloadMap}
	    end
    end

    local
	(*
		Extend labelMap with top-level labels visible by the
		star convention.
	*)
	type acc = varMap * labelMap * overloadMap

	fun add_labels (v : var, labs : labels, signat, acc) : acc =
	    (case signat
	       of SIGNAT_STRUCTURE sdecs =>
		    foldl (add_labels' (v,labs)) acc sdecs
		| _ => acc)

	and add_labels' (v : var, labs : labels) (sdec, acc) : acc =
	    let val SDEC(l,dec) = sdec
		val labs = labs @ [l]
		val acc =
		    (if !NoOpenInternalPaths andalso is_label_internal l
		     then acc
		     else
			let val (vm,lm,om) = acc
			    val (vm,lm,om) = shadow (vm,lm,om,l)
			    val lm = LabelMap.insert (lm,l,(v,labs))
			in  (vm,lm,om)
			end)
	    in
		(case (is_open l,dec)
		   of (true,DEC_MOD (_,_,s)) =>
			add_labels (v,labs,s,acc)
		    | _ => acc)
	    end
    in

	(*
		Extend context with l > v:pc.  Handles starred labels
		and the memo table.
	*)
	fun add (ctxt : context, l : label, v : var,
		 pc : phrase_class) : context =
	    let val ctxt = add' (ctxt,l,v,pc)
	    in  (case pc
	    	   of PHRASE_CLASS_MOD (m,poly,s,_) =>
			let val CONTEXT {varMap, ordering, labelMap,
					 fixityMap, overloadMap} = ctxt
			    val f = transform (ctxt,m,s)
			    val pc = PHRASE_CLASS_MOD(m,poly,s,f)
			    val varMap = VarMap.insert (varMap,v,(l,pc))
			    val (varMap,labelMap,overloadMap) =
				if is_open l then
				    add_labels (v,nil,f(),
						(varMap,labelMap,overloadMap))
				else (varMap,labelMap,overloadMap)
			in  CONTEXT{varMap = varMap,
				    ordering = ordering,
				    labelMap = labelMap,
				    fixityMap = fixityMap,
				    overloadMap = overloadMap}
			end
		   | _ => ctxt)
	    end
    end

    (*
	Add a top-level label that maps to the given bound variable.
	Ignores starred structures.  N.B. This exists because of a
	deficiency of the pattern compiler; it should go away.
    *)
    fun add_context_label (context, label, var) : context = (* ugly hack *)
	let val CONTEXT {varMap, ordering, labelMap,
			 fixityMap, overloadMap} = context
	    val _ = sanitycheck(fn () =>
				(case VarMap.find (varMap, var)
				   of SOME _ => ()
				    | NONE =>
				       (debugdo (fn () =>
						 (print "unbound variable ";
						  pp_var var;
						  print " detected while\
							\ adding label ";
						  pp_label label; print "\n"));
					error "unbound variable in\
					      \ add_context_label")))
	    val (varMap,labelMap,overloadMap) =
		shadow (varMap,labelMap,overloadMap,label)
	    val labelMap = LabelMap.insert (labelMap,label,(var,nil))
	in  CONTEXT{varMap = varMap,
		    ordering = ordering,
		    labelMap = labelMap,
		    fixityMap = fixityMap,
		    overloadMap = overloadMap}
	end

    fun add_context_sdec (ctxt : context, sdec) : context =
	let val SDEC (l,dec) = sdec
	    val (v,pc) =
		(case dec
		    of DEC_EXP (v,c,eopt,inline) =>
			(v,PHRASE_CLASS_EXP (VAR v,c,eopt,inline))
		     | DEC_CON (v,k,copt,inline) =>
			(v,PHRASE_CLASS_CON (CON_VAR v,k,copt,inline))
		     | DEC_MOD (v,poly,s) =>
			(v,PHRASE_CLASS_MOD (MOD_VAR v,poly,s,noTransform)))
	in  add (ctxt,l,v,pc)
	end

    fun add_context_exp (ctxt,l,v,c) =
	add_context_sdec (ctxt,SDEC(l,DEC_EXP(v,c,NONE,false)))
    fun add_context_con (ctxt,l,v,k,copt) =
	add_context_sdec (ctxt,SDEC(l,DEC_CON(v,k,copt,false)))
    fun add_context_mod (ctxt,l,v,s) =
	add_context_sdec (ctxt,SDEC(l,DEC_MOD(v,false,s)))

    fun add_context_sdec' (sdec, ctxt) = add_context_sdec (ctxt, sdec)
    fun add_context_sdecs (ctxt, sdecs) = foldl add_context_sdec' ctxt sdecs

    fun anon_label () = fresh_internal_label "anon"
    fun dec2sdec dec = SDEC(anon_label(), dec)

    fun add_context_exp' (ctxt,v,c) = add_context_exp (ctxt,anon_label(),v,c)
    fun add_context_con' (ctxt,v,k,copt) =
	add_context_con (ctxt,anon_label(),v,k,copt)
    fun add_context_mod' (ctxt,v,s) = add_context_mod (ctxt,anon_label(),v,s)
    fun add_context_dec (ctxt,dec) = add_context_sdec (ctxt,dec2sdec dec)
    fun add_context_decs (ctxt,decs) = add_context_sdecs (ctxt,map dec2sdec decs)

    fun add_context_sig (ctxt,l,v,s) = add (ctxt,l,v,PHRASE_CLASS_SIG (v,s))
    fun add_context_sig' (ctxt,v,s) = add_context_sig (ctxt,anon_label(),v,s)

    fun add_context_extern (ctxt,l,v,l',c) =
	add (ctxt,l,v,PHRASE_CLASS_EXT (v,l',c))
    fun add_context_extern' (ctxt,v,l,c) =
	add_context_extern (ctxt,anon_label(),v,l,c)

    fun add_context_fixity (ctxt : context, l : label, fixity) : context =
	let val CONTEXT {varMap, ordering, labelMap,
			 fixityMap, overloadMap} = ctxt
	    val fixityMap = LabelMap.insert (fixityMap,l,fixity)
	in  CONTEXT {varMap = varMap,
		     ordering = ordering,
		     labelMap = labelMap,
		     fixityMap = fixityMap,
		     overloadMap = overloadMap}
	end

    fun add_context_overexp (ctxt : context, l : label, ovld2 : ovld) : context =
	let val CONTEXT {varMap, ordering, labelMap,
			 fixityMap, overloadMap} = ctxt
	    val ovld1 = (case Name.LabelMap.find(overloadMap, l) of
			     SOME ovld1 => ovld1
			   | NONE => OVLD ([], NONE))
	    val ovld3 = ovld_add(l,ctxt,ovld1,ovld2)
	    val (varMap,labelMap,overloadMap) =
		shadow (varMap,labelMap,overloadMap,l)
	    val overloadMap = LabelMap.insert(overloadMap, l, ovld3)
	in  CONTEXT {varMap = varMap,
		     ordering = ordering,
		     labelMap = labelMap,
		     fixityMap = fixityMap,
		     overloadMap = overloadMap}
	end

    fun add_context_entry (ctxt : context, entry : context_entry) : context =
	(case entry
	   of CONTEXT_SDEC sdec => add_context_sdec (ctxt,sdec)
	    | CONTEXT_SIGNAT (l,v,s) => add_context_sig (ctxt,l,v,s)
	    | CONTEXT_EXTERN (l,v,l',c) => add_context_extern (ctxt,l,v,l',c)
	    | CONTEXT_FIXITY (l,f) => add_context_fixity (ctxt,l,f)
	    | CONTEXT_OVEREXP (l,ovld) => add_context_overexp (ctxt,l,ovld))

    fun add_context_entry' (entry,ctxt) = add_context_entry (ctxt,entry)
    fun add_context_entries (ctxt, entries) =
	foldl add_context_entry' ctxt entries

    (* ----------- context access ----------------------------  *)

    local
	fun varEntry (varMap) (v : var) : context_entry =
	    let val SOME (l,pc) = VarMap.find (varMap,v)
		val sdec = fn dec => CONTEXT_SDEC (SDEC(l,dec))
	    in
		(case pc
		   of PHRASE_CLASS_EXP (_,c,eopt,inline) =>
			sdec (DEC_EXP(v,c,eopt,inline))
		    | PHRASE_CLASS_CON (_,k,copt,inline) =>
			sdec (DEC_CON(v,k,copt,inline))
		    | PHRASE_CLASS_MOD (_,poly,s,_) => sdec (DEC_MOD(v,poly,s))
		    | PHRASE_CLASS_SIG (_,s) => CONTEXT_SIGNAT (l,v,s)
		    | PHRASE_CLASS_EXT (_,l',c) => CONTEXT_EXTERN (l,v,l',c))
	    end

	fun varEntries (varMap, ordering) : entries =
	    map (varEntry varMap) (rev ordering)

	fun overloadEntries overloadMap : entries =
	    map CONTEXT_OVEREXP (LabelMap.listItemsi overloadMap)

	fun fixityEntries fixityMap : entries =
	    map CONTEXT_FIXITY (LabelMap.listItemsi fixityMap)
    in
	fun list_entries (ctxt : context) : entries =
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

    fun Context_Lookup_Var (ctxt : context,
			    v : var) : (label * phrase_class) option =
	(case Context_Lookup_Var_Raw (ctxt, v)
	   of SOME (l, PHRASE_CLASS_MOD (m,poly,s,f)) =>
		SOME (l, PHRASE_CLASS_MOD (m,poly,f(),f))
	    | a => a)

    fun Context_Lookup_Label (ctxt : context, l : label) : path option =
	let val CONTEXT {labelMap, ...} = ctxt
	in  Option.map PATH (LabelMap.find (labelMap, l))
	end

    fun Context_Lookup_Overload (ctxt : context, l : label) : ovld option =
	let val CONTEXT {overloadMap, ...} = ctxt
	in  LabelMap.find (overloadMap, l)
	end

    fun reachableVars(varMap, black, gray) =
	if (VarSet.isEmpty gray)
	    then black
	else
	    let
		val black = VarSet.union(black, gray)
		fun folder (v,s) =
		    (case VarMap.find(varMap,v) of
			 SOME (_, pc) => VarSet.union (s,pc_free pc)
		       | _ => s)
		val freeVars = VarSet.foldl folder VarSet.empty gray
		fun pred v = (case VarMap.find(varMap,v) of
				  SOME _ => true
				| _ => false)
		val temp = VarSet.filter pred freeVars
		val gray = VarSet.difference(temp, black)
	    in  reachableVars(varMap,black,gray)
	    end

    fun reachable (ctxt : context, roots : VarSet.set) : VarSet.set =
	let val CONTEXT {varMap, ...} = ctxt
	in  reachableVars(varMap, VarSet.empty, roots)
	end

    fun gc_context (ctxt : context, roots : VarSet.set) : context =
	let val CONTEXT {varMap, ordering, labelMap,
			 fixityMap, overloadMap} = ctxt
	    val roots = (LabelMap.foldli
			 (fn (l,(v,_),s) => if Name.keep_import l
						then VarSet.add(s,v)
					    else s)
			 roots labelMap)
	    val reachable = reachable(ctxt, roots)
	    fun isReachable v = Name.VarSet.member(reachable,v)
	    val n = VarMap.numItems varMap
	    val varMap = VarMap.filteri (fn (v,_) => isReachable v) varMap
	    val labelMap =
		LabelMap.filteri (fn (_,(v,_)) => isReachable v) labelMap
	    val ordering = List.filter isReachable ordering
	    val _ = msg ("  gc_context: " ^ Int.toString n ^
			 " items in original context.  " ^
			 (Int.toString (VarMap.numItems varMap)) ^
			 " items in reduced context.\n")
	in  CONTEXT{varMap = varMap,
		    ordering = ordering,
		    labelMap = labelMap,
		    fixityMap = fixityMap,
		    overloadMap = overloadMap}
	end
end
