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
	val Ceq_con : (context * con * con -> bool) ref =
	    ref (fn _ => error "eq_con not installed")
	fun eq_con arg = (!Ceq_con arg)
	fun eq ctxt ((c1,e1,d1),(c2,e2,d2)) = eq_con(ctxt,c1,c2)
	fun sub ctxt (exp1, exp2) = Listops.list_diff_eq(eq ctxt, exp1, exp2)
	fun add ctxt (exp1, exp2) = Listops.list_sum_eq(eq ctxt, exp1, exp2)
	fun expanded f (o1, o2) = ovld_collapse (f (ovld_expand o1, ovld_expand o2))
    in
	fun installHelpers{eq_con : context * con * con -> bool} : unit =
	    Ceq_con := eq_con

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

    (* 
       selfify does not check for variable capture, 
       i.e. that the free variables of m do not intersect the bound variables of s.
       I think this is OK, given the assumption that all bound variables are distinct,
       but it should be confirmed. -Derek
    *)

    (* The no_depend option is true for all normal uses of selfify, i.e. to get the
       principal signatures of variables.  It is only supplied as false when called
       from Rds_IsSub.  When no_depend is false, we do not perform any substitutions
       for local references to type components within the signature, but merely
       "transparentiate" the opaque types.  See the note in the Rds_IsSub function
       in ilstatic.sml.  -Derek *)
    fun selfify (no_depend : bool) (ctxt : context, m : mod, s : signat) : signat =
	  let 

	      fun selfify_sig (m : mod) (s : signat) : signat = (
                 case s
                   of SIGNAT_STRUCTURE sdecs => SIGNAT_STRUCTURE(selfify_sdecs m sdecs)
                    | SIGNAT_RDS (v,sdecs) => 
                        SIGNAT_STRUCTURE(selfify_sdecs m (sdecs_subst(sdecs,subst_modvar(v,m))))
		    | SIGNAT_VAR v =>
		       (case Context_Lookup_Var_Raw(ctxt,v)
			  of SOME(_,PHRASE_CLASS_SIG(_,s')) => selfify_sig m s'
                           | _ => error "selfify: signature variable not bound to signature")
		    | SIGNAT_FUNCTOR (v,s1,s2,APPLICATIVE) =>
                       let val s2' = selfify_sig (MOD_APP(m,MOD_VAR v)) s2
		       in  SIGNAT_FUNCTOR(v,s1,s2',APPLICATIVE)
		       end
		    | SIGNAT_FUNCTOR _ => s
		    | SIGNAT_SWITCH {use_private,sig_private,sig_public} => 
                       if !use_private 
			   then selfify_sig m sig_private
		       else selfify_sig m sig_public
              )

	      and selfify_sdecs (m : mod) (sdecs : sdecs) : sdecs = 
		  #1(foldl_acc (selfify_sdec m) empty_subst sdecs)

	      and selfify_sdec (m : mod) (sdec, subst) : sdec * subst =
		let val sdec as SDEC(l,dec) = if no_depend then sdec_subst(sdec,subst) else sdec
		    val (dec,subst) = 
			      case dec
                                of DEC_EXP(v,_,_,_) => 
				    (dec, if is_coercion l 
					      then subst_add_expvar(subst,v,MODULE_PROJECT(m,l))
					  else subst)
			         | DEC_CON(v,_,SOME _,_) => 
				    (dec, subst_add_convar(subst,v,CON_MODULE_PROJECT(m,l)))
				 | DEC_CON(v,k,NONE,inline) =>
				    (DEC_CON(v,k,SOME(CON_MODULE_PROJECT(m,l)),inline),
				     subst_add_convar(subst,v,CON_MODULE_PROJECT(m,l)))
				 | DEC_MOD(v,poly,s) => 
				    (DEC_MOD(v,poly,selfify_sig (MOD_PROJECT(m,l)) s),
				     subst_add_modvar(subst,v,MOD_PROJECT(m,l)))
		in  (SDEC(l,dec),subst)
		end

	  in  selfify_sig m s
	  end

    val noTransform : unit -> signat = 
        fn () => error "noTransform is a dummy function and should never be called"

    fun transform (ctxt : context, m : mod, s : signat) : unit -> signat =
	Util.memoize (fn () => selfify true (ctxt,m,s))

    val sig_changed = ref false

    fun update_context () = sig_changed := true

    fun transform_switchable (ctxt : context, m : mod, s : signat) : unit -> signat =
	let 
	    val r = ref s
	    val _ = sig_changed := true
	    fun reselfify () = (r := selfify true (ctxt,m,s);
				sig_changed := false;
				!r)
	in
	    fn () => if !sig_changed then reselfify() else !r
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
	Handle shadowing.  Equality functions are special: If a type t is
	shadowed, then +Et must be shadowed in order to maintain the
	invariant that if +Et is visible, then it is the equality function
	for type t.
    *)

    fun lookup_vpath_dec (vm:varMap, vpath:vpath) : dec =
	let fun fail() = error "labelMap contains invalid vpath"
	    fun find (sdecs,lab) : dec =
		(case sdecs of
		    (SDEC(l,dec) :: sdecs) =>
			if Name.eq_label(l,lab) then dec
			else find(sdecs,lab)
		 |  nil => fail())
	    fun project (sdecs,labs) : dec =
		(case labs of
		    l::nil => find(sdecs,l)
		 |  l::labs =>
			(case find(sdecs,l) of
			    DEC_MOD (_,_,SIGNAT_STRUCTURE sdecs) =>
				project(sdecs,labs)
			 |  _ => fail())
		 |  nil => fail())
	    val (v,labs) = vpath
	in  (case VarMap.find(vm,v) of
		SOME (_,PHRASE_CLASS_MOD(_,_,_,f)) =>
		    (case f() of
			SIGNAT_STRUCTURE sdecs => project(sdecs,labs)
		     |  _ => fail())
	     |  _ => fail())
	end
			
    fun shadow' (vm : varMap, lm : labelMap, om : overloadMap, l : label,
		 l' : label) : varMap * labelMap * overloadMap * bool =
	(case (LabelMap.find(om,l), LabelMap.find(lm,l))
	   of (SOME _, NONE) => (vm,lm,labelmap_remove(om,l),false)
	    | (NONE, SOME (v,nil)) =>
	       let val SOME (_,pc) = VarMap.find(vm,v)
		   val vm = VarMap.insert (vm,v,(l',pc))
		   val lm = labelmap_remove(lm,l)
		   val lm = LabelMap.insert (lm,l',(v,nil))
		   val eq =
			(case pc of
			    PHRASE_CLASS_CON _ => true
			 |  _ => false)
	       in  (vm,lm,om,eq)
	       end
	    | (NONE, SOME vpath) =>
	       let val lm = labelmap_remove(lm,l)
		   val eq =
			(case (lookup_vpath_dec(vm,vpath)) of
			    DEC_CON _ => true
			 |  _ => false)
	       in  (vm,lm,om,eq)
	       end
	    | (NONE, NONE) => (vm,lm,om,false)
	    | (SOME _, SOME _) => error "overloadMap and labelMap labels\
					\ not disjoint")
    fun shadow (vm : varMap, lm : labelMap, om : overloadMap, l : label)
	: varMap * labelMap * overloadMap =
	let val l' = fresh_internal_label(label2name' l)
	    val (vm,lm,om,eq) = shadow' (vm,lm,om,l,l')
	in  if eq then
		let val eql = to_eq l
		    val eql' = to_eq l'
		    val (vm,lm,om,_) = shadow' (vm,lm,om,eql,eql')
		in  (vm,lm,om)
		end
	    else (vm,lm,om)
	end

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
	fun add_transform (transformer) (ctxt : context, l : label, v : var,
					 pc : phrase_class) : context =
	    let val ctxt = add' (ctxt,l,v,pc)
	    in  (case pc
	    	   of PHRASE_CLASS_MOD (m,poly,s,_) =>
			let val CONTEXT {varMap, ordering, labelMap,
					 fixityMap, overloadMap} = ctxt
			    val f = transformer (ctxt,m,s)
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

    val add = add_transform transform

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

    fun add_context_mod_switchable(ctxt,v,s) =
	add_transform transform_switchable
	  (ctxt,anon_label(),v,PHRASE_CLASS_MOD (MOD_VAR v, false, s, noTransform))

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
