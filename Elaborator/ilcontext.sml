(*$import Prelude TopLevel List Int ILCONTEXT Il Util Name Listops Ppil Stats IlUtil *)
structure IlContext :> ILCONTEXT =
struct

    open Il Util Name Listops Ppil IlUtil
	
    val error = fn s => error "ilcontext.sml" s
    val debug = Stats.ff("IlcontextDebug")
    fun debugdo t = if (!debug) then (t(); ()) else ()

    (* --------------- BASICS ------------------------------- *)
    type var_seq_map = (label * phrase_class) VarMap.map * var list
    fun var_seq_insert ((m,s),v,value) = (VarMap.insert(m,v,value),v::s)
    val empty_context = CONTEXT{fixityMap = LabelMap.empty,
				overloadMap = LabelMap.empty,
				labelMap = LabelMap.empty,
				pathMap = PathMap.empty,
				ordering = []}

    (* ------ Type equivalence needed to ensure overloading resolvable by distinct types ------- *)
    local 
	val Ceq_con = ref (NONE : (context * con * con -> bool) option)
    in
	fun installHelpers{eq_con} = 
	    ((case !Ceq_con of
		 NONE => ()
	       | SOME _ => (print "WARNING: IlContext.installHelpers called more than once.\n"));
	     Ceq_con := SOME eq_con)
	fun eq_con arg = let val SOME eq_con = !Ceq_con
			 in  eq_con arg
			 end
	fun eq ctxt ((c1,e1,d1),(c2,e2,d2)) = eq_con(ctxt,c1,c2)
	fun sub ctxt (exp1, exp2) = Listops.list_diff_eq(eq ctxt, exp1, exp2)
	fun add ctxt (exp1, exp2) = Listops.list_sum_eq(eq ctxt, exp1, exp2)
	fun expanded f (o1, o2) = ovld_collapse (f (ovld_expand o1, ovld_expand o2))
	
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

    (* ---------------- LOOKUP RULES --------------------------- 
     The lookup rules can return item from different grammatical classes
     so we need some additional datatypes to package up the results 
     --------------------------------------------------------- *)

    type phrase_class_p = path * phrase_class


    fun Context_Fixity (CONTEXT {fixityMap,...}) = fixityMap
    fun Context_Ordering (CONTEXT {ordering,...}) = rev ordering
    fun Context_Lookup_Overload (CONTEXT {overloadMap, ...}, lab) = LabelMap.find(overloadMap, lab)
    fun Context_Lookup_Label (CONTEXT {labelMap, ...}, lab) = LabelMap.find(labelMap, lab)
    fun Context_Lookup_Path (CONTEXT {pathMap, ...}, PATH path) = PathMap.find(pathMap, path)
    fun Context_Lookup_Var (CONTEXT {pathMap, ...}, v) = PathMap.find(pathMap, (v,[]))


    (* --------------------- EXTENDERS ---------------------------------------- *)
    fun add_context_fixity(CONTEXT {fixityMap, overloadMap, labelMap, pathMap, ordering}, label, fixity) = 
	CONTEXT({fixityMap = LabelMap.insert(fixityMap,label,fixity),
		 overloadMap = overloadMap,
		 labelMap = labelMap,
		 pathMap = pathMap,
		 ordering = ordering})

    fun reduce_sigvar ctxt v =
	(case (Context_Lookup_Var(ctxt,v)) of
	     SOME(_,PHRASE_CLASS_SIG(v,s)) => s
	   | _ => error "reduce_sigvar found unbound sigvar")

    fun reduce_signat ctxt (SIGNAT_VAR v) = reduce_signat ctxt (reduce_sigvar ctxt v)
      | reduce_signat ctxt (SIGNAT_SELF (p, unself, self)) = reduce_signat ctxt self
      | reduce_signat ctxt (SIGNAT_OF (PATH(v,labs))) = 
	  let val s = (case Context_Lookup_Var(ctxt,v) of
			   SOME(_,PHRASE_CLASS_MOD(_,_,s)) => s
			 | _ => error ("SIGNAT_OF(" ^ (Name.var2string v) ^ ",...) unbound"))
	      fun project (l, s) = 
		  let fun find l [] = error "cannot find label in SIGNAT_OF"
			| find l ((SDEC(l',DEC_MOD(_,_,s)))::rest) = if eq_label(l,l') then s else find l rest
			| find l (_::rest) = find l rest
		  in  (case (reduce_signat ctxt s) of
			   SIGNAT_STRUCTURE sdecs => find l sdecs
			 | _ => error "ill-formed SIGNAT_OF")
		  end
	  in  reduce_signat ctxt (foldl project s labs)
	  end
      | reduce_signat ctxt s = s

    fun shadow (overloadMap, labelMap, pathMap, lab) = 
	let val overloadMap = 
	    (case (LabelMap.find(overloadMap,lab)) of
		   NONE => overloadMap
		 | SOME celist => #1(LabelMap.remove(overloadMap,lab)))
	in  (case (LabelMap.find(labelMap,lab)) of
		   NONE => (overloadMap, labelMap, pathMap)
		 | SOME (PATH p,_) => 
		       let val newlab = fresh_internal_label(label2name' lab)
		       in  (case PathMap.find(pathMap,p) of
				NONE => error "inconsistent context"
			      | SOME (_, pc) => 
				    let val pathMap = PathMap.insert(pathMap,p,(newlab, pc))
					val (labelMap,_) = LabelMap.remove(labelMap, lab)
					val labelMap = LabelMap.insert(labelMap, newlab,(PATH p, pc))
				    in  (overloadMap, labelMap, pathMap)
				    end)
		       end)
	end
	     
    fun add_sdec(ctxt, pathopt, sdec as (SDEC(l,dec))) = 
	let fun mk_path v = (case pathopt of
				 NONE => PATH(v,[])
			       | SOME p => join_path_labels(p,[l]))
	    fun help(CONTEXT {fixityMap, overloadMap, labelMap, pathMap, ordering},
		     v, from_path, pc_maker) =
		let val path = mk_path v
		    val obj = from_path path
		    val pc = pc_maker obj
		    val PATH vpath = path
		    val (overloadMap, labelMap, pathMap) = shadow(overloadMap, labelMap, pathMap, l)
		    val labelMap = LabelMap.insert(labelMap,l,(path,pc))
		    val pathMap = PathMap.insert(pathMap,vpath,(l,pc))
		    val ordering = path :: ordering
		in CONTEXT{fixityMap = fixityMap,
			   overloadMap = overloadMap,
			   labelMap = labelMap,
			   pathMap = pathMap,
			   ordering = ordering}
		end
	    fun sdec_help (v,l) (sdec,ctxt) = add_sdec(ctxt,SOME(mk_path v),sdec)
	in case dec of
	    DEC_EXP(v,c,eopt,inline) => help(ctxt, v, path2exp, 
					     fn obj => PHRASE_CLASS_EXP(obj, c, eopt, inline))
	  | DEC_CON(v,k,copt,inline) => help(ctxt, v, path2con, 
					     fn obj => PHRASE_CLASS_CON(obj, k, copt, inline))
	  | DEC_MOD (v,b,s as SIGNAT_FUNCTOR _) => help(ctxt,v, path2mod, 
						   fn obj => (PHRASE_CLASS_MOD(obj,b,s)))
	  | DEC_MOD (v,b,s) =>
		  let val _ = (case s of
				   SIGNAT_SELF _ => ()
				 | _ => (print "adding non-selfified signature to context: "; pp_sdec sdec; print "\n";
					 error "adding non-selfified signature to context"))
		      val ctxt = help(ctxt, v, path2mod, fn obj => (PHRASE_CLASS_MOD(obj, b,s)))
		  in  if (is_open l) 
			  then let val sdecs = (case (reduce_signat ctxt s) of
						    SIGNAT_STRUCTURE sdecs => sdecs
						  | _ => error "open label - not struct sig")
			       in  foldl (sdec_help (v,l)) ctxt sdecs
			       end
		      else ctxt
		  end
	end



    fun add_context_sig(CONTEXT {fixityMap, overloadMap, labelMap, pathMap, ordering},
			l, v, signat) = 
	let val pc = PHRASE_CLASS_SIG(v,signat)
	    val path = (v,[])
	    val (overloadMap,labelMap, pathMap) = shadow(overloadMap,labelMap, pathMap, l)
	in  CONTEXT({fixityMap = fixityMap,
		     overloadMap = overloadMap,
		     labelMap = Name.LabelMap.insert(labelMap,l,(PATH path, pc)),
		     pathMap = Name.PathMap.insert(pathMap,path,(l, pc)),
		     ordering = (PATH path)::ordering})
	end

    fun add_context_overexp(ctxt as CONTEXT {fixityMap, overloadMap, labelMap, pathMap, ordering},
			    l, ovld2) = 
	let val ovld1 = (case Name.LabelMap.find(overloadMap, l) of
			     SOME ovld1 => ovld1
			   | NONE => OVLD ([], NONE))
	    val ovld3 = ovld_add(l,ctxt,ovld1,ovld2)
	    val overloadMap = LabelMap.insert(overloadMap, l, ovld3)
	in  CONTEXT({fixityMap = fixityMap,
		     overloadMap = overloadMap,
		     labelMap = labelMap,
		     pathMap = pathMap,
		     ordering = ordering})
	end

    fun add_context_entry(ctxt, entry) = 
	(case entry of
	     CONTEXT_FIXITY (l,f) => add_context_fixity(ctxt,l,f)
	   | CONTEXT_SDEC sdec => add_sdec(ctxt,NONE,sdec)
	   | CONTEXT_SIGNAT (l,v,s) => add_context_sig(ctxt,l,v,s)
	   | CONTEXT_OVEREXP(l,ovld) => add_context_overexp(ctxt,l,ovld))

    fun add_context_entry'(entry,ctxt) = add_context_entry(ctxt,entry)
    fun add_context_entries (ctxt, entries) = foldl add_context_entry' ctxt entries
    fun add_context_sdec(ctxt,sdec) = add_sdec(ctxt,NONE,sdec)

    fun add_context_sdec'(sdec,ctxt) = add_sdec(ctxt,NONE,sdec)
    fun add_context_sdecs (ctxt, sdecs) = foldl add_context_sdec' ctxt sdecs

    fun anon_label () = fresh_internal_label "anon"
    fun dec2sdec dec = SDEC(anon_label(),dec)
    fun decs2sdecs decs = map dec2sdec decs
    fun add_context_decs(ctxt, decs) = add_context_sdecs(ctxt, decs2sdecs decs)
    fun add_context_dec(ctxt, dec) = add_context_decs(ctxt,[dec])

    fun add_context_exp(c, l, v, con) = add_context_sdec(c,SDEC(l,DEC_EXP(v,con,NONE,false)))
    fun add_context_mod(c, l, v, signat) = add_context_sdec(c,SDEC(l,DEC_MOD(v,false,signat)))
    fun add_context_con(c, l, v, kind, conopt) = 
	add_context_sdec(c,SDEC(l,DEC_CON(v,kind,conopt,false)))

    fun add_context_exp'(c, v, con) = add_context_exp(c,anon_label(), v, con)
    fun add_context_mod'(c, v, signat) = add_context_mod(c,anon_label(), v, signat)
    fun add_context_con'(c, v, kind, conopt) = add_context_con(c,anon_label(), v, kind, conopt)
    fun add_context_sig'(c, v, signat) = add_context_sig(c,anon_label(), v, signat)
	


    fun pcSubst(pc,subst) = 
	(case pc of
	     PHRASE_CLASS_EXP(e,con,eopt,inline) => 
		 let val e = exp_subst(e,subst)
		     val con = con_subst(con,subst)
		     val eopt = (case eopt of
				     NONE => NONE
				   | SOME e => SOME(exp_subst(e,subst)))
		 in  PHRASE_CLASS_EXP(e,con,eopt,inline)
		 end
	   | PHRASE_CLASS_CON(con,kind,copt,inline) => 
		 let val con = con_subst(con,subst)
		     val copt = (case copt of
				     NONE => NONE
				   | SOME c => SOME(con_subst(c,subst)))
		 in  PHRASE_CLASS_CON(con,kind,copt,inline)
		 end
	   | PHRASE_CLASS_MOD(m,b,signat) => 
		 let val m = mod_subst(m,subst)
		     val signat = sig_subst(signat,subst)
		 in  PHRASE_CLASS_MOD(m,b,signat)
		 end
	   | PHRASE_CLASS_SIG (v,s) => 
		 let val s = sig_subst(s,subst)
		 in  PHRASE_CLASS_SIG(v,s)
		 end)

    fun con_exp_subst subst (c,e) = (con_subst(c,subst), exp_subst(e,subst))
    fun ovld_subst subst (OVLD (celist, default)) = OVLD (map (con_exp_subst subst) celist, default)

    (* alphaVarying a partial context WRT a context so that
       (1) the partial context's unresolved list maps to variables as bound by the full context
       (2) the partial context does not bind variables (at top-level) 
           that are bound (at top-level) of the full context
       If either condition is not already true, a substitution is produced and
          the substitution is applied to partial context.
    *)
    fun alphaVaryPartialContext 
	(CONTEXT{labelMap = lm1, pathMap = pm1, ...},
	 (CONTEXT{fixityMap = fm2, overloadMap = om2, labelMap = lm2, pathMap = pm2, ordering = ord2}, unresolved)) 
	: partial_context option = 
	let 
	    fun folder (vsubst,subst) (PATH p,(lm,pm,ord)) =
		let val (lab,pc) = (case Name.PathMap.find(pm2, p) of
					   SOME lab_pc => lab_pc
					 | NONE => (print "missing path ";
						    pp_path (PATH p); print "\n";
						    error "missing path"))
		    val (v,labs) = p
		    val v = (case VarMap.find(vsubst,v) of
				 NONE => v
			       | SOME v' => v')
		    val p = (v,labs)
		    val pc = pcSubst(pc,subst)
		    val lm = LabelMap.insert(lm,lab,(PATH p,pc))
		    val pm = Name.PathMap.insert(pm,p,(lab,pc))
		in (lm, pm, (PATH p)::ord)
		end
            (* ---- Using the current unresolved of the partial conetxt and the context,
	       ---- compute the substitution of the unresolved variables on the partial context
	       ---- to match the corresponding variables of the context and 
	       ---- generate a new unresolved list. *)
	    fun unresolvedFolder (v,l,(unresolved,vsubst)) =
		(case Name.LabelMap.find(lm1,l) of
		     SOME (PATH(v',[]), pc) =>
			 (VarMap.insert(unresolved, v', l),
			 if (eq_var(v,v'))
			     then vsubst
			 else Name.VarMap.insert(vsubst,v,v'))
		    | _ => error ("add_context could not resolve " ^ (Name.label2name l)))
	    val (unresolved,unresolvedVsubst) = Name.VarMap.foldli unresolvedFolder
		                                (VarMap.empty, VarMap.empty) unresolved
            (* ---- Compute the substitution to avoid clashes of bound variables ------- *)
	    fun clashFolder ((v,[]), (l, pc), vsubst) = 
		(case PathMap.find(pm1, (v, [])) of
		     NONE => vsubst
		   | SOME _ => let val v' = fresh_named_var "clash" (* derived_var v *)
			       in  VarMap.insert(vsubst,v,v')
			       end)
	      | clashFolder (_, _, vsubst) = vsubst
	    val clashVsubst = Name.PathMap.foldli clashFolder VarMap.empty pm2
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
		let val (labelMap,pathMap,ordering) = 
		    foldr (folder (final_vsubst,final_subst)) (LabelMap.empty,PathMap.empty,[]) ord2
		    val overloadMap = LabelMap.map (ovld_subst final_subst) om2
		in  SOME (CONTEXT{fixityMap = fm2,
				  overloadMap = overloadMap,
				  labelMap = labelMap,
				  pathMap = pathMap,
				  ordering = ordering}, 
			  unresolved)
		end 
	end

    fun pcAddFree(pc,set) = 
	(case pc of
	     PHRASE_CLASS_EXP (e,c,eopt,_) => 
		 let val set = VarSet.union(set, exp_free e)
		     val set = VarSet.union(set, con_free c) 
		 in  (case eopt of
			  NONE => set
			| SOME e => VarSet.union(set, exp_free e))
		 end
	   | PHRASE_CLASS_CON (c,_,copt,_) => 
		let val set = VarSet.union(set, con_free c) 
		in (case copt of
			NONE => set
		      | SOME c => VarSet.union(set, con_free c))
		end
	   | PHRASE_CLASS_MOD (m,_,s) => 
		let val set = VarSet.union(set, mod_free m) 
		in  VarSet.union(set, sig_free s)
		end
	   | PHRASE_CLASS_SIG (_, s) => VarSet.union(set, sig_free s))


    (* adding the partial context to the context *)
    fun plus_context (ctxt as CONTEXT{fixityMap = fm1, overloadMap = om1,
				      labelMap = lm1, pathMap = pm1, ordering = ord1},
		      orig_pctxt : partial_context)
	: partial_context option * context = 
	let
	    val _ = debugdo (fn () => (print "plus PARTIAL CONTEXT:\n";
				       pp_pcontext orig_pctxt;
				       print "\n\n"))
	    val pctxt_option = alphaVaryPartialContext(ctxt, orig_pctxt)
	    val (CONTEXT{fixityMap = fm2, overloadMap = om2,
			 labelMap = lm2, pathMap = pm2, ordering = ord2}, 
		 unresolved) = (case pctxt_option of
				    NONE => orig_pctxt (* alpha-varying not needed *)
				  | SOME pctxt => pctxt)
	    val fixityMap = LabelMap.unionWithi
		            (fn (l, _, _) => error ("fixityMap not disjoint at " ^ (label2name l)))
			    (fm1,fm2)
	    fun is_open_internal_path (_,PATH(v,[])) = false
	      | is_open_internal_path (pathmap,PATH(v,labs)) = 
		(case labs of
		     [lab] => (case PathMap.find(pathmap,(v,[])) of
				   NONE => (print "!!!Could not find "; pp_var v; print ": probably bug with SplayMapFn.unionWithi\n"; 
					    false)
				 | SOME (l,_) => is_open l andalso is_label_internal lab)
		   | _ => let val len = length labs
			  in is_open (List.nth(labs, len - 2)) andalso is_label_internal (List.nth(labs, len - 1))
			  end)
	    val labelMap = LabelMap.unionWithi 
		            (fn (l,(p1,pc1),second as (p2,pc2)) => 
			     if (is_open_internal_path(pm1,p1) orelse
				 is_open_internal_path(pm2,p2) orelse
				 is_open_internal_path(pm1,p2) orelse   (* last two are needed only if there's a bug in 
									   SplayMapFn.unionWithi *)
				 is_open_internal_path(pm2,p1))
				 then second
			     else (print "p1 = "; pp_path p1; print " :\n";
				   pp_phrase_class pc1; print "\n\n";
				   print "p2 = "; pp_path p2; print " :\n";
				   pp_phrase_class pc2; print "\n\n";
				   error ("labelMap not disjoint at " ^ (label2name l))))
			     (lm1,lm2)
	    val pathMap = PathMap.unionWithi 
			    (fn (p,(l,_),(l',_)) => (print "pathMap not disjoint at label ";
						     Ppil.pp_label l;
						     print " and label' ";
						     Ppil.pp_label l';
						     print " and path ";
						     Ppil.pp_path (PATH p);
						     print "\n";
						     error "pathMap not disjoint"))
			     (pm1,pm2)
	    (* orderings are backwards *)
	    val ordering = ord2 @ ord1
	    val almostFinalContext = CONTEXT{fixityMap = fixityMap,
					     overloadMap = Name.LabelMap.empty,
					     labelMap = labelMap,
					     pathMap = pathMap,
					     ordering = ordering}
	    val overloadMap = LabelMap.unionWithi
                     	      (fn (l, ovld1, ovld2) =>
			       (
(*
				print "almostFinalContext is ";
				pp_context almostFinalContext;
				print "\n\n\n";
				print "ovld1 is "; pp_ovld ovld1; print "\n";
				print "ovld2 is "; pp_ovld ovld2; print "\n";
*)
				ovld_add(l,almostFinalContext,ovld1,ovld2)))
			      (om1,om2)
	in  (pctxt_option,
	     CONTEXT{fixityMap = fixityMap,
		     overloadMap = overloadMap,
		     labelMap = labelMap,
		     pathMap = pathMap,
		     ordering = ordering})
	end 
(*
    fun plus_context (ctxt, partial_ctxts) 
	: partial_context option list * context = 
	(debugdo (fn() => (print "plus CONTEXT:\n";
			   pp_context ctxt;
			   print "\n\n"));
	 foldl_list plus ctxt partial_ctxts)
*)  
    fun show_varset str set = 
	(print str; print ": ";
	 VarSet.app (fn v => (Ppil.pp_var v; print "  ")) set;
	 print "\n")

    (* The context has cycles because of selfification *)
    fun reachableVars(pathMap, black, gray) = 
	if (VarSet.isEmpty gray)
	    then black
	else 
	    let 
(*		val _ = show_varset "Gray" gray *)
		val black = VarSet.union(black, gray)
		fun folder (v,s) = 
		    (case PathMap.find(pathMap,(v,[])) of
			 SOME (_, pc) => pcAddFree(pc,s)
		       | _ => s)
(* error ("reachableVars could not find free variable " ^ (Name.var2string v)) *)
		val freeVars = VarSet.foldl folder VarSet.empty gray  
		fun pred v = (case PathMap.find(pathMap,(v,[])) of
				  SOME _ => true
				| _ => false)
		val temp = VarSet.filter pred freeVars         (* one step from old gray *)
(*		val _ = show_varset "temp" temp *)
		val gray = VarSet.difference(temp, black)      (* there might be loops *)
	    in  reachableVars(pathMap,black,gray)
	    end

    fun sub_context (bigger : Il.context, smaller : Il.context) : Il.partial_context = 
	let 
	    val CONTEXT{fixityMap=f1, overloadMap=om1, labelMap=l1, pathMap=p1, ordering=o1} = bigger
	    val CONTEXT{fixityMap=f2, overloadMap=om2, labelMap=l2, pathMap=p2, ordering=o2} = smaller
	    val f3 = LabelMap.filteri (fn (l1,_) => (case LabelMap.find(f2,l1) of
							 NONE => true
						       | SOME _ => false)) f1
	    val om3 = LabelMap.mapi (fn (l1, ovld1) => (case LabelMap.find(om2, l1) of
							    NONE => ovld1
							  | SOME ovld2 => ovld_sub(bigger,ovld1,ovld2))) om1
	    val om3 = LabelMap.filteri (fn (l3, OVLD ([],_)) => false
	                                 | _ => true) om3
	    val o3 = List.filter (fn PATH vpath => (case PathMap.find(p2,vpath) of
							NONE => true
						      | SOME _ => false)) o1
	    val l3 = LabelMap.filteri (fn (l,_) => 
					    (case LabelMap.find(l2,l) of
						 NONE => true
					       | SOME _ => false)) l1
	    val p3 = PathMap.filteri (fn (p,_) => 
					   (case PathMap.find(p2,p) of
						NONE => true
					      | SOME _ => false)) p1
	    val diff = CONTEXT{fixityMap = f3, overloadMap = om3,
			       labelMap = l3, pathMap = p3, ordering = o3} 
	    val roots = PathMap.foldli 
		          (fn ((v,[]),_,s) => VarSet.add(s,v)
			    | (_,_,s) => s)
		          VarSet.empty p3
	    val reachable = LabelMap.foldl
		           (fn (OVLD (celist,_),reach) =>
			      foldl (fn ((c,e),reach) => 
				     let val cFree = con_free c
					 val eFree = exp_free e
				     in  VarSet.union(VarSet.union(reach,cFree),eFree)
				     end) reach celist)
			   VarSet.empty om3
	    val reachable = reachableVars(p1, reachable, roots)
	    val keepVars = VarSet.difference(reachable, roots)
	    val unresolved = VarSet.foldl
		                  (fn (v,vm) => 
				   (case PathMap.find(p1, (v,[])) of
					SOME (l, _) => VarMap.insert(vm,v,l)
				      | NONE => error "Missing variable in sub_context"))
				  Name.VarMap.empty keepVars
(*
	    val _ = print ("sub_context: " ^
			   Int.toString(PathMap.numItems p2) ^
			   " items in smaller context. " ^ 
			   (Int.toString (VarMap.numItems unresolved))
			   ^ " items in unresolved of smaller context.\n")
*)
	in  (diff, unresolved)
	end

    fun gc_context ((context, (CONTEXT{pathMap,...}, _), sbndopt_entry) : Il.module) : Il.context = 
	let val exclude = PathMap.foldli
		          (fn ((v,[]),_,s) => VarSet.add(s,v)
			    | (_,_,s) => s)
		          VarSet.empty pathMap
	    val roots = PathMap.foldli
		          (fn ((v,[]),(_,pc),s) => pcAddFree(pc,s)
			    | (_,_,s) => s)
		          VarSet.empty pathMap
	    fun folder ((sbndopt,entry),roots) = 
		let val roots = (case sbndopt of
				     NONE => roots
				   | SOME sbnd => VarSet.union(roots, sbnd_free sbnd))
		in  VarSet.union(roots, entry_free entry)
		end
	    val roots = foldl folder roots sbndopt_entry
	    val roots = VarSet.difference(roots, exclude)
	    val CONTEXT{pathMap, labelMap, ordering, overloadMap, fixityMap} = context
	    val roots = PathMap.foldli
		          (fn ((v,[]),(l,_),s) => if Name.keep_import l
						      then VarSet.add(s,v)
						  else s
			    | (_,_,s) => s)
			  roots pathMap
	    val reachable = reachableVars(pathMap, VarSet.empty, roots)
	    fun isReachable (v,_) = Name.VarSet.member(reachable,v)
	    val pathMap_orig = pathMap (* used for the print statements below *)
	    val pathMap = PathMap.filteri (fn (p,_) => isReachable p) pathMap_orig
	    val labelMap = LabelMap.filteri (fn (_, (PATH p, _)) => isReachable p) labelMap
	    val ordering = List.filter (fn PATH p => isReachable p) ordering
	    val _ = print ("gc_context: " ^
			   Int.toString(PathMap.numItems pathMap_orig) ^
			   " items in original context.  " ^ 
			   (Int.toString (PathMap.numItems pathMap))
			   ^ " items in reduced context.\n")
	in  CONTEXT{pathMap = pathMap,
		    overloadMap = overloadMap,
		    labelMap = labelMap,
		    ordering = ordering,
		    fixityMap = fixityMap}
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
	(case Name.LabelMap.find (li, l)
	   of NONE => (debugdo (fn () =>
				(print "map_labels: "; pp_label l;
				 print " -> "; pp_label l; print "\n"));
		       l)
	    | SOME l' => (debugdo (fn () =>
				   (print "map_labels: "; pp_label l;
				    print " -> "; pp_label l'; print "\n"));
			  l'))
    fun map_context_labels (CONTEXT{pathMap, overloadMap, labelMap, ordering, fixityMap}, li) =
	let val lookup = map_labels li
	    fun folder (l, v, map) = Name.LabelMap.insert (map, lookup l, v)
	    fun changeDomain map = Name.LabelMap.foldli folder Name.LabelMap.empty map
	    val pathMap = Name.PathMap.map (fn (l,v) => (lookup l, v)) pathMap
	    val overloadMap = changeDomain overloadMap
	    val labelMap = changeDomain labelMap
	    val fixityMap = changeDomain fixityMap
	in  CONTEXT{pathMap = pathMap,
		    overloadMap = overloadMap,
		    labelMap = labelMap,
		    ordering = ordering,
		    fixityMap = fixityMap}
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

    fun removeNonExport (CONTEXT{pathMap, overloadMap, labelMap, ordering, fixityMap}, frees) = 
	let val ordering = List.filter (fn PATH p => let val SOME(l,_) = Name.PathMap.find(pathMap,p)
						in  not (is_nonexport l)
						end) ordering
	    val pathMap = Name.PathMap.filteri (fn (_,(l,_)) => not(is_nonexport l)) pathMap
	    val labelMap = Name.LabelMap.filteri (fn (l,_) => not(is_nonexport l)) labelMap
	    val frees = Name.VarMap.filteri (fn (_, l) => not(is_nonexport l)) frees
	in  (CONTEXT{pathMap = pathMap,
		    overloadMap = overloadMap,
		    labelMap = labelMap,
		    ordering = ordering,
		    fixityMap = fixityMap},
	     frees)
	end


   (* --- remove references to internal variables from signature with given module ---- *)
   local
       type state = {selfify : bool,
		     ctxt : context,
		     subst : subst}

       fun initial_state (selfify,ctxt) = {selfify = selfify, 
					   ctxt = ctxt,
					   subst = empty_subst}

       fun isempty_state ({subst,...}:state) = subst_is_empty subst

       fun add_exp (state, _, NONE) = state
	 | add_exp ({ctxt, selfify, subst}, v, SOME p) =
	   {ctxt = ctxt, selfify = selfify,
	    subst = if selfify then subst_add_expvar(subst, v, path2exp p)
		    else subst_add_exppath(subst, p, VAR v)}

       fun add_con (state, _, NONE) = state
	 | add_con ({ctxt, selfify, subst}, v, SOME p) = 
	   {ctxt = ctxt, selfify = selfify,
	    subst = if selfify 
			then subst_add_convar(subst, v, path2con p)
		    else subst_add_conpath(subst, p, CON_VAR v)}
	   
       fun add_mod ({ctxt, selfify, subst}, v, s, pathOpt) = 
	   {ctxt = if selfify then add_context_mod'(ctxt, v, s) else ctxt,
	    selfify = selfify,
	    subst = (case pathOpt of
			 NONE => subst
		       | SOME p => if selfify 
				       then subst_add_modvar(subst, v, path2mod p)
				   else subst_add_modpath(subst, p, MOD_VAR v))}

       fun sdec_folder popt (sdec as (SDEC(l,dec)), state as {selfify,subst,...} : state) =
	   let val popt = mapopt (fn p => join_path_labels(p,[l])) popt
	   val res = (case dec of
		    DEC_EXP(v,c,eopt,inline) => 
			let val c = con_subst(c,subst)
			    val eopt = (case eopt of
					    NONE => NONE
					  | SOME e => SOME(exp_subst(e,subst)))
			    val dec = DEC_EXP(v,c,eopt,inline)
			    val state = if is_coercion l then add_exp(state,v,popt) else state
			in  (SDEC(l,dec), state)
			end
		  | DEC_CON(v,k,copt,inline) => 
		     let 
			 val copt = 
			     (case copt of  (*  if we do this, Unselfify is hard to write. *)
				  NONE => (case popt of
					       NONE => NONE
					     | SOME p => SOME(path2con p))
				| SOME c => 
				      let val c' = con_subst(c,subst)
				      in  (case (selfify, popt) of
					       (false, SOME p) => if eq_cpath(c', path2con p)
								      then NONE
								  else SOME c'
					     | _ => SOME c')
				      end)
			 val dec = DEC_CON(v,k,copt,inline)
			 val state = add_con(state,v,popt)
		     in (SDEC(l,dec), state)
		     end
		  | DEC_MOD(v,b,s) => 
		     let val s' = TransformSig state (popt,s)
			 val dec = DEC_MOD(v,b,s')
			 val state = add_mod(state,v,s',popt)
		     in (SDEC(l,dec), state)
		     end)
	   in  res
	   end
		    
       and TransformSig (state as {selfify,subst,ctxt}) (self: path option, signat : signat) : signat = 
	   (case (selfify, signat) of
		(true, SIGNAT_SELF (p, unselfSigOpt, selfSig)) => 
		    if (case self of
			    SOME self => eq_path(p,self)
			  | _ => false)
			then signat
		    else (* We must traverse selfSig to perform the rest of the substitution even
			    though there are already no internal variable use from within selfSig.
			  *)
			let val selfSig = TransformSig state (NONE, selfSig)
			in  SIGNAT_SELF(p, NONE, selfSig)
			end
	      | (false, SIGNAT_SELF(_, SOME unselfSig, _)) => unselfSig
	      | (false, SIGNAT_SELF(_, NONE, selfSig)) => TransformSig state (self, selfSig)
	      | (true, SIGNAT_VAR v) => let val expanded = reduce_sigvar ctxt v
					    val SIGNAT_SELF(p, _, selfSig) = TransformSig state (self, expanded)
					in  SIGNAT_SELF(p, SOME signat, selfSig)
					end
	      | (false, SIGNAT_VAR v) => signat
	      | (_, SIGNAT_OF p) => let val m = path2mod p
					val m = mod_subst(m, subst)
				    in  (case (selfify, mod2path m) of
					     (false, NONE) => signat
					   | (false, SOME p) => SIGNAT_OF p
					   | (true, NONE) => error "SelfifySig got non-path from SIGNAT_OF"
					   | (true, SOME p) => let val s = SIGNAT_OF p
								   val sFull = reduce_signat ctxt signat
								   val SIGNAT_SELF(_,_,sFull) = TransformSig state (self, sFull)
							       in  (case self of
									NONE => s 
								      | SOME self => SIGNAT_SELF(self, SOME s, sFull))
							       end)
				    end
	      | (_, SIGNAT_FUNCTOR (v,s1,s2,a)) => 
				    let val s1 = if (isempty_state state) then s1 else TransformSig state (NONE,s1)
					val s2 = if (isempty_state state) then s2 else TransformSig state (NONE,s2)
					val s = SIGNAT_FUNCTOR(v,s1,s2,a)
					val res = (case (selfify,self) of
					     (true, SOME self) => SIGNAT_SELF(self, NONE, s)
					   | _ => s)
				    in  res
				    end
	      | (_, SIGNAT_STRUCTURE sdecs) =>
				    let val (sdecs,_) = foldl_acc (sdec_folder self) state sdecs
					val s = SIGNAT_STRUCTURE sdecs
				    in  (case (selfify, self) of
					     (true, SOME self) => SIGNAT_SELF(self, NONE, s)
					   | _ => s)
				    end)


   in

       fun UnselfifySig ctxt (p : path, signat : signat) = 
	   TransformSig (initial_state (false, ctxt)) (SOME p,signat)
	   
       fun SelfifySig ctxt (p : path, signat : signat) = 
	   TransformSig (initial_state (true, ctxt)) (SOME p,signat)
	   handle exn =>
	       (debugdo (fn () =>
	       (print "SelfifySig encountered error while selfifying with p = "; pp_path p;
		print " and signat =\n";
		pp_signat signat; print "\n\n";
		          print " and ctxt =\n"; pp_context ctxt; print "\n\n"));
		raise exn)

       fun SelfifyDec ctxt (DEC_MOD (v,b,s)) = DEC_MOD(v,b,SelfifySig ctxt (PATH(v,[]),s))
	 | SelfifyDec ctxt dec = dec
       fun SelfifySdec ctxt (SDEC (l,dec)) = SDEC(l,SelfifyDec ctxt dec)
       fun SelfifySdecs ctxt (p : path, sdecs : sdecs) =
	   case (SelfifySig ctxt (p,SIGNAT_STRUCTURE sdecs)) of
	       SIGNAT_SELF(_, _, SIGNAT_STRUCTURE sdecs) => sdecs
	     | _ => error "SelfifySdecs: SelfifySig returned non-normal structure"
       fun SelfifyEntry ctxt (CONTEXT_SDEC sdec) = CONTEXT_SDEC(SelfifySdec ctxt sdec)
	 | SelfifyEntry ctxt ce = ce
	   
   end

   fun UnselfifyContext (ctxt as (CONTEXT {pathMap, overloadMap, labelMap, ordering, fixityMap})) = 
       let fun unselfifyPC (p,pc) = 
	   (case pc of
		PHRASE_CLASS_MOD (m, b, s) => PHRASE_CLASS_MOD(m, b, UnselfifySig ctxt (PATH p, s))
	     | _ => pc)
	   fun unselfifyPathmapEntry (p,(l, pc)) = (l, unselfifyPC (p, pc))
	   val pathMap = Name.PathMap.mapi unselfifyPathmapEntry pathMap
       in  CONTEXT{pathMap = pathMap,
		   overloadMap = overloadMap,
		   labelMap = labelMap,
		   ordering = ordering,
		   fixityMap = fixityMap}
       end
   fun UnselfifyPartialContext (ctxt, free) = (UnselfifyContext ctxt, free)

end

