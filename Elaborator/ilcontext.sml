(*$import ILCONTEXT Util Listops Name Ppil Stats IlUtil *)
structure IlContext :> ILCONTEXT =
struct

    structure Il = Il
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

    (* ------ Type equivalence needed to ensure overloading resolvable by distnict types ------- *)
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
	fun eq_conexp ctxt ((c1,e1),(c2,e2)) = eq_con(ctxt,c1,c2)
	fun ce_sub(ctxt, ce1, ce2) = Listops.list_diff_eq(eq_conexp ctxt, ce1, ce2)
	fun ce_add(ctxt, ce1, ce2) = Listops.list_sum_eq(eq_conexp ctxt, ce1, ce2)
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

    fun reduce_signat ctxt (SIGNAT_VAR v) = 
	(case (Context_Lookup_Var(ctxt,v)) of
	     SOME(_,PHRASE_CLASS_SIG(v,s)) => s
	   | _ => error "reduce_signat found unbound sigvar")
      | reduce_signat ctxt (SIGNAT_OF (PATH(v,labs))) = 
	  let val s = (case Context_Lookup_Var(ctxt,v) of
			   SOME(_,PHRASE_CLASS_MOD(_,_,s)) => s
			 | _ => error ("SIGNAT_OF(" ^ (Name.var2string v) ^ ",...) unbound"))
	      fun find l [] = error "cannot find label in SIGNAT_OF"
		| find l ((SDEC(l',DEC_MOD(_,_,s)))::rest) = s
		| find l (_::rest) = find l rest
	      fun project (l, s) = 
		  (case (reduce_signat ctxt s) of
		       SIGNAT_STRUCTURE(SOME _, sdecs) => find l sdecs
		     | SIGNAT_STRUCTURE _ => error "unselfified signatture in context"
		     | _ => error "ill-formed SIGNAT_OF")
	  in  foldl project s labs
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
		       let val newlab = fresh_internal_label(label2name lab)
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
	  | DEC_MOD (v,_,(SIGNAT_STRUCTURE(NONE,_))) => 
	    (print "adding non-selfified signature to context: "; pp_sdec sdec; print "\n";
	     error "adding non-selfified signature to context")
	  | DEC_MOD(v,b,s) =>
		  let val ctxt = help(ctxt, v, path2mod, fn obj => (PHRASE_CLASS_MOD(obj, b,s)))
		  in  if (is_open l) 
			  then let val sdecs = 
			      (case s of
				   SIGNAT_STRUCTURE(SOME p, sdecs) => sdecs
				 | _ => (case (reduce_signat ctxt s) of
					     SIGNAT_STRUCTURE(SOME p, sdecs) => sdecs
					   | _ => error "open label - not self, struct sig"))
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
			    l, ce2) = 
	let val ce1 = (case Name.LabelMap.find(overloadMap, l) of
			   SOME ce => ce
			 | NONE => [])
	    val ce3 = ce_add(ctxt,ce1,ce2)
	    val overloadMap = LabelMap.insert(overloadMap, l, ce3)
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
	   | CONTEXT_OVEREXP(l,celist) => add_context_overexp(ctxt,l,celist))

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

    fun con_exp_list_subst subst con_exp_list = 
	map (fn (c,e) => (con_subst(c,subst), exp_subst(e,subst))) con_exp_list

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
		    val overloadMap = LabelMap.map (con_exp_list_subst final_subst) om2
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
		     val set = VarSet.union(set ,con_free c) 
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

    fun check_pathmap pathmap = 
	PathMap.appi (fn (p as (v,labs), _) =>
		      (case PathMap.find(pathmap,(v,[])) of
			   NONE => (print "bad path since variable is not in pathmap: "; pp_path (PATH p); 
				    print "\n")
			 | SOME _ => ()))
	pathmap


    (* adding the partial context to the context *)
    fun plus (orig_pctxt : partial_context,
	      ctxt as CONTEXT{fixityMap = fm1, overloadMap = om1,
			      labelMap = lm1, pathMap = pm1, ordering = ord1})
	: partial_context option * context = 
	let
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
	    val _ = print "checking pathmap1\n"
	    val _ = check_pathmap pm1
	    val _ = print "pathmap1 ok\n"
	    val _ = print "checking pathmap2\n"
	    val _ = check_pathmap pm2
	    val _ = print "pathmap2 ok\n"
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
                     	      (fn (l, ce1, ce2) => 
			       (
(*
				print "almostFinalContext is ";
				pp_context almostFinalContext;
				print "\n\n\n";
				print "ce1 is "; app (fn (c,e) => (pp_exp e; print ":"; pp_con c; print "\n")) ce1; print "\n";
				print "ce2 is "; app (fn (c,e) => (pp_exp e; print ":"; pp_con c; print "\n")) ce2; print "\n";
*)				ce_add(almostFinalContext,ce1, ce2)))
			      (om1,om2)
	in  (pctxt_option,
	     CONTEXT{fixityMap = fixityMap,
		     overloadMap = overloadMap,
		     labelMap = labelMap,
		     pathMap = pathMap,
		     ordering = ordering})
	end 

    fun plus_context (ctxt, partial_ctxts) 
	: partial_context option list * context = 
	foldl_list plus ctxt partial_ctxts
    
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
	    val om3 = LabelMap.mapi (fn (l1, ce1) => (case LabelMap.find(om2, l1) of
							  NONE => ce1
							| SOME ce2 => ce_sub(bigger,ce1,ce2))) om1
	    val om3 = LabelMap.filteri (fn (l3, []) => false
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
		           (fn (celist,reach) =>
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
	    val _ = print "checking sub_context\n"
	    val _ = check_pathmap p3
	    val _ = print "sub_context ok\n"
	    val _ = print ("sub_context: " ^
			   Int.toString(PathMap.numItems p2) ^
			   " items in smaller context. " ^ 
			   (Int.toString (VarMap.numItems unresolved))
			   ^ " items in unresolved of smaller context.\n")
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
end

