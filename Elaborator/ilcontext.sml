(*$import ILCONTEXT Util Listops Name Ppil Stats IlUtil *)
structure IlContext :> ILCONTEXT =
struct

    structure Il = Il
    type context = Il.context
    type exp = Il.exp
    type con = Il.con
    type kind = Il.kind
    type mod = Il.mod
    type signat = Il.signat
    type label = Il.label
    type var = Il.var
    type tag = Name.tag
    type sdec = Il.sdec
    type sdecs = Il.sdecs
    type path = Il.path
    type context_entry = Il.context_entry
	
    open Il Util Name Listops Ppil IlUtil
	
    val error = fn s => error "ilcontext.sml" s
	
    val debug = Stats.ff("IlcontextDebug")
    fun debugdo t = if (!debug) then (t(); ()) else ()


    (* --------------- BASICS ------------------------------- *)
    type var_seq_map = (label * phrase_class) VarMap.map * var list
    fun var_seq_insert ((m,s),v,value) = (VarMap.insert(m,v,value),v::s)
    val empty_context = CONTEXT{fixityMap = LabelMap.empty,
				labelMap = LabelMap.empty,
				pathMap = PathMap.empty,
				ordering = []}

    (* ---------------- LOOKUP RULES --------------------------- 
     The lookup rules can return item from different grammatical classes
     so we need some additional datatypes to package up the results 
     --------------------------------------------------------- *)

    type phrase_class_p = path * phrase_class


    fun Context_Fixity (CONTEXT {fixityMap,...}) = fixityMap
    fun Context_Ordering (CONTEXT {ordering,...}) = rev ordering
    fun Context_Lookup_Label (CONTEXT {labelMap, ...}, lab) = LabelMap.find(labelMap, lab)
    fun Context_Lookup_Path (CONTEXT {pathMap, ...}, PATH path) = PathMap.find(pathMap, path)
    fun Context_Lookup_Var (CONTEXT {pathMap, ...}, v) = PathMap.find(pathMap, (v,[]))


    (* --------------------- EXTENDERS ---------------------------------------- *)
    fun add_context_fixity(CONTEXT {fixityMap, labelMap, pathMap, ordering}, label, fixity) = 
	CONTEXT({fixityMap = LabelMap.insert(fixityMap,label,fixity),
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
			 | SOME _ => error ("SIGNAT_OF(" ^ (Name.var2string v) ^ ",...) unbound"))
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

    fun shadow (labelMap, pathMap, lab) = 
	(case (LabelMap.find(labelMap,lab) : (path * phrase_class) option) of
	     NONE => (labelMap, pathMap)
	   | SOME (PATH p,_) => 
		 let val newlab = fresh_internal_label(label2name lab)
		 in  (case PathMap.find(pathMap,p) of
			  NONE => error "inconsistent context"
			| SOME (_, pc) => 
			      let val pathMap = PathMap.insert(pathMap,p,(newlab, pc))
				  val (labelMap,_) = LabelMap.remove(labelMap, lab)
				  val labelMap = LabelMap.insert(labelMap, newlab,(PATH p, pc))
			      in  (labelMap, pathMap)
			      end)
		 end)
	     
    fun add_sdec(ctxt, pathopt, sdec as (SDEC(l,dec))) = 
	let fun mk_path v = (case pathopt of
				 NONE => PATH(v,[])
			       | SOME p => join_path_labels(p,[l]))
	    fun help(CONTEXT {fixityMap, labelMap, pathMap, ordering},
		     v, from_path, pc_maker) =
		let val path = mk_path v
		    val obj = from_path path
		    val pc = pc_maker obj
		    val PATH vpath = path
		    val (labelMap, pathMap) = shadow(labelMap, pathMap, l)
		    val labelMap = LabelMap.insert(labelMap,l,(path,pc))
		    val pathMap = PathMap.insert(pathMap,vpath,(l,pc))
		    val ordering = path :: ordering
		in CONTEXT{fixityMap = fixityMap,
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



    fun add_context_sig(CONTEXT {fixityMap, labelMap, pathMap, ordering},
			l, v, signat) = 
	let val pc = PHRASE_CLASS_SIG(v,signat)
	    val path = (v,[])
	    val (labelMap, pathMap) = shadow(labelMap, pathMap, l)
	in  CONTEXT({fixityMap = fixityMap,
		     labelMap = Name.LabelMap.insert(labelMap,l,(PATH path, pc)),
		     pathMap = Name.PathMap.insert(pathMap,path,(l, pc)),
		     ordering = (PATH path)::ordering})
	end

    fun add_context_overexp(CONTEXT {fixityMap, labelMap, pathMap, ordering},
			    l, v, celist) = 
	let val pc = PHRASE_CLASS_OVEREXP celist
	    val path = (v,[])
	    val (labelMap, pathMap) = shadow(labelMap, pathMap, l)
	in  CONTEXT({fixityMap = fixityMap,
		     labelMap = Name.LabelMap.insert(labelMap,l,(PATH path, pc)),
		     pathMap = Name.PathMap.insert(pathMap,path,(l,pc)),
		     ordering = (PATH path)::ordering})
	end

    fun add_context_entry(ctxt, entry) = 
	(case entry of
	     CONTEXT_FIXITY (l,f) => add_context_fixity(ctxt,l,f)
	   | CONTEXT_SDEC sdec => add_sdec(ctxt,NONE,sdec)
	   | CONTEXT_SIGNAT (l,v,s) => add_context_sig(ctxt,l,v,s)
	   | CONTEXT_OVEREXP(l,v,celist) => add_context_overexp(ctxt,l,v,celist))

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
	



    (* adding the partial context to the context *)
    fun plus (ctxt as CONTEXT{fixityMap = fl1, labelMap = lm1, pathMap = pm1, ordering = ord1},
	      (CONTEXT{fixityMap = fl2, labelMap = lm2, pathMap = pm2, ordering = ord2}, unresolved),
	      used) =
	let 
	    fun folder(PATH p,(lm,pm,ord,subst)) =
		let val (lab,pc) = (case Name.PathMap.find(pm2, p) of
					   SOME lab_pc => lab_pc
					 | NONE => (print "missing path ";
						    pp_path (PATH p); print "\n";
						    error "missing path"))
		    val in_current = (case (Name.PathMap.find(pm, p)) of
					  NONE => false
					| SOME _ => true)
		    val (p,subst) = 
			if in_current 
			    then let val (v,labs) = p
				     val v' = Name.derived_var v
				     val p' = (v',labs)
				     val subst = subst_add_expvar(subst,v,VAR v')
				     val subst = subst_add_convar(subst,v,CON_VAR v')
				     val subst = subst_add_modvar(subst,v,MOD_VAR v')
				     val subst = subst_add_sigvar(subst,v,v')
				 in  (p',subst)
				 end
			else (p, subst)
		    val pc = 
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
				 end
			   | PHRASE_CLASS_OVEREXP celist => 
				 let val celist = map (fn (c,e) => (con_subst(c,subst),
								    exp_subst(e,subst))) celist
				 in  PHRASE_CLASS_OVEREXP celist
				 end)
		    val lm = LabelMap.insert(lm,lab,(PATH p,pc))
		    val pm = Name.PathMap.insert(pm,p,(lab,pc))
		    val ord = (PATH p) :: ord
		in (lm, pm, ord, subst)
		end
	    val fixityMap = LabelMap.unionWith (fn (first,second) => second) (fl1,fl2)
            (* The initial substitution comes from unresolved *)
	    val subst = Name.VarMap.foldli 
		(fn (v,l,subst) => 
		 (case Name.LabelMap.find(lm1,l) of
		      SOME (PATH(v',[]), pc) =>
			  if (eq_var (v,v'))
			      then subst
			  else (case pc of
				    PHRASE_CLASS_EXP _ => subst_add_expvar(subst,v,VAR v')
				  | PHRASE_CLASS_CON _ => subst_add_convar(subst,v,CON_VAR v')
				  | PHRASE_CLASS_MOD _ => subst_add_modvar(subst,v,MOD_VAR v')
				  | PHRASE_CLASS_SIG _ => subst_add_sigvar(subst,v,v')
				  | PHRASE_CLASS_OVEREXP _ => subst_add_expvar(subst,v,VAR v'))
		    | _ => error ("add_context could not resolve " ^ (Name.label2name l))))
		 empty_subst unresolved
	    (* use foldr since ord's are backwards *)
	    val (labelMap,pathMap,ordering,_) = foldr folder (lm1,pm1,ord1,subst) ord2
	in  (used,
	     CONTEXT{fixityMap = fixityMap,
		    labelMap = labelMap,
		    pathMap = pathMap,
		    ordering = ordering})
	end 

      fun plus_context (ctxt, partial_ctxts) = 
	  let fun loop (ctxt,[],used) = ctxt
		| loop (ctxt,pctxt::rest,used) = 
	            let val (used,ctxt) = plus(ctxt,pctxt,used)
		    in  loop (ctxt, rest, used)
		    end
	  in  loop (ctxt,partial_ctxts,Name.VarSet.empty)
	  end

(*
      fun check_context (CONTEXT{fixityMap, labelMap, pathMap, ordering}) = 
	  let val _ = (Name.PathMap.appi 
		       (fn (p,(l,_)) => 
			    (case Name.LabelMap.find(labelMap,l) of
				 NONE => error "check_context: missing label in pathmap"
			       | SOME (p',_) => if (eq_path(PATH p,p'))
						    then ()
						else (print "check_context: mismatch path: ";
						      pp_path (PATH p); print " != ";
						      pp_path p'; print "\n";
						      error "check_context: mismatch path")))
		       pathMap)
	      val _ = (Name.LabelMap.appi 
		       (fn (l,(PATH p,_)) => 
			    (case Name.PathMap.find(pathMap,p) of
				 NONE => error "check_context: missing path in labelmap"
			       | SOME (l',_) => if (eq_label(l,l'))
						    then ()
						else error "check_context: mismatch label"))
		       labelMap)
	  in  ()
	  end
*)

      fun sub_context (bigger : Il.context, smaller : Il.context) : Il.partial_context = 
	  let 
(*	      val _ = check_context bigger 
	      handle e => (print "bigger context failed consistency: ";
			   Ppil.pp_context bigger;
			   raise e)
	      val _ = check_context smaller
	      handle e => (print "smaller context failed consistency: ";
			   Ppil.pp_context smaller;
			   raise e)
*)
	      val CONTEXT{fixityMap=f1, labelMap=l1, pathMap=p1, ordering=o1} = bigger
	      val CONTEXT{fixityMap=f2, labelMap=l2, pathMap=p2, ordering=o2} = smaller
	      val f3 = LabelMap.filteri (fn (l1,_) => (case LabelMap.find(f2,l1) of
							   NONE => true
							 | SOME _ => false)) f1
	      val o3 = List.filter (fn PATH vpath => (case Name.PathMap.find(p2,vpath) of
							  NONE => true
							| SOME _ => false)) o1
	      val l3 = Name.LabelMap.filteri (fn (l,_) => 
					      (case Name.LabelMap.find(l2,l) of
						   NONE => true
						 | SOME _ => false)) l1
	      val p3 = Name.PathMap.filteri (fn (p,_) => 
					     (case Name.PathMap.find(p2,p) of
						  NONE => true
						| SOME _ => false)) p1
	      val diff = CONTEXT{fixityMap = f3, labelMap = l3, pathMap = p3, ordering = o3} 
	      (* Could be more clever and figure out only what is needed *)
	      val unresolved = Name.PathMap.foldli 
		                  (fn ((v,[]),(l,_),vm) => Name.VarMap.insert(vm,v,l)
				    | (_,_,vm) => vm) Name.VarMap.empty p2
	  in  (diff, unresolved)
	  end

end

