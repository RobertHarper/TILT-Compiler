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
    type fixity_table = Il.fixity_table
    type path = Il.path
    type context_entry = Il.context_entry
	
    open Il Util Name Listops Ppil IlUtil
	
    val error = fn s => error "ilcontext.sml" s
	
    val debug = Stats.ff("IlcontextDebug")
    fun debugdo t = if (!debug) then (t(); ()) else ()


    (* --------------------- EXTENDERS ---------------------------------------- *)
    type var_seq_map = (label * phrase_class) VarMap.map * var list
    fun var_seq_insert ((m,s),v,value) = (VarMap.insert(m,v,value),v::s)
    val empty_context = CONTEXT{fixityList = [],
				labelMap = LabelMap.empty,
				pathMap = PathMap.empty,
				ordering = []}


    fun add_context_fixity(CONTEXT {fixityList, labelMap, pathMap, ordering}, f) = 
	CONTEXT({fixityList = f @ fixityList,
		 labelMap = labelMap,
		 pathMap = pathMap,
		 ordering = ordering})

    fun add_sdec(ctxt, pathopt, sdec as (SDEC(l,dec))) = 
	let fun mk_path v = (case pathopt of
				 NONE => PATH(v,[])
			       | SOME p => join_path_labels(p,[l]))
	    fun help(CONTEXT {fixityList, labelMap, pathMap, ordering},
		     v, from_path, pc_maker) =
		let val path = mk_path v
		    val obj = from_path path
		    val pc = pc_maker obj
		    val PATH vpath = path
		    val labelMap = Name.LabelMap.insert(labelMap,l,(path,pc))
		    val pathMap = Name.PathMap.insert(pathMap,vpath,(l,pc))
		    val ordering = path :: ordering
		in CONTEXT{fixityList = fixityList,
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
	  | DEC_MOD(v,_,s as SIGNAT_STRUCTURE(SOME p, sdecs)) => 
		  let val ctxt = help(ctxt, v, path2mod, fn obj => (PHRASE_CLASS_MOD(obj, false,s)))
		  in  if (is_open l)
			  then foldl (sdec_help (v,l)) ctxt sdecs
		      else ctxt
		  end
	  | DEC_MOD(v,b,s) =>
		  let val ctxt = help(ctxt, v, path2mod, fn obj => (PHRASE_CLASS_MOD(obj, b, s)))
		  in  if (is_open l)
			  then error "add_context_sdec got DEC_MOD with open label but not a SIGNAT_STRUCTURE"
		      else ctxt
		  end
	end



    fun add_context_sig(CONTEXT {fixityList, labelMap, pathMap, ordering},
			l, v, signat) = 
	let val pc = PHRASE_CLASS_SIG(v,signat)
	    val path = (v,[])
	in  CONTEXT({fixityList = fixityList,
		     labelMap = Name.LabelMap.insert(labelMap,l,(PATH path, pc)),
		     pathMap = Name.PathMap.insert(pathMap,path,(l, pc)),
		     ordering = (PATH path)::ordering})
	end

    fun add_context_overexp(CONTEXT {fixityList, labelMap, pathMap, ordering},
			    l, v, celist) = 
	let val pc = PHRASE_CLASS_OVEREXP celist
	    val path = (v,[])
	in  CONTEXT({fixityList = fixityList,
		     labelMap = Name.LabelMap.insert(labelMap,l,(PATH path, pc)),
		     pathMap = Name.PathMap.insert(pathMap,path,(l,pc)),
		     ordering = (PATH path)::ordering})
	end

    fun add_context_entry(ctxt, entry) = 
	(case entry of
	     CONTEXT_FIXITY f => add_context_fixity(ctxt,f)
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
	


    (* ---------------- LOOKUP RULES --------------------------- 
     The lookup rules can return item from different grammatical classes
     so we need some additional datatypes to package up the results 
     --------------------------------------------------------- *)

    type phrase_class_p = path * phrase_class


    fun Context_Fixity (CONTEXT {fixityList,...}) = fixityList
    fun Context_Ordering (CONTEXT {ordering,...}) = rev ordering
    fun Context_Lookup  (CONTEXT {labelMap, ...}, lab) = Name.LabelMap.find(labelMap, lab)
    fun Context_Lookup_Path (CONTEXT {pathMap, ...}, PATH path) = Name.PathMap.find(pathMap, path)
    fun Context_Lookup' (CONTEXT {pathMap, ...}, v) = Name.PathMap.find(pathMap, (v,[]))

    (* faster when first context is larger than second *)
    fun plus (ctxt1 as CONTEXT{fixityList = fl1, labelMap = lm1, pathMap = pm1, ordering = ord1},
	      ctxt2 as CONTEXT{fixityList = fl2, labelMap = lm2, pathMap = pm2, ordering = ord2}) =
	let 
	    fun inCtxt(p,ctxt) = (case (Context_Lookup'(ctxt,p)) of
				      NONE => false
				    | SOME _ => true)
	    fun inOrig p = inCtxt(p,ctxt1)
	    fun folder(PATH p,(lm,pm,ord,subst)) =
		let val SOME(lab,pc) = Name.PathMap.find(pm2, p)
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
		    val lm = Name.LabelMap.insert(lm,lab,(PATH p,pc))
		    val pm = Name.PathMap.insert(pm,p,(lab,pc))
		    val ord = (PATH p) :: ord
		in (lm, pm, ord, subst)
		end
	    val fixityList = fl1 @ fl2
	    (* use foldr since ord's are backwards *)
	    val (labelMap,pathMap,ordering,_) = foldr folder (lm1,pm1,ord1,empty_subst) ord2
	in  CONTEXT{fixityList = fixityList,
		    labelMap = labelMap,
		    pathMap = pathMap,
		    ordering = ordering}
	end 
    
      fun plus_context [] = empty_context
	| plus_context [ctxt] = ctxt
	| plus_context (ctxt1::ctxt2::rest) = plus_context((plus(ctxt1,ctxt2)) :: rest)

end

