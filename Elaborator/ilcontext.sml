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
    val empty_context = CONTEXT{alias_list = LabelMap.empty,
				flatlist = [],
				fixity_list = [],
				label_list = LabelMap.empty,
				var_list = (VarMap.empty,[])}


    fun add_context_fixity(CONTEXT {alias_list, flatlist,fixity_list,
				    label_list,var_list}, 
			   f) = CONTEXT({alias_list = alias_list,
					 flatlist = flatlist,
					 fixity_list = f @ fixity_list,
					 label_list = label_list,
					 var_list = var_list})
    fun add_context_flat(CONTEXT {alias_list, flatlist,fixity_list,
				  label_list,var_list}, entry) = 
	let val flatlist = entry::flatlist
	in  CONTEXT({alias_list = alias_list,
		     flatlist = flatlist,
			fixity_list = fixity_list,
			label_list = label_list,
			var_list = var_list})
	end


    fun stat_context(CONTEXT {flatlist,fixity_list,
			      label_list,var_list, alias_list}) = 
	() (* (Name.LabelMap.appi 
	 (fn (l,(path,pc)) => (print "label = "; print (Name.label2string l); print "\n"))
	 label_list;
	 print "\n";
	(app
	 (fn v => (print "var = "; print (Name.var2string v); print "\n"))
	 (#2 var_list))) *)

    fun add_context_sdec'(ctxt, pathopt, sdec as (SDEC(l,dec))) = 
	let (* val _ = (print "add_context_sdec':\n ";
		     stat_context ctxt; print "\n\n\n") *)
	    fun mk_path v = (case pathopt of
				 NONE => PATH(v,[])
			       | SOME p => join_path_labels(p,[l]))
	    fun help(CONTEXT {alias_list, flatlist,fixity_list,
			      label_list,var_list}, 
		     v, from_path, pc_maker) =
		let val path = mk_path v
		    val obj = from_path path
		    val pc = pc_maker obj
		    val label_list = Name.LabelMap.insert(label_list,l,(path,pc))
		    val var_list = (case path of
					PATH(v,[]) => var_seq_insert(var_list,v,(l,pc))
				      | _ => var_list)
		in CONTEXT{alias_list = alias_list,
			   flatlist = flatlist,
			   fixity_list = fixity_list,
			   label_list = label_list,
			   var_list = var_list}
		end
	    fun sdec_help (v,l) (sdec,ctxt) = add_context_sdec'(ctxt,SOME(mk_path v),sdec)
	in case dec of
	    DEC_EXP(v,c,eopt,inline) => help(ctxt, v, path2exp, fn obj => PHRASE_CLASS_EXP(obj, c, eopt, inline))
	  | DEC_CON(v,k,copt,inline) => help(ctxt, v, path2con, fn obj => PHRASE_CLASS_CON(obj, k, copt, inline))
	  | DEC_MOD (v,b,s as SIGNAT_FUNCTOR _) => help(ctxt,v, path2mod, fn obj => (PHRASE_CLASS_MOD(obj,b,s)))
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
    fun add_context_sdec(ctxt,sdec) = add_context_sdec'(add_context_flat(ctxt, CONTEXT_SDEC sdec),NONE,sdec)


    fun add_context_sig(CONTEXT {alias_list, flatlist,fixity_list,
				 label_list,var_list}, 
			l, v, signat) = 
	CONTEXT({alias_list = alias_list,
		 flatlist = (CONTEXT_SDEC(SDEC(l,DEC_MOD(v,false,signat))))::flatlist,
		 fixity_list = fixity_list,
		 label_list = Name.LabelMap.insert(label_list,l,
						   (PATH(v,[]), PHRASE_CLASS_SIG(v,signat))),
		 var_list = var_seq_insert(var_list,v,(l, PHRASE_CLASS_SIG (v,signat)))})

    fun add_context_overexp(CONTEXT {alias_list, flatlist,fixity_list,
				     label_list,var_list}, 
			    l, v, celist) = 
	let val pc = PHRASE_CLASS_OVEREXP celist
	in  CONTEXT({alias_list = alias_list,
		     flatlist = (CONTEXT_OVEREXP(l,v,celist))::flatlist,
		     fixity_list = fixity_list,
		     label_list = Name.LabelMap.insert(label_list,l, (PATH(v,[]), pc)),
		     var_list = var_seq_insert(var_list,v,(l, pc))})
	end

    fun add_context_alias(CONTEXT {alias_list, flatlist,fixity_list,
				   label_list,var_list}, 
			  l, labs) = 
	let val _ = error "add_context_alias: no one should be using this now"
	    val alias_list = LabelMap.insert(alias_list,l,labs)
	in CONTEXT{alias_list = alias_list,
		   flatlist = flatlist,
		   fixity_list = fixity_list,
		   label_list = label_list,
		   var_list = var_list}
	end

    fun add_context_entry(ctxt, entry) = 
	(case entry of
	     CONTEXT_FIXITY f => add_context_fixity(ctxt,f)
	   | CONTEXT_ALIAS (l,labs) => add_context_alias(ctxt,l,labs)
	   | CONTEXT_SDEC sdec => add_context_sdec(ctxt,sdec)
	   | CONTEXT_SIGNAT (l,v,s) => add_context_sig(ctxt,l,v,s)
	   | CONTEXT_OVEREXP(l,v,celist) => add_context_overexp(ctxt,l,v,celist))


    fun add_context_entry'(entry,ctxt) = add_context_entry(ctxt,entry)
    fun add_context_entries (ctxt, entries) = foldl add_context_entry' ctxt entries
    fun add_context_sdec'(sdec,ctxt) = add_context_sdec(ctxt,sdec)
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


    fun fixity (CONTEXT {fixity_list,...}) = fixity_list

    fun var_bound(CONTEXT{var_list,...},v) = (case Name.VarMap.find(#1 var_list,v) of
						  NONE => false
						| SOME _ => true)


      fun Context_Varlist (CONTEXT {var_list,...}) = rev(#2 var_list)
      fun Context_Lookup' (CONTEXT {var_list,...},v) = Name.VarMap.find(#1 var_list,v)


     fun Context_Lookup (ctxt as CONTEXT{label_list, ...}, lab) = 
	 Name.LabelMap.find(label_list,lab)


     fun context_to_sdecs (CONTEXT {var_list,...}) =
	  Name.VarMap.foldli 
	  (fn (v,(lab,phrase_class),sdecs) =>
	   case phrase_class of
	       PHRASE_CLASS_EXP(_,con,eopt,inline) => SDEC(lab,DEC_EXP(v,con,eopt,inline))::sdecs
	     | PHRASE_CLASS_CON(_,kind,copt,inline) => SDEC(lab,DEC_CON(v,kind,copt,inline))::sdecs
	     | PHRASE_CLASS_MOD(m,b,signat) => SDEC(lab,DEC_MOD(v,b,signat))::sdecs
	     | PHRASE_CLASS_SIG _ => sdecs
	     | PHRASE_CLASS_OVEREXP _ => sdecs) [] (#1 var_list)

      (* faster when first context is larger than second *)
      fun plus (esubster,csubster,ksubster,ssubster,orig_ctxt, 
		ctxt2 as CONTEXT{flatlist, fixity_list, label_list, var_list,alias_list=_}) =
	  let val ctxt = add_context_fixity(orig_ctxt,fixity_list)
	      fun varIn (v,ctxt) = (case (Context_Lookup'(ctxt,v)) of
					NONE => false
				      | SOME _ => true)
	      fun folder(v,(ctxt,subst)) =
		  let val in_orig = varIn(v,orig_ctxt)
		      val in_current = varIn(v,ctxt)
		      val SOME(lab,pc) = Name.VarMap.find(#1 var_list, v)
		  in  if (not in_orig andalso in_current)
			  then (ctxt,subst) (* inserted already due to open in next context *)
		      else
			  let val (v,subst as (esubst,csubst,msubst)) = 
			      if not in_current
				  then  (v,subst)
			      else  let val v' = Name.derived_var v
					val (esubst,csubst,msubst) = subst
				    in  (v',((v,VAR v')::esubst,
					     (v,CON_VAR v')::csubst,
					     (v,MOD_VAR v')::msubst))
				    end
			      fun do_e e = esubster(e,esubst,csubst,msubst)
			      fun do_c c = csubster(c,esubst,csubst,msubst)
			      fun do_s s = ssubster(s,esubst,csubst,msubst)
			      fun do_k k = ksubster(k,esubst,csubst,msubst)
			  in (case pc of
				  PHRASE_CLASS_EXP(_,con,eopt,inline) => 
				      add_context_sdec(ctxt,SDEC(lab,DEC_EXP(v,do_c con, 
									     Util.mapopt do_e eopt,
									     inline)))
				| PHRASE_CLASS_CON(_,kind,copt,inline) => 
				      add_context_sdec(ctxt,
						       SDEC(lab,DEC_CON(v,do_k kind,
									Util.mapopt do_c copt,
									inline)))
				| PHRASE_CLASS_MOD(m,b,signat) => add_context_sdec(ctxt,SDEC(lab,DEC_MOD(v,b,do_s signat)))
				| PHRASE_CLASS_SIG (v,s) => add_context_entry(ctxt,CONTEXT_SIGNAT(lab,v,do_s s))
				| PHRASE_CLASS_OVEREXP celist => add_context_entry(ctxt,CONTEXT_OVEREXP(lab,v,celist)),
				      subst)
			  end
		  end
              (* cannot fold over #1 var_list since that is unordered *)
	      val (ctxt,_) = foldl folder (ctxt,([],[],[])) (Context_Varlist ctxt2)
	  in  ctxt
	  end  (* MEMO: What about the tag_list ?? - Martin *)
	  
      fun plus_context (esubster,csubster,ksubster,ssubster) ctxts =
	  (case ctxts of
	       [] => empty_context
	     | [ctxt] => ctxt
	     | (ctxt::rest) => foldl (fn (c,acc) => plus(esubster,csubster,ksubster,ssubster,acc,c)) ctxt rest)





end

