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
    type tag = Il.tag
    type sdec = Il.sdec
    type sdecs = Il.sdecs
    type fixity_table = Il.fixity_table
    type path = Il.path
    type inline = Il.inline
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
				var_list = (VarMap.empty,[]),
				tag_list = TagMap.empty}


    fun add_context_fixity(CONTEXT {alias_list, flatlist,fixity_list,
				    label_list,var_list,tag_list}, 
			   f) = CONTEXT({alias_list = alias_list,
					 flatlist = flatlist,
					 fixity_list = f @ fixity_list,
					 label_list = label_list,
					 var_list = var_list,
					 tag_list = tag_list})
    fun add_context_flat(CONTEXT {alias_list, flatlist,fixity_list,
				  label_list,var_list,tag_list}, entry) = 
	let val flatlist = entry::flatlist
	in  CONTEXT({alias_list = alias_list,
		     flatlist = flatlist,
			fixity_list = fixity_list,
			label_list = label_list,
			var_list = var_list,
			tag_list = tag_list})
	end

    (*  path is the path to the inline object; 
       lbl is the local name given to the object *)
    local
	fun help (CONTEXT {alias_list, flatlist,fixity_list,
			   label_list,var_list,tag_list},path,lbl,pc) = 
	    let 
	        val label_list = Name.LabelMap.insert(label_list,lbl,(path,pc))
	        val var_list = (case path of
				    PATH(v,[]) => var_seq_insert(var_list,v,(lbl,pc))
				  | _ => var_list)
            in CONTEXT({alias_list = alias_list,
			flatlist = flatlist,
			fixity_list = fixity_list,
			label_list = label_list,
			var_list = var_list,
			tag_list = tag_list})
	    end
	    fun sbnd_sdec_help path ((SBND(l,bnd), SDEC(_,dec)),ctxt as (CONTEXT{label_list,...})) = 
		let val path = join_path_labels(path,[l])
		    val inline = case (bnd,dec) of
			(BND_EXP (_,e), DEC_EXP(_,c)) => INLINE_EXPCON (e,c)
		      | (BND_CON (_,c), DEC_CON(_,k,_)) => INLINE_CONKIND (c,k)
		      | (BND_MOD(_,true,m), DEC_MOD(_,true,s)) => INLINE_MODSIG(true,m,s)
		      | (BND_MOD(_,false,m), DEC_MOD(_,false,s)) => INLINE_MODSIG(true,m,s)
		      | (_, _) => error "bad argument to add_context_inline'"
		    val ctxt' as (CONTEXT{label_list=label_list',...}) = 
			do_inline(ctxt,path,l,inline)
(*
		    val _ =  (print "sbnd_sdec_help called with label_list length = ";
			      print (Int.toString (Name.LabelMap.numItems label_list)); print "\n";
			      print "and returning with label_list length = ";
			      print (Int.toString (Name.LabelMap.numItems label_list')); print "\n\n")
*)
		in  ctxt'
		end
	    
	    and do_inline (ctxt,path, lbl, inline) = 
		(case inline of
		     INLINE_EXPCON ec => help (ctxt,path,lbl,PHRASE_CLASS_EXP ec)
		   | INLINE_CONKIND ck => help (ctxt,path,lbl,PHRASE_CLASS_CON ck)
		   | INLINE_OVER arg => help (ctxt,path,lbl,PHRASE_CLASS_OVEREXP arg)
		   | INLINE_MODSIG (b,m,s) => 
			 let val ctxt = help (ctxt,path,lbl,PHRASE_CLASS_MOD (m,b,s))
			 in  (case (is_label_open lbl,m,s) of
				  (true,MOD_STRUCTURE sbnds,SIGNAT_STRUCTURE(_,sdecs)) =>
				      foldl (sbnd_sdec_help path) ctxt (zip sbnds sdecs)
				| _ => ctxt)
			 end)
    in
	fun add_context_inline (ctxt, l, v, inline) = 
	    do_inline (add_context_flat(ctxt, CONTEXT_INLINE(l,v,inline)), PATH(v,[]), l, inline)
	fun add_context_inline_signature (ctxt, l, v, quad as {self, code, abs_sig}) = 
	    let val s = SIGNAT_INLINE_STRUCTURE quad
		val ctxt = add_context_flat(ctxt, CONTEXT_SDEC(SDEC(l,DEC_MOD(v,false,s))))
		val ctxt = help (ctxt,PATH(v,[]),l,PHRASE_CLASS_MOD (MOD_STRUCTURE code,false,s))
	    in  if (is_label_open l) 
		    then foldl (sbnd_sdec_help (PATH(v,[]))) ctxt (zip code abs_sig)
		else ctxt
	    end
    end

    fun stat_context(CONTEXT {flatlist,fixity_list,
			      label_list,var_list,tag_list, alias_list}) = 
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
			      label_list,var_list,tag_list}, 
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
			   var_list = var_list,
			   tag_list = tag_list}
		end
	    fun sdec_help (v,l) (sdec,ctxt) = add_context_sdec'(ctxt,SOME(mk_path v),sdec)
	in case dec of
	    (DEC_EXP(v,c)) => help(ctxt, v, path2exp, fn obj => (PHRASE_CLASS_EXP (obj, c)))
	  | DEC_CON(v,k,NONE) => help(ctxt,v, path2con, fn obj => (PHRASE_CLASS_CON(obj, k)))
	  | DEC_CON(v,k,SOME c) => help(ctxt,v, path2con, fn _ => (PHRASE_CLASS_CON(c, k)))
	  | DEC_MOD (v,b,s as SIGNAT_FUNCTOR _) => help(ctxt,v, path2mod, fn obj => (PHRASE_CLASS_MOD(obj,b,s)))
	  | DEC_MOD (v,_,(SIGNAT_STRUCTURE(NONE,_))) => 
	    (print "adding non-selfified signature to context: "; pp_sdec sdec; print "\n";
	     error "adding non-selfified signature to context")
	  | DEC_MOD(_,_,(SIGNAT_INLINE_STRUCTURE {self=NONE,...})) => 
	    (print "adding non-selfified inline signature to context: "; pp_sdec sdec; print "\n";
	     error "adding non-selfified inline signature to context")
	  | DEC_MOD(v,_,SIGNAT_INLINE_STRUCTURE (quad as {self=SOME _,...})) =>
	          add_context_inline_signature (ctxt,l,v,quad)
	  | DEC_MOD(v,_,s as SIGNAT_STRUCTURE(SOME p, sdecs)) => 
		  let val ctxt = help(ctxt, v, path2mod, fn obj => (PHRASE_CLASS_MOD(obj, false,s)))
		  in  if (is_label_open l)
			  then foldl (sdec_help (v,l)) ctxt sdecs
		      else ctxt
		  end
	  | DEC_MOD(v,b,s) =>
		  let val ctxt = help(ctxt, v, path2mod, fn obj => (PHRASE_CLASS_MOD(obj, b, s)))
		  in  if (is_label_open l)
			  then error "add_context_sdec got DEC_MOD with open label but not a SIGNAT_STRUCTURE"
		      else ctxt
		  end
	  | DEC_EXCEPTION(t,c) => 
		  let val CONTEXT {alias_list, flatlist,fixity_list,
				   label_list,var_list,tag_list} = ctxt
		      val tag_list = Name.TagMap.insert(tag_list,t,c)
		  in CONTEXT{alias_list = alias_list,
			     flatlist = flatlist,
			     fixity_list = fixity_list,
			     label_list = label_list,
			     var_list = var_list,
			     tag_list = tag_list}
		  end
	end
    fun add_context_sdec(ctxt,sdec) = add_context_sdec'(add_context_flat(ctxt, CONTEXT_SDEC sdec),NONE,sdec)


    fun add_context_sig(CONTEXT {alias_list, flatlist,fixity_list,
				 label_list,var_list,tag_list}, 
			l, v, signat) = 
	CONTEXT({alias_list = alias_list,
		 flatlist = (CONTEXT_SDEC(SDEC(l,DEC_MOD(v,false,signat))))::flatlist,
		 fixity_list = fixity_list,
		 label_list = Name.LabelMap.insert(label_list,l,
						   (PATH(v,[]), PHRASE_CLASS_SIG(v,signat))),
		 var_list = var_seq_insert(var_list,v,(l, PHRASE_CLASS_SIG (v,signat))),
		 tag_list = tag_list})

    fun add_context_alias(CONTEXT {alias_list, flatlist,fixity_list,
				   label_list,var_list,tag_list}, 
			  l, labs) = 
	let val _ = error "add_context_alias: no one should be using this now"
	    val alias_list = LabelMap.insert(alias_list,l,labs)
	in CONTEXT{alias_list = alias_list,
		   flatlist = flatlist,
		   fixity_list = fixity_list,
		   label_list = label_list,
		   var_list = var_list,
		   tag_list = tag_list}
	end

    fun add_context_entry(ctxt, entry) = 
	(case entry of
	     CONTEXT_FIXITY f => add_context_fixity(ctxt,f)
	   | CONTEXT_ALIAS (l,labs) => add_context_alias(ctxt,l,labs)
	   | CONTEXT_SDEC sdec => add_context_sdec(ctxt,sdec)
	   | CONTEXT_SIGNAT (l,v,s) => add_context_sig(ctxt,l,v,s)
	   | CONTEXT_INLINE (l,v,i) => add_context_inline(ctxt,l,v,i))

    fun add_context_entry'(entry,ctxt) = add_context_entry(ctxt,entry)
    fun add_context_entries (ctxt, entries) = foldl add_context_entry' ctxt entries
    fun add_context_sdec'(sdec,ctxt) = add_context_sdec(ctxt,sdec)
    fun add_context_sdecs (ctxt, sdecs) = foldl add_context_sdec' ctxt sdecs

    fun anon_label () = fresh_internal_label "anon"
    fun dec2sdec dec = SDEC(anon_label(),dec)
    fun decs2sdecs decs = map dec2sdec decs
    fun add_context_decs(ctxt, decs) = add_context_sdecs(ctxt, decs2sdecs decs)
    fun add_context_dec(ctxt, dec) = add_context_decs(ctxt,[dec])

    fun add_context_exp(c, l, v, con) = add_context_sdec(c,SDEC(l,DEC_EXP(v,con)))
    fun add_context_mod(c, l, v, signat) = add_context_sdec(c,SDEC(l,DEC_MOD(v,false,signat)))
    fun add_context_con(c, l, v, kind, conopt) = add_context_sdec(c,SDEC(l,DEC_CON(v,kind,conopt)))

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
    fun name_bound(CONTEXT{tag_list,...},t) = (case Name.TagMap.find(tag_list,t) of
						   NONE => false
						 | SOME _ => true)



      fun Context_Varlist (CONTEXT {var_list,...}) = rev(#2 var_list)
      fun Context_Lookup' (CONTEXT {var_list,...},v) = Name.VarMap.find(#1 var_list,v)
      fun Context_Exn_Lookup (CONTEXT {tag_list,...},t) = Name.TagMap.find(tag_list,t)


     fun Context_Lookup (ctxt as CONTEXT{label_list, ...}, lab) = 
	 Name.LabelMap.find(label_list,lab)


     fun context_to_sdecs (CONTEXT {var_list,...}) =
	  Name.VarMap.foldli (fn (v,(lab,phrase_class),sdecs) =>
			      case phrase_class
				of PHRASE_CLASS_EXP(exp,con) => SDEC(lab,DEC_EXP(v,con))::sdecs
			         | PHRASE_CLASS_CON(con,kind) => SDEC(lab,DEC_CON(v,kind,SOME con))::sdecs
				 | PHRASE_CLASS_MOD(m,b,signat) => SDEC(lab,DEC_MOD(v,b,signat))::sdecs
				 | PHRASE_CLASS_SIG _ => sdecs
				 | PHRASE_CLASS_OVEREXP _ => sdecs) [] (#1 var_list)

      (* faster when first context is larger than second *)
      fun plus (csubster,ksubster,ssubster,orig_ctxt, 
		ctxt2 as CONTEXT{flatlist, fixity_list, label_list, var_list, tag_list,alias_list=_}) =
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
			      fun do_c c = csubster(c,esubst,csubst,msubst)
			      fun do_s s = ssubster(s,esubst,csubst,msubst)
			      fun do_k k = ksubster(k,esubst,csubst,msubst)
			  in (case pc of
				  PHRASE_CLASS_EXP(exp,con) => add_context_sdec(ctxt,SDEC(lab,DEC_EXP(v,do_c con)))
				| PHRASE_CLASS_CON(con,kind) => add_context_sdec(ctxt,
										 SDEC(lab,DEC_CON(v,do_k kind,
												  SOME (do_c con))))
				| PHRASE_CLASS_MOD(m,b,signat) => add_context_sdec(ctxt,SDEC(lab,DEC_MOD(v,b,do_s signat)))
				| PHRASE_CLASS_SIG (v,s) => add_context_entry(ctxt,CONTEXT_SIGNAT(lab,v,do_s s))
				| PHRASE_CLASS_OVEREXP celist => add_context_inline(ctxt,lab,v,INLINE_OVER celist),
				      subst)
			  end
		  end
              (* cannot fold over #1 var_list since that is unordered *)
	      val (ctxt,_) = foldl folder (ctxt,([],[],[])) (Context_Varlist ctxt2)
	  in  ctxt
	  end  (* MEMO: What about the tag_list ?? - Martin *)
	  
      fun plus_context (csubster,ksubster,ssubster) ctxts =
	  (case ctxts of
	       [] => empty_context
	     | [ctxt] => ctxt
	     | (ctxt::rest) => foldl (fn (c,acc) => plus(csubster,ksubster,ssubster,acc,c)) ctxt rest)





end

