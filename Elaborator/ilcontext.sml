functor IlContext(structure Il : ILLEAK) 
    : ILCONTEXT = 
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
	
    open Il Util Name Listops 
	
    val error = fn s => error "ilcontext.sml" s
	
    val blast_debug = ref false
    val debug = ref false
    fun debugdo t = if (!debug) then (t(); ()) else ()
    fun join_path_labels (SIMPLE_PATH v, l) = COMPOUND_PATH(v,l)
      | join_path_labels (COMPOUND_PATH (v,ls), l) = COMPOUND_PATH(v,ls @ l)
    fun print_path (SIMPLE_PATH v) = print (var2string v)
      | print_path (COMPOUND_PATH(v,lbls)) = (print (var2string v);
					      app (fn lbl => (print ".";
							      print (label2string lbl)))
					      lbls)
    fun path2obj (var_maker : var -> 'a, mod_maker : mod * label -> 'a) p = 
      (case p of
	 (SIMPLE_PATH v) => var_maker v
       | (COMPOUND_PATH (v,ls)) => let fun loop [] _ = var_maker v
					 | loop [l] acc = mod_maker(acc,l)
					 | loop (l::rest) acc = loop rest (MOD_PROJECT(acc,l))
				   in loop ls (MOD_VAR v)
				   end)
    val path2mod = path2obj (MOD_VAR,MOD_PROJECT)
    val path2con = path2obj (CON_VAR, CON_MODULE_PROJECT)
    val path2exp = path2obj (VAR, MODULE_PROJECT)

    (* --------------------- EXTENDERS ---------------------------------------- *)
    type var_seq_map = (label * phrase_class) VarMap.map * var list
    fun var_seq_insert ((m,s),v,value) = (VarMap.insert(m,v,value),v::s)
    val empty_context = CONTEXT{flatlist = [],
				fixity_list = [],
				label_list = LabelMap.empty,
				var_list = (VarMap.empty,[]),
				tag_list = TagMap.empty}


    fun add_context_fixity(CONTEXT {flatlist,fixity_list,
				    label_list,var_list,tag_list}, 
			   f) = CONTEXT({flatlist = flatlist,
					 fixity_list = f @ fixity_list,
					 label_list = label_list,
					 var_list = var_list,
					 tag_list = tag_list})
    fun add_context_flat(CONTEXT {flatlist,fixity_list,
				  label_list,var_list,tag_list}, entry) = 
	let val flatlist = entry::flatlist
	in  CONTEXT({flatlist = flatlist,
			fixity_list = fixity_list,
			label_list = label_list,
			var_list = var_list,
			tag_list = tag_list})
	end

    (*  path is the path to the inline object; 
       lbl is the local name given to the object *)
    local
	fun help (CONTEXT {flatlist,fixity_list,
			   label_list,var_list,tag_list},path,lbl,pc) = 
	    let 
	        val label_list = Name.LabelMap.insert(label_list,lbl,(path,pc))
	        val var_list = (case path of
				    SIMPLE_PATH v => var_seq_insert(var_list,v,(lbl,pc))
				  | _ => var_list)
            in CONTEXT({flatlist = flatlist,
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
		      | (BND_MOD(_,m), DEC_MOD(_,s)) => INLINE_MODSIG(m,s)
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
		   | INLINE_MODSIG (m,s) => 
			 let val ctxt = help (ctxt,path,lbl,PHRASE_CLASS_MOD (m,s))
			 in  (case (is_label_open lbl,m,s) of
				  (true,MOD_STRUCTURE sbnds,SIGNAT_STRUCTURE(_,sdecs)) =>
				      foldl (sbnd_sdec_help path) ctxt (zip sbnds sdecs)
				| _ => ctxt)
			 end)
    in
	fun add_context_inline (ctxt, l, v, inline) = 
	    do_inline (add_context_flat(ctxt, CONTEXT_INLINE(l,v,inline)), SIMPLE_PATH v, l, inline)
	fun add_context_inline_signature (ctxt, l, v, quad as {self, code, imp_sig, abs_sig}) = 
	    let val s = SIGNAT_INLINE_STRUCTURE quad
		val ctxt = add_context_flat(ctxt, CONTEXT_SDEC(SDEC(l,DEC_MOD(v,s))))
		val ctxt = help (ctxt,SIMPLE_PATH v,l,PHRASE_CLASS_MOD (MOD_STRUCTURE code,s))
	    in  if (is_label_open l) 
		    then foldl (sbnd_sdec_help (SIMPLE_PATH v)) ctxt (zip code abs_sig)
		else ctxt
	    end
    end

    fun stat_context(CONTEXT {flatlist,fixity_list,
			      label_list,var_list,tag_list}) = 
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
				 NONE => SIMPLE_PATH v
			       | SOME p => join_path_labels(p,[l]))
	    fun help(CONTEXT {flatlist,fixity_list,
			      label_list,var_list,tag_list}, 
		     v, from_path, pc_maker) =
		let val path = mk_path v
		    val obj = from_path path
		    val pc = pc_maker obj
		    val label_list = Name.LabelMap.insert(label_list,l,(path,pc))
		    val var_list = (case path of
					SIMPLE_PATH v => var_seq_insert(var_list,v,(l,pc))
				      | _ => var_list)
		in CONTEXT{flatlist = flatlist,
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
	  | DEC_MOD (v,s as SIGNAT_FUNCTOR _) => help(ctxt,v, path2mod, fn obj => (PHRASE_CLASS_MOD(obj, s)))
	  | DEC_MOD (v,((SIGNAT_STRUCTURE(NONE,_)) | 
			(SIGNAT_INLINE_STRUCTURE {self=NONE,...}))) => 
	          error "adding non-selfified signature to context"
	  | DEC_MOD(v,SIGNAT_INLINE_STRUCTURE (quad as {self=SOME _,...})) =>
	          add_context_inline_signature (ctxt,l,v,quad)
	  | DEC_MOD(v,s as SIGNAT_STRUCTURE(SOME p, sdecs)) => 
		  let val ctxt = help(ctxt, v, path2mod, fn obj => (PHRASE_CLASS_MOD(obj, s)))
		  in  if (is_label_open l)
			  then foldl (sdec_help (v,l)) ctxt sdecs
		      else ctxt
		  end
	  | DEC_EXCEPTION(t,c) => 
		  let val CONTEXT {flatlist,fixity_list,
				   label_list,var_list,tag_list} = ctxt
		      val tag_list = Name.TagMap.insert(tag_list,t,c)
		  in CONTEXT{flatlist = flatlist,
			     fixity_list = fixity_list,
			     label_list = label_list,
			     var_list = var_list,
			     tag_list = tag_list}
		  end
	end
    fun add_context_sdec(ctxt,sdec) = add_context_sdec'(add_context_flat(ctxt, CONTEXT_SDEC sdec),NONE,sdec)


    fun add_context_sig(CONTEXT {flatlist,fixity_list,
				 label_list,var_list,tag_list}, 
			l, v, signat) = 
	CONTEXT({flatlist = (CONTEXT_SDEC(SDEC(l,DEC_MOD(v,signat))))::flatlist,
		 fixity_list = fixity_list,
		 label_list = Name.LabelMap.insert(label_list,l,
						   (SIMPLE_PATH v, PHRASE_CLASS_SIG signat)),
		 var_list = var_seq_insert(var_list,v,(l, PHRASE_CLASS_SIG signat)),
		 tag_list = tag_list})

    fun add_context_entry(ctxt, entry) = 
	(case entry of
	     CONTEXT_FIXITY f => add_context_fixity(ctxt,f)
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
    fun add_context_mod(c, l, v, signat) = add_context_sdec(c,SDEC(l,DEC_MOD(v,signat)))
    fun add_context_con(c, l, v, kind, conopt) = add_context_sdec(c,SDEC(l,DEC_CON(v,kind,conopt)))

    fun add_context_exp'(c, v, con) = add_context_exp(c,anon_label(), v, con)
    fun add_context_mod'(c, v, signat) = add_context_mod(c,anon_label(), v, signat)
    fun add_context_con'(c, v, kind, conopt) = add_context_con(c,anon_label(), v, kind, conopt)
    fun add_context_sig'(c, v, signat) = add_context_sig(c,anon_label(), v, signat)
	


    (* ---------------- LOOKUP RULES --------------------------- 
     The lookup rules can return item from different grammatical classes
     so we need some additional datatypes to package up the results 
     --------------------------------------------------------- *)
    datatype phrase = PHRASE_EXP of exp
                    | PHRASE_CON of con
                    | PHRASE_MOD of mod
                    | PHRASE_SIG of signat
                    | PHRASE_OVEREXP of (con * exp) list

    datatype class = CLASS_EXP of con
                   | CLASS_CON of kind
                   | CLASS_MOD of signat
                   | CLASS_SIG
                   | CLASS_OVEREXP


    type phrase_class_p = path * phrase_class


    fun fixity (CONTEXT {fixity_list,...}) = fixity_list

    fun var_bound(CONTEXT{var_list,...},v) = (case Name.VarMap.find(#1 var_list,v) of
						  NONE => false
						| SOME _ => true)
    fun name_bound(CONTEXT{tag_list,...},t) = (case Name.TagMap.find(tag_list,t) of
						   NONE => false
						 | SOME _ => true)

    fun path2obj (var_maker : var -> 'a, mod_maker : mod * label -> 'a) p = 
      (case p of
	 (SIMPLE_PATH v) => var_maker v
       | (COMPOUND_PATH (v,ls)) => let fun loop [] _ = var_maker v
					 | loop [l] acc = mod_maker(acc,l)
					 | loop (l::rest) acc = loop rest (MOD_PROJECT(acc,l))
				   in loop ls (MOD_VAR v)
				   end)
    val path2mod = path2obj (MOD_VAR,MOD_PROJECT)
    val path2con = path2obj (CON_VAR, CON_MODULE_PROJECT)
    val path2exp = path2obj (VAR, MODULE_PROJECT)

    fun combine_pc (PHRASE_EXP e, CLASS_EXP c) = PHRASE_CLASS_EXP (e,c)
      | combine_pc (PHRASE_CON c, CLASS_CON k) = PHRASE_CLASS_CON (c,k)
      | combine_pc (PHRASE_MOD m, CLASS_MOD s) = PHRASE_CLASS_MOD (m,s)
      | combine_pc (PHRASE_SIG s, CLASS_SIG) = PHRASE_CLASS_SIG s
      | combine_pc (PHRASE_OVEREXP oe, CLASS_OVEREXP) = PHRASE_CLASS_OVEREXP oe 
      | combine_pc _ = error "combine_pc got a phrase and a class of conflicting flavors"
    fun pc2class (PHRASE_CLASS_EXP (e,c)) = CLASS_EXP c
      | pc2class (PHRASE_CLASS_CON (c,k)) = CLASS_CON k
      | pc2class (PHRASE_CLASS_MOD (m,s)) = CLASS_MOD s
      | pc2class (PHRASE_CLASS_SIG _) = CLASS_SIG
      | pc2class (PHRASE_CLASS_OVEREXP _) = CLASS_OVEREXP

    fun classpath2pc (p, CLASS_EXP c) = (p, PHRASE_CLASS_EXP (path2exp p, c))
      | classpath2pc (p, CLASS_CON k) = (p, PHRASE_CLASS_CON (path2con p, k))
      | classpath2pc (p, CLASS_MOD s) = (p, PHRASE_CLASS_MOD (path2mod p, s))
      | classpath2pc (p, CLASS_SIG) = error "classpath2pc got a CLASS_SIG"
      | classpath2pc (p, CLASS_OVEREXP) = error "classpath2pc got a CLASS_OVEREXP"

    fun Sbnds_Lookup (sbnds, labs) : (labels * phrase) option =
	let 
(*
	    val _ = (print "sbnds_lookup called with labs = ";
		     app (fn l => (print (Name.label2string l); print ".")) labs; 
		     print "\nand sbnds are:\n";
		     app (fn (SBND(l,_)) => (print (Name.label2string l); print " ...")) sbnds;
		     print "\n\n")
*)
	    fun loop lbl [] = NONE
	      | loop lbl ((sbnd as SBND(l,b))::r) = 
		let val self = loop lbl 
		in
		    (case b of
			 (BND_EXP (_,e)) => if (eq_label(l,lbl)) 
						then SOME([l],PHRASE_EXP e) else self r
		       | (BND_CON (_,c)) => if (eq_label(l,lbl)) 
						then let val c' = c (* XXX *)
						     in SOME([l],PHRASE_CON c') 
						     end
					    else self r
		       | (BND_MOD (_,m)) => (if (eq_label(l,lbl)) 
						 then SOME([l],PHRASE_MOD m) 
					     else if (is_label_open l)
						      then 
							  (case m of 
							       MOD_STRUCTURE sbnds =>
								   (case (self (rev sbnds)) of
									SOME(lbls',phrase) => SOME(l::lbls',phrase)
								      | NONE => self r)
							     | _ => self r)
						  else self r))
		end
	in
	    (case labs of
		 [] => error "Sbnds_Lookup got []"
	       | [lbl] => loop lbl (rev sbnds)
	       | (lbl :: lbls) =>
		     (case (loop lbl (rev sbnds)) of
			 SOME(labs,PHRASE_MOD (MOD_STRUCTURE sbnds)) => 
			     (case (Sbnds_Lookup(sbnds,lbls)) of
				  SOME(labs2,phrase2) => SOME(labs@labs2,phrase2)
				| _ => NONE)
		       | _ => NONE))
	end

    fun Sdecs_Lookup_help (om, sdecs, labs) : (bool * (phrase_class * labels)) option = 
	let 
	    fun loop m lbl [] = NONE
	      | loop m lbl ((sdec as (SDEC(l,d)))::rest) =
		if (eq_label(l,lbl)) 
		    then (case d of
			      (DEC_EXP (_,c)) => 
				  SOME(false,(PHRASE_CLASS_EXP(MODULE_PROJECT(m,l), c),[l]))
			    | (DEC_CON (_,k,SOME c)) =>
				  SOME(true,(PHRASE_CLASS_CON(c,k),[l]))
			    | (DEC_CON (_,k,NONE)) => 
				  SOME(false,(PHRASE_CLASS_CON(CON_MODULE_PROJECT(m,l),
							   k),[l]))
			    | (DEC_MOD (_,s)) => 
				SOME(false,(PHRASE_CLASS_MOD(MOD_PROJECT(m,l),s),[l]))
			    | _ => loop m lbl rest)
		else if (is_label_open l)
		    then (case d of
			      (DEC_MOD(_,SIGNAT_STRUCTURE (_,sdecs))) =>
				  (case (loop (MOD_PROJECT(m,l)) lbl (rev sdecs)) of
				       SOME (flag,(class,lbls')) => SOME(flag,(class,l::lbls'))
				     | NONE => loop m lbl rest)
			    | (DEC_MOD(_,((SIGNAT_INLINE_STRUCTURE{code,abs_sig=sdecs,...})))) =>
				  (case (Sbnds_Lookup(code,[lbl]),
					 loop (MOD_PROJECT(m,l)) lbl (rev sdecs)) of
				       (SOME (lbl,p), SOME(_,(pc,lbls'))) => 
						SOME(true,(combine_pc(p,pc2class pc),l::lbls'))
				     | (SOME _, NONE) => error "sdecs_Lookup: open case:  SOME/NONE"
				     | (NONE, SOME _) => error "sdecs_Lookup: open case:  NONE/SOME"
				     | (NONE,NONE) => loop m lbl rest)
			    | _ => loop m lbl rest)
		     else loop m lbl rest

	in
	    (case labs of
		 [] => error "Sdecs_Lookup_help got []"
	       | [lbl] => loop om lbl (rev sdecs)
	       | (lbl :: lbls) =>
		     case (loop om lbl (rev sdecs)) of
			 SOME(_,(phrase_class,labs)) =>
			     (case phrase_class of
				  PHRASE_CLASS_MOD (m',((SIGNAT_STRUCTURE (_,sdecs')))) =>
				      (case (Sdecs_Lookup_help(m',sdecs',lbls)) of
					  SOME(nontrivial,(pc2,labs2)) => SOME(nontrivial,(pc2,labs @ labs2))
					| NONE => NONE)
				| PHRASE_CLASS_MOD (m',((SIGNAT_INLINE_STRUCTURE {code,abs_sig,...}))) =>
				    (case (Sbnds_Lookup(code,lbls)) of
					 SOME(labels,phrase) =>
					     (case (Sdecs_Lookup_help(m',abs_sig,lbls)) of
						  SOME(_,(pc,labs2)) => 
						      let val class = pc2class pc
						      in  SOME(true,(combine_pc(phrase,class),labs@labs2))
						      end
						| NONE => NONE)
	                               | NONE => NONE)
				| _ => NONE)
		       | NONE => NONE)
	end

    fun Sdecs_Lookup (m, sdecs, labs) : (labels * phrase_class) option =
	let 
	    fun loop lbl [] = NONE
	      | loop lbl ((sdec as (SDEC(l,d)))::rest) =
		if (eq_label(l,lbl)) 
		    then (case d of
			      (DEC_EXP (_,c)) => 
				  SOME([l],PHRASE_CLASS_EXP(MODULE_PROJECT(m,l), c))
			    | (DEC_CON (_,k,SOME c)) =>
				  SOME([l],PHRASE_CLASS_CON(c,k))
			    | (DEC_CON (_,k,NONE)) => 
				  SOME([l],PHRASE_CLASS_CON(CON_MODULE_PROJECT(m,l),
							k))
			    | (DEC_MOD (_,s)) => 
				  SOME([l],PHRASE_CLASS_MOD(MOD_PROJECT(m,l),s))
			    | _ => loop lbl rest)
		else loop lbl rest

	in
	    (case labs of
		 [] => error "Sdecs_Lookup got []"
	       | [lbl] => loop lbl (rev sdecs)
	       | (lbl :: lbls) =>
		     case (loop lbl (rev sdecs)) of
			 SOME(labs,PHRASE_CLASS_MOD (m',((SIGNAT_STRUCTURE (_,sdecs'))))) =>
			     (case (Sdecs_Lookup(m',sdecs',lbls)) of
				  SOME(labs2, pc2) => SOME(labs @ labs2, pc2)
				| NONE => NONE)
		       | SOME _ => NONE
		       | NONE => NONE)
	end


    fun Context_Lookup (ctxt, [] : label list) : (path * phrase_class) option = NONE
      | Context_Lookup (CONTEXT{label_list, ...}, (lab::labrest)) = 
	(case (labrest,Name.LabelMap.find(label_list,lab)) of
	    (_,NONE) => NONE
	  | ([],SOME (path,pc)) => SOME(path,pc)
	  | (_,SOME (path,pc)) =>
		case pc of
		    ((PHRASE_CLASS_MOD(MOD_STRUCTURE sbnds,SIGNAT_STRUCTURE (_,sdecs))) |
		     (PHRASE_CLASS_MOD(_,SIGNAT_INLINE_STRUCTURE {abs_sig=sdecs,code=sbnds,...}))) =>
		    (case (Sbnds_Lookup(sbnds,labrest)) of
			 SOME(labels,phrase) =>
			     (case (Sdecs_Lookup_help(path2mod path,sdecs,labrest)) of
				  SOME(_,(pc,labels')) => 
				      let val class = pc2class pc
					  val p = join_path_labels(path,labels)
				      in  SOME(p,combine_pc(phrase,class)) 
				      end
				| NONE => NONE)
		       | NONE => NONE)
		  | PHRASE_CLASS_MOD(_,SIGNAT_STRUCTURE (_,sdecs)) =>
			(case (Sdecs_Lookup_help(path2mod path,sdecs,labrest)) of
			     SOME(_,(pc,labels)) =>
				 let val class = pc2class pc
				     val p = join_path_labels(path,labels)
				 in  (* SOME(classpath2pc(p,class)) *)
					SOME (p,pc)
				 end
			   | NONE => NONE)
		  | _ => NONE)



      fun Sdecs_Lookup'(m,sdecs,labels) = 
	  (case (Sdecs_Lookup_help (m,sdecs,labels)) of
	       SOME(_,(pc,labels)) => SOME(labels,pc)
	     | NONE => NONE)


      fun Context_Varlist (CONTEXT {var_list,...}) = rev(#2 var_list)

      fun Context_Lookup' (CONTEXT {var_list,...},v) = Name.VarMap.find(#1 var_list,v)
      fun Context_Exn_Lookup (CONTEXT {tag_list,...},t) = Name.TagMap.find(tag_list,t)


     fun context_to_sdecs (CONTEXT {var_list,...}) =
	  Name.VarMap.foldli (fn (v,(lab,phrase_class),sdecs) =>
			      case phrase_class
				of PHRASE_CLASS_EXP(exp,con) => SDEC(lab,DEC_EXP(v,con))::sdecs
			         | PHRASE_CLASS_CON(con,kind) => SDEC(lab,DEC_CON(v,kind,SOME con))::sdecs
				 | PHRASE_CLASS_MOD(m,signat) => SDEC(lab,DEC_MOD(v,signat))::sdecs
				 | PHRASE_CLASS_SIG _ => sdecs
				 | PHRASE_CLASS_OVEREXP _ => sdecs) [] (#1 var_list)

      (* faster when first context is larger than second *)
      fun plus (csubster,ksubster,ssubster,orig_ctxt, 
		ctxt2 as CONTEXT{flatlist, fixity_list, label_list, var_list, tag_list}) =
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
				| PHRASE_CLASS_MOD(m,signat) => add_context_sdec(ctxt,SDEC(lab,DEC_MOD(v,do_s signat)))
				| PHRASE_CLASS_SIG s => add_context_entry(ctxt,CONTEXT_SIGNAT(lab,v,do_s s))
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


    fun pp_list doer objs (left,sep,right,break) = 
      let 
	  open Formatter
	  fun loop [] = [String right]
	    | loop [a] = [doer a, String right]
	    | loop (a::rest) = (doer a) :: (String sep) :: Break :: (loop rest)
	  val fmts = (String left) :: (loop objs)
      in (if break then Vbox0 else HOVbox0 1) (size left) 1 fmts
      end
    fun print_context ({pp_exp, pp_mod,
			pp_con, pp_fixity_list, pp_inline, 
			pp_kind, pp_label, pp_var, pp_tag, pp_signat},
		       CONTEXT{label_list,...}) = 
	let open Formatter
	    val label_pathpc_list = Name.LabelMap.listItemsi label_list
	    fun pp_path path = 
		(case path of
		     SIMPLE_PATH v => pp_var v
		   | COMPOUND_PATH (v,ls) => HOVbox[Hbox[pp_var v, String "."], 
						    pp_list pp_label ls ("",".","",false)])
	    fun pp_xpc (PHRASE_CLASS_EXP (e,c)) = HOVbox[pp_exp e, String " : ", pp_con c]
	      | pp_xpc (PHRASE_CLASS_CON (c,k)) = HOVbox[pp_con c, String " : ", pp_kind k]
	      | pp_xpc (PHRASE_CLASS_MOD (m,s)) = HOVbox[pp_mod m, String " : ", pp_signat s]
	      | pp_xpc (PHRASE_CLASS_SIG s) = pp_signat s
	      | pp_xpc (PHRASE_CLASS_OVEREXP oe) = String "OVEREXP_NOTDONE"
	    fun doer(lbl,(path,xpc)) = HOVbox[pp_label lbl,
					   String " --> ",
					   pp_path path,
					   String " = ",
					   pp_xpc xpc]
	in  pp_list doer label_pathpc_list ("[",", ", "]", true)
	end



    local 
	open Blaster
	val indent = ref 0 
	fun push() = indent := (!indent) + 2
	fun pop() = indent := (!indent) - 2
	fun tab s = if (!blast_debug)
			then let fun loop 0 = print s
				   | loop n = (print " "; loop (n-1))
			     in  loop (!indent)
			     end
		    else ()
	fun say s = if (!blast_debug) then print s else ()

	fun blastOutChar os c = TextIO.output1(os,c)
	fun blastInChar is =  (case TextIO.input1 is of
				   SOME c => c
				 | NONE => error "premature end of file in input1")

	fun blastOutChoice os i = if (!useOldBlast)
				      then blastOutInt os i
				  else blastOutChar os (chr i)
	fun blastInChoice is = if (!useOldBlast)
				   then blastInInt is
			       else ord(blastInChar is)
    in

	fun blastOutPath os (SIMPLE_PATH v) = (blastOutChoice os 0; blastOutVar os v)
	  | blastOutPath os (COMPOUND_PATH (v,ls)) = (blastOutChoice os 1; blastOutVar os v; 
						      blastOutList blastOutLabel os ls)
	fun blastInPath is = let val _ = tab "blastInPath:" 
				 val which = blastInChoice is
				 val v = blastInVar is
				 val res = if (which = 0)
					       then SIMPLE_PATH v
					   else COMPOUND_PATH(v, blastInList blastInLabel is)
				 val _ = (case res of
					      SIMPLE_PATH v => (say (Name.var2string v))
					    | COMPOUND_PATH (v,ls) => (say (Name.var2string v);
								       app (fn l => (say ".";
										     say (Name.label2string l)))
								       ls))
				 val _ = say "\n"
			     in res
			     end

	fun blastOutArrow os TOTAL = blastOutChoice os 0
	  | blastOutArrow os PARTIAL = blastOutChoice os 1
	fun blastInArrow is =
	    (case (blastInChoice is) of
		 0 => TOTAL
	       | 1 => PARTIAL
	       | _ => error "bad blastInArrow")

	fun blastOutIS os Prim.W8 = blastOutChoice os 0
	  | blastOutIS os Prim.W16 = blastOutChoice os 1
	  | blastOutIS os Prim.W32 = blastOutChoice os 2
	  | blastOutIS os Prim.W64 = blastOutChoice os 3
	fun blastInIS is =
	    (case blastInChoice is of
		 0 => Prim.W8
	       | 1 => Prim.W16
	       | 2 => Prim.W32
	       | 3 => Prim.W64
	       | _ => (error "bad blastInIS" handle e => raise e))
	fun blastOutFS os Prim.F32 = blastOutChoice os 0
	  | blastOutFS os Prim.F64 = blastOutChoice os 1
	fun blastInFS is =
	    (case blastInChoice is of
		 0 => Prim.F32
	       | 1 => Prim.F64
	       | _ => error "bad blastInFS")

	fun blastOutDec os dec = 
	    (case dec of
		 DEC_EXP (v,c) => (blastOutChoice os 0; blastOutVar os v; blastOutCon os c)
	       | DEC_CON (v,k,NONE) => (blastOutChoice os 1; blastOutVar os v; blastOutKind os k)
	       | DEC_CON (v,k,SOME c) => (blastOutChoice os 2; blastOutVar os v; blastOutKind os k; blastOutCon os c)
	       | DEC_MOD (v,s) => (blastOutChoice os 3; blastOutVar os v; blastOutSig os s)
	       | DEC_EXCEPTION (t,c) =>  (blastOutChoice os 4; blastOutTag os t; blastOutCon os c))
	and blastInDec is =
	    (case (blastInChoice is) of
		 0 => DEC_EXP (blastInVar is, blastInCon is)
	       | 1 => DEC_CON (blastInVar is, blastInKind is, NONE)
	       | 2 => DEC_CON (blastInVar is, blastInKind is, SOME (blastInCon is))
	       | 3 => DEC_MOD (blastInVar is, blastInSig is)
	       | 4 => DEC_EXCEPTION (blastInTag is, blastInCon is)
	       | _ => error "bad blastInDec")
	and blastOutBnd os bnd = 
	    (case bnd of
		 BND_EXP (v,e) => (blastOutChoice os 0; blastOutVar os v; blastOutExp os e)
	       | BND_CON (v,c) => (blastOutChoice os 1; blastOutVar os v; blastOutCon os c)
	       | BND_MOD (v,m) => (blastOutChoice os 2; blastOutVar os v; blastOutMod os m))
	and blastInBnd is =
	    (case (blastInChoice is) of
		 0 => BND_EXP(blastInVar is, blastInExp is)
	       | 1 => BND_CON(blastInVar is, blastInCon is)
	       | 2 => BND_MOD(blastInVar is, blastInMod is)
	       | _ => error "bad blastInBnd")
	and blastOutSdec os (SDEC(l,dec)) = (blastOutLabel os l; blastOutDec os dec)
	and blastInSdec is = SDEC(blastInLabel is, blastInDec is)
	and blastOutSbnd os (SBND(l,bnd)) = (blastOutLabel os l; blastOutBnd os bnd)
	and blastInSbnd is = SBND(blastInLabel is, blastInBnd is)

	and blastOutSdecs os sdecs = blastOutList blastOutSdec os sdecs
	and blastInSdecs is = blastInList blastInSdec is
	and blastOutSbnds os sbnds = blastOutList blastOutSbnd os sbnds
	and blastInSbnds is = blastInList blastInSbnd is

	and blastOutKind os k = 
	    (case k of
		KIND_TUPLE n => (blastOutChoice os 0; blastOutChoice os n)
	      | KIND_ARROW (m,n) => (blastOutChoice os 1; blastOutChoice os m;  blastOutChoice os n)
	      | KIND_INLINE (k,c) => (blastOutChoice os 2; blastOutKind os k; blastOutCon os c))
		    
	and blastInKind is = 
	    (case blastInChoice is of
		0 => KIND_TUPLE(blastInChoice is)
	      | 1 => KIND_ARROW(blastInChoice is, blastInChoice is)
	      | 2 => KIND_INLINE(blastInKind is, blastInCon is)
	      | _ => error "bad blastInKind")

	and blastOutCon os c = 
	    (case c of
		 CON_VAR v => (blastOutChoice os 0; blastOutVar os v)
	       | CON_TYVAR tv => (case Tyvar.tyvar_deref tv of
				      SOME c => blastOutCon os c
				    | NONE => error "cannot blastOut unresolved CON_TYVAR")
	       | CON_OVAR oc => blastOutCon os (CON_TYVAR (Tyvar.ocon_deref oc))
	       | CON_FLEXRECORD (ref (INDIRECT_FLEXINFO r)) => blastOutCon os (CON_FLEXRECORD r)
	       | CON_FLEXRECORD (ref (FLEXINFO (_, true, lclist))) => blastOutCon os (CON_RECORD lclist)
	       | CON_FLEXRECORD (ref (FLEXINFO (_, false, _))) => error "cannot blastOut flex record type"
	       | CON_INT is => (blastOutChoice os 1; blastOutIS os is)
	       | CON_UINT is => (blastOutChoice os 2; blastOutIS os is)
	       | CON_FLOAT fs => (blastOutChoice os 3; blastOutFS os fs)
	       | CON_ARRAY c => (blastOutChoice os 4; blastOutCon os c)
	       | CON_VECTOR c => (blastOutChoice os 5; blastOutCon os c)
	       | CON_ANY => (blastOutChoice os 6)
	       | CON_REF c => (blastOutChoice os 7; blastOutCon os c)
	       | CON_TAG c => (blastOutChoice os 8; blastOutCon os c)
	       | CON_ARROW (cs,c,f,oa) => (blastOutChoice os 9; blastOutList blastOutCon os cs;
					   blastOutCon os c; blastOutBool os f;
					   blastOutArrow os (case (oneshot_deref oa) of
								 SOME a => a
							       | _ => error "unresolved CON_ARROW"))
	       | CON_APP (c1,c2) => (blastOutChoice os 10; blastOutCon os c1; blastOutCon os c2)
	       | CON_MUPROJECT (i,c) => (blastOutChoice os 11; blastOutChoice os i; blastOutCon os c)
	       | CON_RECORD lclist => (blastOutChoice os 12; blastOutList (blastOutPair blastOutLabel blastOutCon) os lclist)
	       | CON_FUN (vlist, c) => (blastOutChoice os 13; blastOutList blastOutVar os vlist; blastOutCon os c)
	       | CON_SUM {noncarriers, carriers, special = NONE} => 
		     (blastOutChoice os 14; blastOutChoice os noncarriers; 
		      blastOutList blastOutCon os carriers)
	       | CON_SUM {noncarriers, carriers, special = SOME i} => 
		     (blastOutChoice os 15; blastOutChoice os noncarriers; 
		      blastOutList blastOutCon os carriers; blastOutChoice os i)
	       | CON_TUPLE_INJECT clist => (blastOutChoice os 16; blastOutList blastOutCon os clist)
	       | CON_TUPLE_PROJECT (i,c) => (blastOutChoice os 17; blastOutChoice os i; blastOutCon os c)
	       | CON_MODULE_PROJECT (m,l) => (blastOutChoice os 18; blastOutMod os m; blastOutLabel os l))

        and blastInCon is = 
	    let val _ = push()
		val _ = tab "blastInCon\n"
		val res = blastInCon' is
		val _ = pop()
	    in  res
	    end

	and blastInCon' is = 
	    (case (blastInChoice is) of
		 0 => CON_VAR (blastInVar is)
	       | 1 => CON_INT (blastInIS is)
	       | 2 => CON_UINT (blastInIS is)
	       | 3 => CON_FLOAT (blastInFS is)
	       | 4 => CON_ARRAY (blastInCon is)
	       | 5 => CON_VECTOR (blastInCon is)
	       | 6 => CON_ANY
	       | 7 => CON_REF (blastInCon is)
	       | 8 => CON_TAG (blastInCon is)
	       | 9 => let val _ = tab "  ARROW case\n"
			  val cs = blastInList blastInCon is
			  val _ = tab "  ARROW done cs\n"
			  val c = blastInCon is
			  val _ = tab " ARROW done c\n"
			  val f = blastInBool is
			  val _ = tab "  ARROW done f\n"
			  val a = oneshot_init (blastInArrow is)
			  val _ = tab "  ARROW done a\n"
		      in 
			  CON_ARROW (cs,c,f,a) (* (blastInList blastInCon is,
				     blastInCon is, blastInBool is,
				     oneshot_init (blastInArrow is))) *)
		      end
	       | 10 => CON_APP (blastInCon is, blastInCon is)
	       | 11 => CON_MUPROJECT (blastInChoice is, blastInCon is)
	       | 12 => CON_RECORD(blastInList (blastInPair blastInLabel blastInCon) is)
	       | 13 => CON_FUN (blastInList blastInVar is, blastInCon is)
	       | 14 => CON_SUM {noncarriers = blastInChoice is,
				carriers = blastInList blastInCon is,
				special = NONE} 
	       | 15 => CON_SUM {noncarriers = blastInChoice is,
				carriers = blastInList blastInCon is,
				special = SOME (blastInChoice is)}
	       | 16 => CON_TUPLE_INJECT (blastInList blastInCon is)
	       | 17 => CON_TUPLE_PROJECT (blastInChoice is, blastInCon is)
	       | 18 => CON_MODULE_PROJECT (blastInMod is, blastInLabel is)
	       | _ => error "bad blastInCon")

	and blastOutValue os v = 
	    (case v of
		 (Prim.int (is,w64)) => (blastOutChoice os 0; blastOutIS os is; blastOutWord64 os w64)
	       | (Prim.uint (is,w64)) => (blastOutChoice os 1; blastOutIS os is; blastOutWord64 os w64)
	       | (Prim.float (fs,str)) => (blastOutChoice os 2; blastOutFS os fs; blastOutString os str)
	       | (Prim.tag (t,c)) => (blastOutChoice os 3; blastOutTag os t; blastOutCon os c)
	       | _ => error "blasting of array/vector/refcell not supported")

	and blastInValue is =
	    (case (blastInChoice is) of
		 0 => Prim.int (blastInIS is, blastInWord64 is)
	       | 1 => Prim.uint (blastInIS is, blastInWord64 is)
	       | 2 => Prim.float (blastInFS is, blastInString is)
	       | 3 => Prim.tag (blastInTag is, blastInCon is)
	       | _ => error "bad blastInValue")

	and blastOutIlPrim os ilprim = 
	    let open Prim
	    in  (case ilprim of
		     eq_uint is => (blastOutChoice os 0; blastOutIS os is)
		   | neq_uint is => (blastOutChoice os 1; blastOutIS os is)
		   | not_uint is => (blastOutChoice os 2; blastOutIS os is)
		   | and_uint is => (blastOutChoice os 3; blastOutIS os is)
		   | or_uint is => (blastOutChoice os 4; blastOutIS os is)
		   | lshift_uint is => (blastOutChoice os 5; blastOutIS os is))
	    end

	and blastInIlPrim is = 
	    let open Prim
	    in  (case (blastInChoice is) of
		     0 => eq_uint(blastInIS is)
		   | 1 => neq_uint(blastInIS is)
		   | 2 => not_uint(blastInIS is)
		   | 3 => and_uint(blastInIS is)
		   | 4 => or_uint(blastInIS is)
		   | 5 => lshift_uint(blastInIS is)
		   | _ => error "bad blastInIlPrim")
	    end


	and blastOutTT os tt =
	    let open Prim 
	    in  (case tt of
		     int_tt => blastOutChoice os 0
		   | real_tt => blastOutChoice os 1
		   | both_tt => blastOutChoice os 2)
	    end


	and blastInTT is =
	    let open Prim 
	    in  (case (blastInChoice is) of
		     0 => int_tt
		   | 1 => real_tt
		   | 2 => both_tt
		   | _ => error "bad blastInTT")
	    end
			 
	and blastOutTable os table = 
	    let open Prim 
	    in  (case table of
		     IntArray is => (blastOutChoice os 0; blastOutIS os is)
		   | IntVector is => (blastOutChoice os 1; blastOutIS os is)
		   | FloatArray fs => (blastOutChoice os 2; blastOutFS os fs)
		   | FloatVector fs => (blastOutChoice os 3; blastOutFS os fs)
		   | PtrArray => (blastOutChoice os 4)
		   | PtrVector => (blastOutChoice os 5)
		   | WordArray => (blastOutChoice os 6)
		   | WordVector => (blastOutChoice os 7))
	    end

	and blastInTable is =
	    let open Prim 
	    in  (case (blastInChoice is) of
		     0 => IntArray (blastInIS is)
		   | 1 => IntVector (blastInIS is)
		   | 2 => FloatArray (blastInFS is)
		   | 3 => FloatVector (blastInFS is)
		   | 4 => PtrArray
		   | 5 => PtrVector
		   | 6 => WordArray
		   | 7 => WordVector
		   | _ => error "bad blastInTable")
	    end

	and blastOutPrim os prim = 
	    let open Prim
	    in  (case prim of
		     soft_vtrap tt => (blastOutChoice os 0; blastOutTT os tt)
		   | soft_ztrap tt => (blastOutChoice os 1; blastOutTT os tt)
		   | hard_vtrap tt => (blastOutChoice os 2; blastOutTT os tt)
		   | hard_ztrap tt => (blastOutChoice os 3; blastOutTT os tt)
			 
		   | mk_ref => (blastOutChoice os 4)
		   | deref => (blastOutChoice os 5)

		       (* conversions amongst floats, ints, uints with w32 and f64 *)
		   | float2int (* floor *) => (blastOutChoice os 6)
		   | int2float (* real  *) => (blastOutChoice os 7)
		   | int2uint (is1,is2) => (blastOutChoice os 8; blastOutIS os is1; blastOutIS os is2)
		   | uint2int (is1,is2) => (blastOutChoice os 9; blastOutIS os is1; blastOutIS os is2)
		   | uinta2uinta (is1,is2) => (blastOutChoice os 10; blastOutIS os is1; blastOutIS os is2)
		   | uintv2uintv (is1,is2) => (blastOutChoice os 11; blastOutIS os is1; blastOutIS os is2)

		   (* ref operation *)
		   | eq_ref => (blastOutChoice os 12)
		   | setref => (blastOutChoice os 13)

		   (* floatint-point operations *)	
		   | neg_float fs  => (blastOutChoice os 14; blastOutFS os fs)
		   | abs_float fs  => (blastOutChoice os 15; blastOutFS os fs)
		   | plus_float fs  => (blastOutChoice os 16; blastOutFS os fs)
		   | minus_float fs  => (blastOutChoice os 17; blastOutFS os fs)
		   | mul_float fs  => (blastOutChoice os 18; blastOutFS os fs)
		   | div_float fs  => (blastOutChoice os 19; blastOutFS os fs)
		   | less_float fs  => (blastOutChoice os 20; blastOutFS os fs)
		   | greater_float fs  => (blastOutChoice os 21; blastOutFS os fs)
		   | lesseq_float fs  => (blastOutChoice os 22; blastOutFS os fs)
		   | greatereq_float  fs  => (blastOutChoice os 23; blastOutFS os fs)
		   | eq_float  fs  => (blastOutChoice os 24; blastOutFS os fs)
		   | neq_float fs  => (blastOutChoice os 25; blastOutFS os fs)

		   (* int operations *)
		   | plus_int is  => (blastOutChoice os 26; blastOutIS os is)
		   | minus_int is  => (blastOutChoice os 27; blastOutIS os is)
		   | mul_int is  => (blastOutChoice os 28; blastOutIS os is)
		   | div_int is  => (blastOutChoice os 29; blastOutIS os is)
		   | mod_int is  => (blastOutChoice os 30; blastOutIS os is)
		   | quot_int is  => (blastOutChoice os 31; blastOutIS os is)
		   | rem_int is  => (blastOutChoice os 32; blastOutIS os is)
		   | plus_uint is  => (blastOutChoice os 33; blastOutIS os is)
		   | minus_uint is  => (blastOutChoice os 34; blastOutIS os is)
		   | mul_uint is  => (blastOutChoice os 35; blastOutIS os is)
		   | div_uint is  => (blastOutChoice os 36; blastOutIS os is)
		   | mod_uint is  => (blastOutChoice os 37; blastOutIS os is)
		   | less_int is  => (blastOutChoice os 38; blastOutIS os is)
		   | greater_int is  => (blastOutChoice os 39; blastOutIS os is)
		   | lesseq_int is  => (blastOutChoice os 40; blastOutIS os is)
		   | greatereq_int is  => (blastOutChoice os 41; blastOutIS os is)
		   | less_uint is  => (blastOutChoice os 42; blastOutIS os is)
		   | greater_uint is  => (blastOutChoice os 43; blastOutIS os is)
		   | lesseq_uint is  => (blastOutChoice os 44; blastOutIS os is)
		   | greatereq_uint is  => (blastOutChoice os 45; blastOutIS os is)
		   | eq_int is  => (blastOutChoice os 46; blastOutIS os is)
		   | neq_int is  => (blastOutChoice os 47; blastOutIS os is)
		   | neg_int is  => (blastOutChoice os 48; blastOutIS os is)
		   | abs_int is  => (blastOutChoice os 49; blastOutIS os is)

		   (* bit-pattern manipulation *)
		   | not_int is  => (blastOutChoice os 50; blastOutIS os is)
		   | and_int is  => (blastOutChoice os 51; blastOutIS os is)
		   | or_int is  => (blastOutChoice os 52; blastOutIS os is)
		   | lshift_int is  => (blastOutChoice os 53; blastOutIS os is)
		   | rshift_int is  => (blastOutChoice os 54; blastOutIS os is)
		   | rshift_uint is  => (blastOutChoice os 55; blastOutIS os is)
			 
		   (* array and vectors *)
		   | array2vector t => (blastOutChoice os 56; blastOutTable os t)
		   | create_table t => (blastOutChoice os 57; blastOutTable os t)
		   | sub t => (blastOutChoice os 58; blastOutTable os t)
		   | update t => (blastOutChoice os 59; blastOutTable os t)
		   | length_table t => (blastOutChoice os 60; blastOutTable os t)
		   | equal_table t => (blastOutChoice os 61; blastOutTable os t)

		   (* IO operations *)
		   | open_in => (blastOutChoice os 62)
		   | input => (blastOutChoice os 63)
		   | input1 => (blastOutChoice os 64)
		   | lookahead => (blastOutChoice os 65)
		   | open_out => (blastOutChoice os 66)
		   | close_in => (blastOutChoice os 67)
		   | output => (blastOutChoice os 68)
		   | flush_out => (blastOutChoice os 69)
		   | close_out => (blastOutChoice os 70)
		   | end_of_stream => (blastOutChoice os 71))

	    end

	and blastInPrim is =
	    let open Prim
	    in  (case (blastInChoice is) of
		     0 => soft_vtrap(blastInTT is)
		   | 1 => soft_ztrap(blastInTT is)
		   | 2 => hard_vtrap(blastInTT is)
		   | 3 => hard_ztrap(blastInTT is)
			 
		   | 4 => mk_ref
		   | 5 => deref

		       (* conversions amongst floats, ints, uints with w32 and f64 *)
		   | 6 => float2int (* floor *)
		   | 7 => int2float (* real  *)
		   | 8 => int2uint (blastInIS is, blastInIS is)
		   | 9 => uint2int(blastInIS is, blastInIS is)
		   | 10 => uinta2uinta(blastInIS is, blastInIS is)
		   | 11 => uintv2uintv(blastInIS is, blastInIS is)

		   (* ref operation *)
		   | 12 => eq_ref
		   | 13 => setref

		   (* floatint-point operations *)	
		   | 14 => neg_float(blastInFS is)
		   | 15 => abs_float(blastInFS is)
		   | 16 => plus_float(blastInFS is)
		   | 17 => minus_float(blastInFS is)
		   | 18 => mul_float(blastInFS is)
		   | 19 => div_float(blastInFS is)
		   | 20 => less_float(blastInFS is)
		   | 21 => greater_float(blastInFS is)
		   | 22 => lesseq_float(blastInFS is)
		   | 23 => greatereq_float (blastInFS is)
		   | 24 => eq_float (blastInFS is)
		   | 25 => neq_float(blastInFS is)

		   (* int operations *)
		   | 26 => plus_int(blastInIS is)
		   | 27 => minus_int(blastInIS is)
		   | 28 => mul_int(blastInIS is)
		   | 29 => div_int(blastInIS is)
		   | 30 => mod_int(blastInIS is)
		   | 31 => quot_int(blastInIS is)
		   | 32 => rem_int(blastInIS is)
		   | 33 => plus_uint(blastInIS is)
		   | 34 => minus_uint(blastInIS is)
		   | 35 => mul_uint(blastInIS is)
		   | 36 => div_uint(blastInIS is)
		   | 37 => mod_uint(blastInIS is)
		   | 38 => less_int(blastInIS is)
		   | 39 => greater_int(blastInIS is)
		   | 40 => lesseq_int(blastInIS is)
		   | 41 => greatereq_int(blastInIS is)
		   | 42 => less_uint(blastInIS is)
		   | 43 => greater_uint(blastInIS is)
		   | 44 => lesseq_uint(blastInIS is)
		   | 45 => greatereq_uint(blastInIS is)
		   | 46 => eq_int(blastInIS is)
		   | 47 => neq_int(blastInIS is)
		   | 48 => neg_int(blastInIS is)
		   | 49 => abs_int(blastInIS is)

		   (* bit-pattern manipulation *)
		   | 50 => not_int(blastInIS is)
		   | 51 => and_int(blastInIS is)
		   | 52 => or_int(blastInIS is)
		   | 53 => lshift_int(blastInIS is)
		   | 54 => rshift_int(blastInIS is)
		   | 55 => rshift_uint(blastInIS is)
			 
		   (* array and vectors *)
		   | 56 => array2vector (blastInTable is)
		   | 57 => create_table (blastInTable is)
		   | 58 => sub (blastInTable is)
		   | 59 => update (blastInTable is)
		   | 60 => length_table (blastInTable is)
		   | 61 => equal_table (blastInTable is)

		   (* IO operations *)
		   | 62 => open_in
		   | 63 => input
		   | 64 => input1
		   | 65 => lookahead
		   | 66 => open_out
		   | 67 => close_in
		   | 68 => output
		   | 69 => flush_out
		   | 70 => close_out
		   | 71 => end_of_stream
		   | _ => error "bad blastInPrim")

	    end

	and blastOutExp os exp = 
	    (case exp of
		 OVEREXP (_,_,oe) => (case oneshot_deref oe of
					  SOME e => blastOutExp os e
					| NONE => error "cannot blastOut unresolved OVEREXP")
	       | SCON v => (blastOutChoice os 0; blastOutValue os v)
	       | PRIM (p,clist,elist) => (blastOutChoice os 1; blastOutPrim os p;
					  blastOutList blastOutCon os clist;
					  blastOutList blastOutExp os elist)
	       | ILPRIM (p,clist,elist) => (blastOutChoice os 2; blastOutIlPrim os p;
					    blastOutList blastOutCon os clist;
					    blastOutList blastOutExp os elist)
	       | ETAPRIM (p,clist) => (blastOutChoice os 3; blastOutPrim os p;
				       blastOutList blastOutCon os clist)
	       | ETAILPRIM (p,clist) => (blastOutChoice os 4; blastOutIlPrim os p;
					 blastOutList blastOutCon os clist)
	       | VAR v => (blastOutChoice os 5; blastOutVar os v)
	       | APP (e,elist) => (blastOutChoice os 6; blastOutExp os e; blastOutList blastOutExp os elist)
	       | FIX (b,a,fbnds) => (blastOutChoice os 7; blastOutBool os b; blastOutArrow os a;
				     blastOutList blastOutFbnd os fbnds)
	       | RECORD lelist => (blastOutChoice os 8; blastOutList (blastOutPair blastOutLabel blastOutExp) os lelist)
	       | RECORD_PROJECT (e,l,c) => (blastOutChoice os 9; blastOutExp os e; blastOutLabel os l; blastOutCon os c)
	       | SUM_TAIL (c,e) => (blastOutChoice os 10; blastOutCon os c; blastOutExp os e)
	       | HANDLE (e1,e2) => (blastOutChoice os 11; blastOutExp os e1; blastOutExp os e2)
	       | RAISE (c,e) => (blastOutChoice os 12; blastOutCon os c; blastOutExp os e)
	       | LET(bnds,e) => (blastOutChoice os 13; blastOutList blastOutBnd os bnds; blastOutExp os e)
	       | NEW_STAMP c => (blastOutChoice os 14; blastOutCon os c)
	       | EXN_INJECT (str,e1,e2) => (blastOutChoice os 15; blastOutString os str; blastOutExp os e1; blastOutExp os e2)
	       | ROLL (c,e) => (blastOutChoice os 16; blastOutCon os c; blastOutExp os e)
	       | UNROLL (c,e) => (blastOutChoice os 17; blastOutCon os c; blastOutExp os e)
	       | INJ {noncarriers, carriers, special, inject} =>
		     (blastOutChoice os 18; blastOutChoice os noncarriers; blastOutList blastOutCon os carriers;
		      blastOutChoice os special; blastOutOption blastOutExp os inject)
	       | CASE {noncarriers, carriers, arg, arms, tipe, default} => 
		      (blastOutChoice os 19;
		       blastOutChoice os noncarriers; 
		       blastOutList blastOutCon os carriers;
		       blastOutExp os arg;
		       blastOutList (blastOutOption blastOutExp) os arms;
		       blastOutCon os tipe;
		       blastOutOption blastOutExp os default)
	       | EXN_CASE {arg, arms, default, tipe} => 
		      (blastOutChoice os 20;
		       blastOutExp os arg;
		       blastOutList (blastOutTriple blastOutExp blastOutCon blastOutExp) os arms;
		       blastOutOption blastOutExp os default;
		       blastOutCon os tipe)
	       | MODULE_PROJECT (m,l) => (blastOutChoice os 21; blastOutMod os m; blastOutLabel os l)
	       | SEAL(e,c) => (blastOutChoice os 22; blastOutExp os e; blastOutCon os c))

        and blastInExp is = 
	    let val _ = push()
		val _ = tab "blastInExp\n" 
		val res = blastInExp' is
		val _ = pop()
	    in  res
	    end

	and blastInExp' is =
	     (case (blastInChoice is) of
	         0 => (tab "  SCON\n"; 
		       SCON(blastInValue is))
	       | 1 => (tab "  PRIM\n"; 
		       PRIM (blastInPrim is,
			    blastInList blastInCon is,
			    blastInList blastInExp is))
	       | 2 => ILPRIM (blastInIlPrim is,
			      blastInList blastInCon is,
			      blastInList blastInExp is)
	       | 3 => ETAPRIM (blastInPrim is,
			       blastInList blastInCon is)
	       | 4 => ETAILPRIM (blastInIlPrim is,
				 blastInList blastInCon is)
	       | 5 => let val _ = tab "  VAR"
			  val v = blastInVar is
			  val _ = (say (Name.var2string v); say "\n")
		      in  VAR v
		      end
	       | 6 => APP (blastInExp is, blastInList blastInExp is)
	       | 7 => FIX (blastInBool is, blastInArrow is, blastInList blastInFbnd is)
	       | 8 => RECORD (blastInList (blastInPair blastInLabel blastInExp) is)
	       | 9 => RECORD_PROJECT (blastInExp is, blastInLabel is, blastInCon is)
	       | 10 => SUM_TAIL (blastInCon is, blastInExp is)
	       | 11 => HANDLE (blastInExp is, blastInExp is)
	       | 12 => RAISE (blastInCon is, blastInExp is)
	       | 13 => LET(blastInList blastInBnd is, blastInExp is)
	       | 14 => NEW_STAMP (blastInCon is)
	       | 15 => EXN_INJECT (blastInString is, blastInExp is, blastInExp is)
	       | 16 => ROLL (blastInCon is, blastInExp is)
	       | 17 => UNROLL (blastInCon is, blastInExp is)
	       | 18 => let val _ = tab "  INJ\n"
			   val noncarriers = blastInChoice is
			   val _ = tab "  done noncarriers\n"
			   val carriers = blastInList blastInCon is
			   val _ = tab "  done carriers\n"
			   val special = blastInChoice is
			   val _ = tab "  done special\n"
			   val inject = blastInOption blastInExp is
			   val _ = tab "  done inject\n"
		       in  INJ {noncarriers = noncarriers,
				carriers = carriers,
				special = special,
				inject = inject}
		       end
	       | 19 => (tab "  CASE\n";
			CASE {noncarriers = blastInChoice is,
			     carriers = blastInList blastInCon is,
			     arg = blastInExp is, 
			     arms = blastInList (blastInOption blastInExp) is,
			     tipe = blastInCon is, 
			     default = blastInOption blastInExp is})
	       | 20 => (tab "  EXN_CASE\n";
			EXN_CASE {arg = blastInExp is, 
				 arms = blastInList (blastInTriple blastInExp blastInCon blastInExp) is,
				 default = blastInOption blastInExp is, 
				 tipe = blastInCon is})
	       | 21 => MODULE_PROJECT (blastInMod is, blastInLabel is)
	       | 22 => SEAL(blastInExp is, blastInCon is)
	       | _ => error "bad blastInExp")

	and blastOutFbnd os (FBND(v1,v2,c1,c2,e)) = (blastOutVar os v1;
						     blastOutVar os v2;
						     blastOutCon os c1;
						     blastOutCon os c2;
						     blastOutExp os e)
	and blastInFbnd is = let val _ = tab "blastInFbnd"
				 val v = blastInVar is
				 val _ = (say (Name.var2string v); say "\n")    
				 val _ = push()
				 val res = FBND(v, blastInVar is,
						blastInCon is, blastInCon is, blastInExp is)
				 val _ = pop()
			     in  res
			     end
				  

	and blastOutMod os m = 
	    (case m of
		 MOD_VAR v => (blastOutChoice os 0; blastOutVar os v)
	       | MOD_STRUCTURE sbnds => (blastOutChoice os 1; blastOutSbnds os sbnds)
	       | MOD_FUNCTOR (v,s,m) => (blastOutChoice os 2; blastOutVar os v; blastOutSig os s; blastOutMod os m)
	       | MOD_APP (m1,m2) => (blastOutChoice os 3; blastOutMod os m1; blastOutMod os m2)
	       | MOD_PROJECT (m,l) => (blastOutChoice os 4; blastOutMod os m; blastOutLabel os l)
	       | MOD_SEAL (m,s) => (blastOutChoice os 5; blastOutMod os m; blastOutSig os s)
	       | MOD_LET (v,m1,m2) => (blastOutChoice os 6; blastOutVar os v; blastOutMod os m1; blastOutMod os m2))

        and blastInMod is = 
	    let val _ = push()
		val _ = tab "blastInMod\n"
		val res = blastInMod' is
		val _ = pop()
	    in  res
	    end

	and blastInMod' is =
	    (
		     case (blastInChoice is) of
		 0 => MOD_VAR(blastInVar is)
	       | 1 => MOD_STRUCTURE(blastInSbnds is)
	       | 2 => MOD_FUNCTOR(blastInVar is, blastInSig is, blastInMod is)
	       | 3 => MOD_APP (blastInMod is, blastInMod is)
	       | 4 => MOD_PROJECT (blastInMod is, blastInLabel is)
	       | 5 => MOD_SEAL (blastInMod is, blastInSig is)
	       | 6 => MOD_LET (blastInVar is, blastInMod is, blastInMod is)
	       | _ => error "bad blastInMod")

	and blastOutSig os s = 
		 (tab "    blastInSig\n"; 
		  case s of
		 SIGNAT_STRUCTURE (NONE, sdecs) => (blastOutChoice os 0; blastOutSdecs os sdecs)
	       | SIGNAT_STRUCTURE (SOME p, sdecs) => (blastOutChoice os 1; blastOutPath os p; blastOutSdecs os sdecs)
	       | SIGNAT_FUNCTOR(v, s1, s2, arrow) => (blastOutChoice os 2; blastOutVar os v;
						      blastOutSig os s1; blastOutSig os s2; blastOutArrow os arrow)
	       | SIGNAT_INLINE_STRUCTURE {self=NONE,code,imp_sig,abs_sig} => 
		     (blastOutChoice os 3; blastOutSbnds os code; blastOutSdecs os imp_sig; blastOutSdecs os abs_sig)
	       | SIGNAT_INLINE_STRUCTURE {self=SOME p,code,imp_sig,abs_sig} => 
		     (blastOutChoice os 4; blastOutPath os p;
		      blastOutSbnds os code; blastOutSdecs os imp_sig; blastOutSdecs os abs_sig))

	and blastInSig is =
	    (case (blastInChoice is) of
		 0 => SIGNAT_STRUCTURE (NONE, blastInSdecs is)
	       | 1 => SIGNAT_STRUCTURE (SOME (blastInPath is), blastInSdecs is)
	       | 2 => SIGNAT_FUNCTOR(blastInVar is, blastInSig is, blastInSig is, blastInArrow is)
	       | 3 => SIGNAT_INLINE_STRUCTURE {self=NONE, code = blastInSbnds is, 
					       imp_sig = blastInSdecs is, abs_sig = blastInSdecs is}
	       | 4 => SIGNAT_INLINE_STRUCTURE {self=SOME(blastInPath is), code = blastInSbnds is, 
					       imp_sig = blastInSdecs is, abs_sig = blastInSdecs is}
	       | _ => error "bad blastInSig")
				     
	fun blastOutPC os pc = 
	    case pc of
		PHRASE_CLASS_EXP (e,c) => (blastOutChoice os 0; blastOutExp os e; blastOutCon os c)
	      | PHRASE_CLASS_CON (c,k) => (blastOutChoice os 1; blastOutCon os c; blastOutKind os k)
	      | PHRASE_CLASS_MOD (m,s) => (blastOutChoice os 2; blastOutMod os m; blastOutSig os s)
	      | PHRASE_CLASS_SIG s => (blastOutChoice os 3; blastOutSig os s)
	      | PHRASE_CLASS_OVEREXP celist => (blastOutChoice os 4; 
						blastOutList (blastOutPair blastOutCon blastOutExp) os celist)

	fun blastInPC is = 
	    (tab "  blastInPC\n"; 
	    case (blastInChoice is) of
		0 => PHRASE_CLASS_EXP(blastInExp is, blastInCon is)
	      | 1 => PHRASE_CLASS_CON(blastInCon is, blastInKind is)
	      | 2 => PHRASE_CLASS_MOD(blastInMod is, blastInSig is)
	      | 3 => PHRASE_CLASS_SIG(blastInSig is)
	      | 4 => PHRASE_CLASS_OVEREXP(blastInList (blastInPair blastInCon blastInExp) is)
	      | _ => error "bad blastInPC")

	fun blastOutFixity os Fixity.NONfix = blastOutChoice os 0
	  | blastOutFixity os (Fixity.INfix (m,n)) = (blastOutChoice os 1; 
						      blastOutChoice os m; blastOutChoice os n)
	fun blastInFixity is =
	    if (blastInChoice is = 0) 
		then Fixity.NONfix 
	    else let val m = blastInChoice is
		     val n = blastInChoice is
		 in  Fixity.INfix (m,n)
		 end

	fun blastOutFixityTable os ft = blastOutList (blastOutPair Name.blastOutLabel blastOutFixity) os ft
	fun blastInFixityTable is = blastInList (blastInPair Name.blastInLabel blastInFixity) is 


	fun blastOutLabelList os label_list = 
	    blastOutLabelmap os (blastOutPair blastOutPath blastOutPC) label_list
	fun blastInLabelList is = 
	    (tab "blastLabelList\n"; 
	    blastInLabelmap is (blastInPair blastInPath blastInPC) )

	fun blastOutVarList os (vmap,vlist) = 
	    (blastOutVarmap os (blastOutPair blastOutLabel blastOutPC) vmap;
	     blastOutList blastOutVar os vlist)

	fun blastInVarList is = 
	    let val _ = tab "blastInVarList\n";
		val vmap = blastInVarmap is (blastInPair blastInLabel blastInPC) 
		val vlist = blastInList blastInVar is
	    in  (vmap, vlist)
	    end

	fun blastOutTagList os tag_list = blastOutTagmap os blastOutCon tag_list
	fun blastInTagList is = blastInTagmap is blastInCon 

    fun blastOutContext os (CONTEXT {flatlist, fixity_list, label_list, var_list, tag_list}) = 
	(blastOutFixityTable os fixity_list;
	 blastOutLabelList os label_list;
	 blastOutVarList os var_list;
	 blastOutTagList os tag_list)

    fun blastInContext is = 
	let val fixity_list = blastInFixityTable is
	    val label_list = blastInLabelList is
	    val var_list = blastInVarList is
	    val tag_list = blastInTagList is
	in CONTEXT {flatlist = [], fixity_list = fixity_list, label_list = label_list, 
		    var_list = var_list, tag_list = tag_list}
	end

    end (* local *)

end