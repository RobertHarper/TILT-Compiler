functor IlContext(structure Il : ILLEAK) 
    : ILCONTEXT = 
struct

exception XXX

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
                    | PHRASE_OVEREXP of unit -> exp * (context,con) Tyvar.ocon

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
	    fun loop lbl _ [] = NONE
	      | loop lbl prev ((sbnd as SBND(l,b))::r) = 
		let val self = loop lbl (b::prev) 
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
								   (case (self sbnds) of
									SOME(lbls',phrase) => SOME(l::lbls',phrase)
								      | NONE => self r)
							     | _ => self r)
						  else self r))
		end
	in
	    (case labs of
		 [] => error "Sbnds_Lookup got []"
	       | [lbl] => loop lbl [] sbnds
	       | (lbl :: lbls) =>
		     (case (loop lbl [] sbnds) of
			 SOME(labs,PHRASE_MOD (MOD_STRUCTURE sbnds)) => 
			     (case (Sbnds_Lookup(sbnds,lbls)) of
				  SOME(labs2,phrase2) => SOME(labs@labs2,phrase2)
				| _ => NONE)
		       | _ => NONE))
	end

    fun Sdecs_Lookup_help (m, sdecs, labs) : (bool * (phrase_class * labels)) option = 
	let 
	    fun loop lbl [] = NONE
	      | loop lbl ((sdec as (SDEC(l,d)))::rest) =
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
			    | _ => loop lbl rest)
		else if (is_label_open l)
		    then (case d of
			      (DEC_MOD(_,SIGNAT_STRUCTURE (_,sdecs))) =>
				  (case (loop lbl sdecs) of
				       SOME (flag,(class,lbls')) => SOME(flag,(class,l::lbls'))
				     | NONE => loop lbl rest)
			    | (DEC_MOD(_,((SIGNAT_INLINE_STRUCTURE{code,abs_sig=sdecs,...})))) =>
				  (case (Sbnds_Lookup(code,[lbl]),loop lbl sdecs) of
				       (SOME (lbl,p), SOME(_,(pc,lbls'))) => 
						SOME(true,(combine_pc(p,pc2class pc),l::lbls'))
				     | (SOME _, NONE) => error "sdecs_Lookup: open case:  SOME/NONE"
				     | (NONE, SOME _) => error "sdecs_Lookup: open case:  NONE/SOME"
				     | (NONE,NONE) => loop lbl rest)
			    | _ => loop lbl rest)
		     else loop lbl rest

	in
	    (case labs of
		 [] => error "Sdecs_Lookup_help got []"
	       | [lbl] => loop lbl sdecs
	       | (lbl :: lbls) =>
		     case (loop lbl sdecs) of
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
	       | [lbl] => loop lbl sdecs
	       | (lbl :: lbls) =>
		     case (loop lbl sdecs) of
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


    end
