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
	type sdec = Il.sdec
	type sdecs = Il.sdecs
	type fixity_table = Il.fixity_table
	type path = Il.path

	open Il Util Name Listops

	val error = fn s => error "ilcontext.sml" s
    val debug = ref false
    fun debugdo t = if (!debug) then (t(); ()) else ()

    val empty_context = CONTEXT[]
    fun context_entries (CONTEXT ce) = ce
    fun add_context_sdecs(CONTEXT c, sdecs) = CONTEXT((map CONTEXT_SDEC sdecs) @ c)
    fun add_context_sdec(CONTEXT c, sdec : sdec) = CONTEXT((CONTEXT_SDEC sdec) :: c)
    fun anon_label () = fresh_internal_label "anon"
    fun dec2sdec dec = SDEC(anon_label(),dec)
    fun decs2sdecs decs = map dec2sdec decs
    fun add_context_decs(ctxt, decs) = add_context_sdecs(ctxt, decs2sdecs decs)
    fun add_context_dec(ctxt, dec) = add_context_decs(ctxt,[dec])
    fun add_context_inline(CONTEXT c, l, v, inline) = CONTEXT((CONTEXT_INLINE(l,v,inline))::c)
    fun add_context_entries(CONTEXT c, e : context_entry list) : context = 
	CONTEXT(e @ c)

    fun add_context_exp(c, l, v, con) = add_context_sdec(c,SDEC(l,DEC_EXP(v,con)))
    fun add_context_mod(c, l, v, signat) = add_context_sdec(c,SDEC(l,DEC_MOD(v,signat)))
    fun add_context_con(c, l, v, kind, conopt) = add_context_sdec(c,SDEC(l,DEC_CON(v,kind,conopt)))
    fun add_context_sig(c, l, v, signat) = add_context_entries(c,[CONTEXT_SIGNAT(l,v,signat)])

    fun add_context_exp'(c, v, con) = add_context_exp(c,anon_label(), v, con)
    fun add_context_mod'(c, v, signat) = add_context_mod(c,anon_label(), v, signat)
    fun add_context_con'(c, v, kind, conopt) = add_context_con(c,anon_label(), v, kind, conopt)
    fun add_context_sig'(c, v, signat) = add_context_sig(c,anon_label(), v, signat)
	
    fun Context_Get_FixityTable (CONTEXT c) : fixity_table = 
	let 
	    fun help (CONTEXT_FIXITY vflist) = vflist
	      | help _ = []
	    val res = List.concat (map help c)
	in  res 
	end
	    
     (* ---------- Bound Rules from page 15 - 16 ----------------- *)
	    datatype bound_name = BOUND_VAR of var | BOUND_NAME of tag
	    local
		fun Dec_Bound (DEC_EXP (v,_)) = SOME(BOUND_VAR v)
		  | Dec_Bound (DEC_MOD (v,_)) = SOME(BOUND_VAR v)
		  | Dec_Bound (DEC_CON (v,_,_)) = SOME(BOUND_VAR v)
		  | Dec_Bound (DEC_EXCEPTION (n,_)) = SOME(BOUND_NAME n)
		fun Ctxt_Bound (CONTEXT_INLINE (_,v,_)) = SOME(BOUND_VAR v)
		  | Ctxt_Bound (CONTEXT_SDEC(SDEC(_,d))) = Dec_Bound d
		  | Ctxt_Bound (CONTEXT_SIGNAT(_,v,_)) = SOME(BOUND_VAR v)
		  | Ctxt_Bound (CONTEXT_FIXITY _) = NONE
	    in
		fun Decs_Bound x = List.mapPartial Dec_Bound x
		fun Context_Bound (CONTEXT entries) = List.mapPartial Ctxt_Bound entries
	    end
	    fun var_in (var,[]) = false
	      | var_in (var,(BOUND_VAR v)::rest) = eq_var(var,v) orelse var_in(var,rest)
	      | var_in (var,_::rest) = var_in(var,rest)
	    fun name_in (name,[]) = false
	      | name_in (name,(BOUND_NAME v)::rest) = eq_tag(name,v) orelse name_in(name,rest)
	      | name_in (name,_::rest) = name_in(name,rest)
	    fun var_bound(ctxt,v) = var_in(v,Context_Bound ctxt)
	    fun name_bound(ctxt,n) = name_in(n,Context_Bound ctxt)


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

    datatype phrase_class = PHRASE_CLASS_EXP  of exp * con
                          | PHRASE_CLASS_CON  of con * kind
                          | PHRASE_CLASS_MOD  of mod * signat
                          | PHRASE_CLASS_SIG  of signat
                          | PHRASE_CLASS_OVEREXP of unit -> exp * (context,con) Tyvar.ocon

    type phrase_class_p = path * phrase_class
    exception NOTFOUND of string

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
    fun join_path_labels (SIMPLE_PATH v, l) = COMPOUND_PATH(v,l)
      | join_path_labels (COMPOUND_PATH (v,ls), l) = COMPOUND_PATH(v,ls @ l)

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

    fun Context_Lookup (context, labels : label list) : path * phrase_class =
	let 
(*
	    val _ = debugdo (fn () => (print "Context_Lookup called with labels = ";
				       pp_list pp_label' labels ("","",", ",false);
				       print " and context: \n";
				       pp_context context;
				       print "\n\n"))
*)
	  fun context_search lab centry = 
	      (case centry of
		  (CONTEXT_INLINE (l,v,b)::r) =>
		      if (eq_label(lab,l))
			  then (SIMPLE_PATH v,
				case b of
				    (INLINE_MODSIG ms) =>  PHRASE_CLASS_MOD ms
				  | (INLINE_EXPCON ec) =>  PHRASE_CLASS_EXP ec
				  | (INLINE_CONKIND ck) => PHRASE_CLASS_CON ck
				  | (INLINE_OVER over) =>  PHRASE_CLASS_OVEREXP over)
		      else if (is_label_open l)
			       then 
				   (case b of
					INLINE_MODSIG (m as MOD_STRUCTURE sbnds,
						       s as SIGNAT_STRUCTURE (NONE,sdecs)) => 
					    (let 
						 val (phrase,labels) = Sbnds_Lookup(sbnds,[lab])
(*						 val (class,labels') = Signat_Lookup(s,[lab]) *)
						 val (pc,labels') = Sdecs_Lookup(MOD_VAR v,
										 sdecs,[lab]) 
						 val class = pc2class pc
					     in  if (eq_list(eq_label,labels,labels'))
						     then (COMPOUND_PATH(v,labels), 
							   combine_pc(phrase,class))
						 else error "Context_Lookup labels and labels' different"
					     end
					 handle NOTFOUND _ => context_search lab r)
				      | _ => context_search lab r)
			   else (context_search lab r)
		| (CONTEXT_SDEC(SDEC(l,dec))::r) =>
		     (case dec of
			  DEC_EXP(v,c) =>
			      if (eq_label(lab,l))
				  then (SIMPLE_PATH v, PHRASE_CLASS_EXP(path2exp (SIMPLE_PATH v), c))
			      else (context_search lab r)
			| DEC_CON(v,k,NONE) =>
			      if (eq_label(lab,l))
				  then (SIMPLE_PATH v, PHRASE_CLASS_CON(path2con (SIMPLE_PATH v), k))
			      else (context_search lab r)
			| DEC_CON(v,k,SOME c) =>
			      if (eq_label(lab,l))
				  then (SIMPLE_PATH v, PHRASE_CLASS_CON(c, k))
			      else (context_search lab r)
			| DEC_MOD(v,s) =>
			      if (eq_label(lab,l))
				  then (SIMPLE_PATH v, PHRASE_CLASS_MOD(path2mod (SIMPLE_PATH v), s))
			      else if (is_label_open l) 
				       then (case s of 
						 (SIGNAT_STRUCTURE (_,sdecs)) => 
						     (let val (pc,labels) = 
							  Sdecs_Lookup(MOD_VAR v, sdecs,[lab])
						      in  classpath2pc(COMPOUND_PATH(v,labels),
								       pc2class pc)
						      end
							  handle NOTFOUND _ => context_search lab r)
					       | _ => context_search lab r)
				   else context_search lab r
		        | ((DEC_EXCEPTION _)) => context_search lab r)
	      | (CONTEXT_SIGNAT (l,v,s)::r) => if (eq_label(lab,l)) 
							 then (SIMPLE_PATH v,PHRASE_CLASS_SIG s)
						     else context_search lab r
	      | ((CONTEXT_FIXITY _)::r) => context_search lab r
	      | [] => ((* debugdo (fn () => (print "Context_Lookup failed on ";
					       print (label2string lab);
					       print ".  Context was\n";
					       pp_context context; print "\n")); *)
			    raise (NOTFOUND ("Context_Lookup failed on " 
					     ^ (label2string lab)))))
	  val CONTEXT context_entries = context
	in case labels of
	     [] => error "Context_Lookup got empty label list"
	   | [lab] => context_search lab context_entries
	   | (lab::labrest) =>
		 (case (context_search lab context_entries) of
		      (path,PHRASE_CLASS_MOD(module as (MOD_STRUCTURE sbnds),
					     (SIGNAT_STRUCTURE (_,sdecs)))) =>
			  let 
			      val (phrase,labels) = Sbnds_Lookup(sbnds,labrest)
			      val (pc,labels') = Sdecs_Lookup(path2mod path,sdecs,labrest)
			      val class = pc2class pc
			      val p = join_path_labels(path,labels)
			  in  (p,combine_pc(phrase,class))
			  end
		   | (path,PHRASE_CLASS_MOD(_,((SIGNAT_STRUCTURE (_,sdecs))))) =>
			  let 
			      val (pc,labels) = Sdecs_Lookup(path2mod path,sdecs,labrest)
			      val class = pc2class pc
			      val p = join_path_labels(path,labels)
			  in  classpath2pc(p,class)
			  end
		    | _ => raise (NOTFOUND ("lbl did not look up to a structure")))
	end (* Context_Lookup *)
      


    and Sbnds_Lookup (sbnds, labs) : (phrase * labels) = 
	let 
	    fun loop lbl _ [] = raise (NOTFOUND "Sdecs_Lookup reached []")
	      | loop lbl prev ((sbnd as SBND(l,b))::r) = 
		let val self = loop lbl (b::prev) 
		in
		    (case b of
			 (BND_EXP (_,e)) => if (eq_label(l,lbl)) 
						then (PHRASE_EXP e,[l]) else self r
		       | (BND_CON (_,c)) => if (eq_label(l,lbl)) 
						then let val c' = c (* XXX *)
						     in (PHRASE_CON c',[l]) 
						     end
					    else self r
		       | (BND_MOD (_,m)) => (if (eq_label(l,lbl)) 
						 then (PHRASE_MOD m,[l]) 
					     else if (is_label_open l)
						      then 
							  (case m of 
							       MOD_STRUCTURE sbnds =>
								   (let val (phrase,lbls') = self sbnds
								    in (phrase,l::lbls')
								    end handle (NOTFOUND _) => self r)
							     | _ => self r)
					      else self r))
		end
	in
	    (case labs of
		 [] => error "Sbnds_Lookup got []"
	       | [lbl] => loop lbl [] sbnds
	       | (lbl :: lbls) =>
		     let val (phrase,labs) = loop lbl [] sbnds
		     in
			 (case phrase of
			      PHRASE_MOD (MOD_STRUCTURE sbnds) => 
				  let val (phrase2,labs2) = Sbnds_Lookup(sbnds,lbls)
				  in (phrase2,labs @ labs2)
				  end 
			    | _ => error "Sbnds_Lookup did not find a structure")
		     end)
	end

    and Sdecs_Lookup (m, sdecs, labs) = #2(Sdecs_Lookup'(m,sdecs,labs))
    and Sdecs_Lookup' (m, sdecs, labs) : (bool * (phrase_class * labels)) = 
	let 
	    fun loop lbl prev [] : (bool * (phrase_class * labels)) = 
		  raise (NOTFOUND "Sdecs_Lookup reached []")
	      | loop lbl prev ((sdec as (SDEC(l,d)))::rest) =
		let val self = loop lbl (sdec::prev) 
		in case d of
		    (DEC_EXP (_,c)) => 
			if (eq_label(l,lbl)) 
			    then  (false,(PHRASE_CLASS_EXP(MODULE_PROJECT(m,l), c),[l]))
			else self rest
		  | (DEC_CON (_,k,SOME c)) => if (eq_label(l,lbl)) 
						  then (true,(PHRASE_CLASS_CON(c,k),[l]))
					      else self rest
		  | (DEC_CON (_,k,NONE)) => if (eq_label(l,lbl)) 
					     then (false,(PHRASE_CLASS_CON(CON_MODULE_PROJECT(m,l),
									   k),[l]))
					    else self rest
		  | (DEC_MOD (_,s)) => 
			 (if (eq_label(l,lbl))
			      then (false,(PHRASE_CLASS_MOD(MOD_PROJECT(m,l),s),[l]))
			  else if (is_label_open l)
				   then (case s of
					     (SIGNAT_STRUCTURE (_,sdecs)) =>

						 (let val (flag,(class,lbls')) = self sdecs
						  in (flag,(class,l::lbls'))
						  end 
						      handle (NOTFOUND _) => self rest)
					   | _ => self rest)
			       else self rest)
		  | _ => self rest
		end
	in
	    (case labs of
		 [] => error "Sdecs_Lookup got []"
	       | [lbl] => loop lbl [] sdecs
	       | (lbl :: lbls) =>
		     let val (_,(phrase_class,labs)) = loop lbl [] sdecs
		     in
			 (case phrase_class of
			      PHRASE_CLASS_MOD (m',((SIGNAT_STRUCTURE (_,sdecs')))) =>
				  let val (nontrivial,(pc2,labs2)) = Sdecs_Lookup'(m',sdecs',lbls)
				  in (nontrivial,(pc2,labs @ labs2))
				  end 
			    | _ => error "Sdecs_Lookup did not find a structure sig")
		     end)
	end


      fun modsig_lookup arg = ((case Context_Lookup arg of
				  (path,PHRASE_CLASS_MOD (m,s)) => SOME(path,m,s)
				| _ => NONE)
			       handle (NOTFOUND _) => NONE)
      val Sbnds_Lookup =
	  fn (sbnds,labels) => (let val (phrase,labels) = Sbnds_Lookup (sbnds,labels)
				  in (labels,phrase)
				  end)
      val Sdecs_Lookup' = 
	  fn (m,sdecs,labels) => (let val (f,(pc,labels)) = Sdecs_Lookup' (m,sdecs,labels)
				  in (f,(labels,pc))
				  end)
      val Sdecs_Lookup = 
	  fn (m,sdecs,labels) => (let val (pc,labels) = Sdecs_Lookup (m,sdecs,labels)
				  in (labels,pc)
				  end
				      handle e => (
(*print "error in Sdecs_Lookup with decs = ...\n";
 print "  and m = "; pp_mod m;
 print "  and sig = \n"; pp_sdecs sdecs;
 print "\n and labels = "; 
 pp_list pp_label' labels ("","",".",false); *)
						     raise e))
      val Context_Lookup = 
	  fn (arg as (context,labels)) =>
	  (#2(Context_Lookup arg)
	       handle e => ((* print "error in Context_Lookup with labels = ";
			    pp_pathlist pp_label' labels;
			    print "\nand context = ";
			    pp_context context;
			    print "\n"; *)
			    raise e))

      fun var2label (CONTEXT entries, tarv) = 
	  let fun loop [] = (print ("var2label failed on var = " ^ (var2string tarv));
			     error ("var2label failed on var = " ^ (var2string tarv)))
		| loop (this::rest) = 
		  (case this of
		       CONTEXT_INLINE(l,v,_) => if (eq_var(v,tarv)) then l else loop rest
		     | CONTEXT_SIGNAT(l,v,_) => if (eq_var(v,tarv)) then l else loop rest
		     | CONTEXT_SDEC(SDEC(l,(DEC_MOD(v,_) | DEC_EXP(v,_) | DEC_CON(v,_,_)))) =>
			   if (eq_var(v,tarv)) then l else loop rest
		     | _ => loop rest)
	  in  loop entries
	  end
      fun Context_Lookup' (ctxt,v) = Context_Lookup(ctxt,[var2label(ctxt,v)])
      fun Context_Exn_Lookup (CONTEXT entries,tag) = 
	  let fun loop [] = raise (NOTFOUND ("Context_Exn_Lookup failed on " ^ (tag2string tag)))
		| loop ((CONTEXT_SDEC(SDEC(_,DEC_EXCEPTION(t,c)))) :: rest) = 
		  if (eq_tag(t,tag)) then c else loop rest
		| loop (_::rest) = loop rest
	  in loop entries
	  end

    end
