functor IlLookup(structure Il : IL
		 structure IlUtil : ILUTIL
		 structure Ppil : PPIL
		 structure AstHelp : ASTHELP
		 sharing Ppil.Il = IlUtil.Il = Il)

    : ILLOOKUP  = 
  struct

    structure Il = Il
    open AstHelp Il (* IlStatic *) IlUtil Ppil
    open Util Listops Name
  
    val error = fn s => error "illookup.sml" s
    val debug = ref false
    fun debugdo t = if (!debug) then (t(); ()) else ()

    (* ---------------- LOOKUP RULES --------------------------- 
     The lookup rules can return item from different grammatical classes
     so we need some additional datatypes to package up the results 
     --------------------------------------------------------- *)
    datatype phrase = PHRASE_EXP of exp
                    | PHRASE_CON of con
                    | PHRASE_MOD of mod
                    | PHRASE_SIG of signat
                    | PHRASE_OVEREXP of unit -> (int list -> Il.exp) * Il.con Il.Tyvar.ocon

    datatype class = CLASS_EXP of con
                   | CLASS_CON of kind
                   | CLASS_MOD of signat
                   | CLASS_SIG
                   | CLASS_OVEREXP

    datatype phrase_class = PHRASE_CLASS_EXP  of exp * con
                          | PHRASE_CLASS_CON  of con * kind
                          | PHRASE_CLASS_MOD  of mod * signat
                          | PHRASE_CLASS_SIG  of signat
                          | PHRASE_CLASS_OVEREXP of unit -> (int list -> Il.exp) * Il.con Il.Tyvar.ocon

    type phrase_class_p = path * phrase_class

    fun combine_pc (PHRASE_EXP e, CLASS_EXP c) = PHRASE_CLASS_EXP (e,c)
      | combine_pc (PHRASE_CON c, CLASS_CON k) = PHRASE_CLASS_CON (c,k)
      | combine_pc (PHRASE_MOD m, CLASS_MOD s) = PHRASE_CLASS_MOD (m,s)
      | combine_pc (PHRASE_SIG s, CLASS_SIG) = PHRASE_CLASS_SIG s
      | combine_pc (PHRASE_OVEREXP oe, CLASS_OVEREXP) = PHRASE_CLASS_OVEREXP oe
      | combine_pc _ = error "combine_pc got a phrase and a class of conflicting flavors"

    fun classpath2pc (p, CLASS_EXP c) = (p, PHRASE_CLASS_EXP (path2exp p, c))
      | classpath2pc (p, CLASS_CON k) = (p, PHRASE_CLASS_CON (path2con p, k))
      | classpath2pc (p, CLASS_MOD s) = (p, PHRASE_CLASS_MOD (path2mod p, s))
      | classpath2pc (p, CLASS_SIG) = error "classpath2pc got a CLASS_SIG"
      | classpath2pc (p, CLASS_OVEREXP) = error "classpath2pc got a CLASS_OVEREXP"

    fun Context_Lookup (context, labels : label list) : path * phrase_class =
	let 
	    val _ = debugdo (fn () => (print "Context_Lookup called with labels = ";
				       pp_list pp_label' labels ("","",", ",false);
				       print " and context: \n";
				       pp_context context;
				       print "\n\n"))
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
						       s as SIGNAT_STRUCTURE sdecs) => 
					    (let 
						 val (phrase,labels) = Sbnds_Lookup(m,sbnds,[lab])
(*						 val (class,labels') = Signat_Lookup(s,[lab]) *)
						 val (class,labels') = Sdecs_Lookup(m,sdecs,[lab]) 
					     in  if (eq_list(eq_label,labels,labels'))
						     then (COMPOUND_PATH(v,labels), combine_pc(phrase,class))
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
			| DEC_CON(v,k,c) =>
			      if (eq_label(lab,l))
				  then (SIMPLE_PATH v, PHRASE_CLASS_CON(path2con (SIMPLE_PATH v), k))
			      else (context_search lab r)
			| DEC_MOD(v,s) =>
			      if (eq_label(lab,l))
				  then (SIMPLE_PATH v, PHRASE_CLASS_MOD(path2mod (SIMPLE_PATH v), s))
			      else if (is_label_open l) 
				       then (case s of 
						 SIGNAT_STRUCTURE sdecs =>
						     (let val (class,labels) = 
							  Sdecs_Lookup(MOD_VAR v,sdecs,[lab])
						      in  classpath2pc(COMPOUND_PATH(v,labels),class)
						      end
							  handle NOTFOUND _ => context_search lab r)
					       | _ => context_search lab r)
				   else context_search lab r
		        | ((DEC_EXCEPTION _) | (DEC_FIXITY _)) => context_search lab r)
	      | (CONTEXT_SIGNAT (l,v,s)::r) => if (eq_label(lab,l)) 
							 then (SIMPLE_PATH v,PHRASE_CLASS_SIG s)
						     else context_search lab r
	      | _ => (debugdo (fn () => (print "Context_Lookup failed on ";
					       print (label2string lab);
					       print ".  Context was\n";
					       pp_context context; print "\n"));
			    raise (NOTFOUND ("Context_Lookup failed on " 
					     ^ (label2string lab)))))
	  val CONTEXT context_entries = context
	in case labels of
	     [] => error "Context_Lookup got empty label list"
	   | [lab] => context_search lab context_entries
	   | (lab::labrest) =>
		 (case (context_search lab context_entries) of
		      (path,PHRASE_CLASS_MOD(module as (MOD_STRUCTURE sbnds),
					     signat as (SIGNAT_STRUCTURE sdecs))) => 
			  let 
			      val (phrase,labels) = Sbnds_Lookup(module,sbnds,labrest)
			      val (class,labels') = Sdecs_Lookup(path2mod path,sdecs,labrest)
			      val p = join_path_labels(path,labels)
			  in  (p,combine_pc(phrase,class))
			  end
		   | (path,PHRASE_CLASS_MOD(_,signat as SIGNAT_STRUCTURE sdecs)) => 
			  let 
			      val (class,labels) = Sdecs_Lookup(path2mod path,sdecs,labrest)
			      val p = join_path_labels(path,labels)
			  in  classpath2pc(p,class)
			  end
		    | _ => raise (NOTFOUND ("lbl did not look up to a structure")))
	end (* Context_Lookup *)
      


    and Sbnds_Lookup (replace_mod, sbnds, labs) : (phrase * labels) = 
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
					      else self r)
		   | _ => self r)
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
				  let val (phrase2,labs2) = Sbnds_Lookup(MOD_PROJECT(replace_mod,lbl),
									 sbnds,lbls)
				  in (phrase2,labs @ labs2)
				  end 
			    | _ => error "Sbnds_Lookup did not find a structure")
		     end)
	end

    and Sdecs_Lookup (replace_mod, sdecs, labs) : (class * labels) = 
	let fun loop lbl prev [] : (class * labels) = raise (NOTFOUND "Sdecs_Lookup reached []")
	      | loop lbl prev ((sdec as (SDEC(l,d)))::rest) =
		let val self = loop lbl (sdec::prev) 
		in case d of
		    (DEC_EXP (_,c)) => if (eq_label(l,lbl)) 
					   then let val c' = add_modvar_type(c,replace_mod,rev prev)
						in (CLASS_EXP c',[l]) 
						end
				       else self rest
		  | (DEC_CON (_,k,_)) => if (eq_label(l,lbl)) 
					     then (CLASS_CON k,[l]) else self rest
		  | (DEC_MOD (_,s)) => (if (eq_label(l,lbl)) (* XXX need to make independent *) 
					    then 
						let val s' = add_modvar_sig(s,replace_mod,rev prev)
						in (CLASS_MOD s',[l]) 
						end
				       else if (is_label_open l)
						then (case s of
							  SIGNAT_STRUCTURE sdecs => 
							      (let val (class,lbls') = self sdecs
							       in (class,l::lbls')
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
		     let val (class,labs) = loop lbl [] sdecs
		     in
			 (case class of
			      CLASS_MOD (SIGNAT_STRUCTURE sdecs') => 
				  let val (class2,labs2) = Sdecs_Lookup(MOD_PROJECT(replace_mod,lbl),
									sdecs',lbls)
				  in (class2,labs @ labs2)
				  end 
			    | _ => error "Sdecs_Lookup did not find a structure sig")
		     end)
	end


      fun modsig_lookup arg = ((case Context_Lookup arg of
				  (path,PHRASE_CLASS_MOD (m,s)) => SOME(path,m,s)
				| _ => NONE)
			       handle (NOTFOUND _) => NONE)
      val Sbnds_Lookup =
	  fn (m,sbnds,labels) => (let val (phrase,labels) = Sbnds_Lookup (m,sbnds,labels)
				  in (labels,phrase)
				  end)
      val Sdecs_Lookup = 
	  fn (d,(m,sdecs),labels) => (let val (class,labels) = Sdecs_Lookup (m,sdecs,labels)
				  in (labels,class)
				  end
				      handle e => (print "error in Sdecs_Lookup with decs = ...\n";
						   print "  and m = "; pp_mod m;
						   print "  and sig = \n"; pp_sdecs sdecs;
						   print "\n and labels = "; 
						   pp_list pp_label' labels ("","",".",false);
						   raise e))
      val Context_Lookup = 
	  fn (arg as (context,labels)) =>
	  (#2(Context_Lookup arg)
	       handle e => (print "error in Context_Lookup with labels = ";
			    pp_pathlist pp_label' labels;
			    print "\nand context = ";
			    pp_context context;
			    print "\n";
			    raise e))


  end
