
functor IlLookup(structure Il : IL
		 structure IlStatic : ILSTATIC
		 structure IlUtil : ILUTIL
		 structure Ppil : PPIL
		 structure AstHelp : ASTHELP
		 sharing Ppil.Il = IlUtil.Il = IlStatic.Il = Il
		 sharing Ppil.Formatter = AstHelp.Formatter)
   : ILLOOKUP = 
  struct

    structure Il = Il
    open AstHelp Il IlStatic IlUtil Ppil
    open Util Name
  
    val error = error "illookup.sml"
    val debug = ref false
    fun debugdo t = if (!debug) then (t(); ()) else ()


    (* ---------------- LOOKUP RULES --------------------------- 
     The lookup rules can return item from different grammatical classes
     so we need some additional datatypes to package up the results 
     --------------------------------------------------------- *)
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

    datatype phrase_class_p = PHRASE_CLASS_P_EXP  of path * con
                            | PHRASE_CLASS_P_CON  of path * kind
                            | PHRASE_CLASS_P_MOD  of path * signat
                            | PHRASE_CLASS_P_SIG  of signat
                            | PHRASE_CLASS_P_INLINE  of phrase_class
    local
      datatype phrase = PHRASE_EXP  of path
                      | PHRASE_CON  of path
                      | PHRASE_MOD  of path
                      | PHRASE_SIG  of signat
                      | PHRASE_INLINE  of phrase_class
      datatype flavor = FL_EXP | FL_CON | FL_MOD | FL_SIG | FL_INLINE
      fun pp_phrase (PHRASE_EXP p)   = (print "PHRASE_EXP: "; pp_path p)
	| pp_phrase (PHRASE_CON p)   = (print "PHRASE_CON: "; pp_path p)
	| pp_phrase (PHRASE_MOD p)   = (print "PHRASE_MOD: "; pp_path p)
	| pp_phrase (PHRASE_SIG s)   = (print "PHRASE_SIG: "; pp_signat s)
	| pp_phrase (PHRASE_INLINE inline) = (print "PHRASE_INLINE: not done")


      fun Context_Phrase_Class_Lookup (context, labels : label list) : phrase_class_p = 
	let 
	  val decs = context2decs context
	  val ph = Context_Phrase_Lookup (context, labels)
	    handle e as NOTFOUND _ => (debugdo (fn () => (print "Context_Phrase_Class_Lookup calling ";
							  print "Context_PHRASE_Lookup did not find ";
							  pp_list pp_label' labels ("",".","",false);
							  print "\n"));
				       raise e)
	  val _ = debugdo (fn () => (pp_phrase ph; print "\n"))
	in
	  (case ph of
	     PHRASE_EXP p => let val exp = path2exp p in PHRASE_CLASS_P_EXP(p,GetExpCon(decs,exp)) end
	   | PHRASE_CON p => let val con = path2con p 
			     in PHRASE_CLASS_P_CON(p,GetConKind(decs,con)) 
			     end
	   | PHRASE_MOD p => let val m   = path2mod p in PHRASE_CLASS_P_MOD(p,GetModSig(decs,m)) end
	   | PHRASE_SIG signat => PHRASE_CLASS_P_SIG signat
	   | PHRASE_INLINE b  => PHRASE_CLASS_P_INLINE b)
	end
      
      and Context_Phrase_Lookup (context, labels : label list) : phrase =
	let 
	  fun help lab l v maker r = if (eq_label(lab,l)) 
				       then maker(SIMPLE_PATH v) else loop lab r
	  and loop lab (CONTEXT_INLINE (l,b)::r) = 
	    if (eq_label(lab,l))
	      then (case b of
		      (INLINE_MODSIG ms) => PHRASE_INLINE(PHRASE_CLASS_MOD ms)
		    | (INLINE_EXPCON ec) => PHRASE_INLINE(PHRASE_CLASS_EXP ec)
		    | (INLINE_OVER over) => PHRASE_INLINE(PHRASE_CLASS_OVEREXP over))
	    else (loop lab r)
	    | loop lab (CONTEXT_VAR (l,v,c)::r)                  = help lab l v PHRASE_EXP r
	    | loop lab (CONTEXT_CONVAR (l,v,k,c)::r)             = help lab l v PHRASE_CON r
	    | loop lab (CONTEXT_MODULE (l,v,s)::r) = 
			if (is_label_open l) 
			  then (let val (fl,sig_path) = Signat_Label_Lookup(s,lab)
				in  (case fl of
				       FL_EXP => PHRASE_EXP(COMPOUND_PATH(v,sig_path))
				     | FL_CON => PHRASE_CON(COMPOUND_PATH(v,sig_path))
				     | FL_MOD => PHRASE_MOD(COMPOUND_PATH(v,sig_path))
				     | FL_SIG => error "FL_SIG returned from SIG_Lookup"
				     | FL_INLINE => error "FL_INLINE returned from SIG_Lookup")
				end
				  handle NOTFOUND _ => loop lab r)
			else help lab l v PHRASE_MOD r
	    | loop lab (CONTEXT_SIGNAT (l,v,s)::r) = if (eq_label(lab,l)) then PHRASE_SIG s else loop lab r
            | loop lab _ = (debugdo (fn () => (print "Context_Phrase_Lookup failed on ";
					       print (label2string lab);
					       print ".  Context was\n";
					       pp_context context; print "\n"));
			    raise (NOTFOUND ("Context_Phrase_Lookup failed on " 
					     ^ (label2string lab))))
	  (* Rule 271 *)
	  fun listloop [] = error "Context_Phrase_Lookup got empty label list"
	    | listloop [lab : label] = loop lab (case context of (CONTEXT entries) => entries)
	    | listloop (lab::labrest) = 
	    (case (Context_Phrase_Class_Lookup(context,[lab])) of
	       PHRASE_CLASS_P_MOD(path,signat) => 
		 let 
		   val (labrest',class) = Signat_Labels_Class_Lookup(context2decs context,
								     (path,signat),labrest)
		   val p = join_path_labels(path,labrest')
		 in  (case class of
			CLASS_EXP _ => PHRASE_EXP p
		      | CLASS_CON _ => PHRASE_CON p
		      | CLASS_MOD _ => PHRASE_MOD p
		      | _ => error "Rule 309")
		 end
	     | _ => raise (NOTFOUND ("lbl looked up not to a normal module")))
	in listloop labels
	end (* Context_Phrase_Lookup *)
      

      (* Signature Lookup - Rules 272 - 283 *)
    and Signat_Labels_Class_Lookup (decs, (path : path, signat : signat), labs) : (labels * class) = 
	(case labs of
	   [] => error "Signat_Labels_Class_Lookup got []"
	 | [lbl] => let val (fl,lbls') = Signat_Label_Lookup(signat,lbl)
			val p = join_path_labels(path,lbls')
		    in (lbls',(case fl of
				 FL_EXP => CLASS_EXP(GetExpCon(decs,path2exp p))
			       | FL_CON => CLASS_CON(GetConKind(decs,path2con p))
			       | FL_MOD => CLASS_MOD(GetModSig(decs,path2mod p))
			       | _ => error "Signat_Label_Lookup returned strange flavor"))
		    end
	 | (lbl :: lbls) =>
		    (case (Signat_Labels_Class_Lookup(decs, (path,signat), [lbl])) of
		       (labels',CLASS_MOD signat') =>
			 let val (labels'',class) = Signat_Labels_Class_Lookup(decs, 
									       (join_path_labels(path,labels'),
										signat'), lbls)
			 in  (labels' @ labels'', class)
			 end
	       | _ => error "Signat_Labels_Class_Lookup failed"))

      and Signat_Label_Lookup (SIGNAT_FUNCTOR _, lab) = error "Signat_Label_Lookup got functor"
	| Signat_Label_Lookup (SIGNAT_DATATYPE (_,_,sdecs), lab : label) : flavor * labels =
	Signat_Label_Lookup (SIGNAT_STRUCTURE sdecs, lab)
	| Signat_Label_Lookup (SIGNAT_STRUCTURE sdecs, lab : label) : flavor * labels =
	let  (* Rules 276 - 283 *)
	  fun loop [] = raise (NOTFOUND "Signat_Label_Lookup reched []")
	    | loop (SDEC(l,d)::r) = 
	    (case (is_label_open l,d) of
	       (true,DEC_MOD(_,s)) => ((let val (fl,lbls') = Signat_Label_Lookup(s,lab)
					in (fl,l::lbls')
					end) handle NOTFOUND _ => loop r)
	     |  (_,DEC_EXP _) => if (eq_label(l,lab)) then (FL_EXP,[l]) else loop r
	     |  (_,DEC_CON _) => if (eq_label(l,lab)) then (FL_CON,[l]) else loop r
	     |  (_,DEC_MOD _) => if (eq_label(l,lab)) then (FL_MOD,[l]) else loop r
	     |  _ => loop r)
	in loop sdecs
	end
      
    in
      val Signat_Lookup = Signat_Labels_Class_Lookup 
      val Context_Lookup' = Context_Phrase_Class_Lookup
      fun Context_Lookup arg = (case Context_Phrase_Class_Lookup arg of
				  PHRASE_CLASS_P_EXP (p,c)   => PHRASE_CLASS_EXP(path2exp p,c)
				| PHRASE_CLASS_P_CON (p,k)   => PHRASE_CLASS_CON(path2con p,k)
				| PHRASE_CLASS_P_MOD (p,s)   => PHRASE_CLASS_MOD(path2mod p,s)
				| PHRASE_CLASS_P_SIG s       => PHRASE_CLASS_SIG s
				| PHRASE_CLASS_P_INLINE pc    => pc)
      fun modsig_lookup arg = ((case Context_Phrase_Class_Lookup arg of
				  PHRASE_CLASS_P_MOD (p,s)   => SOME(p,path2mod p,s)
				| _ => NONE)
			       handle (NOTFOUND _) => NONE)
    end
  end