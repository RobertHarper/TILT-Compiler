(* The Il datatypes. *)
functor Il(structure Prim : PRIM
	   structure Tyvar : TYVAR)
  : ILLEAK = 
  struct

    open Util Listops Name
    structure Prim = Prim
    structure Tyvar = Tyvar
    open Prim

    val error = fn s => error "il.sml" s
    type tag = Name.tag
    type var = Name.var
    type label = Name.label
    type labels = Name.label list
    type prim = Prim.prim
    type ilprim = Prim.ilprim

    datatype path = SIMPLE_PATH   of var 
                  | COMPOUND_PATH of var * labels

    type fixity_table = (label * Fixity.fixity) list 
    type prim = Prim.prim
    type ilprim = Prim.ilprim

    datatype arrow = TOTAL | PARTIAL

    datatype exp = OVEREXP of con * bool * exp Util.oneshot
                 | SCON    of value
                 | PRIM    of prim * con list
                 | ILPRIM  of ilprim          (* for type-checking reasons *)
                 | VAR     of var
                 | APP     of exp * exp
                 | FIX     of arrow * fbnd list * var
                 | RECORD  of (label * exp) list
                 | RECORD_PROJECT of exp * label * con
                 | SUM_TAIL of con * exp
                 | HANDLE  of exp * exp      (* body and handler: type ANY  *)
                 | RAISE   of con * exp
                 | LET     of bnd list * exp
                 | NEW_STAMP of con
                 | EXN_INJECT of exp * exp (* tag and value *)
                 | ROLL    of con * exp
                 | UNROLL  of con * exp
                 | INJ     of con list * int * exp
                 | TAG     of tag * con
                 (* case over sum types of exp with arms and defaults*)
                 | CASE    of con list * exp * (exp option) list * exp option
                 | EXN_CASE of exp * (exp * con * exp) list * exp option
                 | MODULE_PROJECT of mod * label
                 | SEAL    of exp * con

    and     fbnd = FBND    of var * var * con * con * exp  (* var = (var : con) : con |-> exp *)
    and flexinfo = FLEXINFO of (Tyvar.stamp * bool * (label * con) list) 
	         | INDIRECT_FLEXINFO of flexinfo ref (* <--- this ref is necessary for unification *)
    and      con = CON_VAR           of var
                 | CON_TYVAR         of (context,con) Tyvar.tyvar  (* supports type inference *)
                 | CON_OVAR          of (context,con) Tyvar.ocon   (* supports "overloaded" types *)
                 | CON_FLEXRECORD    of flexinfo ref
                 | CON_INT           of Prim.intsize
                 | CON_UINT          of Prim.intsize
                 | CON_FLOAT         of Prim.floatsize
                 | CON_ARRAY         of con
                 | CON_VECTOR        of con
                 | CON_ANY
                 | CON_REF           of con
                 | CON_TAG           of con
                 | CON_ARROW         of con * con * (arrow Util.oneshot)
                 | CON_APP           of con * con
                 | CON_MUPROJECT     of int * con
                 | CON_RECORD        of (label * con) list
                 | CON_FUN           of var list * con
                 | CON_SUM           of int option * con list
                 | CON_TUPLE_INJECT  of con list
                 | CON_TUPLE_PROJECT of int * con 
                 | CON_MODULE_PROJECT of mod * label
    and     kind = KIND_TUPLE of int
                 | KIND_ARROW of int * int
    and      mod = MOD_VAR of var
                 | MOD_STRUCTURE of sbnd list
                 | MOD_FUNCTOR of var * signat * mod
                 | MOD_APP of mod * mod
                 | MOD_PROJECT of mod * label
                 | MOD_SEAL of mod * signat
                 | MOD_LET of var * mod * mod
    and     sbnd = SBND of label * bnd
    and      bnd = BND_EXP of var * exp
                 | BND_MOD of var * mod
                 | BND_CON of var * con
                 | BND_FIXITY    of fixity_table  (* tracks infix precedence *)
    and   signat = SIGNAT_STRUCTURE       of path option * sdec list
                 | SIGNAT_FUNCTOR of var * signat * signat * (arrow Util.oneshot)
    and     sdec = SDEC of label * dec
    and      dec = DEC_EXP       of var * con
                 | DEC_MOD       of var * signat
                 | DEC_CON       of var * kind * con option 
                 | DEC_EXCEPTION of tag * con
                 | DEC_FIXITY    of fixity_table   (* tracks infix precedence *)

    and inline = INLINE_MODSIG of mod * signat
	      | INLINE_EXPCON of exp * con
	      | INLINE_CONKIND of con * kind
	      | INLINE_OVER   of unit -> exp * (context,con) Tyvar.ocon
    and context_entry = 
		CONTEXT_INLINE of label * var * inline
	      | CONTEXT_SDEC   of sdec
	      | CONTEXT_SIGNAT of label * var * signat
(*
    and inline = XINLINE_MODSIG of mod * signat
                    | XINLINE_EXPCON of exp * con
                    | XINLINE_CONKIND of con * kind
                    | XINLINE_OVER   of unit -> exp * (context,con) Tyvar.ocon

    and context_entry = XCONTEXT_INLINE of label * var * inline    
      | XCONTEXT_SDEC   of sdec
      | XCONTEXT_SIGNAT of label * var * signat
*)

    and context = CONTEXT of context_entry list

    withtype value = (con,exp) Prim.value
    and decs = dec list

    type bnds  = bnd list
    type sdecs = sdec list
    type sbnds = sbnd list
(*
    structure IlContext = 
	struct
	    type context = context
	    fun unconvert' (XINLINE_MODSIG arg) = INLINE_MODSIG arg
	      | unconvert' (XINLINE_EXPCON arg) = INLINE_EXPCON arg
	      | unconvert' (XINLINE_CONKIND arg) = INLINE_CONKIND arg
	      | unconvert' (XINLINE_OVER arg) = INLINE_OVER arg
	    fun unconvert (XCONTEXT_INLINE (l,v,i)) = CONTEXT_INLINE (l,v,unconvert' i)
	      | unconvert (XCONTEXT_SDEC arg) = CONTEXT_SDEC arg
	      | unconvert (XCONTEXT_SIGNAT arg) = CONTEXT_SIGNAT arg
	    fun context_entries (CONTEXT entries) : context_entry list = map unconvert entries
	    fun convert' (INLINE_MODSIG arg) = XINLINE_MODSIG arg
	      | convert' (INLINE_EXPCON arg) = XINLINE_EXPCON arg
	      | convert' (INLINE_CONKIND arg) = XINLINE_CONKIND arg
	      | convert' (INLINE_OVER arg) = XINLINE_OVER arg
	    fun convert (CONTEXT_INLINE (l,v,i)) = XCONTEXT_INLINE (l,v,convert' i)
	      | convert (CONTEXT_SDEC arg) = XCONTEXT_SDEC arg
	      | convert (CONTEXT_SIGNAT arg) = XCONTEXT_SIGNAT arg

	    val empty_context = CONTEXT[]
	    fun add_context_dec(CONTEXT c, l, dec) = CONTEXT((XCONTEXT_SDEC(SDEC(l,dec)))::c)
	    fun add_context_var(c, l, v, con) = add_context_dec(c,l,DEC_EXP(v,con))
	    fun add_context_module(c, l, v, signat) = add_context_dec(c,l,DEC_MOD(v,signat))
	    fun add_context_convar(c, l, v, kind, conopt) = add_context_dec(c,l,DEC_CON(v,kind,conopt))
	    fun add_context_inline(CONTEXT c, l, v, inline) = CONTEXT((XCONTEXT_INLINE(l,v,convert' inline))::c)
	    fun add_context_entries(CONTEXT c, e : context_entry list) : context = 
		CONTEXT((map convert e) @ c)
	    fun add_context_signat(c, l, v, signat) = add_context_entries(c,[CONTEXT_SIGNAT(l,v,signat)])
	    fun add_context_sdecs(CONTEXT c, sdecs : sdecs) : context = CONTEXT((map XCONTEXT_SDEC sdecs) @ c)
	    fun add_context_sdec(CONTEXT c, sdec : sdec) : context = CONTEXT((XCONTEXT_SDEC sdec) :: c)
	    val add_context_dec = fn (ctxt,dec) => add_context_dec(ctxt,fresh_internal_label "anon",dec)

	    fun add_context_module' (ctxt,v,s) = add_context_dec(ctxt,DEC_MOD(v,s))
	    fun add_context_var' (ctxt,v,c) = add_context_dec(ctxt,DEC_EXP(v,c))
	    fun add_context_convar' (ctxt,v,k,copt) = add_context_dec(ctxt,DEC_CON(v,k,copt))


	    fun Context_Get_FixityTable (CONTEXT c) : fixity_table = 
		let 
		    fun help (XCONTEXT_SDEC(SDEC(_, DEC_FIXITY vflist))) = vflist
		      | help _ = []
		    val res = List.concat (map help c)
		in  res 
		end
	    fun Context_Get_BoundConvars (CONTEXT c) : var list = 
		let 
		    fun sdechelp (SDEC(l,d)) = 
			(case (is_label_open l, d) of
			     (true,DEC_MOD(v,s)) => sighelp s
			   | (_,DEC_CON (v,_,_)) => [v]
			       | (_,(DEC_FIXITY _ | 
				     DEC_EXCEPTION _ | DEC_EXP _ | DEC_MOD _)) => [])
		    and sighelp (SIGNAT_STRUCTURE sdecs) = flatten(map sdechelp sdecs)
		      | sighelp (SIGNAT_NORMAL_STRUCTURE (_,sdecs)) = flatten(map sdechelp sdecs)
		      | sighelp (SIGNAT_FUNCTOR _) = []
		    fun loop [] = []
		      | loop (XCONTEXT_SDEC(SDEC(l,dec))::rest) = 
			(case dec of 
			     (DEC_CON(v,k,co)) => v :: (loop rest)
			   | (DEC_MOD(v, s)) => if (is_label_open l) then (sighelp s) @ (loop rest) else loop rest
			   | _ => loop rest)
		      | loop ((XCONTEXT_INLINE _ | 
			       XCONTEXT_SIGNAT _)::rest) = loop rest
		in loop c
		end
	    
     (* ---------- Bound Rules from page 15 - 16 ----------------- *)
	    datatype bound_name = BOUND_VAR of var | BOUND_NAME of tag
	    local
		fun Dec_Bound (DEC_EXP (v,_)) = SOME(BOUND_VAR v)
		  | Dec_Bound (DEC_MOD (v,_)) = SOME(BOUND_VAR v)
		  | Dec_Bound (DEC_CON (v,_,_)) = SOME(BOUND_VAR v)
		  | Dec_Bound (DEC_EXCEPTION (n,_)) = SOME(BOUND_NAME n)
		  | Dec_Bound (DEC_FIXITY _) = NONE
		fun Ctxt_Bound (XCONTEXT_INLINE (_,v,_)) = SOME(BOUND_VAR v)
		  | Ctxt_Bound (XCONTEXT_SDEC(SDEC(_,d))) = Dec_Bound d
		  | Ctxt_Bound (XCONTEXT_SIGNAT(_,v,_)) = SOME(BOUND_VAR v)
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
		  (XCONTEXT_INLINE (l,v,b)::r) =>
		      if (eq_label(lab,l))
			  then (SIMPLE_PATH v,
				case b of
				    (XINLINE_MODSIG ms) =>  PHRASE_CLASS_MOD ms
				  | (XINLINE_EXPCON ec) =>  PHRASE_CLASS_EXP ec
				  | (XINLINE_CONKIND ck) => PHRASE_CLASS_CON ck
				  | (XINLINE_OVER over) =>  PHRASE_CLASS_OVEREXP over)
		      else if (is_label_open l)
			       then 
				   (case b of
					XINLINE_MODSIG (m as MOD_STRUCTURE sbnds,
						       s as SIGNAT_STRUCTURE sdecs) => 
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
		| (XCONTEXT_SDEC(SDEC(l,dec))::r) =>
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
						 ((SIGNAT_STRUCTURE sdecs) |
						  (SIGNAT_NORMAL_STRUCTURE (_,sdecs))) =>
						     (let val (pc,labels) = 
							  Sdecs_Lookup(MOD_VAR v, sdecs,[lab])
						      in  classpath2pc(COMPOUND_PATH(v,labels),
								       pc2class pc)
						      end
							  handle NOTFOUND _ => context_search lab r)
					       | _ => context_search lab r)
				   else context_search lab r
		        | ((DEC_EXCEPTION _) | (DEC_FIXITY _)) => context_search lab r)
	      | (XCONTEXT_SIGNAT (l,v,s)::r) => if (eq_label(lab,l)) 
							 then (SIMPLE_PATH v,PHRASE_CLASS_SIG s)
						     else context_search lab r
	      | _ => ((* debugdo (fn () => (print "Context_Lookup failed on ";
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
					     ((SIGNAT_STRUCTURE sdecs) |
					      (SIGNAT_NORMAL_STRUCTURE (_,sdecs))))) => 
			  let 
			      val (phrase,labels) = Sbnds_Lookup(sbnds,labrest)
			      val (pc,labels') = Sdecs_Lookup(path2mod path,sdecs,labrest)
			      val class = pc2class pc
			      val p = join_path_labels(path,labels)
			  in  (p,combine_pc(phrase,class))
			  end
		   | (path,PHRASE_CLASS_MOD(_,((SIGNAT_STRUCTURE sdecs) |
					       (SIGNAT_NORMAL_STRUCTURE(_,sdecs))))) =>
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
					     (SIGNAT_STRUCTURE sdecs |
					      SIGNAT_NORMAL_STRUCTURE (_,sdecs))=> 
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
			      PHRASE_CLASS_MOD (m',((SIGNAT_STRUCTURE sdecs') |
						    (SIGNAT_NORMAL_STRUCTURE (_,sdecs')))) => 
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
	  let fun loop [] = error ("var2label failed on var = " ^ (var2string tarv))
		| loop (this::rest) = 
		  (case this of
		       XCONTEXT_INLINE(l,v,_) => if (eq_var(v,tarv)) then l else loop rest
		     | XCONTEXT_SIGNAT(l,v,_) => if (eq_var(v,tarv)) then l else loop rest
		     | XCONTEXT_SDEC(SDEC(l,(DEC_MOD(v,_) | DEC_EXP(v,_) | DEC_CON(v,_,_)))) =>
			   if (eq_var(v,tarv)) then l else loop rest
		     | _ => loop rest)
	  in  loop entries
	  end
      fun Context_Lookup' (ctxt,v) = Context_Lookup(ctxt,[var2label(ctxt,v)])
      fun Context_Exn_Lookup (CONTEXT entries,tag) = 
	  let fun loop [] = raise (NOTFOUND ("Context_Exn_Lookup failed on " ^ (tag2string tag)))
		| loop ((XCONTEXT_SDEC(SDEC(_,DEC_EXCEPTION(t,c)))) :: rest) = 
		  if (eq_tag(t,tag)) then c else loop rest
		| loop (_::rest) = loop rest
	  in loop entries
	  end


	end
*)    
  end
