structure Signature :> SIGNATURE =
  struct


    open Il IlUtil Ppil
    open IlStatic
    open Util Listops Name Tyvar
    open IlContext Error


    fun error s : 'a = Util.error "signature.sml" s
    fun pat_error s : 'a = Util.error "signature.sml pattern impossibility" s
    fun elab_error s : 'a = Util.error "signature.sml elaborator impossibility" s

    val debug = Stats.ff("SignatureDebug")
    val diag = Stats.ff("SignatureDiag")
    val PermissiveSharing = Stats.tt("PermissiveSharing")

    fun msg s = if !debug then print s else ()

    fun debugdo t = if (!debug) then (t(); ()) else ()
    type labels = label list



 local
     val Cpolyinst : (context * sdecs -> sbnd list * sdecs * con list) ref =
	ref (fn _ => error "polyinst not installed")
     val Ceq_compile : (Il.context * Il.con -> (Il.exp * Il.con) option) ref =
	ref (fn _ => error "eq_compile not installed")
 in
     fun installHelpers
	{polyinst : context * sdecs -> sbnd list * sdecs * con list,
	 eq_compile : Il.context * Il.con -> (Il.exp * Il.con) option} : unit =
	(Cpolyinst := polyinst;
	 Ceq_compile := eq_compile)
     fun polyinst arg = !Cpolyinst arg
     fun eq_compile arg = !Ceq_compile arg
 end

    (* ----------------- Misc Helper Functions ----------------------- *)
    fun is_eta_expand vsig ([],[]) = true
      | is_eta_expand vsig ((SBND(l1,bnd))::sbnds, (SDEC(l2,dec))::sdecs) =
	(is_eta_expand vsig (sbnds,sdecs) andalso
	 (case (bnd,dec) of
	      (BND_CON(_,c), DEC_CON _) =>
		  (case con_deref c of
		       CON_MODULE_PROJECT(MOD_VAR v,l3) =>
			   (eq_label(l1,l2) andalso eq_label(l2,l3) andalso
			    eq_var(vsig,v))
		     | _ => false)
	    | (BND_EXP(_,_), DEC_EXP _) => eq_label(l1,l2)
	    | _ => false))
      | is_eta_expand _ _ = false

    fun is_polyval_sig (SIGNAT_FUNCTOR(var_poly,sig_poly,
				       SIGNAT_STRUCTURE [SDEC(maybe_it,DEC_EXP(_,c,eopt,i))], _)) =
	if (eq_label (maybe_it, it_lab))
	    then SOME(var_poly,sig_poly,c,eopt,i)
	else NONE
      | is_polyval_sig _ = NONE

    fun is_exception_sig (SIGNAT_STRUCTURE[_,SDEC(maybe_mk,DEC_EXP(_,c,_,_))]) =
        if eq_label (maybe_mk, mk_lab) then SOME c else NONE
      | is_exception_sig _ = NONE

    fun is_datatype_constr (PATH(_,labs)) =
      let val len = length labs
      in (not (len < 2)) andalso is_dt(List.nth(labs,len-2))
      end

    fun make_polyval_sig (var_poly,sig_poly,v,c,eopt,i) =
	SIGNAT_FUNCTOR(var_poly,sig_poly,SIGNAT_STRUCTURE
		       [SDEC(it_lab,DEC_EXP(v,c,eopt,i))],TOTAL)

    (* ---------------- helpers ---------------- *)

    exception SharingError
    exception WhereError
    type vlpath = (var * label) list
    type lpath = label list
    type cluster = path option ref * lpath list
    fun eq_lpath'(lpath1,lpath2) = Listops.eq_list(eq_label,lpath1,lpath2)
    fun eq_lpath lpath1 lpath2 = Listops.eq_list(eq_label,lpath1,lpath2)
    fun vlpath_eq_lpath(vlpath1,vlpath2) =
	let fun eq_vl((_,l1),l2) = eq_label(l1,l2)
	in  Listops.eq_list(eq_vl, vlpath1, vlpath2)
	end
    fun sub_lpath _ [] = false
      | sub_lpath [] _ = true
      | sub_lpath (l1::lrest1) (l2::lrest2) = eq_label(l1,l2) andalso sub_lpath lrest1 lrest2
    fun inter_lpaths lpaths1 lpaths2 = Listops.list_inter_eq(eq_lpath',lpaths1,lpaths2)

    fun pp_lpath lpath = pp_pathlist pp_label' lpath
    fun pp_vlpath vlpath = pp_pathlist (fn (v,l) => Formatter.Hbox[pp_var' v, pp_label' l]) vlpath


    fun vlpath2lpath (vlpath : vlpath) = map #2 vlpath
    fun vlpath2path [] = elab_error "vlpath2path got empty path"
      | vlpath2path ((v,_)::vlrest) = PATH(v,map #2 vlrest)

  datatype typeslot = ABSTRACT of label list * kind
		    | CONCRETE of label list * kind * con

  fun splitAbstractConcrete slots =
      let fun folder (ABSTRACT(labs,_),(abs,conc)) = (labs::abs, conc)
	    | folder (CONCRETE(labs,_,_),(abs,conc)) = (abs, labs::conc)
      in  foldl folder ([],[]) slots
      end

  fun splitAbstract slot_pairs =
      let fun mapper (ABSTRACT(labs1,k1),ABSTRACT (labs2,k2)) = if eq_kind(k1,k2) then SOME [labs1,labs2] else NONE
	    | mapper _ = NONE
      in  List.mapPartial mapper slot_pairs
      end

  fun pp_typeslot (ABSTRACT (labels,k)) = (print "ABSTRACT ";
					   pp_lpath labels;
					   print "  -->  ";
					   pp_kind k)
    |  pp_typeslot (CONCRETE (labels,k,c)) = (print "CONCRETE ";
					      pp_lpath labels;
					      print "  -->  ";
					      pp_kind k;
					      print "  = ";
					      pp_con c)

  (* follow_labels looks up the "labels" type component of
     sdecs and follows type definitions to determine whether it is
     flexible or rigid (ABSTRACT or CONCRETE).  In order to do so,
     a variable is put into the context (unless pathopt is supplied)
     with the sdecs signature.  If the normalized form of the "labels"
     component is a path with that variable at its head, then it is
     flexible, otherwise rigid.
   *)

  fun follow_labels (pathopt : path option,sdecs,ctxt) : labels -> typeslot =
      let
	  val (v,path,ctxt) =
	      (case pathopt of
		   NONE => let val v = fresh_named_var "modtemp"
			       val path = PATH (v,[])
			       val ctxt = add_context_mod'(ctxt,v,SIGNAT_STRUCTURE sdecs)
			   in  (v,path,ctxt)
			   end
		 | SOME (path as PATH(v,_)) => (v,path,ctxt))
	  val module = path2mod path
      in  fn labels =>
	  let val c = path2con(join_path_labels(path,labels))
	      val k = GetConKind(ctxt,c)  (* XXX not efficient *)
	      val c' = con_normalize(ctxt,c)
	  in  case (con2path c') of
	      SOME p => let val PATH(v',lbls) = p
			in  if (eq_var(v,v'))
				then ABSTRACT (lbls,k)
			    else CONCRETE (labels,k,c')
			end
	    | NONE => CONCRETE (labels,k,c')
	  end
      end

     fun signat2sdecs ctxt s =
	 (case reduce_signat ctxt s of
	      SIGNAT_STRUCTURE sdecs => SOME sdecs
	    | s => NONE)

     fun find_labels_sdecs context sdecs =
	 let fun driver path sdecs = rev(foldl (find_labels path) [] sdecs)
	     (* the path being carried is backwards so we must reverse when we add to accumulator *)
	     and find_labels path (SDEC(l,DEC_CON (_,k,NONE,_)),_) =
		 (debugdo(fn () =>
			  (print "sdecs = "; pp_sdecs sdecs; print "\n";
			   print "offending type is ";
			   pp_pathlist pp_label' (rev (l :: path));
			   print "\n"));
		  error "find_labels should not encounter any abstract types")
	       | find_labels path (SDEC(l,DEC_CON (_,k,SOME _,_)),kpaths) =  (k,rev(l::path))::kpaths
	       | find_labels path (SDEC(l,DEC_MOD (v,_,s)),kpaths) =
		 (case signat2sdecs context s of
		      SOME sdecs => (driver (l::path) sdecs) @ kpaths
		    | NONE => kpaths)
	       | find_labels path (_,acc) = acc
	     val rev_klabs = foldl (find_labels []) [] sdecs
	 in  driver [] sdecs
	 end


    (* ---------------------------------------------------------
      ------------------ SIGNATURE PATCHING -------------------
      --------------------------------------------------------- *)


    fun rewrite_type_component (ctxt,orig_sdecs,labs,con) =
	let
	    local val fv = con_free con
	    in    fun bound v =
	            if (Name.VarSet.member(fv,v))
			then (error_region();
			      print "signature wheretype leads to variable capture\n")
		    else ()
	    end
	    fun docon curl sdecs : sdecs =
		(case sdecs of
		     [] => (error_region();
			    print "signature wheretype could not find specified component ";
			    pp_label curl; print "\n";
			    tab_region();
			    pp_lpath labs;
			    print "\n";
			  print "orig_sdecs = "; pp_sdecs orig_sdecs; print "\n";
			  [])
		 | ((sdec as SDEC(l,dec))::rest) =>
		       (case dec of
			    DEC_CON(v,k,NONE,i) =>
				(bound v;
				 if eq_label(l,curl)
				     then (SDEC(l,DEC_CON(v,k,SOME con,i)))::rest
				 else sdec::(docon curl rest))
			  | DEC_EXP(v,_,_,_) => (bound v; sdec::(docon curl rest))
			  | DEC_CON(v,_,_,_) => (bound v; sdec::(docon curl rest))
			  | DEC_MOD(v,_,_)  => (bound v; sdec::(docon curl rest))))
	  fun domod [] sdecs = elab_error "rewrite_type_component got empty lbls"
	    | domod [l] sdecs = docon l sdecs
	    | domod (curl::restl) sdecs =
	      let
		  fun loop [] : sdecs =
		      (error_region();
		       print "signature wheretype could not find component ";
		       pp_label curl; print " in sdecs:\n";
		       pp_sdecs sdecs; print "\n";
		       [])
		    | loop ((sdec as SDEC(l,DEC_EXP(v,_,_,_)))::rest) = (bound v; sdec::(loop rest))
		    | loop ((sdec as SDEC(l,DEC_CON(v,_,_,_)))::rest) = (bound v; sdec::(loop rest))
		    | loop ((sdec as SDEC(l,DEC_MOD(v,b,s)))::rest) =
		      (bound v;
		       if eq_label(l, curl)
			   then (case (reduce_signat ctxt s) of
				     SIGNAT_STRUCTURE sdecs =>
					 (SDEC(l,DEC_MOD(v,b,SIGNAT_STRUCTURE
							 (domod restl sdecs))))::rest
				   | s => (error_region();
					   print "signature not reduced to structure signature";
					   pp_signat s;
					   sdec::(loop rest)))
		       else sdec::(loop rest))
	      in loop sdecs
	      end
	in  domod labs orig_sdecs
	end

      fun xsig_sharing_rewrite (ctxt,sdecs) =
        let val v = fresh_named_var "mod_sharing_temp"
	    val s = SIGNAT_STRUCTURE sdecs
	    val ctxt = add_context_mod'(ctxt,v,s)
	    fun combine_path first [] = path2con(vlpath2path first)
	      | combine_path [] _ = error "bad paths"
	      | combine_path (first as ((v,_)::firstrest))
			      ((v',_)::secondrest) =
			      if (eq_var(v,v'))
				  then combine_path firstrest secondrest
			      else path2con(vlpath2path first)
	in  fn (typeslots : lpath list list, sdecs) =>
            let
	      val table = map (fn slots => (ref NONE, slots)) typeslots
	      fun find_representative (curpath : lpath) : vlpath option ref list =
		  let fun loop ([],[]) = error "find_representative failed"
			| loop ([],acc) = acc
			| loop ((rep,tslots : lpath list)::rest,acc) =
		      if (Listops.member_eq(eq_lpath',curpath,tslots))
			  then loop(rest,rep::acc)  else loop (rest,acc)
		  in  loop (table,[])
		  end
              fun transparent (curpath : (var * label) list, copt) =
		  let
		    val reps = (find_representative (map #2 curpath))
		    fun loop [rep as ref NONE] = NONE
		      | loop ((rep as ref NONE)::rest) = loop rest
		      | loop (rep::rest) = !rep
		    val repopt = loop reps
		    val (rep,res) =
		      case repopt
			of NONE => (curpath,copt)
			 | SOME path => (path,SOME (combine_path path curpath))
		    val _ = app (fn (r as ref NONE) => r := SOME rep | _ => ()) reps
		  in res
		  end
	      fun separate (target : vlpath) (candidates : lpath list) =
		let val target = map #2 target
		    fun prefix ([],_) = true
		      | prefix (_,[]) = false
		      | prefix (a::aa,b::bb) = eq_label(a,b) andalso prefix(aa,bb)
		    fun folder (cur,(match,mismatch)) = if (prefix(target,cur))
							    then (cur::match,mismatch)
							else (match,cur::mismatch)
		in  foldl folder ([],[]) candidates
		end


	      fun traverse (_,[]) [] = []
		| traverse (_,typeslots) [] = (print "traverse finished but there are leftover typeslots\n";
					       app (fn lp => (pp_lpath lp; print "\n")) typeslots;
					       error "traverse finished but there are leftover typeslots")
	        | traverse (_,[]) sdecs = sdecs
                | traverse (cur_path,typeslots) ((SDEC(l,dec))::rest) =
		  let val this_v = (case dec of
					DEC_CON(v,_,_,_) => v
				      | DEC_MOD(v,_,_) => v
				      | DEC_EXP(v,_,_,_) => v)
		      val cur_path' = cur_path @ [(this_v,l)]
		      val (this_typeslots, typeslots) = separate cur_path' typeslots
		      val dec' =
			  (case (this_typeslots,dec) of
			       ([], _) => dec
			     | (_, DEC_CON(v,k,copt,i)) =>
				   let val copt = transparent(cur_path',copt)
				   in  DEC_CON(v,k,copt,i)
				   end
			     | (_, DEC_MOD(v,b,s)) =>
				  (case (reduce_signat ctxt s) of
				       SIGNAT_STRUCTURE sdecs =>
					   let val is_first =
					       (case this_typeslots of
						    [this] => andfold (fn x => not (isSome (!x))) (find_representative this)
						  | _ => false)
					       val sdecs' = traverse(cur_path',this_typeslots) sdecs
					   in  DEC_MOD(v,b,if is_first then s
							   else SIGNAT_STRUCTURE sdecs')
					   end
				     | _ => dec)
			     | _ => dec)
		  in (SDEC(l,dec')) :: (traverse (cur_path,typeslots) rest)
		  end
	    in traverse ([],Listops.flatten typeslots) sdecs
	    end (* fn *)
        end (* xsig_sharing_rewrite *)

    fun xsig_where_type(ctxt,orig_sdecs,lpath, con, kind) : sdecs =
	let val _ = debugdo (fn () =>
			     (print "-------xsig_where_type-------\n";
			      print "lpath = "; pp_lpath lpath; print "\n";
			      print "con = "; pp_con con; print "\n";
			      print "kind = "; pp_kind kind; print "\n"))
	    val mjunk = fresh_named_var "xsig_where_type_mjunk"
	    val mpath = PATH(mjunk,[])
	    val msig = SIGNAT_STRUCTURE orig_sdecs
	    val ctxt = add_context_mod'(ctxt,mjunk,msig)
	in
	    (case (follow_labels (SOME mpath,orig_sdecs,ctxt) lpath) of
		 ABSTRACT (lbls,k) => if (eq_kind(k,kind))
					  then rewrite_type_component (ctxt, orig_sdecs, lbls, con)
				      else
					  (error_region();
					   print "kind mismatch in where type of component:";
					   pp_lpath lbls;
					   print "\n";
					   orig_sdecs)
	       | CONCRETE (lbls,_,c) => let val _ = debugdo (fn () => print "where_type concrete case\n")
					in  if (eq_con(ctxt,con,c))
						then orig_sdecs
					    else (error_region();
						  print "cannot where type CONCRETE but unequal type component:";
						  pp_lpath lbls;
						  print "\n";
						  print "Actual type: "; pp_con (con_normalize(ctxt,c)); print "\n";
						  print "Target type: "; pp_con (con_normalize(ctxt,con)); print "\n";
						  orig_sdecs)
					end)
	end


    (* We have to selfify the sdecs so lookup works correctly,
         but then we must recognize that some transparent type
	   slots are actually opaque *)
    and xsig_where_structure(context,sdecs,labs1,m2,sig2) =
	let val _ = debugdo (fn () =>
			     (print "-------xsig_where_structure-------\n";
			      print "sdesc = "; pp_sdecs sdecs; print "\n";
			      print "labs1 = "; pp_lpath labs1; print "\n";
			      print "m2 = "; pp_mod m2; print "\n";
			      print "sig2 = "; pp_signat sig2; print "\n"))
	    val SOME sdecs2 = signat2sdecs context sig2
	    val mjunk_var = fresh_named_var "mjunk_where_structure"
	    val mjunk = MOD_VAR mjunk_var
	    val context = add_context_mod'(context,mjunk_var,SIGNAT_STRUCTURE sdecs)
	    val (labels1,s1) =
		(case Context_Lookup_Path(context,PATH(mjunk_var,labs1)) of
		     SOME(PATH(_,labels1 as _::_),PHRASE_CLASS_MOD(_,_,s1,_)) => (labels1,s1)
		   | _ => (error_region();
			   print "can't where non-existent or non-structure component\n";
			   raise WhereError))
	    val nlabels1 = length labels1

            (* kinds and reversed partial lpaths *)
	    val SOME sdecs1 = signat2sdecs context s1
	    val kind_plabels = find_labels_sdecs context sdecs1

            (* as we constrain, some ABSTRACT paths become CONCRETE so
	      we cannot follow_labels all at once at the beginning *)
	    fun constrain((k,plabs),sdecs) =
		let val labs = labels1 @ plabs
		in  (case follow_labels (NONE,sdecs,context) labs of
		       ABSTRACT (labs',_) =>
			   let fun use (c:con) : sdecs =
				   xsig_where_type(context,sdecs,labs',c,k)
			   in
			       (case (Sdecs_Lookup context (m2,sdecs2,plabs)) of
				    SOME(_,PHRASE_CLASS_CON(_,_,SOME c,true)) => use c
				  | SOME(_,PHRASE_CLASS_CON(c,_,_,_)) => use c
				  | _ => (error_region();
					  print "where's rhs structure missing components\n";
					  sdecs))
			   end
		     | CONCRETE _ => sdecs)
		end

	in  foldl constrain sdecs kind_plabels
	end
    handle WhereError => sdecs
	 | e => (debugdo (fn () =>
			  (print "\nsig2 = "; pp_signat sig2;
			   print "\nsdecs= "; pp_sdecs sdecs;
			   print "\n"));
		 raise e)

  (* ------ These structure components are being shared ----------- *)
  (* Look up, using the star convention, a path to a structure and
     return its actual path and its sdecs. *)
  fun path2triple (ctxt : context, v : var, lpath : labels) : path * labels * sdecs =
      (case Context_Lookup_Path (ctxt,PATH(v,lpath))
	 of SOME (path as PATH (_,labs), PHRASE_CLASS_MOD (_,_,SIGNAT_STRUCTURE sdecs,_)) =>
	     (path,labs,sdecs)
	  | _ => (error_region();
		  print "structure sharing given a non-structure component: ";
		  pp_lpath lpath; print "\n";
		  raise SharingError))

  and xsig_sharing_structure(ctxt,sdecs,lpath1, lpath2: lpath) : sdecs =
      let
	  val mjunk = fresh_named_var "mjunk_sharing_structure"
	  val s = SIGNAT_STRUCTURE sdecs
	  val ctxt' = add_context_mod'(ctxt,mjunk,s)

	  val (path1,lpath1,sdecs1) = path2triple (ctxt',mjunk,lpath1)
	  val (path2,lpath2,sdecs2) = path2triple (ctxt',mjunk,lpath2)

	  val slabs1 = map #2 (find_labels_sdecs ctxt' sdecs1)
	  val slabs2 = map #2 (find_labels_sdecs ctxt' sdecs2)
	  val slabs = inter_lpaths slabs1 slabs2
	  val slabs_abs =
	    let
	      val f1 = (follow_labels(SOME path1, sdecs1, ctxt'))
	      val f2 = (follow_labels(SOME path2, sdecs2, ctxt'))
	      val slot_pairs = map (fn ls => (f1 ls,f2 ls)) slabs
	    in splitAbstract slot_pairs
	    end
	  val sdecsAbstractEqual = xsig_sharing_rewrite(ctxt,sdecs) (slabs_abs,sdecs)
	  val _ = debugdo (fn () => (print "\n";pp_signat (SIGNAT_STRUCTURE(sdecsAbstractEqual));print "\n"))
	  val sigAbstractEqual= SIGNAT_STRUCTURE(sdecsAbstractEqual)
	  val ctxt'' = add_context_mod'(ctxt,mjunk,sigAbstractEqual)

	  fun check labs =
	    let
	      val c  = path2con(join_path_labels(path1,labs))
	      val c' = path2con(join_path_labels(path2,labs))
	      val res = (Name.is_label_internal(List.last labs))
		orelse eq_con(ctxt'',c,c')
	      in
		if res then ()
		else
		  let
		    val A = lpath1
		    val B = lpath2
		    val At = lpath1 @ labs
		    val Bt = lpath2 @ labs
		    val reduced_c = con_normalize(ctxt'',c)
		    val reduced_c' = con_normalize(ctxt'',c')
		    val yell = if !PermissiveSharing then warn_region_with else error_region_with
		  in
		    yell "Incompatible types encountered while expanding sharing spec:\n";
		    print "\t\t"; print "sharing "; pp_lpath A; print " = "; pp_lpath B; print "\n";
		    print "\t\t"; pp_lpath At; print " = "; pp_con reduced_c; print "\n";
		    print "\t\t"; pp_lpath Bt; print " = "; pp_con reduced_c'; print "\n";
		    print "\t\t"; print "Types cannot be made equal!\n";
		    print "\n\n"
		  end
	      end
      in
	app check slabs;
	sdecsAbstractEqual
      end

  and xsig_sharing_structures(ctxt,sdecs,[])  = sdecs
    | xsig_sharing_structures(ctxt,sdecs,[_]) = sdecs
    | xsig_sharing_structures(ctxt,sdecs,p1::p2::rest) : sdecs =
      let  val _ = if (!debug)
		       then print "xsig_sharing_structures started\n"
		   else ()
	   val sdecs = xsig_sharing_structure(ctxt,sdecs,p1,p2)
	   val _ = if (!debug)
		       then (print "xsig_sharing_structures half-done\n") else ()
	   val sdecs = xsig_sharing_structures(ctxt,sdecs,p2::rest)
	   val _ = if (!debug)
		       then (print "xsig_sharing_structures finished with\n";
			     pp_signat (SIGNAT_STRUCTURE sdecs); print "\n\n")
		   else ()
      in  sdecs
      end

  (* ------ These type components are being shared ----------------- *)
  fun xsig_sharing_type(ctxt,sdecs,path1,path2) : sdecs =
      let val _ = if (!debug)
		      then print "xsig_sharing_type started\n"
		  else ()
	  val mjunk = fresh_named_var "mjunk_sharing_type"
	  val ctxt = add_context_mod'(ctxt,mjunk,SIGNAT_STRUCTURE sdecs)
	  fun path2label (lpath : labels) : typeslot =
	      (case (Sdecs_Lookup ctxt (MOD_VAR mjunk,sdecs,lpath)) of
		   SOME(labs,_) => follow_labels (NONE,sdecs,ctxt) labs
		 | NONE => (error_region();
			    print "sharing type given non-existent path ";
			    pp_lpath lpath; print "\n";
			    raise SharingError))
	  val typeslot1 = path2label path1
	  val typeslot2 = path2label path2
	  val res = (case (typeslot1,typeslot2) of
			 (ABSTRACT(lbls1,k1), ABSTRACT(lbls2,k2)) =>
			     if (eq_kind(k1,k2))
				 then xsig_sharing_rewrite(ctxt,sdecs)([[lbls1,lbls2]],sdecs)
			     else (error_region_with "cannot share abstract types with unequal kinds\n";
				   sdecs)
		       | (CONCRETE (lbls1,k1,c1), CONCRETE (lbls2,k2,c2)) =>
				 if (eq_con(ctxt,c1,c2))
				     then sdecs
				 else (error_region_with "sharing of two unequal concrete types\n";
				       sdecs)
		       | _ => (error_region_with "cannot share abstract with concrete type:\n";
				     pp_typeslot typeslot1; print "\n";
				     pp_typeslot typeslot2; print "\n";
				     sdecs))
	  val _ = if (!debug)
		      then print "xsig_sharing_type finished\n"
		  else ()
      in   res
      end
  handle SharingError => sdecs

  and xsig_sharing_types(ctxt,sdecs,[])  = sdecs
    | xsig_sharing_types(ctxt,sdecs,[_]) = sdecs
    | xsig_sharing_types(ctxt,sdecs,p1::p2::rest) : sdecs =
      let val sdecs = xsig_sharing_type(ctxt,sdecs,p1,p2)
      in  xsig_sharing_types(ctxt,sdecs,p2::rest)
      end

    (* ---------------------------------------------------------
      ------------------ COERCION COMPILATION-------------------
      --------------------------------------------------------- *)

    (* the variables of sdecs_change is alpha-varied to match variables of
       corresponding components of sdecs_name *)
    fun sdecs_rename(sdecs_change, sdecs_name) =
	let
	    fun folder (sdec_change as (SDEC(l,dec_change)),subst) =
		(case find_sdec (sdecs_name,l) of
		     SOME(SDEC(_,dec_name)) =>
			 (case (dec_change, dec_name) of
			      (DEC_EXP(v,c,e,i), DEC_EXP(v',_,_,_)) =>
				  (SDEC(l,DEC_EXP(v',c,e,i)),
				   subst_add_expvar(subst,v,VAR v'))
	                     | (DEC_CON(v,k,c,i), DEC_CON(v',_,_,_)) =>
				  (SDEC(l,DEC_CON(v',k,c,i)),
				   subst_add_convar(subst,v,CON_VAR v'))
	                     | (DEC_MOD(v,s,p), DEC_MOD(v',_,_)) =>
				  (SDEC(l,DEC_MOD(v',s,p)),
				   subst_add_modvar(subst,v,MOD_VAR v'))
			     | _ => (sdec_change,subst))
		   | _ => (sdec_change, subst))
	    val (sdecs_change,subst) = foldl_acc folder empty_subst sdecs_change
	    val sig_temp = SIGNAT_STRUCTURE sdecs_change
	    val SIGNAT_STRUCTURE sdecs_result = sig_subst(sig_temp,subst)
	in  sdecs_result
	end

	(* Coercion of a monoval to match a monoval spec. *)
	fun coerce_monoval (ctxt:context,
			    spec : var * con * exp option * bool,
			    path_actual : path,
			    actual : (label list * phrase_class) option) : (bnd * dec) option =
	    let val (v_spec,con_spec,eopt,_) = spec
	    in
		(case actual of
		    SOME(lbls,PHRASE_CLASS_EXP (_,con_actual,eopt',inline)) =>
			if (sub_con(ctxt,con_actual,con_spec) andalso
			    (case (eopt,eopt') of
				(SOME _, NONE) => false
			     |	_ => true)) (* XXX: We used to call IlStatic.eq_exp in (SOME, SOME) case. *)
			then
			    let val exp =
				    (case (inline,eopt') of
					(true, SOME e) => e
				    |	_ => path2exp(join_path_labels(path_actual,lbls)))
				(* Attempt to make unsealed signature smaller. *)
				val con_actual_reduced = con_normalize(ctxt, con_actual)
				val con_actual_smaller =
				    if (con_size con_actual) < (con_size con_actual_reduced)
				    then con_actual else con_actual_reduced
				val bnd = BND_EXP(v_spec,exp)
				(* Derek: Why do we put in the inlining info of the "from" component? *)
				(* NB dec is only used by transparant ascription. *)
				val dec = DEC_EXP(v_spec,con_actual_smaller,eopt',inline)
			    in  SOME(bnd,dec)
			    end
			 else NONE
		 |  _ => NONE)
	    end

       (* ---- coercion of a poly component to a mono/poly specification --- *)
       fun polyval_case (ctxt : context, coerce : bool * string -> unit)
	   {name : var, (* use this name for the final binding *)
	    path : path, (* this path is to the actual polymorphic component *)
	    varsig_spec : (var * signat) option, (* argument & argument signature of the spec,
						    unless it's monomorphic *)
	    con_spec : con, (* the type of the specification *)
	    var_actual : var, (* argument of functor signature of actual component *)
	    eopt : exp option, (* optional inlined definition of actual component *)
	    inline : bool, (* whether to inline it *)
	    sdecs_actual : sdec list, (* argument signature of functor signature of actual component *)
	    con_actual : con} (* the type of the actual component *)
		  : (bnd * dec) option =
	   let
	       (* make con_spec refer to var_actual instead of var_spec *)
	       val con_spec =
		   (case varsig_spec of
			NONE => con_spec
		      | SOME(v,_) => con_subst(con_spec,subst_add_modvar(empty_subst, v, MOD_VAR var_actual)))

	       fun local_error () =
		   (error_region_with "Coercion of a polymorphic value component to a\n";
		    error_region_with "  monomorphic/polymorphic specification failed at ";
		    pp_path path; print "\n";
		    error_region_with "Expected type: ";
		    (case varsig_spec of
			 NONE => pp_con con_spec
		       | SOME (v,s) => pp_signat (make_polyval_sig (v,s,name,con_spec,NONE,false)));
		    print "\n";
		    error_region_with "Actual type: ";
		    pp_signat  (make_polyval_sig (var_actual,
						  SIGNAT_STRUCTURE sdecs_actual,
						  name,con_actual,NONE,false));
		    print "\n";
		    NONE)


	       val ctxt' =
		   (case varsig_spec of
			NONE => ctxt
		      | SOME(_,sig_spec) =>
			    add_context_mod'(ctxt,var_actual,sig_spec))

	       val (sbnds_poly,sdecs_poly,_) = polyinst(ctxt',sdecs_actual)

	       local
		   fun folder (SBND(l,BND_CON(v,c)),subst) =
		       subst_add_conpath(subst, PATH(var_actual, [l]), c)
		     | folder (_,subst) = subst
		   val subst = foldl folder empty_subst sbnds_poly
	       in
		   (* This substitutes the unifiable tyvars generated by
		      polymorphic instantiation into the type and optional
		      inlined body of the actual component.
                    *)
		   val con_actual_tyvar = con_subst(con_actual,subst)
		   val eopt_tyvar = (case eopt of
		                       NONE => NONE
				     | SOME e => SOME(exp_subst(e,subst)))
	       end
	       (**) val _ = if (!debug)
			   then (print "con_actual_tyvar = ";
				 pp_con con_actual_tyvar; print "\n";
				 print "spec_con = "; pp_con con_spec; print "\n";
				 (case varsig_spec of
				      NONE => print "no varsig_spec\n"
				    | SOME(v,s) => (print "varsig_spec = "; pp_var v;
						    print "\n"; pp_signat s; print "\n")))
		       else () (**)
	   val res =
	       (* First unify and solve for the tyvars. *)
	       if sub_con(ctxt',con_actual_tyvar,con_spec)
		 then
		   ((case varsig_spec of
			 (* when the spec is monomorphic *)
			 NONE =>
			     let val mtemp = MOD_APP(path2mod path,MOD_STRUCTURE sbnds_poly)

				 val exp_actual_tyvar =
				     (case (eopt_tyvar,inline) of
					  (SOME e,true) => e
					| _ => MODULE_PROJECT(mtemp,it_lab))
				 val _ = coerce(true,"polyval")
			     in  SOME(BND_EXP(name,exp_actual_tyvar),
				      DEC_EXP(name,con_actual_tyvar,eopt_tyvar,inline))
			     end
			 (* when the spec is polymorphic *)
		       | SOME (_,s1) =>
			   let
			       val pathmod = path2mod path
			       val mtemp = MOD_APP(pathmod, MOD_STRUCTURE sbnds_poly)
			       val dummy_var = fresh_var()
			       val inner_sig =
				   SIGNAT_STRUCTURE [SDEC(it_lab,
							  DEC_EXP(dummy_var,
								  con_actual_tyvar,
								  eopt_tyvar,inline))]
			       val inner_mod =
				   (case (eopt_tyvar,inline) of
					(SOME e,true) => MOD_STRUCTURE [SBND(it_lab,BND_EXP(dummy_var,e))]
				      | _ => mtemp)
			       val s2 = SIGNAT_FUNCTOR(var_actual,s1,inner_sig,TOTAL)
			       (* only need to coerce if spec is less polymorphic,
				  i.e. if unification did nothing *)
			       val coerced = not (is_eta_expand var_actual (sbnds_poly,sdecs_poly))
			       (*
				  Even if coercion was unnecessary, we must inline if the actual
				  component was a datatype constructor.  This has the effect of
				  eliminating references to the inner module of a datatype, which
				  is necessary in order for the phase-splitter to discard those inner
				  modules.

				  Note, however, that this condition does not affect the
				  coerced flag.  Thus, most of the time, e.g. when a datatype is matching
				  a datatype signature and a coercion is really unnecessary, the inner
				  module will not be inlined, merely path-copied.  This optimization is OK,
				  because inlining is only really necessary when coercing a datatype
				  constructor to a value spec.  All other coercions of datatype
				  constructors will be to (components of) an inner module of a datatype
                                  and will thus be eliminated by the phase-splitter anyway.
			       *)
			       val m = if coerced orelse (is_datatype_constr path)
					   then MOD_FUNCTOR(TOTAL,var_actual,s1,inner_mod,inner_sig)
				       else pathmod
			       val _ = coerce(coerced,"polyval")
			   in  SOME(BND_MOD(name, true, m),
				    DEC_MOD(name, true, s2))
			   end))
	       else local_error()

	   val _ =
		(case (!debug,res)
		   of (true, SOME (bnd,dec)) =>
			(print "polyval: result bnd = ";
			 pp_bnd bnd; print "\nresult dec = ";
			 pp_dec dec; print "\n\n")
		    | _ => ())
	   in  res
	   end



    (* The resulting module and signature will involve var_actual.
       The sdecs will be ordered as in sig_target.

	 The main algorithm maintains:
	       The current context to perform typecheck signature matching.
	       The signature whose order we are using to traverse and create
	         the final module and signature.
              The current list of labels indicating our current position.
	          This list allows us to look up components in the
		    sig_actual and sig_target to type-check.
    *)

    and xcoerce (context : context,
		 path_actual : path,
		 sig_actual : signat,
		 sig_target : signat) : (bool * Il.mod * Il.signat) =
	let val sig_actual = reduce_signat context sig_actual
	    val sig_target = deep_reduce_signat context sig_target
	in  (case (sig_actual,sig_target) of
		 (SIGNAT_FUNCTOR(v1,s1,s1',a1),
		  SIGNAT_FUNCTOR(v2,s2,s2',a2)) =>
		 let
		     val _ = if (a1 = a2) then ()
			     else raise (FAILURE "arrow mismatch in xcoerce")
		     val p2 = PATH(v2,[])
		     val context' = add_context_mod'(context,v2,s2)
		     val (_,m3,_) = xcoerce(context',p2,s2,s1)
		     val subst = subst_add_modvar(empty_subst,v1,MOD_VAR v2)
		     val s1' = sig_subst(s1',subst)
		     val v1' = fresh_named_var "var_actual_xcoerce"
		     val p1' = PATH(v1',[])
		     val context'' = add_context_mod'(context',v1',s1')
		     val (_,m4,_) = xcoerce(context'',p1',s1',s2')
		     val arg = MOD_APP(path2mod path_actual, m3)
		     val body = MOD_LET(v1',arg,m4)
		 in (true,
		     MOD_FUNCTOR(a1,v2,s2,body,s2'),
		     sig_target)
		 end
	   | (SIGNAT_STRUCTURE sdecs_actual,
	      SIGNAT_STRUCTURE sdecs_target) =>
		 xcoerce_structure (context, path_actual, sdecs_actual, sdecs_target)
	   | (SIGNAT_FUNCTOR _, _) => (error_region_with "cannot coerce a functor to a structure\n";
				       (true, MOD_STRUCTURE [], SIGNAT_STRUCTURE []))
	   | (_, SIGNAT_FUNCTOR _) => (error_region_with "cannot coerce a structure to a functor\n";
				      (true, MOD_STRUCTURE [], SIGNAT_STRUCTURE []))
	   | _ => (print "xcoerce got either non-reduced sig_actual:\n";
		   pp_signat sig_actual; print "\n";
		   print "or xcoerce got non-reduced sig_target:\n";
		   pp_signat sig_target; print "\n";
		   error "xcoerce should get reduced sig"))
	end

   and xcoerce_structure (ctxt : context,
			  path_actual : path,
			  sdecs_actual : sdec list,
			  sdecs_target : sdec list) : (bool * Il.mod * Il.signat) =
      let

	  val coerced = ref false
	  fun coerce (false,_) = ()
	    | coerce (true,str) = (if (!debug)
				       then (print "coerced because of "; print str; print "\n")
				   else ();
				   coerced := true)
(*	  val sdecs_target = sdecs_rename(sdecs_target, sdecs_actual) *)
	  local
	      val sig_actual_self = GetModSig (ctxt,path2mod path_actual)
	      val SIGNAT_STRUCTURE sdecs_actual_self = reduce_signat ctxt sig_actual_self
	      val _ = debugdo (fn () =>
			       (print "\n\n-------xcoerce_structure------\n";
				print "path_actual = "; pp_path path_actual; print "\n";
				print "sdecs_actual = \n"; pp_sdecs sdecs_actual; print "\n";
				print "sdecs_actual_self = \n"; pp_sdecs sdecs_actual_self; print "\n";
				print "sdecs_target = \n"; pp_sdecs sdecs_target; print "\n"))
	      val _ = if (Listops.eq_list (fn (SDEC(l1,_),SDEC(l2,_)) => eq_label(l1,l2),
					   sdecs_actual, sdecs_target))
			  then ()
		      else coerce (true,"length/order mismatch")
	      val self = path2mod path_actual
	  in  fun actual_self_lookup lbl = Sdecs_Lookup ctxt (self, sdecs_actual_self, [lbl])
	  end

        val xcoerce_error = ref false

        val error_region_with = fn str => (xcoerce_error := true; Error.error_region_with str)

	fun eqtype_error (eqlab:label) =
	    (error_region_with "type component does not admit equality: ";
	     print(Name.label2name' eqlab); print "\n";
	     NONE)

	fun doit ctxt (lab,spec_dec) : (bnd * dec) option =

	    (case (Name.is_eq lab, spec_dec, actual_self_lookup lab) of

		(* --------- coercion of an equality function spec to an equality function ---- *)
		(*
			For these cases, actual_self_lookup sometimes return SOME e where
			e is the wrong equality function.  This occurs with code like

				structure B :> sig eqtype t end =
				struct
					structure Eqtype :> sig eqtype t end =
					struct type t = unit end
					open Eqtype
					type t = int -> int
				end

			here the label +Et (Eqtype*.+Et) would be found by
			actual_self_lookup but it would be the wrong equality function.  The
			equality compiler can not get confused in this case because it uses
			a context and adding a type labelled t to context always shadows
			+Et.  With EqPayAsYouGo set, you can change "int -> int" to "unit"
			to  an example that TILT erroneously rejects.

			In the monomorphic case, we can not always invoke the equality
			compiler because while we are coercing the primitive unit to the
			primitive interface, the equality compiler does not just work: It
			must be invoked speically because bool is defined in that unit.
			Rather than special case for the primitive unit, we invoke the
			equality compiler only if the candidate equality function found by
			acutal_self_lookup has the wrong type.

			We use eqfun_con to get the actual type definition rather than
			looking back in sdecs_target.  In the monomorphic case,
			con_eqfun_spec has the form con*con->bool where con=v and
			v:TYPE=v_actual.labs in ctxt.  In the polymorphic case, con_eqfun
			has the form con'*con'->bool where con'=v(varp.lab1,...,varp.labn)
			and v:(n->TYPE)=v_actual.labs in ctxt.
		*)

		  (true, DEC_EXP (dec_spec as (v_spec,con_eqfun_spec,NONE,false)), candidate) =>
			(case (coerce_monoval(ctxt,dec_spec,path_actual,candidate)) of
			    (r as SOME _) => r
			|   NONE =>
				let val con = IlUtil.eqfun_con(ctxt,con_eqfun_spec)
				in  (case (eq_compile(ctxt,con)) of
					SOME (exp,_) =>
					    let val bnd = BND_EXP(v_spec,exp)
					    in	SOME(bnd,spec_dec)
					    end
				    |	NONE => eqtype_error lab)
				end)

		| (true, DEC_MOD (v_spec, true,
			SIGNAT_FUNCTOR(varp,sigp,
				innersig as SIGNAT_STRUCTURE[SDEC(it, DEC_EXP(itv,con_eqfun,NONE,false))],
				TOTAL)), _) =>
			let val ctxt = add_context_mod'(ctxt,varp,sigp)
			    val con' = IlUtil.eqfun_con(ctxt,con_eqfun)
			in
			    (case (eq_compile(ctxt,con')) of
				SOME (exp,_) =>
				    let val bnd = BND_MOD(v_spec,true,MOD_FUNCTOR(TOTAL, varp, sigp,
						MOD_STRUCTURE[SBND(it,BND_EXP(itv,exp))], innersig))
				    in  SOME(bnd,spec_dec)
				    end
			     |	NONE => eqtype_error lab)
			end

		(* --------------- Coercion from monoval to monoval ----------------------- *)
		| (_, DEC_EXP dec_spec, actual as SOME(_,PHRASE_CLASS_EXP pc_actual)) =>
		    (case (coerce_monoval(ctxt,dec_spec,path_actual,actual)) of
			(r as SOME _) => r
		     |	NONE =>
			let val (_,con_spec,_,_) = dec_spec
			    val (_,con_actual,_,_) = pc_actual
			    val con_spec_norm = con_normalize(ctxt,con_spec)
			    val con_actual_norm = con_normalize(ctxt,con_actual)
			in  error_region_with "coercion of a monomorphic value component\n";
			    tab_region_with "to a monomorphic value specification failed\n";
			    print "Component name: ";   pp_label lab;
			    print "\nExpected type:\n"; pp_con con_spec;
			    print "\nFound type:\n";    pp_con con_actual;
			    print "\nExpanded expected type:\n"; pp_con con_spec_norm;
			    print "\nExpanded found type:\n"; pp_con con_actual_norm;
			    print "\n";
			    NONE
			end)

		(* --------------- Coercion from module/polyval to monoval ----------------------- *)
		(* XXX not checking eopt XXX *)
		| (_, DEC_EXP(v_spec,con_spec,eopt,_), SOME(lbls,PHRASE_CLASS_MOD (_,_,signat,_))) =>
		      (case (is_polyval_sig signat) of
			   NONE =>
			       (case (is_exception_sig signat) of
				   NONE =>
				       (error_region_with "polymorphic value specification but module component";
					pp_signat signat;
					NONE)
			         (* --- Coercion from exception constructor to value spec --- *)
				 | SOME con_actual =>
				       (coerce(true,"exception constructor to value spec");
				       if (sub_con(ctxt,con_actual,con_spec)) then
					   let val exp = path2exp(join_path_labels(path_actual, lbls @ [mk_lab]))
					       val bnd = BND_EXP(v_spec,exp)
					       val dec = DEC_EXP(v_spec,con_spec,NONE,false)
					   in SOME(bnd,dec)
					   end
				       else
					   let val con_spec_norm = con_normalize(ctxt,con_spec)
					       val con_actual_norm = con_normalize(ctxt,con_actual)
					   in  error_region_with "coercion of an exception constructor\n";
					       tab_region_with "to a monomorphic value specification failed\n";
					       print "Component name: ";   pp_label lab;
					       print "\nExpected type:\n"; pp_con con_spec;
					       print "\nFound type:\n";    pp_con con_actual;
					       print "\nExpanded expected type:\n"; pp_con con_spec_norm;
					       print "\nExpanded found type:\n"; pp_con con_actual_norm;
					       print "\n";
					       NONE
					   end
				       ))
			 | SOME (var_actual,
				 SIGNAT_STRUCTURE sdecs_actual,
				 con_actual,eopt,inline) =>
			       let val path = join_path_labels(path_actual,lbls)
			       in  polyval_case (ctxt,coerce)
				         {name = v_spec, path = path,
					  varsig_spec = NONE, con_spec = con_spec, eopt = eopt,
					  inline = inline, var_actual = var_actual,
					  sdecs_actual = sdecs_actual, con_actual = con_actual}
			       end)

		(* --------- Coercion from con, overexp, or none to monoval --------------- *)
		| (_, DEC_EXP _, SOME(_,PHRASE_CLASS_CON _)) =>
			     (error_region_with "value specification but type component: ";
			      pp_label lab; print "\n"; NONE)

		| (_, DEC_EXP _, SOME(_,PHRASE_CLASS_SIG _)) =>
			     (error_region_with "value specification but signature component: ";
			      pp_label lab; print "\n"; NONE)

		| (_, DEC_EXP _, SOME(_,PHRASE_CLASS_EXT _)) =>
			     error "Sdecs_Lookup returned extern"

		| (_, DEC_EXP _, NONE) =>
			     (error_region_with "value specification but missing component: ";
			      pp_label lab; print "\n"; NONE)

		(* ----- Coercion from module/polyval to module/polyval -------------------------- *)
		| (_, DEC_MOD (v_spec,b,sig_spec), SOME(lbls, PHRASE_CLASS_MOD (_,_,sig_actual,_))) =>
	            (case (is_polyval_sig sig_spec, is_polyval_sig sig_actual) of
			 (* ----------- Coercion from polyval to polyval ------------------ *)
			 (SOME (v1,s1,con_spec,_,_),
			  SOME (v2,SIGNAT_STRUCTURE sdecs2,con_actual,eopt,inline)) =>
			     polyval_case (ctxt,coerce)
				 {name = v_spec, path = join_path_labels(path_actual,lbls),
				  varsig_spec = SOME(v1,s1), con_spec = con_spec, eopt = eopt,
				  inline = inline, var_actual = v2,
				  sdecs_actual = sdecs2, con_actual = con_actual}
			(* ----------- Coercion from module to module ------------------ *)
			| (NONE, NONE) =>
			 let val mod_path = join_path_labels(path_actual,lbls)
			     val (coerced,mbody,sig_ret) =
				 xcoerce(ctxt,mod_path,sig_actual,sig_spec)
			     val _ = coerce(coerced,"inner module")
			     val bnd = BND_MOD(v_spec,b,mbody)
			     val dec = DEC_MOD(v_spec,b,sig_ret)
			 in SOME(bnd,dec)
			 end
			(* ----------- Coercion from module to polyval or polyval to module --------- *)
			| (NONE, SOME _) =>
			 (error_region_with "module specification but polymorphic value component: ";
			  pp_label lab; print "\n"; NONE)
			| (SOME _, _) =>
			 (error_region_with "polymorphic value specification but module component: ";
			  pp_label lab; print "\n"; NONE))

		(* ------- coercion of a non-existent or non-module component to a module spec ---- *)
		| (_, DEC_MOD _, _) =>
			(error_region_with "coercion of a non-module or non-existent component\n";
			 tab_region_with   "  to a module specification failed at ";
			 pp_label lab; print "\n";
			 NONE)

		(* ------- coercion of a type component to a type spec ---- *)
		| (_, DEC_CON (v_spec, k_spec, copt_spec, _),
	          SOME(lbls,PHRASE_CLASS_CON (con_actual,k_actual,con_opt,inline))) =>
			let val bnd = BND_CON(v_spec,con_actual)
			    val dec = DEC_CON(v_spec,k_actual,SOME con_actual,inline)
			    val res = SOME(bnd,dec)
			in  if eq_kind(k_spec,k_actual)
			      then
			        (case copt_spec of
			       	   NONE => res
				 | SOME con_spec =>
				     if sub_con(ctxt,con_actual,con_spec)
					 then res
				     else
					 let val con_actual' = con_normalize(ctxt,con_actual)
					     val con_spec' = con_normalize(ctxt,con_spec)
					 in  error_region_with "coercion of a type component to a\n";
					     tab_region_with "type specification failed at ";
					     pp_label lab;
					     print "\nExpected type: ";  pp_con con_spec;
					     print "\nActual type: ";    pp_con con_actual;
					     print "\nExpanded expected type: ";  pp_con con_spec';
					     print "\nExpanded actual type: ";    pp_con con_actual';
					     print "\n";
					     NONE
					 end)
			    else (error_region_with "coercion of a type component to a ";
				  tab_region_with "type specification failed at ";
				  pp_label lab;
				  print "\nExpected kind: ";  pp_kind k_spec;
				  print "\nActual kind: ";    pp_kind k_actual;
				  print "\n";
				  NONE)
			end

		(* ------- coercion of a non-type component to a type spec ---- *)
		| (_, DEC_CON (v_spec, k_spec, SOME c_spec, inline_spec), NONE) =>
			(* Used to check is_questionable for transparent datatypes. *)
			(error_region_with "coercion of a non-existent component\n";
			 tab_region_with   "  to a type specification failed at ";
			 pp_label lab; print "\n";
			 NONE)

		| (_, DEC_CON _, _) =>
			(error_region_with "coercion of a non-type or non-existent component\n";
			 tab_region_with   "  to a type specification failed at ";
			 pp_label lab; print "\n";
			 NONE))

	fun loop _ [] = ([],[])
	  | loop ctxt ((SDEC(l,dec))::rest) =
	    (if (!debug)
		 then (print "xcoerce_help' working on SDEC with label = ";
		       pp_label l; print "\n")
	     else ();
	     case (doit ctxt (l,dec)) of
		 SOME (resbnd,resdec) =>
		     let val ctxt' = add_context_dec(ctxt,resdec)
			 val (sbnds,sdecs) = loop ctxt' rest
		     in ((SBND(l,resbnd))::sbnds,
			 (SDEC(l,resdec))::sdecs)
		     end
	       | NONE =>
		     let (* So subsequent target decs are well-formed. *)
			 val ctxt' = add_context_dec(ctxt,dec)
		     in  loop ctxt' rest
		     end)

        val (sbnds_coerced, sdecs_coerced) = loop ctxt sdecs_target

        val _ = if (!xcoerce_error) then reject "signature matching failed" else ()

	val res = if !coerced
		      then (true, MOD_STRUCTURE sbnds_coerced,
			    SIGNAT_STRUCTURE sdecs_coerced)
		  else (false, path2mod path_actual,
			SIGNAT_STRUCTURE sdecs_actual)
	val _ = if (!debug)
		    then let val (_,m,s) = res
			 in  print "\n\n-------xcoerce_structure------\n";
			     print "Returning mod = \n"; pp_mod m; print "\n";
			     print "Returning sig = \n"; pp_signat s; print "\n\n"
			 end
		else ()
      in  res
      end


    (* ---------- The exported signature coercion routines ------------ *)

    fun xcoerce_seal (context : context,
		      mod_actual : mod,
		      sig_actual : signat,
		      sig_target : signat) : mod =
	    let val var_actual = fresh_named_var "origSeal"
		val path_actual = PATH(var_actual,[])
		val context' = add_context_mod'(context,var_actual,sig_actual)
		val (coerced,mod_coerced,_) =
		    xcoerce(context', path_actual, sig_actual, sig_target)
	    in  if coerced
		then MOD_LET(var_actual, mod_actual,
			     MOD_SEAL(mod_coerced, sig_target))
		else mod_actual
	    end

    fun xcoerce_functor (context : context,
			 path_actual : path,
			 sig_actual : signat,
			 sig_target : signat) : mod =
	    let val (coerced,mod_coerced,_) =
		    xcoerce(context, path_actual, sig_actual, sig_target)
	    in  if coerced
		then mod_coerced
		else path2mod path_actual
	    end

    fun sig_describe_size s =
	let fun describe d m s = let val sz = sig_size s
				 in  if (sz > 2)
				     then (print (Int.toString d); print ": ";
					   pp_mod m; print " has size "; print (Int.toString sz);
					   print "\n")
				     else ()
				 end
	    fun help d m (SDEC(l,DEC_MOD(_,_,s))) = loop d (MOD_PROJECT (m,l)) s
	      | help d _ _ = ()
	    and loop d m (s as (SIGNAT_STRUCTURE sdecs)) =
		(describe d m s;
		 if (d<4) then app (help (d+1) m) sdecs else ())
	      | loop d m s = describe d m s
	in  loop 0 (MOD_VAR (fresh_var())) s
	end

    val track_coerce = Stats.ff("IlstaticTrackCoerce")

    fun generateModFromSig (ctxt, path : path, signat : signat, typeOnly : bool) =
	let val signat = deep_reduce_signat ctxt signat
	    fun generateSdec m (sdec as (SDEC(l,dec))) =
		(case dec of
		     DEC_MOD(v,b,s) =>
			 (case generateSig (MOD_PROJECT(m, l)) s of
			      NONE => NONE
			    | SOME (m,s) => SOME(SBND(l,BND_MOD(v,b,m)),
						 SDEC(l,DEC_MOD(v,b,s))))
		   | DEC_EXP (v,c,eopt,b) =>
			      if not typeOnly orelse is_coercion l orelse is_eq l then
				  SOME(SBND(l, BND_EXP(v,MODULE_PROJECT(m,l))),
				       SDEC(l, DEC_EXP(v,c,eopt,b)))
			      else NONE
		   | DEC_CON (v,_,_,_) => SOME (SBND(l, BND_CON(v,CON_MODULE_PROJECT(m,l))), sdec))
	    and generateSig m s =
		(case s of
		     SIGNAT_STRUCTURE sdecs =>
			 let val (sbnds,sdecs) = Listops.unzip(List.mapPartial (generateSdec m) sdecs)
			 in  SOME(MOD_STRUCTURE sbnds, SIGNAT_STRUCTURE sdecs)
			 end
		   | SIGNAT_FUNCTOR _ => if typeOnly then NONE  (* these can be polymorphic values *)
					 else SOME(m, s)
		   | _ => error "deep_reduce_signat failed")
	in  generateSig (path2mod path) signat
	end

    fun structureGC(ctxt : context,
		    var_actual : var,
 		    mod_client : mod,
 		    sig_client : signat,
		    used : Name.PathSet.set) : mod * signat =
	let
	(*
	    val _ = (print "XXXstructureGCXXX\n";
		     print "  var_actual = "; pp_var var_actual; print "\n";
		     print "  mod_client = "; pp_mod mod_client; print "\n";
		     print "  sig_client = "; pp_signat sig_client; print "\n";
		     print "  initial used = "; Name.PathSet.app (fn p => (pp_path (PATH p); print "\n")) used;
		     print "\n")
	*)
	    val ctxt = add_context_mod'(ctxt, var_actual, sig_client)
	    fun getChildren path =
		Name.PathSet.filter (fn (v,_) => eq_var(v,var_actual))
		(case Context_Lookup_Path(ctxt, PATH path) of
		     SOME (_,PHRASE_CLASS_MOD(_,_,s,_)) => findPathsInSig s
		   | SOME (_,PHRASE_CLASS_EXP(_,c,_,_)) => findPathsInCon c
		   | SOME (_,PHRASE_CLASS_CON(_,_,copt,_)) =>
			let val conpaths = (case copt
					      of SOME c => findPathsInCon c
					       | NONE => Name.PathSet.empty)
			    fun to_eq (L : label list) : label list =
				(case L
				   of nil => nil
				    | l :: nil => [Name.to_eq l]
				    | l :: ls => l :: (to_eq ls))
			    val (v,labs) = path
			    val eqpath = (v,to_eq labs)
			in  if isSome (Context_Lookup_Path(ctxt, PATH eqpath))
			    then Name.PathSet.add (conpaths,eqpath)
			    else conpaths
			end
		   | SOME (_,PHRASE_CLASS_EXT (_,_,c)) => findPathsInCon c
		   | SOME _ => (print "unexpected result from  path "; pp_path (PATH path); print "\n";
			   error "no such path")
		   | _ => (print "no such path "; pp_path (PATH path); print "\n";
			   error "no such path"))

	    fun reachable black gray =
		(case Name.PathSet.find (fn _ => true) gray of
		     NONE => black
		   | SOME first =>
			 let fun folder (p, gray) =
				 if (Name.PathSet.member(gray,p) orelse
				     Name.PathSet.member(black,p))
				     then gray
				 else Name.PathSet.add(gray, p)
			     val children = getChildren first
			     val gray = Name.PathSet.foldl folder gray children
			     val gray = Name.PathSet.delete(gray,first)
			     val black = Name.PathSet.add(black,first)
			 in  reachable black gray
			 end)
	    val used = reachable Name.PathSet.empty used
	    (* Verify all paths start with var_actual and maintain necessary structures by adding all prefixes *)
	    fun folder ((v,labs),set) =
		let fun loop ([],set) = set
		      | loop (revLabs,set) =
		        let val labs = rev revLabs
			in  if (Name.PathSet.member(set,(var_actual,labs)))
				then set
			    else loop(tl revLabs, Name.PathSet.add(set,(var_actual,labs)))
			end
		in  if eq_var(v,var_actual)
			then () else error "bad path found";
		    loop (rev labs, set)
		end
	    val used = Name.PathSet.foldl folder Name.PathSet.empty used
	(*
	    val _ = (print "  final used = ";
		     Name.PathSet.app (fn p => (pp_path (PATH p); print "\n")) used;
		     print "\n")
	*)
	    fun filterModSig currentPrefix (m,s) =
		let val MOD_STRUCTURE sbnds = m
		    val SIGNAT_STRUCTURE sdecs = s
		    val (sbnds,sdecs) = Listops.unzip(List.mapPartial (filterSbndSdec currentPrefix)
						   (Listops.zip sbnds sdecs))
		in  (MOD_STRUCTURE sbnds, SIGNAT_STRUCTURE sdecs)
		end
	    and filterSbndSdec currentPrefix (sbnd as SBND(lb,bnd),sdec as SDEC(ld,dec)) =
		let val _ = if (eq_label(lb,ld)) then () else error "filterSbndSdec: labels not the same"
		    val currentPrefix = currentPrefix @ [lb]
		in  if Name.PathSet.member(used,(var_actual,currentPrefix))
			then (case (bnd,dec) of
				  (BND_MOD(vb,bb,m), DEC_MOD(vd,bd,s)) =>
				      let val(m,s) = filterModSig currentPrefix (m,s)
				      in  SOME(SBND(lb,BND_MOD(vb,bb,m)),SDEC(ld,DEC_MOD(vd,bd,s)))
				      end
				| _ => SOME(sbnd,sdec))
		    else NONE
		end

	    val (module, signat) = filterModSig [] (mod_client, sig_client)
	(*
	    val _ = (print "XXXXstructureGCXXXX\n";
		     print "  module = "; pp_mod module; print "\n";
		     print "  signat = "; pp_signat signat; print "\n")
	*)
	in  (module, signat)
	end

    (* We are given a module modActual (not a module variable), its signature sigActual, and a desired signature sigTarget
       against which we wish to coerce transparently.  That is, we must add/drop components and change polymorphism
       as necessary so that exactly the right number and shape of terms, type and structures are visible.
       However, we are/must leak type information through even if the ascribing signature does not give it.

       Here is one possible implementation which turns out to have a performance problem.
       (1) Compute modCoerced and sigCoerced which has the components of sigTarget
           (with type information exposed) in terms of varActual.  Return modActual and sigActual
	   if no coercion was actually necessary.
       (2) Assuming sigTarget has type labels taus and term labels exps with corresponding types, return

           {labelHidden :> varCoerced =
	       MOD_LET varActual = modActual
	       IN      coercedMod
	       END,
	    tau1 = varCoerced.tau1
	    exp1 = varCoerced.exp1
	    ...
           }

	   {labelHidden :> varCoerced : sigCoerced,
	    tau1 = varCoerced.tau1
	    exp1 : varCoerced.tau1
	    ...
	   }

       (3) Unfortunately, the resulting signatures now has two copies of all necessary type
           AND term components.  Some duplication of type components is necessary due to
	   the transparency semantics but redundant term components are not and can well
	   almost double the size of the signature.

       Here is an alternative solution.
       (1) As before, compute modCoerced and sigCoerced which has the components of sigTarget
           (with type information exposed) in terms of varActual.  Return modActual and sigActual
	   if no coercion was actually necessary.
       (2) Compute modThin and sigThin which contains all the types, datatype
           coercions, and equality functions of modActual and sigActual.
	   Note that creating these from modCoerced and sigCoerced fails to capture all necessary type components.
       (3) Assuming sigTarget has type labels taus and term labels exps with corresponding types, return

	   MOD_LET varActual = modActual
                   varCoerced = coercedMod
	   IN      {labelHidden :> varThin = modThin,
	            tau1 = varCoerced.tau1,
		    exp1 = varCoerced.exp2,
		    ...
		    }

	   {labelHidden :> varThin : sigThin,
	    tau1 = varThin.tau1,
	    exp1 : varThin.tau1,
	    ...
	   }
       (4) Comparing against the previous solution, the returned module (code) is bigger.
           This size increase will be taken care of by NIL optimizations which eliminate the
	   redundant copies.  What is important is that the returned HIL signature is smaller.
	   In particular, sigThin contains fewer term components than sigCoerced.
    *)

    fun xcoerce_transparent (context : context,
			     mod_actual : mod,
			     sig_actual : signat,
			     sig_target : signat) : Il.mod * Il.signat =
	let
	    (* Compute modCoerced and sigCoerced *)
	    val _ = if (!track_coerce)
			then Stats.bool("IlstaticShowing") := true
		    else ()
	    val var_actual = fresh_named_var "origModule"
	    val path_actual = PATH(var_actual,[])
	    val context = add_context_mod'(context,var_actual,sig_actual)
	    val (coerced,mod_coerced,sig_coerced) = xcoerce(context,path_actual,sig_actual,sig_target)
	    val _ = if (!debug)
			then (print "coerced to mod_coerced = \n"; pp_mod mod_coerced; print "\n";
			      print "coerced to sig_coerced = \n"; pp_signat sig_coerced; print "\n")
		    else ()
	    val _ = if (!track_coerce)
			then Stats.bool("IlstaticShowing") := false
		    else ()

	in
	    if (coerced)
	      then let val hidden_lbl = internal_label "hiddenThinModule"
		       val var_coerced = fresh_named_var "coercedModule"
		       val var_thin = fresh_named_var "thinModule"
		       val SOME(mod_thick, sig_thick) = generateModFromSig(context, path_actual, sig_coerced, false)
		       val mod_sig_thin_option = generateModFromSig(context, path_actual, sig_actual, true)
		       val substActualToCoerced = subst_add_modvar(empty_subst, var_actual, MOD_VAR var_coerced)
		       val substActualToThin = subst_add_modvar(empty_subst, var_actual, MOD_VAR var_thin)
		       val MOD_STRUCTURE copy_sbnds = mod_subst(mod_thick, substActualToCoerced)
		       val sig_copy = sig_subst(sig_thick, substActualToThin)
		       val SIGNAT_STRUCTURE copy_sdecs = sig_copy
		       val (hidden_sbnd, hidden_sdec) : sbnd list * sdec list =
			   (case mod_sig_thin_option of
				NONE => ([],[])
			      | SOME (mod_thin, sig_thin) =>
				    let
					val sig_thin = sig_subst(sig_thin, substActualToThin)
					val paths = findPathsInSig sig_copy
					val used = Name.PathSet.filter (fn (v,_) => eq_var(v,var_thin)) paths
					val (mod_thin, sig_thin) = structureGC(context, var_thin, mod_thin, sig_thin, used)
				    in  (case sig_thin
					   of SIGNAT_STRUCTURE [] => ([],[])
					    | _ =>
						([SBND(hidden_lbl, BND_MOD(var_thin, false, mod_thin))],
						 [SDEC(hidden_lbl, DEC_MOD(var_thin, false, sig_thin))]))
				    end)
		       val mod_result = MOD_LET(var_actual,mod_actual,
						MOD_LET(var_coerced, mod_coerced,
							MOD_STRUCTURE(hidden_sbnd @ copy_sbnds)))
		       val sig_result = SIGNAT_STRUCTURE(hidden_sdec @ copy_sdecs)
		       val _ = if (!debug)
				   then (print "mod_result = \n"; pp_mod mod_result; print "\n";
					 print "sig_result = \n"; pp_signat sig_result; print "\n")
			       else ()
		       val _ = case (!diag, hidden_sdec) of
			   (true, [SDEC(_,DEC_MOD(_, _, sig_thin))]) =>
			       (print "XXX hidden component\n";
				sig_describe_size sig_thin; print "\n";
				print "XXX remaining component\n";
				sig_describe_size (SIGNAT_STRUCTURE copy_sdecs); print "\n")
			 | _ => ()
		   in  (mod_result, sig_result)
		   end
	    else  (mod_actual, sig_actual)
	end

    fun subtimer(str,f) = f  (* use Stats.subtimer for real timing *)
    val xsig_where_type = subtimer("Elab-Signature",xsig_where_type)
    val xsig_where_structure = subtimer("Elab-Signature",xsig_where_structure)
    val xsig_sharing_types = subtimer("Elab-Signature",xsig_sharing_types)
    val xsig_sharing_structures = subtimer("Elab-Signature",xsig_sharing_structures)

    val xcoerce_seal = subtimer("Elab-Signature",xcoerce_seal)
    val xcoerce_transparent = subtimer("Elab-Signature",xcoerce_transparent)
    val xcoerce_functor = subtimer("Elab-Signature",xcoerce_functor)

end
