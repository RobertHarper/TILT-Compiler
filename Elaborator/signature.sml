(*$import Il IlStatic IlUtil Ppil IlContext Error SIGNATURE Bool Stats Option *)

(* Need to improve where_structure to use SIGNAT_OF *)

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
    fun msg s = if !debug then print s else ()

    fun debugdo t = if (!debug) then (t(); ()) else ()
    type labels = label list

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

    fun is_polyval_sig (SIGNAT_FUNCTOR(var_poly,sig_poly,SIGNAT_STRUCTURE 
				       (NONE, [SDEC(maybe_it,DEC_EXP(_,c,eopt,i))]),_)) = 
	if (eq_label (maybe_it, it_lab))
	    then SOME(var_poly,sig_poly,c,eopt,i)
	else NONE
      | is_polyval_sig _ = NONE

    fun make_polyval_sig (var_poly,sig_poly,v,c,eopt,i) = 
	SIGNAT_FUNCTOR(var_poly,sig_poly,SIGNAT_STRUCTURE 
		       (NONE, [SDEC(it_lab,DEC_EXP(v,c,eopt,i))]),PARTIAL)


    (* ---------------- helpers *)

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

  (* this function is staged to reduce repeated selfification *)
  fun follow_labels (pathopt,sdecs,ctxt) =
      let val signat = SIGNAT_STRUCTURE(NONE,sdecs)
	  val (v,path,ctxt) = 
	      (case pathopt of
		   NONE => let val v = fresh_named_var "modtemp"
			       val path = PATH(v,[])
			       val signat = SelfifySig ctxt (path, signat)
			       val ctxt = add_context_mod'(ctxt,v,signat)
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
	      SIGNAT_STRUCTURE(_,sdecs) => SOME sdecs
	    | s => NONE)

     fun find_labels_sdecs context sdecs = 
	 let fun driver path sdecs = rev(foldl (find_labels path) [] sdecs)
	     (* the path being carried is backwards so we must reverse when we add to accumulator *)
	     and find_labels path (SDEC(l,DEC_CON (_,k,NONE,_)),_) = 
		 error "find_labels should not encounter any abstract types"
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
	    local val (_,fv,_) = con_free con
	    in    fun bound v = 
	            if (member_eq(eq_var,v,fv)) 
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
				     SIGNAT_STRUCTURE(_, sdecs) => 
					 (SDEC(l,DEC_MOD(v,b,SIGNAT_STRUCTURE
							 (NONE, domod restl sdecs))))::rest
				   | s => (error_region();
					   print "signature not reduced to structure signature";
					   pp_signat s;
					   sdec::(loop rest)))
		       else sdec::(loop rest))
	      in loop sdecs
	      end		
	in  domod labs orig_sdecs
	end


      (* labels is a list of typeslot paths (relative to the sdecs) to type components;
	 we search for the one that occurs first and then transparently 
	 type-abbreviate all of the rest to the first one if it is abstract or equivalent *)

      fun xsig_sharing_rewrite_structure (context,sdecs,lpath_list,sigopt) = 
	  (* using a ref like this is error-prone; should change this *)
	  let val target = ref(sigopt, NONE)
	      fun getsig(cur_path,orig_sig) = 
		  (case !target of
		       (SOME s,_) => s
		     | (NONE, SOME vlpath) => 
			   let fun loop [] current = SIGNAT_OF(vlpath2path current)
				 | loop ((v1,l1)::rest1) (all2 as ((v2,l2)::rest2)) = 
			            if (eq_var(v1,v2))
					then loop rest1 rest2
				    else SIGNAT_OF(vlpath2path all2)
				 | loop _ [] = error "empty target path"
			   in  loop cur_path vlpath
			   end
		     | (NONE, NONE) => (target := (NONE, SOME cur_path); orig_sig))
	      fun traverse ctxt _ [] = [] 
                | traverse ctxt cur_path ((SDEC(l,DEC_MOD(v,b,s)))::rest) =
		  let val cur_path' = cur_path @ [(v,l)]
		      val cur_lpath' = vlpath2lpath cur_path'
		      val matches = List.filter (eq_lpath cur_lpath') lpath_list
		      val prematches = List.filter (sub_lpath cur_lpath') lpath_list
		      val match_count = length matches
		      val prematch_count = length prematches
		      val s = 
			  if (match_count > 0)
			      then ((* print "match called getsig with\n";
				    pp_signat s;
				    print "\n"; *)
				    getsig(cur_path',s))
			  else if (prematch_count > 0)
			      then (case reduce_signat ctxt s of
				   SIGNAT_STRUCTURE(popt,sdecs) =>
				       let  (* check the ref before traversing! *)
					   val first = prematch_count = 1
					       andalso (not (Option.isSome (#1 (!target))))
					       andalso (not (Option.isSome (#2 (!target))))
					   val sdecs' = traverse ctxt cur_path' sdecs
				       in  if first
					   then s else SIGNAT_STRUCTURE(popt,sdecs')
				       end
				 | _ => error "prematched sig not reducing to sig_struct")
			      else s
			val ctxt = add_context_mod'(ctxt,v,SelfifySig ctxt(PATH (v,[]),s))
		  in (SDEC(l,DEC_MOD(v,b,s))) :: (traverse ctxt cur_path rest)
		  end
                | traverse ctxt cur_path (sdec::rest) = sdec :: (traverse ctxt cur_path rest)
	  in traverse context [] sdecs
	  end


      fun xsig_sharing_rewrite (ctxt,sdecs) = 
        let val v = fresh_named_var "modtemp"
	    val s = SIGNAT_STRUCTURE(NONE, sdecs)
	    val ctxt = add_context_mod'(ctxt,v,SelfifySig ctxt (PATH (v,[]), s))
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
	      fun find_representative (curpath : lpath) : vlpath option ref =
		  let fun loop [] = error "find_representative failed"
			| loop ((rep,tslots : lpath list)::rest) = 
		      if (Listops.member_eq(eq_lpath',curpath,tslots))
			  then rep else loop rest
		  in  loop table
		  end
              fun transparent (curpath : (var * label) list, copt) =
		  let val rep = (find_representative (map #2 curpath))
		  in  (case !rep of
			   SOME rep_path => SOME(combine_path rep_path curpath)
			 | NONE => (rep := SOME curpath; copt))
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
				       SIGNAT_STRUCTURE(popt,sdecs) =>
					   let val is_first = 
					       (case this_typeslots of
						    [this] => (case !(find_representative this) of
								   NONE => true
								 | SOME _ => false)
						  | _ => false)
					       val sdecs' = traverse(cur_path',this_typeslots) sdecs
					   in  DEC_MOD(v,b,if is_first then s
							   else SIGNAT_STRUCTURE(popt,sdecs'))
					   end
				     | _ => dec)
			     | _ => dec)
		  in (SDEC(l,dec')) :: (traverse (cur_path,typeslots) rest)
		  end
	    in traverse ([],Listops.flatten typeslots) sdecs
	    end (* fn *)
        end (* xsig_sharing_rewrite *)

    fun xsig_where_type(ctxt,orig_sdecs,lpath, con, kind) : sdecs =
	let val mjunk = fresh_named_var "xsig_where_type_mjunk"
	    val mpath = PATH(mjunk,[])
	    val msig = SelfifySig ctxt (mpath,SIGNAT_STRUCTURE(NONE,orig_sdecs))
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
	       | CONCRETE (lbls,_,c) => let val _ = print "where_type concrete case\n"
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
    and xsig_where_structure_slow(context,sdecs,labs1,m2,sig2) = 
	let 
	    val SOME sdecs2 = signat2sdecs context sig2
	    val mjunk_var = fresh_named_var "mjunk_where_structure_slow"
	    val mjunk = MOD_VAR mjunk_var
	    val context = add_context_mod'(context,mjunk_var,
					   SelfifySig context (PATH (mjunk_var,[]),
							       SIGNAT_STRUCTURE(NONE,sdecs)))
	    val (labels1,s1) = 
		(case Context_Lookup_Path_Open(context,PATH(mjunk_var,labs1)) of
		     SOME(PATH(_,labels1 as _::_),PHRASE_CLASS_MOD(_,_,s1)) => (labels1,s1)
		   | _ => (error_region();
			   print "can't where non-existent or non-structure component\n";
			   raise WhereError))

            (* kinds and reversed partial lpaths *)
	    val SOME sdecs1 = signat2sdecs context s1
	    val kind_plabels = find_labels_sdecs context sdecs1

            (* as we constrain, some ABSTRACT paths become CONCRETE so
	      we cannot follow_labels all at once at the beginning *)
	    fun constrain((k,plabs),sdecs) = 
		let val labs = labels1 @ plabs
		in  (case follow_labels (NONE,sdecs,context) (labels1 @ plabs) of
		       ABSTRACT (labs,_) => 
			 (case (Sdecs_Lookup_Open context (m2,sdecs2,plabs)) of
			      SOME(_,PHRASE_CLASS_CON(c,_,_,_)) => 
				  xsig_where_type(context,sdecs,labs,c,k)
			    | _ => (error_region();
				    print "where's rhs structure missing components\n";
				    sdecs))
		     | CONCRETE _ => sdecs)
		end

	in  foldl constrain sdecs kind_plabels
	end
    handle WhereError => sdecs
	 | e => (print "\nsig2 = "; pp_signat sig2;
		 print "\nsdecs= "; pp_sdecs sdecs;
		 print "\n";
		 raise e)

     (* it is difficult to correctly optimize this case: we try to catch only some cases *)
    and xsig_where_structure(context,sdecs,labs1,m2,sig2) = 
	let fun find_sig [] s = SOME s
	      | find_sig (l::ls) (SIGNAT_STRUCTURE(_,sdecs)) =
		(case find_sdec (sdecs,l) of
		     SOME (SDEC(_,DEC_MOD(_,_,signat))) => find_sig ls signat
		   | _ => NONE)
	      | find_sig labs (s as (SIGNAT_VAR v)) = find_sig labs (reduce_signat context s)
	      | find_sig _ _ = NONE
	in  (case find_sig labs1 (SIGNAT_STRUCTURE(NONE,sdecs)) of
		 SOME (sig1 as SIGNAT_VAR _) =>
		     (case (IlStatic.Sig_IsSub(context,sig2,sig1), mod2path m2) of
			  (true, SOME p2) =>
			      xsig_sharing_rewrite_structure(context,sdecs,[labs1],SOME(SIGNAT_OF p2))
			| _ => xsig_where_structure_slow(context,sdecs,labs1,m2,sig2))
	       | _ => xsig_where_structure_slow(context,sdecs,labs1,m2,sig2))
	end
    handle WhereError => sdecs
	 | e => (print "\nsig2 = "; pp_signat sig2;
		 print "\nsdecs= "; pp_sdecs sdecs;
		 print "\n";
		 raise e)


  (* ------ These structure components are being shared ----------- *)
  fun path2triple (p,s,mjunk,ctxt') = 
      let val SIGNAT_STRUCTURE(_,sdecs') = s
	  val mpath = PATH (mjunk,[])
      in  (case (Sdecs_Lookup_Open ctxt' (MOD_VAR mjunk,sdecs',p)) of
	       SOME(lpath,PHRASE_CLASS_MOD(_,_,s)) => 
		   let 
		       val vpath = join_path_labels(mpath,lpath)
		       val sdecs = 
			   (case reduce_signat ctxt' s of
				SIGNAT_STRUCTURE(_,sdecs) => sdecs
			      | _ => error "sharing got bad structure component\n")
		   in  (vpath,lpath,sdecs)
		   end
	     | _ => (error_region();
			     print "structure sharing given a non-structure component: ";
			     pp_lpath p; print "\n";
			     raise SharingError))
      end
  
  and xsig_sharing_structure_slow(ctxt,sdecs,lpath1, lpath2: lpath) : sdecs = 
      let
	  val mjunk = fresh_named_var "mjunk_sharing_structure"
	  val mpath = PATH (mjunk,[])
	  val s = SelfifySig ctxt (mpath, SIGNAT_STRUCTURE(NONE, sdecs))
	  val ctxt' = add_context_mod'(ctxt,mjunk,s)

	  val (path1,lpath1,sdecs1) = path2triple (lpath1, s, mjunk, ctxt')
	  val (path2,lpath2,sdecs2) = path2triple (lpath2, s, mjunk, ctxt')

	  val slabs1 = map #2 (find_labels_sdecs ctxt' sdecs1)
	  val slabs2 = map #2 (find_labels_sdecs ctxt' sdecs2)
	  val slabs = inter_lpaths slabs1 slabs2
	  val slots1 = map (follow_labels(SOME path1, sdecs1, ctxt')) slabs
	  val slots2 = map (follow_labels(SOME path2, sdecs2, ctxt')) slabs
	  val (slabs_abs1, slabs_conc1) = splitAbstractConcrete slots1
	  val (slabs_abs2, slabs_conc2) = splitAbstractConcrete slots2
	  val slabs_abs_both = Listops.transpose [slabs_abs1, slabs_abs2]
	  val sdecsAbstractEqual = xsig_sharing_rewrite(ctxt,sdecs) (slabs_abs_both,sdecs)
	  val sigAbstractEqual= SelfifySig ctxt(mpath,SIGNAT_STRUCTURE(NONE,sdecsAbstractEqual))
	  val ctxt'' = add_context_mod'(ctxt,mjunk,sigAbstractEqual)

	  fun eq_concrete (labs,labs') = 
	      let val c = path2con(join_path_labels(path1,labs))
		  val c' = path2con(join_path_labels(path2,labs'))
		  val res = (Name.is_label_internal(List.last labs) andalso 
			     Name.is_label_internal(List.last labs))
		      orelse eq_con(ctxt'',c,c')
		  val _ = if res then () 
			  else 
			      let val reduced_c = con_normalize(ctxt'',c)
				  val reduced_c' = con_normalize(ctxt'',c')
			      in  (print "both concrete but type not equal\n";
				   print "c = "; pp_con c; print "\n";
				   print "c' = "; pp_con c'; print "\n";
				   print "reduced c = "; pp_con reduced_c; print "\n";
				   print "reduced c' = "; pp_con reduced_c'; print "\n";
				   print "\n\n")
			      end
	      in  res
	      end
      in  if (eq_list(eq_concrete,slabs_conc1,slabs_conc2))
	      then sdecsAbstractEqual
	  else sdecs
      end

  and xsig_sharing_structure_fast(ctxt,sdecs,lpath1, lpath2: lpath) : sdecs = 
      let
	  val mjunk = fresh_named_var "mjunk_sharing_structure"
	  val mpath = PATH (mjunk,[])
	  val s = SelfifySig ctxt (mpath, SIGNAT_STRUCTURE(NONE, sdecs))
	  val ctxt' = add_context_mod'(ctxt,mjunk,s)

	  val (path1,lpath1,sdecs1) = path2triple (lpath1, s, mjunk, ctxt')
	  val (path2,lpath2,sdecs2) = path2triple (lpath2, s, mjunk, ctxt')

	  val slabs1 = map #2 (find_labels_sdecs ctxt' sdecs1)
	  val slabs2 = map #2 (find_labels_sdecs ctxt' sdecs2)
	  val slots1 = map (follow_labels(SOME path1, sdecs1, ctxt')) slabs1
	  val slots2 = map (follow_labels(SOME path2, sdecs2, ctxt')) slabs2
	  val (slabs_abs1, slabs_conc1) = splitAbstractConcrete slots1
	  val (slabs_abs2, slabs_conc2) = splitAbstractConcrete slots2

      in  if ((eq_list(eq_lpath',slabs_abs1,slabs_abs2)) andalso
	      let val slabs_abs_both = Listops.transpose [slabs_abs1, slabs_abs2]
		  val sdecsAbstractEqual = xsig_sharing_rewrite(ctxt,sdecs) (slabs_abs_both,sdecs)
		  val sigAbstractEqual= SelfifySig ctxt(mpath,SIGNAT_STRUCTURE(NONE,sdecsAbstractEqual))
		  val ctxt'' = add_context_mod'(ctxt,mjunk,sigAbstractEqual)
		  val s1 = GetModSig(ctxt'',path2mod path1)
		  val s2 = GetModSig(ctxt'',path2mod path2)
	      in  Sig_IsEqual(ctxt'',s1,s2)
	      end)
	      then 
		  let val _ = print "STRUCTURE_SHARING_FAST succeeded\n"
		      val sdecs = xsig_sharing_rewrite_structure(ctxt,sdecs,[lpath1,lpath2], NONE)
		  in  sdecs
		  end
	  else 
	      (print "xsig_sharing_structure_fast calling slow version\n";
	       xsig_sharing_structure_slow(ctxt, sdecs, lpath1, lpath2))

      end

  and xsig_sharing_structures(ctxt,sdecs,[])  = sdecs
    | xsig_sharing_structures(ctxt,sdecs,[_]) = sdecs
    | xsig_sharing_structures(ctxt,sdecs,p1::p2::rest) : sdecs = 
      let val sdecs = xsig_sharing_structure_fast(ctxt,sdecs,p1,p2)
      in  xsig_sharing_structures(ctxt,sdecs,p2::rest) 
      end

  (* ------ These type components are being shared ----------------- *)
  fun xsig_sharing_type(ctxt,sdecs,path1,path2) : sdecs = 
      let val mjunk = MOD_VAR(fresh_named_var "mjunk_sharing_type")
	  fun path2label p = 
	      (case (Sdecs_Lookup_Open ctxt (mjunk,sdecs,p)) of
		   SOME(labs,_) => follow_labels (NONE,sdecs,ctxt) labs
		 | NONE => (error_region();
			    print "sharing type given non-existent path ";
			    pp_lpath p; print " with the following components\n";
			    pp_sdecs sdecs; print "\n";
			    raise SharingError))
	  val typeslot1 = path2label path1
	  val typeslot2 = path2label path2
      in  (case (typeslot1,typeslot2) of
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
    type polyinst = context * sdec list -> sbnds * sdecs * con list

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
	    val sig_temp = SIGNAT_STRUCTURE(NONE, sdecs_change)
	    val SIGNAT_STRUCTURE(_,sdecs_result) = sig_subst(sig_temp,subst)
	in  sdecs_result
	end

       (* ---- coercion of a poly component to a mono/poly specification --- *)
       fun polyval_case (ctxt : context, subst : subst, polyinst)
	   {name : var, (* use this name for the final binding *)
	    path : path, (* this path is to the actual polymorphic component *)
	    varsig_spec : (var * signat) option, (* spec might me monomorphic *)
	    con_spec : con, (* the type of the specification *)
	    var_actual : var,
	    inline : bool,
	    sdecs_actual : sdec list,
	    con_actual : con} (* the signature of the component *)
		  : bool * (bnd * dec * subst) option = 
	   let 
	       val subst = (case varsig_spec of
				NONE => subst
			      | SOME(v,_) => subst_add_modvar(subst, v, MOD_VAR var_actual))
	       val con_spec = con_subst(con_spec,subst)

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
						  SIGNAT_STRUCTURE(NONE, sdecs_actual),
						  name,con_actual,NONE,false));
		    print "\n";
		    (true,NONE))


	       val ctxt' = 
		   (case varsig_spec of
			NONE => ctxt
		      | SOME(_,sig_spec) => 
			    let val _ = if (!debug)
					    then (print "adding var_actual = "; pp_var var_actual;
						  print "\n"; pp_signat
						  (SelfifySig ctxt (PATH (var_actual,[]), 
								    sig_spec)))
					else ()
			    in add_context_mod'(ctxt,var_actual,
						SelfifySig ctxt (PATH (var_actual,[]), 
								 sig_spec))
			    end)

	       val (sbnds_poly,sdecs_poly,_) = polyinst(ctxt',sdecs_actual)

	       local
		   fun folder (SBND(l,BND_CON(v,c)),subst) =
		       subst_add_conpath(subst, PATH(var_actual, [l]), c)
		     | folder (_,subst) = subst
		   val subst = foldl folder empty_subst sbnds_poly
	       in
		   val con_actual_tyvar = con_subst(con_actual,subst)
	       end
	       val _ = if (!debug)
			   then (print "con_actual_tyvar = "; 
				 pp_con con_actual_tyvar; print "\n";
				 print "spec_con = "; pp_con con_spec; print "\n";
				 (case varsig_spec of
				      NONE => print "no varsig_spec"
				    | SOME(v,s) => (print "varsig_spec = "; pp_var v;
						    print "\n"; pp_signat s; print "\n")))
		       else ()

	   val res = 
	       if (sub_con(ctxt',con_actual_tyvar,con_spec))
		 then 
		   ((case varsig_spec of
			 NONE => 
			     let val mtemp = MOD_APP(path2mod path,MOD_STRUCTURE sbnds_poly)
			     in  (true,
				  SOME(BND_EXP(name,MODULE_PROJECT(mtemp,it_lab)),
				       DEC_EXP(name,con_actual_tyvar,NONE,inline),
				       subst))
			     end
		       | SOME (_,s1) => 
			   let val is_expand = is_eta_expand var_actual (sbnds_poly,sdecs_poly)
			       val pathmod = path2mod path
			       val mtemp = MOD_APP(pathmod, MOD_STRUCTURE sbnds_poly)
			       val inner_sig = SIGNAT_STRUCTURE(NONE,
								[SDEC(it_lab,
								      DEC_EXP(fresh_var(),
									      con_actual_tyvar,NONE,inline))])
			       val s2 = SIGNAT_FUNCTOR(var_actual,s1,inner_sig,TOTAL)
			       val m = if is_expand
					   then pathmod
				       else MOD_FUNCTOR(TOTAL,var_actual,s1,mtemp,inner_sig)
			   in  (not is_expand,
				SOME(BND_MOD(name, true, m), 
				     DEC_MOD(name, true, s2), 
				     subst))
			   end))
	       else local_error()

	   val _ = if (!debug)
		       then let val (_,SOME(bnd,dec,_)) = res
			    in  (print "polyval: result bnd = ";
				 pp_bnd bnd; print "\nresult dec = ";
				 pp_dec dec; print "\n\n")
			    end
		   else ()
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
		    sig_actual and sig_target to type-check. *)

    and xcoerce (polyinst : polyinst,
		 context : context,
		 path_actual : path,
		 sig_actual : signat,
		 sig_target : signat) : (bool * Il.mod * Il.signat) = 

	let val sig_actual = reduce_signat context sig_actual
	    val sig_target = reduce_signat context sig_target
	in  (case (sig_actual,sig_target) of
		 (SIGNAT_FUNCTOR(v1,s1,s1',a1), 
		  SIGNAT_FUNCTOR(v2,s2,s2',a2)) =>
		 let 
		     val _ = if (a1 = a2) then () 
			     else raise (FAILURE "arrow mismatch in xcoerce")
		     val p2 = PATH(v2,[])
		     val (_,m3body,_) = xcoerce(polyinst,
						add_context_mod'(context,v2,
								 SelfifySig context (p2, s2)),
						p2,s2,s1)
		     val m4_arg = MOD_APP(path2mod path_actual, m3body)
		     val m4var = fresh_named_var "var_actual_xcoerce"
		     val p4 = PATH(m4var,[])
		     val (_,m4body,_) = xcoerce(polyinst,
						add_context_mod'(context,m4var,
								       SelfifySig context (p4,s1')),
						p4,s1',s2')
		     val m4body = mod_subst(m4body,subst_add_modvar(empty_subst,m4var,m4_arg))
		     val context' = add_context_mod'(context,v2,(SelfifySig context (p2,s2)))
		     val s = GetModSig(context',m4body)
		 in (true,
		     MOD_FUNCTOR(a1,v2,s2,m4body,s),
		     SIGNAT_FUNCTOR(v2,s2,s,a1))
		 end
	   | (SIGNAT_STRUCTURE(_,sdecs_actual),
		 SIGNAT_STRUCTURE (_,sdecs_target)) =>
		 xcoerce_structure (polyinst, context,
					    path_actual,
					    sdecs_actual,
					    sdecs_target)
	       | (SIGNAT_FUNCTOR _, _) =>
		 (error_region();
		  print "cannot coerce a functor to a structure\n";
		  (true, MOD_STRUCTURE [], SIGNAT_STRUCTURE(NONE,[])))
	       | (_,SIGNAT_FUNCTOR _) => 
		 (error_region();
		  print "cannot coerce a structure to a functor\n";
		  (true, MOD_STRUCTURE [], SIGNAT_STRUCTURE(NONE,[])))
	       | _ => error "xcoerce should get reduced sig")
	end

   and xcoerce_structure (polyinst : polyinst,
			  ctxt : context,
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

	   val _ = if (Listops.eq_list (fn (SDEC(l1,_),SDEC(l2,_)) => eq_label(l1,l2),
					sdecs_actual, sdecs_target))
		       then () 
		   else 
		       coerce (true,"length/order mismatch")

	  val sdecs_target = sdecs_rename(sdecs_target, sdecs_actual)

	  local
	      (* ---- Selfifying sig_target in an attempt to avoid the substitutions does not work 
	         ---- because the shapes of sig_target and sig_actual may be very different due to open
	       *)
	      val sig_actual = SIGNAT_STRUCTURE(NONE, sdecs_actual)
	      val SIGNAT_STRUCTURE(_,sdecs_actual_self) = SelfifySig ctxt (path_actual,sig_actual)
	      val self = path2mod path_actual

	      val _ = if (!debug)
			  then (print "xcoerce_structure------";
				print "sdecs_actual_self = \n"; pp_sdecs sdecs_actual_self; print "\n";
				print "sdecs_target = \n"; pp_sdecs sdecs_target; print "\n")
		      else ()
	  in
	      fun actual_self_lookup lbl = Sdecs_Lookup_Open ctxt (self, sdecs_actual_self, [lbl])
	  end
		  

	fun doit subst (lab,spec_dec) 
	    : (bnd * dec * subst) option = 

	    (case (spec_dec, actual_self_lookup lab) of

		 (* --------------- Coercion from monoval to monoval ----------------------- *)
		 (DEC_EXP(_,con_spec,eopt,_), 
		  SOME(lbls,PHRASE_CLASS_EXP (e,con_actual,eopt',inline))) => 
		    let val con_spec = con_subst(con_spec,subst)
		    in  if (sub_con(ctxt,con_actual,con_spec) andalso
			    (case (eopt,eopt') of
				 (SOME e, SOME e') => eq_exp(ctxt,e,e')
			       | (SOME _, NONE) => false
			       | (NONE, _) => true))
			    then
				let 
				    val v = fresh_named_var ("copy_" ^ (Name.label2string lab))
				    val exp = if (inline) then e 
					      else path2exp(join_path_labels(path_actual,lbls))
				    val bnd = BND_EXP(v,exp)
				    val dec = DEC_EXP(v,con_actual,eopt',inline)
				in  SOME(bnd,dec,subst)
				end 
			else 
			    let val con_spec_norm = con_normalize(ctxt,con_spec)
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
			    end
		    end
		    
	       (* --------------- Coercion from module/polyval to monoval ----------------------- *)
	       (* XXX not checking eopt XXX *)
	       | (DEC_EXP(_,con_spec,eopt,_),
		  SOME(lbls,PHRASE_CLASS_MOD (_,_,signat))) =>
			(case (is_polyval_sig signat) of
			     NONE => (error_region();
				      print "polymorphic value specification but module component";
				      NONE)
			   | SOME (var_actual, 
				   SIGNAT_STRUCTURE(_,sdecs_actual),
				   con_actual,_,inline) =>
				 let val con_spec = con_subst(con_spec,subst)
				     val v = fresh_named_var ("copy_" ^ (Name.label2string lab))
				     val path = join_path_labels(path_actual,lbls)
				     val (coerced,result) = 
					 polyval_case (ctxt, subst, polyinst)
					 {name = v,
					  path = path,
					  varsig_spec = NONE,
					  con_spec = con_spec,
					  inline = inline,
					  var_actual = var_actual,
					  sdecs_actual = sdecs_actual,
					  con_actual = con_actual}
				     val _ = coerce(coerced,"polyval")
				 in  result
				 end)

                (* --------- Coercion from con, overexp, or none to monoval --------------- *)
	        | (DEC_EXP _, SOME(_,PHRASE_CLASS_CON _)) => 
			     (error_region_with "value specification but type component: ";
			      pp_label lab; print "\n"; NONE)
	        | (DEC_EXP _, SOME(_,PHRASE_CLASS_SIG _)) => 
			     (error_region_with "value specification but signature component: ";
			      pp_label lab; print "\n"; NONE)
	        | (DEC_EXP _, SOME(_,PHRASE_CLASS_OVEREXP _)) => 
			     (error_region_with "value specification but overloaded term component: ";
			      pp_label lab; print "\n"; NONE)
                | (DEC_EXP _, NONE) => 
			     (error_region_with "value specification but missing component: ";
			      pp_label lab; print "\n"; NONE)

	        (* ----- Coercion from mod/polyval to mod/polyval -------------------------- *)
	        | (DEC_MOD (v_spec,b,sig_spec),
	           SOME(lbls, PHRASE_CLASS_MOD (_,_,sig_actual))) => 
		       let val sig_spec = sig_subst(sig_spec,subst)
		       in
			   (case (is_polyval_sig sig_spec, is_polyval_sig sig_actual) of
				(* ----------- Coercion from polyval to polyval ------------------ *)
				(SOME (v1,s1,con_spec,_,_),
				 SOME (v2,SIGNAT_STRUCTURE(NONE, sdecs2),con_actual,_,inline_actual)) =>
				let val v = fresh_named_var ("copy_" ^ (Name.label2string lab))
				    val (coerced,result) = 
					polyval_case (ctxt,subst,polyinst)
					{name = v,
					 path = join_path_labels(path_actual,lbls),
					 varsig_spec = SOME(v1,s1),
					 con_spec = con_spec,
					 inline = inline_actual,
					 var_actual = v2,
					 sdecs_actual = sdecs2,
					 con_actual = con_actual}
				    val _ = coerce(coerced,"polyval")
			       in  result
			       end
			       (* ----------- Coercion from module to module ------------------ *)
			       | (NONE, NONE) => 
				let val mod_path = join_path_labels(path_actual,lbls)
				    val (coerced,mbody,sig_ret) = 
					xcoerce(polyinst,ctxt,mod_path,sig_actual,sig_spec)
				    val _ = coerce(coerced,"inner module")
				    val v = fresh_named_var ("copy_" ^ (Name.label2string lab))
				    val bnd = BND_MOD(v,b,mbody)
				    val dec = DEC_MOD(v,b,sig_ret)
				    val subst = subst_add_modvar(subst,v_spec,path2mod mod_path)
			       in SOME(bnd,dec,subst)
			       end
			    (* ----------- Coercion from mod to poly or polt to mod --------- *)
			    | (NONE, SOME _) => 
				  (error_region_with "module specification but value component: ";
				   pp_label lab; print "\n"; NONE)
			    | (SOME _, _) => 
				  (error_region_with "value specification but module component: ";
				   pp_label lab; print "\n"; NONE))
			end
		    
	       (* ------- coercion of a non-module component to a module spec ---- *)
	       | (DEC_MOD _, _) => 
			(error_region_with "coercion of a non-module or non-existent component\n";
			 tab_region_with   "  to a module specification failed at ";
			 pp_label lab; print "\n";
			 NONE)

	       (* ------- coercion of a type component to a type spec ---- *)
	       | (DEC_CON (v_spec, k_spec, copt_spec, _),
	          SOME(lbls,PHRASE_CLASS_CON (con_actual,k_actual,_,inline))) => 
			let
			    val v = fresh_named_var ("copy_" ^ (Name.label2string lab))
			    val bnd = BND_CON(v,con_actual)
			    val dec = DEC_CON(v,k_actual,SOME con_actual,inline)
			    val con_path = join_path_labels(path_actual, lbls)
			    val subst = subst_add_convar(subst, v_spec, path2con con_path)
			    val res = SOME(bnd,dec,subst)
			in  if eq_kind(k_spec,k_actual)
			      then 
			        (case copt_spec of
			       	   NONE => res
				 | SOME con_spec =>
				     let val con_spec = con_subst(con_spec,subst)
				     in
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
					     end
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
		   | (DEC_CON _, _) => 
			(error_region_with "coercion of a non-type or non-existent component\n";
			 tab_region_with   "  to a type specification failed at ";
			 pp_label lab; print "\n";
			 NONE))

	fun loop _ [] = ([],[])
	  | loop subst ((SDEC(l,dec))::rest) =
	    (if (!debug)
		 then (print "xcoerce_help' working on SDEC with label = "; 
		       pp_label l; print "\n")
	     else ();
	     case (doit subst (l,dec)) of
		 SOME (resbnd,resdec,subst) =>
		     let val (sbnds,sdecs) = loop subst rest
		     in ((SBND(l,resbnd))::sbnds, 
			 (SDEC(l,resdec))::sdecs)
		     end
	       | NONE => loop subst rest)
	
        val (sbnds_coerced, sdecs_coerced) = loop empty_subst sdecs_target

      in if !coerced
	  then (true, MOD_STRUCTURE sbnds_coerced,
		SIGNAT_STRUCTURE(NONE, sdecs_coerced))
	 else (false, path2mod path_actual, 
	       SIGNAT_STRUCTURE(SOME path_actual,sdecs_actual))
      end


    (* ---------- The exported signature coercion routines ------------ *)

    fun xcoerce_seal (polyinst : polyinst,
		      context : context,
		      mod_actual : mod,
		      sig_actual : signat,
		      sig_target : signat) : mod =
	    let val var_actual = fresh_named_var "orig_var"
		val path_actual = PATH(var_actual,[])
		val sig_actual_self = SelfifySig context (path_actual, sig_actual)
		val context = add_context_mod'(context,var_actual,sig_actual_self)
		val (_,mod_coerced,_) = xcoerce(polyinst, context, 
						path_actual, sig_actual, sig_target)
	    in  MOD_LET(var_actual, mod_actual, MOD_SEAL(mod_coerced, sig_target))
	    end

    fun xcoerce_functor (polyinst : polyinst,
			 context : context,
			 path_actual : path,
			 sig_actual : signat,
			 sig_target : signat) : mod * signat =
	    let val (_,mod_coerced, sig_coerced) =
		    xcoerce(polyinst, context, path_actual, sig_actual, sig_target)
	    in  (mod_coerced, sig_coerced)
	    end

    fun sig_describe_size s = 
	let fun describe d m s = (print (Int.toString d); print ": "; 
				  pp_mod m; print " has size "; print (Int.toString (sig_size s)); 
				  print "\n")
	    fun help d m (SDEC(l,DEC_MOD(_,_,s))) = loop d (MOD_PROJECT (m,l)) s
	      | help d _ _ = ()
	    and loop d m (s as (SIGNAT_STRUCTURE (_,sdecs))) = 
		(describe d m s; 
		 if (d<4) then app (help (d+1) m) sdecs else ())
	      | loop d m s = describe d m s
	in  loop 0 (MOD_VAR (fresh_var())) s
	end

    val track_coerce = Stats.ff("IlstaticTrackCoerce")
    fun xcoerce_transparent (polyinst : polyinst,
			     context : context,
			     mod_actual : mod,
			     sig_actual : signat,
			     sig_target : signat) : Il.mod * Il.signat =
	let 
	    val _ = if (!track_coerce)
			then (print "turning showing on\n";
			      Stats.bool("IlstaticShowing") := true)
		    else ()

	    (* first extract all necessary components with necessary coercions *)
	    val hidden_lbl = internal_label "hidden_module"
	    val var_actual = fresh_named_var "orig_var"
	    val path_actual = PATH(var_actual,[])
	    val sig_actual_self = SelfifySig context (path_actual, sig_actual)
	    val context = add_context_mod'(context,var_actual,sig_actual_self)
	    val (coerced,coerced_mod,coerced_sig) = xcoerce(polyinst, context, 
							    path_actual, sig_actual,sig_target)

	    val _ = if (!track_coerce)
			then (print "turning showing off\n";
			      Stats.bool("IlstaticShowing") := false)
		    else ()
	    val _ = (print "XXX hidden component\n"; 
		     sig_describe_size sig_actual; print "\n";
		     print "XXX coerced component\n"; 
		     sig_describe_size coerced_sig; print "\n")
	in
	    if (coerced)
	      then let
		       val MOD_STRUCTURE coerced_sbnds = coerced_mod
		       val SIGNAT_STRUCTURE (_,coerced_sdecs) = coerced_sig
		       val sbnd = SBND(hidden_lbl, BND_MOD(var_actual, false, mod_actual))
		       val sdec = SDEC(hidden_lbl, DEC_MOD(var_actual, false, sig_actual))
		       val mod_result = MOD_STRUCTURE(sbnd :: coerced_sbnds)
		       val sig_result = SIGNAT_STRUCTURE(NONE, sdec :: coerced_sdecs)
		   in  (mod_result, sig_result)
		   end
	    else  (mod_actual, sig_actual)
	end

    val xsig_where_type = Stats.subtimer("Elab-Signature",xsig_where_type)
    val xsig_where_structure = Stats.subtimer("Elab-Signature",xsig_where_structure)
    val xsig_sharing_types = Stats.subtimer("Elab-Signature",xsig_sharing_types)
    val xsig_sharing_structures = Stats.subtimer("Elab-Signature",xsig_sharing_structures)

    val xcoerce_seal = Stats.subtimer("Elab-Signature",xcoerce_seal)
    val xcoerce_transparent = Stats.subtimer("Elab-Signature",xcoerce_transparent)
    val xcoerce_functor = Stats.subtimer("Elab-Signature",xcoerce_functor)

end



