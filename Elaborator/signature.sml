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



	
    fun eq_labs(labs1,labs2) = Listops.eq_list(eq_label,labs1,labs2)

    (* ---------------- helpers *)

    exception SharingError
    exception WhereError
    type vlpath = (var * label) list
    type lpath = label list
    type cluster = path option ref * lpath list
    fun eq_lpath'(lpath1,lpath2) = Listops.eq_list(eq_label,lpath1,lpath2)
    fun eq_lpath lpath1 lpath2 = Listops.eq_list(eq_label,lpath1,lpath2)
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

  (* could these signatures be made equal: 
       (1) same number of type components
       (2) corresponding components are abstract or are concrete and equal 
  *)
  fun SigCouldEqual(ctxt,p,sdecs,p',sdecs') =
      let val slabs = map #2 (find_labels_sdecs ctxt sdecs)
	  val slabs' = map #2 (find_labels_sdecs ctxt sdecs')
	  val follow = follow_labels(SOME p, sdecs,ctxt)
	  val follow' = follow_labels(SOME p', sdecs',ctxt)
	  fun equaler(labs,labs') = 
	      (if eq_list(eq_label,labs,labs') then true else (print "labs not same\n"; false)) andalso
	      (case (follow labs, follow labs') of
		   (ABSTRACT _, ABSTRACT _) => true
		 | (CONCRETE _,CONCRETE _) => 
		       let val c = path2con(join_path_labels(p,labs))
			   val c' = path2con(join_path_labels(p',labs'))
			   val res = (Name.is_label_internal(List.last labs) andalso 
				      Name.is_label_internal(List.last labs))
			               orelse eq_con(ctxt,c,c')
			   val _ = if res then () 
				   else (print "both concrete failed\n";
					 print "c = "; pp_con c; print "\n";
					 print "c' = "; pp_con c'; print "\n";
					 print "\n\n")
		       in  res
		       end
		 | _ => (print "one concrete one absract failed\n"; false))
	  val res = eq_list(equaler,slabs,slabs')
	  val _ = if res then ()
		  else (print "SigCouldEqual returning false with p = ";
			pp_path p; print "  and  p' = "; pp_path p'; 
			print "\n\n sdecs = "; pp_sdecs sdecs;
			print "\n\n sdecs' = "; pp_sdecs sdecs';
			print "\n\n\n")
      in  res
      end


    (* ----------------- Substitution Helper Functions ----------------------- *)
    local
	type mapping = con Name.PathMap.map * mod Name.PathMap.map

	fun chandle (cmap,_) c =
	    (case IlUtil.con2path c of
	         SOME (PATH (v,labs)) => Name.PathMap.find(cmap,(v,labs))
	       | NONE => NONE)

	fun mhandle (_,mmap) m =
	    (case IlUtil.mod2path m of
	         SOME (PATH (v,labs)) => Name.PathMap.find(mmap,(v,labs))
	       | NONE => NONE)

	fun sdechandle mapping (SDEC(l,DEC_CON(v,k,SOME c,i))) =
	    (case chandle mapping c of
		 NONE => NONE
	       | SOME (CON_VAR v') => 
		     if (eq_var(v,v'))
			 then SOME(SDEC(l,DEC_CON(v,kind_substconmod(k,mapping),SOME c,i)))
		     else NONE
	       | _ => NONE)
	  | sdechandle _ _ = NONE

	and kind_substconmod(k,_) = k
	and con_substconmod(c,mapping) = 
		con_all_handle(fn _ => NONE, chandle mapping, mhandle mapping,
					sdechandle mapping) c
	and exp_substconmod(e,mapping) = 
		exp_all_handle(fn _ => NONE, chandle mapping, mhandle mapping,
					sdechandle mapping) e
	and sig_substconmod(s,mapping) = 
		sig_all_handle(fn _ => NONE, chandle mapping, mhandle mapping,
					sdechandle mapping) s
	and mod_substconmod(m,mapping) = 
		mod_all_handle(fn _ => NONE, chandle mapping, mhandle mapping,
					sdechandle mapping) m
	and bnd_substconmod(bnd,mapping) = 
		bnd_all_handle(fn _ => NONE, chandle mapping, mhandle mapping,
					sdechandle mapping) bnd
	and dec_substconmod(dec,mapping) = 
		dec_all_handle(fn _ => NONE, chandle mapping, mhandle mapping,
			       sdechandle mapping) dec
    in
	type mapping = mapping
	val empty_mapping = (Name.PathMap.empty, Name.PathMap.empty)
	fun path2vpath (PATH vlabs) = vlabs
	fun con_addmap((cmap,mmap),path,c) = (Name.PathMap.insert(cmap,path2vpath path,c), mmap)
	fun mod_addmap((cmap,mmap),path,m) = (cmap, Name.PathMap.insert(mmap,path2vpath path,m))
	val kind_substconmod = kind_substconmod
	val con_substconmod = con_substconmod
	val exp_substconmod = exp_substconmod
	val sig_substconmod = sig_substconmod
	val mod_substconmod = mod_substconmod
	val dec_substconmod = dec_substconmod
	val bnd_substconmod = bnd_substconmod
    end


    (* --------------------------------------------------------- 
      ------------------ SIGNATURE PATCHING -------------------
      --------------------------------------------------------- *)


    fun rewrite_type_component (ctxt,orig_sdecs,labs,con) = 
	let
	    local val fv = con_free_convar con
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
			  | DEC_MOD(v,_,_)  => (bound v; sdec::(docon curl rest))
			  | DEC_EXCEPTION _ => sdec::(docon curl rest)))
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
		    | loop ((sdec as SDEC(l,DEC_EXCEPTION(_,_)))::rest) = (sdec::(loop rest))
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
			   let fun loop [] current = SIGNAT_OF(path2mod (vlpath2path current))
				 | loop ((v1,l1)::rest1) (all2 as ((v2,l2)::rest2)) = 
			            if (eq_var(v1,v2))
					then loop rest1 rest2
				    else SIGNAT_OF(path2mod (vlpath2path all2))
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
	in  fn (typeslots : typeslot list, sdecs) =>
            let 
	      val first = ref NONE
              fun found_first() = case (!first) of NONE => false | _ => true
              fun transparent (curpath : (var * label) list, copt) =
		case (!first) of
		  SOME (first_path,first_abstract) =>
		    (case (first_abstract,copt) of
		       (true,NONE) => SOME(combine_path first_path curpath)
		     | (false,SOME _) => 
				if (IlStatic.eq_con(ctxt,path2con(vlpath2path first_path),
						    path2con(vlpath2path curpath)))
				then SOME(combine_path first_path curpath)
				else copt
		     | _ => copt)
                | NONE => let val abstract = case copt of 
					    NONE => true
					  | SOME _ => false
		 	      val _ = first := SOME (curpath, abstract)
			  in  copt
			  end
	      fun meta_match prefix (vlpath : (var * label) list) typeslot : bool =
		let val labs1 = map #2 vlpath
		    val labs2 = case typeslot of ABSTRACT (l,_) => l | CONCRETE (l,_,_) => l
		    fun loop ([],[]) = true
		      | loop ([],_) = prefix
		      | loop (_,[]) = false
		      | loop (a::aa,b::bb) = eq_label(a,b) andalso loop(aa,bb)
		in  loop (labs1,labs2)
		end
	      fun traverse _ [] = [] 
	        | traverse (_,[]) sdecs = sdecs
                | traverse (cur_path,typeslots) ((SDEC(l,dec))::rest) =
		  let val dec' = (case dec of
		       DEC_CON(v,k,copt,i) => 
		       let val cur_path' = cur_path @ [(v,l)]
			   val is_match = List.exists (meta_match false cur_path') typeslots
		           val copt = if is_match then transparent(cur_path',copt) else copt
		       in  DEC_CON(v,k,copt,i)
		       end
		     | DEC_MOD(v,b,s) =>
			let val cur_path' = cur_path@[(v,l)]
			    val typeslots' = List.filter (meta_match true cur_path') typeslots
			in  if (null typeslots')
				then dec
			    else (case (reduce_signat ctxt s) of
				      SIGNAT_STRUCTURE(popt,sdecs) =>
					  let val is_first = (not(found_first())
							      andalso (length typeslots' = 1))
					      val sdecs' = traverse(cur_path',typeslots') sdecs
					  in  DEC_MOD(v,b,if is_first then s
							  else SIGNAT_STRUCTURE(popt,sdecs'))
					  end
				    | _ => dec)
			end
		     | _ => dec)
                   in (SDEC(l,dec')) :: (traverse (cur_path,typeslots) rest)
	           end
	    in traverse ([],typeslots) sdecs
	    end (* fn *)
        end (* xsig_sharing_rewrite *)

    fun xsig_where_type(ctxt,orig_sdecs,lpath, con, kind) : sdecs =
	(case (follow_labels (NONE,orig_sdecs,ctxt) lpath) of 
	     ABSTRACT (lbls,k) => if (eq_kind(k,kind))
				      then rewrite_type_component (ctxt, orig_sdecs, lbls, con)
				  else
				      (error_region();
				       print "kind mismatch in where type of component:";
				       pp_lpath lbls;
				       print "\n";
				       orig_sdecs)
	   | CONCRETE (lbls,_,c) => if (eq_con(ctxt,con,c))
				      then orig_sdecs
				    else (error_region();
					  print "cannot where type CONCRETE but unequal type component:";
					  pp_lpath lbls;
					  print "\n";
					  orig_sdecs))


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
		(case Context_Lookup_Path(context,PATH(mjunk_var,labs1)) of
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
			 (case (Sdecs_Lookup' context (m2,sdecs2,plabs)) of
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
		     if (IlStatic.Sig_IsSub(context,sig2,sig1))
			 then xsig_sharing_rewrite_structure(context,sdecs,[labs1],SOME(SIGNAT_OF m2))
		     else xsig_where_structure_slow(context,sdecs,labs1,m2,sig2)
	       | _ => xsig_where_structure_slow(context,sdecs,labs1,m2,sig2))
	end
    handle WhereError => sdecs
	 | e => (print "\nsig2 = "; pp_signat sig2;
		 print "\nsdecs= "; pp_sdecs sdecs;
		 print "\n";
		 raise e)


  and xsig_sharing_structure(ctxt,sdecs,paths : lpath list) : sdecs = 
      let
	  val _ = print "xsig_sharing_structre called\n"
	  val mjunk = fresh_named_var "mjunk_sharing_structure"
	  val mpath = PATH (mjunk,[])
	  val s = SelfifySig ctxt (mpath, SIGNAT_STRUCTURE(NONE, sdecs))
	  val SIGNAT_STRUCTURE(_,sdecs') = s
	  val ctxt' = add_context_mod'(ctxt,mjunk,s)
	  fun path2triple p = 
	      (case (Sdecs_Lookup' ctxt' (MOD_VAR mjunk,sdecs',p)) of
		   SOME(lpath,PHRASE_CLASS_MOD(_,_,s)) => 
		       let 
			   val vpath = join_path_labels(mpath,lpath)
			   val sdecs = 
			       (case reduce_signat ctxt' s of
				    SIGNAT_STRUCTURE(_,sdecs) => sdecs
				  | _ => error "sharing got bad structure component\n")
		       in  (vpath,lpath,(lpath,sdecs))
		       end
		 | _ => (error_region();
			 print "structure sharing given a non-structure component: ";
			 pp_lpath p; print "\n";
			 raise SharingError))
	  val lpath_var_sdecs_list = map path2triple paths
	  fun triple_has_var NONE [] = false
	    | triple_has_var (SOME _) [] = true
	    | triple_has_var NONE ((vpath,_,(_,sdecs))::rest) = triple_has_var (SOME (vpath,sdecs)) rest
	    | triple_has_var (sopt as SOME (vpath,sdecs)) ((vpath',_,(_,sdecs'))::rest) = 
	      SigCouldEqual(ctxt',vpath,sdecs,vpath',sdecs') andalso triple_has_var sopt rest
	      
      in  if (triple_has_var NONE lpath_var_sdecs_list)
	      then 
		  let val _ = print "STRUCTURE_SHARING succeeded\n"
(*
		   app (fn (_,s,_) => (print "STRUCTURE_SHARING succeeded with s = ";
				       pp_signat s; 
				       print "  which selfified to = \n";
				       pp_signat (SelfifySig ctxt (PATH (mjunk2,[]), s));
				       print "\n\n\n")) lpath_var_sdecs_list;
*)
		      val sdecs = xsig_sharing_rewrite_structure(ctxt,sdecs,
								 map #2 lpath_var_sdecs_list,NONE)
(*
		      val _ = (print "returning sdecs = ";
			       pp_sdecs sdecs; print "\n\n\n")
*)
		  in  sdecs
		  end
	  else let val _ = print "STRUCTURE_SHARING failed; expanding to components\n"
		   val res = xsig_sharing_structure_components(ctxt,sdecs,
							       map #3 lpath_var_sdecs_list)
(*
		      val _ = (print "returning sdecs = ";
			       pp_sdecs sdecs; print "\n\n\n")
*)
	       in  res
	       end
      end



  (* we are supposed to share the type components only where possible *)
  and xsig_sharing_structure_components
      (ctxt,sdecs,lpath_sdecs_list : (label list * sdecs) list) : sdecs = 
      let
	  val _ = print "xsig_sharing_structre called\n"
	  fun getcomponents (lpath,sdecs) : (lpath * lpath list) = 
	      let
		  fun traverse (SDEC(l,DEC_CON (_,_,_,_))) = [[l]]
		    | traverse (SDEC(l,DEC_MOD (v,_,SIGNAT_STRUCTURE(_,sdecs)))) = 
			  let val lpaths = List.concat (map traverse sdecs)
			  in map (fn lpath => (l :: lpath)) lpaths
			  end
		    | traverse _ = []
	      in (lpath, List.concat (map traverse sdecs))
	      end
	  val lpath_lpaths_list : (lpath * lpath list) list = map getcomponents lpath_sdecs_list
	  val common_lpaths = foldl (fn ((_,cur),acc) => inter_lpaths cur acc)
	                       (#2 (hd lpath_lpaths_list)) (tl lpath_lpaths_list)
	  val lpath_lpaths_list = map (fn (lp,cur) => (lp,inter_lpaths cur common_lpaths))
	                             lpath_lpaths_list
(*
	  val lpaths = #2 (hd lpath_lpaths_list)
	  val num_types = length lpaths
	  val _ = if (andfold (fn (_,lpaths) => length lpaths = num_types) lpath_lpaths_list)
		      then ()
		  else (error_region();
			print "structure sharing failed\n";
			raise SharingError)
*)
	  val labels : lpath list list = (map (fn (lpath,lpaths) => map (fn lps => lpath @ lps) lpaths) 
					  lpath_lpaths_list)
	  val follow_labels = follow_labels (NONE,sdecs,ctxt)
	  val labels_list : typeslot list list = mapmap follow_labels labels
	  val slots_list = Listops.transpose labels_list

	  val _ = print "xsig_sharing_structre calling rewrite\n"
	  val xsig_sharing_rewrite = xsig_sharing_rewrite(ctxt,sdecs)
	  fun folder (labels : typeslot list,sdecs) = xsig_sharing_rewrite(labels, sdecs)
	  val res = (foldl folder sdecs slots_list)
	  val _ = print "xsig_sharing_structre done\n"
      in  res
      end
  handle SharingError => sdecs



  (* ------ The paths are to be shared ----- *)
  and xsig_sharing_type(ctxt,sdecs,paths) : sdecs = 
      let val mjunk = MOD_VAR(fresh_named_var "mjunk_sharing_type")
	  fun path2label p = 
	      (case (Sdecs_Lookup' ctxt (mjunk,sdecs,p)) of
		   SOME(labs,_) => follow_labels (NONE,sdecs,ctxt) labs
		 | NONE => (error_region();
			    print "sharing type given non-existent path ";
			    pp_lpath p; print " with the following components\n";
			    pp_sdecs sdecs; print "\n";
			    raise SharingError))
	  val (slots as (first::rest)) : typeslot list = map path2label paths
	  fun is_abstract k (ABSTRACT (_,k')) = eq_kind(k,k')
	    | is_abstract _ _ = false
	  fun is_concrete c (CONCRETE (_,_,c')) = eq_con(ctxt,c,c')
	    | is_concrete _ _ = false
      in  (case first of
	       ABSTRACT (lbls,k) => if (Listops.andfold (is_abstract k) rest)
				    then xsig_sharing_rewrite(ctxt,sdecs)(slots,sdecs)
				else (error_region(); 
				      print "cannot share abstract with concrete type components\n";
				      raise SharingError)
	     | CONCRETE (lbls,k,c) => if (Listops.andfold (is_concrete c) rest)
					then sdecs
				    else (error_region(); 
					  print "cannot share abstract with concrete type components\n";
					  raise SharingError))
      end
      handle SharingError => sdecs


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
				   ((v,VAR v')::(#1 subst), #2 subst, #3 subst))
	                     | (DEC_CON(v,k,c,i), DEC_CON(v',_,_,_)) => 
				  (SDEC(l,DEC_CON(v',k,c,i)),
				   (#1 subst, (v,CON_VAR v')::(#2 subst), #3 subst))
	                     | (DEC_MOD(v,s,p), DEC_MOD(v',_,_)) => 
				  (SDEC(l,DEC_MOD(v',s,p)),
				   (#1 subst, #2 subst, (v,MOD_VAR v')::(#3 subst)))
			     | _ => (sdec_change,subst))
		   | _ => (sdec_change, subst))
	    val (sdecs_change,(esubst,csubst,msubst)) = foldl_acc folder ([],[],[]) sdecs_change
	    val sig_temp = SIGNAT_STRUCTURE(NONE, sdecs_change)
	    val SIGNAT_STRUCTURE(_,sdecs_result) = sig_subst_expconmodvar(sig_temp,esubst,csubst,msubst)
	in  sdecs_result
	end

       (* ---- coercion of a poly component to a mono/poly specification --- *)
       fun polyval_case (ctxt : context, polyinst)
	   {name : var, (* use this name for the final binding *)
	    path : path, (* this path is to the actual polymorphic component *)
	    varsig_spec : (var * signat) option, (* spec might me monomorphic *)
	    con_spec : con, (* the type of the specification *)
	    var_actual : var,
	    inline : bool,
	    sdecs_actual : sdec list,
	    con_actual : con} (* the signature of the component *)
		  : bool * (bnd * dec * context) option = 
	   let 
	       val con_spec = 
		   (case varsig_spec of
			NONE => con_spec
		      | SOME(v,_) => con_subst_modvar(con_spec, [(v, MOD_VAR var_actual)]))

	       fun local_error () =
		   (error_region();
		    print "Coercion of a polymorphic value component to a\n";
		    error_region();
		    print "  monomorphic/polymorphic specification failed at ";
		    pp_path path;
		    print "\n"; 
		    error_region();
		    print "Expected type: ";
		    (case varsig_spec of
			 NONE => pp_con con_spec
		       | SOME (v,s) => pp_signat 
			     (SIGNAT_FUNCTOR(v,s,
					     SIGNAT_STRUCTURE(NONE,[(SDEC(it_lab,
								    DEC_EXP(name,con_spec,NONE,false)))]),
					     PARTIAL)));
			 print "\n";
		    error_region();
		    print "Actual type: ";
		    pp_signat 
		    (SIGNAT_FUNCTOR(var_actual, 
				    SIGNAT_STRUCTURE(NONE, sdecs_actual),
				    SIGNAT_STRUCTURE(NONE,[(SDEC(it_lab,
								 DEC_EXP(name,con_actual,
									 NONE,false)))]),
				    PARTIAL));
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
		   fun folder (SBND(l,BND_CON(v,c)),csubst) =
			(CON_MODULE_PROJECT(MOD_VAR var_actual, l), c)::csubst
		     | folder (_,csubst) = csubst
		   val csubst = foldl folder [] sbnds_poly
		   fun con_handler c = assoc_eq(eq_cpath,c,csubst)
	       in
		   val con_actual_tyvar = con_all_handle(default_exp_handler,con_handler,
							 default_mod_handler,default_sdec_handler) con_actual
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

	   val res as (_,SOME(bnd,dec,_)) =   if (sub_con(ctxt',con_actual_tyvar,con_spec))
		 then 
		   ((case varsig_spec of
			 NONE => 
			     let val mtemp = MOD_APP(path2mod path,MOD_STRUCTURE sbnds_poly)
			     in  (true,
				  SOME(BND_EXP(name,MODULE_PROJECT(mtemp,it_lab)),
				       DEC_EXP(name,con_actual_tyvar,NONE,inline),
				       ctxt))
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
				     DEC_MOD(name, true, s2), ctxt))
			   end))
	       else local_error()

	   val _ = if (!debug)
		       then (print "polyval: result bnd = ";
			     pp_bnd bnd; print "\nresult dec = ";
			     pp_dec dec; print "\n\n")
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
		     val m4body = mod_subst_modvar(m4body,[(m4var,m4_arg)])
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
		  (true, MOD_STRUCTURE [], SIGNAT_STRUCTURE(NONE,[]))))
	end

   and xcoerce_structure (polyinst : polyinst,
			  context : context,
			  path_actual : path,
			  sdecs_actual : sdec list,
			  sdecs_target : sdec list) : (bool * Il.mod * Il.signat) = 
      let 

	  val sdecs_target = sdecs_rename(sdecs_target, sdecs_actual)
	  val coerced = ref false

	  val _ = if (!debug)
		      then (print "xcoerce_structure------";
			    print "sdecs_actual = \n"; pp_sdecs sdecs_actual; print "\n";
			    print "sdecs_target = \n"; pp_sdecs sdecs_target; print "\n")
		  else ()

	  val _ = 
	      if (Listops.eq_list (fn (SDEC(l1,_),SDEC(l2,_)) => eq_label(l1,l2),
				   sdecs_actual, sdecs_target))
		  then () 
	      else 
		  (coerced := true;
		   print "coerced set to true because of length/order mismatch\n")

	  fun makecon(path,labs) = path2con(join_path_labels(path,labs))

	  local
	      fun sig_lookup (p,s) lbls : (label list * phrase_class) option = 
		  (case s of
		       SIGNAT_STRUCTURE (_,self_sdecs) =>
			   Sdecs_Lookup' context (path2mod p, self_sdecs, lbls)
		     | SIGNAT_FUNCTOR _ => NONE)
	      fun lookup [] lbl : dec option = NONE
		| lookup (SDEC(l,d)::rest) lbl = 
		  if (eq_label(l,lbl))
		      then SOME d
		  else (case (is_open l, d) of
			    (true, DEC_MOD(_,_,s)) =>
				(case s of
				     SIGNAT_STRUCTURE(_,sdecs) =>
					 (case lookup sdecs lbl of
					      NONE => lookup rest lbl
					    | decopt => decopt)
				   | _ => lookup rest lbl)
			  | _ => lookup rest lbl)
	      fun labs_lookup sdecs (lbl::rest) = 
		  let val decopt = lookup sdecs lbl
		  in  case (rest,decopt) of
		      ([],SOME dec) => SOME dec
		    | (_,SOME(DEC_MOD(_,_,SIGNAT_STRUCTURE(_,sdecs)))) => labs_lookup sdecs rest
		    | _ => NONE
		  end
		| labs_lookup _ _ = NONE
  
	      val sig_actual_self =
		  SelfifySig context (path_actual, 
				      SIGNAT_STRUCTURE(NONE,sdecs_actual))

	  in  val labs_lookup = labs_lookup
	      val sig_actual_lookup = sig_lookup (path_actual,sig_actual_self)
	      val dec_actual_lookup = labs_lookup sdecs_actual
	      val dec_target_lookup = labs_lookup sdecs_target
	  end
		  
	fun doit ctxt labs : (bnd * dec * context) option = 
	  let 
		fun general_mod v =
		(case (sig_actual_lookup labs) of
		     SOME(lbls,PHRASE_CLASS_MOD (_,b,s1)) =>
		     let 
			 val s = (case (is_open (hd lbls), dec_target_lookup labs, s1) of
				      (_,SOME(DEC_MOD (_,_,s)),_) =>  s
				    | (true,_,SIGNAT_STRUCTURE (_, sdecs1)) =>
					  SIGNAT_STRUCTURE(NONE,
					  List.filter (fn(SDEC(l,_)) =>
						    (case labs_lookup sdecs1 [l] of
							 NONE => false
						       | SOME _ => true)) sdecs_target)
				    | _ => SIGNAT_STRUCTURE(NONE,[]))
			 val (inner_coerced,mbody,sig_ret) = 
			     xcoerce(polyinst,ctxt,
					  join_path_labels(path_actual,lbls),s1,s)
			 val _ = if inner_coerced
				     then (print "coerced set to true because of innermod\n";
					   coerced := true)
				 else ()
			 val v1 = fresh_var()
			 val v' = derived_var v
			 val bnd = BND_MOD(v',b,mbody)
			 val dec = DEC_MOD(v',b,sig_ret)
			 val ctxt = add_context_dec(ctxt,SelfifyDec ctxt (DEC_MOD(v,b,sig_ret)))
		     in SOME(bnd,dec,ctxt)
		     end
		   | _ => (error_region();
			   print "coercion of a non-structure component to a ";
			   print "structure specification failed: ";
			   pp_lpath labs;
			   print "\n";
			   NONE))
	    in
		(case (dec_target_lookup labs, dec_actual_lookup labs) of
                   (* ------- coercion to a monomorphic value specificiation ---- *)
		     (SOME(DEC_EXP(v,c,eopt,i)),_) =>
			 (case (sig_actual_lookup labs) of
			      SOME(lbls,PHRASE_CLASS_EXP (e,con,eopt',inline)) => (* con has var_actual *)
				    if (sub_con(ctxt,con,c) andalso
					(case (eopt,eopt') of
					     (SOME e, SOME e') => eq_exp(ctxt,e,e')
					   | (SOME _, NONE) => false
					   | (NONE, _) => true))
				    then
					let 
					    val v' = derived_var v
				            val exp = if (inline) then e 
							else path2exp(join_path_labels(path_actual,lbls))
					    val bnd = BND_EXP(v',exp)
					    val dec = DEC_EXP(v',con,eopt',i)
					in  SOME(bnd,dec,ctxt)
					end 
				    else 
				     (error_region();
				      print "coercion of a monomorphic value component to a\n";
				      error_region();
				      print "monomorphic value specification failed\n";
				      print "Component name: ";
				      pp_lpath labs;
				      print "\nExpected type:\n";
				      pp_con c;
				      print "\nFound type:\n";
				      pp_con con;
				      print "\nExpected expanded type:\n";
				      pp_con (con_normalize(ctxt,c));
				      print "\nFound expanded type:\n";
				      pp_con (con_normalize(ctxt,con));
				      print "\n";
				      NONE)
			    | SOME(lbls,PHRASE_CLASS_MOD (_,_,
				 (SIGNAT_FUNCTOR(var_actual,
						 SIGNAT_STRUCTURE(_,sdecs_actual),
						 SIGNAT_STRUCTURE (NONE,
								   [SDEC(maybe_it,DEC_EXP(_,con_actual,_,inline))]),_)))) => 
				  let val name = derived_var v
				      val path = join_path_labels(path_actual,lbls)
				      val (coerced',result) = 
					  polyval_case (ctxt, polyinst)
					  {name = derived_var v,
					   path = path,
					   varsig_spec = NONE,
					   con_spec = c,
					   inline = inline,
					   var_actual = var_actual,
					   sdecs_actual = sdecs_actual,
					   con_actual = con_actual}
				      val _ = if coerced' 
						  then (msg "coerced because of polyval\n";
							coerced := true)
					      else ()
				  in  result
				  end
			    | SOME(_,PHRASE_CLASS_CON _) => 
				  (error_region();
				   print "value specification but type component";
				   NONE)
			    | SOME(_,PHRASE_CLASS_SIG _) => 
				  (error_region();
				   print "value specification but signature component";
				   NONE)
			    | SOME(_,PHRASE_CLASS_OVEREXP _) => 
				  (error_region();
				   print "value specification but OVEREXP";
				    NONE)
			    | NONE => (error_region();
				       pp_lpath labs;
				       print " component in signature not in structure\n";
				       NONE))
		   | (_, SOME(DEC_EXP _)) => NONE
	           (* ----- check for polymorphic specification case first ---- *)
		   | (SOME(DEC_MOD(v,_,ss1)), SOME(DEC_MOD(_,_,_))) => 
			let val (ss2,lbls) = 
			    (case (sig_actual_lookup labs) of
				 SOME(lbls, PHRASE_CLASS_MOD(_,_,s2)) => (s2,lbls)
			       | _ => error "lookup inconsistent")
			in
			  (case (ss1,ss2) of
			     (SIGNAT_FUNCTOR(v1,s1,SIGNAT_STRUCTURE (NONE,
				   [SDEC(maybe_it1,DEC_EXP(_,c1,_,_))]),_),
			      SIGNAT_FUNCTOR(v2,
					     SIGNAT_STRUCTURE(NONE, sdecs2),
					     SIGNAT_STRUCTURE (NONE,
					      [SDEC(maybe_it2,DEC_EXP(_,c2,_,i2))]),_)) =>
			       if (eq_label (maybe_it1, it_lab) andalso
				   eq_label (maybe_it1, it_lab))
				   then  
				       let val (coerced',result) = 
					       polyval_case (ctxt,polyinst)
					       {name = derived_var v,
						path = join_path_labels(path_actual,lbls),
						varsig_spec = SOME(v1,s1),
						con_spec = c1,
						inline = i2,
						var_actual = v2,
						sdecs_actual = sdecs2,
						con_actual = c2}
					   val _ = if coerced' 
					       then (print "coerced because of polyval\n";
						     coerced := true)
						   else ()
				       in  result
				       end
			       else (error_region();
				     print "Functor specifiction inside structure\n";
				     NONE)
				 | _ => general_mod v)
			end

                   (* ---- coercion of non-polyfun module component ---------- *)
         	   | (SOME(DEC_MOD(v,_,s)),_) => 
			(error_region();
			 print "module specification but non-module component\n";
			 NONE)

		   (* ------- coercion of a type component to a type spec ---- *)
		   | (SOME(DEC_CON(v_tar,_,copt_tar,_)),SOME(DEC_CON(v,_,_,i))) =>
			(case (sig_actual_lookup labs) of
			     SOME(lbls,PHRASE_CLASS_CON (con,k,_,_)) => 
				  (* con is typically a path from var_actual *)
				 let
				     val ctxt = add_context_dec(ctxt,DEC_CON(v,k,SOME con,i))
				     val ctxt = if (eq_var(v,v_tar))
						    then ctxt
						else add_context_dec(ctxt,DEC_CON(v_tar,k,SOME con,i))
				     val _ = 
				       (case copt_tar of
					  NONE => ()
					| SOME c_tar =>
					 if sub_con(ctxt,con,c_tar) 
					     then ()
					 else (let val con' = con_normalize(ctxt,con)
						   val c_tar' = con_normalize(ctxt,c_tar)
					       in  error_region();
						   print "coercion of a type component to a ";
						   print "type specification failed at ";
						   pp_lpath labs;
						   print "\nExpected type: ";
						   pp_con c_tar;
						   print "\nActual type: ";
						   pp_con con;
						   print "\nReduced Expected type: ";
						   pp_con c_tar';
						   print "\nReduced Actual type: ";
						   pp_con con';
						   print "\n"
					       end))
				     val bnd = BND_CON(v,con)
				     val dec = DEC_CON(v,k,SOME con,i)
				 in SOME(bnd,dec,ctxt)
				 end
			    | _ => (error_region();
				    print "coercion of a non-type or non-existent component to a ";
				    print "type specification failed at ";
				    pp_lpath labs;
				    print "\n";
				    NONE))
		   | (SOME(DEC_EXCEPTION _),_) => elab_error "bad spec - DEC_EXCEPTION"
		   | (NONE,_) => elab_error "ill-formed specification - no actual")
	    end

	fun loop (ctxt,[]) = ([],[])
	  | loop (ctxt,(SDEC(l,_))::rest) =
	    (if (!debug)
		 then (print "xcoerce_help' working on SDEC with label = "; 
		       pp_label l; print "\n")
	     else ();
	     case (doit ctxt [l]) of
		 SOME (resbnd,resdec,ctxt') =>
		     let val (sbnds,sdecs) = loop(ctxt',rest)
		     in ((SBND(l,resbnd))::sbnds, 
			 (SDEC(l,resdec))::sdecs)
		     end
	       | NONE => loop(ctxt,rest))
	
        val (sbnds_coerced, sdecs_coerced) = loop(context,sdecs_target)

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
		val path = PATH(var_actual,[])
		val sig_actual_self = SelfifySig context (PATH (var_actual,[]), sig_actual)
		val context = add_context_mod'(context,var_actual,sig_actual_self)
		val (_,mod_coerced,_) = 
		    xcoerce(polyinst, context, path, sig_actual,sig_target)
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

	    (* first call perform an opaque sealing for type-checking reasons *)
	    val hidden_lbl = internal_label "hidden_module"
	    val var_actual = fresh_named_var "orig_var"
	    val path = PATH(var_actual,[])
	    val sig_actual_self = SelfifySig context (PATH (var_actual,[]), sig_actual)
	    val context = add_context_mod'(context,var_actual,sig_actual_self)
	    val (coerced,coerced_mod,coerced_sig) = 
		xcoerce(polyinst, context, path, sig_actual,sig_target)

	    val _ = if (!track_coerce)
			then (print "turning showing off\n";
			      Stats.bool("IlstaticShowing") := false)
		    else ()			
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


end



