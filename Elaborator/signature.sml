(*$import Il IlStatic IlUtil Ppil IlContext Error SIGNATURE Bool Stats Option *)

(* Need to improve where_structure to use SIGNAT_OF *)

structure Signature :> SIGNATURE =
  struct


    open Il IlUtil Ppil 
    open IlStatic 
    open Util Listops Name Tyvar
    open IlContext Error


    fun error s : 'a = Util.error "signature.sml" s
    fun pat_error s : 'a= Util.error "signature.sml pattern impossibility" s
    fun elab_error s : 'a = Util.error "signature.sml elaborator impossibility" s

    val debug = Stats.ff("SignatureDebug")
    val debug_full = Stats.ff("SignatureDebugFull")
    fun msg s = if !debug then print s else ()

    fun debugdo t = if (!debug) then (t(); ()) else ()
    type labels = label list

    (* ----------------- Misc Helper Functions ----------------------- *)
    fun eta_contract (m as (MOD_FUNCTOR(_,vsig,signat,MOD_APP(f,arg),_))) = 
	let fun match (SBND(l1,BND_CON(_,c)),SDEC(l2,DEC_CON _)) =
		(case con_deref c of
		  CON_MODULE_PROJECT(MOD_VAR v,l3) =>
			(eq_label(l1,l2) andalso eq_label(l2,l3) andalso eq_var(vsig,v))
		| _ => false)
	      | match (SBND(l1,BND_EXP(_,_)), SDEC(l2,DEC_EXP _)) = 
		     (eq_label(l1,l2) andalso is_eq l1)
	      | match _ = (print "match OTHER case\n"; false)
	in  (case (signat,arg) of
		(SIGNAT_STRUCTURE(_,sdecs),MOD_STRUCTURE sbnds) => 
			if ((length sbnds = length sdecs) andalso
			    (andfold match (Listops.zip sbnds sdecs)))
			then (true,f) else (false,m)
	      | _ => (false,m))
	end

      | eta_contract m = (false,m)

    fun kind_eq_shape (KIND_INLINE(k1,_),k2) = kind_eq_shape(k1,k2)
      | kind_eq_shape (k1,KIND_INLINE(k2,_)) = kind_eq_shape(k1,k2)
      | kind_eq_shape (KIND_TUPLE m, KIND_TUPLE n) = m = n
      | kind_eq_shape (KIND_ARROW(m1,m2),KIND_ARROW(n1,n2)) = m1=n1 andalso m2=n2
      | kind_eq_shape _ = false

    fun con_normalize (arg as (ctxt,con)) = 
	IlStatic.con_normalize arg handle e => 
	    (print "ERROR in con_normalize from signat.sml\n"; con)
    fun con_head_normalize (arg as (ctxt,con)) = 
	IlStatic.con_head_normalize arg handle e => 
	    (print "ERROR in con_head_normalize from signat.sml\n"; con)
    fun sub_con arg = IlStatic.sub_con arg handle e => 
	    (print "ERROR in sub_con from signat.sml\n"; false)
    fun eq_con arg = IlStatic.eq_con arg handle e => 
	    (print "ERROR in eq_con from signat.sml\n"; false)

	
    fun eq_labs(labs1,labs2) = Listops.eq_list(eq_label,labs1,labs2)

  datatype typeslot = ABSTRACT of label list
		    | CONCRETE of label list


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
      in  fn labels =>
	  let val c = path2con(join_path_labels(path,labels))
	      val c' = con_normalize(ctxt,c)
	  in  case (con2path c') of
	      SOME p => let val PATH(v',lbls) = p
			in  if (eq_var(v,v')) 
				then ABSTRACT lbls
			    else CONCRETE labels
			end
	    | NONE => CONCRETE labels
	  end
      end

     fun signat2sdecs ctxt s = 
	 (case reduce_signat ctxt s of
	      SIGNAT_STRUCTURE(_,sdecs) => SOME sdecs
	    | SIGNAT_INLINE_STRUCTURE{abs_sig,...} => SOME abs_sig
	    | s => NONE)

     fun find_labels_sdecs context sdecs = 
	 let fun driver path sdecs = rev(foldl (find_labels path) [] sdecs)
	     (* the path being carried is backwards so we must reverse when we add to accumulator *)
	     and find_labels path (SDEC(l,DEC_CON (_,k,NONE)),_) = 
		 error "find_labels should not encounter any abstract types"
	       | find_labels path (SDEC(l,DEC_CON (_,k,SOME c)),kpaths) =  (k,rev(l::path))::kpaths
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

	fun sdechandle mapping (SDEC(l,DEC_CON(v,k,SOME c))) =
	    (case chandle mapping c of
		 NONE => NONE
	       | SOME (CON_VAR v') => 
		     if (eq_var(v,v'))
			 then SOME(SDEC(l,DEC_CON(v,kind_substconmod(k,mapping),SOME c)))
		     else NONE
	       | _ => NONE)
	  | sdechandle _ _ = NONE

	and kind_substconmod(KIND_INLINE(k,c),mapping) = KIND_INLINE(k,con_substconmod(c,mapping))
	  | kind_substconmod(k,_) = k
	and con_substconmod(c,mapping) = 
		con_all_handle(c, fn _ => NONE, chandle mapping, mhandle mapping,
					sdechandle mapping)
	and sig_substconmod(s,mapping) = 
		sig_all_handle(s, fn _ => NONE, chandle mapping, mhandle mapping,
					sdechandle mapping)
	and mod_substconmod(m,mapping) = 
		mod_all_handle(m, fn _ => NONE, chandle mapping, mhandle mapping,
					sdechandle mapping)
	and bnd_substconmod(bnd,mapping) = 
		bnd_all_handle(bnd, fn _ => NONE, chandle mapping, mhandle mapping,
					sdechandle mapping)
	and dec_substconmod(dec,mapping) = 
		dec_all_handle(dec, fn _ => NONE, chandle mapping, mhandle mapping,
					sdechandle mapping)
    in
	type mapping = mapping
	val empty_mapping = (Name.PathMap.empty, Name.PathMap.empty)
	fun path2vpath (PATH vlabs) = vlabs
	fun con_addmap((cmap,mmap),path,c) = (Name.PathMap.insert(cmap,path2vpath path,c), mmap)
	fun mod_addmap((cmap,mmap),path,m) = (cmap, Name.PathMap.insert(mmap,path2vpath path,m))
	val kind_substconmod = kind_substconmod
	val con_substconmod = con_substconmod
	val sig_substconmod = sig_substconmod
	val mod_substconmod = mod_substconmod
	val dec_substconmod = dec_substconmod
	val bnd_substconmod = bnd_substconmod
    end


    (* --------------------------------------------------------- 
      ------------------ SIGNATURE PATCHING -------------------
      --------------------------------------------------------- *)
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
		    val labs2 = case typeslot of ABSTRACT l => l | CONCRETE l => l
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
		       DEC_CON(v,k,copt) => 
		       let val cur_path' = cur_path @ [(v,l)]
			   val is_match = List.exists (meta_match false cur_path') typeslots
		           val copt = if is_match then transparent(cur_path',copt) else copt
		       in  DEC_CON(v,k,copt)
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

    fun xsig_where_type(ctxt,orig_sdecs,lbls, con, kind) : sdecs =
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
			  pp_lpath lbls;
			  print "\n";
			  print "orig_sdecs = "; pp_sdecs orig_sdecs; print "\n";
			  [])
		 | ((sdec as SDEC(l,dec))::rest) => 
		       (case dec of
			    DEC_CON(v,k,NONE) => 
				(bound v; 
				 if eq_label(l,curl)
				     then if (kind_eq_shape(k,kind))
					      then (SDEC(l,DEC_CON(v,k,SOME con)))::rest
					  else (error_region();
						print "signature wheretype failed due to constructor arity\n";
						print "\nExpected kind: ";
						pp_kind k;
						print "\nActual kind: ";
						pp_kind kind; print "\n";
						sdecs)
				 else sdec::(docon curl rest))
			  | DEC_EXP(v,_) => (bound v; sdec::(docon curl rest))
			  | DEC_MOD(v,_,_)  => (bound v; sdec::(docon curl rest))
			  | DEC_CON(v,_,_) => (bound v; sdec::(docon curl rest))
			  | DEC_EXCEPTION _ => sdec::(docon curl rest)))
	  fun dosig [] sdecs = elab_error "xsig_wheretype got empty lbls"
	    | dosig [l] sdecs = docon l sdecs
	    | dosig (curl::restl) sdecs = 
	      let 
		  fun loop [] : sdecs = 
		      (error_region();
		       print "signature wheretype could not find component ";
		       pp_label curl; print " in sdecs:\n";
		       pp_sdecs sdecs; print "\n";
		       [])
		    | loop ((sdec as SDEC(l,DEC_CON(v,_,_)))::rest) = (bound v; sdec::(loop rest))
		    | loop ((sdec as SDEC(l,DEC_EXP(v,_)))::rest) = (bound v; sdec::(loop rest))
		    | loop ((sdec as SDEC(l,DEC_EXCEPTION(_,_)))::rest) = (sdec::(loop rest))
		    | loop ((sdec as SDEC(l,DEC_MOD(v,b,s)))::rest) = 
		      (bound v; 
		       if eq_label(l, curl) 
			   then (case (reduce_signat ctxt s) of
				     SIGNAT_STRUCTURE(_, sdecs) => 
					 (SDEC(l,DEC_MOD(v,b,SIGNAT_STRUCTURE
							 (NONE, dosig restl sdecs))))::rest
				   | s => (error_region();
					   print "signature not reduced to structure signature";
					   pp_signat s;
					   sdec::(loop rest)))
		       else sdec::(loop rest))
	      in loop sdecs
	      end		
      in case (follow_labels (NONE,orig_sdecs,ctxt) lbls) of 
	  ABSTRACT lbls => dosig lbls orig_sdecs
	| CONCRETE lbls => (error_region();
			       print "cannot where type CONCRETE type component:";
			       pp_lpath lbls;
			       print "\n";
			       orig_sdecs)
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
		       ABSTRACT labs => 
			 (case (Sdecs_Lookup' context (mjunk,sdecs2,plabs)) of
			      SOME(_,PHRASE_CLASS_CON(c,_)) => 
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
	let fun find_sdec l [] = NONE
	      | find_sdec l ((SDEC(l',DEC_MOD(_,_,s)))::rest) = 
	            if (eq_label(l,l')) then SOME s else find_sdec l rest
	      | find_sdec l (_::rest) = find_sdec l rest
	    fun find_sig [] s = SOME s
	      | find_sig (l::ls) (SIGNAT_STRUCTURE(_,sdecs)) = (case find_sdec l sdecs of
								    SOME s => find_sig ls s
								  | NONE => NONE)
	      | find_sig labs (s as (SIGNAT_VAR v)) = find_sig labs (reduce_signat context s)
	      | find_sig _ _ = NONE
	in  (case find_sig labs1 (SIGNAT_STRUCTURE(NONE,sdecs)) of
		 SOME (s as SIGNAT_VAR _) =>
		     if (IlStatic.Sig_IsSub(context,sig2,s))
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
		  fun traverse (SDEC(l,DEC_CON (_,_,_))) = [[l]]
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




  and xsig_sharing_type(ctxt,sdecs,path) : sdecs = 
      let val mjunk = MOD_VAR(fresh_named_var "mjunk_sharing_type")
	  fun path2label p = 
	      (case (Sdecs_Lookup' ctxt (mjunk,sdecs,p)) of
		   SOME(l,_) => l
		 | NONE => (error_region();
			    print "sharing type got non-existent component ";
			    pp_lpath p; print " from sdecs\n";
			    pp_sdecs sdecs; print "\n";
			    raise SharingError))
      in  
	  let val labels = map path2label path
	      val labels : typeslot list = map (fn l => case (follow_labels (NONE,sdecs,ctxt) l) of 
				ABSTRACT lbls => ABSTRACT lbls
			      | CONCRETE _ => raise SharingError) labels
	  in xsig_sharing_rewrite(ctxt,sdecs)(labels,sdecs)
	  end
      handle SharingError => sdecs
      end


    (* --------------------------------------------------------- 
      ------------------ COERCION COMPILATION-------------------
      --------------------------------------------------------- *)
    type polyinst = context * sdec list -> sbnds * sdecs * con list


    (* Given a signature orig_sig named by var_actual, change orig_sig
      by replacing all projections from var_actual with internal variables
      of orig_sig.  For the remaining projections, change the path head
      variable to local_label and return a list of pairs of these projections.
      Each pair contains the labels part of the projection and the new path. *)

    fun internalize(orig_sig, var_actual, var_local, label_local) : signat * (label list * path) list = 
	let
	    (* First we replace paths with internal variables. *)
	    fun traverse (path,mapping) arg_sig = 
		(case arg_sig of
		     (SIGNAT_FUNCTOR _) => sig_substconmod(arg_sig,mapping)
		   | (SIGNAT_STRUCTURE(popt,sdecs)) => 
			 SIGNAT_STRUCTURE(popt,traverse_sdecs (path,mapping) sdecs)
		   | (SIGNAT_INLINE_STRUCTURE{code,abs_sig,self}) =>
			 let val MOD_STRUCTURE code = mod_substconmod(MOD_STRUCTURE code,mapping)
			 in  SIGNAT_INLINE_STRUCTURE{code=code,
						     abs_sig=traverse_sdecs (path,mapping) abs_sig,
						     self=self}
			 end)
	    and traverse_sdecs (path,mapping) sdecs = 
		let fun folder (SDEC(l,DEC_EXP(v,c)),m) = (SDEC(l,DEC_EXP(v,con_substconmod(c,m))),m)
		      | folder (SDEC(l,DEC_EXCEPTION _),_) = error "got DEC_EXCEPTION"
		      | folder (SDEC(l,DEC_CON(v,k,copt)),m) = 
		          let val p = join_path_labels(path,[l])
			      val m' = con_addmap(m,p,CON_VAR v)
			  in  (SDEC(l,DEC_CON(v,kind_substconmod(k,m),
					      (case copt of
						   NONE => NONE
						 | SOME c => SOME(con_substconmod(c,m))))), m')
			  end
		      | folder (SDEC(l,DEC_MOD(v,b,s)),m) = 
		          let val p = join_path_labels(path,[l])
			      val s = traverse (p,m) s
			      val m' = mod_addmap(m,p,MOD_VAR v)
			  in  (SDEC(l,DEC_MOD(v,b,s)),m')
			  end
		    val (sdecs,m) = foldl_acc folder mapping sdecs
		in  sdecs
		end

	    val internal_sig = traverse (PATH (var_actual,[]), empty_mapping) orig_sig

	    (* Now we compute the remaining paths and replace them *)
	    val neededpaths = ref ([] : (label list * path) list)
	    fun labels2path_local labs = PATH (var_local,labs)
	    fun add_path p = (case (assoc_eq(eq_labs, p, !neededpaths)) of
				  SOME p' => p'
				| NONE => let val p' = (labels2path_local p)
					  in neededpaths := (p,p') :: (!neededpaths); p'
					  end)


	    fun chandle (CON_MODULE_PROJECT (m,l)) = 
		let fun loop labs (MOD_VAR v) = if (eq_var(v,var_actual))
						    then SOME(path2con(add_path labs))
						else NONE
		      | loop labs (MOD_PROJECT(m,l)) = loop (l::labs) m 
		      | loop labs _ = NONE
		in  loop [l] m
		end
	      | chandle _ = NONE
	    fun sdec_help (p as PATH(v,labs)) = if (eq_var(v,var_actual)) then add_path labs else p
	    fun sdec_handle (SDEC(l,DEC_MOD(v,b,SIGNAT_STRUCTURE(SOME p,sdecs)))) = 
		let val s = SIGNAT_STRUCTURE(SOME (sdec_help p), sdecs)
		in  SOME(SDEC(l,DEC_MOD(v,b,do_sig s)))
		end
	      | sdec_handle (SDEC(l,DEC_MOD(v,b,SIGNAT_INLINE_STRUCTURE{self=SOME p,code,
								      abs_sig}))) = 
		let 
		    val s = SIGNAT_INLINE_STRUCTURE{self=SOME (sdec_help p),code=code,
						    abs_sig=abs_sig}
		in  SOME(SDEC(l,DEC_MOD(v,b,do_sig s)))
		end
	      | sdec_handle _ = NONE
	    and do_sig s = sig_all_handle(s, fn _ => NONE, chandle, 
					  fn _ => NONE, sdec_handle)

	    val final_sig = do_sig internal_sig
	    val neededpaths = !neededpaths
	    val _ = if (!debug)
			then (print "extract_hidden: there are ";
			      print (Int.toString (length neededpaths)); 
			      print " paths:\n";
			      app (fn (labs,_) => (pp_lpath labs;
						   print "\n")) neededpaths;
			      print "\n\n")
		    else ()
	in   (final_sig, neededpaths)
	end 
    
    
    (* Augment coerced_mod and coerced_sig so that all references to
      var_actual are removed from coerced_sig.  *)
    fun extract_hidden(context,coerced_mod, coerced_sig, var_actual, sig_actual) : mod * signat = 
	let 
	    val var_local = fresh_named_var "hidden_module"
	    val label_local = internal_label "hidden_module"
	    fun labels2path_actual labs = PATH(var_actual, labs)
	    val (coerced_sig, neededpaths) = internalize(coerced_sig, var_actual, 
							 var_local, label_local)

	    val _ = if (!debug)
		then (print "extract_hidden: internalized signature is\n ";
		      pp_signat coerced_sig; print "\n")
		    else ()
            (* we create the augmentation module and name it var_local; 
	      note that only the label of the var_local must be hidden;
	      for now, we copy all type components *)
	    fun dosdec (p,mapping : mapping, sdec) = 
		case sdec of
		    SDEC(l,DEC_CON(v,k,copt)) => 
			let val v' = derived_var v
			    val k' = kind_substconmod(k,mapping)
			    val copt' = Util.mapopt (fn c => con_substconmod(c,mapping)) copt
			    val p' = labels2path_actual(rev(l::p))
			    val c' = path2con p'
			    val mapping = con_addmap(mapping,PATH (v,[]),CON_VAR v')
			    val mapping = (case p of
					       [] => con_addmap(mapping,p',CON_VAR v')
					     | _ => mapping)
			in  SOME(mapping,
				 SBND(l,BND_CON(v',c')),
				 SDEC(l,DEC_CON(v',k',copt')))
			end
		  | SDEC(l,DEC_EXP(v,c)) => 
			if (is_eq l)
			    then
				let val v' = derived_var v
				    val e = path2exp(labels2path_actual(rev(l::p)))
				    val c' = con_substconmod(c,mapping)
				in  SOME(mapping,
					 SBND(l,BND_EXP(v',e)),
					 SDEC(l,DEC_EXP(v',c')))
				end
			else NONE
		  | SDEC(l,DEC_MOD(v,b,s)) => 
			(case dosig(l::p, mapping, s) of
			     SOME(_,m,s) => let val v' = derived_var v
					      val mapping = mod_addmap(mapping, PATH(v,[]), MOD_VAR v')
					      val p' = labels2path_actual[l]
					      val mapping = 
						  (case p of
						       [] => mod_addmap(mapping,p',MOD_VAR v')
						     | _ => mapping)
					  in  SOME(mapping,
						   SBND(l,BND_MOD(v',b,m)), 
						   SDEC(l,DEC_MOD(v',b,s)))
					  end
			   | NONE => NONE)
		  | _ => NONE
	    and dosdecs (p,mapping,[]) = (mapping,[],[])
	      | dosdecs (p,mapping,sdec::rest) = 
		let val (mapping,sbsdopt) = (case dosdec(p,mapping,sdec) of
					   NONE => (mapping, NONE)
					 | SOME(mapping,sbnd,sdec) => (mapping,SOME(sbnd,sdec)))
		    val (mapping,restsbnds,restsdecs) = dosdecs(p,mapping,rest)
		in  (case sbsdopt of
			 NONE => (mapping,restsbnds,restsdecs)
		       | SOME(sbnd,sdec) => (mapping,sbnd::restsbnds, sdec::restsdecs))
		end
            and dosig (p, mapping, s) =
		(case (reduce_signat context s) of
		     SIGNAT_STRUCTURE(_,sdecs) =>
			 let val (mapping,sbnds,sdecs) = dosdecs(p,mapping,sdecs)
			 in  SOME(mapping,MOD_STRUCTURE sbnds, SIGNAT_STRUCTURE(NONE,sdecs))
			 end
		   | SIGNAT_INLINE_STRUCTURE{abs_sig=sdecs,...} =>
			 let val (mapping,sbnds,sdecs) = dosdecs(p,mapping,sdecs)
			 in  SOME(mapping,MOD_STRUCTURE sbnds, SIGNAT_STRUCTURE(NONE,sdecs))
			 end
		   | _ => NONE)
	in
	    if (length neededpaths = 0)
		then (coerced_mod, coerced_sig)
	    else let
		     val (mapping,augment_m,augment_s) = 
			 (case dosig([],empty_mapping,sig_actual) of
			      SOME mapmodsig => mapmodsig
			    | NONE => (print "sig_actual is not a structure\n";
				       elab_error "sig_actual is not a structure"))
(*		     val coerced_sig = sig_substconmod(coerced_sig,mapping) *)
		     val sbnd_augment = SBND(label_local,BND_MOD(var_local, false, augment_m))
		     val sdec_augment = SDEC(label_local,DEC_MOD(var_local, false, augment_s))
		     val res as (m,s) = case (coerced_mod, coerced_sig) of
			 (MOD_STRUCTURE sbnds, SIGNAT_STRUCTURE (popt,sdecs)) =>
			     (MOD_STRUCTURE (sbnd_augment::sbnds), 
			      SIGNAT_STRUCTURE (popt,
						sdec_augment::sdecs))
		       | _ => 
			  let val _ = print "WARN: extract_hidden given coerced_mod/sig not strutures\n"
			      val l = to_open(fresh_internal_label "internal_mod")
			      val v = fresh_named_var "internal_mod"
			      val sbnd = SBND(l,BND_MOD(v,false,coerced_mod))
			      val sdec = SDEC(l,DEC_MOD(v,false,coerced_sig))
			  in  (MOD_STRUCTURE [sbnd_augment,sbnd],
			       SIGNAT_STRUCTURE(NONE,[sdec_augment,sdec]))
			  end
			     
		 in  res
		 end
	end


       (* ---- coercion of a poly component to a mono/poly specification --- *)
       fun polyval_case (ctxt : context, polyinst)
	   {name : var, (* use this name for the final binding *)
	    path : path, (* this path is to the actual polymorphic component *)
	    varsig_option : (var * signat) option, (* indicates whether spec is poly *)
	    spec_con : con, (* the type of the specification *)
	    actual_sig : signat} (* the signature of the component *)
		  : bool * (bnd * dec * context) option = 
	   let 
	       fun local_error () =
		   (error_region();
		    print "Coercion of a polymorphic value component to a\n";
		    error_region();
		    print "  monomorphic/polymorphic specification failed at ";
		    pp_path path;
		    print "\n"; 
		    error_region();
		    print "Expected type: ";
		    (case varsig_option of
			 NONE => pp_con spec_con
		       | SOME (v,s) => pp_signat (SIGNAT_FUNCTOR(v,s,
						  SIGNAT_STRUCTURE(NONE,[(SDEC(it_lab,
									  DEC_EXP(name,spec_con)))]),
						  PARTIAL)));
		    print "\n";
		    error_region();
		    print "Actual type: ";
		    pp_signat actual_sig;
		    print "\n";
		    (true,NONE))
		   
	       val SIGNAT_FUNCTOR(var_poly,
				  sig_poly as SIGNAT_STRUCTURE (NONE,
								sig_poly_sdecs),
				  SIGNAT_STRUCTURE(NONE,[SDEC(maybe_it_lab,
							      DEC_EXP(vp,con'))]), _) = actual_sig
		   
	       val ctxt' =  add_context_mod'(ctxt,var_poly,
					     SelfifySig ctxt (PATH (var_poly,[]), sig_poly))
	       val ctxt' = 
		   (case varsig_option of
			NONE => ctxt'
		      | SOME(v1,s1) => add_context_mod'(ctxt',v1,
							SelfifySig ctxt' (PATH (v1,[]), s1)))

	       val (sbnds_poly,sdecs_poly,_) = 
		   polyinst(ctxt',sig_poly_sdecs)
		   handle e => (print "polyval call to polyinst failed\n";
				pp_sdecs sig_poly_sdecs; print "\n";
				raise e)

	       (* can't use con instead of con'' *)
	       fun folder (SBND(l,BND_CON(v,c)),csubst) =
		   ((CON_MODULE_PROJECT(MOD_VAR var_poly, l), c)::csubst)
		 | folder (_,csubst) = csubst
	       val csubst = foldl folder [] sbnds_poly
	       fun chandle c = assoc_eq(eq_cpath,c,csubst)
	       fun ehandle _ = NONE : exp option
	       fun mhandle _ = NONE : mod option
	       fun sdechandle _ = NONE : sdec option
	       val con'' = con_all_handle(con', ehandle,chandle,mhandle,sdechandle)

	   in  case varsig_option of
	       NONE => 
		   let val mtemp = MOD_APP(path2mod path,MOD_STRUCTURE sbnds_poly)
		   in if (sub_con(ctxt',con'',spec_con))
			  then 
			      (true,
			       SOME(BND_EXP(name,MODULE_PROJECT(mtemp,it_lab)),
				    DEC_EXP(name,con''),
				    ctxt))
		      else local_error()
		   end
	     | SOME (v1,s1) => 
		   let 
		       val mtemp = MOD_APP(path2mod path,MOD_STRUCTURE sbnds_poly)
		       val inner_sig = SIGNAT_STRUCTURE(NONE,
							[SDEC(it_lab,
							      DEC_EXP(fresh_var(),con''))])
		       val s2 = SIGNAT_FUNCTOR(v1,s1,inner_sig,TOTAL)
		   in  if (sub_con(ctxt',con'',spec_con))
			   then 
			       let val (reduced,m) = eta_contract(MOD_FUNCTOR(TOTAL,v1,s1,mtemp,inner_sig))
			       in  (not reduced,
				    SOME(BND_MOD(name, true, m), DEC_MOD(name, true, s2), ctxt))
			       end
		       else local_error()
		   end
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

    and xcoerce_help (polyinst : polyinst,
		       context : context,
		       path_actual : path,
		       sig_actual : signat,
		       sig_target : signat) : (bool * Il.mod * Il.signat) = 
	let val match = Sig_IsSub(context,sig_actual,sig_target)
	    val _ = (print "xocerce_help on "; pp_path path_actual;
		     print "  match = "; print (Bool.toString match); print "\n")
	in  if match
		then (false,path2mod path_actual,sig_actual)
	    else xcoerce_help'(polyinst,context,path_actual,sig_actual,sig_target)
	end

    and xcoerce_help' (polyinst : polyinst,
		      context : context,
		      path_actual : path,
		      sig_actual : signat,
		      sig_target : signat) : (bool * Il.mod * Il.signat) = 
      let 

	  val sig_actual = reduce_signat context sig_actual
	  val sig_target = reduce_signat context sig_target
	  val coerced = ref false

	  fun makecon(path,labs) = path2con(join_path_labels(path,labs))

	  val _ =  if (!debug)
		       then (print "trying to xcoerce with path_actual = \n";
			     pp_path path_actual;
			     print " and signat_actual = \n";
			     pp_signat sig_actual; 
			     print "\nand signat_target = \n";
			     pp_signat sig_target;
(*			     print "\nand ctxt = \n"; pp_context context;  *)
			     print "\n")
		   else ()

	  local
	      fun sig_inline_lookup (p,s) lbls : (label list * phrase) option = 
		  (case s of
		       SIGNAT_STRUCTURE (_,self_sdecs) => NONE
		     | SIGNAT_INLINE_STRUCTURE {code,...} =>
			   Sbnds_Lookup context (code, lbls)
		     | SIGNAT_FUNCTOR _ => NONE)
	      fun sig_lookup (p,s) lbls : (label list * phrase_class) option = 
		  (case s of
		       SIGNAT_STRUCTURE (_,self_sdecs) =>
			   Sdecs_Lookup' context (path2mod p, self_sdecs, lbls)
		     | SIGNAT_INLINE_STRUCTURE {abs_sig = self_sdecs,...} =>
			   Sdecs_Lookup' context (path2mod p, self_sdecs, lbls)
		     | SIGNAT_FUNCTOR _ => NONE)
	      fun find lbl [] : dec option = NONE
		| find lbl (SDEC(l,d)::rest) = 
		  if (eq_label(l,lbl))
		      then SOME d
		  else (case (is_open l, d) of
			    (true, DEC_MOD(_,_,s)) =>
				(case s of
				     SIGNAT_STRUCTURE(_,sdecs) =>
					 (case find lbl sdecs of
					      NONE => find lbl rest
					    | decopt => decopt)
				   | SIGNAT_INLINE_STRUCTURE{abs_sig=sdecs,...} =>
					      (case find lbl sdecs of
						   NONE => find lbl rest
						 | decopt => decopt)
				   | _ => find lbl rest)
			  | _ => find lbl rest)
 
          fun dec_lookup s lbls = 
	      let val sdecs = (case s of
				   (SIGNAT_FUNCTOR _) => NONE
				 |  SIGNAT_STRUCTURE (_,sdecs) => SOME sdecs
				 | SIGNAT_INLINE_STRUCTURE{abs_sig=sdecs,...} => SOME sdecs
				 | _ => NONE)
	      in  (case (lbls,sdecs) of
		       (_,NONE) => NONE
		     | ([],_) => NONE
		     | (lbl::rest,SOME sdecs) => let val decopt = find lbl sdecs
						 in  case (rest,decopt) of
						     ([],SOME dec) => SOME dec
						   | (_,SOME(DEC_MOD(_,_,s))) => dec_lookup s rest
						   | _ => NONE
						 end)
	      end
	      val var_target = fresh_named_var "xcoerce_var_target"
	      val sig_actual_self = SelfifySig context (path_actual, sig_actual)
	  in  val dec_lookup = dec_lookup
	      val sig_actual_lookup = sig_lookup (path_actual,sig_actual_self)
	      val sig_inline_actual_lookup = sig_inline_lookup (path_actual,sig_actual_self)
	      val dec_actual_lookup = dec_lookup sig_actual
	      val dec_target_lookup = dec_lookup sig_target
	  end


		  
	fun doit ctxt labs : (bnd * dec * context) option = 
	  let 
	      val _ = if (!debug)
			  then (print "\n\ndoit called with labs = ";
				pp_lpath labs;
				print "\n\n\n")
		      else ()


		fun general_mod v =
		(case (sig_actual_lookup labs) of
		     SOME(lbls,PHRASE_CLASS_MOD (_,b,s1)) =>
		     let 
			 val s = (case (is_open (hd lbls), dec_target_lookup labs, sig_target) of
				      (_,SOME(DEC_MOD (_,_,s)),_) =>  s
				    | (true,_,SIGNAT_STRUCTURE(popt,sdecs1)) =>
					  SIGNAT_STRUCTURE(popt,
					  List.filter (fn(SDEC(l,_)) =>
						 case dec_lookup s1 [l] of
						       NONE => false
						     | SOME _ => true) sdecs1)
				    | _ => SIGNAT_STRUCTURE(NONE,[]))
			 val (inner_coerced,mbody,sig_ret) = 
			     xcoerce_help(polyinst,ctxt,
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
		     (SOME(DEC_EXP(v,c)),_) =>
			 (case (sig_actual_lookup labs) of
			      SOME(lbls,PHRASE_CLASS_EXP (_,con)) => (* con has var_actual *)
				  let val _ = (debugdo (fn () => 
					    (print "Looking up with path_actual = "; 
					     pp_path path_actual; 
					     print " with labs = "; pp_lpath labs;
					     print "\nand sig_actual = "; pp_signat sig_actual;
					     print "\ngot back "; pp_con con; print "\n")))
				  in
				    if (sub_con(ctxt,con,c))
				    then
					let 
					    val v' = derived_var v
					    val exp_path = join_path_labels(path_actual,lbls)
					    val bnd = BND_EXP(v',path2exp exp_path)
					    val dec = DEC_EXP(v',con)
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
				  end
			    | SOME(lbls,PHRASE_CLASS_MOD (_,_,s)) => 
				  let val name = derived_var v
				      val path = join_path_labels(path_actual,lbls)
				      val (coerced',result) = 
					  polyval_case (ctxt, polyinst)
					  {name = derived_var v,
					   path = path,
					   varsig_option = NONE,
					   spec_con = c,
					   actual_sig = s}
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
				   [SDEC(maybe_it1,DEC_EXP(_,c1))]),_),
			      SIGNAT_FUNCTOR(v2,s2,SIGNAT_STRUCTURE (NONE,
			           [SDEC(maybe_it2,DEC_EXP(_,c2))]),_)) =>
			       if (eq_label (maybe_it1, it_lab) andalso
				   eq_label (maybe_it1, it_lab))
				   then  
				       let val (coerced',result) = 
					       polyval_case (ctxt,polyinst)
					       {name = derived_var v,
						path = join_path_labels(path_actual,lbls),
						varsig_option = SOME(v1,s1),
						spec_con = c1,
						actual_sig = ss2}
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
         	   | (SOME(DEC_MOD(v,_,s)),_) => (error_region();
						print "module specification but non-module component\n";
						NONE)

		   (* ------- coercion of a type component to a type spec ---- *)
		   | (SOME(DEC_CON(v,_,copt)),actual_decopt) =>
			 (case (sig_actual_lookup labs) of
			      SOME(lbls,PHRASE_CLASS_CON (con,k)) => 
				  (* con is typically a path from var_actual *)
				  let
				      val v' = derived_var v
				      val ctxt = add_context_dec(ctxt,DEC_CON(v,k,SOME con))
				      fun check spec_con = 
					  if (sub_con(ctxt,con,spec_con))
					      then ()
					  else (let val con' = con_normalize(ctxt,con)
						    val spec_con' = con_normalize(ctxt,spec_con)
						in  error_region();
						    print "coercion of a type component to a ";
						    print "type specification failed at ";
						    pp_lpath labs;
						    print "\nExpected type: ";
						    pp_con spec_con;
						    print "\nActual type: ";
						    pp_con con;
						    print "\nReduced Expected type: ";
						    pp_con spec_con';
						    print "\nReduced Actual type: ";
						    pp_con con';
						    print "\n"
					       end)
				      val _ = (case copt of 
						   NONE => ()
						 | SOME spec_con => check spec_con)
				      val bnd = BND_CON(v',con)
				      val dec = DEC_CON(v',k,SOME con)
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

	fun sdecs_loop lbls (ctxt,sdecs) : sbnd list * sdec list =
	    let fun loop (ctxt,[]) = ([],[])
		  | loop (ctxt,(SDEC(l,_))::rest) =
		     (case (doit ctxt (lbls @ [l])) of
			  SOME (resbnd,resdec,ctxt') =>
			      let val (sbnds,sdecs) = loop(ctxt',rest)
			      in ((SBND(l,resbnd))::sbnds, 
				  (SDEC(l,resdec))::sdecs)
			      end
			| NONE => loop(ctxt,rest))
	    in  loop (ctxt,sdecs)
	    end

	fun sbnds_loop lbls (ctxt,sdecs) sbnds_code =
	    let val (sbnds,sdecs) = sdecs_loop lbls (ctxt,sdecs)

		fun help lbl bnd = 
		    (case (bnd,sig_inline_actual_lookup [lbl]) of
			 (BND_EXP(v,_), SOME (_,PHRASE_EXP e)) => BND_EXP(v,e)
		       | (BND_CON(v,_), SOME (_,PHRASE_CON c)) => BND_CON(v,c)
		       | (BND_MOD(v,b,_), SOME(_,PHRASE_MOD m)) => BND_MOD(v,b,m)
		       | _ => error "sbnd_find: help failed")

		fun sbnd_find lbl [] = error "sbnd_find failed"
		  | sbnd_find lbl ((sbnd as SBND(l,bnd))::rest) 
				= if (eq_label(l,lbl)) 
				      then SBND(l,help lbl bnd (* bnd *) )
				  else sbnd_find lbl rest
		fun sdec_find lbl [] = error "sdec_find failed"
		  | sdec_find lbl ((sdec as SDEC(l,dec))::rest) = 
				if (eq_label(l,lbl)) then SDEC(l,dec)
					else sdec_find lbl rest
		val sbnds_code = map (fn (SDEC(l,_)) => sbnd_find l sbnds_code) sdecs
	    in  (sbnds,sbnds_code,sdecs)
	    end

			
	val (m,s) =
	    (case (sig_actual,sig_target) of
		       (SIGNAT_FUNCTOR(v1,s1,s1',a1), 
			SIGNAT_FUNCTOR(v2,s2,s2',a2)) =>
			 let 
			   val _ = (print "coerced set to true because of functor coercion\n";
				    coerced := true)
			   val _ = if (a1 = a2) then () 
				   else raise (FAILURE "arrow mismatch in xcoerce")
			   val p2 = PATH(v2,[])
			   val (_,m3body,_) = xcoerce_help(polyinst,
							   add_context_mod'(context,v2,
									    SelfifySig context (p2, s2)),
							   p2,s2,s1)
			   val m4_arg = MOD_APP(path2mod path_actual, m3body)
			   val m4var = fresh_named_var "var_actual_xcoerce"
			   val p4 = PATH(m4var,[])
			   val (_,m4body,_) = xcoerce_help(polyinst,
							   add_context_mod'(context,m4var,
									    SelfifySig context (p4,s1')),
							   p4,s1',s2')
			   val m4body = mod_subst_modvar(m4body,[(m4var,m4_arg)])
			   val context' = add_context_mod'(context,v2,(SelfifySig context (p2,s2)))
			   val s = GetModSig(context',m4body)
			 in (MOD_FUNCTOR(a1,v2,s2,m4body,s),
			     SIGNAT_FUNCTOR(v2,s2,s,a1))
			 end
		   | (SIGNAT_FUNCTOR _, _) =>
			   (error_region();
			    print "cannot coerce a functor to a structure\n";
			    (MOD_STRUCTURE [], SIGNAT_STRUCTURE(NONE,[])))
		  | (_,SIGNAT_FUNCTOR _) => 
			   (error_region();
			    print "cannot coerce a structure to a functor\n";
			    (MOD_STRUCTURE [], SIGNAT_STRUCTURE(NONE,[])))
		   | (_,SIGNAT_STRUCTURE (_,sdecs)) =>
			 let val (sbnds,sdecs) = sdecs_loop [] (context,sdecs)
			 in (MOD_STRUCTURE sbnds,
			     SIGNAT_STRUCTURE (NONE, sdecs))
			 end
		   | (SIGNAT_STRUCTURE (NONE,sdecs),
			 SIGNAT_INLINE_STRUCTURE {self,code,abs_sig}) =>
			 let val (sbnds,sbnds_code,abs_sdecs) =
			     sbnds_loop [] (context,sdecs) code
			 in (MOD_STRUCTURE sbnds,
			     SIGNAT_INLINE_STRUCTURE {self = self,
						      code = sbnds_code,
						      abs_sig = abs_sdecs})
			 end
		   | (SIGNAT_INLINE_STRUCTURE{abs_sig=sdecs,...},
			 SIGNAT_INLINE_STRUCTURE {self,code,abs_sig}) =>
			 let val (sbnds,sbnds_code,abs_sdecs) =
			     sbnds_loop [] (context,sdecs) code
			 in (MOD_STRUCTURE sbnds,
			     SIGNAT_INLINE_STRUCTURE {self = self,
						      code = sbnds_code,
						      abs_sig = abs_sdecs})
			 end)


	val (asd,actual_str_length) = (case sig_actual of
				     SIGNAT_STRUCTURE(_,sdecs) => (sdecs,length sdecs)
				   | SIGNAT_INLINE_STRUCTURE{abs_sig=sdecs,...} => (sdecs,length sdecs)
				   | _ => ([],~1))
	val (tsd,target_str_length) = (case sig_target of
				     SIGNAT_STRUCTURE(_,sdecs) => (sdecs,length sdecs)
				   | SIGNAT_INLINE_STRUCTURE{abs_sig=sdecs,...} => (sdecs,length sdecs)
				   | _ => ([],~1))
	val match = (actual_str_length = target_str_length) andalso
			Listops.eq_list((fn (SDEC(l1,_),SDEC(l2,_)) => eq_label(l1,l2)), asd,tsd)

	val _ = if match then () 
		else 
		    (print "coerced set to true because of length/order mismatch\n";
		     coerced := true)

	val _ = if ((true orelse !debug) andalso not match) 
	        then (print "coerced set to true because of length/order mismatch\n";
		      print "actual_str_length = "; 
		      print (Int.toString actual_str_length); print "\n";
		      print "target_str_length = "; 
		      print (Int.toString target_str_length); print "\n";
		      if (!debug)
			then (print "actual_sdecs: "; 
			      app (fn SDEC(l,_) => (pp_label l; print "  ")) asd; print "\n";
			      print "target_sdecs: "; 
			      app (fn SDEC(l,_) => (pp_label l; print "  ")) tsd; print "\n")
		      else ())
		else ()


	val _ = if (!debug)
		    then (print "xcoerce_help with path_actual = ";
			  pp_path path_actual;
			  print " and coerced = ";
			  print (Bool.toString (!coerced));
			  print "\n")
		else ()

	val _ = if (!debug_full)
		    then (print "\nmodule:\n";
			  pp_mod m;
			  print "\nsig:\n";
			  pp_signat s)
		else ()
		    
      in if !coerced
	  then (true,m,s)
	 else (false,path2mod path_actual, sig_actual)
      end


    (* ---------- The exported signature coercion routines ------------ *)

    (* The returned module and signature will contain references to var_actual *)
    fun xcoerce_seal (polyinst : polyinst,
		      context : context,
		      var_actual : var,
		      sig_actual : signat,
		      sig_target : signat) : Il.mod * Il.signat =
	    let val (_,m,s) = 
		xcoerce_help(polyinst,
			     add_context_mod'(context,var_actual,
					      SelfifySig context (PATH (var_actual,[]), sig_actual)),
			     PATH (var_actual,[]),
			     sig_actual,sig_target)
	    in  (m,s)
	    end

    (* The resulting signature should not contain references to var_actual *)
    fun xcoerce_transparent (polyinst : polyinst,
			     context : context,
			     var_actual : var,
			     sig_actual : signat,
			     sig_target : signat) : Il.mod * Il.signat =
	let val _ = if (!debug)
			then (print "xcoerce_transparent just started... sig_actual = \n";
			      pp_signat sig_actual; 
			      if (!debug_full)
				  then (print "\nand sig_target = \n";
					pp_signat sig_target)
			      else ();
			      print "\n\n")
		    else ()
	    (* first call perform an opaque sealing for type-checking reasons *)
	    val context' = add_context_mod'(context,var_actual,
					    SelfifySig context (PATH (var_actual,[]), sig_actual))
	    val (coerced,coerced_mod,coerced_sig) = 
		xcoerce_help(polyinst,context',		
			     PATH (var_actual,[]),
			     sig_actual,sig_target)


	    val _ = if (!debug)
			then (print "xcoerce_transparent about to call extract_hidden..\n";
			      print  "with var_actual = ";
			      pp_var var_actual;
			      print "\nand coerced_sig = \n";
			      pp_signat coerced_sig; 
			      print "\n\n")
		    else ()

	    (* --- Now, we augment with enough additional hidden types and modules
	       so that we can make all occurrences of var_actual in sig_actual
	       disappear.  Note that coerced_sig does use its own
	       internal variables. *)

	    val (m,s) = extract_hidden(context',coerced_mod, coerced_sig, var_actual, sig_actual)

	    val _ = if (!debug)
			then (print "xcoerce_transparent returned with var_actual = ";
			      pp_var var_actual;
			      print " and with signature = \n";
			      pp_signat s;
			      print "\n\n")
		    else ()

	in  (m, s)
	end


end



