(* We box all floats and translate floating point operations accordingly.
   Thus, kind type is replaced by work types.
   Also note that all term-level (but not type-level) 
        record fields must be sorted by their labels. *)

functor Tonil(structure ArgIl : IL
	      structure ArgNil : NIL
              structure Ilutil : ILUTIL
              structure Nilutil : NILUTIL
	      structure NilError : NILERROR
              structure Ilcontext : ILCONTEXT
              structure IlStatic : ILSTATIC
              structure ArgNilcontext : NILCONTEXT
              structure Nilstatic : NILSTATIC
              structure Nilprimutil : PRIMUTIL
              structure Ppnil : PPNIL
              structure Ppil : PPIL
	      structure Subst : NILSUBST
               sharing ArgIl = Ilutil.Il = Ppil.Il = Ilcontext.Il = IlStatic.Il
               sharing Nilutil.Nil.Prim = Nilprimutil.Prim = ArgIl.Prim
	       sharing ArgNil = Nilutil.Nil = ArgNilcontext.Nil = Ppnil.Nil = 
		   Nilstatic.Nil = NilError.Nil
	       sharing type Nilstatic.context = ArgNilcontext.context
	       sharing type Nilprimutil.exp = Nilutil.Nil.exp
                   and type Nilprimutil.con = Nilutil.Nil.con
	       sharing type Subst.con = ArgNil.con
		   and type Subst.exp = ArgNil.exp
		   and type Subst.kind = ArgNil.kind
		   and type Subst.subst = ArgNilcontext.subst
             ) :(*>*) TONIL where Il = ArgIl 
                            and NilContext = ArgNilcontext =
struct
   structure Il = ArgIl
   structure NilContext = ArgNilcontext
   structure Nilcontext = ArgNilcontext
   structure Nil = ArgNil
   structure VarSet = Name.VarSet

   open Nil Listops
   val debug      = ref false
   val diag       = ref true
   val full_debug = ref false
   val trace      = ref false
   val do_memoize = ref true
   val use_imprecise_kind_at_bind = ref true
   val do_kill_cpart_of_functor = ref true
   val do_kill_dead_import = ref true

   val elaborator_specific_optimizations = ref true
   val optimize_empty_structure = ref true
   val omit_datatype_bindings = ref true
   val select_carries_types = Stats.bool "select_carries_types"
   fun error msg = Util.error "tonil.sml" msg

   val printl = Util.printl
   val lprintl = Util.lprintl
   val strip_var = Nilutil.strip_var
   val strip_arrow = Nilutil.strip_arrow
   val strip_singleton = Nilutil.strip_singleton
   val strip_record = Nilutil.strip_record
   val is_unit_c = Nilutil.is_unit_c
   val curry2 = Util.curry2


   val perr_c = NilError.perr_c
   val perr_e = NilError.perr_e
   val perr_k = NilError.perr_k
   val perr_c_k = NilError.perr_c_k
   val eq_label = Name.eq_label


   fun gt_label_pair ((l1,_),(l2,_)) = 
       (case Name.compare_label (l1,l2) of
	    GREATER => true
	  | _ => false)
   fun gt_label_pairpair (((l1,_),_),((l2,_),_)) = 
       (case Name.compare_label (l1,l2) of
	    GREATER => true
	  | _ => false)
   fun gt_label_triple ((l1,_,_),(l2,_,_)) = 
       (case Name.compare_label (l1,l2) of
	    GREATER => true
	  | _ => false)

   (* Variable-splitting mapping.  

      For the phase-splitting, we need a way of turning each module
      variable var into a new constructor variable var_c and a new
      term variable var_r.  Instead of messing around with "the
      internal number of the variable mod 2 or 3", we simply maintain a
      mapping var |-> (var_c, var_r) 
   *)
   local

       fun addToVmap (vmap, var, var_c, var_r) = 
	   Name.VarMap.insert (vmap, var, (var_c, var_r))

   in
       fun lookupVmap (var, vmap) = Name.VarMap.find (vmap, var)
	   

       val empty_vmap = Name.VarMap.empty

       fun printVmap vmap = 
	   Name.VarMap.appi (fn (k,(v1,v2)) => (Ppnil.pp_var k;
						print "=(";
						Ppnil.pp_var v1;
						print ",";
						Ppnil.pp_var v2;
						print ") ")) vmap
	   	   
       fun newSplit (var, vmap) = 
	   let
	       val var_name = Name.var2string var
	       val var_c = Name.fresh_named_var (var_name ^ "_c")
	       val var_r = Name.fresh_named_var (var_name ^ "_r")
	   in
	       (var_c, var_r, addToVmap(vmap, var, var_c, var_r))
	   end
       
       fun splitFreshVar vmap = 
	   let
	       val var = Name.fresh_var ()
	       val (var_c, var_r, vmap') = newSplit (var, vmap)
	   in
	       (var, var_c, var_r, vmap')
	   end
       
       fun splitVar (var, vmap) =
	   (case (lookupVmap (var, vmap)) of
		NONE => newSplit (var, vmap)
	      | SOME (var_c, var_r) => (var_c, var_r, vmap))
   end


   (* Catenable lists.

      In translating modules, we need to add new lists of bindings to
      the end of existing lists.  It's not clear yet how long these
      lists tend to get, but to avoid exponential behavior due to
      repeated appends, and to avoid lots of list-reverses to keep
      the bindings in reverse order, we use a simple implementation of
      catenable lists.  The idea is that we delay all appends until the
      very end.
    *)
      

   (* upto.  Given n, returns the list of integers 1..n *)
   fun upto n =
       let
	   fun loop i = if (i > n) then nil else i :: (loop (i+1))
       in
	   loop 1
       end


   (* makeLabels.  Returns a list of labels for a tuple of length n. *)
   fun makeLabels n = map Ilutil.generate_tuple_label (upto n)

   (* makeVars.  Returns a list of fresh variables of length n. *)
   fun makeVars n = map (Name.fresh_var o ignore) (upto n)

   (* makeKindTuple.  
         Creates the kind for a "tuple" of types of length n. 
         1-tuples yield kind Word, rather than a record kind.
    *)
   fun makeKindTuple 1 = Word_k Runtime
     | makeKindTuple n =
       let
	   fun makeField i =
	       ((Ilutil.generate_tuple_label i, Name.fresh_var()), 
		Word_k Runtime)
       in  
	   Record_k (Util.list2sequence (map makeField (upto n)))
       end

   (* makeLetC.
         Creates a constructor-level sequential let given bindings and
         a body.  Optimizes the let when the bindings or the body are
         especially simple:

         LET_C [] IN con                 ==>  con
         LET_C cvar = con IN cvar        ==>  con
         LET_C cvar' = con IN cvar       ==>  cvar  [where cvar != cvar']
         LET_C cbnds, cvar = con IN cvar ==>  makeLetC cbnds con
         LET_C cbnds IN (LET_C cbnds' IN con) ==> makeLetC (cbnds@cbnds') con
         LET_C cvar = con IN cvar.l      ==>  con.l
    *) 
   fun makeLetC nil body = body
     | makeLetC [conbnd as Con_cb(var,_,con)] (cv as Var_c var') =
       if (Name.eq_var(var,var')) then con else cv
     | makeLetC [conbnd as Con_cb(var,_,con)] (cv as Proj_c(Var_c var',l)) =
       if (Name.eq_var(var,var')) then Proj_c(con,l) else cv
     | makeLetC cbnds (cv as Var_c var') =
       (case (List.rev cbnds) of
	    Con_cb(var,_,con)::rest => 
		if (Name.eq_var(var,var')) then
		    makeLetC (rev rest) con
		else
		    Let_c (Sequential, cbnds, cv)
	  | _ => Let_c (Sequential, cbnds, cv))
     | makeLetC cbnds (Let_c(Sequential, cbnds', body)) =
          makeLetC (cbnds @ cbnds') body
     | makeLetC cbnds body = Let_c (Sequential, cbnds, body)

   (* makeLetE.
         Creates a term-level sequential let given bindings and
         a body.  Optimizes the let when the bindings or the body are
         especially simple:

         LET_E [] IN exp                    ==> exp
         LET_E bnds, evar = exp IN evar     ==> makeLetE bnds exp
         LET_E bnds IN (LET_E bnds' in EXP) ==> makeLetE (bnds@bnds') exp
         LET_E bnds, fixopen-LEAF foo formal = fnbody IN foo arg ==>
                 makeLetE (bnds, formal = arg) IN fnbody
    *)
   fun makeLetE nil body = body
     | makeLetE ebnds (Let_e(Sequential, ebnds', body)) =
          makeLetE (ebnds @ ebnds') body
     | makeLetE ebnds body =
       (case (List.rev ebnds, body) of
           (Exp_b(evar',_,exp)::rest, Var_e evar) => 
	       if (Name.eq_var(evar',evar)) then 
                  makeLetE (List.rev rest) exp
               else
		   Let_e(Sequential, ebnds, body)
         | (Fixopen_b fset :: rest, 
	    App_e(_,Var_e evar, con_args, exp_args, fp_args))=>
	       (case (Util.set2list fset) of
		    [(evar', 
		      Function(_,Leaf,vklist,vclist,fplist,fnbody,body_t))] => 
 		       if (Name.eq_var(evar',evar)) then
			   makeLetE ((List.rev rest) @ 
				     (createBindings(vklist, con_args,
						     vclist, exp_args,
						     fplist, fp_args)))
			   fnbody
		       else
			   Let_e(Sequential, ebnds, body)
		    | _ => Let_e(Sequential, ebnds, body))
	 | _ => Let_e(Sequential, ebnds, body))

   and createBindings (vklist, con_args, vclist, exp_args, fplist, fp_args) =
       let
	   val con_bnds = map (fn((cvar, knd), con) => Con_b (cvar, knd, con))
                              (Listops.zip vklist con_args)
           val exp_bnds = map (fn((evar, con), exp) => Exp_b (evar, con, exp))
	                      (Listops.zip vclist exp_args)
	   val float_type = Prim_c (Float_c Prim.F64, [])
           val fp_bnds = map (fn (evar, exp) => Exp_b (evar, float_type, exp))
	                     (Listops.zip fplist fp_args)
       in
           con_bnds @ exp_bnds @ fp_bnds
       end

   (* makeApply
         Creates an application.  Optimizes (beta-reduces) 
         certain simple cases:

         APP(LET bnds IN body, arg) ==> LET bnds IN (APP(body, arg))
           [when FV(args) and BV(bnds) are disjoint]
    *)
    fun makeAppE (fn_exp as Let_e (Sequential, bnds, exp)) cargs eargs fargs =
	let 
            fun addone(v1,fv) = VarSet.addList(fv,v1)
	    fun addpair((v1,v2),fv) = VarSet.addList(VarSet.addList(fv,v1),v2)

	    val fv = foldl addpair VarSet.empty 
		           (map (fn e => Nilutil.freeExpConVarInExp(true,e))  eargs)
	    val fv = foldl addpair fv (map (fn e => Nilutil.freeExpConVarInExp(true,e)) fargs)
	    val fv = foldl addone  fv (map (fn c => Nilutil.freeConVarInCon(true,c)) cargs)

	    fun vf_mem(v,_) = VarSet.member(fv,v)

	    fun bnd_check (Con_b(v,_,_)) = VarSet.member(fv,v)
	      | bnd_check (Exp_b(v,_,_)) = VarSet.member(fv,v)
	      | bnd_check ((Fixopen_b vfset) | (Fixcode_b vfset)) =
		Listops.orfold vf_mem (Util.set2list vfset)
	      | bnd_check (Fixclosure_b (_,vfset)) = 
		Listops.orfold vf_mem (Util.set2list vfset)

	    val intersect = Listops.orfold bnd_check bnds
	in  
	    if (intersect) then 
		App_e(Open, fn_exp, cargs, eargs, fargs)
	    else
		makeLetE bnds (makeAppE exp cargs eargs fargs)
	end
      | makeAppE fn_exp cargs eargs fargs = 
            App_e(Open, fn_exp, cargs, eargs, fargs)
            
   (* extractPathLabels.  Splits a module "mod.lbls" into
        the "mod" and a list of labels.
    *)
   fun extractPathLabels module =
       let
	   fun loop (Il.MOD_PROJECT(module, lbl), accum) =
	              loop (module, lbl :: accum)
             | loop (module, accum) = (module, accum)
       in
	   loop (module, nil)
       end




   (* xeffect.  
         Translates the total/partial distinction from HIL to MIL.
    *)
   fun xeffect (Il.TOTAL) = Total
     | xeffect (Il.PARTIAL) = Partial

   (* xilprim.  Translates the so-called "IL primitives" (primitives
       which are only segregated in the IL for typing reasons) into
       their corresponding primitives. 
    *)
   fun xilprim (Prim.eq_uint intsize)     = Prim.eq_int intsize
     | xilprim (Prim.neq_uint intsize)    = Prim.neq_int intsize
     | xilprim (Prim.not_uint intsize)    = Prim.not_int intsize
     | xilprim (Prim.and_uint intsize)    = Prim.and_int intsize
     | xilprim (Prim.or_uint intsize)     = Prim.or_int intsize
     | xilprim (Prim.xor_uint intsize)     = Prim.xor_int intsize
     | xilprim (Prim.lshift_uint intsize) = Prim.lshift_int intsize

   (* derefOneshot, derefTyvar, derefOvar.
        Partial functions to extract the values from "oneshot"-like
        objects.
    *)
   fun derefOneshot oneshot = 
       (case (Util.oneshot_deref oneshot) of
	     NONE   => error "(derefOneshot)  oneshot unset"
	  |  SOME x => x)

   fun derefTyvar tyvar =
       (case (Il.Tyvar.tyvar_deref tyvar) of
	    NONE => error "(derefTyvar)  tyvar unset"
          | SOME x => x)

   fun derefOvar ovar = derefTyvar (Il.Tyvar.ocon_deref ovar)

   (* selectFromKind.  Given a record kind and a list lbls of labels,
         produce the selected kind fields and remove all singeltons.
    *)
   fun selectFromKindStrip (k, lbls) = 
       let fun decompose (Record_k lvk_seq) = Util.sequence2list lvk_seq
	     | decompose _ = error "selectFromKind failed to get record kind"
	   fun loop k [] = Nilutil.kill_singleton k
	     | loop k (lbl::lbls) = 
	       let val lvk_list = decompose k
		   fun find [] = error "selectFromKind could not find field"
		     | find (((l,_),k)::rest) = if (Name.eq_label(l,lbl))
						    then k
						else find rest
	       in  loop (find lvk_list) lbls
	       end
       in  loop k (rev lbls)
       end

   (* selectFromCon.  Given con and a list lbls of labels,
         produce the constructor corresponding to con.lbls
    *)
   fun selectFromCon (con, lbls) = 
       let
	   fun loop [] = con
	     | loop (lbl::lbls) = Proj_c(loop lbls, lbl)
       in
	   loop (rev lbls)
       end

   (* selectFromRec.  Given a record expression r and a list lbls 
         of labels, produce the term corresponding to r.lbls
    *)
   fun selectFromRec (r,t,lbls) = 
     if !select_carries_types then
       let
	 fun loop [] = (r,t)
	   | loop (lbl::lbls) = 
	   let
	     val (r,t) = loop lbls
	     val (labels,cons) = 
	       case strip_record t
		 of SOME (labels,cons) => (labels,cons)
		  | NONE => error "Expected record type in selectFromRec"
	     val t = case find2 (fn (l,c) => eq_label (l,lbl)) (labels,cons)
		       of SOME (label,con) => con
			| NONE => error "Label not found in record projection"
	   in
	     (Prim_e(NilPrimOp(select lbl), cons, [r]),t)
	   end
	 val (r,t) = loop (rev lbls)
       in
	 r
       end
     else
       let
	 fun loop [] = r
	   | loop (lbl::lbls) = 
	   Prim_e(NilPrimOp(select lbl), [], [loop lbls])
       in
	 loop (rev lbls)
       end

   fun chooseName (NONE, vmap) = splitFreshVar vmap
     | chooseName (SOME (var,var_c,var_r), vmap) = (var, var_c, var_r, vmap)
       
   val count = ref 0

   fun lookupList eq lst k' =
       let
	   fun loop nil = NONE
	     | loop ((k,v)::xs) = if (eq (k,k')) then SOME v else loop xs
       in
	   loop lst
       end

   val w0 = TilWord32.fromInt 0

   fun Record_cc str labels = 
       if (Name.labels_sorted_distinct labels)
	   then Record_c labels
       else 
	   (print ("Record_cc failed from " ^ str ^ "\n");
	    app (fn l => (Ppnil.pp_label l; print "\n")) labels;
	    error ("Record_cc failed from " ^ str))



   (* xmod:  Translation of an IL module.

      {name_c, name_r, cbnd_cat, ebnd_cat, knd_c, type_r,
       valuable} =
         xmod ctx (il_mod, vmap_0, SOME var)

      Preconditions:  

        (1) var not free in ctx.

      Postconditions: 

        Let cbnds = flatten_catlist cbnd_cat, ebnds = flatten_catlist ebnd_cat

        (1) the compile-time part of mod is LET_C cbnds IN name_c END
        (2) the compile-time part has kind knd_c
        (3) the run-time part of mod is LET_E cbnds, ebnds IN name_r END
        (4) the run-time part has type LET_C cbnds IN type_r END
        (5) ctx |- il_signat == il_signat' : Sig
        (6) valuable <=> il_mod is valuable
    *)

   val xmod_count = ref 0
   val xsig_count = ref 0
   val xcon_count = ref 0
   val xexp_count = ref 0
   val xsdecs_count = ref 0
   val xsbnds_count = ref 0

local
   datatype splitting_context = CONTEXT of {NILctx : Nilcontext.context,
					    used   : (bool ref) Name.VarMap.map,
					    vmap   : (var * var) Name.VarMap.map,
					    convarmap : kind Name.VarMap.map,
					    alias  : (con * exp * kind * kind * con) Name.PathMap.map}


in
    type splitting_context = splitting_context
    fun make_splitting_context vmap = CONTEXT{NILctx = Nilcontext.empty(),
					      used = Name.VarMap.empty,
					      vmap = vmap,
					      convarmap = Name.VarMap.empty,
					      alias = Name.PathMap.empty}

   fun print_splitting_context (CONTEXT{NILctx,used,vmap,alias,convarmap}) = 
       (Name.VarMap.appi (fn (v,(vc,vr)) => (Ppnil.pp_var v; print "  -->  "; 
					     Ppnil.pp_var vc; print ", ";
					     Ppnil.pp_var vr; print "\n")) vmap;
	print "\n";
	Nilcontext.print_context NILctx;
	print "\n")

   fun filter_NILctx (CONTEXT{NILctx,used,...}) = (NILctx, used)
   fun vmap_of (CONTEXT{vmap,...}) = vmap

   fun nilcontext_find_con(CONTEXT{NILctx,used,...},v) = 
       let val res = Nilcontext.find_con(NILctx,v)
	   val _ = (case (res,Name.VarMap.find(used,v)) of
			(NONE,NONE) => ()
		      | (SOME _, SOME r) => r := true
		      | (SOME _, NONE) => error "exp variable in context but not in used list"
		      | (NONE, SOME _) => error "exp variable not in context but in used list")
       in  res
       end
		
   fun nilcontext_find_kind(CONTEXT{NILctx,used,...},v) = 
       let val res = Nilcontext.find_kind(NILctx,v)
	   val _ = (case (res,Name.VarMap.find(used,v)) of
			(NONE,NONE) => ()
		      | (SOME _, SOME r) => r := true
		      | (SOME _, NONE) => (print "con variable "; Ppnil.pp_var v;
					   print " in context but not in used list";
					   error "con variable not in used list")
		      | (NONE, SOME _) => (print "con variable "; Ppnil.pp_var v;
					   print " not in context but in used list";
					   error "con variable not in context but in used list"))
       in  res
       end
			    
		
   fun nilcontext_find_kind'(CONTEXT{convarmap,used,...},v) = 
       let val res = Name.VarMap.find(convarmap,v)
	   val _ = (case (res,Name.VarMap.find(used,v)) of
			(NONE,NONE) => ()
		      | (SOME _, SOME r) => r := true
		      | (SOME _, NONE) => (print "con variable "; Ppnil.pp_var v;
					   print " in context but not in used list";
					   error "con variable not in used list")
		      | (NONE, SOME _) => (print "con variable "; Ppnil.pp_var v;
					   print " not in context but in used list";
					   error "con variable not in context but in used list"))
       in  res
       end

   fun nilcontext_con_reduce(CONTEXT{NILctx,...},c) = Nilstatic.con_reduce(NILctx,c)
   fun nilcontext_con_valid(CONTEXT{NILctx,...},c) = Nilstatic.con_valid(NILctx,c)
   fun nilcontext_print(CONTEXT{NILctx,...}) = Nilcontext.print_context NILctx

   fun update_vmap  (CONTEXT{NILctx,used,vmap,alias,convarmap}, vmap') = 
       CONTEXT{NILctx=NILctx, used=used, vmap=vmap',alias=alias,convarmap=convarmap}

   fun update_NILctx_insert_con(CONTEXT{NILctx,vmap,used,alias,convarmap},v,c) = 
       let val NILctx' = Nilcontext.insert_con(NILctx, v, c)
	   val used' = Name.VarMap.insert(used,v,ref false)
       in  CONTEXT{NILctx=NILctx', vmap=vmap, used = used',alias=alias,convarmap=convarmap}
       end
   fun update_NILctx_insert_kind(CONTEXT{NILctx,vmap,used,alias,convarmap},v,k) = 
       let val NILctx' = Nilcontext.insert_kind(NILctx,v,k)
	   val used' = Name.VarMap.insert(used,v,ref false)
	   val convarmap' = Name.VarMap.insert(convarmap,v,k)
       in  CONTEXT{NILctx=NILctx', vmap=vmap, used = used', alias = alias,convarmap=convarmap'}
       end

   fun update_NILctx_insert_con_list(CONTEXT{NILctx,used,vmap,alias,convarmap},vclist) = 
       let val NILctx' = foldl (fn ((v,c),ctxt) => Nilcontext.insert_con(ctxt, v, c)) NILctx vclist
	   val used' = foldl (fn ((v,_),used) => Name.VarMap.insert(used,v,ref false)) used vclist
       in  CONTEXT{NILctx=NILctx', vmap=vmap, used=used',alias=alias,convarmap=convarmap}
       end


   fun update_NILctx_insert_kind_list(CONTEXT{NILctx,used,vmap,alias,convarmap},vklist) =
       let val NILctx' = foldl (fn ((v,k),ctxt) => Nilcontext.insert_kind(ctxt, v, k)) NILctx vklist
	   val used' = foldl (fn ((v,_),used) => Name.VarMap.insert(used,v,ref false)) used vklist
	   val convarmap' = foldl (fn ((v,k),m) => Name.VarMap.insert(m,v,k)) convarmap vklist
       in  CONTEXT{NILctx=NILctx', vmap=vmap, used=used',alias=alias,convarmap=convarmap'}
       end

       fun check_cbndpresent (CONTEXT{NILctx,...}) cbnd_cat = 
	  let val cbnds = flattenCatlist cbnd_cat
	      fun loop [] = true
                | loop ((v,_,_)::rest) = 
		(case (NilContext.find_kind(NILctx,v)) of
			NONE => (print "cbndpresent false because of ";
				Ppnil.pp_var v; print "\n"; false)
	 	 	| SOME _ => loop rest)
	  in  loop cbnds
	  end

       fun check_ebndpresent (CONTEXT{NILctx,...}) ebnd_cat = 
	  let val ebnds = flattenCatlist ebnd_cat
	      fun loop [] = true
                | loop ((Exp_b(v,_,_))::rest) = 
		(case (NilContext.find_con(NILctx,v)) of
			NONE => false
	 	 	| SOME _ => loop rest)
	  in  loop ebnds
	  end

   fun update_NILctx_cbndcat_help strict (CONTEXT{NILctx,used,vmap,alias,convarmap},cbnd_cat) = 
       let fun folder((v,k,c:con),(ctxt,used,cm)) = 
           let val insert = (strict orelse 
			     (case (Nilcontext.find_kind(ctxt,v)) of
				  NONE => true
				| SOME _ => (print "update_NILctx_cbndcat found variable already present\n"; false)))
           in  if insert
		   then (print "update_NILctx_bndcat_help inserting ";
			Ppnil.pp_var v; print "  --> \n"; Ppnil.pp_kind k;
			print "\n\n";
			(Nilcontext.insert_kind(ctxt,v,k), 
			Name.VarMap.insert(used,v,ref false),
			 Name.VarMap.insert(cm,v,k)))
	       else (ctxt,used,cm)
           end
           val cbnd_flat = flattenCatlist cbnd_cat
           val (NILctx',used',convarmap') =  foldl folder (NILctx,used,convarmap) cbnd_flat
       in  CONTEXT{NILctx=NILctx', vmap=vmap, used=used',alias=alias, convarmap=convarmap'}
       end

    fun update_NILctx_ebndcat_help strict (CONTEXT{NILctx,vmap,used,alias,convarmap},ebnd_cat) = 
       let fun folder' openness ((v,f), (ctxt, used,cm)) = 
             let val insert = (strict orelse 
			        (case (Nilcontext.find_con(ctxt,v)) of
				     NONE => true
				   | SOME _ => (print "update_NILctx_cbndcat found variable already present\n"; false)))
	     in if insert
		    then (Nilcontext.insert_con(ctxt, v, Nilutil.get_function_type openness f),
			  Name.VarMap.insert(used,v, ref false),
			  cm)
		else (ctxt, used, cm)
	     end
           fun folder(Exp_b(v,c,_),(ctxt,used,cm)) = 
               let val insert = (strict orelse
				 (case Nilcontext.find_con(ctxt,v) of
				      NONE => true
				    | SOME _ => (print "update_NILctx_ebndcat found variable alreadt present\n"; false)))
	       in  if insert
		       then (Nilcontext.insert_con(ctxt,v,c), 
			     Name.VarMap.insert(used, v, ref false), cm)
		   else (ctxt, used, cm)
	       end

             | folder(Con_b(v,k,c),(ctxt,used,cm)) = 
               let val insert = (strict orelse
				 (case Nilcontext.find_kind(ctxt,v) of
				      NONE => true
				    | SOME _ => (print "update_NILctx_ebndcat found variable alreadt present\n"; false)))
	       in  if insert 
                       then (NilContext.insert_kind(ctxt,v,#2 (Nilstatic.con_valid (ctxt, c))),
			     Name.VarMap.insert(used, v, ref false),
			     Name.VarMap.insert(cm,v,k))
                   else (ctxt,used,cm)
	       end
             | folder(Fixopen_b vfset, acc) = Util.foldset (folder' Open) acc vfset
             | folder(Fixcode_b vfset, acc) = Util.foldset (folder' Code) acc vfset
             | folder(Fixclosure_b vfset, acc) = raise Util.UNIMP
               
           val ebnd_flat = flattenCatlist ebnd_cat
           val (NILctx',used',convarmap') = foldl folder (NILctx,used, convarmap) ebnd_flat
       in  CONTEXT{NILctx=NILctx', vmap=vmap, used=used',alias=alias, convarmap=convarmap'}
       end


   val update_NILctx_cbndcat = update_NILctx_cbndcat_help true
   val update_NILctx_cbndcat' = update_NILctx_cbndcat_help false
   val update_NILctx_ebndcat = update_NILctx_ebndcat_help true
   val update_NILctx_ebndcat' = update_NILctx_ebndcat_help false

   local fun extract_path labs (Il.MOD_VAR v) = SOME(v,labs)
	   | extract_path labs (Il.MOD_PROJECT(m,l)) = extract_path (l::labs) m
	   | extract_path _ _ = NONE
   in    fun add_module_alias(CONTEXT{NILctx,vmap,used,alias,convarmap},m,name_c,name_r,k1,k2,c) = 
		case (extract_path [] m) of
		  SOME p => let val alias' = Name.PathMap.insert(alias,p,(name_c,name_r,k1,k2,c))
			    in  CONTEXT{NILctx=NILctx, vmap=vmap, 
					used=used,alias=alias', convarmap=convarmap}
			    end
		| NONE => error "add_module_alias given non-path module"

	 fun lookup_module_alias(CONTEXT{alias,...},m) = 
		case (extract_path [] m) of
		   NONE =>  error "lookup_module_alias given non-path module"
	 	 | SOME p => Name.PathMap.find(alias,p)
  end

end (* local defining splitting context *)

 
   fun projectFromRecord nilctxt con [] = con
     | projectFromRecord nilctxt (Prim_c(Record_c (lbl::lbls), con::cons)) (l' as (lbl'::lbls')) =
       if (Name.eq_label (lbl, lbl')) then 
	   projectFromRecord nilctxt con lbls'
       else
	   projectFromRecord nilctxt (Prim_c(Record_cc "1" lbls, cons)) l'
     | projectFromRecord nilctxt c labels =
	   (case nilcontext_con_reduce(nilctxt,c) of
		c as (Prim_c(Record_c _, _)) => projectFromRecord nilctxt c labels
	      | _ => 
		    (print "Error: bad projection from constructor ";
		     Ppnil.pp_con c;
		     print " and labels ";
		     app (fn l => (Ppnil.pp_label l; print " ")) labels;
		     print "\n";
		     error "projectFromRecord: bad projection"))

   local
       type con_result = con * kind * kind
       type mod_result = {cbnd_cat : (var * kind * con) catlist,
			  ebnd_cat : bnd catlist,
			  name_c : con,
			  name_r : exp,
			  knd_c : kind,
			  knd_c_context : kind,
			  context : splitting_context,
			  type_r : con,
			  vmap : (var * var) Name.VarMap.map,
			  valuable : bool}
       val con_memo = ref (Name.VarMap.empty : con_result Name.PathMap.map Name.VarMap.map)
       val mod_memo = ref (Name.VarMap.empty : mod_result Name.PathMap.map Name.VarMap.map)
   in
       fun reset_memo() = (con_memo := Name.VarMap.empty; mod_memo := Name.VarMap.empty)

       fun clear_memo v = (con_memo := (#1(Name.VarMap.remove(!con_memo,v))
					handle LibBase.NotFound => !con_memo);
			   mod_memo := (#1(Name.VarMap.remove(!mod_memo,v))
					handle LibBase.NotFound => !mod_memo))
	   

       fun lookup_con_memo (path as (v,lbls)) thunk =
	     (case Name.VarMap.find(!con_memo,v) of
		NONE => (con_memo := Name.VarMap.insert(!con_memo,v,Name.PathMap.empty);
			 lookup_con_memo path thunk)
	      | SOME pathmap =>
		   (case Name.PathMap.find(pathmap,path) of
			SOME result => result
		      | NONE => let val res = thunk()
				in  con_memo := Name.VarMap.insert(!con_memo,v,
								   Name.PathMap.insert(pathmap,path,res));
				    res
				end))

       fun augment_cbndcat cbnd_cat (from_var,to_var) = 
	   let val cbnds = flattenCatlist cbnd_cat
	       fun traverse((cbnd as (v,k,c))::rest) = 
		   if Name.eq_var(v,from_var)
		       then cbnd::(to_var,k,c)::rest
		   else cbnd::(traverse rest)
		 | traverse[] = error "augment_cbndcat could not find from_var"
	   in  LIST(traverse cbnds)
	   end

       fun augment_ebndcat ebnd_cat (from_var,to_var) = 
	   let val ebnds = flattenCatlist ebnd_cat
	       fun traverse((ebnd as Exp_b(v,c,e))::rest) = 
		   if Name.eq_var(v,from_var)
		       then ebnd::(Exp_b(to_var,c,e))::rest
		   else ebnd::(traverse rest)
		 | traverse[] = error "augment_ebndcat could not find from_var"
	   in  LIST(traverse ebnds)
	   end


       fun lookup_mod_memo (context,preferred_name) (path as (v,lbls)) thunk =
	     (case Name.VarMap.find(!mod_memo,v) of
		NONE => (mod_memo := Name.VarMap.insert(!mod_memo,v,Name.PathMap.empty);
			 lookup_mod_memo (context,preferred_name) (v,lbls) thunk)
	      | SOME pathmap =>
		   (case Name.PathMap.find(pathmap,path) of
			SOME result => 
			    let val {name_c,name_r,knd_c,knd_c_context,type_r,valuable,
				     ebnd_cat,cbnd_cat,context=_,vmap=_} = result
				(* if all the bindings are still present,
					then we can reuse the result *)
				val _ = if (check_cbndpresent context cbnd_cat)
					then ()
					else error "cbndpresent failed\n"
				val _ = if (check_ebndpresent context ebnd_cat)
					then ()
					else error "ebndpresent failed\n"
			 	val (name_c,name_r,cbnd_cat,ebnd_cat,context) = 
				    (case (name_c,name_r,preferred_name) of
					 (Var_c vc, Var_e vr, SOME(_,pc,pr)) =>
					     (if (Name.eq_var(vc,pc) andalso
						  Name.eq_var(vr,pr))
						then (name_c, name_r, LIST[], LIST[], context)
					 else
					  (Var_c pc, Var_e pr,
						LIST[(pc,knd_c,Var_c vc)],
						LIST[Exp_b(pr,type_r,Var_e vr)],
					   	update_NILctx_insert_con(
						update_NILctx_insert_kind(context,pc,
						knd_c_context),pr,type_r)))
					| _ => (name_c, name_r, LIST[], LIST[], context))

				val _ = (print "\nlookup_mod_memo of "; Ppnil.pp_var v;
				Ppnil.pp_list Ppnil.pp_label' lbls ("",".","",false);
				print " returning memoized result\n  knd_c = \n";
				Ppnil.pp_kind knd_c; print "\nand knd_c_context = \n";
				Ppnil.pp_kind knd_c_context; print "\n\n")
			    in  {cbnd_cat = cbnd_cat, ebnd_cat = ebnd_cat,
				 name_c = name_c, name_r = name_r,
				 knd_c = knd_c, knd_c_context = knd_c_context,
				 type_r = type_r, valuable = valuable, 
				 
				 (* these fields are not cached *)
				 vmap = vmap_of context, context = context} : mod_result
			    end
		      | NONE => let val res = thunk()
				val {knd_c, knd_c_context,...} = res
				val _ = (print "\nlookup_mod_memo of "; Ppnil.pp_var v;
				Ppnil.pp_list Ppnil.pp_label' lbls ("",".","",false);
				print " returning first-time result\n  knd_c = \n";
				Ppnil.pp_kind knd_c; print "\nand knd_c_context = \n";
				Ppnil.pp_kind knd_c_context; print "\n\n")
				in  mod_memo := Name.VarMap.insert(!mod_memo,v,
								   Name.PathMap.insert(pathmap,path,res));
				    res
				end)
			handle e => 
			(* some variable has gone out of scope! must clear the memo *)
			    (clear_memo v;
			     lookup_mod_memo (context,preferred_name) path thunk)
			)

   end

(* TO DO:
    precheck to drop all datatype-label components
*)

   fun projectKind c record_kind [] = record_kind
     | projectKind c record_kind (label::lbls) = 
	 let
	   val entry_kinds = 
	       (case (strip_singleton record_kind) of
		    Record_k kinds => Util.sequence2list kinds
		  | _ => error "projectKind did not get record_kind")

	   fun proj_kind (((l,v),k)::rest,subst) = 
	       if eq_label (l,label) then Subst.substConInKind subst k
	       else
		   proj_kind (rest,Subst.add subst (v,Proj_c (c,l)))
	     | proj_kind ([],subst) = error "projectKind could not project from kind"
	       
	   val kind = proj_kind (entry_kinds,Subst.empty())
	 in projectKind (Proj_c(c,label)) kind lbls
	 end
	
   fun generate_con_from_kind (k : kind) : con option = 
       (case k of
	    Arrow_k(openness,vklist,reck as Record_k seq) => 
		if null(Util.sequence2list seq)
		    then let val v = Name.fresh_named_var "generated_con"
			     val conbnd = Open_cb(v,vklist,Crecord_c[], reck)
			 in  SOME (Let_c(Sequential,[conbnd],Var_c v))
			 end
		else NONE
	  | _ => NONE)

   fun strip_kind (k : kind) : kind = 
       let open Nilutil
	   fun khandle(_,Record_k seq) = 
	       let val ls = Util.sequence2list seq
		   fun not_dead_arrow(v,k as Arrow_k(_,_,Record_k seq)) = if (null (Util.sequence2list seq))
									      then NONE
									  else SOME(v,strip_kind k)
		     | not_dead_arrow (v,k) = SOME(v,strip_kind k)
		   val ls = List.mapPartial not_dead_arrow ls
	       in  CHANGE_NORECURSE(Record_k(Util.list2sequence ls))
	       end 
	     | khandle _ = NOCHANGE
	   val handlers = (fn _ => NOCHANGE,
			   fn _ => NOCHANGE,
			   fn _ => NOCHANGE,
			   fn _ => NOCHANGE,
			   khandle)
       in kind_rewrite handlers k
       end

   fun xmod context (args as (il_mod, preferred_name)) =
       let
	   val this_call = ! xmod_count
	   val _ = 
	       if (!debug) then
		   (xmod_count := this_call + 1;
		    print ("\nCall " ^ (Int.toString this_call) ^ " to xmod\n");
		    if (!full_debug) then Ppil.pp_mod il_mod else ();
		    print"\n")
	       else ()

(*
	   fun check_proj(Il.MOD_VAR v,ls) = 
	       lookup_mod_memo (context,preferred_name) (v,ls) (fn()=>xmod' context args)
	     | check_proj(Il.MOD_PROJECT(m,l),ls) = check_proj(m,l::ls)
	     | check_proj _ = xmod' context args
*)
	   fun check_proj(Il.MOD_VAR v,[]) = xmod' context args
	     | check_proj(Il.MOD_VAR v, _) = 
		 (case (lookup_module_alias(context,il_mod)) of
			NONE => (print "--- lookup_module_alias failed to find ";
				Ppil.pp_mod il_mod; print "\n";
				xmod' context args)
		      | SOME(name_c,name_r,knd_c,knd_c_context,type_r) =>
			let val (name_c,name_r,cbnd_cat,ebnd_cat,context) = 
				(case preferred_name of 
				  NONE => (name_c,name_r,LIST[],LIST[],context)
				| SOME(_,pc,pr) => 
					let val context = update_NILctx_insert_kind(context,pc,knd_c_context)
					    val context = update_NILctx_insert_con(context,pr,type_r)
					in  (Var_c pc, Var_e pr, 
						LIST[(pc,knd_c,name_c)], LIST[Exp_b(pr,type_r,name_r)],
						context)
					end)
			in {cbnd_cat=cbnd_cat, ebnd_cat=ebnd_cat, name_c=name_c, name_r=name_r,
				context = context, knd_c=knd_c, knd_c_context=knd_c_context, type_r=type_r,
				vmap = vmap_of context, valuable=true}
			end)
	     | check_proj(Il.MOD_PROJECT(m,l),ls) = check_proj(m,l::ls)
	     | check_proj _ = xmod' context args
	   val result = (case (!do_memoize,args) of
			     (true,(Il.MOD_PROJECT(m,l),_)) => check_proj(m,[l])
			   | _ => xmod' context args)
	       handle e => (if (!debug) then (print ("Exception detected in call " ^ 
						    (Int.toString this_call) ^ " to xmod\n");
					      print "\nwith mod = \n";
					      Ppil.pp_mod il_mod;
(*
					      print "\nwith context = \n";
					      print_splitting_context context;
*)
					      print "\n")
			    else ();
			    raise e)
	in
	    if (!debug) then print ("Return " ^ (Int.toString this_call) ^ " from xmod\n") else ();
	    result
        end

   and preproject (var_arg, il_signat, context) = 
	let val _ =  print "preproject 1\n"
	    fun find_structure_paths m acc (Il.SIGNAT_STRUCTURE(popt,(Il.SDEC(l,Il.DEC_MOD (_,s)))::rest)) = 
		if (Ilutil.is_datatype_lab l)
		then find_structure_paths m acc (Il.SIGNAT_STRUCTURE(popt,rest))
		else	let val acc = (Il.MOD_PROJECT(m,l))::acc
			    val acc = find_structure_paths (Il.MOD_PROJECT(m,l)) acc s
			in  find_structure_paths m acc (Il.SIGNAT_STRUCTURE(popt,rest))
			end
	      | find_structure_paths m acc (Il.SIGNAT_STRUCTURE(popt,_::rest)) =
			    find_structure_paths m acc (Il.SIGNAT_STRUCTURE(popt,rest))
	      | find_structure_paths m acc _ = acc
	    val rev_paths : Il.mod list = find_structure_paths (Il.MOD_VAR var_arg) [] il_signat
	val _ =  (print "preproject 2: there are "; print (Int.toString (length rev_paths)); print " paths\n")
	    fun folder (mpath,(cbnds,ebnds,context)) = 
		let val _ = (print "Working on "; Ppil.pp_mod mpath; print "\n")
		    val {cbnd_cat,ebnd_cat,name_c,name_r,knd_c,
			 knd_c_context,context,type_r,valuable,vmap} = xmod context (mpath,NONE)
		    val cbnds = cbnd_cat::cbnds
		    val ebnds = ebnd_cat::ebnds
		    val context = add_module_alias(context,mpath,name_c,name_r,knd_c,knd_c_context,type_r)
		in  (cbnds,ebnds,context)
		end
	    val (rev_cbnds,rev_ebnds,context) = foldr folder ([],[],context) rev_paths
	val _ =  print "preproject 3\n"
	in  {cbnd_cat = APPEND (rev rev_cbnds), ebnd_cat = APPEND (rev rev_ebnds), context = context}
	end

   and xmod' context (il_mod as (Il.MOD_VAR var_mod), preferred_name) = 
       let
	   val (var_mod_c, var_mod_r, vmap') = splitVar (var_mod, vmap_of context)

           val _ = if (!full_debug)
		       then (print "About to look up :\n";
			     Ppnil.pp_exp (Var_e var_mod_r);
			     print " and ";
			     Ppnil.pp_con (Var_c var_mod_c);
			     print "\n")
		   else ()

           val knd_c = 
	     case nilcontext_find_kind' (context, var_mod_c) of
		 SOME knd => knd
		| _ => (print "Variable: "; Ppnil.pp_var var_mod_c;
			error "Constructor variable not found in context")

	   val generated_mod_c = generate_con_from_kind knd_c
	   val knd_c_real = strip_kind knd_c

           val type_r = 
	     case nilcontext_find_con (context, var_mod_r) of
		 SOME con => con
		| _ => (print "Variable: "; Ppnil.pp_var var_mod_r;
			error "Expression variable not found in context")

	   val (var_mod_c,cbnds,context) = 
	       (case generated_mod_c of
		    NONE => (var_mod_c,[],context)
		  | SOME c => let val var_mod_c = Name.derived_var var_mod_c
			      in  (var_mod_c,
				   [(var_mod_c, knd_c_real, c)],
				   update_NILctx_insert_kind(context,var_mod_c,knd_c))
			      end)

	   val (name_c, name_r) = 
	       (case preferred_name of
		    NONE => (Var_c var_mod_c, Var_e var_mod_r)
		  | SOME (_, name_c, name_r) => (Var_c name_c, Var_e name_r))

	   val (cbnd_cat, ebnd_cat, context) =
	       (case preferred_name of
		    NONE => (LIST cbnds, LIST nil, context)
		  | SOME (_, name_c, name_r) => 
			(LIST (cbnds@[(name_c, knd_c_real, Var_c var_mod_c)]),
			 LIST [Exp_b (name_r, type_r, Var_e var_mod_r)],
			 update_NILctx_insert_kind
			  (update_NILctx_insert_con(context,name_r, type_r), name_c, knd_c)))

       in
	   {cbnd_cat = cbnd_cat,
	    ebnd_cat = ebnd_cat,
            name_c   = name_c,
            name_r   = name_r,
	    knd_c    = knd_c_real,
	    knd_c_context = knd_c,
	    context  = context,
	    type_r   = type_r,
	    vmap     = vmap',
	    valuable = true}
       end

     | xmod' context (Il.MOD_APP(ilmod_fun, ilmod_arg), preferred_name) =
       let

	   val {cbnd_cat = cbnd_cat_fun,
		ebnd_cat = ebnd_cat_fun,
		name_c = name_fun_c,
                name_r = name_fun_r,
		knd_c = knd_fun_c,
		knd_c_context = knd_fun_c_context,
		context = context,
		type_r = type_fun_r,
		valuable = valuable_fun,
		vmap = vmap
		} = xmod context (ilmod_fun, NONE)

	   val {cbnd_cat = cbnd_cat_arg,
		ebnd_cat = ebnd_cat_arg,
		name_c = name_arg_c,
		name_r = name_arg_r,
		knd_c = knd_arg_c,
		knd_c_context = knd_arg_c_context,
		context = context,
		type_r = type_arg_r,
		valuable = valuable_arg,
		vmap = vmap
		} = xmod context (ilmod_arg, NONE)
	       
	   val (var, var_c, var_r, vmap) = chooseName (preferred_name, vmap)
	       
	   val var_arg_c = 
	       case strip_var name_arg_c
		   of SOME v => v
		 | NONE => (perr_c name_arg_c;
			    error "Expected constructor variable")
		       
	   val var_arg_r = 
	       case name_arg_r
		   of (Var_e v) => v
		 | _ => (perr_e name_arg_r;
			 error "Expected expression variable")
		       
	   val is_type_arg_unit = is_unit_c type_arg_r 
	       
	   val name_c = Var_c var_c
	   val name_r = Var_e var_r
	       
	   local
	       val (v_c,con_body_kind) = 
		   case strip_singleton knd_fun_c 
		       of (Arrow_k(_, [(v_c, _)], con_body_kind)) => (v_c,con_body_kind)
		     | _ => (perr_k knd_fun_c;
			     error "Expected arrow kind")

	       val (_,con_body_kind_context) = 
		   case strip_singleton knd_fun_c_context 
		       of (Arrow_k(_, [(v_c, _)], con_body_kind_context)) => (v_c,con_body_kind_context)
		     | _ => (perr_k knd_fun_c_context;
			     error "Expected arrow kind")
			   
			   
	       val (effect,var_body_arg_c,exp_body_type) = 
		   case strip_arrow type_fun_r 
		       of SOME (_,effect,[(var_body_arg_c,_)],_,_,exp_body_type) =>
			   (effect,var_body_arg_c,exp_body_type)
		     | _ => (perr_c type_fun_r;
			     error "Expected arrow constructor with one arg")
	   in
	       
	       val knd_c_context = Subst.varConKindSubst v_c name_arg_c con_body_kind_context
	       val knd_c = Subst.varConKindSubst v_c name_arg_c con_body_kind
		   
	       val context = update_vmap(context,vmap)
	       val context = update_NILctx_insert_kind(context,var_c,knd_c_context)

	       val cbnd_cat_new =
		       if (!do_kill_cpart_of_functor andalso
			   case knd_c of
			       (Record_k seq) => null (Util.sequence2list seq)
			     | _ => false)
			   then (LIST[(var_c, knd_c, Crecord_c[])])
		       else LIST[(var_c, knd_c, App_c(name_fun_c,[name_arg_c]))]
	       val cbnd_cat = APPEND[cbnd_cat_fun, cbnd_cat_arg, cbnd_cat_new]
		       

	       val is_var = (case ilmod_arg of
				 Il.MOD_VAR _ => true 
			       | _ => false)

		 
	     val type_r = Subst.varConConSubst var_body_arg_c name_arg_c exp_body_type
	     val valuable = (effect = Total) andalso valuable_fun andalso valuable_arg 
	   end  
       
	   val ebnd_cat_new = LIST[Exp_b(var_r, type_r,
					 makeAppE
					 name_fun_r
					 [name_arg_c]
					     (if (is_type_arg_unit andalso 
						  !optimize_empty_structure) then
						  []
					      else [name_arg_r])
						  [])]
	   val context = update_NILctx_ebndcat(context, ebnd_cat_new)
	   val ebnd_cat = APPEND[ebnd_cat_fun, 
			      ebnd_cat_arg,
			      ebnd_cat_new]

       in
	   {cbnd_cat  = cbnd_cat,
	    ebnd_cat  = ebnd_cat,
	    name_c    = name_c,
	    name_r    = name_r,
	    knd_c_context = knd_c_context,
	    knd_c     = knd_c,
	    context  = context,
	    type_r    = type_r,
	    vmap      = vmap,
	    valuable  = valuable}
       end


     | xmod' context (Il.MOD_SEAL(il_mod,_), preferred_name) = 
       (* The phase-splitting breaks abstraction *)
       xmod context (il_mod, preferred_name)
    
     | xmod' context (initial_mod as (Il.MOD_PROJECT _), preferred_name) =
       let
(*           val (il_module, lbls) = extractPathLabels initial_mod *)
           val Il.MOD_PROJECT(il_module, lbl) = initial_mod
	   val lbls = [lbl]

	   val _ = if (!omit_datatype_bindings)
		       then app (fn l => if (Ilutil.is_datatype_lab l)
					     then error "use of datatype labels detected"
					 else ()) lbls
		   else ()
	   val {cbnd_cat = cbnd_mod_cat, 
		ebnd_cat = ebnd_mod_cat,
		name_c   = name_mod_c, 
		name_r   = name_mod_r,
		knd_c    = knd_mod_c,
		knd_c_context = knd_mod_c_context,
		type_r   = type_mod_r,
		valuable = mod_valuable, 
		vmap     = vmap,
		context  = context,
		...} = xmod context (il_module, NONE)

	   val (var_proj, var_proj_c, var_proj_r, vmap) = 
	       chooseName (preferred_name, vmap)

(*
	   val _ = (print "------------------\nMOD_PROJECT case entered with var_proj_c = ";
		    Ppnil.pp_var var_proj_c;
		    print "\n")
*)
	   val name_proj_c = Var_c var_proj_c
	   val name_proj_r = Var_e var_proj_r

	   local
	       val Var_c var_mod_c = name_mod_c
	       val labels_r = (case strip_record type_mod_r 
				 of SOME (labels,cons) => labels
				  | _ => [])
	       fun mapper ((l,v),k) = if (Listops.member_eq(Name.eq_label,l,labels_r))
					  then NONE 
				      else SOME(v,(case (generate_con_from_kind k) of
						       NONE => Proj_c(name_mod_c,l)
						     | SOME c => c))
	       val table = 
		   (case (strip_singleton knd_mod_c) of
			Record_k lvk_seq => List.mapPartial mapper (Util.sequence2list lvk_seq)
		      | _ => [])
	       val subst = Subst.fromList table
(* val _ = (print "type_mod_r before subst is = \n";
	 Ppnil.pp_con type_mod_r; print "\n")
*)
	       val type_mod_r = Subst.substConInCon subst type_mod_r
	       val [l] = lbls
	       fun chandler (_,Proj_c(Var_c v, lab)) = 
			if (eq_label(l,lab))
				then Nilutil.CHANGE_NORECURSE name_proj_c
			else Nilutil.NOCHANGE
		 | chandler _ = Nilutil.NOCHANGE
	       val type_mod_r = Nilutil.con_rewrite (fn _ => Nilutil.NOCHANGE, fn _ => Nilutil.NOCHANGE,
				     chandler, fn _ => Nilutil.NOCHANGE, fn _ => Nilutil.NOCHANGE) type_mod_r
						
	       val type_mod_r' = type_mod_r
(* val _ = (print "type_mod_r after subst is = \n";
	 Ppnil.pp_con type_mod_r; print "\n")
*)
	   in

	       val con_proj_c = selectFromCon(name_mod_c, lbls)

	       val knd_proj_c = 
		   if (!use_imprecise_kind_at_bind)
		       then selectFromKindStrip(knd_mod_c_context,lbls)
		   else #2(nilcontext_con_valid(context,con_proj_c))

	       val (con_proj_c,knd_proj_c_real) = 
		   if (!do_kill_cpart_of_functor) 
		       then ((case (generate_con_from_kind knd_proj_c) of
				  NONE => con_proj_c
				| SOME c => c),
			     strip_kind knd_proj_c)
		   else (con_proj_c, knd_proj_c)

	       val type_proj_r = projectFromRecord context type_mod_r' lbls
	   end

	   val context = update_vmap(context,vmap)

	   val context = update_NILctx_insert_kind(context,var_proj_c, knd_proj_c)
	   val cbnd_cat_new = LIST [(var_proj_c, knd_proj_c_real,con_proj_c)]
           val cbnd_proj_cat = APPEND[cbnd_mod_cat,cbnd_cat_new]

	   val ebnd_cat_new = LIST [Exp_b(var_proj_r, type_proj_r,
					  selectFromRec(name_mod_r,type_mod_r,
							lbls))]
	   val context = update_NILctx_ebndcat(context, ebnd_cat_new)
	   val ebnd_proj_cat = APPEND[ebnd_mod_cat, ebnd_cat_new]

(*
	   val _ = (print "MOD_PROJECT case returning with var_proj_c = ";
		    Ppnil.pp_var var_proj_c;
		    print "\n")
*)
      in
	   {cbnd_cat = cbnd_proj_cat,
	    ebnd_cat = ebnd_proj_cat,
            name_c   = name_proj_c,
	    name_r   = name_proj_r,
	    knd_c    = knd_proj_c_real,
	    knd_c_context = knd_proj_c,
	    context  = context,
	    type_r   = type_proj_r,
	    valuable = mod_valuable,
	    vmap     = vmap}
       end

     | xmod' context (Il.MOD_FUNCTOR(var_arg, il_arg_signat, ilmod_body), 
		    preferred_name) =
       let

	   (* Split the argument parameter *)
	   val (var_arg_c, var_arg_r, vmap') = splitVar (var_arg, vmap_of context)
	   val (var_arg_c, var_arg_r, vmap',var_arg,ilmod_body) = 
		(case nilcontext_find_kind(context,var_arg_c) of
			NONE => (var_arg_c, var_arg_r, vmap',var_arg,ilmod_body)
		      | SOME _ =>
			let val _ = print "Duplicate functor var_arg in HIL\n"
			    val var_arg' = Name.derived_var var_arg
			    val ilmod_body = Ilutil.mod_subst_convar(ilmod_body,[(var_arg,Il.CON_VAR var_arg')])
		     	    val (var_arg_c, var_arg_r, vmap') = splitVar (var_arg', vmap_of context)
			in  (var_arg_c, var_arg_r, vmap',var_arg',ilmod_body)
			end)

	   val _ = clear_memo var_arg
	   val (knd_arg_context, knd_arg, con_arg) = xsig context (Var_c var_arg_c, il_arg_signat)
	   val is_con_arg_unit = is_unit_c con_arg 

	   (* Pre-project all sub-structures of functor argument *)
	   local val context = update_vmap(context, vmap')
		 val context = update_NILctx_insert_kind(context, var_arg_c, knd_arg_context)
		 val context = update_NILctx_insert_con(context, var_arg_r, con_arg)
	   in    val {cbnd_cat = cbnd_preproject_cat,
			ebnd_cat = ebnd_preproject_cat,
			context = inner_context} = preproject(var_arg,il_arg_signat,context)
	   end
	

           (* Split the functor body *)
		
	   val {cbnd_cat = cbnd_body_cat, 
		ebnd_cat = ebnd_body_cat, 
		name_c = name_body_c,
		name_r = name_body_r,
		knd_c = knd_body_c,
		knd_c_context = knd_body_c_context,
		context = _,
		type_r = type_body_r,
		valuable = body_valuable,
		vmap = vmap
		} = xmod inner_context (ilmod_body, NONE)


	   val cbnd_body_cat = APPEND[cbnd_preproject_cat, cbnd_body_cat]
	   val ebnd_body_cat = APPEND[ebnd_preproject_cat, ebnd_body_cat]

	   val (arrow, effect) = 
	       if body_valuable then
		   (Il.TOTAL, Total)
	       else 
		   (Il.PARTIAL, Partial)

	   val (var_fun, var_fun_c, var_fun_r, vmap) = 
	       chooseName (preferred_name, vmap)

           val name_fun_c = Var_c var_fun_c
	   val name_fun_r = Var_e var_fun_r

           val cbnds_body = flattenCatlist cbnd_body_cat
           val ebnds_body = flattenCatlist ebnd_body_cat

           val local_name_fun_c = Name.fresh_var ()

	   val cbnds_body_subst = 
	       let fun folder ((v,k,c),s) = Subst.con_subst_compose(s, Subst.fromList[(v,c)])
	       in  foldl folder (Subst.empty()) cbnds_body
	       end

	   val knd_body_c' = Subst.substConInKind cbnds_body_subst knd_body_c
	   val knd_body_c_context' = Subst.substConInKind cbnds_body_subst knd_body_c_context

	   val type_body_r' = makeLetC (map Con_cb cbnds_body) type_body_r

	   val knd_fun_c = Arrow_k(Open, [(var_arg_c, knd_arg)], knd_body_c')
	   val knd_fun_c_context = Arrow_k(Open, [(var_arg_c, knd_arg_context)], knd_body_c_context')
	   val type_fun_r = AllArrow_c(Open, effect, [(var_arg_c, knd_arg)], 
				       if (is_con_arg_unit andalso !optimize_empty_structure)
					   then []
				       else [con_arg], 
				       w0,
				       type_body_r')

           val cbnd_fun_cat = 
	       LIST[(var_fun_c, knd_fun_c,
		     makeLetC [Open_cb(local_name_fun_c, [(var_arg_c, knd_arg)],
				       makeLetC (map Con_cb cbnds_body) name_body_c,
				       knd_body_c')]
		              (Var_c local_name_fun_c))]

	   val ebnd_fun_cat =  
	       LIST[Fixopen_b (Util.list2set
			      [(var_fun_r,
			       Function(effect, Leaf,
					 [(var_arg_c, knd_arg)],
					 if (is_con_arg_unit andalso !optimize_empty_structure)
					   then []
					 else [(var_arg_r, con_arg)],
					 [],
					 makeLetE
					  ((map Con_b cbnds_body) @ ebnds_body)
					  name_body_r,
					 type_body_r'))])]

	   val context = (update_vmap
			  (update_NILctx_ebndcat
			   (update_NILctx_insert_kind(context, var_fun_c, knd_fun_c_context), ebnd_fun_cat), vmap))
       in
	   {cbnd_cat = cbnd_fun_cat,
            ebnd_cat = ebnd_fun_cat,
	    name_c = name_fun_c,
	    name_r = name_fun_r,
	    knd_c = knd_fun_c,
	    knd_c_context = knd_fun_c_context,
	    context = context,
	    type_r = type_fun_r,
	    vmap = vmap,
	    valuable = true}
       end
   
     | xmod' context (Il.MOD_STRUCTURE sbnds, preferred_name) =
       let
	   val {final_context = context, cbnd_cat, ebnd_cat, valuable, record_c_con_items,
		record_c_knd_items, record_c_knd_items_context, 
		record_r_labels, record_r_field_types,
		record_r_exp_items} = 
		xsbnds context sbnds

	   val (var_str, var_str_c, var_str_r, vmap) = 
	       chooseName (preferred_name, vmap_of context)

	   fun mapper (Il.SBND(l,Il.BND_CON(v,_))) = SOME(v,Proj_c(Var_c var_str_c,l))
	     | mapper (Il.SBND(l,Il.BND_MOD(v,_))) = SOME(v,Proj_c(Var_c var_str_c,l))
	     | mapper _ = NONE
	   val subst = Subst.fromList(List.mapPartial mapper sbnds)
	   val record_r_field_types = map (Subst.substConInCon subst) record_r_field_types
	   val (record_r_labels,record_r_field_types,record_r_exp_items) = 
	       let 
		   val temp = (ListMergeSort.sort gt_label_triple
			       (Listops.zip3 record_r_labels record_r_field_types record_r_exp_items))
	       in  (map #1 temp, map #2 temp, map #3 temp)
	       end

           val name_str_c = Var_c var_str_c
	   val name_str_r = Var_e var_str_r

	   val knd_str_c = Record_k (Util.list2sequence record_c_knd_items)
	   val knd_str_c_context = Record_k (Util.list2sequence record_c_knd_items_context)
	        
	   val cbnd_cat_new = LIST [(var_str_c, knd_str_c, Crecord_c record_c_con_items)]
           val cbnd_str_cat = APPEND[cbnd_cat,cbnd_cat_new]

           val specialize =
	       (case (!elaborator_specific_optimizations, sbnds) of
		    (true, [Il.SBND(lab, Il.BND_EXP _)]) => Name.eq_label (lab, Ilutil.it_lab)
		  | _ => false)

	   val type_str_r = 
	       if specialize then
		   hd record_r_field_types
	       else
		   Prim_c(Record_cc "2" record_r_labels, record_r_field_types)

	   val ebnd_cat_new = 
	       if specialize 
		   then LIST[Exp_b (var_str_r, type_str_r, hd record_r_exp_items)]
	       else     LIST[Exp_b (var_str_r, type_str_r,
				    Prim_e (NilPrimOp (record record_r_labels),
					    record_r_field_types, record_r_exp_items))]
	   val ebnd_str_cat = APPEND[ebnd_cat,ebnd_cat_new]


	   val context = update_NILctx_insert_kind(context, var_str_c, knd_str_c_context)
	   val context = update_NILctx_ebndcat(context, ebnd_cat_new)
	   val context = update_vmap(context, vmap)

       in
	   {cbnd_cat = cbnd_str_cat,
	    ebnd_cat = ebnd_str_cat,
            name_c = name_str_c,
	    name_r = name_str_r,
	    knd_c = knd_str_c,
	    knd_c_context = knd_str_c_context,
	    context = context,
	    type_r = type_str_r,
	    valuable = valuable,
	    vmap = vmap}
       end

(*
    | xmod' context (Il.MOD_LET (var_loc, il_loc_mod, Il.MOD_VAR v),preferred_name) = 
       if (eq_var(var_loc,v))
	   then xmod context (il_locmod,preferred_name)
       else
*)

    | xmod' context (il_let_mod as (Il.MOD_LET (var_loc, il_loc_mod, il_body_mod)),
		   preferred_name) =
       let

	   val _ = clear_memo var_loc

	   val (var_loc_c, var_loc_r, vmap) = splitVar (var_loc, vmap_of context)

	   val {cbnd_cat = cbnd_loc_cat,
		ebnd_cat = ebnd_loc_cat,
		knd_c = knd_loc_c,
		knd_c_context = knd_loc_c_context,
		type_r = type_loc_r,
		valuable = loc_valuable,
	        vmap = vmap,
		context = context,
		...} = let val context = update_vmap(context,vmap)
		       in  xmod context (il_loc_mod, SOME (var_loc, var_loc_c, var_loc_r))
		       end

	   local
	       val il_body_mod' = (case il_body_mod of
				       Il.MOD_SEAL(m,_) => m
				     | _ => il_body_mod)
	       val loc_context = context
	   in
	       val {cbnd_cat = cbnd_body_cat,
		    ebnd_cat = ebnd_body_cat,
		    name_c = name_let_c,
		    name_r = name_let_r,
		    knd_c = knd_let_c,
		    knd_c_context = knd_let_c_context,
		    type_r = type_let_r,
		    valuable = body_valuable,
		    vmap = vmap, 
		    context = context, ...} =  
			     xmod context (il_body_mod', preferred_name)
	       val (cbnd_body_cat,knd_let_c,knd_let_c_context,type_let_r) = 
		   case il_body_mod of
(*		       Il.MOD_SEAL(m,s) => 
			   let 
			       val mod_cname = 
				   (case name_let_c of
					Var_c v => v
				      | _ => error "name_let_c not a var_c")
			       val (kc_context,kc,tr) = xsig loc_context(name_let_c,s)
			       fun mapper(t as (v,k,c)) = 
				   if (Name.eq_var(v,mod_cname))
				       then (v,kc,c)
				   else t
			       val cbnd_body_list = 
				   map mapper (flattenCatlist cbnd_body_cat)
			   in  (LIST cbnd_body_list,kc,kc_context,tr)
			   end
		     | *) _ => (cbnd_body_cat, knd_let_c, knd_let_c_context, type_let_r)
	   end
(*
       val _ = (print "MOD_LET with letvar = "; Ppnil.pp_var var_loc; print "  and knd_let_c_context = \n";
		Ppnil.pp_kind knd_let_c_context; print "\nand var_let_c = ";
		(case name_let_c of
		       Var_c v => (case (nilcontext_find_kind(context, v)) of
				       NONE => error "var_let_c not bound\n"
				     | SOME k => (Ppnil.pp_var v; print " has kind = \n"; 
						  Ppnil.pp_kind k; print "\n\n"))
		     | _ => error "name_let_c not a var_c"))
*)
           val cbnd_let_cat = APPEND[cbnd_loc_cat, cbnd_body_cat]
           val ebnd_let_cat = APPEND[ebnd_loc_cat, ebnd_body_cat]

       in
	   {cbnd_cat = cbnd_let_cat,
	    ebnd_cat = ebnd_let_cat,
            name_c = name_let_c,
	    name_r = name_let_r,
	    knd_c = knd_let_c,
	    knd_c_context = knd_let_c_context,
	    context = context,
	    type_r = type_let_r,
	    valuable = loc_valuable andalso body_valuable,
	    vmap = vmap}
       end

   and xsbnds context il_sbnds =
       let
	   val this_call = ! xsbnds_count
	   val _ = 
	       if (!debug) then
		   (xsbnds_count := this_call + 1;
		    print ("Call " ^ (Int.toString this_call) ^ " to xsbnds\n");
		    if (!full_debug) then 
			(Ppil.pp_sbnds il_sbnds;
		        print ("\n");
		        print "vmap = ";
		        printVmap (vmap_of context);
		        print ("\n")) else ())
	       else ()

	   val result = (xsbnds_rewrite_1 context il_sbnds)
	       handle e => (if (!debug) then (print ("Exception detected in call " ^ 
						    (Int.toString this_call) ^ " to xsbnds\n");
					      print "\nwith context = \n";
					      print_splitting_context context;
					      print "\n")
			    else ();
			    raise e)

	in
	    if (!debug) then 
               print ("Return " ^ (Int.toString this_call) ^ " from xsbnds\n") else ();
	    result
        end

   and xsbnds_rewrite_1 context [] =
       {final_context = context,
	cbnd_cat = LIST nil,
	ebnd_cat = LIST nil,
	valuable = true,
	record_c_con_items = nil,
	record_c_knd_items = nil,
	record_c_knd_items_context = nil,
	record_r_labels = nil,
	record_r_field_types = nil,
	record_r_exp_items = nil}

     | xsbnds_rewrite_1 context (il_sbnds as (Il.SBND(lab, _))::rest_il_sbnds) =
        if ((Ilutil.is_datatype_lab lab) andalso (! omit_datatype_bindings)) then
	    xsbnds context rest_il_sbnds
        else
	    xsbnds_rewrite_2 context il_sbnds
(*
     | xsbnds_rewrite_1 context il_sbnds = 
	    xsbnds_rewrite_2 context il_sbnds
*)

   and xsbnds_rewrite_2 context 
                        (il_sbnds as
			 Il.SBND(lbl, 
				 Il.BND_EXP(top_var, 
					    il_exp as Il.FIX(_, _, fbnds)))
			 ::rest_il_sbnds) =
       (if (Util.substring("polyfun",Name.label2string lbl)) then
	   let
	       (* external_labels = Exported labels for these functions.
                  external_vars = Variables to which the functions should be bound
                                  in the returned NIL bindings 
                  rest_il_sbnds' = remaining il_sbnds after this group of functions 
                *)
	       val (rest_il_sbnds', external_labels, external_vars) = 
		   let
		       val num_functions = length fbnds

		       fun getFunctionNames 0 (rest, labs, vars) = (rest, rev labs, rev vars)
			 | getFunctionNames n (Il.SBND(lab,Il.BND_EXP(var,_))::rest, labs, vars) = 
			      getFunctionNames (n-1) (rest, lab::labs, var::vars)
			 | getFunctionNames _ _ = error "xsbnds: Can't optimize mono function"
		   in
		       getFunctionNames num_functions (rest_il_sbnds, nil, nil)
		   end

               val _ = if (!debug) then 
		        (print "Optimizing monomorphic functions: \n";
			 app (fn l => (Ppnil.pp_var l; print " ")) external_vars;
                         print "\n")
                       else ()

       (* internal_vars = Variables to which the functions are bound
                        in the HIL fix-construct.
        il_functions = Bodies of the functions in this mutually-recursive group *)
               val (internal_vars, il_functions) = 
		   let
		       val (Let_e (_,[Fixopen_b il_fn_set],_),_,_) = xexp context il_exp
		   in
		       Listops.unzip (Util.set2list il_fn_set)
		   end

               (* subst = substitution mapping internal vars to external vars *)
	       val subst = Subst.fromList (Listops.map2 (fn (iv,ev) => (iv,Var_e ev)) 
					   (internal_vars,external_vars))
		  
	       (* Replace uses of "internal" variable names with "external"
                  variables names in the function bodies *)
               fun reviseFunction (external_var,
				   Function(effect,recursive,[],
					    vclist,[],body,body_con)) =
		   let val body' = Subst.substExpInExp subst body
		   in  (external_var,
			Function(effect, recursive, [],
				 vclist, [], body', body_con))
                   end


               val ebnd_entries = Listops.map2 reviseFunction (external_vars, il_functions)

               val ebnd_types = map (fn (_,Function(effect,_,carg,vclist,_,_,body_type)) =>
				     AllArrow_c(Open,effect,carg,map #2 vclist,
						w0,body_type)) 
                                    ebnd_entries

	       val ebnds = [Fixopen_b (Util.list2set ebnd_entries)]

	       (* Add these functions to the context *)
               val context'' = 
		   let val nil_edecs = Listops.zip external_vars ebnd_types
		   in  update_NILctx_insert_con_list(context,nil_edecs)
		   end

	       val {final_context, cbnd_cat, ebnd_cat, valuable, record_c_con_items,
		    record_c_knd_items, record_c_knd_items_context,
		    record_r_labels, record_r_field_types,
		    record_r_exp_items} = xsbnds context'' rest_il_sbnds'
		    
	   in
	       {final_context = final_context,
		cbnd_cat = cbnd_cat,
		ebnd_cat = APPEND [LIST ebnds, ebnd_cat],
		valuable = valuable,
		record_c_con_items   = record_c_con_items,
		record_c_knd_items   = record_c_knd_items,
		record_c_knd_items_context = record_c_knd_items_context,
		record_r_labels      = external_labels @ record_r_labels,
		record_r_field_types = ebnd_types @ record_r_field_types,
		record_r_exp_items   = (map Var_e external_vars) @ record_r_exp_items}
	   end
	else
	    xsbnds_rewrite_3 context il_sbnds)

     | xsbnds_rewrite_2 context
                        (il_sbnds as
			 Il.SBND(lbl, 
				Il.BND_MOD
				(top_var, m as 
				 Il.MOD_FUNCTOR
				 (poly_var, il_arg_signat, 
				  Il.MOD_STRUCTURE
				  [Il.SBND(it_lbl,
					   Il.BND_EXP
					   (_, il_exp as Il.FIX(is_recur, arrow, fbnds)))])))
			 :: rest_il_sbnds) =

       if ((Name.eq_label (it_lbl, Ilutil.it_lab))
	   andalso ((Name.is_label_internal lbl)  (* this or is needed to handler bnds *)
		    orelse (Util.substring("polyfun",Name.label2string lbl)))
	   andalso (not (Name.eq_label (lbl, Ilutil.expose_lab)))
	   andalso (not (Ilutil.is_eq_lab lbl))) then
				  
	   let
	       val _ = clear_memo top_var
	       val _ = clear_memo poly_var

	       (* external_labels = Exported labels for these functions.
                  external_vars = Variables to which the functions should be bound
                                  in the returned NIL bindings 
                  rest_il_sbnds' = remaining il_sbnds after this group of functions 
                *)
	       val (rest_il_sbnds', external_labels, external_vars) = 
		   let
		       val num_functions = length fbnds

		       fun getFunctionNames 0 (rest, labs, vars) = (rest, rev labs, rev vars)
			 | getFunctionNames n (Il.SBND(lab,Il.BND_MOD(var,_))::rest, labs, vars) = 
			      getFunctionNames (n-1) (rest, lab::labs, var::vars)
			 | getFunctionNames _ _ = error "xsbnds: Can't optimize poly function"
		   in
		       getFunctionNames num_functions (rest_il_sbnds, nil, nil)
		   end

       (* internal_vars = Variables to which the functions are bound
                        in the HIL fix-construct.
        il_functions = Bodies of the functions in this mutually-recursive group *)

               val (external_vars_c, external_vars_r, vmap') = 
		   let fun loop ([], ns_c, ns_r, vmap) = (rev ns_c, rev ns_r, vmap)
			 | loop (n::ns, ns_c, ns_r, vmap) =
			   let val (n_c, n_r, vmap') = splitVar (n, vmap)
			   in
			       loop (ns, n_c :: ns_c, n_r :: ns_r, vmap')
			   end
		   in
		       loop (external_vars, nil, nil, vmap_of context)
		   end

	       val (poly_var_c, poly_var_r, vmap'') = splitVar (poly_var, vmap')
	       val (knd_arg_context, knd_arg, con_arg) = xsig context (Var_c poly_var_c, il_arg_signat)
	       val is_con_arg_unit = is_unit_c con_arg 

	       val context' = update_vmap(context, vmap'')
	       val context' = update_NILctx_insert_kind(context', poly_var_c, knd_arg_context)
	       val context' = update_NILctx_insert_con(context', poly_var_r, con_arg)

	       val (Let_e (_, [Fixopen_b set], _), _, _) = xexp context' il_exp

               val (internal_vars, functions) = Listops.unzip (Util.set2list set)
	       val inner_vars = map (fn v => Name.fresh_named_var((Name.var2name v) ^ "_inner")) external_vars_r
               fun subst self =
		   let 
		     fun loop (nil,nil,nil) = []
		       | loop (v'::vs, n::ns, iv::ivs) = 
		       let
			 val exp = 
			   (if (Name.eq_var(v',self))
			      then (Var_e iv)
			    else
			      (makeAppE 
			         (Var_e n) 
				 [Var_c poly_var_c]
				 (if (is_con_arg_unit andalso 
				      !optimize_empty_structure) then
				      []
				  else [Var_e poly_var_r])
				 []))
		       in
			 (v',exp)::loop (vs,ns,ivs)
		       end
		   in Subst.fromList (loop (internal_vars, external_vars_r, inner_vars))
		   end

               fun reviseFunction (internal_var,
				   external_var_r, inner_var,
				   Function(effect,recursive,[],
					    [(arg_var,arg_con)],[],body,body_con)) =
		   let val body' = Subst.substExpInExp (subst internal_var) body
		   in  (external_var_r,
		       Function(Total, Leaf, 
				[(poly_var_c, knd_arg)],
				if (is_con_arg_unit andalso !optimize_empty_structure)
				    then []
				else [(poly_var_r, con_arg)],
				[],
				Let_e (Sequential,
				       [Fixopen_b
					(Util.list2set 
					 [(inner_var, Function(effect,recursive,[],
							  [(arg_var,arg_con)],[],
							  body',body_con))])],
				       Var_e inner_var),
				AllArrow_c(Open, effect, [], [arg_con], w0, body_con)))
		   end

               val nullfunction_c = 
		   let
		       val var'' = Name.fresh_var ()
		   in
		       makeLetC [Open_cb(var'', 
					 [(poly_var_c, knd_arg)], 
					 Crecord_c [],
					 Record_k (Util.list2sequence []))]
		       (Var_c var'')
		   end

               val nullfunction_k =
		   Arrow_k(Open, [(poly_var_c, knd_arg)], Record_k (Util.list2sequence []))

               val cbnds = (map (fn n_c => (n_c, nullfunction_k, nullfunction_c))
			    external_vars_c)
		       

               val cbnd_knds = map (fn _ => nullfunction_k) external_vars_c

               val ebnd_entries = (Listops.map4 reviseFunction 
				   (internal_vars, external_vars_r, inner_vars, functions))

               val ebnd_types = map (fn (_,Function(_,_,carg,vclist,w,_,body_type)) =>
				     AllArrow_c(Open,Total,carg,map #2 vclist,
						Word32.fromInt (List.length w),body_type)) 
                                    ebnd_entries

	       val ebnds = [Fixopen_b (Util.list2set ebnd_entries)]

               val nil_cdecs = map (fn n_c => (n_c, nullfunction_k)) external_vars_c

               val nil_edecs = map (fn (n_r,Function(effect,_,a1,a2,_,_,a4)) =>
				    (n_r, AllArrow_c(Open,effect,a1,map #2 a2,w0,a4)))
		                   ebnd_entries


	       val context'' = update_vmap(context, vmap')
               val context'' = update_NILctx_cbndcat(context'', LIST cbnds)
               val context'' = update_NILctx_ebndcat(context'', LIST ebnds)


               val dummy_vars = map (fn _ => Name.fresh_var()) external_vars_c
		    
	       val {final_context, cbnd_cat, ebnd_cat, valuable, record_c_con_items,
		    record_c_knd_items, record_c_knd_items_context, 
		    record_r_labels, record_r_field_types,
		    record_r_exp_items} = xsbnds context'' rest_il_sbnds'
		    
	   in
	       {final_context = final_context,
		cbnd_cat = if !do_kill_cpart_of_functor
			       then cbnd_cat
			   else
			       APPEND [LIST cbnds, cbnd_cat],
		ebnd_cat = APPEND [LIST ebnds, ebnd_cat],
		valuable = valuable,
		record_c_con_items = if !do_kill_cpart_of_functor
					 then record_c_con_items
				     else (Listops.zip external_labels (map Var_c external_vars_c))
					 @ record_c_con_items,
		record_c_knd_items = if !do_kill_cpart_of_functor
					 then record_c_knd_items
				     else (Listops.zip (Listops.zip external_labels dummy_vars) cbnd_knds)
					 @ record_c_knd_items,
  	        record_c_knd_items_context = (Listops.zip (Listops.zip external_labels dummy_vars) cbnd_knds)
		                             @ record_c_knd_items_context,
		record_r_labels = external_labels @ record_r_labels,
		record_r_field_types = ebnd_types @ record_r_field_types,
		record_r_exp_items = (map Var_e external_vars_r) @ record_r_exp_items}
	   end
       else
	   xsbnds_rewrite_3 context il_sbnds

     | xsbnds_rewrite_2 context il_sbnds = xsbnds_rewrite_3 context il_sbnds

   and xsbnds_rewrite_3 context (Il.SBND(lbl, Il.BND_EXP(var, il_exp)) :: rest_il_sbnds) =
       let
	   val (exp, con, exp_valuable) = xexp context il_exp

           val context' = update_NILctx_insert_con(context, var, con)

	   val {final_context, cbnd_cat, ebnd_cat, valuable, record_c_con_items,
		record_c_knd_items,  record_c_knd_items_context, 
		record_r_labels, record_r_field_types,
		record_r_exp_items} = xsbnds context' rest_il_sbnds
       in
	   {final_context = final_context,
	    cbnd_cat = cbnd_cat,
	    ebnd_cat = APPEND[LIST [Exp_b(var,con,exp)], ebnd_cat],
	    valuable = valuable andalso exp_valuable,
	    record_c_con_items = record_c_con_items,
	    record_c_knd_items = record_c_knd_items,
	    record_c_knd_items_context = record_c_knd_items_context,
	    record_r_labels = lbl :: record_r_labels,
	    record_r_field_types = con :: record_r_field_types,
	    record_r_exp_items = (Var_e var) :: record_r_exp_items}
       end

     | xsbnds_rewrite_3 context (Il.SBND(lbl, Il.BND_CON(var, il_con)) :: rest_il_sbnds) =
       let

           (* Unfortunately, the HIL may duplicate variables, and the flattening
              of modules may put duplicates that used to have disjoint scopes
              into overlapping scopes. *)

	   val (var,rest_il_sbnds) = 
	       (case nilcontext_find_kind(context, var) of
		    NONE => (var,rest_il_sbnds)
		  | SOME _ => let val _ = (print ("WARNING (xsbnds/BND_CON):  " ^
						  "Duplicate variable found:");
					   Ppnil.pp_var var;
					   print "\n")
				  val v = Name.derived_var var
				  val table = [(var, Il.CON_VAR v)]
				  val Il.MOD_STRUCTURE rest' = 
				      Ilutil.mod_subst_convar(Il.MOD_STRUCTURE rest_il_sbnds,table)
			      in (v,rest')
			      end)

	   val (con, knd, knd_context) = xcon context il_con
	   val kill_con = (!do_kill_cpart_of_functor andalso
			   case knd of
			       Arrow_k(_,_,Record_k seq) => null (Util.sequence2list seq)
			     | _ => false)

           val context' = update_NILctx_insert_kind(context, var, knd_context)

	   val {final_context, cbnd_cat, ebnd_cat, valuable, record_c_con_items,
		record_c_knd_items, record_c_knd_items_context,
		record_r_labels, record_r_field_types,
		record_r_exp_items} = xsbnds context' rest_il_sbnds
       in
	   {final_context = final_context,
	    cbnd_cat = CONS ((var, knd, con), cbnd_cat),
	    ebnd_cat = ebnd_cat,
	    valuable = valuable,
	    record_c_con_items = if (kill_con)
				     then record_c_con_items
				 else (lbl, Var_c var) :: record_c_con_items,
	    record_c_knd_items = if (kill_con)
				     then record_c_knd_items
				 else ((lbl, var), knd) :: record_c_knd_items, 
	    record_c_knd_items_context = ((lbl, var), knd_context) :: record_c_knd_items_context, 
	    record_r_labels = record_r_labels,
	    record_r_field_types = record_r_field_types,
	    record_r_exp_items = record_r_exp_items}
       end

     | xsbnds_rewrite_3 context (Il.SBND(lbl, Il.BND_MOD(var, il_module))::rest_il_sbnds) =
       let

	   val _ = clear_memo var

           (* Unfortunately, the HIL may duplicate variables, and the flattening
              of modules may put duplicates that used to have disjoint scopes
              into overlapping scopes. *)

	   val (var,rest_il_sbnds) = 
	       (case lookupVmap(var,vmap_of context) of
		    NONE => (var,rest_il_sbnds)
		  | SOME _ => 
			let val _ = (print ("WARNING (xsbnds/BND_MOD):  " ^
					    "Duplicate variable found:");
				     Ppnil.pp_var var;
				     print "\n")
			    val v = Name.derived_var var
			    val table = [(var, Il.MOD_VAR v)]
			    val Il.MOD_STRUCTURE rest' = 
				Ilutil.mod_subst_modvar(Il.MOD_STRUCTURE rest_il_sbnds,table)
			in (v,rest')
			end)


	   val (var_c, var_r, vmap) = splitVar (var, vmap_of context)
	       
(*
	   val _ = (print "------------------\nBND_MOD case entered with var = ";
		    Ppnil.pp_var var;
		    print " and var_c = ";
		    Ppnil.pp_var var_c;
		    print "\n")
*)

	   val {cbnd_cat = cbnd_mod_cat, 
		ebnd_cat = ebnd_mod_cat,
		knd_c    = knd_mod_c,
		knd_c_context = knd_mod_c_context,
		context = context,
		type_r   = type_mod_r,
		valuable = mod_valuable, 
		vmap = vmap,
		name_c, name_r,
		...} = xmod (update_vmap(context,vmap)) (il_module, SOME(var, var_c, var_r))

(*
	   val _ = (print "xsbnd: BND_MOD with var = "; Ppnil.pp_var var; 
		    print " and knd_mod_c_context = \n";
		    Ppnil.pp_kind knd_mod_c;
		    print "\n\n")
*)

	   val {final_context, cbnd_cat, ebnd_cat, valuable, record_c_con_items,
		record_c_knd_items, record_c_knd_items_context, 
		record_r_labels, record_r_field_types,
		record_r_exp_items} = xsbnds context rest_il_sbnds

	   val kill_con = (!do_kill_cpart_of_functor andalso
			   case knd_mod_c of
			       Arrow_k(_,_,Record_k seq) => null (Util.sequence2list seq)
			     | _ => false)


       in
	   {final_context = final_context,
	    cbnd_cat = APPEND[cbnd_mod_cat, cbnd_cat],
	    ebnd_cat = APPEND[ebnd_mod_cat, ebnd_cat],
	    valuable = mod_valuable andalso valuable,
	    record_c_con_items = if (kill_con)
				     then record_c_con_items
				 else (lbl, name_c) :: record_c_con_items,
	    record_c_knd_items = if (kill_con)
				     then record_c_knd_items 
				 else ((lbl, var_c), knd_mod_c) :: record_c_knd_items, 
	    record_c_knd_items_context = ((lbl, var_c), knd_mod_c_context) :: record_c_knd_items_context, 
	    record_r_labels = lbl :: record_r_labels,
	    record_r_field_types = type_mod_r :: record_r_field_types,
	    record_r_exp_items = name_r :: record_r_exp_items}
       end

   and xflexinfo context (ref (Il.INDIRECT_FLEXINFO f)) = 
       xflexinfo context f
     | xflexinfo context (ref (Il.FLEXINFO(_,true, recs))) = 
       let
	   val (lbls, cons) = xrdecs context recs
	   val con = Prim_c(Record_cc "3" lbls, cons) (* already sorted *)
	   val knd = Singleton_k (Runtime, Word_k Runtime, con)
       in
	   (con, knd, knd)
       end

   and xrdecs context [] = ([], [])
     | xrdecs context ((lab, il_con) :: rest) = 
       let
	   val (labs, cons) = xrdecs context rest
	   val (con, _, _) = xcon context il_con
       in
	   (lab :: labs, con :: cons)
       end

   and xcon context il_con : con * kind * kind =
       let
	   val this_call = ! xcon_count
	   val _ = 
	       if (!debug) then
		   (xcon_count := this_call + 1;
		    print ("Call " ^ (Int.toString this_call) ^ " to xcon\n");
		    if (!full_debug) then (Ppil.pp_con il_con; print"\n") else ())
	       else ()

	   fun check_proj(Il.MOD_VAR v,ls) = 
	       lookup_con_memo (v,ls) (fn()=> xcon' context il_con)
	     | check_proj(Il.MOD_PROJECT(m,l),ls) = check_proj(m,l::ls)
	     | check_proj _ = xcon' context il_con
	   val result = (case (!do_memoize,il_con) of
			     (true,Il.CON_MODULE_PROJECT(m,l)) => check_proj(m,[l])
			   | _ => xcon' context il_con)
	       handle e => (if (!debug) then print ("Exception detected in call " ^ 
						    (Int.toString this_call) ^ " to xcon\n") else ();
			    raise e)

	in
	    if (!debug) then print ("Return " ^ (Int.toString this_call) ^ " from xcon\n") else ();
	    result
        end

   and xcon' context (il_con as (Il.CON_VAR var)) : con * kind * kind = 
       let
	   val con = Var_c var
	   val kind = (case nilcontext_find_kind' (context, var) of
			   SOME kind => kind
			 | NONE => (print "Could not find constructor variable ";
				    Ppil.pp_var var;
				    if (!debug)
				      then (print "in context:\n"; nilcontext_print context)
				    else ();
				    error "xcon: CON_VAR\n"))
	       
	   val con = (case generate_con_from_kind kind of
			  NONE => con
			| SOME c => c)
	   val kind_real = strip_kind kind
       in
	   (con, kind_real, kind)
       end

     | xcon' context (Il.CON_TYVAR tv) = xcon context (derefTyvar tv)

     | xcon' context (Il.CON_OVAR ov) = xcon context (derefOvar ov)

     | xcon' context (Il.CON_FLEXRECORD fr) = xflexinfo context fr

     | xcon' context ((Il.CON_INT Prim.W64) | (Il.CON_UINT Prim.W64)) =
       raise Util.BUG "64-bit integers not handled during/after phase-split"

     | xcon' context ((Il.CON_INT intsize) | (Il.CON_UINT intsize)) =
       let
           val con = Prim_c (Int_c intsize, [])
           val knd = Word_k Runtime
       in
	   (con, knd, knd)
       end

     | xcon' context (Il.CON_FLOAT floatsize) = 
       let
	   val con = Prim_c (BoxFloat_c floatsize, [])
	   val knd = Word_k Runtime
       in
	   (con, knd, knd)
       end

     | xcon' context (Il.CON_ARRAY il_con) = 
       let
	   val (con', _, _) = xcon context il_con 
	   val con = Prim_c (Array_c, [con'])
	   val knd = Word_k Runtime
       in  (con, knd, knd)
       end

     | xcon' context (Il.CON_VECTOR il_con) = 
       let
	   val (con', _, _) = xcon context il_con 
	   val con = Prim_c (Vector_c, [con'])
	   val knd = Word_k Runtime
       in  (con, knd, knd)
       end

     | xcon' context (Il.CON_ANY) = 
       let
	   val con = Prim_c(Exn_c, [])
	   val knd = Word_k Runtime
       in
	   (con, knd, knd)
       end

     | xcon' context (Il.CON_REF il_con) = 
       let
	   val (con', _, _) = xcon context il_con
	   val con = Prim_c (Ref_c, [con'])
	   val knd = Word_k Runtime
       in
	   (con, knd, knd)
       end

     | xcon' context (Il.CON_TAG il_con) = 
       let
	   val (con', _, _) = xcon context il_con
	   val con = Prim_c (Exntag_c, [con'])
	   val knd = Word_k Runtime
       in  (con, knd, knd)
       end

     | xcon' context (Il.CON_ARROW (il_cons1, il_con2, closed, arr)) =
       let
	   val (floats,il_cons1) = 
	       if closed
		   then 
		       let fun folder (Il.CON_FLOAT Prim.F64, (cons,f)) = (cons, TilWord32.uplus(f,0w1))
			     | folder (c,(cons,f)) = (c::cons,f)
			   val (rev_il_cons1,floats) = foldl folder ([],w0) il_cons1
		       in  (floats, rev rev_il_cons1)
		       end
	       else (w0, il_cons1)
	   val cons1 = map (#1 o (xcon context)) il_cons1
           val con2 = #1(xcon context il_con2)
           val con2 = (case (closed,il_con2) of
			   (true,Il.CON_FLOAT Prim.F64) => Prim_c (Float_c Prim.F64, [])
			 | _ => #1(xcon context il_con2))
	   val eff = xeffect (derefOneshot arr)
	   val con = AllArrow_c(if closed then ExternCode else Open, eff, [], cons1, floats, con2)
	   val knd = Word_k Runtime
       in  (con, knd, knd)
       end
(*
       let
	   fun folder (Il.CON_FLOAT Prim.F64, (cons,f)) = (cons, TilWord32.uplus(f,0w1))
	     | folder (c,(cons,f)) = (c::cons,f)
	   val (rev_il_cons1,floats) = foldl folder ([],w0) il_cons1
	   val il_cons1 = rev rev_il_cons1
	   val cons1 = map (#1 o (xcon context)) il_cons1
           val con2 = (case (closed,il_con2) of
			   (true,Il.CON_FLOAT Prim.F64) => Prim_c (Float_c Prim.F64, [])
			 | _ => #1(xcon context il_con2))
	   val eff = xeffect (derefOneshot arr)
	   val con = AllArrow_c(if closed then ExternCode else Open, eff, [], cons1, floats, con2)
       in
	   (con, Word_k Runtime)
       end
*)
     | xcon' context (il_con as Il.CON_APP (il_con1, il_con2)) = 
       let
	   val (con1, knd1, knd1_context) = xcon context il_con1
           val (Arrow_k(_,[(v_arg,_)],body)) = Nilutil.strip_singleton knd1
           val (Arrow_k(_,[(v_arg,_)],body_context)) = Nilutil.strip_singleton knd1_context
           val (con2, _, _) = xcon context il_con2
	   val con = App_c(con1, [con2])
	   val knd = Subst.varConKindSubst v_arg con2 body
	   val knd_context = Subst.varConKindSubst v_arg con2 body_context
       in  (con, knd, knd_context)
       end

     | xcon' context (Il.CON_MUPROJECT(i, Il.CON_FUN(vars, 
						     Il.CON_TUPLE_INJECT cons))) =
       let

	   fun is_bound v = (case nilcontext_find_kind' (context, v) of
				 NONE => false | SOME _ => true)
	   val Il.CON_FUN(vars, Il.CON_TUPLE_INJECT cons) = 
	       Ilutil.rename_confun(is_bound,vars,Il.CON_TUPLE_INJECT cons)

	   val context' = update_NILctx_insert_kind_list(context,map (fn v => (v,Word_k Runtime)) vars)
	       
	   val cons'= map (#1 o (xcon context')) cons
	   val freevars = Listops.flatten (map Ilutil.con_free_convar cons)
	   val is_recur = Listops.orfold (fn v => Listops.member_eq(Name.eq_var,v,freevars)) vars

	   val con = Mu_c (is_recur,
			   Util.list2set (Listops.zip vars cons'), 
			   List.nth (vars, i))
	   val knd = Word_k Runtime
       in
	   (con, knd, knd)
       end

     | xcon' context (Il.CON_MUPROJECT(i, Il.CON_FUN([var], con))) =
       let
	   fun is_bound v = (case nilcontext_find_kind' (context, v) of
				 NONE => false | SOME _ => true)
	   val Il.CON_FUN([var],con) = Ilutil.rename_confun(is_bound,[var],con)

	   val context' = update_NILctx_insert_kind(context, var, Word_k Runtime)
	       
	   val (con',_,_) = xcon context' con
	   val freevars = Ilutil.con_free_convar con
	   val is_recur = Listops.member_eq(Name.eq_var,var,freevars)
	   val con = Mu_c (is_recur,Util.list2set [(var, con')], var)
	   val knd = Word_k Runtime
       in
	   (con, knd, knd)
       end

     | xcon' context (Il.CON_RECORD rdecs) = 
       let
	   val (lbls, cons) = xrdecs context rdecs
	   val con = Prim_c (Record_cc "4"  lbls, cons) (* already sorted *)
	   val knd = Word_k Runtime
       in
	   (con, knd, knd)
       end

     | xcon' context (Il.CON_FUN (vars, il_con1)) = 
       let
	   val context' = update_NILctx_insert_kind_list(context, map (fn v => (v,Word_k Runtime)) vars)

	   val (con1, knd1, knd1_context) = xcon context' il_con1
	   val (arg, con1, knd1, knd1_context) =
	       case vars of
		   [v] => ((v, Word_k Runtime), con1, knd1, knd1_context)
		 | _ => let fun mapper (n,_) = ((Nilutil.generate_tuple_label (n+1),
						 Name.fresh_var()),Word_k Runtime)
			    val arg_var = Name.fresh_var()
			    val arg_kind = Record_k(Util.sequence2list
						    (Listops.mapcount mapper vars))
			    fun mapper (n,v) = 
				(v,Proj_c(Var_c arg_var, 
					  Nilutil.generate_tuple_label (n+1)))
			    val substlist = Listops.mapcount mapper vars
				
			    val con1' = makeLetC (map (fn (v,c) => Con_cb(v,Word_k Runtime, c))
						  substlist) con1
			    val knd1' = Subst.substConInKind (Subst.fromList substlist) knd1
			    val knd1'_context = Subst.substConInKind (Subst.fromList substlist) knd1_context
			in  ((arg_var, arg_kind), con1', knd1', knd1'_context)
			end
		    
	   val fun_name = Name.fresh_var ()
	   val con = makeLetC ([Open_cb(fun_name, [arg], con1, knd1)])
	       (Var_c fun_name)
	   val knd = Arrow_k(Open, [arg], knd1)
	   val knd_context = Arrow_k(Open, [arg], knd1_context)
       in  (con, knd, knd_context)
       end


     | xcon' context (Il.CON_SUM {carriers, noncarriers, special}) =
       let
	   val known = (case special of
			       NONE => NONE
			     | SOME i => SOME (Word32.fromInt i))
	   val cons = map (#1 o (xcon context)) carriers
	   val con = Prim_c (Sum_c {tagcount = Word32.fromInt noncarriers,
				    known = known}, cons)
	   val knd = Word_k Runtime
       in
	   (con, knd, knd)
       end

     | xcon' context (il_con as (Il.CON_TUPLE_INJECT il_cons)) = 
       let
	   val (cons, knds, knds_context) = Listops.unzip3 (map (xcon context) il_cons)
	   val tuple_length = List.length cons
	   val labels = makeLabels tuple_length
	   val vars = makeVars tuple_length
	   val con = Crecord_c(Listops.zip labels cons)
	   val knd = Record_k (Util.list2sequence 
			       (Listops.zip (Listops.zip labels vars) knds))
	   val knd_context = Record_k (Util.list2sequence 
			       (Listops.zip (Listops.zip labels vars) knds_context))
       in
	   (con, knd, knd_context)
       end

     | xcon' context (il_con as (Il.CON_TUPLE_PROJECT (i, il_con1))) = 
       let
	   val (con1, Record_k seq, Record_k seq_context) = xcon context il_con1
	   val lbl = Ilutil.generate_tuple_label i
	   val con = Proj_c(con1, lbl)
	   fun equal((l,_),(l',_)) = Name.eq_label (l,l')
	   val knd = 
	       (case (Util.sequence_lookup equal seq (lbl,Name.fresh_var())) of
		    SOME knd => knd
		  | NONE => (print "(xcon) Error translating:\n";
			     Ppil.pp_con il_con;
			     print "Cannot find label ";
			     Ppil.pp_label lbl;
			     error "(xcon) CON_TUPLE_PROJECT"))
	   val knd_context = 
	       (case (Util.sequence_lookup equal seq_context (lbl,Name.fresh_var())) of
		    SOME knd => knd
		  | NONE => (print "(xcon) Error translating:\n";
			     Ppil.pp_con il_con;
			     print "Cannot find label ";
			     Ppil.pp_label lbl;
			     error "(xcon) CON_TUPLE_PROJECT"))
	   val con = (case generate_con_from_kind knd of
			  NONE => con
			| SOME c => c)
		
       in (con, knd, knd_context)
       end

     | xcon' context (il_con as (Il.CON_MODULE_PROJECT (modv, lbl))) = 
       let
	   val {cbnd_cat,name_c,knd_c,context,...} = 
	       xmod context (modv, NONE)
           val cbnd_list = flattenCatlist cbnd_cat

	   val proj_con = Proj_c (name_c, lbl)
	   val con = makeLetC (map Con_cb cbnd_list) proj_con

	   val knd = if (!use_imprecise_kind_at_bind)
			 then selectFromKindStrip(knd_c,[lbl])
		     else #2(nilcontext_con_valid (context, proj_con))

	   (* this gets rid of Arrow_k(...,Record_k[]) *)
	   val knd_stripped = strip_kind knd
       in
	   (con, knd_stripped, knd)
       end
    
     | xcon' _ c = (print "Error:  Unrecognized constructor:\n";
		    Ppil.pp_con c;
		    print "\n";
		    error "(xcon):  Unrecognized constructor")
   
   and toFunction context (exp as Il.FIX _) =
       let
	   val (Let_e (_, [Fixopen_b fns], Var_e var), _, _) = xexp context exp
       in
	   case	(Util.sequence_lookup (Name.eq_var) fns var) of
	       SOME f => f
	     | NONE => error "(toFunction): impossible"
       end
     | toFunction _ e = 
       (Ppil.pp_exp e;
	error "(toFunction): not a FIX expression")

   and xvalue context (Prim.int (intsize, w)) = 
       (Const_e (Prim.int (intsize, w)), Prim_c(Int_c intsize, nil), true)

     | xvalue context (Prim.uint (intsize, w)) = 
       (Const_e (Prim.uint (intsize, w)), Prim_c(Int_c intsize, nil), true)

     | xvalue context (Prim.float (floatsize, f)) = 
       (Prim_e (NilPrimOp (box_float floatsize),
		[], [Const_e (Prim.float (floatsize, f))]),
	Prim_c(BoxFloat_c floatsize,nil), true)

     | xvalue context (Prim.array (il_con, a)) = 
       let
	   val il_exps = Array.foldr (op ::) nil a
           val (con,_,_) = xcon context il_con
           val exps = map (#1 o (xexp context)) il_exps
       in
	   (Const_e (Prim.array (con, Array.fromList exps)), 
	    Prim_c(Array_c, [con]), true)
       end

     | xvalue context (Prim.vector (il_con, v)) = 
       let
	   val il_exps = Array.foldr (op ::) nil v
           val (con, _,_) = xcon context il_con
	   val exps = map (#1 o (xexp context)) il_exps
       in
	   (Const_e (Prim.vector (con, Array.fromList exps)), 
	    Prim_c(Vector_c, [con]), true)
       end

     | xvalue context (Prim.refcell (ref il_exp)) = 
       let
	   val (exp, con, _) = xexp context il_exp
       in
	   (* BUG *)
	   (* SHOULD PRESERVE EQUIVALENCE OF REF VALUES BUT DOESN'T !!! *)
	   (Const_e (Prim.refcell (ref exp)), Prim_c(Ref_c, [con]), true)
       end

     | xvalue context (Prim.tag(tag, il_con))  =
       let
	   val (con, _,_) = xcon context il_con
       in
	   (Const_e (Prim.tag (tag, con)), Prim_c(Exntag_c, [con]), true)
       end

   and xexp context il_exp =
       let
	   val this_call = ! xexp_count
	   val _ = 
	       if (!debug) then
		   (xexp_count := this_call + 1;
		    print ("Call " ^ (Int.toString this_call) ^ " to xexp\n");
		    if (!full_debug) then (Ppil.pp_exp il_exp; print"\n") else ())
	       else ()

	   val result = (xexp' context il_exp)
	       handle e => (if (!debug) then (print ("Exception detected in call " ^ 
						    (Int.toString this_call) ^ " to xexp\n");
					      print "\nwith context = \n";
					      print_splitting_context context;
					      print "\n")
			    else ();
			    raise e)

	in
	    if (!debug) then print ("Return " ^ (Int.toString this_call) ^ " from xexp\n") else ();
	    result
        end


   and xexp' context (Il.OVEREXP(_, true, exp_oneshot)) = 
       xexp context (derefOneshot exp_oneshot)

     | xexp' context (Il.SCON il_scon) = xvalue context il_scon

     | xexp' context (Il.ETAPRIM (prim, il_cons)) = 
       xexp context (Ilutil.prim_etaexpand(prim,il_cons))

     | xexp' context (Il.ETAILPRIM (ilprim, il_cons)) = 
       xexp context (Ilutil.ilprim_etaexpand(ilprim,il_cons))

(* XXX need to handle floats *)
     | xexp' context (il_exp as (Il.PRIM (prim, il_cons, il_args))) = 
       let
	   open Prim
	   val cons = map (#1 o (xcon context)) il_cons
	   val (args, _, valuables) = Listops.unzip3 (map (xexp context) il_args)
           val (effect,con) = 
	     case strip_arrow (Nilprimutil.get_type' prim cons)
	       of SOME (_,effect,_,_,_,con) => (effect,con)
		| _ => (perr_c (Nilprimutil.get_type' prim cons);
			error "Expected arrow constructor")
           val valuable = (effect = Total) andalso (Listops.andfold (fn x => x) valuables)

	   val con : con = (case con of
				Prim_c(Float_c fs,[]) => Prim_c(BoxFloat_c fs,[])
			      | _ => con)
	   fun id (e : exp) = e
	   fun box fs e = Prim_e(NilPrimOp(box_float fs), [], [e])
	   fun unbox fs e = Prim_e(NilPrimOp(unbox_float fs), [], [e])
	   fun float_float fs = (map (unbox fs) args, box fs)
	   fun float_int fs = (map (unbox fs) args, id)
	   fun int_float fs = (args, box fs)
	   val (args,wrap) = 
	       (case prim of
		    ((neg_float fs) | (abs_float fs) |
		     (plus_float fs) | (minus_float fs) |
		     (mul_float fs) | (div_float fs)) => float_float fs
		  | ((less_float fs) | (greater_float fs) |
		     (lesseq_float fs) | (greatereq_float fs) |
		     (eq_float fs) | (neq_float fs)) => float_int fs
		   | float2int => float_int F64
		   | int2float => int_float F64
		   | _ => (args,id))
       in
	   (wrap(Prim_e (PrimOp prim, cons, args)), con, valuable)
       end
(*
     | xexp' context (il_exp as Il.PRIM (prim, il_cons)) = 
       let
	   val cons = map (#1 o (xcon context)) il_cons
	   val il_con = Ilstatic.GetExpCon (HILctx_of context, il_exp)
	   val (con, _) = xcon context il_con
       in
	   (Prim_e (PrimOp prim, cons, NONE), con, true)
       end
*)
     | xexp' context (il_exp as (Il.ILPRIM (ilprim, il_cons, il_args))) = 
       let
	   val cons = map (#1 o (xcon context)) il_cons
	   val (args, _, valuables) = Listops.unzip3 (map (xexp context) il_args)
           val (effect,con) = 
	     case strip_arrow (Nilprimutil.get_iltype' ilprim cons)
	       of SOME (_,effect,_,_,_,con) => (effect,con)
		| _ => (perr_c (Nilprimutil.get_iltype' ilprim cons);
			error "Expected arrow constructor")
           val valuable = (effect = Total) andalso (Listops.andfold (fn x => x) valuables)
       in
	   (Prim_e (PrimOp (xilprim ilprim), cons, args), con, valuable)
       end
(*
     | xexp' context (il_exp as (Il.ILPRIM ilprim)) = 
       let
	   val il_con = Ilstatic.GetExpCon (decs, il_exp)
	   val (con, _) = xcon context il_con
       in
	   (Prim_e (PrimOp (xilprim ilprim), [], NONE), con, true)
       end
*)
     | xexp' context (il_exp as (Il.VAR var)) = 
       let
	   val con = (case (nilcontext_find_con(context, var)) of
			  SOME c => c
			| NONE => (print "could not find var "; Ppil.pp_var var;
				   print "\n";
				   error "bad context to phase-splitter"))
       in
	   (Var_e var, con, true)
       end

     | xexp' context (il_exp as (Il.APP (il_exp1, il_exps2))) = 
       let fun default() = 
	   let
	       val (exp1, con1, valuable1) = xexp context il_exp1
	       val exp_con_va = map (xexp context) il_exps2
	       val exps2 = map #1 exp_con_va
	       val cons2 = map #1 exp_con_va
	       val valuable2 = Listops.andfold #3 exp_con_va
		   
	       val (openness,effect,con) = 
		   (case (nilcontext_con_reduce(context, con1)) of
			AllArrow_c(openness,effect,_,_,_,con) => (openness, effect, con)
		      | nonarrow => (print "Error: could not reduce function type to arrow!\n";
				     print "Expression was :\n";
				     Ppil.pp_exp il_exp;
				     print "\nFunction type was :\n";
				     Ppnil.pp_con con1;
				     print "\nWhich reduces to :\n";
				     Ppnil.pp_con nonarrow;
				     print "\n";
				     error "xexp:  could not reduce function type to arrow!\n"))
	       val valuable = (effect = Total) andalso valuable1 andalso valuable2
	   in  case openness of

	       ExternCode => let 
			   fun split((e,Prim_c(BoxFloat_c fs,[]),_),(nf,f)) = 
			       (nf,f @ [(Prim_e (NilPrimOp (unbox_float fs),
						 [], [e]))])
			     | split((e,_,_),(nf,f)) = (nf @ [e], f)
			   val (nonfloat,float) = foldl split ([],[]) exp_con_va
			   val result = App_e (openness, exp1, [], nonfloat, float)
			   val (result,con) = 
			       (case con of
				    Prim_c(Float_c fs, []) => (Prim_e (NilPrimOp (box_float fs),
								       [], [result]),
							       Prim_c(BoxFloat_c fs, []))
				  | _ => (result,con))
		       in  (result, con, valuable)
		       end
	     | _ => (App_e (openness, exp1, [], exps2, []), con, valuable)
	   end	   
       in (case il_exps2 of
	       [il_exp2] => 
		   (case (Ilutil.beta_reduce(il_exp1,il_exp2)) of
			NONE => default()
		      | SOME il_exp => xexp context il_exp)
	     | _ => default())
       end

     | xexp' context (Il.FIX (is_recur, il_arrow, fbnds)) = 
       let
	   val fbnds'= xfbnds context (is_recur, fbnds)
           val set = Util.list2set fbnds'
           val names = map (fn (var,_) => Var_e var) fbnds'
           val types = map 
                (fn (_,Function(effect,_,_,[(_,con1)],[],_,con2)) =>  
                        AllArrow_c(Open, effect, [], [con1], w0, con2))
                fbnds'
           val num_names = List.length types
           val labels = makeLabels num_names (* already sorted *)
       in
	   if (List.length names = 1) then
               (makeLetE [Fixopen_b set] (hd names),
		hd types, true)
           else
	       (makeLetE [Fixopen_b set]
		         (Prim_e(NilPrimOp (record labels), types, names)),
		Prim_c (Record_cc "5" labels, types), true)
       end

     | xexp' context (Il.RECORD rbnds) = 
       let
	   val (labels, exps, cons, valuable) = xrbnds context rbnds
       in  (* labels already sorted *)
	   (Prim_e (NilPrimOp (record labels), cons, exps),
	    Prim_c (Record_cc "6"  labels, cons), valuable) 
       end

     | xexp' context (il_exp0 as (Il.RECORD_PROJECT (il_exp, label, il_record_con))) =
       let
	   val (exp_record, con_record, valuable) = xexp context il_exp

	   val con = projectFromRecord context con_record [label]
	   val cons = 
	     if !select_carries_types then
	       case strip_record con_record 
		 of SOME (labels,cons) => cons
		  | _ => error "Expected record type"
	     else
	       []
       in
	   (Prim_e (NilPrimOp (select label), cons, [exp_record]), con, valuable)
       end

     | xexp' context (Il.SUM_TAIL (_, il_exp)) =
       let
	   val (exp, Prim_c(Sum_c {known = SOME i, tagcount}, cons), valuable) = xexp context il_exp

       in
	   (Prim_e (NilPrimOp (project_sum {sumtype = i, tagcount = tagcount}), 
		    cons, [exp]),
	    List.nth (cons, TilWord32.toInt (TilWord32.uminus(i,tagcount))), valuable)
       end

     | xexp' context (Il.HANDLE (il_exp1, il_exp2)) = 
       let
	   val (exp1, con, _) = xexp context il_exp1
	   val exp2 = toFunction context il_exp2
       in
	   (Handle_e (exp1, exp2), con, false)
       end

     | xexp' context (Il.RAISE (il_con, il_exp)) = 
       let
	   val (exp, _, _) = xexp context il_exp
	   val (con, _, _) = xcon context il_con
       in
	   (Raise_e (exp, con), con, false)
       end

     | xexp' context (Il.LET (bnds, il_exp)) = 
       let
	   val {cbnd_cat, ebnd_cat, valuable, final_context=context'} = xbnds context bnds
           val cbnd_list = flattenCatlist cbnd_cat
           val ebnds = (map Con_b cbnd_list) @ (flattenCatlist ebnd_cat)
           val cbnds = (map Con_cb cbnd_list)
	   val (exp, con, valuable') = xexp context' il_exp


	   val reduced_con = nilcontext_con_reduce(context', con)


           val _ =
             if (!full_debug) then
	       (print "\nInput to LET:\n";
                Ppil.pp_exp (Il.LET (bnds, il_exp));
                print "\nUnoptimized output from LET:\n";
                Ppnil.pp_exp (Let_e(Sequential, ebnds, exp));
                print "\nOutput from LET:\n";
                Ppnil.pp_exp (makeLetE ebnds exp);
                print "\n")
             else ()

       in
	   (makeLetE ebnds exp,
	    makeLetC cbnds con, valuable andalso valuable')
       end

     | xexp' context (Il.NEW_STAMP il_con) = 
       let
	   val (con, _, _) = xcon context il_con
       in 
	   (Prim_e(NilPrimOp make_exntag, [con], []),
	    Prim_c (Exntag_c, [con]),
	    false)
       end

     | xexp' context (Il.EXN_INJECT (s, il_tag, il_exp)) =
       let
	   val (tag, _, valuable) = xexp context il_tag
	   val (exp, _, valuable') = xexp context il_exp
       in
           (Prim_e (NilPrimOp (inj_exn s), [], [tag, exp]),
	    Prim_c (Exn_c, []),
	    valuable andalso valuable')
       end

     | xexp' context (Il.ROLL (il_con, il_exp)) = 
       let
	   val (con, _, _) = xcon context il_con
	   val (exp, _, valuable) = xexp context il_exp
       in
	   (Prim_e(NilPrimOp roll, [con], [exp]), con, valuable)
       end

     | xexp' context (il_exp as (Il.UNROLL (il_con, il_exp1))) = 
       let
	   val (con, _, _) = xcon context il_con
	   val (exp, _, valuable) = xexp context il_exp1
	   val con' = (case con of
			 Mu_c (flag,vc_seq,v) => Nilutil.muExpand(flag,vc_seq,v)
			 | _ => error "type of unroll is not a mu type")
       in
	   (Prim_e(NilPrimOp unroll, [con], [exp]), con', valuable)
       end

     | xexp' context (Il.INJ {carriers, noncarriers, special, inject=NONE}) =
       let
	   val sumtype = Word32.fromInt special
	   val tagcount = Word32.fromInt noncarriers
	   val cons = map (#1 o (xcon context)) carriers
	   val con = Prim_c(Sum_c{tagcount = tagcount,
				  known = NONE},
			    cons)
       in
	   (Prim_e(NilPrimOp (inject {tagcount = tagcount,
				      sumtype = sumtype}),
		   cons, []),
	    con, true)
       end


     | xexp' context (Il.INJ {carriers, noncarriers, special, inject=SOME il_exp}) =
       let
	   val sumtype = Word32.fromInt special
	   val tagcount = Word32.fromInt noncarriers
	   val cons = map (#1 o (xcon context)) carriers
	   val con = Prim_c(Sum_c{tagcount = tagcount,
				  known = NONE},
			    cons)
	   val (exp, _, valuable) = xexp context il_exp
       in
	   (Prim_e(NilPrimOp (inject {tagcount = tagcount,
				      sumtype = sumtype}),
		   cons, [exp]),
	    con, 
	    valuable)
       end

     | xexp' context (Il.CASE {noncarriers, carriers=il_cons, arg=il_arg, arms=il_arms,
			      tipe,default=il_default}) =
       let
	   val cons = map (#1 o (xcon context)) il_cons
	   val (exp, _, valuable) = xexp context il_arg
	       
	   fun xarms (n, []) = []
             | xarms (n, NONE :: rest) = xarms (n+1, rest)
	     | xarms (n, SOME ilexp :: rest) = 
	       (if (n < noncarriers) then
		    let
			val (exp, con, valuable) = xexp context ilexp
			val effect = if valuable then Total else Partial
		    in
			(Word32.fromInt n, Function(effect, Leaf, [], [], [], exp, con))
		    end
		else
		    (Word32.fromInt n, toFunction context ilexp)) 
		:: (xarms (n+1, rest))

	   val (arms as ((_,Function(_,_,_,_,_,_,con))::_)) = xarms (0, il_arms)

	   val default = (case il_default of 
			      NONE => NONE
			    | SOME e => SOME (#1 (xexp context e)))
       in
	   (Switch_e(Sumsw_e {info = (Word32.fromInt noncarriers, cons), 
			      arg  = exp, arms = arms, default = default}),
	    con, 
	    (* OVERLY CONSERVATIVE!! *)
	    false)
       end

     | xexp' context (e as Il.EXN_CASE {arg = il_exp, arms = il_arms, default = il_default, tipe}) =
       let
	   val (exp, _, _) = xexp context il_exp

	   fun xarms [] = []
             | xarms ((il_tag_exp, _, exp) :: rest) = 
	       (#1 (xexp context il_tag_exp), 
		toFunction context exp) :: (xarms rest)

	   val (arms as ((_,Function(_,_,_,_,_,_,con))::_)) = xarms il_arms

	   val default = (case il_default of 
			      NONE => NONE
			    | SOME e => SOME (#1 (xexp context e)))
       in
	   (Switch_e(Exncase_e {info = (), arg = exp, arms = arms,
				default = default}),
	    con, false)
       end

     | xexp' context (il_exp as (Il.MODULE_PROJECT (module, label))) =
       let

	   
	 val {cbnd_cat, ebnd_cat, 
	      name_c, name_r, 
	      knd_c, type_r,
	      valuable, ...} = xmod context (module, NONE)
	   
	   val specialize = (Name.eq_label(label, Ilutil.it_lab)) andalso 
                            ! elaborator_specific_optimizations

	   val cbnds = flattenCatlist cbnd_cat


	   val bnds = (map Con_b cbnds) @ 
	              (flattenCatlist ebnd_cat)

           val unnormalized_con = 
	       makeLetC (map Con_cb cbnds) 
	                (if specialize then 
			     type_r 
			 else
			     projectFromRecord context type_r [label])

           val con = nilcontext_con_reduce(context, unnormalized_con)


	   val let_body = 
	       if specialize then 
		   name_r
	       else
		 let
		   val cons = 
		     if !select_carries_types then
		       case strip_record type_r 
			 of SOME (labels,cons) => cons
			  | NONE => error "Expected record type"
		     else
		       []
		 in
		   Prim_e (NilPrimOp (select label), cons, [name_r])
		 end

       in
	       (makeLetE bnds let_body, 
		con,
		valuable)
       end

     | xexp' context (Il.SEAL (exp,_)) = xexp context exp

     | xexp' _ _ = error "(xexp) unrecognized expression"

   and xfbnds context (is_recur, fbnds) = 
       let
	   val nil_funtypes =
	       map (fn (Il.FBND(var,_,con,con',_)) => 
		    let val (c,_,_) = xcon context con
			val (c',_,_) = xcon context con'
		    in
			(var, AllArrow_c(Open,Partial,[],[c],w0,c'))
		    end)
	       fbnds

           val funvars = map (#1) nil_funtypes

	   val context' = update_NILctx_insert_con_list(context, nil_funtypes)
	     
	   fun loop [] = []
		     | loop (((_, AllArrow_c(_,_,_,[con1],_,con2)),
	                      Il.FBND(var, var', il_con1, il_con2, body))
	                      :: rest) = 
		       let
	   	          val context'' = update_NILctx_insert_con(context', var', con1)
				       
                          val (body', _, valuable) = xexp context'' body
				 
                          val effect = if valuable then Total else Partial

                          val recursive = if is_recur then Nonleaf else Leaf
					   
	                  val rest' = loop rest
		       in
			   (var, Function(effect, recursive, [],
					  [(var', con1)], [], body', con2))
			   :: rest'
		       end
	       in
		   loop (Listops.zip nil_funtypes fbnds)
	       end

   handle e => (print "uncaught exception in xfbnds\n";
		raise e)

   and xrbnds context [] = ([], [], [], true)
     | xrbnds context ((label, il_exp) :: rest) = 
       let
	   val (exp, con, valuable) = xexp context il_exp
	   val (labels, exps, cons, valuable') = xrbnds context rest
       in
	   (label :: labels, exp :: exps, con :: cons,
	    valuable andalso valuable')
       end

   and xbnds context bnds =
       let
	   val temporary_labels = makeLabels (length bnds)
	   val sbnds = map Il.SBND (Listops.zip temporary_labels bnds)

	   val {final_context, cbnd_cat, ebnd_cat, valuable, record_c_con_items,
		record_c_knd_items, record_c_knd_items_context, 
		record_r_labels, record_r_field_types,
		record_r_exp_items} = 
		xsbnds context sbnds

       in
	   {cbnd_cat = cbnd_cat,
	    ebnd_cat = ebnd_cat,
	    final_context = final_context,
	    valuable = valuable}
       end

   and xsig' context (con0,Il.SIGNAT_FUNCTOR (var, sig_dom, sig_rng, arrow))=
       let

	   val _ = clear_memo var

	   val is_polyfun_sig = 
	       (case sig_rng of
		    Il.SIGNAT_STRUCTURE(_,[Il.SDEC(it_lbl,Il.DEC_EXP _)]) => Name.eq_label(it_lbl,Ilutil.it_lab)
		  | _ => false)

	   val (var_c, var_r, vmap') = splitVar (var, vmap_of context)
	   val (knd_context, knd, con) = xsig context (Var_c var_c, sig_dom)
           val d = Il.DEC_MOD(var, sig_dom)
	      
	   val context' = update_vmap(context, vmap')
	   val context' = update_NILctx_insert_kind(context', var_c, knd_context)
	   val context' = update_NILctx_insert_con(context', var_r, con)
	       
	   val (knd'_context, knd', con') = xsig context' (App_c(con0, [Var_c var_c]), sig_rng)
	       
	   val is_con_unit = is_unit_c con
       in
		 (Arrow_k (Open, [(var_c, knd_context)], knd'_context),
		  Arrow_k (Open, [(var_c, knd)], knd'),
		  AllArrow_c (Open, xeffect arrow, [(var_c, knd)],
			      if is_con_unit andalso !optimize_empty_structure then 
				  [] else [con], w0, con'))

       end

     | xsig' context (con0, ((Il.SIGNAT_STRUCTURE (NONE,sdecs)) |
			     (Il.SIGNAT_INLINE_STRUCTURE {self=NONE,abs_sig=sdecs,...}))) =
       let
	   val {crdecs_context, crdecs, erlabs, ercons} = 
	       xsdecs context (!elaborator_specific_optimizations,true,con0, Subst.empty(), sdecs)
	   val kind = Record_k (Util.list2sequence crdecs)
	   val kind_context = Record_k (Util.list2sequence crdecs_context)
	   val (erlabs,ercons) = 
	       let 
		   val temp = ListMergeSort.sort gt_label_pair
					 (Listops.zip erlabs ercons)
	       in  (map #1 temp, map #2 temp)
	       end
	   val default = (kind_context,kind,Prim_c(Record_cc "7" erlabs, ercons))
       in  (case (!elaborator_specific_optimizations,sdecs,erlabs,ercons) of
		(true,[Il.SDEC(it_lbl,_)],[erlab],[ercon]) =>
		    (if (Name.eq_label(it_lbl,Ilutil.it_lab))
			 then (kind_context,kind,ercon)
		    else default)
	      | _ => default)
       end

     | xsig' context (con0, ((Il.SIGNAT_STRUCTURE (SOME p,sdecs)) |
			     (Il.SIGNAT_INLINE_STRUCTURE {self=SOME p,imp_sig=sdecs,...}))) =
       let val s = Il.SIGNAT_STRUCTURE(SOME p,sdecs)
	   val sig' = IlStatic.UnselfifySig(p,s)
       in  xsig' context (con0, sig')
       end

   and xsig context (con, il_sig) =
       let
	   val this_call = ! xsig_count
	   val _ = 
	       if (!debug) then
		   (xsig_count := this_call + 1;
		    print ("\nCall " ^ (Int.toString this_call) ^ " to xsig\n");
		    if (!full_debug) then (Ppil.pp_signat il_sig; print "\n") else ())
	       else ()
	   val result = xsig' context (con, il_sig)
	       handle e => (if (!debug) then print ("Exception detected in call " ^ 
						    (Int.toString this_call) ^ " to xsig\n") else ();
				raise e)
       in  if (!debug) then print ("Return " ^ (Int.toString this_call) ^ " from xsig\n") else ();
	    result
       end
		    
       
   and xsdecs context (args as (_,_,_,_,sdecs)) =
       let
	   val this_call = ! xsdecs_count
	   val _ = if (! debug) then
	            (xsdecs_count := this_call + 1;
		     print ("Call " ^ (Int.toString this_call) ^ " to xsdecs\n");
		     if (!full_debug) then (Ppil.pp_sdecs sdecs; print "\n";
(*
					    print "\nwith context = \n";
					    nilcontext_print context;
*)
					    print "\n\n") else ())
                   else ()

	   val result = xsdecs'' context args
	       handle e => (if (!debug) then (print ("Exception detected in call " ^ 
						    (Int.toString this_call) ^ " to xsdecs\n");
					      print "\nwith context = \n";
					      print_splitting_context context;
					      print "\n")
			    else ();
				raise e)
		   
       in
	   if (!debug) then print ("Return " ^ (Int.toString this_call) ^ " from xsdecs\n") else ();
	   result
       end

  and xsdecs'' context (elab_spec,firstpass,con0,subst,sdecs) = 
       let 
	   fun loop [] = []
	     | loop (all as (Il.SDEC(lab,_)::rest)) = 
	       if ((Ilutil.is_datatype_lab lab) andalso (! omit_datatype_bindings)) 
		   then loop rest
	       else loop' all
	   and loop' [] = []
	     | loop' ((sdec as 
		     Il.SDEC(lab,Il.DEC_EXP(top_var,il_con))) :: rest) = 
	        if (Util.substring("polyfun",Name.label2string lab)) then
		   let
(*		       val _ = print "entered mono optimization case\n" *)
		       val clist = (case il_con of
					Il.CON_RECORD lclist => map #2 lclist
				      | Il.CON_ARROW _ => [il_con]
				      | _ => error "can't optimize mono fun")
		       val numFunctions = length clist
		       fun getFunctionNames 0 (ls, vs, rest) = (rev ls, rev vs, rest)
			 | getFunctionNames n (ls, vs, Il.SDEC(lbl,Il.DEC_EXP(var,il_module))::rest) = 
			   getFunctionNames (n-1) (lbl::ls, var::vs, rest)
			 | getFunctionNames _ _ = error "xsbnds: Can't optimize mono function"
		       val (external_labels, external_vars, rest) = 
			   getFunctionNames numFunctions (nil, nil, rest)
		       fun make_sdec (lbl,c) = Il.SDEC(lbl,Il.DEC_EXP(Name.fresh_var(),c))
		       val sdecs' = Listops.map2 make_sdec (external_labels,clist)
		   in  sdecs' @ (loop rest)
		   end
	       else
		   sdec::loop rest
	     | loop' ((sdec as 
		     Il.SDEC(lbl,
			     Il.DEC_MOD
			     (top_var, s as
			      Il.SIGNAT_FUNCTOR(poly_var, il_arg_signat,
						Il.SIGNAT_STRUCTURE(_,[Il.SDEC(it_lbl,
									    Il.DEC_EXP(_,il_con))]),
						arrow))))
		     :: rest) = 
	       if ((Name.eq_label (it_lbl, Ilutil.it_lab))
		   andalso (Name.is_label_internal lbl)
		   andalso (Util.substring("polyfun",Name.label2string lbl))
		   andalso (not (Name.eq_label (lbl, Ilutil.expose_lab)))
		   andalso (not (Ilutil.is_eq_lab lbl))) then
		   let
(*		       val _ = print "entered poly optimization case\n" *)
		       val clist = (case il_con of
					Il.CON_RECORD lclist => map #2 lclist
				      | Il.CON_ARROW _ => [il_con]
				      | _ => (print "can't optimize polyfun sdec with il_con =\n";
					      Ppil.pp_con il_con;
					      error "can't optimize polyfun sdec"))
		       val numFunctions = length clist
		       fun getFunctionNames 0 (ls, vs, rest) = (rev ls, rev vs, rest)
			 | getFunctionNames n (ls, vs, Il.SDEC(lbl,Il.DEC_MOD(var,il_module))::rest) = 
			   getFunctionNames (n-1) (lbl::ls, var::vs, rest)
			 | getFunctionNames _ _ = error "xsdecs: Can't optimize poly function sdec"
		       val (external_labels, external_vars, rest) = 
			   getFunctionNames numFunctions (nil, nil, rest)
		       fun make_sdec (l,c) =
			   let val inner_sig =
			       Il.SIGNAT_STRUCTURE(NONE,[Il.SDEC(it_lbl,Il.DEC_EXP(Name.fresh_var(),c))])
			   in  Il.SDEC(l,Il.DEC_MOD(top_var,
						      Il.SIGNAT_FUNCTOR(poly_var, il_arg_signat, inner_sig, arrow)))
			   end
		   val sdecs' = Listops.map2 make_sdec (external_labels,clist)
		   in  sdecs' @ (loop rest)
		   end
	       else
		   sdec::loop rest
	     | loop' (sdec::rest) = sdec::(loop rest)
(*	   val _ = (print "before loop: length sdecs = "; 
print (Int.toString (length sdecs)); print "\n") *)
	   val sdecs' = if (elab_spec andalso firstpass)
			    then loop sdecs
			else sdecs
(* 	   val _ = (print "after loop: length sdecs' = "; 
print (Int.toString (length sdecs')); print "\n") *)
       in  xsdecs' context (elab_spec,firstpass,con0,subst,sdecs')
       end
   
   and xsdecs' context (_,_,con0, _, []) = {crdecs_context = nil, crdecs = nil, erlabs = nil, ercons = nil}

     | xsdecs' context (elab_spec,_,con0, subst,  
		    Il.SDEC(lbl, d as Il.DEC_MOD(var,signat)) :: rest) =
       let
	   val _ = clear_memo var
	   val (var_c, var_r, vmap') = splitVar (var, vmap_of context)
	   val (knd_context, knd, con) = xsig context (Proj_c(con0, lbl), signat)
	       
	   val context' = update_vmap(context, vmap')
	   val context' = update_NILctx_insert_kind(context', var_c, knd_context)
	   val context' = update_NILctx_insert_con(context', var_r, con)

	   val {crdecs_context, crdecs, erlabs, ercons} =
	       xsdecs context' (elab_spec,false,con0, Subst.add subst (var_c, Proj_c(con0, lbl)),
				rest)
       in  {crdecs_context = ((lbl, var_c), knd_context) :: crdecs_context,
	    crdecs = if (!do_kill_cpart_of_functor andalso
			 case knd of
			     Arrow_k(_,_,Record_k seq) => null (Util.sequence2list seq)
			   | _ => false)
			 then crdecs
		     else ((lbl, var_c), knd) :: crdecs,
	    erlabs = lbl :: erlabs,
	    ercons = (Subst.substConInCon subst con) :: ercons}
       end

     | xsdecs' context(elab_spec,_,con0, subst, Il.SDEC(lbl, d as Il.DEC_EXP(var,con)) :: rest) =
       let
	   val (con',_,_) = xcon context con

	   val context' = update_NILctx_insert_con( context, var, con')
	       
	   val {crdecs_context, crdecs, erlabs, ercons} = xsdecs context' (elab_spec,false,con0, subst, rest)
       in
	   {crdecs_context = crdecs_context,
	    crdecs = crdecs,
	    erlabs = lbl :: erlabs,
	    ercons = (Subst.substConInCon subst con') :: ercons}
       end

     | xsdecs' context (elab_spec,_,con0, subst, sdecs as Il.SDEC(lbl, d as Il.DEC_CON(var, knd, 
									maybecon))::rest)=
       let
	   val knd' = xkind context knd
	   val (knd'',knd''_context) = 
	       (case maybecon of
		    NONE => (knd', knd')
		  | SOME il_con => 
			(let val (c,k,k_context) = xcon context il_con
			 in  (Singleton_k(Runtime,k,c),
			      Singleton_k(Runtime,k_context,c))
			 end handle _ => (knd',knd')))

	   val context' = update_NILctx_insert_kind(context, var, knd''_context)
		       
	   val {crdecs_context, crdecs, erlabs, ercons} = 
	       xsdecs context' (elab_spec,false,con0, Subst.add subst (var, Proj_c(con0, lbl)),
				rest)

 	   val kill_con = (!do_kill_cpart_of_functor andalso
			   case knd'' of
			       Arrow_k(_,_,Record_k seq) => null (Util.sequence2list seq)
			     | _ => false)

      in   {crdecs_context = ((lbl, var), knd''_context) :: crdecs_context,
	    crdecs = if kill_con then crdecs else ((lbl, var), knd'') :: crdecs,
	    erlabs = erlabs,
	    ercons = ercons}
       end


   and xkind context (Il.KIND_TUPLE n) = makeKindTuple n
     | xkind context (Il.KIND_ARROW (1,m)) =
         Arrow_k (Open, 
		  [(Name.fresh_var(), Word_k Runtime)], 
		  makeKindTuple m)
     | xkind context (Il.KIND_ARROW (n,m)) = 
          Arrow_k (Open,
		   [(Name.fresh_var(), makeKindTuple n)],
		   makeKindTuple m)
     | xkind context (Il.KIND_INLINE (k,c)) = 
	 let 
	     val (con,knd,knd_context) = xcon context c
	 in  Singleton_k(Runtime, knd, con)
	 end

   fun xHILctx context HILctx =
       let
	   fun folder (v,context) =
	       let val (l,pc) = 
		 case Ilcontext.Context_Lookup'(HILctx,v) of
		      SOME v => v
		    | NONE => error "Variable not found in ilcontext"
	       in  if (Ilutil.is_datatype_lab l andalso (!omit_datatype_bindings))
		       then context
		   else 
		   (case pc of
			Ilcontext.PHRASE_CLASS_EXP (_,il_type) => 
			    let
				val (nil_type,_,_) = xcon context il_type
			    in  update_NILctx_insert_con(context,v, nil_type)
			    end
		      | Ilcontext.PHRASE_CLASS_CON (il_con, il_kind) => 
			    let
				val nil_kind = xkind context il_kind
				val nil_con = 
				    (case il_con of
					 Il.CON_VAR v' => 
					     if (Name.eq_var(v,v'))
						 then NONE
					     else ((SOME (#1 (xcon context il_con))) handle _ => NONE)
				       | _ => ((SOME (#1 (xcon context il_con))) handle _ => NONE))
				val nil_kind' = 
				    (case nil_con of 
					 NONE => nil_kind
				       | SOME c => Singleton_k(Runtime, nil_kind, c))
			    in update_NILctx_insert_kind(context, v, nil_kind)
			    end
		      | Ilcontext.PHRASE_CLASS_MOD (_,il_sig) => 
			    let
				val (v_c, v_r,_) = splitVar (v, vmap_of context)
				val (knd_c_context, knd_c, type_r) = xsig context (Var_c v_c, il_sig)

				val context = update_NILctx_insert_kind(context, v_c, knd_c_context)
				    
			    in	update_NILctx_insert_con(context, v_r, type_r)
			    end
		      | _ => context)
	       end
       in
	   foldl folder context (Ilcontext.Context_Varlist HILctx)
       end

   fun xcompunit HILctx vmap il_sbnds =
       let
           val empty_splitting_context = make_splitting_context vmap

	   val _ = 
	       if (!debug) then
		   (print "\nInitial HIL context varlist:\n";
		    app (fn v => (print "  "; Ppnil.pp_var v; print "\n")) (Ilcontext.Context_Varlist HILctx);
		    print "\n";
		    print "\nInitial HIL context:\n";
		    Ppil.pp_context HILctx;
		    print "\n")
	       else
		   ()

	   val initial_splitting_context = xHILctx empty_splitting_context HILctx

	   val _ = 
	       if (!debug) then
		   (print "\nInitial NIL context:\n";
		    nilcontext_print initial_splitting_context;
		    print "\n")
	       else
		   ()
	   val _ = if (!diag) 
		       then print "tonil.sml: initial splitting context computed\n"
		   else ()
	   val {cbnd_cat, ebnd_cat, final_context, ...} =
	       xsbnds initial_splitting_context il_sbnds

	   val cu_c_list = map Con_b (flattenCatlist cbnd_cat)
	   val cu_r_list = flattenCatlist ebnd_cat

	   val (nil_initial_context,initial_used) = filter_NILctx initial_splitting_context
	   val (nil_final_context,_) = filter_NILctx final_context
       in
	   {nil_initial_context = nil_initial_context,
	    initial_used = initial_used,
	    nil_final_context = nil_final_context,
	    cu_bnds = cu_c_list @ cu_r_list,
	    vmap = vmap_of final_context}
       end

    fun phasesplit (ctxt : Il.context, 
		    sbnd_entries : (Il.sbnd option * Il.context_entry) list) : Nil.module = 
	let
	    val _ = reset_memo()
	    open Nil Ilcontext Il Name IlStatic
	    fun folder((SOME sbnd,CONTEXT_SDEC sdec),(ctxt,sbnds)) = (ctxt, (sbnd,sdec)::sbnds)
	      | folder((NONE, ce),(ctxt,sbnds)) = 
		(add_context_entries(ctxt,
			      [case ce of
				   CONTEXT_SDEC(SDEC(l,dec)) => CONTEXT_SDEC(SDEC(l,SelfifyDec dec))
				 | _ => ce]),
		 sbnds)
	    val (ctxt,rev_sbnd_sdecs) = foldl folder (ctxt,[]) sbnd_entries
	    val sbnds_sdecs = rev rev_sbnd_sdecs
	    val sbnds = map #1 sbnds_sdecs
	    val sdecs = map #2 sbnds_sdecs

	    fun make_cr_labels l = (internal_label((label2string l) ^ "_c"),
				    internal_label((label2string l) ^ "_r"))

            (* obtain all the imports with classifiers from the context *)
	    datatype import_type = ImpExp | ImpType | ImpMod
		
	    fun mapper v =
		let val (l,pc) = valOf (Ilcontext.Context_Lookup'(ctxt,v))
		in                
		    (case pc of
			 PHRASE_CLASS_EXP _ => SOME(ImpExp,v,l)
		       | PHRASE_CLASS_CON _ => SOME(ImpType,v,l)
		       | PHRASE_CLASS_MOD _ => SOME(ImpMod,v,l)
		       | PHRASE_CLASS_SIG _ => NONE
		       | PHRASE_CLASS_OVEREXP _ => NONE)
		end
	    val varlist = Ilcontext.Context_Varlist ctxt
	    val import_temp = List.mapPartial mapper varlist

            (* create a module-variable to pair of variables map from import_modmap *)
	    fun folder ((ImpMod,v,l),map) = let val vc = fresh_named_var (var2string v ^ "myvc")
						val vr = fresh_named_var (var2string v ^ "myvr")
					    in  VarMap.insert(map,v,(vc,vr))
					    end
	      | folder (_,map) = map
	    val import_varmap = foldl folder VarMap.empty import_temp


            (* call the phase splitter *)
	    val {initial_used : (bool ref) Name.VarMap.map,
		 nil_initial_context : NilContext.context , nil_final_context : NilContext.context, 
		 cu_bnds = bnds, vmap = total_varmap} = 
		xcompunit ctxt import_varmap sbnds

(*
val _ = (print "Nil final context is:\n";
	 NilContext.print_context nil_final_context;
	 print "\n\n")
*)

            (* create the imports with classifiers by using the NIL context 
	      returned by the phase splitter *)
	    fun folder ((ImpExp,v,l),imps) = 
		if (!do_kill_dead_import andalso case VarMap.find(initial_used,v) of
			 NONE => error "missing import expvar"
		       | SOME r => !r)
		    then let val c' = (case NilContext.find_con(nil_initial_context,v) of
					   SOME c' => c'
					 | NONE => error "exp var not in NIL context")
			     val _ = Stats.counter("import_live")()
			 in  (ImportValue(l,v,c'))::imps
			 end
		else (Stats.counter("import_dead")(); imps)
	      | folder ((ImpType,v,l),imps) = 
		if (!do_kill_dead_import andalso case VarMap.find(initial_used,v) of
			 NONE => error "missing import expvar"
		       | SOME r => !r)
		then let 
			 val _ = Stats.counter("import_live")() 
    
		    fun strip_var_from_singleton (var,kind) =
			let open Nilutil
			    fun handler (_,Singleton_k(p,k,c)) = 
				if (convar_occurs_free(var,c))
				    then CHANGE_NORECURSE(strip_var_from_singleton(var,k))
				else NOCHANGE
			      | handler _ = NOCHANGE
			    fun nada _ = NOCHANGE
			    val handlers = (nada,nada,nada,nada,handler)
			    val res = kind_rewrite handlers kind
(*
			    val _ = (print "strip_var_from_singleton: "; PpNil.pp_var var;
				     print "\n from k = "; PpNil.pp_kind kind;
				     print "\nresult = "; PpNil.pp_kind res; print "\n\n")
*)
			in  res
			end
		    val (c,k) = Nilstatic.con_valid(nil_initial_context,Var_c v) 
		    val k = strip_var_from_singleton(v,k) (* Singleton_k(Runtime,k,c)) *)
                    val k = if (!do_kill_cpart_of_functor)
				then strip_kind k
			    else k
		  in  (ImportType(l,v,k))::imps
		  end
	        else (Stats.counter("import_dead")(); imps )
	      | folder ((ImpMod,v,l),imps) = 
		if (Ilutil.is_exportable_lab l) (* a label is exportable iff it is importable *)
		    then
			let val (cvar,rvar) = valOf (VarMap.find(import_varmap,v))
			    val (cl,rl) = make_cr_labels l
			in  folder((ImpExp,rvar,rl),folder((ImpType,cvar,cl),imps))
			end
		else imps
(*	    val _ = print "---about to compute imports\n" *)
	    val imports : import_entry list = rev (foldl folder [] import_temp)
(*	    val _ = print "---adone with compute imports\n" *)

	    (* create the export map by looking at the original sbnds;
	      labels that are "exportable" must be exported
             open labels must be recursively unpackaged 
             for module bindings, use the varmap returned by the phase-splitter *)

	    fun folder cr_pathopts ((SDEC(l,dec)),exports) = 
		let open Ilutil
		    fun make_cpath v = (case cr_pathopts of
					   SOME (path,_) => join_path_labels(path,[l])
					 | NONE => SIMPLE_PATH v)
		    fun make_rpath v = (case cr_pathopts of
					   SOME (_,path) => join_path_labels(path,[l])
					 | NONE => SIMPLE_PATH v)

		    fun path2exp (SIMPLE_PATH v) = Var_e v
		      | path2exp (COMPOUND_PATH (v,lbls)) = 
			let fun loop e [] = e
			      | loop e (lbl::lbls) = loop (Prim_e(NilPrimOp (select lbl),[],[e])) lbls
			in loop (Var_e v) lbls
			end
		    fun path2con (SIMPLE_PATH v) = Var_c v
		      | path2con (COMPOUND_PATH (v,lbls)) = 
			let fun loop c [] = c
			      | loop c (lbl::lbls) = loop (Proj_c(c,lbl)) lbls
			in loop (Var_c v) lbls
			end
		in
		    (case (is_exportable_lab l andalso (not (!omit_datatype_bindings) orelse 
							not (Ilutil.is_datatype_lab l)), 
			   false andalso Name.is_label_open l, dec) of
			 (false,false,_) => exports
		       | (true,_,DEC_EXP (v,_)) => 
			     let val e = path2exp (make_rpath v)
				 val (_,c) = Nilstatic.exp_valid(nil_final_context,e)
			     in  (ExportValue(l,e,c)::exports)
			     end
		       | (true,_,DEC_CON (v,_,_)) =>
			     let val c = path2con (make_cpath v)
				 val (_,k) = Nilstatic.con_valid(nil_final_context,c)
				 val k = if (!do_kill_cpart_of_functor)
					     then strip_kind k
					 else k
			     in  if (!do_kill_cpart_of_functor andalso
				     (case k of 
					  (Arrow_k(_,_,Record_k seq)) => null (Util.sequence2list seq)
					| _ => false))
				     then exports
				 else (ExportType(l,c,k)::exports)
			     end
		       | (false,true,DEC_EXP _) => error "DEC_EXP with open label"
		       | (false,true,DEC_CON _) => error "DEC_CON with open label"
		       | (is_export,is_open,DEC_MOD (v,s)) => 
			     let val (lc,lr) = make_cr_labels l
				 val (vc,vr) = (case VarMap.find(total_varmap,v) of
						    SOME vrc => vrc
				                  | NONE => (print "Cannot find variable ";
							Ppnil.pp_var v;
							print "\n";
							error "total_varmap missing bindings"))
(*
				 val _ = (print "v = "; PpNil.pp_var v; print "\n";
					  print "vc = "; PpNil.pp_var vc; print "\n";
					  print "vr = "; PpNil.pp_var vr; print "\n")
*)
				 val rpath = make_rpath vr
				 val cpath = make_cpath vc
				 val exports = 
				     if is_export
					 then
					     let 
(*						 val _ = print "exporting module\n"  *)
						 val er = path2exp rpath
(*						 val _ = (print "er = "; PpNil.pp_exp er; print "\n") *)
						 val (_,cr) = Nilstatic.exp_valid(nil_final_context,er)
(*						 val _ = print "exporting module: type-checked exp\n"  *)
						 val cc = path2con cpath
						 val (_,kc) = Nilstatic.con_valid(nil_final_context,cc)
						 val kc = if (!do_kill_cpart_of_functor)
							     then strip_kind kc
							 else kc
(*						 val _ = print "done exporting module\n"  *)
					     in if (!do_kill_cpart_of_functor andalso
						    (case kc of 
							 (Arrow_k(_,_,Record_k seq)) => 
							     null (Util.sequence2list seq)
						       | _ => false))
						    then 
							(ExportValue(lr,er,cr)::exports)
						else 
						 (ExportValue(lr,er,cr)::
						 ExportType(lc,cc,kc)::
						 exports)
					     end
				     else exports
				 val exports = 
				     (case (is_open,s) of
					  (true,SIGNAT_STRUCTURE (_,sdecs)) =>
					      let val _ = print "exporting open module\n"
						  val res = foldl (folder (SOME (cpath,rpath))) exports sdecs
						  val _ = print "done exporting open module\n"
					      in res
					      end
					| (true, _) => 
					      (print "DEC_MOD (non-structure) with open label:\n";
					       Ppil.pp_signat s; print "\n";
					       error "DEC_MOD (non-structure) with open label")
					| _ => exports)
			     in  exports
			     end)
		end
(*	    val _ = print "---about to compute exports\n" *)
	   val exports : export_entry list = rev(foldl (folder NONE) [] sdecs)
(*	   val _ = print "---done with compute exports\n" *)

	    val nilmod = MODULE{bnds = bnds, 
				imports = imports,
				exports = exports}

	    val _ = reset_memo()
	in  nilmod
	end



end

