(* We box all floats and translate floating point operations accordingly.
   Thus, kind type is replaced by work types.
   Also note that all record fields must be sorted by their labels. *)

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

   open Nil
   val debug = ref false
   val full_debug = ref false
   val trace = ref false
(*
   val debug = ref (Prim_c (Record_c nil, nil))
   val il_debug = ref (Il.CON_ANY)
*)

   val elaborator_specific_optimizations = ref true
   val optimize_empty_structure = ref true
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
   val find2 = Listops.find2

   val perr_c = NilError.perr_c
   val perr_e = NilError.perr_e
   val perr_k = NilError.perr_k
   val perr_c_k = NilError.perr_c_k
   val eq_label = Name.eq_label

   fun gt_label_pair ((l1,_),(l2,_)) = (case Name.compare_label (l1,l2) of
							 GREATER => true
						       | _ => false)
   fun gt_label_pairpair (((l1,_),_),((l2,_),_)) = (case Name.compare_label (l1,l2) of
							GREATER => true
						      | _ => false)
   fun gt_label_triple ((l1,_,_),(l2,_,_)) = (case Name.compare_label (l1,l2) of
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

       fun lookupVmap (var, vmap) = Name.VarMap.find (vmap, var)
	   
   in
       val empty_vmap = Name.VarMap.empty
	   	   
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
      
   datatype 'a catlist =
       LIST of 'a list
     | CONS of 'a * 'a catlist
     | APP of 'a catlist list
   
   fun flattenCatlist ps = 
       let
	   fun flatten' (ps as LIST lst, accum) = lst :: accum
	     | flatten' (CONS (x, ps), accum) = 
	       let
		   val accum' = flatten' (ps, accum)
	       in
		   [x] :: accum'
	       end
	     | flatten' (APP nil, accum) = accum
	     | flatten' (APP (ps::pss), accum) = 
	       let
		   val accum' = flatten' (APP pss, accum)
	       in
		   flatten' (ps,accum')
	       end
       in
	   List.concat (flatten' (ps, nil))
       end

   (* makeLabels.  Returns the list of labels for a tuple of length n. *)

   fun makeLabels n =
       let
	   fun loop i = 
	       if (i > n) then 
		   nil
	       else
		   (Ilutil.generate_tuple_label i) :: (loop (i + 1))
       in
	   loop 1
       end


   (* makeVars.  Returns a list of fresh variables of length n. *)

   fun makeVars n =
       if (n <= 0) then
	   nil
       else
	   Name.fresh_var() :: (makeVars (n-1))


   (* makeKindTuple.  
         Creates the kind for a "tuple" of types of length n. 
    *)
   fun makeKindTuple 1 = Word_k Runtime
     | makeKindTuple n =
       let
	   fun makeFields i =
	       if (i <= n) then
		   ((Ilutil.generate_tuple_label i, Name.fresh_var()), 
		    Word_k Runtime)
                   :: makeFields (i+1)
	       else
		   nil
       in  (* already sorted *)
	   Record_k (Util.list2sequence (makeFields n))
       end

   fun makeLetC nil body = body
     | makeLetC [conbnd as Con_cb(var,_,con)] (cv as Var_c var') =
       if (Name.eq_var(var,var')) then con else cv
     | makeLetC cbnds (cv as Var_c var') =
       (case (List.rev cbnds) of
	    Con_cb(var,_,con)::rest => 
		if (Name.eq_var(var,var')) then
		    makeLetC (rev rest) con
		else
		    Let_c (Sequential, cbnds, cv)
	  | _ => Let_c (Sequential, cbnds, cv))
     | makeLetC cbnds body = Let_c (Sequential, cbnds, body)

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

   fun myunzip lst = 
       let
	   fun loop nil xaccum yaccum = (List.rev xaccum, List.rev yaccum)
	     | loop ((x,y)::rest) xaccum yaccum = loop rest (x::xaccum) (y::yaccum)
       in
	   loop lst nil nil
       end

   fun myunzip3 lst = 
       let
	   fun loop nil xaccum yaccum zaccum = 
	       (List.rev xaccum, List.rev yaccum, List.rev zaccum)
	     | loop ((x,y,z)::rest) xaccum yaccum zaccum = 
	       loop rest (x::xaccum) (y::yaccum) (z::zaccum)
       in
	   loop lst nil nil nil
       end


   fun lookupList eq lst k' =
       let
	   fun loop nil = NONE
	     | loop ((k,v)::xs) = if (eq (k,k')) then SOME v else loop xs
       in
	   loop lst
       end

   val w0 = TilWord32.fromInt 0

   fun Record_cc str labels = if (Name.labels_sorted_distinct labels)
				  then Record_c labels
			      else 
				  (print ("Record_cc failed from " ^ str ^ "\n");
				   app (fn l => (Ppnil.pp_label l; print "\n")) labels;
				   error ("Record_cc failed from " ^ str))

   fun projectFromRecord con [] = con
     | projectFromRecord (Prim_c(Record_c (lbl::lbls), con::cons)) (l' as (lbl'::lbls')) =
       if (Name.eq_label (lbl, lbl')) then 
	   projectFromRecord con lbls'
       else
	   projectFromRecord (Prim_c(Record_cc "1" lbls, cons)) l'
     | projectFromRecord c labels =
       (print "Error: bad projection from constructor ";
	Ppnil.pp_con c;
	print " and labels ";
	app (fn l => (Ppnil.pp_label l; print " ")) labels;
        print "\n";
	error "projectFromRecord: bad projection")
       

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

   val do_kill_cpart_of_functor = ref false
   val xmod_count = ref 0
   val xcon_count = ref 0
   val xexp_count = ref 0
   val xsdecs_count = ref 0
   val xsbnds_count = ref 0

   datatype splitting_context = CONTEXT of {NILctx : Nilcontext.context,
					    vmap   : (var * var) Name.VarMap.map}

   fun NILctx_of (CONTEXT{NILctx,...}) = NILctx
   fun vmap_of (CONTEXT{vmap,...}) = vmap

   fun update_vmap  (CONTEXT{NILctx,vmap}, vmap') = 
       CONTEXT{NILctx=NILctx, vmap=vmap'}
   fun update_NILctx (CONTEXT{NILctx,vmap}, NILctx') =
       CONTEXT{NILctx=NILctx', vmap=vmap}

   fun xmod context (args as (il_mod, _)) =
       let
	   val this_call = ! xmod_count
	   val _ = 
	       if (!debug) then
		   (xmod_count := this_call + 1;
		    
		    print ("\nCall " ^ (Int.toString this_call) ^ " to xmod\n");
		    Ppil.pp_mod il_mod;
		    (* print"\n";
		     Nilcontext.print_context (NILctx_of context); *)
		    print"\n")
	       else ()

	   val result = xmod' context args
	       handle e => (if (!debug) then print ("Exception detected in call " ^ 
						    (Int.toString this_call) ^ " to xmod\n") else ();
			    raise e)
	in
	    if (!debug) then print ("Return " ^ (Int.toString this_call) ^ " from xmod\n") else ();
	    result
        end

   and xmod' context (il_mod as (Il.MOD_VAR var_mod), preferred_name) = 
       let
	   val (var_mod_c, var_mod_r, vmap') = splitVar (var_mod, vmap_of context)

	   val (name_c, name_r) = 
	       (case preferred_name of
		    NONE => (Var_c var_mod_c, Var_e var_mod_r)
		  | SOME (_, name_c, name_r) => (Var_c name_c, Var_e name_r))

           val _ = if (!debug)
		       then (print "About to look up :\n";
			     Ppnil.pp_exp (Var_e var_mod_r);
			     print " and ";
			     Ppnil.pp_con (Var_c var_mod_c);
			     print "\n")
		   else ()

           val knd_c = 
	     case Nilcontext.find_kind (NILctx_of context, var_mod_c)
	       of SOME knd => knd
		| _ => (print "Variable: ";
			Ppnil.pp_var var_mod_c;
			error "Constructor variable not found in context")
           val type_r = 
	     case Nilcontext.find_con (NILctx_of context, var_mod_r)
	       of SOME con => con
		| _ => (print "Variable: ";
			Ppnil.pp_var var_mod_r;
			error "Expression variable not found in context")

	   val (cbnd_cat, ebnd_cat) =
	       (case preferred_name of
		    NONE => (LIST nil, LIST nil)
		  | SOME (_, name_c, name_r) => 
			(LIST [(name_c, knd_c, Var_c var_mod_c)], 
			 LIST [Exp_b (name_r, type_r, Var_e var_mod_r)]))
       in
	   {cbnd_cat = cbnd_cat,
	    ebnd_cat = ebnd_cat,
            name_c   = name_c,
            name_r   = name_r,
	    knd_c    = knd_c,
	    type_r   = type_r,
	    valuable = true}
       end

     | xmod' context (Il.MOD_APP(ilmod_fun, ilmod_arg), preferred_name) =
       let

	   val (var, var_c, var_r, vmap) = chooseName (preferred_name, vmap_of context)

	   val _ = 
	     if (!trace) then
	       (lprintl "MOD IS";
		Ppil.pp_mod ilmod_fun)
	     else
	       ()
	   val {cbnd_cat = cbnd_cat_fun,
		ebnd_cat = ebnd_cat_fun,
		name_c = name_fun_c,
                name_r = name_fun_r,
		knd_c = knd_fun_c,
		type_r = type_fun_r,
		valuable = valuable_fun
		} = xmod context (ilmod_fun, NONE)

(*
val _ = (print "\nMOD_APP: type_fun_r = \n";
	 Ppnil.pp_con type_fun_r; print "\n\n\n")
*)
	   val {cbnd_cat = cbnd_cat_arg,
		ebnd_cat = ebnd_cat_arg,
                name_c = name_arg_c,
                name_r = name_arg_r,
		knd_c = knd_arg_c,
		type_r = type_arg_r,
		valuable = valuable_arg
		} = xmod context (ilmod_arg, NONE)
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

	       val argument_c = makeLetC (map Con_cb (flattenCatlist cbnd_cat_arg)) name_arg_c
	       val reduced_argument_c = Nilstatic.con_reduce (NILctx_of context, argument_c)
               val (effect,var_body_arg_c,exp_body_type) = 
		 case strip_arrow type_fun_r 
		   of SOME (_,effect,[(var_body_arg_c,_)],_,_,exp_body_type) =>
		     (effect,var_body_arg_c,exp_body_type)
		    | _ => (perr_c type_fun_r;
			    error "Expected arrow constructor with one arg")
	   in

	     val knd_c = Nilstatic.kind_reduce 
	       (NILctx_of context,
		Subst.varConKindSubst v_c reduced_argument_c con_body_kind)



	     val cbnd_cat = 
		 APP[cbnd_cat_fun, 
		     cbnd_cat_arg,
		     if (!do_kill_cpart_of_functor andalso
			 case knd_c of
			     (Record_k seq) => null (Util.sequence2list seq)
			   | _ => false)
			 then LIST[(var_c, knd_c, Crecord_c[])]
		     else LIST[(var_c, knd_c, App_c(name_fun_c,[name_arg_c]))]
		     ]
		   
(*
               val _ = (print "ZZZ\n";
			print "type_fun_r = ";
			Ppnil.pp_con type_fun_r;
			print "\n";
			print "name_arg_c = ";
			Ppnil.pp_con name_arg_c;
			print "\n";
			print "constructor bindings = ";
			Ppnil.pp_bnds (map Con_b (flattenCatlist cbnd_cat));
			print "\n";
			print "exp_body_type = ";
			Ppnil.pp_con exp_body_type;
			print "\n")
*)

	     val NILctx = NILctx_of context
	     val is_var = (case ilmod_arg of
			       Il.MOD_VAR _ => true 
			     | _ => false)
	     val NILctx' = (case (is_var,Nilcontext.find_kind(NILctx, var_arg_c)) of
				(_,NONE) => Nilcontext.insert_kind(NILctx, var_arg_c, knd_arg_c)
			      | (true, SOME _) => NILctx
			      | _ => error "variable already in context and not a MOD_VAR")
	     val NILctx'' = (case (is_var,Nilcontext.find_con(NILctx', var_arg_r)) of
				 (_,NONE) => Nilcontext.insert_con(NILctx', var_arg_r, type_arg_r)
			       | (true, SOME _) => NILctx'
			       | _ => error "variable already in context and not a MOD_VAR")
		 
(*
val _ = (print "-----about to compute type_r;  exp_body_type =\n";
	 Ppnil.pp_con exp_body_type;
	 print "\n-----about to compute type_r = con_reduce of\n";
	 Ppnil.pp_con (Subst.varConConSubst var_body_arg_c name_arg_c exp_body_type);
	 print "\n\n")
*)
	       val type_r = Nilstatic.con_reduce 
		 (NILctx'', Subst.varConConSubst var_body_arg_c name_arg_c exp_body_type)
	       val valuable = (effect = Total) andalso valuable_fun andalso valuable_arg 
	   end  

           val ebnd_cat = APP[ebnd_cat_fun, 
			      ebnd_cat_arg,
			      LIST[Exp_b(var_r, type_r,
					 App_e(Open,
					       name_fun_r,
					       [name_arg_c],
					       if (is_type_arg_unit andalso !optimize_empty_structure)
						   then []
					       else [name_arg_r], 
					       []))]]
       in
	   {cbnd_cat  = cbnd_cat,
	    ebnd_cat  = ebnd_cat,
            name_c    = name_c,
	    name_r    = name_r,
	    knd_c     = knd_c,
	    type_r    = type_r,
	    valuable  = valuable}
       end
   
     | xmod' context (Il.MOD_SEAL(il_mod,_), preferred_name) = 
       (* The phase-splitting breaks abstraction *)
       xmod context (il_mod, preferred_name)
    
     | xmod' context (initial_mod as (Il.MOD_PROJECT _), preferred_name) =
       let
           val (il_module, lbls) = extractPathLabels initial_mod

	   val {cbnd_cat = cbnd_mod_cat, 
		ebnd_cat = ebnd_mod_cat,
		name_c   = name_mod_c, 
		name_r   = name_mod_r,
		knd_c    = knd_mod_c,
		type_r   = type_mod_r,
		valuable = mod_valuable, 
		...} = xmod context (il_module, NONE)

	   val (var_proj, var_proj_c, var_proj_r, vmap) = 
	       chooseName (preferred_name, vmap_of context)

	   val name_proj_c = Var_c var_proj_c
	   val name_proj_r = Var_e var_proj_r

	   local
	       val Var_c var_mod_c = name_mod_c
	       val labels_r = (case strip_record type_mod_r 
				 of SOME (labels,cons) => labels
				  | _ => [])
	       fun mapper ((l,v),_) = if (Listops.member_eq(Name.eq_label,l,labels_r))
					  then NONE else SOME(v,Proj_c(name_mod_c,l))
	       val table = 
		   (case (strip_singleton knd_mod_c) of
			Record_k lvk_seq => List.mapPartial mapper (Util.sequence2list lvk_seq)
		      | _ => [])
	       val subst = Subst.fromList table
(*
	       val _ = (print "knd_mod_c is "; Ppnil.pp_kind knd_mod_c; print "\n")
	       val _ = (print "type_mod_r before is "; Ppnil.pp_con type_mod_r; print "\n")
*)
	       val type_mod_r = Subst.substConInCon subst type_mod_r
(*	       val _ = (print "type_mod_r after is "; Ppnil.pp_con type_mod_r; print "\n") *)
(*	       val type_mod_r' = Nilstatic.con_reduce(NILctx_of context, type_mod_r) *)
	       val type_mod_r' = type_mod_r
	   in

	       val con_proj_c = selectFromCon(name_mod_c, lbls)

	       val (con_proj_c,knd_proj_c) = 
		 Nilstatic.con_valid(NILctx_of context,con_proj_c)

(*
	       val _ = (print "calling projectFromRecord with type_mod_r' = ";
			Ppnil.pp_con type_mod_r'; print "\n and hd lbls = ";
			Ppnil.pp_label (hd lbls); print "\n")
*)
	       val type_proj_r = projectFromRecord type_mod_r' lbls
	   end


           val cbnd_proj_cat = APP[cbnd_mod_cat,
				   LIST [(var_proj_c, knd_proj_c,con_proj_c)]]

	   val ebnd_proj_cat = APP[ebnd_mod_cat,
				   LIST [Exp_b(var_proj_r, type_proj_r,
					       selectFromRec(name_mod_r,type_mod_r,
							     lbls))]]
       in
	   {cbnd_cat = cbnd_proj_cat,
	    ebnd_cat = ebnd_proj_cat,
            name_c   = name_proj_c,
	    name_r   = name_proj_r,
	    knd_c    = knd_proj_c,
	    type_r   = type_proj_r,
	    valuable = mod_valuable}
       end

     | xmod' context (Il.MOD_FUNCTOR(var_arg, il_arg_signat, ilmod_body), 
		    preferred_name) =
       let
	   (* Split the argument parameter *)
	   val (var_arg_c, var_arg_r, vmap') = splitVar (var_arg, vmap_of context)
	   val (knd_arg, con_arg) = xsig context (Var_c var_arg_c, il_arg_signat)
	   val is_con_arg_unit = is_unit_c con_arg 

           val context' = update_vmap(context, vmap')

           (* Split the functor body *)
		
	   val {cbnd_cat = cbnd_body_cat, 
		ebnd_cat = ebnd_body_cat, 
		name_c = name_body_c,
		name_r = name_body_r,
		knd_c = knd_body_c,
		type_r = type_body_r,
		valuable = body_valuable
		} = let
			fun cont1 NILctx' =
			    Nilcontext.c_insert_con(NILctx', var_arg_r, con_arg, cont2)

	                and cont2 NILctx'' =
			    let
				val context'' = 
				    update_NILctx
				     (context', NILctx'')
			    in
				xmod context'' (ilmod_body, NONE)
			    end
		    in
			Nilcontext.c_insert_kind(NILctx_of context, var_arg_c, knd_arg, cont1)
		    end

	   val (arrow, effect) = 
	       if body_valuable then
		   (Il.TOTAL, Total)
	       else 
		   (Il.PARTIAL, Partial)

	   val (var_fun, var_fun_c, var_fun_r, _) = 
	       chooseName (preferred_name, vmap_of context)

           val name_fun_c = Var_c var_fun_c
	   val name_fun_r = Var_e var_fun_r

           val cbnds_body = flattenCatlist cbnd_body_cat
           val ebnds_body = flattenCatlist ebnd_body_cat

           val local_name_fun_c = Name.fresh_var ()

	   val knd_fun_c = Arrow_k(Open, [(var_arg_c, knd_arg)], knd_body_c)
	   val type_fun_r = AllArrow_c(Open, effect, [(var_arg_c, knd_arg)], 
				       if (is_con_arg_unit andalso !optimize_empty_structure)
					   then []
				       else [con_arg], 
				       w0,
				       type_body_r)

           val cbnd_fun_cat = 
	       LIST[(var_fun_c, knd_fun_c,
		     makeLetC [Open_cb(local_name_fun_c, [(var_arg_c, knd_arg)],
				       makeLetC (map Con_cb cbnds_body) name_body_c,
				       knd_body_c)]
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
					 Let_e(Sequential,
					       (map Con_b cbnds_body) @ ebnds_body,
					       name_body_r),
					 type_body_r))])]


       in
	   {cbnd_cat = cbnd_fun_cat,
            ebnd_cat = ebnd_fun_cat,
	    name_c = name_fun_c,
	    name_r = name_fun_r,
	    knd_c = knd_fun_c,
	    type_r = type_fun_r,
	    valuable = true}
       end
   
     | xmod' context (Il.MOD_STRUCTURE sbnds, preferred_name) =
       let
	   val (var_str, var_str_c, var_str_r, _) = 
	       chooseName (preferred_name, vmap_of context)

	   val {final_context, cbnd_cat, ebnd_cat, valuable, record_c_con_items,
		record_c_knd_items, record_r_labels, record_r_field_types,
		record_r_exp_items} = 
		xsbnds context (!elaborator_specific_optimizations, true, sbnds)

	   fun mapper (Il.SBND(l,Il.BND_CON(v,_))) = SOME(v,Proj_c(Var_c var_str_c,l))
	     | mapper (Il.SBND(l,Il.BND_MOD(v,_))) = SOME(v,Proj_c(Var_c var_str_c,l))
	     | mapper _ = NONE
	   val table = List.mapPartial mapper sbnds
	   val subst = Subst.fromList table
	   val record_r_field_types = map (Subst.substConInCon subst) record_r_field_types
	   val (record_r_labels,record_r_field_types,record_r_exp_items) = 
	       let 
		   val temp = (ListMergeSort.sort gt_label_triple
			       (Listops.zip3 record_r_labels record_r_field_types record_r_exp_items))
	       in  (map #1 temp, map #2 temp, map #3 temp)
	       end

           val name_str_c = Var_c var_str_c
	   val name_str_r = Var_e var_str_r

           val record_c_knd_items = ListMergeSort.sort gt_label_pairpair record_c_knd_items
           val record_c_con_items = ListMergeSort.sort gt_label_pair record_c_con_items

	   val knd_str_c = Record_k (Util.list2sequence record_c_knd_items)
           val cbnd_str_cat = 
	       APP[cbnd_cat,
		   LIST [(var_str_c, knd_str_c, Crecord_c record_c_con_items)]]

           val specialize =
	       (case (!elaborator_specific_optimizations, sbnds) of
		    (true, [Il.SBND(lab, Il.BND_EXP _)]) => Name.eq_label (lab, Ilutil.it_lab)
		  | _ => false)

	   val type_str_r = 
	       if specialize then
		   hd record_r_field_types
	       else
		   Prim_c(Record_cc "2" record_r_labels, record_r_field_types)

	   val ebnd_str_cat = 
	       if specialize then
		   APP[ebnd_cat,
		       LIST[Exp_b (var_str_r, type_str_r, hd record_r_exp_items)]]
	       else
		   APP[ebnd_cat,
		       LIST[Exp_b (var_str_r, type_str_r,
				   Prim_e (NilPrimOp (record record_r_labels),
					   record_r_field_types, record_r_exp_items))]]

       in
	   {cbnd_cat = cbnd_str_cat,
	    ebnd_cat = ebnd_str_cat,
            name_c = name_str_c,
	    name_r = name_str_r,
	    knd_c = knd_str_c,
	    type_r = type_str_r,
	    valuable = valuable}
       end

    | xmod' context (il_let_mod as (Il.MOD_LET (var_loc, il_loc_mod, il_body_mod)),
		   preferred_name) =
       let
	   val (var_loc_c, var_loc_r, vmap') = splitVar (var_loc, vmap_of context)

	   val {cbnd_cat = cbnd_loc_cat,
		ebnd_cat = ebnd_loc_cat,
		knd_c = knd_loc_c,
		type_r = type_loc_r,
		valuable = loc_valuable,
	        ...} = xmod context (il_loc_mod, 
				   SOME (var_loc, var_loc_c, var_loc_r))

	   val {cbnd_cat = cbnd_body_cat,
		ebnd_cat = ebnd_body_cat,
		name_c = name_let_c,
		name_r = name_let_r,
		knd_c = knd_let_c,
		type_r = type_let_r,
		valuable = body_valuable,
                ...} =  let
			    fun cont1 NILctx' =
				Nilcontext.c_insert_con(NILctx',var_loc_r, type_loc_r, cont2)
				
			    and cont2 NILctx'' =
				let
				    val context' = 
					update_NILctx
					(update_vmap(context,vmap'), NILctx'')
				in
				    xmod context' (il_body_mod, preferred_name)
				end
			in
			    Nilcontext.c_insert_kind
			    (NILctx_of context, var_loc_c, knd_loc_c, cont1)
			end
		    
           val cbnd_let_cat = APP[cbnd_loc_cat, cbnd_body_cat]
           val ebnd_let_cat = APP[ebnd_loc_cat, ebnd_body_cat]

       in
	   {cbnd_cat = cbnd_let_cat,
	    ebnd_cat = ebnd_let_cat,
            name_c = name_let_c,
	    name_r = name_let_r,
	    knd_c = knd_let_c,
	    type_r = type_let_r,
	    valuable = loc_valuable andalso body_valuable}
       end

   and xsbnds context (args as (flag1, flag2, il_sbnds)) =
       let
	   val this_call = ! xcon_count
	   val _ = 
	       if (!debug andalso !full_debug) then
		   (xcon_count := this_call + 1;
		    print ("Call " ^ (Int.toString this_call) ^ " to xsbnds, optimize= ");
                    print (Bool.toString flag1);
		    print (" and firstpass= ");
		    print (Bool.toString flag2);
		    print ("\n");
		    Ppil.pp_sbnds il_sbnds;
		    print ("\n"))
	       else ()

	   val result = (xsbnds' context args)
	       handle e => (if (!debug) then print ("Exception detected in call " ^ 
						    (Int.toString this_call) ^ " to xsbnds\n") else ();
			    raise e)

	in
	    if (!debug andalso !full_debug) then 
               print ("Return " ^ (Int.toString this_call) ^ " from xsbnds\n") else ();
	    result
        end

   and xsbnds' context (_, _, []) =
       {final_context = context,
	cbnd_cat = LIST nil,
	ebnd_cat = LIST nil,
	valuable = true,
	record_c_con_items = nil,
	record_c_knd_items = nil,
	record_r_labels = nil,
	record_r_field_types = nil,
	record_r_exp_items = nil}

     | xsbnds' context (true, true, sbnds as 
		       Il.SBND(lbl, Il.BND_EXP(top_var, il_exp as Il.FIX(is_recur, arrow, fbnds)))
			       :: rest) =
       (if (Util.substring("polyfun",Name.var2string top_var)) then
				  
	   let
	       val numFunctions = length fbnds
	       fun getFunctionNames 0 (ls, vs, rest) = (rev ls, rev vs, rest)
                 | getFunctionNames n (ls, vs, Il.SBND(lbl,Il.BND_EXP(var,il_exp))::rest) = 
		       getFunctionNames (n-1) (lbl::ls, var::vs, rest)
                 | getFunctionNames _ _ = error "xsbnds: Can't optimize mono function"
		   
	       val (external_labels, external_vars, rest') = 
		   getFunctionNames numFunctions (nil, nil, rest)

	       val (Let_e (_, [Fixopen_b set], _), _, _) = xexp context il_exp

               val (internal_vars, functions) = myunzip (Util.set2list set)

	       val subst = Subst.fromList (Listops.map2 (fn (iv,ev) => (iv,Var_e ev)) 
					   (internal_vars,external_vars))

               fun reviseFunction (external_var,
				   Function(effect,recursive,[],
					    vclist,[],body,body_con)) =
		   let val body' = Subst.substExpInExp subst body
		   in  (external_var,
		       Function(effect,recursive,[],
                       	        vclist,	[], body', body_con))
                   end


               val ebnd_entries = Listops.map2 reviseFunction (external_vars, functions)

               val ebnd_types = map (fn (_,Function(_,_,carg,vclist,w,_,body_type)) =>
				     AllArrow_c(Open,Total,carg,map #2 vclist,
						Word32.fromInt (List.length w),body_type)) 
                                    ebnd_entries

	       val ebnds = [Fixopen_b (Util.list2set ebnd_entries)]

               val nil_edecs = map (fn (n_r,Function(effect,_,a1,a2,_,_,a4)) =>
				    (n_r, AllArrow_c(Open,effect,a1,map #2 a2,w0,a4)))
		                   ebnd_entries


               val context'' = 
		   update_NILctx
		    (context,
		     Nilcontext.c_insert_con_list 
	             (NILctx_of context,
		     nil_edecs, fn x => x))

	       val {final_context, cbnd_cat, ebnd_cat, valuable, record_c_con_items,
		    record_c_knd_items, record_r_labels, record_r_field_types,
		    record_r_exp_items} = xsbnds context'' (true, true, rest')
		    
	   in
	       {final_context = final_context,
		cbnd_cat = cbnd_cat,
		ebnd_cat = APP [LIST ebnds, ebnd_cat],
		valuable = valuable,
		record_c_con_items = record_c_con_items,
		record_c_knd_items = record_c_knd_items,
		record_r_labels = external_labels @ record_r_labels,
		record_r_field_types = ebnd_types @ record_r_field_types,
		record_r_exp_items = (map Var_e external_vars) @ record_r_exp_items}
	   end
       else
	   xsbnds context (true,false,sbnds))

     | xsbnds' context (do_optimize, _, Il.SBND(lbl, Il.BND_EXP(var, il_exp)) :: rest) =
       let
	   val (exp, con, exp_valuable) = xexp context il_exp

           val context' = 
	       update_NILctx(context,
			      Nilcontext.insert_con(NILctx_of context, var, con))

	   val {final_context, cbnd_cat, ebnd_cat, valuable, record_c_con_items,
		record_c_knd_items, record_r_labels, record_r_field_types,
		record_r_exp_items} = xsbnds context' (do_optimize, true, rest)
       in
	   {final_context = final_context,
	    cbnd_cat = cbnd_cat,
	    ebnd_cat = APP[LIST [Exp_b(var,con,exp)], ebnd_cat],
	    valuable = valuable andalso exp_valuable,
	    record_c_con_items = record_c_con_items,
	    record_c_knd_items = record_c_knd_items,
	    record_r_labels = lbl :: record_r_labels,
	    record_r_field_types = con :: record_r_field_types,
	    record_r_exp_items = (Var_e var) :: record_r_exp_items}
       end

     | xsbnds' context (do_optimize, _, Il.SBND(lbl, Il.BND_CON(var, il_con)) :: rest) =
       let
	   val (var,rest) = (case Nilcontext.find_kind(NILctx_of context, var) of
				NONE => (var,rest)
			      | SOME _ => let val v = Name.derived_var var
					      val table = [(var, Il.CON_VAR v)]
				 	      val Il.MOD_STRUCTURE rest' = 
							Ilutil.mod_subst_convar(Il.MOD_STRUCTURE rest,table)
					  in (v,rest')
					  end)

	   val (con, knd) = xcon context il_con

           val context' = 
	       update_NILctx(context,
			     Nilcontext.insert_kind(NILctx_of context, var, knd))

	   val {final_context, cbnd_cat, ebnd_cat, valuable, record_c_con_items,
		record_c_knd_items, record_r_labels, record_r_field_types,
		record_r_exp_items} = xsbnds context' (do_optimize, true, rest)
       in
	   {final_context = final_context,
	    cbnd_cat = APP [LIST [(var, knd, con)], cbnd_cat],
	    ebnd_cat = ebnd_cat,
	    valuable = valuable,
	    record_c_con_items = (lbl, Var_c var) :: record_c_con_items,
	    record_c_knd_items = ((lbl, Name.fresh_var ()), knd) :: record_c_knd_items,
	    record_r_labels = record_r_labels,
	    record_r_field_types = record_r_field_types,
	    record_r_exp_items = record_r_exp_items}
       end


     | xsbnds' context (true, true, sbnds as 
		       Il.SBND(lbl, 
			       Il.BND_MOD
			       (top_var, m as 
				Il.MOD_FUNCTOR(poly_var, il_arg_signat, 
					       Il.MOD_STRUCTURE
					       [Il.SBND(it_lbl,
							Il.BND_EXP
							(_, il_exp as Il.FIX(is_recur, arrow, fbnds)))])))
			       :: rest) =
       if ((Name.eq_label (it_lbl, Ilutil.it_lab))
	   andalso ((Name.is_label_internal lbl)  (* this or is needed to handler bnds *)
		    orelse (Util.substring("polyfun",Name.var2string top_var)))
	   andalso (not (Name.eq_label (lbl, Ilutil.expose_lab)))
	   andalso (not (Ilutil.is_eq_lab lbl))) then
				  
	   let
	       val numFunctions = length fbnds
	       fun getFunctionNames 0 (ls, vs, rest) = (rev ls, rev vs, rest)
                 | getFunctionNames n (ls, vs, Il.SBND(lbl,Il.BND_MOD(var,il_mod))::rest) = 
		       getFunctionNames (n-1) (lbl::ls, var::vs, rest)
                 | getFunctionNames _ _ = error "xsbnds: Can't optimize poly function"
		   
	       val (external_labels, external_vars, rest') = 
		   getFunctionNames numFunctions (nil, nil, rest)

               val (external_vars_c, external_vars_r, vmap') = 
		   let fun loop [] (ns_c, ns_r, vmap) = (rev ns_c, rev ns_r, vmap)
			 | loop (n::ns) (ns_c, ns_r, vmap) =
			   let val (n_c, n_r, vmap') = splitVar (n, vmap)
			   in
			       loop ns (n_c :: ns_c, n_r :: ns_r, vmap')
			   end
		   in
		       loop external_vars (nil, nil, vmap_of context)
		   end

	       val (poly_var_c, poly_var_r, vmap'') = splitVar (poly_var, vmap')
	       val (knd_arg, con_arg) = xsig context (Var_c poly_var_c, il_arg_signat)
	       val is_con_arg_unit = is_unit_c con_arg 

	       val context' = 
		   update_NILctx(update_vmap(context, vmap''),
				  Nilcontext.insert_con
				  (Nilcontext.insert_kind
				   (NILctx_of context, poly_var_c, knd_arg),
				   poly_var_r, con_arg))

	       val (Let_e (_, [Fixopen_b set], _), _, _) = xexp context' il_exp

               val (internal_vars, functions) = myunzip (Util.set2list set)
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
			      (App_e(Open, Var_e n, 
				     [Var_c poly_var_c], 
				     if (is_con_arg_unit andalso !optimize_empty_structure)
				       then []
				     else [Var_e poly_var_r], 
				       [])))
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
		       Let_c (Sequential,
			      [Open_cb(var'', 
				      [(poly_var_c, knd_arg)], 
				      Crecord_c [],
				      Record_k (Util.list2sequence []))],
			      Var_c var'')
		   end

               val nullfunction_k =
		   Arrow_k(Open, [(poly_var_c, knd_arg)], Record_k (Util.list2sequence []))

               val cbnds = 
		   if !do_kill_cpart_of_functor
		       then []
		   else
		       map (fn n_c => ((n_c, nullfunction_k, nullfunction_c)))
		       external_vars_c

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


               val context'' = 
		   update_NILctx
		    (update_vmap(context, vmap'),
		     Nilcontext.c_insert_con_list 
		     (Nilcontext.c_insert_kind_list
		      (NILctx_of context, nil_cdecs, fn x => x),
		      nil_edecs, fn x => x))

               val dummy_vars = map (fn _ => Name.fresh_var()) external_vars_c
		    
	       val {final_context, cbnd_cat, ebnd_cat, valuable, record_c_con_items,
		    record_c_knd_items, record_r_labels, record_r_field_types,
		    record_r_exp_items} = xsbnds context'' (true, true, rest')
		    
	   in
	       {final_context = final_context,
		cbnd_cat = APP [LIST cbnds, cbnd_cat],
		ebnd_cat = APP [LIST ebnds, ebnd_cat],
		valuable = valuable,
		record_c_con_items = (Listops.zip external_labels (map Var_c external_vars_c))
		                     @ record_c_con_items,
		record_c_knd_items = (Listops.zip (Listops.zip external_labels dummy_vars) cbnd_knds)
                                     @ record_c_knd_items,
		record_r_labels = external_labels @ record_r_labels,
		record_r_field_types = ebnd_types @ record_r_field_types,
		record_r_exp_items = (map Var_e external_vars_r) @ record_r_exp_items}
	   end
       else
	   xsbnds context (true,false,sbnds)

     | xsbnds' context (do_optimize, _, Il.SBND(lbl, Il.BND_MOD(var, il_mod)) :: rest) =
       let
	   val (var_c, var_r, vmap') = splitVar (var, vmap_of context)
	   val {ebnd_cat, cbnd_cat, valuable, knd_c, type_r,...} = 
	       xmod context (il_mod, SOME (var, var_c, var_r))

           val context' = 
	       update_NILctx
		(update_vmap(context, vmap'),
		 Nilcontext.insert_kind
		 (Nilcontext.insert_con(NILctx_of context, var_r, type_r), var_c, knd_c))

	   val {final_context, cbnd_cat=cbnd_cat', ebnd_cat=ebnd_cat', 
                valuable=valuable', record_c_con_items, record_c_knd_items, 
                record_r_labels, record_r_field_types,
		record_r_exp_items} = xsbnds context' (do_optimize, true, rest)
       in
	   {final_context = final_context,
	    cbnd_cat = APP [cbnd_cat, cbnd_cat'],
	    ebnd_cat = APP [ebnd_cat, ebnd_cat'],
	    valuable = valuable andalso valuable',
	    record_c_con_items = (lbl, Var_c var_c) :: record_c_con_items,
	    record_c_knd_items = ((lbl, Name.fresh_var()), knd_c) :: record_c_knd_items,
	    record_r_labels = lbl :: record_r_labels,
	    record_r_field_types = type_r :: record_r_field_types,
	    record_r_exp_items = (Var_e var_r) :: record_r_exp_items}
       end

   and xflexinfo context (ref (Il.INDIRECT_FLEXINFO f)) = 
       xflexinfo context f
     | xflexinfo context (ref (Il.FLEXINFO(_,true, recs))) = 
       let
	   val (lbls, cons) = xrdecs context recs
	   val con = Prim_c(Record_cc "3" lbls, cons) (* already sorted *)
       in
	   (con, Singleton_k (Runtime, Word_k Runtime, con))
       end

   and xrdecs context [] = ([], [])
     | xrdecs context ((lab, il_con) :: rest) = 
       let
	   val (labs, cons) = xrdecs context rest
	   val (con, _) = xcon context il_con
       in
	   (lab :: labs, con :: cons)
       end

   and xcon context il_con =
       let
	   val this_call = ! xcon_count
	   val _ = 
	       if (!debug) then
		   (xcon_count := this_call + 1;
		    print ("Call " ^ (Int.toString this_call) ^ " to xcon\n");
		    Ppil.pp_con il_con;
		    (*
		     print"\n";
		     Nilcontext.print_context (NILctx_of context); *)
		    print"\n")
	       else ()

	   val result = (xcon' context il_con)
	       handle e => (if (!debug) then print ("Exception detected in call " ^ 
						    (Int.toString this_call) ^ " to xcon\n") else ();
			    raise e)

	in
	    if (!debug) then print ("Return " ^ (Int.toString this_call) ^ " from xcon\n") else ();
	    result
        end

   and xcon' context (il_con as (Il.CON_VAR var)) = 
       let
	   val con = Var_c var
	   val kind = (case Nilcontext.find_kind (NILctx_of context, var) of
			   SOME kind => kind
			 | NONE => (print "Could not find constructor variable ";
				    Ppil.pp_var var;
				    print "in context:\n";
				    Nilcontext.print_context (NILctx_of context);
				    error "xcon: CON_VAR\n"))
       in
	   (con, kind)
       end

     | xcon' context (Il.CON_TYVAR tv) = xcon context (derefTyvar tv)

     | xcon' context (Il.CON_OVAR ov) = xcon context (derefOvar ov)

     | xcon' context (Il.CON_FLEXRECORD fr) = xflexinfo context fr

     | xcon' context ((Il.CON_INT intsize) | (Il.CON_UINT intsize)) =
       let
	   val (con, knd) = Nilstatic.con_valid (NILctx_of context, Prim_c (Int_c intsize, []))
       in
	   (con, knd)
       end

     | xcon' context (Il.CON_FLOAT floatsize) = 
       let
	   val con = Prim_c (BoxFloat_c floatsize, [])
       in
	   (con, Word_k Runtime)
       end

     | xcon' context (Il.CON_ARRAY il_con) = 
       let
	   val (con', knd') = xcon context il_con 
	   val con = Prim_c (Array_c, [con'])
       in
	   (con, Word_k Runtime)
       end

     | xcon' context (Il.CON_VECTOR il_con) = 
       let
	   val (con', knd') = xcon context il_con 
	   val con = Prim_c (Vector_c, [con'])
       in
	   (con, Word_k Runtime)
       end

     | xcon' context (Il.CON_ANY) = 
       let
	   val con = Prim_c(Exn_c, [])
       in
	   (con, Word_k Runtime)
       end

     | xcon' context (Il.CON_REF il_con) = 
       let
	   val (con', knd') = xcon context il_con
	   val con = Prim_c (Ref_c, [con'])
       in
	   (con, Word_k Runtime)
       end

     | xcon' context (Il.CON_TAG il_con) = 
       let
	   val (con', knd') = xcon context il_con
	   val con = Prim_c (Exntag_c, [con'])
       in
	   (con, Word_k Runtime)
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
       in
	   (con, Word_k Runtime)
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
	   val (con1, _) = xcon context il_con1
           val (con2, _) = xcon context il_con2
	   val con = App_c(con1, [con2])
           val (_, knd) = Nilstatic.con_valid(NILctx_of context, con)
       in
	   (con, knd)
       end

     | xcon' context (Il.CON_MUPROJECT(i, Il.CON_FUN(vars, 
						     Il.CON_TUPLE_INJECT cons))) =
       let
	   fun is_bound v = (case Nilcontext.find_kind (NILctx_of context, v) of
				 NONE => false | SOME _ => true)
	   val Il.CON_FUN(vars, Il.CON_TUPLE_INJECT cons) = 
	       Ilutil.rename_confun(is_bound,vars,Il.CON_TUPLE_INJECT cons)
	   fun cont1 NILctx' = 
	       let
		   val context' = 
		       update_NILctx
		       (context, NILctx')

		   val cons'= map (#1 o (xcon context')) cons
		   val freevars = Listops.flatten (map Ilutil.con_free_convar cons)
		   val is_recur = Listops.orfold (fn v => Listops.member_eq(Name.eq_var,v,freevars)) vars

		   val con = Mu_c (is_recur,
				   Util.list2set (Listops.zip vars cons'), 
				   List.nth (vars, i-1))
	       in
		   (con, Word_k Runtime)
	       end
       in
	   Nilcontext.c_insert_kind_list(NILctx_of context,
			      map (fn v => (v,Word_k Runtime)) vars,
			      cont1)
       end

     | xcon' context (Il.CON_MUPROJECT(i, Il.CON_FUN([var], con))) =
       let
	   fun is_bound v = (case Nilcontext.find_kind (NILctx_of context, v) of
				 NONE => false | SOME _ => true)
	   val Il.CON_FUN([var],con) = Ilutil.rename_confun(is_bound,[var],con)
	   fun cont1 NILctx' = 
	       let
		   val context' = 
		       update_NILctx
		       (context, NILctx')
		       
		   val (con',_) = xcon context' con
		   val freevars = Ilutil.con_free_convar con
		   val is_recur = Listops.member_eq(Name.eq_var,var,freevars)
		   val con = Mu_c (is_recur,Util.list2set [(var, con')], var)
	       in
		   (con, Word_k Runtime)
	       end
       in
	   Nilcontext.c_insert_kind(NILctx_of context, var, Word_k Runtime, cont1)
       end

     | xcon' context (Il.CON_RECORD rdecs) = 
       let
	   val (lbls, cons) = xrdecs context rdecs
	   val con = Prim_c (Record_cc "4"  lbls, cons) (* already sorted *)
       in
	   (con, Word_k Runtime)
       end

     | xcon' context (Il.CON_FUN (vars, il_con1)) = 
       let
	   fun cont1 NILctx' =
	       let
		   val context' = 
		       update_NILctx
		       (context, NILctx')

		   val (con1, knd1) = xcon context' il_con1
		   val (arg, con1') =
		       case vars of
			   [v] => ((v, Word_k Runtime), con1)
			 | _ => let fun mapper (n,_) = ((Nilutil.generate_tuple_label (n+1),
							 Name.fresh_var()),Word_k Runtime)
				    val arg_var = Name.fresh_var()
					(* already sorted *)
				    val arg_kind = Record_k(Util.sequence2list
							    (Listops.mapcount mapper vars))
				    fun mapper (n,v) = 
					Con_cb(v,Word_k Runtime, 
					       Proj_c(Var_c arg_var, 
						      Nilutil.generate_tuple_label (n+1)))

				    val con1' = Let_c(Sequential,Listops.mapcount mapper vars,con1)
				in  ((arg_var, arg_kind), con1')
				end

		   val fun_name = Name.fresh_var ()
		   val con = Let_c(Sequential,
				   [Open_cb(fun_name, [arg], con1', knd1)],
				   Var_c fun_name)
	       in
		   (con, Arrow_k(Open, [arg], knd1))
	       end
       in
	   Nilcontext.c_insert_kind_list(NILctx_of context, 
			      (map (fn v => (v, Word_k Runtime)) vars),
			      cont1)
       end

     | xcon' context (Il.CON_SUM {carriers, noncarriers, special}) =
       let
	   val known = (case special of
			       NONE => NONE
			     | SOME i => SOME (Word32.fromInt i))
	   val cons = map (#1 o (xcon context)) carriers
	   val con = Prim_c (Sum_c {tagcount = Word32.fromInt noncarriers,
				    known = known}, cons)
       in
	   (con, Word_k Runtime)
       end

     | xcon' context (il_con as (Il.CON_TUPLE_INJECT il_cons)) = 
       let
	   val (cons, knds) = myunzip (map (xcon context) il_cons)
	   val tuple_length = List.length cons
	   val labels = makeLabels tuple_length
	   val vars = makeVars tuple_length
	(* already sorted *)
	   val con = Crecord_c(Listops.zip labels cons)
	   val knd = Record_k (Util.list2sequence 
			       (Listops.zip (Listops.zip labels vars) knds))
       in
	   (con, knd)
       end

     | xcon' context (il_con as (Il.CON_TUPLE_PROJECT (i, il_con1))) = 
       let
	   val (con1, Record_k seq) = 
	       xcon context il_con1
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
       in
	   (con, knd)
       end

     | xcon' context (il_con as (Il.CON_MODULE_PROJECT (modv, lbl))) = 
       let
	   val {cbnd_cat,name_c,...} = 
	       xmod context (modv, NONE)

	   val con = makeLetC (map Con_cb (flattenCatlist cbnd_cat))
	                      (Proj_c (name_c, lbl))
	   val (_,knd) = Nilstatic.con_valid (NILctx_of context, con)
       in
	   (con, knd)
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
           val (con,_) = xcon context il_con
           val exps = map (#1 o (xexp context)) il_exps
       in
	   (Const_e (Prim.array (con, Array.fromList exps)), 
	    Prim_c(Array_c, [con]), true)
       end

     | xvalue context (Prim.vector (il_con, v)) = 
       let
	   val il_exps = Array.foldr (op ::) nil v
           val (con, _) = xcon context il_con
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
	   val (con, _) = xcon context il_con
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
		    Ppil.pp_exp il_exp;
		    print"\n")
	       else ()

	   val result = (xexp' context il_exp)
	       handle e => (if (!debug) then print ("Exception detected in call " ^ 
						    (Int.toString this_call) ^ " to xexp\n") else ();
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
	   val (args, _, valuables) = myunzip3 (map (xexp context) il_args)
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
	   val (args, _, valuables) = myunzip3 (map (xexp context) il_args)
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
	   val con = (case (Nilcontext.find_con(NILctx_of context, var)) of
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
		   (case (Nilstatic.con_reduce(NILctx_of context, con1)) of
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
	   val fbnds'= xfbnds context fbnds
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
               (Let_e (Sequential, [Fixopen_b set], hd names),
		hd types, true)
           else
	       (Let_e (Sequential, [Fixopen_b set], 
		       Prim_e(NilPrimOp (record labels), types, names)),
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

	   val con = projectFromRecord con_record [label]
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
	   val (con, _) = xcon context il_con
       in
	   (Raise_e (exp, con), con, false)
       end

     | xexp' context (Il.LET (bnds, il_exp)) = 
       let
	   val {ebnds, valuable, final_context=context'} = xbnds context bnds
	   val (exp, con, valuable') = xexp context' il_exp

	   val _ = 
	     if (!trace) then
	       (lprintl "HERE WE ARE1";
		perr_c con)
	     else
	       ()

	   val reduced_con = Nilstatic.con_reduce(NILctx_of context', con)

	   val _ = 
	     if (!trace) then 
	       printl "OUT1"
	     else ()

       in
	   (Let_e (Sequential, ebnds, exp),
	    reduced_con, valuable andalso valuable')
       end

     | xexp' context (Il.NEW_STAMP il_con) = 
       let
	   val (con, _) = xcon context il_con
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
	   val (con, _) = xcon context il_con
	   val (exp, _, valuable) = xexp context il_exp
       in
	   (Prim_e(NilPrimOp roll, [con], [exp]), con, valuable)
       end

     | xexp' context (il_exp as (Il.UNROLL (il_con, il_exp1))) = 
       let
	   val (con, _) = xcon context il_con
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
	 val _ = 
	   if (!trace) then
	     (lprintl "IL MODULE IS";
	      Ppil.pp_mod module;
	      printl "")
	   else ()
	   
	 val {cbnd_cat, ebnd_cat, 
	      name_c, name_r, 
	      knd_c, type_r,
	      valuable} = xmod context (module, NONE)
	   
	   val specialize = (Name.eq_label(label, Ilutil.it_lab)) andalso 
                            ! elaborator_specific_optimizations

	   val cbnds = flattenCatlist cbnd_cat

	   val _ = 
	     if (!trace) then
	       (lprintl "Con bounds kinds are";
		ignore (map (fn (v,k,c) => perr_k k) cbnds);
		lprintl "DONE")
	     else ()

	   val bnds = (map Con_b cbnds) @ 
	              (flattenCatlist ebnd_cat)


(*
	   val _ = (print "tonil.sml: MODULE_PROEJCT case.  type_r =\n";
		    Ppnil.pp_con type_r;
		    print "\n\n")
*)
           val unnormalized_con = 
	       makeLetC (map Con_cb cbnds) 
	                (if specialize then 
			     type_r 
			 else
			     projectFromRecord type_r [label])

(*
           val _ = (print "unnormalized con = ";
		    Ppnil.pp_con unnormalized_con;
		    print "\n\nwith a context of\n";
		    NilContext.print_context (NILctx_of context); print "\n")
*)

	   val _ = 
	     if (!trace) then
	       (lprintl "HERE WE ARE2";
		perr_c unnormalized_con)
	     else ()

           val con = Nilstatic.con_reduce(NILctx_of context, unnormalized_con)

	   val _ = 
	     if (!trace) then
	       printl "OUT2"
	     else ()
(*
           val _ = (print "normalized con = ";
		    Ppnil.pp_con con;
		    print "\n")
*)
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
	       (Let_e (Sequential, bnds, let_body), 
		con,
		valuable)
       end

     | xexp' context (Il.SEAL (exp,_)) = xexp context exp

     | xexp' _ _ = error "(xexp) unrecognized expression"

   and xfbnds context fbnds = 
       let
	   val nil_funtypes =
	       map (fn (Il.FBND(var,_,con,con',_)) => 
		    let val (c,_) = xcon context con
			val (c',_) = xcon context con'
		    in
			(var, AllArrow_c(Open,Partial,[],[c],w0,c'))
		    end)
	       fbnds

	   fun cont1 NILctx' =
	       let
		   val context' = 
		       update_NILctx
		       (context, NILctx')

		   fun loop [] = []
		     | loop (Il.FBND(var, var', il_con1, il_con2, body) :: rest) = 
		       let
			   (*XXX REDUNDANT AND SLOW!*)
			   val (con1, _) = xcon context il_con1
			   val (con2, _) = xcon context il_con2
			       
			   fun cont2 NILctx' =
			       let
				   val context'' = 
				       update_NILctx
				       (context', NILctx')
				       
				   val (body', _, valuable) = xexp context'' body
				       
				   (* XXX OVERLY CONSERVATIVE ! *)
				   val (effect, recursive) = 
				       if valuable then
					   (Total, Leaf)
				       else
					   (Partial, Nonleaf)
					   
				   val rest' = loop rest
			       in
				   (var, Function(effect, recursive, [],
						  [(var', con1)], [], body', con2))
				   :: rest'
			       end
		       in
			   Nilcontext.c_insert_con(NILctx_of context', var', con1, cont2)
		       end
	       in
		   loop fbnds
	       end
       in
	   Nilcontext.c_insert_con_list(NILctx_of context, nil_funtypes, cont1)
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
		record_c_knd_items, record_r_labels, record_r_field_types,
		record_r_exp_items} = 
		xsbnds context (!elaborator_specific_optimizations, true, sbnds)

       in
	   {ebnds = (map Con_b (flattenCatlist cbnd_cat)) @ (flattenCatlist ebnd_cat),
	    final_context = final_context,
	    valuable = valuable}
       end

   and xsig' context (con0,Il.SIGNAT_FUNCTOR (var, sig_dom, sig_rng, arrow))=
       let

	   val is_polyfun_sig = 
	       (case sig_rng of
		    Il.SIGNAT_STRUCTURE(_,[Il.SDEC(it_lbl,Il.DEC_EXP _)]) => Name.eq_label(it_lbl,Ilutil.it_lab)
		  | _ => false)

	   val (var_c, var_r, vmap') = splitVar (var, vmap_of context)
	   val (knd, con) = xsig context (Var_c var_c, sig_dom)
           val d = Il.DEC_MOD(var, sig_dom)
	      
           fun cont1 NILctx' =
	       Nilcontext.c_insert_con(NILctx', var_r, con, cont2)

	   and cont2 NILctx'' =
	       let
		   val context' = 
		       update_NILctx
		       (update_vmap(context, vmap'),
			NILctx'')
		       
		   val (knd', con') = 
		       xsig context' (App_c(con0, [Var_c var]), sig_rng)

		   val is_con_unit = is_unit_c con
	       in
		 (Arrow_k (Open, [(var_c, knd)], knd'),
		  AllArrow_c (Open, xeffect arrow, [(var_c, knd)],
			      if is_con_unit andalso !optimize_empty_structure then 
				[] else [con], w0, con'))
	       end
       in
	   Nilcontext.c_insert_kind(NILctx_of context, var_c, knd, cont1)
       end

     | xsig' context (con0, ((Il.SIGNAT_STRUCTURE (NONE,sdecs)) |
			     (Il.SIGNAT_INLINE_STRUCTURE {self=NONE,abs_sig=sdecs,...}))) =
       let
	   val {crdecs, erlabs, ercons} = 
	       xsdecs context (!elaborator_specific_optimizations,true,con0, Subst.empty(), sdecs)
	   val crdecs = ListMergeSort.sort gt_label_pairpair crdecs
	   val kind = Record_k (Util.list2sequence crdecs)
	   val (erlabs,ercons) = 
	       let 
		   val temp = ListMergeSort.sort gt_label_pair
					 (Listops.zip erlabs ercons)
	       in  (map #1 temp, map #2 temp)
	       end
	   val default = (kind,Prim_c(Record_cc "7" erlabs, ercons))
       in  (case (!elaborator_specific_optimizations,sdecs,erlabs,ercons) of
		(true,[Il.SDEC(it_lbl,_)],[erlab],[ercon]) =>
		    (if (Name.eq_label(it_lbl,Ilutil.it_lab))
			 then (kind,ercon)
		    else default)
	      | _ => default)
       end

     | xsig' context (con0, ((Il.SIGNAT_STRUCTURE (SOME p,sdecs)) |
			     (Il.SIGNAT_INLINE_STRUCTURE {self=SOME p,imp_sig=sdecs,...}))) =
       let 
(*	   val _ = print "calling unselfify\n" *)
	   val s = Il.SIGNAT_STRUCTURE(SOME p,sdecs)
	   val sig' = IlStatic.UnselfifySig(p,s)
(*
	   val _ = print "returned from unselfify\n"
	   val _ = (print "original selfifed signture:\n";
		    Ppil.pp_signat s; print "\n\n";
		    print "new unselfifed signture:\n";
		    Ppil.pp_signat sig'; print "\n\n\n")
*)
       in
	   xsig' context (con0, sig')
       end

   and xsig context (con, il_sig) =
       let
(*	   val _ = (print "call to xsig\n";
		    Ppil.pp_signat il_sig) *)
	   val result = xsig' context (con, il_sig)
(* 	   val _ = (print "back from xsig\n") *)
       in
	   result
       end
		    
       
   and xsdecs context (args as (_,_,_,_,sdecs)) =
       let
	   val this_call = ! xmod_count
	   val _ = if (! debug) then
	            (xsdecs_count := this_call + 1;
		     print ("Call " ^ (Int.toString this_call) ^ " to xsdecs\n");
		     Ppil.pp_sdecs sdecs) else ()

	   val result = xsdecs'' context args
	       handle e => (if (!debug) then print ("Exception detected in call " ^ 
						    (Int.toString this_call) ^ " to xsdecs\n") else ();
				raise e)
		   
       in
	   if (!debug) then print ("Return " ^ (Int.toString this_call) ^ " from xsdecs\n") else ();
	   result
       end

   and xsdecs'' context (elab_spec,firstpass,con0,subst,sdecs) = 
       let 
	   fun loop [] = []
	     | loop ((sdec as 
		     Il.SDEC(_,Il.DEC_EXP(top_var,il_con))) :: rest) = 
	        if (Util.substring("polyfun",Name.var2string top_var)) then
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
	     | loop ((sdec as 
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
		   andalso (Util.substring("polyfun",Name.var2string top_var))
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
	     | loop (sdec::rest) = sdec::(loop rest)
(*	   val _ = (print "before loop: length sdecs = "; print (Int.toString (length sdecs)); print "\n") *)
	   val sdecs' = if (elab_spec andalso firstpass)
			    then loop sdecs
			else sdecs
(* 	   val _ = (print "after loop: length sdecs' = "; print (Int.toString (length sdecs')); print "\n") *)
       in  xsdecs' context (elab_spec,firstpass,con0,subst,sdecs')
       end
   
   and xsdecs' context (_,_,con0, _, []) = {crdecs = nil, erlabs = nil, ercons = nil}

     | xsdecs' context (elab_spec,_,con0, subst,  
		    Il.SDEC(lbl, d as Il.DEC_MOD(var,signat)) :: rest) =
       let
	   val (var_c, var_r, vmap') = splitVar (var, vmap_of context)
	   val (knd, con) = xsig context (Proj_c(con0, lbl), signat)
	       
	   fun cont1 NILctx' =
	       Nilcontext.c_insert_con(NILctx', var_r, con, cont2)

	   and cont2 NILctx'' =
	       let
		   val context' = 
		       update_NILctx
		       (update_vmap
			(context, vmap'),
			NILctx'')
		       
		   val {crdecs, erlabs, ercons} =
		       xsdecs context' (elab_spec,false,con0, Subst.add subst (var_c, Proj_c(con0, lbl)),
					rest)
	       in
		   {crdecs = if (!do_kill_cpart_of_functor andalso
				 case knd of
				     Arrow_k(_,_,Record_k seq) => null (Util.sequence2list seq)
				   | _ => false)
				 then crdecs
			     else ((lbl, var_c), knd) :: crdecs,
		    erlabs = lbl :: erlabs,
		    ercons = (Subst.substConInCon subst con) :: ercons}
	       end
       in
	   Nilcontext.c_insert_kind(NILctx_of context, var_c, knd, cont1)
       end

     | xsdecs' context(elab_spec,_,con0, subst, Il.SDEC(lbl, d as Il.DEC_EXP(var,con)) :: rest) =
       let
	   val (con',_) = xcon context con

	   fun cont1 NILctx' =
	       let
		   val context' = 
		       update_NILctx
		       (context, NILctx')
		       
		   val {crdecs, erlabs, ercons} = xsdecs context' (elab_spec,false,con0, subst, rest)
	       in
		   {crdecs = crdecs,
		    erlabs = lbl :: erlabs,
		    ercons = (Subst.substConInCon subst con') :: ercons}
	       end
       in
	   Nilcontext.c_insert_con(NILctx_of context, var, con', cont1)
       end

     | xsdecs' context (elab_spec,_,con0, subst, sdecs as Il.SDEC(lbl, d as Il.DEC_CON(var, knd, 
									maybecon))::rest)=
       let
	   val knd' = xkind context knd
	   val knd'' =(case maybecon of
			   NONE => knd'
			 | SOME il_con => 
			       (let val (c,k) = xcon context il_con
				in  Singleton_k(Runtime,k,c)
				end handle _ => knd'))

	   fun cont1 NILctx' =
	       let
		   val context' = 
		       update_NILctx
		       (context, NILctx')
		       
		   val {crdecs, erlabs, ercons} = 
		       xsdecs context' (elab_spec,false,con0, Subst.add subst (var, Proj_c(con0, lbl)),
					rest)
	       in
		   {crdecs = ((lbl, var), knd'') :: crdecs,
		    erlabs = erlabs,
		    ercons = ercons}
	       end
       in
	   Nilcontext.c_insert_kind(NILctx_of context, var, knd'', cont1)
       end

   and xkind context (Il.KIND_TUPLE n) = makeKindTuple n
     | xkind context (Il.KIND_ARROW (1,m)) =
         Arrow_k (Open, 
		  [(Name.fresh_var(), Word_k Runtime)], 
		  makeKindTuple m)
     | xkind context (Il.KIND_ARROW (n,m)) = 
         let (* already sorted *)
	     val (Record_k args) = makeKindTuple n
	 in
	     Arrow_k (Open,
		      map (fn ((_,v),k) => (v,k)) args,
		      makeKindTuple m)
	 end
     | xkind context (Il.KIND_INLINE (k,c)) = 
	 let 
	     val (con,knd) = xcon context c
	 in  Singleton_k(Runtime, knd, con)
	 end

   fun xHILctx context HILctx =
       let
	   fun folder (v,context) =
	       let val (l,pc) = 
		 case Ilcontext.Context_Lookup'(HILctx,v)
		   of SOME v => v
		    | NONE => error "Variable not found in ilcontext"
	       in 
		   (case pc of
			Ilcontext.PHRASE_CLASS_EXP (_,il_type) => 
			    let
				val (nil_type,_) = xcon context il_type
			    in
				update_NILctx
				(context, Nilcontext.insert_con(NILctx_of context, v, nil_type))
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
			    in
				update_NILctx
			    (context, Nilcontext.insert_kind(NILctx_of context, v, nil_kind))
			    end
		      | Ilcontext.PHRASE_CLASS_MOD (_,il_sig) => 
			    let
				val (v_c, v_r,_) = splitVar (v, vmap_of context)
				val (knd_c, type_r) = xsig context (Var_c v_c, il_sig)
				val inner_ctxt = 
				    if (false andalso
					!do_kill_cpart_of_functor andalso
					case knd_c of
					    Arrow_k(_,_,Record_k seq) => null (Util.sequence2list seq)
					  | _ => false)
					then NILctx_of context
				    else Nilcontext.insert_kind(NILctx_of context, v_c, knd_c)
			    in
				update_NILctx
				(context,
				 Nilcontext.insert_con
				 (inner_ctxt, v_r, type_r))
			    end
		      | _ => context)
	       end
       in
	   foldl folder context (Ilcontext.Context_Varlist HILctx)
       end

   fun xcompunit HILctx vmap il_sbnds =
       let
           val empty_splitting_context = 
	         CONTEXT{NILctx = Nilcontext.empty(),
			 vmap = vmap}

	   val initial_splitting_context = xHILctx empty_splitting_context HILctx

	   val _ = 
	       if (!debug) then
		   (print "\nInitial HIL context varlist:\n";
		    app (fn v => (print "  "; Ppnil.pp_var v; print "\n")) (Ilcontext.Context_Varlist HILctx);
		    print "\n";
		    print "\nInitial HIL context:\n";
		    Ppil.pp_context HILctx;
		    print "\nInitial NIL context:\n";
		    Nilcontext.print_context (NILctx_of initial_splitting_context);
		    print "\n")
	       else
		   ()

	   val {cbnd_cat, ebnd_cat, final_context, ...} =
	       xsbnds initial_splitting_context 
                      (!elaborator_specific_optimizations, true, il_sbnds)

	   val cu_c_list = map Con_b (flattenCatlist cbnd_cat)
	   val cu_r_list = flattenCatlist ebnd_cat

       in
	   {nil_initial_context = NILctx_of initial_splitting_context,
	    nil_final_context = NILctx_of final_context,
	    cu_bnds = cu_c_list @ cu_r_list,
	    vmap = vmap_of final_context}
       end

end

