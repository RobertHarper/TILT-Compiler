functor Tonil(structure Il : IL
              structure Ilutil : ILUTIL
              structure Nilutil : NILUTIL
              structure Ilcontext : ILCONTEXT
              structure Nilcontext : NILCONTEXT
              structure Nilstatic : NILSTATIC
              structure Nilprimutil : PRIMUTIL
              structure Ppnil : PPNIL
              structure Ppil : PPIL
                 sharing Il = Ilutil.Il = Ppil.Il = Ilcontext.Il
		 sharing Nilutil.Nil.Prim = Nilprimutil.Prim = Il.Prim
                 sharing Nilutil.Nil = Nilcontext.Nil = Ppnil.Nil = 
                         Nilstatic.Nil
                 sharing type Nilstatic.context = Nilcontext.context
		 sharing type Nilprimutil.exp = Nilutil.Nil.exp
                     and type Nilprimutil.con = Nilutil.Nil.con
             ) : TONIL =
struct
   structure Il = Il
   structure Nil = Nilutil.Nil

   open Nil
   val debug = ref true
(*
   val debug = ref (Prim_c (Record_c nil, nil))
   val il_debug = ref (Il.CON_ANY)
*)

   val elaborator_specific_optimizations = ref false

   fun error msg = Util.error "tonil.sml" msg


   (* Variable-splitting mapping.  

      For the phase-splitting, we need a way of turning each module
      variable var into a new constructor variable var_c and a new
      term variable var_r.  Instead of messing around with "the
      internal number of the variable mod 3", we simply maintain a
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
   fun makeKindTuple 1 = Type_k Runtime
     | makeKindTuple n =
       let
	   fun makeFields i =
	       if (i <= n) then
		   ((Ilutil.generate_tuple_label i, Name.fresh_var()), 
		    Type_k Runtime)
                   :: makeFields (i+1)
	       else
		   nil
       in
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
   fun selectFromRec (r, lbls) = 
       let
	   fun loop [] = r
	     | loop (lbl::lbls) = 
	       Prim_e(NilPrimOp(select lbl), [], [loop lbls])
       in
	   loop (rev lbls)
       end



   fun chooseName (NONE, vmap) = splitFreshVar vmap
     | chooseName (SOME (var,var_c,var_r), vmap) = (var, var_c, var_r, vmap)

   val xcon_count = ref 0


   fun extendmap (map, key, value) key' =
       if (Name.eq_var (key, key')) then SOME value else map key'
       
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

   val w0 = Word32.fromInt 0

   fun projectFromRecord con [] = con
     | projectFromRecord (Prim_c(Record_c (lbl::lbls), con::cons)) (l' as (lbl'::lbls')) =
       if (Name.eq_label (lbl, lbl')) then 
	   projectFromRecord con lbls'
       else
	   projectFromRecord (Prim_c(Record_c lbls, cons)) l'

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
	   val _ = (xmod_count := this_call + 1
(*		    ;print ("Call " ^ (Int.toString this_call) ^ " to xmod\n");
		    Ppil.pp_mod il_mod;
		    print"\n";
		    Nilcontext.print_context (NILctx_of context);
		    print"\n"
*)
		   )
	   val result = xmod' context args
	in
(*	    print ("Return " ^ (Int.toString this_call) ^ " from xmod\n"); *)
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

           val (SOME knd_c) = Nilcontext.find_kind (NILctx_of context, var_mod_c)
           val (SOME type_r) = Nilcontext.find_con (NILctx_of context, var_mod_r)

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

	   val {cbnd_cat = cbnd_cat_fun,
		ebnd_cat = ebnd_cat_fun,
		name_c = name_fun_c,
                name_r = name_fun_r,
		knd_c = knd_fun_c,
		type_r = type_fun_r,
		valuable = valuable_fun
		} = xmod context (ilmod_fun, NONE)

	   val {cbnd_cat = cbnd_cat_arg,
		ebnd_cat = ebnd_cat_arg,
                name_c = name_arg_c,
                name_r = name_arg_r,
		knd_c = knd_arg_c,
		type_r = type_arg_r,
		valuable = valuable_arg
		} = xmod context (ilmod_arg, NONE)

           val name_c = Var_c var_c
	   val name_r = Var_e var_r

           local
	       val (Arrow_k(_, [(v_c, _)], con_body_kind)) = knd_fun_c
	       val argument_c = makeLetC (map Con_cb (flattenCatlist cbnd_cat_arg)) name_c
	       val reduced_argument_c = Nilstatic.con_reduce (NILctx_of context, argument_c)

               val (AllArrow_c(_,effect,_,_,_,exp_body_type)) = type_fun_r

               fun subst v = 
		   if (Name.eq_var(v_c,v)) then 
		       SOME reduced_argument_c
		   else
		       NONE

	       fun subst2 v =
		   if (Name.eq_var(v_c,v)) then 
		       SOME (Var_c var_c)
		   else
		       NONE
	   in
	       val knd_c = Nilstatic.kind_reduce (NILctx_of context,
						  Nilutil.substConInKind subst con_body_kind)
	       val type_r = Nilstatic.con_reduce 
		   (Nilcontext.insert_kind(NILctx_of context, var_c, knd_c),
		    Nilutil.substConInCon subst2 exp_body_type)
	       val valuable = (effect = Total) andalso valuable_fun andalso valuable_arg 
	   end  

	   val cbnd_cat = APP[cbnd_cat_fun, 
			      cbnd_cat_arg,
			      LIST[(var_c, knd_c, App_c(name_fun_c,[name_arg_c]))]]

           val ebnd_cat = APP[ebnd_cat_fun, 
			      ebnd_cat_arg,
			      LIST[Exp_b(var_r, type_r,
					 App_e(Open,
					       name_fun_r,
					       [name_arg_c],
					       [name_arg_r], []))]]
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
	       val v = Name.fresh_var ()
	       val type_mod_r' = Nilstatic.con_reduce(NILctx_of context, type_mod_r)
	   in

	       val (_,knd_proj_c) = Nilstatic.con_valid(Nilcontext.insert_kind(NILctx_of context, v, knd_mod_c),
							selectFromCon(Var_c v, lbls))
	       val type_proj_r = projectFromRecord type_mod_r' lbls
	   end

           val cbnd_proj_cat = APP[cbnd_mod_cat,
				   LIST [(var_proj_c, knd_proj_c,
					  selectFromCon(name_mod_c, lbls))]]
	   val ebnd_proj_cat = APP[ebnd_mod_cat,
				   LIST [Exp_b(var_proj_r, type_proj_r,
					       selectFromRec(name_mod_r,
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
				       [con_arg], w0,
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
					 [(var_arg_r, con_arg)],
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

	   val {cbnd_cat, crbnds, ebnd_cat, erlabels, erfields, ercons,
		valuable, vmap, crknds} = xsbnds context sbnds

           val name_str_c = Var_c var_str_c
	   val name_str_r = Var_e var_str_r

	   val knd_str_c = 
	       let
		   fun loop [] [] = []
                     | loop (k::crknds) ((l,Var_c v)::crbnds) =
		       ((l,v),k) :: (loop crknds crbnds)
	       in
		   Record_k (Util.list2sequence (loop crknds crbnds))
	       end
           val type_str_r = Prim_c(Record_c erlabels, ercons)

           val cbnd_str_cat = 
	       APP[cbnd_cat,
		   LIST [(var_str_c, knd_str_c, Crecord_c crbnds)]]
	   val ebnd_str_cat = 
	       APP[ebnd_cat,
		   LIST[Exp_b (var_str_r, type_str_r,
			       Prim_e (NilPrimOp (record erlabels),
				       ercons, erfields))]]


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
	   val (var_loc_c, var_loc_r,_) = splitVar (var_loc, vmap_of context)

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
				Nilcontext.c_insert_con(NILctx', var_loc_r, type_loc_r, cont2)
				
			    and cont2 NILctx'' =
				let
				    val context' = 
					update_NILctx
					(context, NILctx'')
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

   and xsbnds context il_sbnds =
       let
	   val il_bnds = map (fn Il.SBND(_,bnd) => bnd) il_sbnds

           val (final_context, cbnds, ebnds , valuable, ilvars_kept, ercons, crknds) = 
	       xbnds context (!elaborator_specific_optimizations,true) il_bnds

	   fun checkVar v = Listops.member_eq (Name.eq_var, v, ilvars_kept)

	   fun split [] = {crbnds = nil, erlabels = nil, erfields = nil}

             | split (Il.SBND(lab, bnd as Il.BND_EXP(var, il_exp)) :: rest) =
	       let
		   val {crbnds, erlabels, erfields} = split rest
	       in
		   {crbnds = crbnds,
		    erlabels = lab :: erlabels,
		    erfields = (Var_e var) :: erfields}
	       end

	     | split (Il.SBND(lab, Il.BND_CON(var, il_con)) :: rest) = 
	       let
		   val {crbnds, erlabels, erfields} = split rest
		       
	       in
		   {crbnds = (lab, Var_c var) :: crbnds,
		    erlabels = erlabels,
		    erfields = erfields}
	       end

	     | split (Il.SBND(lab, Il.BND_MOD(var', il_mod)) :: rest) =
	       let
		   val {crbnds, erlabels, erfields} = split rest
		   val (var'_c, var'_r, _) = splitVar (var', vmap_of final_context)
	       in
		   if (checkVar var') then
		       {crbnds = (lab, Var_c var'_c) :: crbnds,
			erlabels = lab :: erlabels,
			erfields = (Var_e var'_r) :: erfields}
		   else
		       {crbnds = crbnds, erlabels = erlabels, erfields = erfields}
	       end

	   val {crbnds, erlabels, erfields} = split il_sbnds
       in
	   {cbnd_cat = cbnds,
	    crbnds = crbnds, 
	    crknds = crknds,
	    ebnd_cat = ebnds,
	    erlabels = erlabels, 
	    erfields = erfields, 
	    ercons = ercons, 
	    valuable = valuable, 
	    vmap = vmap_of final_context}
       end

   and xflexinfo context (ref (Il.INDIRECT_FLEXINFO f)) = 
       xflexinfo context f
     | xflexinfo context (ref (Il.FLEXINFO(_,true, recs))) = 
       let
	   val (lbls, cons) = xrdecs context recs
	   val con = Prim_c(Record_c lbls, cons)
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

   and xcon context (il_con as (Il.CON_VAR var)) = 
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

     | xcon context (Il.CON_TYVAR tv) = xcon context (derefTyvar tv)

     | xcon context (Il.CON_OVAR ov) = xcon context (derefOvar ov)

     | xcon context (Il.CON_FLEXRECORD fr) = xflexinfo context fr

     | xcon context ((Il.CON_INT intsize) | (Il.CON_UINT intsize)) =
       let
	   val con = Prim_c (Int_c intsize, [])
       in
           (* XXX *)
	   (* BUG---SHOULD CALL ISWORD SINCE INTSIZE CAN BE >= 64!  *)
	   (con, Word_k Runtime)
       end

     | xcon context (Il.CON_FLOAT floatsize) = 
       let
	   val con = Prim_c (BoxFloat_c floatsize, [])
       in
	   (con, Word_k Runtime)
       end

     | xcon context (Il.CON_ARRAY il_con) = 
       let
	   val (con', knd') = xcon context il_con 
	   val con = Prim_c (Array_c, [con'])
       in
	   (con, Word_k Runtime)
       end

     | xcon context (Il.CON_VECTOR il_con) = 
       let
	   val (con', knd') = xcon context il_con 
	   val con = Prim_c (Vector_c, [con'])
       in
	   (con, Word_k Runtime)
       end

     | xcon context (Il.CON_ANY) = 
       let
	   val con = Prim_c(Exn_c, [])
       in
	   (con, Word_k Runtime)
       end

     | xcon context (Il.CON_REF il_con) = 
       let
	   val (con', knd') = xcon context il_con
	   val con = Prim_c (Ref_c, [con'])
       in
	   (con, Word_k Runtime)
       end

     | xcon context (Il.CON_TAG il_con) = 
       let
	   val (con', knd') = xcon context il_con
	   val con = Prim_c (Exntag_c, [con'])
       in
	   (con, Word_k Runtime)
       end

     | xcon context (Il.CON_ARROW (il_con1, il_con2, arr)) =
       let
	   val (con1, _) = xcon context il_con1
           val (con2, _) = xcon context il_con2
	   val eff = xeffect (derefOneshot arr)
	   val con = AllArrow_c(Open, eff, [], [con1], w0, con2)
       in
	   (con, Word_k Runtime)
       end

     | xcon context (il_con as Il.CON_APP (il_con1, il_con2)) = 
       let
	   val (con1, _) = xcon context il_con1
           val (con2, _) = xcon context il_con2
	   val con = App_c(con1, [con2])
           val (_, knd) = Nilstatic.con_valid(NILctx_of context, con)
       in
	   (con, knd)
       end

     | xcon context (Il.CON_MUPROJECT(i, Il.CON_FUN(vars, 
						       Il.CON_TUPLE_INJECT cons))) =
       let
	   fun cont1 NILctx' = 
	       let
		   val context' = 
		       update_NILctx
		       (context, NILctx')

		   val cons'= map (#1 o (xcon context')) cons
		   val con = Mu_c (Util.list2set (Listops.zip vars cons'), 
				   List.nth (vars, i-1))
	       in
		   (con, Word_k Runtime)
	       end
       in
	   Nilcontext.c_insert_kind_list(NILctx_of context,
			      map (fn v => (v,Word_k Runtime)) vars,
			      cont1)
       end

     | xcon context (Il.CON_MUPROJECT(i, Il.CON_FUN([var], con))) =
       let
	   fun cont1 NILctx' = 
	       let
		   val context' = 
		       update_NILctx
		       (context, NILctx')
		       
		   val (con',_) = xcon context' con
		   val con = Mu_c (Util.list2set [(var, con')], var)
	       in
		   (con, Word_k Runtime)
	       end
       in
	   Nilcontext.c_insert_kind(NILctx_of context, var, Word_k Runtime, cont1)
       end

     | xcon context (Il.CON_RECORD rdecs) = 
       let
	   val (lbls, cons) = xrdecs context rdecs
	   val con = Prim_c (Record_c lbls, cons)
       in
	   (con, Word_k Runtime)
       end

     | xcon context (Il.CON_FUN (vars, il_con1)) = 
       let
	   fun cont1 NILctx' =
	       let
		   val context' = 
		       update_NILctx
		       (context, NILctx')

		   val (con1, knd1) = xcon context' il_con1
		   val (arg, con1') =
		       case vars of
			   [v] => ((v, Type_k Runtime), con1)
			 | _ => let fun mapper (n,_) = ((Nilutil.generate_tuple_label (n+1),
							 Name.fresh_var()),Type_k Runtime)
				    val arg_var = Name.fresh_var()
				    val arg_kind = Record_k(Util.sequence2list(Listops.mapcount mapper vars))
				    fun mapper (n,v) = Con_cb(v,Type_k Runtime, 
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

     | xcon context (Il.CON_SUM {carriers, noncarriers, special}) =
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

     | xcon context (il_con as (Il.CON_TUPLE_INJECT il_cons)) = 
       let
	   val (cons, knds) = myunzip (map (xcon context) il_cons)
	   val tuple_length = List.length cons
	   val labels = makeLabels tuple_length
	   val vars = makeVars tuple_length
	   val con = Crecord_c(Listops.zip labels cons)
	   val knd = Record_k (Util.list2sequence 
			       (Listops.zip (Listops.zip labels vars) knds))
       in
	   (con, knd)
       end

     | xcon context (il_con as (Il.CON_TUPLE_PROJECT (i, il_con1))) = 
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

     | xcon context (il_con as (Il.CON_MODULE_PROJECT (modv, lbl))) = 
       let
	   val {cbnd_cat,name_c,...} = 
	       xmod context (modv, NONE)

	   val con = makeLetC (map Con_cb (flattenCatlist cbnd_cat))
	                      (Proj_c (name_c, lbl))
	   val (_,knd) = Nilstatic.con_valid (NILctx_of context, con)
       in
	   (con, knd)
       end
    
     | xcon _ c = (print "Error:  Unrecognized constructor:\n";
		      Ppil.pp_con c;
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
	    Prim_c(Array_c, [con]), true)
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

   and xexp context (Il.OVEREXP(_, true, exp_oneshot)) = 
       xexp context (derefOneshot exp_oneshot)

     | xexp context (Il.SCON il_scon) = xvalue context il_scon

     | xexp context (il_exp as (Il.PRIM (prim, il_cons, il_args))) = 
       let
	   val cons = map (#1 o (xcon context)) il_cons
	   val (args, _, valuables) = myunzip3 (map (xexp context) il_args)
           val (AllArrow_c(_,effect,_,_,_,con)) = Nilprimutil.get_type prim cons
           val valuable = (effect = Total) andalso (Listops.andfold (fn x => x) valuables)
       in
	   (Prim_e (PrimOp prim, cons, args), con, valuable)
       end
(*
     | xexp context (il_exp as Il.PRIM (prim, il_cons)) = 
       let
	   val cons = map (#1 o (xcon context)) il_cons
	   val il_con = Ilstatic.GetExpCon (HILctx_of context, il_exp)
	   val (con, _) = xcon context il_con
       in
	   (Prim_e (PrimOp prim, cons, NONE), con, true)
       end
*)
     | xexp context (il_exp as (Il.ILPRIM (ilprim, il_cons, il_args))) = 
       let
	   val cons = map (#1 o (xcon context)) il_cons
	   val (args, _, valuables) = myunzip3 (map (xexp context) il_args)
           val (AllArrow_c(_,effect,_,_,_,con)) = Nilprimutil.get_iltype ilprim cons
           val valuable = (effect = Total) andalso (Listops.andfold (fn x => x) valuables)
       in
	   (Prim_e (PrimOp (xilprim ilprim), cons, args), con, valuable)
       end
(*
     | xexp context (il_exp as (Il.ILPRIM ilprim)) = 
       let
	   val il_con = Ilstatic.GetExpCon (decs, il_exp)
	   val (con, _) = xcon context il_con
       in
	   (Prim_e (PrimOp (xilprim ilprim), [], NONE), con, true)
       end
*)
     | xexp context (il_exp as (Il.VAR var)) = 
       let
	   val (SOME con) = Nilcontext.find_con(NILctx_of context, var)
       in
	   (Var_e var, con, true)
       end

     | xexp context (il_exp as (Il.APP (il_exp1, il_exp2))) = 
       (case (Ilutil.beta_reduce(il_exp1,il_exp2)) of
	    NONE =>
		let
		    val (exp1, con1, valuable1) = xexp context il_exp1
		    val (exp2, con2, valuable2) = xexp context il_exp2
			
		    val (AllArrow_c(_,effect,_,_,_,con)) = Nilstatic.con_reduce(NILctx_of context, con1)
			
		    val valuable = (effect = Total) andalso valuable1 andalso valuable2
		in
		    (App_e (Open, exp1, [], [exp2], []), con, valuable)
		end
	  | SOME il_exp => xexp context il_exp)

     | xexp context (Il.FIX (il_arrow, fbnds)) = 
       let
	   val fbnds'= xfbnds context fbnds
           val set = Util.list2set fbnds'
           val names = map (fn (var,_) => Var_e var) fbnds'
           val types = map 
                (fn (_,Function(effect,_,_,[(_,con1)],[],_,con2)) =>  
                        AllArrow_c(Open, effect, [], [con1], w0, con2))
                fbnds'
           val num_names = List.length types
           val labels = makeLabels num_names
       in
	   if (List.length names = 1) then
               (Let_e (Sequential, [Fixopen_b set], hd names),
		hd types, true)
           else
	       (Let_e (Sequential, [Fixopen_b set], 
		       Prim_e(NilPrimOp (record labels), types, names)),
		Prim_c (Record_c labels, types), true)
       end

     | xexp context (Il.RECORD rbnds) = 
       let
	   val (labels, exps, cons, valuable) = xrbnds context rbnds
       in
	   (Prim_e (NilPrimOp (record labels), cons, exps),
	    Prim_c (Record_c labels, cons), valuable)
       end

     | xexp context (il_exp0 as (Il.RECORD_PROJECT (il_exp, label, il_record_con))) =
       let
	   val (exp_record, con_record, valuable) = xexp context il_exp

	   val con = projectFromRecord con_record [label]
       in
	   (Prim_e (NilPrimOp (select label), [], [exp_record]), con, valuable)
       end

     | xexp context (Il.SUM_TAIL (_, il_exp)) =
       let
	   val (exp, Prim_c(Sum_c {known = SOME i, tagcount}, cons), valuable) = xexp context il_exp

       in
	   (Prim_e (NilPrimOp (project_sum {sumtype = i, tagcount = tagcount}), 
		    cons, [exp]),
	    List.nth (cons, TilWord32.toInt (TilWord32.uminus(i,tagcount))), valuable)
       end

     | xexp context (Il.HANDLE (il_exp1, il_exp2)) = 
       let
	   val (exp1, con, _) = xexp context il_exp1
	   val exp2 = toFunction context il_exp2
       in
	   (Handle_e (exp1, exp2), con, false)
       end

     | xexp context (Il.RAISE (il_con, il_exp)) = 
       let
	   val (exp, _, _) = xexp context il_exp
	   val (con, _) = xcon context il_con
       in
	   (Raise_e (exp, con), con, false)
       end

     | xexp context (Il.LET (bnds, il_exp)) = 
       let
	   val (context', cbnds, ebnds, valuable, _, _, _) = 
	          xbnds context (!elaborator_specific_optimizations,true) bnds
	   val (exp, con, valuable') = xexp context' il_exp
	   val cbnds' = LIST(map Con_b (flattenCatlist cbnds))
       in
	   (Let_e (Sequential, flattenCatlist (APP[cbnds',ebnds]), exp), 
	    con, valuable andalso valuable)
       end

     | xexp context (Il.NEW_STAMP il_con) = 
       let
	   val (con, _) = xcon context il_con
       in 
	   (Prim_e(NilPrimOp make_exntag, [con], []),
	    Prim_c (Exntag_c, []),
	    false)
       end

     | xexp context (Il.EXN_INJECT (il_tag, il_exp)) =
       let
	   val (tag, _, valuable) = xexp context il_tag
	   val (exp, _, valuable') = xexp context il_exp
       in
           (Prim_e (NilPrimOp inj_exn, [], [tag, exp]),
	    Prim_c (Exn_c, []),
	    valuable andalso valuable')
       end

     | xexp context (Il.ROLL (il_con, il_exp)) = 
       let
	   val (con, _) = xcon context il_con
	   val (exp, _, valuable) = xexp context il_exp
       in
	   (Prim_e(NilPrimOp roll, [con], [exp]), con, valuable)
       end

     | xexp context (il_exp as (Il.UNROLL (il_con, il_exp1))) = 
       let
	   val (con, _) = xcon context il_con
	   val (exp, _, valuable) = xexp context il_exp1
       in
	   (* XXX BUG XXX  WRONG CON ARGUMENT!!! *)
	   (Prim_e(NilPrimOp unroll, [con], [exp]), con, valuable)
       end

     | xexp context (Il.INJ {carriers, noncarriers, special, inject=NONE}) =
       let
	   val field = Word32.fromInt special
	   val tagcount = Word32.fromInt noncarriers
	   val cons = map (#1 o (xcon context)) carriers
	   val con = Prim_c(Sum_c{tagcount = tagcount,
				  known = SOME field},
			    cons)
       in
	   (Prim_e(NilPrimOp (inject {tagcount = tagcount,
				      field = field}),
		   cons, []),
	    con, true)
       end


     | xexp context (Il.INJ {carriers, noncarriers, special, inject=SOME il_exp}) =
       let
	   val field = Word32.fromInt special
	   val tagcount = Word32.fromInt noncarriers
	   val cons = map (#1 o (xcon context)) carriers
	   val con = Prim_c(Sum_c{tagcount = tagcount,
				  known = SOME field},
			    cons)
	   val (exp, _, valuable) = xexp context il_exp
       in
	   (Prim_e(NilPrimOp (inject {tagcount = tagcount,
				      field = field}),
		   cons, [exp]),
	    con, 
	    valuable)
       end

     | xexp context (Il.CASE {noncarriers, carriers=il_cons, arg=il_arg, arms=il_arms,
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

     | xexp context (e as Il.EXN_CASE (il_exp, il_arms, il_default)) =
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

     | xexp context (il_exp as (Il.MODULE_PROJECT (module, label))) =
       let
	   val {cbnd_cat, ebnd_cat, 
		name_c, name_r, 
		knd_c, type_r,
		valuable} = xmod context (module, NONE)

           val con = projectFromRecord type_r [label]

	   val cbnds = flattenCatlist cbnd_cat
	   val bnds = (map Con_b cbnds) @ 
	              (flattenCatlist ebnd_cat)
       in
	   (Let_e (Sequential, bnds, Prim_e (NilPrimOp (select label), 
					     [], [name_r])),
	    makeLetC (map Con_cb cbnds) con,
	    valuable)
       end

     | xexp context (Il.SEAL (exp,_)) = xexp context exp

     | xexp _ _ = error "(xexp) unrecognized expression"

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

   and xbnds context _ [] = (context, LIST nil, LIST nil, true, nil, nil, nil)
     | xbnds context (optimize_fun,_) (Il.BND_EXP(var,il_exp) :: rest) =
       let 
	   val (exp, con, valuable) = xexp context il_exp

           val context' = 
	       update_NILctx(context,
			      Nilcontext.insert_con(NILctx_of context, var, con))

	   val (rest_context, rest_cbnds, rest_ebnds, rest_valuable, rest_ilvars_kept,
		rest_cons, rest_knds) = 
	       xbnds context' (optimize_fun, true) rest
       in
	   (rest_context,
	    rest_cbnds,
	    APP[LIST [Exp_b(var, con, exp)], rest_ebnds],
	    valuable andalso rest_valuable,
	    var :: rest_ilvars_kept,
	    con :: rest_cons,
	    rest_knds)
       end

     | xbnds context (optimize_fun,_) (Il.BND_CON(var,il_con) :: rest) =
       let
	   val (con, knd) = xcon context il_con

           val context' = 
	       update_NILctx(context,
			      Nilcontext.insert_kind(NILctx_of context, var, knd))

	   val (rest_context, rest_cbnds, rest_ebnds,rest_valuable, rest_ilvars_kept, rest_cons, rest_knds) = 
	       xbnds context' (optimize_fun,true) rest
       in
	   (rest_context,
	    APP [LIST [(var, knd, con)], rest_cbnds],
	    rest_ebnds,
	    rest_valuable,
	    var :: rest_ilvars_kept,
	    rest_cons,
	    knd :: rest_knds)
       end

     | xbnds context (true,true) (bnds as 
				  (Il.BND_MOD
				   (top_var, m as 
				    Il.MOD_FUNCTOR(poly_var, il_arg_signat, 
						   Il.MOD_STRUCTURE
						   [Il.SBND(lbl,
							    Il.BND_EXP
							    (_, il_exp as Il.FIX(arrow, fbnds)))]))
				   :: rest)) =
       if (Name.eq_label (lbl, Ilutil.it_lab)) then
	   let
	       val _ = print "entered optimization case\n"

	       fun getFunctionVars 0 (vs, rest) = (rev vs, rest)
                 | getFunctionVars n (vs, Il.BND_MOD(var,il_module)::rest) = 
		       getFunctionVars (n-1) (var::vs, rest)
                 | getFunctionVars _ _ = error "xbnds: Can't optimize function"
		   
	       val (external_names, rest') = getFunctionVars (length fbnds) (nil, rest)

               val (external_names_c, external_names_r, vmap') = 
		   let fun loop [] (ns_c, ns_r, vmap) = (rev ns_c, rev ns_r, vmap)
			 | loop (n::ns) (ns_c, ns_r, vmap) =
			   let val (n_c, n_r, vmap') = splitVar (n, vmap)
			   in
			       loop ns (n_c :: ns_c, n_r :: ns_r, vmap')
			   end
		   in
		       loop external_names (nil, nil, vmap_of context)
		   end

	       val (poly_var_c, poly_var_r, vmap'') = splitVar (poly_var, vmap')
	       val (knd_arg, con_arg) = xsig context (Var_c poly_var_c, il_arg_signat)
		   
	       val context' = 
		   update_NILctx(update_vmap(context, vmap''),
				  Nilcontext.insert_con
				  (Nilcontext.insert_kind
				   (NILctx_of context, poly_var_c, knd_arg),
				   poly_var_r, con_arg))


	       val (Let_e (_, [Fixopen_b set], _), _, _) = xexp context' il_exp

               val (internal_names, functions) = myunzip (Util.set2list set)

               fun subst v =
		   let fun loop (nil,nil) = NONE
			 | loop (v'::vs, n::ns) = 
			   if (Name.eq_var (v,v')) then 
			       SOME (App_e(Open, Var_e n, 
					   [Var_c poly_var_c], 
					   [Var_e poly_var_r], []))
			   else
			       loop (vs,ns)
		   in
		       loop (internal_names, external_names_r)
		   end

               fun reviseFunction (Function(effect,recursive,[],[(arg_var,arg_con)],[],body,body_con)) =
		   let
		       val var' = Name.fresh_var ()
		       val body' = Nilutil.substExpInExp subst body
		   in
		       Function(Total, Leaf, 
				[(poly_var_c, knd_arg)],
				[(poly_var_r, con_arg)],
				[],
				Let_e (Sequential,
				       [Fixopen_b
					(Util.list2set 
					 [(var', Function(effect,recursive,[],
							  [(arg_var,arg_con)],[],body',body_con))])],
				       Var_e var'),
				AllArrow_c(Open, effect, [], [arg_con], w0, body_con))
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
		   map (fn n_c => ((n_c, nullfunction_k, nullfunction_c)))
		       external_names_c

               val cbnd_knds = map (fn _ => nullfunction_k) external_names 

               val revised_functions = map reviseFunction functions

               val ebnd_entries = Listops.zip external_names_r revised_functions

               val ebnd_types = map (fn Function(_,_,carg,[(_,earg)],w,_,body_type) =>
				     AllArrow_c(Open,Total,carg,[earg],
						Word32.fromInt (List.length w),body_type)) revised_functions

	       val ebnds = [Fixopen_b (Util.list2set ebnd_entries)]

               val nil_cdecs = map (fn n_c => (n_c, nullfunction_k)) external_names_c

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

	       val (rest_context, rest_cbnds, rest_ebnds, rest_valuable, rest_ilvars_kept, rest_cons, rest_knds) = 
		   xbnds context'' (true,true) rest'
	   in
	       (rest_context,
		APP[LIST cbnds, rest_cbnds],
		APP[LIST ebnds, rest_ebnds],
		rest_valuable,
		external_names @ rest_ilvars_kept,
		ebnd_types @ rest_cons,
		cbnd_knds @ rest_knds)
	   end
       else
	   xbnds context (true,false) bnds
       
     | xbnds context (optimize_fun,_) (Il.BND_MOD(var,il_module) :: rest) =

       let
	   val (var_c, var_r, vmap') = splitVar (var, vmap_of context)
	   val {ebnd_cat, cbnd_cat, valuable,
		knd_c, type_r,...} = 
	       xmod context (il_module, SOME (var, var_c, var_r))

           val context' = 
	       update_NILctx
		(update_vmap(context, vmap'),
		 Nilcontext.insert_kind
		 (Nilcontext.insert_con(NILctx_of context, var_r, type_r), var_c, knd_c))

	   val (rest_context, rest_cbnds, rest_ebnds, rest_valuable, rest_ilvars_kept, rest_cons, rest_knds) = 
	       xbnds context' (optimize_fun,true) rest

       in
	   (rest_context,
	    APP [cbnd_cat, rest_cbnds],
	    APP [ebnd_cat, rest_ebnds],
	    valuable andalso rest_valuable,
	    var :: rest_ilvars_kept,
	    type_r :: rest_cons,
	    knd_c :: rest_knds)
       end

   and xsig' context (con0,Il.SIGNAT_FUNCTOR (var, sig_dom, sig_rng, arrow))=
       let
	   val (var_c, var_r, vmap') = splitVar (var, vmap_of context)
	   val (knd, con) = xsig context (Var_c var, sig_dom)
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
	       in
		   (Arrow_k (Open, [(var_c, knd)], knd'),
		    AllArrow_c (Open, xeffect arrow, [(var_c, knd)],
				[con], w0, con'))
	       end
       in
	   Nilcontext.c_insert_kind(NILctx_of context, var_c, knd, cont1)
       end

     | xsig' context (con0, Il.SIGNAT_STRUCTURE (_,sdecs)) =
       let
	   val {crdecs, erlabs, ercons} = 
	       xsdecs context (con0, fn _ => NONE, sdecs)
       in
	   (Record_k (Util.list2sequence crdecs),
	    Prim_c(Record_c erlabs, ercons))
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
		    
       
   and xsdecs context (args as (_,_,sdecs)) =
       let
	   val this_call = ! xmod_count
	   val _ = (xmod_count := this_call + 1
(*		    ;print ("Call " ^ (Int.toString this_call) ^ " to xsdecs\n") *)
	            (* ; Ppil.pp_sdecs sdecs *) )
	   val result = xsdecs' context args
	in
(*	    print ("Return " ^ (Int.toString this_call) ^ " from xsdecs\n"); *)
	    result
       end

   and xsdecs' context (con0, _, []) = {crdecs = nil, erlabs = nil, ercons = nil}
     | xsdecs' context (con0, subst,  
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
		       xsdecs context' (con0, extendmap (subst, var_c, Proj_c(con0, lbl)),
					rest)
	       in
		   {crdecs = ((lbl, var_c), knd) :: crdecs,
		    erlabs = lbl :: erlabs,
		    ercons = (Nilutil.substConInCon subst con) :: ercons}
	       end
       in
	   Nilcontext.c_insert_kind(NILctx_of context, var_c, knd, cont1)
       end

     | xsdecs' context(con0, subst, Il.SDEC(lbl, d as Il.DEC_EXP(var,con)) :: rest) =
       let
	   val (con',_) = xcon context con

	   fun cont1 NILctx' =
	       let
		   val context' = 
		       update_NILctx
		       (context, NILctx')
		       
		   val {crdecs, erlabs, ercons} = xsdecs context' (con0, subst, rest)
	       in
		   {crdecs = crdecs,
		    erlabs = lbl :: erlabs,
		    ercons = (Nilutil.substConInCon subst con') :: ercons}
	       end
       in
	   Nilcontext.c_insert_con(NILctx_of context, var, con', cont1)
       end

     | xsdecs' context (con0, subst, sdecs as Il.SDEC(lbl, d as Il.DEC_CON(var, knd, 
									maybecon))::rest)=
       let
	   val knd' = xkind knd
	   val knd'' =(case maybecon of
			   NONE => knd'
			 | SOME il_con => ((#2 (xcon context il_con)) handle _ => knd'))

	   fun cont1 NILctx' =
	       let
		   val context' = 
		       update_NILctx
		       (context, NILctx')
		       
		   val {crdecs, erlabs, ercons} = 
		       xsdecs context' (con0, extendmap (subst, var, Proj_c(con0, lbl)),
					rest)
	       in
		   {crdecs = ((lbl, var), knd'') :: crdecs,
		    erlabs = erlabs,
		    ercons = ercons}
	       end
       in
	   Nilcontext.c_insert_kind(NILctx_of context, var, knd'', cont1)
       end

   and xkind (Il.KIND_TUPLE n) = makeKindTuple n
     | xkind (Il.KIND_ARROW (1,m)) =
         Arrow_k (Open, 
		  [(Name.fresh_var(), Type_k Runtime)], 
		  makeKindTuple m)
     | xkind (Il.KIND_ARROW (n,m)) = 
         let
	     val (Record_k args) = makeKindTuple n
	 in
	     Arrow_k (Open,
		      map (fn ((_,v),k) => (v,k)) args,
		      makeKindTuple m)
	 end

   fun xHILctx context HILctx =
       let
	   fun folder (v,(l,pc),context) =
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
			    val nil_kind = xkind il_kind
			    val nil_con = ((SOME (#1 (xcon context il_con))) handle _ => NONE)
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
			in
			    update_NILctx
			    (context,
			     Nilcontext.insert_con
			     (Nilcontext.insert_kind
			      (NILctx_of context, v_c, knd_c),
			      v_r, type_r))
			end
		  | _ => context)
       in
	   Name.VarMap.foldli folder context (Ilcontext.Context_Varmap HILctx)
       end

   fun xcompunit HILctx vmap il_sbnds =
       let
           val empty_splitting_context = 
	         CONTEXT{NILctx = Nilcontext.empty(),
			 vmap = vmap}

	   val initial_splitting_context = xHILctx empty_splitting_context HILctx

	   val _ = 
	       if (!debug) then
		   (print "Initial HIL context:\n";
		    Ppil.pp_context HILctx;
		    print "Initial NIL context:\n";
		    Nilcontext.print_context (NILctx_of initial_splitting_context);
		    print "\n")
	       else
		   ()

	   val {cbnd_cat, ebnd_cat, vmap, ...} =
	       xsbnds initial_splitting_context il_sbnds

	   val cu_c_list = map Con_b (flattenCatlist cbnd_cat)

	   val cu_r_list = flattenCatlist ebnd_cat

       in
	   {cu_bnds = cu_c_list @ cu_r_list,
	    vmap = vmap}
       end

end

