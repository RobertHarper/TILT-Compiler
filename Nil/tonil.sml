functor Tonil(structure Ilstatic : ILSTATIC
              structure Ilutil : ILUTIL
              structure Nilutil : NILUTIL
              structure Ilcontext : ILCONTEXT
              structure Nilcontext : NILCONTEXT
              structure Ppnil : PPNIL
              structure Ppil : PPIL
                 sharing Ilutil.Il = Ilstatic.Il = Ppil.Il = Ilcontext.Il
		 sharing Nilutil.Nil.Prim = Ilstatic.Il.Prim
                 sharing Nilutil.Nil = Nilcontext.Nil = Ppnil.Nil
             ) : TONIL =
struct
   structure Il = Ilstatic.Il
   structure Nil = Nilutil.Nil

   open Nil
(*
   val debug = ref (Prim_c (Record_c nil, nil))
   val il_debug = ref (Il.CON_ANY)
*)

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
   fun gms (args as (_, module)) = 
       (Ilstatic.GetModSig args)

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


   (* xmod:  Translation of an IL module.

      {name_c, name_r, cbnd_cat, ebnd_cat, knd_c, type_r,
       il_signat, valuable} =
         xmod ctx (il_mod, vmap_0, SOME var)

      Preconditions:  

        (1) ctx |- il_mod : il_signat'
        (2) var not free in ctx.

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

   datatype splitting_context = CONTEXT of {HILctx : Il.context, 
					    NILctx : Nilcontext.context,
					    vmap   : (var * var) Name.VarMap.map}

   fun HILctx_of (CONTEXT{HILctx,...}) = HILctx
   fun NILctx_of (CONTEXT{NILctx,...}) = NILctx
   fun vmap_of (CONTEXT{vmap,...}) = vmap

   fun update_HILctx (CONTEXT{HILctx,NILctx,vmap}, HILctx') = 
       CONTEXT{HILctx=HILctx', NILctx=NILctx, vmap=vmap}
   fun update_vmap  (CONTEXT{HILctx,NILctx,vmap}, vmap') = 
       CONTEXT{HILctx=HILctx, NILctx=NILctx, vmap=vmap'}
   fun update_NILctx (CONTEXT{HILctx,NILctx,vmap}, NILctx') =
       CONTEXT{HILctx=HILctx, NILctx=NILctx', vmap=vmap}

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

	   val il_signat = gms (HILctx_of context, il_mod)

           val _ = (print "About to look up :\n";
		    Ppnil.pp_exp (Var_e var_mod_r);
		    print " and ";
                    Ppnil.pp_con (Var_c var_mod_c);
		    print "\n")

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
	    il_signat = il_signat,
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
		il_signat = Il.SIGNAT_FUNCTOR(var_funarg,ilsig_funarg,ilsig_funrng,fun_arrow),
		valuable = valuable_fun
		} = xmod context (ilmod_fun, NONE)

	   val {cbnd_cat = cbnd_cat_arg,
		ebnd_cat = ebnd_cat_arg,
                name_c = name_arg_c,
                name_r = name_arg_r,
		knd_c = knd_arg_c,
		type_r = type_arg_r,
		il_signat = Il.SIGNAT_STRUCTURE(_,arg_sdecs),
		valuable = valuable_arg
		} = xmod context (ilmod_arg, NONE)

           val name_c = Var_c var_c
	   val name_r = Var_e var_r
	   val il_signat = Ilutil.remove_modvar_signat(ilsig_funrng,var_funarg,arg_sdecs)
	   val (knd_c, type_r) = xsig context (name_c, il_signat)

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
	    il_signat = il_signat,
	    valuable = valuable_fun andalso valuable_arg andalso (fun_arrow = Il.TOTAL)}
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
		il_signat = il_mod_signat,
		valuable = mod_valuable, 
		...} = xmod context (il_module, NONE)

	   val (var_proj, var_proj_c, var_proj_r, vmap) = 
	       chooseName (preferred_name, vmap_of context)

           val (Il.SIGNAT_STRUCTURE(_,sdecs)) = 
	       Ilstatic.SelfifySig(Il.SIMPLE_PATH var_proj, il_mod_signat)
	   val SOME (_, Ilcontext.PHRASE_CLASS_MOD(_,il_proj_signat)) = 
	       Ilcontext.Sdecs_Lookup(Il.MOD_VAR var_proj, sdecs, lbls)

	   val name_proj_c = Var_c var_proj_c
	   val name_proj_r = Var_e var_proj_r

           val (knd_proj_c, type_proj_r) = xsig context (name_proj_c, il_proj_signat)

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
	    il_signat = il_proj_signat,
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
		il_signat = il_body_signat,
		valuable = body_valuable
		} = let
			val d = Il.DEC_MOD(var_arg, il_arg_signat)
	                
			fun cont1 NILctx' =
			    Nilcontext.c_insert_con(NILctx', var_arg_r, con_arg, cont2)

	                and cont2 NILctx'' =
			    let
				val d = Il.DEC_MOD(var_arg, il_arg_signat)
				val context'' = 
				    update_NILctx
				     (update_HILctx 
				      (context',
				       Ilcontext.add_context_dec 
				       (HILctx_of context, 
					Ilstatic.SelfifyDec d)),
				      NILctx'')
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

           val il_fun_signat = 
	       Il.SIGNAT_FUNCTOR(var_arg, il_arg_signat, il_body_signat, arrow)

           val local_name_fun_c = Name.fresh_var ()

	   val (knd_fun_c, type_fun_r) = xsig context' (name_fun_c, il_fun_signat)

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
	    il_signat = il_fun_signat,
	    valuable = true}
       end
   
     | xmod' context (Il.MOD_STRUCTURE sbnds, preferred_name) =
       let
	   val (var_str, var_str_c, var_str_r, _) = 
	       chooseName (preferred_name, vmap_of context)

	   val {cbnd_cat, crbnds, ebnd_cat, erlabels, erfields, ercons,
		il_sdecs, valuable, vmap_out} = xsbnds context sbnds

           val name_str_c = Var_c var_str_c
	   val name_str_r = Var_e var_str_r

           val il_str_signat = Il.SIGNAT_STRUCTURE(NONE, il_sdecs)

           val (knd_str_c, type_str_r) = xsig context (name_str_c, il_str_signat)

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
	    il_signat = il_str_signat,
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
		il_signat = il_loc_signat,
		valuable = loc_valuable,
	        ...} = xmod context (il_loc_mod, 
				   SOME (var_loc, var_loc_c, var_loc_r))

	   val {cbnd_cat = cbnd_body_cat,
		ebnd_cat = ebnd_body_cat,
		name_c = name_let_c,
		name_r = name_let_r,
		knd_c = knd_let_c,
		type_r = type_let_r,
		il_signat = il_body_signat,
		valuable = body_valuable,
                ...} =  let
			    fun cont1 NILctx' =
				Nilcontext.c_insert_con(NILctx', var_loc_r, type_loc_r, cont2)
				
			    and cont2 NILctx'' =
				let
				    val d = Il.DEC_MOD(var_loc, il_loc_signat)
				    val context' = 
					update_NILctx
					(update_HILctx (context, 
							Ilcontext.add_context_dec 
							(HILctx_of context, 
							 Ilstatic.SelfifyDec d)),
					 NILctx'')
				in
				    xmod context' (il_body_mod, preferred_name)
				end
			in
			    Nilcontext.c_insert_kind
			    (NILctx_of context, var_loc_c, knd_loc_c, cont1)
			end
		    
           val cbnd_let_cat = APP[cbnd_loc_cat, cbnd_body_cat]
           val ebnd_let_cat = APP[ebnd_loc_cat, ebnd_body_cat]

           (* SLOW! *)
	   val il_let_signat = gms (HILctx_of context, il_let_mod)

       in
	   {cbnd_cat = cbnd_let_cat,
	    ebnd_cat = ebnd_let_cat,
            name_c = name_let_c,
	    name_r = name_let_r,
	    knd_c = knd_let_c,
	    type_r = type_let_r,
	    il_signat = il_let_signat,
	    valuable = loc_valuable andalso body_valuable}
       end

   and xsbnds context ([]) =  
       {cbnd_cat = LIST nil, crbnds = nil, 
	ebnd_cat = LIST nil, erlabels = nil, 
	erfields = nil, ercons = nil,
	il_sdecs = nil,	valuable = true,
	vmap_out = vmap_of context}

     | xsbnds context (Il.SBND(lab, Il.BND_EXP(var, il_exp)) :: rest) = 
       let
	   val il_con = Ilstatic.GetExpCon (HILctx_of context, il_exp)
	   val il_dec = Il.DEC_EXP(var, il_con)
	   val context' = update_HILctx
	       (context, 
		Ilcontext.add_context_dec 
		(HILctx_of context, Ilstatic.SelfifyDec il_dec))

	   val (exp, tipe, valuable') = xexp context il_exp

	   val {cbnd_cat, crbnds, ebnd_cat, erlabels, 
		erfields, ercons, il_sdecs, valuable, vmap_out} = 
	       let
		   fun cont1 NILctx' =
		       let
			   val context' = 
			       update_NILctx
			       (update_HILctx(context, 
					      Ilcontext.add_context_dec 
					      (HILctx_of context, Ilstatic.SelfifyDec il_dec)),
				NILctx')
		       in
			   xsbnds context' rest
		       end
	       in
		   Nilcontext.c_insert_con(NILctx_of context, var, tipe, cont1)
	       end
		   
       in
	   {cbnd_cat = cbnd_cat,
	    crbnds = crbnds,
	    ebnd_cat = CONS (Exp_b (var, tipe, exp), ebnd_cat),
	    erlabels = lab :: erlabels,
	    erfields = (Var_e var) :: erfields,
	    ercons = tipe :: ercons,
	    il_sdecs = Il.SDEC(lab, il_dec) :: il_sdecs,
	    valuable = valuable andalso valuable',
	    vmap_out = vmap_out}
       end

     | xsbnds context (Il.SBND(lab, Il.BND_CON(var, il_con)) :: rest) = 
       let
           val il_knd = Ilstatic.GetConKind (HILctx_of context, il_con)
	   val il_dec = Il.DEC_CON(var, il_knd, SOME il_con)

           val (con,knd) = xcon context il_con

	   val {cbnd_cat, crbnds, ebnd_cat, erlabels, 
		erfields, ercons, il_sdecs, valuable, vmap_out} = 
	       let
		   fun cont1 NILctx' =
		       let
			   val context' = 
			       update_NILctx
			       (update_HILctx(context, 
					      Ilcontext.add_context_dec 
					      (HILctx_of context, Ilstatic.SelfifyDec il_dec)),
				NILctx')
		       in
			   xsbnds context' rest
		       end
	       in
		   Nilcontext.c_insert_kind(NILctx_of context, var, knd, cont1)
	       end

       in
	   {cbnd_cat = CONS((var, knd, con), cbnd_cat),
	    crbnds = (lab, Var_c var) :: crbnds,
	    ebnd_cat = ebnd_cat,
	    erlabels = erlabels, 
	    erfields = erfields,
	    ercons = ercons,
	    il_sdecs = Il.SDEC(lab, il_dec) :: il_sdecs,
	    valuable = valuable,
	    vmap_out = vmap_out}
       end

    | xsbnds context (Il.SBND(lab, Il.BND_MOD(var', il_mod)) :: rest) =
      let
	  val (var'_c, var'_r, vmap') = splitVar (var', vmap_of context)

	  val {cbnd_cat, ebnd_cat, il_signat, valuable = valuable', ...} = 
	      xmod context (il_mod, SOME (var', var'_c, var'_r))

	  val il_dec = Il.DEC_MOD(var', il_signat)

	  val (knd, con) = xsig context (Var_c var'_c, il_signat)

	  val {cbnd_cat = cbnd_cat', crbnds, ebnd_cat = ebnd_cat', erlabels, 
	       erfields, ercons, il_sdecs, valuable, vmap_out} = 
	       let
		   fun cont1 NILctx' =
		       Nilcontext.c_insert_con (NILctx', var'_r, con, cont2)
		   and cont2 NILctx'' =
		       let
			   val context' = 
			       update_NILctx
			       (update_HILctx(update_vmap(context, vmap'),
					      Ilcontext.add_context_dec 
					      (HILctx_of context, Ilstatic.SelfifyDec il_dec)),
				NILctx'')
		       in
			   xsbnds context' rest
		       end
	       in
		   Nilcontext.c_insert_kind(NILctx_of context, var'_c, knd, cont1)
	       end
	  
      in
	  {cbnd_cat = APP[cbnd_cat, cbnd_cat'],
	   crbnds = (lab, Var_c var'_c) :: crbnds,
	   ebnd_cat = APP[ebnd_cat, ebnd_cat'],
	   erlabels = lab :: erlabels,
	   erfields = (Var_e var'_r) :: erfields,
	   ercons = con :: ercons,
	   il_sdecs = Il.SDEC(lab, il_dec) :: il_sdecs,
	   valuable = valuable andalso valuable',
	   vmap_out = vmap_out}
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
	   val (SOME kind) = Nilcontext.find_kind (NILctx_of context, var)
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
	   val il_knd = Ilstatic.GetConKind (HILctx_of context, il_con)
	   val knd = xkind il_knd
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
		       (update_HILctx(context,
				     Ilcontext.add_context_decs 
				     (HILctx_of context,
				      (map (fn v => 
					    Il.DEC_CON(v, Il.KIND_TUPLE 1, NONE)) vars))),
			NILctx')

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
		       (update_HILctx(context,
				     Ilcontext.add_context_decs 
				     (HILctx_of context,
				      [Il.DEC_CON(var, Il.KIND_TUPLE 1, NONE)])),
			NILctx')
		       
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
		       (update_HILctx(context,
				     Ilcontext.add_context_decs 
				     (HILctx_of context, 
				      (map (fn v => 
					    Il.DEC_CON(v, Il.KIND_TUPLE 1, NONE)) vars))),
			NILctx')

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
	   val {cbnd_cat,name_c,il_signat,...} = 
	       xmod context (modv, NONE)

	   val var = Name.fresh_var ()
           val (Il.SIGNAT_STRUCTURE(_,sdecs)) = 
	       Ilstatic.SelfifySig(Il.SIMPLE_PATH var, il_signat)
	   val SOME(_, Ilcontext.PHRASE_CLASS_CON(_,il_knd)) = 
	       Ilcontext.Sdecs_Lookup(Il.MOD_VAR var, sdecs, [lbl])

	   val con = makeLetC (map Con_cb (flattenCatlist cbnd_cat))
	                      (Proj_c (name_c, lbl))
	   val knd = xkind il_knd
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
	   val temp = map (xexp context) il_args
	   val args = map #1 temp
	   val valuables = map #3 temp
	   val valuable = Listops.andfold (fn x => x) valuables
           (* slow *)
	   val il_con = Ilstatic.GetExpCon (HILctx_of context, il_exp)
           val valuable = Ilstatic.Exp_IsValuable (HILctx_of context, il_exp)

	   val (con, _) = xcon context il_con
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
	   val temp = map (xexp context) il_args
	   val args = map #1 temp
	   (* SLOW *)
	   val il_con = Ilstatic.GetExpCon (HILctx_of context, il_exp)
           val valuable = Ilstatic.Exp_IsValuable (HILctx_of context, il_exp)

	   val (con, _) = xcon context il_con
       in
	   (Prim_e (PrimOp (xilprim ilprim), [], args), con, valuable)
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
	   val il_con = Ilstatic.GetExpCon (HILctx_of context, il_exp)
	   val (con, _) = xcon context il_con
       in
	   (Var_e var, con, true)
       end

     | xexp context (il_exp as (Il.APP (il_exp1, il_exp2))) = 
       let
           (* SLOW *)
           val il_fun_con = Ilstatic.GetExpCon (HILctx_of context, il_exp1)
           val (il_arrow, il_fun_result) = 
	       (case (Ilstatic.con_head_normalize (HILctx_of context, il_fun_con)) of
                   Il.CON_ARROW(_,il_con,il_arrow) => 
		       (derefOneshot il_arrow, il_con)
                | _ => error "xexp: APP of non-arrow-type function")
           val (con, _) = xcon context il_fun_result

	   val (exp1, _, valuable) = xexp context il_exp1
	   val (exp2, _, valuable') = xexp context il_exp2

	   val valuable'' = (case il_arrow of
			       Il.TOTAL => valuable' andalso valuable
			     | Il.PARTIAL => false)
       in
	   (App_e (Open, exp1, [], [exp2], []), con, valuable'')
       end

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
	   val (exp, _, valuable) = xexp context il_exp
           val fields = (case (Ilstatic.con_head_normalize (HILctx_of context, 
							    il_record_con)) of
                           Il.CON_RECORD fields => fields
                         | hnf => (print "Oops\n";
				   Ppil.pp_exp il_exp0;
				   print "\n";
				   Ppil.pp_con il_record_con;
				   print "\n";
				   Ppil.pp_con hnf;
				   error "xexp: RECORD_PROJECT 1"))
           val il_con = (case (Listops.assoc_eq(Name.eq_label, label, fields)) of
			     SOME il_con => il_con
			   | NONE => (print "Oops\n";
				      Ppil.pp_exp il_exp0;
				      print "\n";
				      Ppil.pp_con il_record_con;
				      print "\n";
				      Ppil.pp_label label;
				      error "xexp: RECORD_PROJECT 2"))
	   val (con, _) = xcon context il_con
       in
	   (Prim_e (NilPrimOp (select label), [], [exp]), con, valuable)
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
	   val (context', bnds', valuable) = xbnds context true bnds
	   val (exp, con, valuable') = xexp context' il_exp
       in
	   (Let_e (Sequential, flattenCatlist bnds', exp), 
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

     | xexp context (il_exp as (Il.UNROLL (_, il_exp1))) = 
       let
	   val il_con = Ilstatic.GetExpCon (HILctx_of context, il_exp)
	   val (con, _) = xcon context il_con
	   val (exp1, con1, valuable) = xexp context il_exp1
       in
	   (Prim_e(NilPrimOp unroll, [con1], [exp1]), con, valuable)
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
		il_signat, valuable} = xmod context (module, NONE)

	   val var = Name.fresh_var ()
           val (Il.SIGNAT_STRUCTURE(_,sdecs)) = 
	       Ilstatic.SelfifySig(Il.SIMPLE_PATH var, il_signat)
	   val SOME(_, Ilcontext.PHRASE_CLASS_EXP(_,il_con)) = 
	       Ilcontext.Sdecs_Lookup(Il.MOD_VAR var, sdecs, [label])
	   val (con,_) = xcon context il_con
	     
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
		       (update_HILctx(context,
				     Ilcontext.add_context_decs
				     (HILctx_of context, 
				      (map (fn (Il.FBND(var,_,con,con',_)) => 
					    Il.DEC_EXP(var, Il.CON_ARROW(con,con',
									 Util.oneshot_init 
									 Il.PARTIAL)))
				       fbnds))),
		       NILctx')

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
				       (update_HILctx(context',
						      Ilcontext.add_context_exp'
						      (HILctx_of context', var', il_con1)),
					NILctx')
				       
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



   and xbnds context _ [] = (context, LIST nil, true)
     | xbnds context optimize_fun (Il.BND_EXP(var,il_exp) :: rest) =
       let 
	   val il_con = Ilstatic.GetExpCon (HILctx_of context, il_exp)
           val il_dec = Il.DEC_EXP(var, il_con)
	   val (exp, con, valuable) = xexp context il_exp

           val context' = 
	       update_HILctx
	       (update_NILctx(context,
			      Nilcontext.insert_con(NILctx_of context, var, con)),
		Ilcontext.add_context_exp'(HILctx_of context, var, il_con))

	   val (rest_context, rest_bnds, rest_valuable) = 
	       xbnds context' optimize_fun rest
       in
	   (rest_context,
	    APP[LIST [Exp_b(var, con, exp)], rest_bnds],
	    valuable andalso rest_valuable)
       end

     | xbnds context optimize_fun (Il.BND_CON(var,il_con) :: rest) =
       let
	   val il_knd = Ilstatic.GetConKind (HILctx_of context, il_con) 
	   val (con, knd) = xcon context il_con

           val context' = 
	       update_HILctx
	       (update_NILctx(context,
			      Nilcontext.insert_con(NILctx_of context, var, con)),
		Ilcontext.add_context_con'(HILctx_of context, var, il_knd, SOME il_con))

	   val (rest_context, rest_bnds, rest_valuable) = 
	       xbnds context' optimize_fun rest
       in
	   (rest_context,
	    APP [LIST [Con_b(var, knd, con)], rest_bnds],
	    rest_valuable)
       end

     | xbnds context optimize_fun (Il.BND_MOD(var,il_module) :: rest) =

       let
	   val (var_c, var_r, vmap') = splitVar (var, vmap_of context)
	   val {ebnd_cat, cbnd_cat, il_signat, valuable,
		knd_c, type_r,...} = 
	       xmod context (il_module, SOME (var, var_c, var_r))

	   val il_dec = Ilstatic.SelfifyDec (Il.DEC_MOD(var, il_signat))

           val context' = 
	       update_HILctx
	       (update_NILctx
		(update_vmap(context, vmap'),
		 Nilcontext.insert_kind
		 (Nilcontext.insert_con(NILctx_of context, var_r, type_r), var_c, knd_c)),
		Ilcontext.add_context_dec(HILctx_of context, il_dec))

	   val (rest_context, rest_bnds, rest_valuable) = 
	       xbnds context' optimize_fun rest

       in
	   (rest_context,
	    APP [LIST (map Con_b (flattenCatlist cbnd_cat)),
		 ebnd_cat, rest_bnds],
	    valuable andalso rest_valuable)
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
		       (update_HILctx
			(update_vmap(context, vmap'),
			 Ilcontext.add_context_dec 
			 (HILctx_of context, Ilstatic.SelfifyDec d)),
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
			(update_HILctx(context,
				       Ilcontext.add_context_dec
				       (HILctx_of context, Ilstatic.SelfifyDec d)),
			 vmap'),
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
		       (update_HILctx(context,
				      Ilcontext.add_context_dec
				      (HILctx_of context, Ilstatic.SelfifyDec d)),
			NILctx')
		       
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
			 | SOME il_con => #2 (xcon context il_con))

	   fun cont1 NILctx' =
	       let
		   val context' = 
		       update_NILctx
		       (update_HILctx(context,
				      Ilcontext.add_context_dec
				      (HILctx_of context, Ilstatic.SelfifyDec d)),
			NILctx')
		       
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

   fun xcompunit HILctx vmap il_sbnds =
       let
	   val (cuvar, cuvar_c, cuvar_r, vmap') = splitFreshVar vmap

           val initial_splitting_context = 
	         CONTEXT{HILctx = HILctx,
			 NILctx = Nilcontext.empty(),
			 vmap = vmap'}

	   val {cbnd_cat, ebnd_cat, vmap_out, ...} =
	       xsbnds initial_splitting_context il_sbnds

	   val cu_c_list = map Con_b (flattenCatlist cbnd_cat)

	   val cu_r_list = flattenCatlist ebnd_cat

       in
	   {cu_bnds = cu_c_list @ cu_r_list,
	    vmap = vmap_out}
       end
			     
			     
	       

end

