(*$import Prelude TopLevel Name Listops Util Sequence List Prim Tyvar Int Word32 Array TilWord64 TilWord32 Il Nil Ppil IlUtil IlContext IlStatic NilUtil NilError NilContext Ppnil NilSubst Normalize Stats LibBase TONIL Option NilPrimUtil *)

(* We box all floats and translate floating point operations accordingly.
   Thus, kind type is replaced by work types.
   Also note that all term-level (but not type-level) 
        record fields must be sorted by their labels. *)

structure Tonil :> TONIL =
struct


   structure VarSet = Name.VarSet

   open Nil Listops
   val diag       = Stats.tt("TonilDiag")
   val debug      = Stats.ff("TonilDebug")
   val full_debug = Stats.ff("TonilFullDebug")

   val do_kill_cpart_of_functor = Stats.tt("do_thin")
   val killDeadImport = Stats.tt("killDeadImport")
   val do_preproject = Stats.tt("do_preproject")
   val do_memoize = Stats.tt("do_memoize")
   val keep_hil_numbers = Stats.ff("keep_hil_numbers")
   val do_polyrec = Stats.tt("do_polyrec")

   val typeof_count = Stats.counter "PS_Typeofs"
     
   val elaborator_specific_optimizations = ref true
   fun error msg = Util.error "tonil.sml" msg

   fun msg str = if (!diag) then print str else ()

   val strip_arrow = NilUtil.strip_arrow

   val perr_c = NilError.perr_c
   val perr_e = NilError.perr_e
   val perr_k = NilError.perr_k
   val perr_c_k = NilError.perr_c_k
   val eq_label = Name.eq_label


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
	   	   
       fun newSplitVar (var, vmap) = 
	   let
	       val _ = (case (lookupVmap (var, vmap)) of
			    NONE => ()
			  | SOME _ => (print "Warning: newSplitVar called on alredy existing variable ";
				       Ppnil.pp_var var; print "\n"))
	       val var_name = if (!keep_hil_numbers) then
			    Name.var2string var
			      else
				  Name.var2name var
	       val var_c = Name.fresh_named_var (var_name ^ "_c")
	       val var_r = Name.fresh_named_var (var_name ^ "_r")
	   in
	       (var_c, var_r, addToVmap(vmap, var, var_c, var_r))
	   end
       
       fun splitVar (var, vmap) =
	   (case (lookupVmap (var, vmap)) of
		NONE => error ("splitVar called on non-existent variable " ^ (Name.var2string var))
	      | SOME (var_c, var_r) => (var_c, var_r, vmap))

   end

   fun insert_rename_var (v, rmap) = 
       let
	   val var_name = if (!keep_hil_numbers) then
	              Name.var2string v
		   else
		      Name.var2name v
	   val v' = Name.fresh_named_var var_name
       in
	   (v', Name.VarMap.insert (rmap, v, v'))
       end

   fun insert_rename_vars (vs, rmap) = 
       Listops.foldl_acc insert_rename_var rmap vs


   fun insert_given_vars ([],[],rmap) = rmap
     | insert_given_vars ((v::vs),(v'::vs'), rmap) = 
       insert_given_vars (vs, vs', Name.VarMap.insert(rmap, v, v'))

   fun rename_var (v, rmap) = 
       (case Name.VarMap.find(rmap, v) of 
	    SOME v' => v'
	  | NONE => (print "Couldn't find IL variable ";
		     Ppnil.pp_var v;
		     print " in rmap\n";
		     error "can't rename_var"))
   fun rename_vars(vs, rmap) = map (fn v => rename_var(v,rmap)) vs



   fun makeConb cbnd = Con_b (Runtime,cbnd)

   fun getSbndNames n sbnds =
       let 
	   fun loop 0 (rest, labs, vars) = (rest, rev labs, rev vars)
	     | loop n (Il.SBND(lab,bnd)::rest, labs, vars) = 
	       let val var = (case bnd of
				  Il.BND_MOD (v,_,_) => v
				| Il.BND_EXP (v,_) => v
				| Il.BND_CON (v,_) => v)
	       in  loop (n-1) (rest, lab::labs, var::vars)
	       end
	     | loop _ _ = error "getSbndName: ran out of bnds"
       in   loop n (sbnds,[],[])
       end

   fun getSdecNames n sbnds =
       let 
	   fun loop 0 (rest, labs, vars) = (rest, rev labs, rev vars)
	     | loop n (Il.SDEC(lab,bnd)::rest, labs, vars) = 
	       let val var = (case bnd of
				  Il.DEC_MOD (v,_,_) => v
				| Il.DEC_EXP (v,_,_,_) => v
				| Il.DEC_CON (v,_,_,_) => v)
	       in  loop (n-1) (rest, lab::labs, var::vars)
	       end
	     | loop _ _ = error "getSdecName: ran out of bnds"
       in   loop n (sbnds,[],[])
       end

   (* makeLabels.  Returns a list of labels for a tuple of length n. *)
   fun makeLabels n = Listops.map0count (fn n => IlUtil.generate_tuple_label(n+1)) n

   fun makeInternalLabels n = Listops.map0count (fn n => Name.fresh_internal_label "bnd") n

   (* makeVars.  Returns a list of fresh variables of length n. *)
   fun makeVars n = Listops.map0count (fn _ => Name.fresh_var ()) n

   (* makeKindTuple.  
         Creates the kind for a "tuple" of types of length n. 
         1-tuples yield kind Word, rather than a record kind.
    *)
   val makeKindTuple = NilUtil.kind_type_tuple

   fun substConInCon subst con = NilSubst.substConInCon subst con

   (*This will work as long as you don't do any composing of substitutions*)
   val addToConSubst = NilSubst.C.sim_add

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
     | xilprim _ = error "other ilprims should not get here"

   (* derefOneshot, derefTyvar, derefOvar.
        Partial functions to extract the values from "oneshot"-like
        objects.
    *)
   fun derefOneshot oneshot = 
       (case (Util.oneshot_deref oneshot) of
	     NONE   => error "(derefOneshot)  oneshot unset"
	  |  SOME x => x)

   fun derefTyvar tyvar =
       (case (Tyvar.tyvar_deref tyvar) of
	    NONE => error "(derefTyvar)  tyvar unset"
          | SOME x => x)

   fun derefOvar ovar = derefTyvar (Tyvar.ocon_deref ovar)

   (* selectFromCon.  Given con and a list lbls of labels,
         produce the constructor corresponding to con.lbls
    *)
   fun selectFromCon (con, []) = con
     | selectFromCon (con, lbl::lbls) = selectFromCon(Proj_c(con,lbl),lbls)

   (* selectFromRec.  Given a record expression r and a list lbls 
         of labels, produce the term corresponding to r.lbls
    *)
   fun selectFromRec (exp,[]) = exp
     | selectFromRec (exp,lbl::lbls) = selectFromRec(Prim_e(NilPrimOp(select lbl), [], 
							    [exp]), lbls)


   (* xmod:  Translation of an IL module.

      {name_c, name_r, cbnd_cat, ebnd_cat} =
         xmod ctx (il_mod, vmap_0, SOME var)

      Preconditions:  

        (1) var not free in ctx.

      Postconditions: 

        Let cbnds = flatten_catlist cbnd_cat, ebnds = flatten_catlist ebnd_cat

        (1) the compile-time part of mod is LET_C cbnds IN name_c END
        (3) the run-time part of mod is LET_E cbnds, ebnds IN name_r END
        (4) ctx |- il_signat == il_signat' : Sig
    *)

   val xmod_count = ref 0
   val xsig_count = ref 0
   val xcon_count = ref 0
   val xexp_count = ref 0
   val xsdecs_count = ref 0
   val xsbnds_count = ref 0

local
   datatype splitting_context = 
       CONTEXT of {NILctx : NilContext.context,
		   HILctx : Il.context,
		   sigmap : Il.signat Name.VarMap.map,
		   used   : Name.VarSet.set ref,
		   vmap   : (var * var) Name.VarMap.map,
		   rmap   : var Name.VarMap.map,
		   alias  : (var * label list) Name.VarMap.map,
		   memoized_mpath : (con * exp (* * kind*)) Name.PathMap.map,
                   polyfuns : Name.VarSet.set}
       

in
    type splitting_context = splitting_context
    fun get_nilctxt (CONTEXT{NILctx,...}) = NILctx
    fun get_hilctxt (CONTEXT{HILctx,...}) = HILctx
    fun empty_splitting_context HILctx = 
          CONTEXT{NILctx = NilContext.empty(),
		  HILctx = HILctx,
		  sigmap = Name.VarMap.empty,
		  used = ref Name.VarSet.empty,
		  vmap = Name.VarMap.empty,
		  rmap = Name.VarMap.empty,
		  alias = Name.VarMap.empty,
		  memoized_mpath = Name.PathMap.empty,
		  polyfuns = Name.VarSet.empty}

   fun print_splitting_context (CONTEXT{NILctx,vmap,rmap,...}) =
       (Name.VarMap.appi 
	   (fn (v,(vc,vr)) => (Ppnil.pp_var v; print "  -->  "; 
			       Ppnil.pp_var vc; print ", ";
			       Ppnil.pp_var vr; print "\n")) vmap;
	
	print "\n";
	Name.VarMap.appi
	   (fn (v,v') => (Ppnil.pp_var v; print "  -->  "; 
			  Ppnil.pp_var v'; print "\n")) rmap;
	print "\n";
	NilContext.print_context NILctx;
	print "\n")

   fun filter_NILctx (CONTEXT{NILctx,used,...}) = (NILctx, !used)

   fun find_sig(CONTEXT{sigmap,...},v) = Name.VarMap.find(sigmap,v)

   fun NilContext_use_var(CONTEXT{used,...},v) = 
       if (Name.VarSet.member(!used,v))
	   then ()
       else used := Name.VarSet.add(!used,v)

   fun NilContext_reset_used(CONTEXT{used,...}) =
       (used := Name.VarSet.empty)

   fun NilContext_find_kind(ctxt as CONTEXT{NILctx,used,...},v) = 
       (let val k = NilContext.find_kind(NILctx,v)
	in  SOME k
	end
	handle NilContext.Unbound => NONE)
			    

   fun reduce_to_sum(CONTEXT{NILctx,...},c) = Normalize.reduceToSumtype(NILctx,c)
   fun NilContext_con_hnf(CONTEXT{NILctx,...},c) = #2(Normalize.reduce_hnf(NILctx,c))
   fun NilContext_print(CONTEXT{NILctx,...}) = NilContext.print_context NILctx

   fun NilContext_kind_standardize(CONTEXT{NILctx,...},k) = NilContext.kind_standardize(NILctx, k)
   fun NilContext_kind_of(CONTEXT{NILctx,...},c) = NilContext.kind_of(NILctx, c)

   val splitVar = fn (var,CONTEXT{NILctx,HILctx,sigmap,
			     used,vmap,rmap,alias,memoized_mpath,polyfuns}) =>
                  let val (var_c,var_r,vmap') = splitVar(var,vmap)
		  in  ((var_c,var_r),
		       CONTEXT{NILctx=NILctx, HILctx=HILctx,
			       sigmap=sigmap,
			       used=used, vmap=vmap', rmap=rmap,
			       memoized_mpath=memoized_mpath,
			       alias=alias,polyfuns=polyfuns})
		  end

   val newSplitVar = fn (var,CONTEXT{NILctx,HILctx,sigmap,
				     used,vmap,rmap,alias,memoized_mpath,polyfuns}) =>
       let val (var_c,var_r,vmap') = newSplitVar(var,vmap)
       in  ((var_c,var_r),
	    CONTEXT{NILctx=NILctx, HILctx=HILctx,
		    sigmap=sigmap,
		    used=used, vmap=vmap', rmap=rmap,
		    memoized_mpath=memoized_mpath,
		    alias=alias,polyfuns=polyfuns})
       end

   val chooseName = (fn (NONE, ctxt) => let val v = Name.fresh_var()
					    val ((vc,vr),ctxt) = newSplitVar(v,ctxt)
					in  (v,vc,vr,ctxt)
					end
		      | (SOME (var,var_c,var_r), ctxt) => (var, var_c, var_r, ctxt))

   val lookupVmap = fn (var, CONTEXT{vmap,...}) => lookupVmap (var,vmap)

   val insert_rename_var = fn (v, CONTEXT{NILctx,HILctx,sigmap,
					  used,vmap,rmap,alias,memoized_mpath,polyfuns}) =>
       let
	   val (v',rmap') = insert_rename_var(v,rmap)
       in
	   (v', CONTEXT{NILctx=NILctx, HILctx=HILctx,
			sigmap=sigmap,
			used=used, vmap=vmap, rmap=rmap',
			memoized_mpath=memoized_mpath,
			alias=alias,polyfuns=polyfuns})
       end

   val insert_rename_vars = 
       fn (vs, CONTEXT{NILctx,HILctx,sigmap,
		       used,vmap,rmap,alias,memoized_mpath,polyfuns}) =>
       let
	   val (vs',rmap') = insert_rename_vars(vs,rmap)
       in
	   (vs', CONTEXT{NILctx=NILctx, HILctx=HILctx,
			 sigmap=sigmap,
			 used=used, vmap=vmap, rmap=rmap',
			 memoized_mpath=memoized_mpath,
			 alias=alias,polyfuns=polyfuns})
       end


   val insert_given_vars =
       fn (vs, vs', CONTEXT{NILctx,HILctx,sigmap,
			    used,vmap,rmap,alias,memoized_mpath,polyfuns}) =>
       let
	   val rmap' = insert_given_vars(vs,vs',rmap)
       in
	   CONTEXT{NILctx=NILctx, HILctx=HILctx, sigmap=sigmap,
		   used=used, vmap=vmap, rmap=rmap',
		   memoized_mpath=memoized_mpath,alias=alias,polyfuns=polyfuns}
       end

   val rename_var = fn (v, CONTEXT{rmap,...}) => rename_var(v,rmap)
   val rename_vars = fn (vs, CONTEXT{rmap,...}) => rename_vars(vs, rmap)


   fun update_insert_sig  (CONTEXT{NILctx,HILctx,sigmap,
			     used,vmap,rmap,alias,memoized_mpath,polyfuns}, 
			   v, hilsig) = 
       CONTEXT{NILctx=NILctx, HILctx=HILctx,
	       sigmap=Name.VarMap.insert(sigmap,v,hilsig),
	       used=used, vmap=vmap, rmap=rmap,
	       memoized_mpath=memoized_mpath,alias=alias,polyfuns=polyfuns}

   fun update_polyfuns  (CONTEXT{NILctx,HILctx,sigmap,
			     used,vmap,rmap,alias,memoized_mpath,polyfuns}, 
			   v) = 
       CONTEXT{NILctx=NILctx, HILctx=HILctx, sigmap=sigmap,
	       used=used, vmap=vmap, rmap=rmap,
	       memoized_mpath=memoized_mpath,alias=alias,
	       polyfuns=Name.VarSet.add(polyfuns,v)}

   fun update_polyfuns_list(CONTEXT{NILctx,HILctx,sigmap,
			     used,rmap,vmap,alias,memoized_mpath,polyfuns}, 
			   vs) = 
       CONTEXT{NILctx=NILctx, HILctx=HILctx, sigmap=sigmap,
	       used=used, vmap=vmap, rmap=rmap,
	       memoized_mpath=memoized_mpath,alias=alias,
	       polyfuns=Name.VarSet.addList(polyfuns,vs)}

   fun var_is_polyfun (CONTEXT{polyfuns,...}, v) = 
       Name.VarSet.member(polyfuns,v)

   fun update_NILctx_insert_kind(CONTEXT{NILctx,HILctx,sigmap,vmap,rmap, used,
					 memoized_mpath,alias,polyfuns},v,k) = 
       let 
	   val NILctx' = NilContext.insert_kind(NILctx,v,k)
       in  CONTEXT{NILctx=NILctx', HILctx=HILctx,
		   sigmap=sigmap, vmap=vmap, rmap=rmap, used = used, 
		   memoized_mpath=memoized_mpath, alias=alias,
		   polyfuns=polyfuns}
       end

   fun update_NILctx_insert_kind_equation(CONTEXT{NILctx,HILctx,sigmap,vmap,rmap, used,
						memoized_mpath,alias,polyfuns},
					  v,c) = 
       let val k = Single_k c
	   val NILctx' = NilContext.insert_kind(NILctx,v,k)
       in  CONTEXT{NILctx=NILctx', HILctx=HILctx,
		   sigmap=sigmap, vmap=vmap, rmap=rmap, used = used, 
		   memoized_mpath=memoized_mpath, alias=alias,
		   polyfuns=polyfuns}
       end

   fun update_NILctx_insert_kind_list(ctxt,vklist) = 
       foldl (fn ((v,k),ctxt) => update_NILctx_insert_kind (ctxt,v,k)) ctxt vklist

   fun update_NILctx_insert_label(CONTEXT{NILctx,HILctx,sigmap,vmap,rmap,used,
					  memoized_mpath,alias,polyfuns},
				  l,v) =
       let val NILctx' = NilContext.insert_label(NILctx,l,v)
       in  CONTEXT{NILctx=NILctx', HILctx=HILctx,
		   sigmap=sigmap, vmap=vmap, rmap=rmap, used = used, 
		   memoized_mpath=memoized_mpath, alias=alias,
		   polyfuns=polyfuns}
       end

    fun add_modvar_alias(CONTEXT{NILctx,HILctx,sigmap,vmap,rmap,used,
				 memoized_mpath,alias,polyfuns},var,path) =
	let val alias' = Name.VarMap.insert(alias,var,path)
	in  CONTEXT{NILctx=NILctx, HILctx=HILctx, sigmap=sigmap, vmap=vmap, rmap=rmap,  
		    used=used,alias=alias',  memoized_mpath=memoized_mpath,
		    polyfuns=polyfuns}
	end

    fun add_module_alias(CONTEXT{NILctx,HILctx,sigmap,vmap,rmap,used,alias,
				 memoized_mpath,polyfuns},m,name_c,name_r(*,k1*)) = 
	case (extractPathLabels m) of
		  (Il.MOD_VAR v, labs) => 
		      let val p = (v,labs)
			  val memoized_mpath' = Name.PathMap.insert(memoized_mpath,p,(name_c,name_r(*,k1*)))
		      in  CONTEXT{NILctx=NILctx, HILctx=HILctx,
				  sigmap=sigmap, vmap=vmap, rmap=rmap,
				  used=used,alias=alias,  
				  memoized_mpath=memoized_mpath',
				  polyfuns = polyfuns}
		      end
		| _ => error "add_module_alias given non-path module"

    fun lookup_module_alias(CONTEXT{alias,memoized_mpath,...},m) = 
	case (extractPathLabels m) of
	    (Il.MOD_VAR v,labs) => 
		let fun follow_alias(v,labs) = 
		    (case (Name.VarMap.find(alias,v)) of
			 NONE => (v,labs)
		       | SOME (v',labs') => follow_alias(v',labs' @ labs))
		    val p = follow_alias(v,labs)
		in  Name.PathMap.find(memoized_mpath,p)
		end
	  | _ =>  error "lookup_module_alias given non-path module"
		



end (* local defining splitting context *)





       type con_result = con 
       type mod_result = {cbnd_cat : conbnd catlist,
			  ebnd_cat : bnd catlist,
			  name_c : con,
			  name_r : exp,
(*			  knd_c : kind,*)
			  context : splitting_context}
   local
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
				in  if (!do_memoize)
				    then con_memo := Name.VarMap.insert(!con_memo,v,
								   Name.PathMap.insert(pathmap,path,res))
				    else ();
				    res
				end))

       fun lookup_mod_memo (context,preferred_name) (path as (v,lbls)) thunk =
	     (case Name.VarMap.find(!mod_memo,v) of
		NONE => (mod_memo := Name.VarMap.insert(!mod_memo,v,Name.PathMap.empty);
			 lookup_mod_memo (context,preferred_name) (v,lbls) thunk)
	      | SOME pathmap =>
		   (case Name.PathMap.find(pathmap,path) of
			SOME result => 
			    let val {name_c,name_r,(*knd_c,*)
				     ebnd_cat,cbnd_cat,context=_} = result
			 	val (name_c,name_r,cbnd_cat,ebnd_cat,context) = 
				    (case (name_c,name_r,preferred_name) of
					 (Var_c vc, Var_e vr, SOME(_,pc,pr)) =>
					     (if (Name.eq_var(vc,pc) andalso
						  Name.eq_var(vr,pr))
						then (name_c, name_r, LIST[], LIST[], context)
					 else
					  (Var_c pc, Var_e pr,
						LIST[Con_cb(pc,Var_c vc)],
						LIST[Exp_b(pr,TraceUnknown,Var_e vr)],
						update_NILctx_insert_kind_equation(context,pc,
						Var_c vc)))
					| _ => (name_c, name_r, LIST[], LIST[], context))

				val _ = (print "\nlookup_mod_memo of "; Ppnil.pp_var v;
				Ppnil.pp_list Ppnil.pp_label' lbls ("",".","",false);
(*				print " returning memoized result\n  knd_c = \n";
				Ppnil.pp_kind knd_c; *) print "\n\n")
			    in  {cbnd_cat = cbnd_cat, ebnd_cat = ebnd_cat,
				 name_c = name_c, name_r = name_r,
				(* knd_c = knd_c, *)
				 
				 (* these fields are not cached *)
				 context = context} : mod_result
			    end
		      | NONE => let val res = thunk()
				    val _ = if (!do_memoize)
					then mod_memo := Name.VarMap.insert
					(!mod_memo,v,Name.PathMap.insert(pathmap,path,res))
					else ()
				(*val {knd_c,...} = res *)
				val _ = (print "\nlookup_mod_memo of "; Ppnil.pp_var v;
				Ppnil.pp_list Ppnil.pp_label' lbls ("",".","",false);
				(*print " returning first-time result\n  knd_c = \n";
				Ppnil.pp_kind knd_c; *)print "\n\n")
				in  res
				end)
			handle e => 
			(* some variable has gone out of scope! must clear the memo *)
			    (clear_memo v;
			     lookup_mod_memo (context,preferred_name) path thunk)
			)

   end

   fun xmod context (args as (il_mod, preferred_name)) : mod_result =
       let
	   val this_call = ! xmod_count
	   val _ = 
	       if (!debug) then
		   (xmod_count := this_call + 1;
		    print ("\nCall " ^ (Int.toString this_call) ^ " to xmod\n");
		    if (!full_debug) then Ppil.pp_mod il_mod else ();
		    print"\n")
	       else ()


	   fun check_proj(Il.MOD_VAR v,[]) = (xmod' context args) : mod_result
	     | check_proj(Il.MOD_VAR v, _) = 
		 (case (lookup_module_alias(context,il_mod)) of
			NONE => (if (!debug)
				     then (print "--- lookup_module_alias failed to find ";
					   Ppil.pp_mod il_mod; print "\n")
				 else ();
				xmod' context args)
		      | SOME(name_c,name_r(*,knd_c*)) =>
			let val (name_c,name_r,cbnd_cat,ebnd_cat,context) = 
				(case preferred_name of 
				  NONE => (name_c,name_r,LIST[],LIST[],context)
				| SOME(_,pc,pr) => 
					let val context = update_NILctx_insert_kind_equation(context,pc,
										    name_c)
					in  (Var_c pc, Var_e pr, 
						LIST[Con_cb(pc,name_c)], LIST[Exp_b(pr,TraceUnknown, name_r)],
						context)
					end)
			in {cbnd_cat=cbnd_cat, ebnd_cat=ebnd_cat, name_c=name_c, name_r=name_r,
				context = context(*, knd_c=knd_c*)}

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
	let 
	    fun find_structure_paths m acc 
		(Il.SIGNAT_STRUCTURE((Il.SDEC(l,Il.DEC_MOD(_,false,s)))::rest)) = 
  		   if (Name.is_dt l) then
		       find_structure_paths m acc (Il.SIGNAT_STRUCTURE(rest))
		   else	
		       let val acc = (Il.MOD_PROJECT(m,l))::acc
			   val acc = find_structure_paths (Il.MOD_PROJECT(m,l)) acc s
		       in  find_structure_paths m acc (Il.SIGNAT_STRUCTURE(rest))
		       end
	      | find_structure_paths m acc (Il.SIGNAT_STRUCTURE(_::rest)) =
		   find_structure_paths m acc (Il.SIGNAT_STRUCTURE(rest))
	      | find_structure_paths m acc _ = acc

	    val rev_paths : Il.mod list = find_structure_paths (Il.MOD_VAR var_arg) [] il_signat

	    val _ =  if (!debug)
		     then (print "preproject: there are "; 
			   print (Int.toString (length rev_paths)); print " paths\n")
		 else ()

	    fun folder (mpath,(cbnds,ebnds,context)) = 
		let fun loop (Il.MOD_VAR v) acc = (Name.var2name v) ^ acc
		      | loop (Il.MOD_PROJECT (m,l)) acc = loop m ("_" ^ (Name.label2name l) ^ acc)
		    val var = Name.fresh_named_var(loop mpath "")
		    val ((var_c, var_r), context) = newSplitVar (var, context)
		    val {cbnd_cat : conbnd catlist,
			 ebnd_cat,name_c,name_r,
			 (*knd_c,*)context} =
			  xmod context (mpath,SOME(var,var_c,var_r))
		    val cbnds = cbnd_cat::cbnds
		    val ebnds = ebnd_cat::ebnds
		    val context = add_module_alias(context,mpath,name_c,name_r(*,knd_c*))
		in  (cbnds,ebnds,context)
		end

	    val (rev_cbnds,rev_ebnds,context) = foldr folder ([],[],context) rev_paths

	in  
	    {cbnd_cat = APPEND (rev rev_cbnds), 
	     ebnd_cat = APPEND (rev rev_ebnds), 
	     context = context}
	end

   and xmod' context (il_mod as (Il.MOD_VAR var_mod), preferred_name) : mod_result = 
       let
	   val ((var_mod_c, var_mod_r), context) = splitVar (var_mod, context)
	   val _ = NilContext_use_var(context,var_mod_r)
	   val _ = NilContext_use_var(context,var_mod_c)

           val _ = if (!full_debug)
		       then (print "About to look up :\n";
			     Ppnil.pp_exp (Var_e var_mod_r);
			     print " and ";
			     Ppnil.pp_con (Var_c var_mod_c);
			     print "\n")
		   else ()
(*
           val knd_c = Single_k(Var_c var_mod_c)
*)
	   val (var_mod_c,cbnds,context) = (var_mod_c,[],context)
	       

	   val (name_c, name_r) = 
	       (case preferred_name of
		    NONE => (Var_c var_mod_c, Var_e var_mod_r)
		  | SOME (_, name_c, name_r) => (Var_c name_c, Var_e name_r))

	   val (cbnd_cat, ebnd_cat, context) =
	       (case preferred_name of
		    NONE => (LIST cbnds, LIST nil, context)
		  | SOME (_, name_c, name_r) => 
			(APPEND[LIST cbnds, LIST[Con_cb(name_c, Var_c var_mod_c)]],
			 LIST [Exp_b (name_r, TraceUnknown, Var_e var_mod_r)],
			 update_NILctx_insert_kind_equation
			  (context, name_c, Var_c var_mod_c)))

       in
	   {cbnd_cat = cbnd_cat,
	    ebnd_cat = ebnd_cat,
            name_c   = name_c,
            name_r   = name_r,
(*
	    knd_c = knd_c,
*)
	    context  = context}
       end

     | xmod' context (Il.MOD_APP(ilmod_fun, ilmod_arg), preferred_name) =
       let

	   val {cbnd_cat = cbnd_cat_fun,
		ebnd_cat = ebnd_cat_fun,
		name_c = name_fun_c,
                name_r = name_fun_r,
(*		knd_c = knd_fun_c,*)
		context = context
		} = xmod context (ilmod_fun, NONE)

	   val {cbnd_cat = cbnd_cat_arg,
		ebnd_cat = ebnd_cat_arg,
		name_c = name_arg_c,
		name_r = name_arg_r,
(*		knd_c = knd_arg_c, *)
		context = context
		} = xmod context (ilmod_arg, NONE)
	       
	   val (var, var_c, var_r, context) = chooseName (preferred_name, context)
	   val name_c = Var_c var_c
	   val name_r = Var_e var_r
(*	       
	   val Arrow_k(_, [_], con_body_kind) = NilContext_kind_standardize(context, knd_fun_c)
	   val knd_c = varConKindSubst var_c name_arg_c con_body_kind
*)

           val con_new = App_c(name_fun_c,[name_arg_c])
	   val cbnd_cat_new = LIST[Con_cb(var_c, con_new)]
	   val cbnd_cat = APPEND[cbnd_cat_fun, cbnd_cat_arg, cbnd_cat_new]
(*
	   val context = update_NILctx_insert_kind(context,var_c,knd_c)
*)
	   val context = update_NILctx_insert_kind_equation(context, var_c, con_new)
       
	   val ebnd_cat_new = LIST[Exp_b(var_r, TraceUnknown,
					 NilUtil.makeAppE
					 name_fun_r
					 [name_arg_c]
					 [name_arg_r]
					 [])]
	   val ebnd_cat = APPEND[ebnd_cat_fun, 
				 ebnd_cat_arg,
				 ebnd_cat_new]

       in
	   {cbnd_cat  = cbnd_cat,
	    ebnd_cat  = ebnd_cat,
	    name_c    = name_c,
	    name_r    = name_r,
(*	    knd_c = knd_c,*)
	    context  = context}
       end

     | xmod' context (Il.MOD_SEAL(il_mod,_), preferred_name) = 
       (* The phase-splitting breaks abstraction *)
       xmod context (il_mod, preferred_name)
    
     | xmod' context (initial_mod as (Il.MOD_PROJECT _), preferred_name) =
       let
(*           val (il_module, lbls) = extractPathLabels initial_mod *)
           val Il.MOD_PROJECT(il_module, lbl) = initial_mod
	   val lbls = [lbl]

	   val _ = app (fn l => if (Name.is_dt l)
				    then error "use of datatype labels detected"
				else ()) lbls

	   val {cbnd_cat = cbnd_mod_cat, 
		ebnd_cat = ebnd_mod_cat,
		name_c   = name_mod_c, 
		name_r   = name_mod_r,
(*		knd_c = knd_mod_c, *)
		context  = context,
		...} = xmod context (il_module, NONE)

	   val (var_proj, var_proj_c, var_proj_r, context) = 
	       chooseName (preferred_name, context)


	   val name_proj_c = Var_c var_proj_c
	   val name_proj_r = Var_e var_proj_r

	   val con_proj_c = selectFromCon(name_mod_c, lbls)
(*
	   val knd_proj_c = Single_k(NilUtil.makeLetC (flattenCatlist cbnd_mod_cat) con_proj_c)
*)
	   val context = update_NILctx_insert_kind_equation(context,var_proj_c,con_proj_c)
	   val cbnd_cat_new = LIST [Con_cb(var_proj_c, con_proj_c)]
           val cbnd_proj_cat = APPEND[cbnd_mod_cat,cbnd_cat_new]

	   val ebnd_cat_new = LIST [Exp_b(var_proj_r, TraceUnknown,
					  selectFromRec(name_mod_r,lbls))]
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
(*	    knd_c = knd_proj_c, *)
	    context  = context}
       end

     | xmod' context (Il.MOD_FUNCTOR(arrow,var_arg, il_arg_signat, ilmod_body, ilmod_signat), 
		    preferred_name) =
       let
	   (* Pick the name of the result *)
	   val (var_fun, var_fun_c, var_fun_r, context) = 
	       chooseName (preferred_name, context)
           val name_fun_c = Var_c var_fun_c
	   val name_fun_r = Var_e var_fun_r

	   (* Split the argument parameter *)
	   val ((var_arg_c, var_arg_r), context') = newSplitVar (var_arg, context)


	   val _ = clear_memo var_arg

	   val (knd_arg, con_arg) = 
	         xsig context' (Var_c var_arg_c, il_arg_signat)

	   val context' = update_NILctx_insert_kind(context', var_arg_c, knd_arg)

	   val (_,con_res) = xsig context' (App_c(name_fun_c, [Var_c var_arg_c]), 
					    ilmod_signat)

           val (cbnd_preproject_cat,ebnd_preproject_cat,context') = 
	       if (!do_preproject)
		   then let val {cbnd_cat = cbnd_preproject_cat,
				 ebnd_cat = ebnd_preproject_cat,
				 context = context'} = 
		             preproject(var_arg,il_arg_signat,context')
			in (cbnd_preproject_cat, ebnd_preproject_cat, context')
			end
	       else (LIST[], LIST[], context')

           (* Split the functor body *)
		
	   val {cbnd_cat = cbnd_body_cat, 
		ebnd_cat = ebnd_body_cat, 
		name_c = name_body_c,
		name_r = name_body_r,
(*		knd_c = knd_body_c, *)
		context = _
		} = xmod context' (ilmod_body, NONE)

	   val cbnd_body_cat = APPEND[cbnd_preproject_cat, cbnd_body_cat]
	   val ebnd_body_cat = APPEND[ebnd_preproject_cat, ebnd_body_cat]

	   val effect = xeffect arrow

           val cbnds_body = flattenCatlist cbnd_body_cat
           val ebnds_body = flattenCatlist ebnd_body_cat

	   val con_res' = NilUtil.makeLetC cbnds_body con_res
(*
	   val knd_fun_c = Arrow_k(Open, [(var_arg_c, knd_arg)], knd_body_c)
*)
           val con_body = NilUtil.makeLetC cbnds_body name_body_c
           val cbnd_fun_cat = 
	       LIST[Open_cb(var_fun_c, [(var_arg_c, knd_arg)],
			    con_body)]

	   val ebnd_fun_cat =  
	       LIST[Fixopen_b (Sequence.fromList
			      [(var_fun_r,
			       Function{recursive = Leaf,
					effect = effect,
					isDependent =  true,
					tFormals = [(var_arg_c, knd_arg)],
					eFormals = [(var_arg_r, TraceUnknown, con_arg)],
					fFormals = [],
					body = (NilUtil.makeLetE Sequential
						((map makeConb cbnds_body) @ ebnds_body)
						name_body_r),
					body_type = con_res'})])]
	   val context = update_NILctx_insert_kind(context, var_fun_c, 
                           Arrow_k(Open, [(var_arg_c, knd_arg)], Single_k(con_body)))

       in
	   {cbnd_cat = cbnd_fun_cat,
            ebnd_cat = ebnd_fun_cat,
	    name_c = name_fun_c,
	    name_r = name_fun_r,
	    (* knd_c = knd_fun_c, *)
	    context = context}
       end
   
     | xmod' context (Il.MOD_STRUCTURE sbnds, preferred_name) =
       let
           (* XXX is final_context =context ok, or should it be thrown away? *)
	   val {final_context = context, 
		cbnd_cat, ebnd_cat, record_c_con_items,
		record_c_knd_items, 
		record_r_exp_items : (label * exp) list} = 
		(xsbnds context sbnds)

	   val (var_str, var_str_c, var_str_r, context) = 
	       chooseName (preferred_name, context)

           val name_str_c = Var_c var_str_c
	   val name_str_r = Var_e var_str_r
(*
	   val knd_str_c = Record_k (Sequence.fromList record_c_knd_items)
*)
	   val con_str_c = Crecord_c record_c_con_items
	   val cbnd_cat_new = LIST [Con_cb(var_str_c, con_str_c)]
           val cbnd_str_cat = APPEND[cbnd_cat,cbnd_cat_new]

           val specialize =
	       (case (!elaborator_specific_optimizations, sbnds) of
		    (true, [Il.SBND(lab, Il.BND_EXP _)]) => Name.eq_label (lab, IlUtil.it_lab)
		  | _ => false)

	   val ebnd_cat_new = 
	       if specialize 
		   then LIST[Exp_b (var_str_r, TraceUnknown, #2(hd record_r_exp_items))]
	       else     LIST[Exp_b (var_str_r, TraceUnknown,
				    Prim_e (NilPrimOp (record (map #1 record_r_exp_items)),
					    [], map #2 record_r_exp_items))]
	   val ebnd_str_cat = APPEND[ebnd_cat,ebnd_cat_new]


	   val context = update_NILctx_insert_kind_equation(context, var_str_c, con_str_c)

       in
	   {cbnd_cat = cbnd_str_cat,
	    ebnd_cat = ebnd_str_cat,
            name_c = name_str_c,
	    name_r = name_str_r,
(* 	    knd_c = knd_str_c, *)
	    context = context}
       end

    | xmod' context (Il.MOD_LET (var_loc, il_loc_mod, il_body_mod),
		     preferred_name) =
       let

	   val _ = clear_memo var_loc

	   val ((var_loc_c, var_loc_r), context) = newSplitVar (var_loc, context)

	   val {cbnd_cat = cbnd_loc_cat,
		ebnd_cat = ebnd_loc_cat,
(*		knd_c = knd_loc_c, *)
		context = context,
		...} = xmod context (il_loc_mod, SOME (var_loc, var_loc_c, var_loc_r))
(*
           val _ = print "BETWEEN MOD_LET: Context =\n"
           val _ = print_splitting_context context
*)
	   val {cbnd_cat = cbnd_body_cat,
		ebnd_cat = ebnd_body_cat,
		name_c = name_let_c,
		name_r = name_let_r,
(*		knd_c = knd_let_c, *)
		context = context} =  
	       xmod context (il_body_mod, preferred_name)

           val cbnd_let_cat = APPEND[cbnd_loc_cat, cbnd_body_cat]
           val ebnd_let_cat = APPEND[ebnd_loc_cat, ebnd_body_cat]


       in
	   {cbnd_cat = cbnd_let_cat,
	    ebnd_cat = ebnd_let_cat,
            name_c = name_let_c,
	    name_r = name_let_r,
(*	    knd_c = knd_let_c, *)
	    context = context}
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
		        print ("\n")) else ())
	       else ()

	   val result = (xsbnds_rewrite_1 context il_sbnds)
	       handle e => (if (!debug) then (print ("Exception detected in call " ^ 
						    (Int.toString this_call) ^ " to xsbnds\n");
(*
					      print "\nwith context = \n";
					      print_splitting_context context;
*)
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
	record_c_con_items = nil,
	record_c_knd_items = nil,
	record_r_exp_items = nil}

     | xsbnds_rewrite_1 context (il_sbnds as (Il.SBND(lab, Il.BND_MOD(var,_, _)))::rest_il_sbnds) =
        if (Name.is_dt lab) then
	    xsbnds context rest_il_sbnds
        else
	    xsbnds_rewrite_2 context il_sbnds


     | xsbnds_rewrite_1 context il_sbnds = 
	    xsbnds_rewrite_2 context il_sbnds


   and xsbnds_rewrite_2 context 
                        (il_sbnds as
			 Il.SBND(lbl, 
				 Il.BND_EXP(top_var, 
					    il_exp as Il.FIX(_, _, fbnds)))
			 ::rest_il_sbnds) =
       (if (Name.is_cluster lbl) then
	   let
	       (* external_labels = Exported labels for these functions.
                  external_vars = Variables to which the functions should be bound
                                  in the returned NIL bindings 
                  rest_il_sbnds' = remaining il_sbnds after this group of functions 
                *)
	       val num_functions = length fbnds
	       val (rest_il_sbnds', external_labels, external_vars) = 
		   getSbndNames num_functions rest_il_sbnds

       (* internal_vars = Variables to which the functions are bound
                        in the HIL fix-construct.
        il_functions = Bodies of the functions in this mutually-recursive group *)
               val (internal_renamed_vars, nil_functions) = 
		   let
		       val Let_e (_,[Fixopen_b nil_fn_set],_) = xexp context il_exp
		   in
		       Listops.unzip (Sequence.toList nil_fn_set)
		   end


	       val context' = insert_given_vars(external_vars, internal_renamed_vars, 
						context)

               val ebnd_entries = Listops.zip internal_renamed_vars nil_functions
               val ebnd_types = map (NilUtil.function_type Open) nil_functions

	       val ebnds = [Fixopen_b (Sequence.fromList ebnd_entries)]


	       val {final_context, cbnd_cat, ebnd_cat, record_c_con_items,
		    record_c_knd_items,
		    record_r_exp_items} = 
		   (xsbnds context' rest_il_sbnds') 

	   in
	       {final_context = final_context,
		cbnd_cat = cbnd_cat,
		ebnd_cat = APPEND [LIST ebnds, ebnd_cat],
		record_c_con_items   = record_c_con_items,
		record_c_knd_items = record_c_knd_items,
		record_r_exp_items = (Listops.zip external_labels 
				      (map Var_e internal_renamed_vars)) @ record_r_exp_items}
	   end
	else
	    xsbnds_rewrite_3 context il_sbnds)

     | xsbnds_rewrite_2 context
                        (il_sbnds as
			 Il.SBND(lbl, 
				Il.BND_MOD
				(top_var, true, m as 
				 Il.MOD_FUNCTOR
				 (Il.TOTAL,poly_var, il_arg_signat, 
				  Il.MOD_STRUCTURE
				  [Il.SBND(them_lbl,
					   Il.BND_EXP
					   (_, il_exp as Il.FIX(is_recur, _, fbnds)))],
				  il_body_signat)))
			 :: rest_il_sbnds) =

       if ((!do_polyrec)
	   andalso (Name.is_label_internal lbl) 
           andalso (not (Name.eq_label (lbl, IlUtil.expose_lab)))
	   andalso (Name.eq_label (them_lbl, IlUtil.them_lab))
	   andalso (not (Name.is_eq lbl))
           ) then
				  
	   let
	       val _ = clear_memo top_var
	       val _ = clear_memo poly_var

	       (* external_labels = Exported labels for these functions.
                  external_vars = Variables to which the functions should be bound
                                  in the returned NIL bindings 
                  rest_il_sbnds' = remaining il_sbnds after this group of functions 
                *)
	       val num_functions = length fbnds
	       val (rest_il_sbnds', external_labels, external_vars) = 
		   getSbndNames num_functions rest_il_sbnds

       (* internal_vars = Variables to which the functions are bound
                        in the HIL fix-construct.
        il_functions = Bodies of the functions in this mutually-recursive group *)

	       val ((poly_var_c, poly_var_r), context') = newSplitVar (poly_var, context)
	       val (knd_arg, con_arg) = 
		                  xsig context' (Var_c poly_var_c, il_arg_signat)

	       val context' = update_NILctx_insert_kind(context', poly_var_c, 
							knd_arg)

	       val Let_e (_, [Fixopen_b set], _) = xexp context' il_exp

               val (internal_vars, functions) = Listops.unzip (Sequence.toList set)

               val inner_vars = map 
		   (fn v => Name.fresh_named_var((Name.var2name v) ^ "_inner")) external_vars


               val (external_var_rs, context) =
		   let
		       fun folder (v,context) = 
			   let
			       val ((_,v_r),context) = newSplitVar(v,context)
			   in
			       (v_r, context)
			   end
		   in
		       Listops.foldl_acc folder context external_vars
		   end

	       fun wrap(current_internal_var, inner_var,e) = 
		   let 
		       fun mapper(internal_var,external_var_r) = 
			   if (Name.eq_var(internal_var,current_internal_var))
			       then Exp_b(internal_var, TraceUnknown, Var_e inner_var)
			   else Exp_b(internal_var, TraceUnknown, NilUtil.makeAppE 
				      (Var_e external_var_r)
				      [Var_c poly_var_c]
				      [Var_e poly_var_r]
				      [])
		       val bnds = Listops.map2 mapper (internal_vars, external_var_rs)
		   in  NilUtil.makeLetE Sequential bnds e
		   end

               fun reviseFunction (internal_var,
				   external_var_r, inner_var,
				   Function{effect,recursive,isDependent,
					    tFormals = [],
					    eFormals = [(arg_var, arg_tr, arg_con)],
					    fFormals = [],
					    body,
					    body_type = inner_body_type}) =
		   let val body' = wrap(internal_var, inner_var, body)
		       val outer_body_type = AllArrow_c{openness = Open, effect = effect, 
							isDependent = false,
							tFormals = [], 
							eFormals = [(NONE,arg_con)], 
							fFormals = 0w0, 
							body_type = inner_body_type}
		   in  (external_var_r,
		       Function{effect = Total,
				recursive = Leaf, 
				isDependent = false,
				tFormals = [(poly_var_c, knd_arg)],
				eFormals = [(poly_var_r, TraceUnknown, con_arg)],
				fFormals = [],
				body = Let_e (Sequential,
				       [Fixopen_b
					(Sequence.fromList 
					 [(inner_var, Function{effect=effect,recursive=recursive,isDependent=false,
							       tFormals = [],
							       eFormals = [(arg_var,arg_tr,arg_con)],
							       fFormals = [],
							       body = body',
							       body_type = inner_body_type})])],
				       Var_e inner_var),
				body_type = outer_body_type})
		   end

               val ebnd_entries = (Listops.map4 reviseFunction 
				   (internal_vars, external_var_rs, inner_vars, functions))
               val ebnd_types = map (NilUtil.function_type Open) functions


	       val ebnds = [Fixopen_b (Sequence.fromList ebnd_entries)]

               val context = update_polyfuns_list(context, external_var_rs)

	       val {final_context, cbnd_cat, ebnd_cat, record_c_con_items,
		    record_c_knd_items, 
		    record_r_exp_items} = xsbnds context rest_il_sbnds'
		    
	   in
	       {final_context = final_context,
		cbnd_cat = cbnd_cat,
		ebnd_cat = APPEND [LIST ebnds, ebnd_cat],
		record_c_con_items = record_c_con_items,
  	        record_c_knd_items =  record_c_knd_items,
		record_r_exp_items = (Listops.zip external_labels (map Var_e external_var_rs))
		                     @ record_r_exp_items}
	   end
       else
	   xsbnds_rewrite_3 context il_sbnds

     | xsbnds_rewrite_2 context il_sbnds = xsbnds_rewrite_3 context il_sbnds

   and xsbnds_rewrite_3 context (Il.SBND(lbl, Il.BND_EXP(var, il_exp)) :: rest_il_sbnds) =
       let
	   val exp = xexp context il_exp

	   val (var', context') = insert_rename_var (var, context)

	   val {final_context, cbnd_cat, ebnd_cat, record_c_con_items,
		record_c_knd_items, 
		record_r_exp_items} = xsbnds context' rest_il_sbnds
       in
	   {final_context = final_context,
	    cbnd_cat = cbnd_cat,
	    ebnd_cat = APPEND[LIST [Exp_b(var',TraceUnknown, exp)], ebnd_cat],
	    record_c_con_items = record_c_con_items,
	    record_c_knd_items = record_c_knd_items,
	    record_r_exp_items = (lbl, Var_e var') :: record_r_exp_items}
       end

     | xsbnds_rewrite_3 context (Il.SBND(lbl, Il.BND_CON(var, il_con)) :: rest_il_sbnds) =
       let

	   val con = xcon context il_con
	     
	   val (var',context') = insert_rename_var (var, context)

           val context'' = update_NILctx_insert_kind_equation(context', var', con) 

	   val {final_context, cbnd_cat, ebnd_cat, record_c_con_items,
		record_c_knd_items,
		record_r_exp_items} = xsbnds context'' rest_il_sbnds
       in
	   {final_context = final_context,
	    cbnd_cat = CONS (Con_cb(var', con), cbnd_cat),
	    ebnd_cat = ebnd_cat,
	    record_c_con_items = (lbl, Var_c var') :: record_c_con_items,
	    record_c_knd_items = ((lbl, var), Single_k con) :: record_c_knd_items, 
	    record_r_exp_items = record_r_exp_items}
       end

     | xsbnds_rewrite_3 context (Il.SBND(lbl, Il.BND_MOD(var, false, il_module))::rest_il_sbnds) =
       let

	   val _ = clear_memo var

           (* Unfortunately, the HIL may duplicate variables, and the flattening
              of modules may put duplicates that used to have disjoint scopes
              into overlapping scopes. *)

	   val (var,rest_il_sbnds) = 
	       (case lookupVmap(var,context) of
		    NONE => (var,rest_il_sbnds)
		  | SOME _ => 
			let val _ = (print ("WARNING (xsbnds/BND_MOD):  " ^
					    "Duplicate variable found:");
				     Ppnil.pp_var var;
				     print "\n")
			    val v = Name.derived_var var
			    val subst = IlUtil.subst_add_modvar(IlUtil.empty_subst, var, Il.MOD_VAR v)
			    val Il.MOD_STRUCTURE rest' = 
				IlUtil.mod_subst(Il.MOD_STRUCTURE rest_il_sbnds,subst)
			in (v,rest')
			end)


	   val ((var_c, var_r), context) = newSplitVar (var, context)
	       
	   val {cbnd_cat = cbnd_mod_cat, 
		ebnd_cat = ebnd_mod_cat,
(*		knd_c = knd_mod_c, *)
		context = context,
		name_c, name_r,
		...} = xmod context (il_module, SOME(var, var_c, var_r))

	   val context = (case (extractPathLabels il_module) of
			      (Il.MOD_VAR tovar,labs) => add_modvar_alias(context,var,(tovar,labs))
			    | _ => context)

	       
	   val {final_context, cbnd_cat, ebnd_cat, record_c_con_items,
		record_c_knd_items, 
		record_r_exp_items} = xsbnds context rest_il_sbnds

       in
	   {final_context = final_context,
	    cbnd_cat = APPEND[cbnd_mod_cat, cbnd_cat],
	    ebnd_cat = APPEND[ebnd_mod_cat, ebnd_cat],
	    record_c_con_items = (lbl, name_c) :: record_c_con_items,
	    record_c_knd_items = ((lbl, var_c), Single_k(name_c)) :: record_c_knd_items, 
	    record_r_exp_items = (lbl,name_r) :: record_r_exp_items}
       end

     | xsbnds_rewrite_3 context (Il.SBND(lbl, Il.BND_MOD(var, true, il_polymod))::rest_il_sbnds) =
       let
           val ((_, var_r), context) = newSplitVar(var, context)
           val bnd = xpolymod context (var_r, il_polymod)

           val context = update_polyfuns (context, var_r)
           
	   val {final_context, cbnd_cat, ebnd_cat, record_c_con_items,
		record_c_knd_items, 
		record_r_exp_items} = xsbnds context rest_il_sbnds
       in
	   {final_context = final_context,
	    cbnd_cat = cbnd_cat,
	    ebnd_cat = APPEND[LIST [bnd], ebnd_cat],
	    record_c_con_items = record_c_con_items,
	    record_c_knd_items = record_c_knd_items,
	    record_r_exp_items = (lbl, Var_e var_r) :: record_r_exp_items}
       end

   and xpolymod context (v_r,Il.MOD_FUNCTOR(arrow,poly_var, il_arg_signat, il_body,
			    il_result_sig as Il.SIGNAT_STRUCTURE _)) =
         let
	     val _ = clear_memo poly_var
(*	     val _ = (print "xpolymod binding for "; Ppil.pp_var v_r; print "\n") *)
	     val ((poly_var_c, poly_var_r), context') = newSplitVar (poly_var, context)

	     val (knd_arg, arg_type) = 
		 xsig context' (Var_c poly_var_c, il_arg_signat)
             val effect = xeffect arrow

	     val context' = update_NILctx_insert_kind(context', poly_var_c, knd_arg) 

             val il_body_structure =
		 (case il_body of
		      Il.MOD_STRUCTURE _ => il_body
		    | _ => Il.MOD_STRUCTURE
			     [Il.SBND(IlUtil.it_lab, 
				      Il.BND_EXP(Name.fresh_var(), 
						 Il.MODULE_PROJECT(il_body, IlUtil.it_lab)))])

	     val {ebnd_cat = ebnd_cat, name_r = name_r,...} = 
		 xmod context' (il_body_structure, NONE)

             val exp = NilUtil.makeLetE Sequential (flattenCatlist ebnd_cat) name_r

             val (_,con) = xsig context' (Var_c (Name.fresh_var()),il_result_sig)

	 in
	     Fixopen_b (Sequence.fromList
			[(v_r,
			  Function{effect=effect, recursive=Leaf, isDependent = false,
				   tFormals = [(poly_var_c, knd_arg)],
				   eFormals = [(poly_var_r, TraceUnknown, arg_type)],
				   fFormals = [],
				   body = exp,
				   body_type = con})])
	 end
     | xpolymod context (v_r, il_mod) =
	 (case extractPathLabels il_mod of
	      (Il.MOD_VAR v', lbls) => 
		  let
		      val ((_,v'_r),_) = splitVar(v',context)
		      val _ = NilContext_use_var(context,v'_r)
		  in 
		      Exp_b(v_r, TraceUnknown, selectFromRec(Var_e v'_r, lbls))
		  end
            | _ => (print "xpolymod: bad module argument\n";
                    Ppil.pp_mod il_mod;
                    error "xpolymod: bad module argument"))

   and xflexinfo context (ref (Il.INDIRECT_FLEXINFO f)) = 
       xflexinfo context f
     | xflexinfo context (ref (Il.FLEXINFO(_,true, recs))) = 
       let
	   val (lbls, cons) = xrdecs context recs
	   val con = Prim_c(Record_c(lbls,NONE), cons) (* already sorted *)
       in
	   con
       end

   and xrdecs context [] = ([], [])
     | xrdecs context ((lab, il_con) :: rest) = 
       let
	   val (labs, cons) = xrdecs context rest
	   val con = xcon context il_con
       in
	   (lab :: labs, con :: cons)
       end

   (* Returns the translated NIL con and a singleton-less (thick) kind *)
   and xcon context il_con : con  =
       let
	   val this_call = ! xcon_count
	   val _ = 
	       if (!debug) then
		   (xcon_count := this_call + 1;
		    print ("Call " ^ (Int.toString this_call) ^ " to xcon\n");
		    if (!full_debug) then (Ppil.pp_con il_con; print"\n") else ())
	       else ()

	   fun check_proj(Il.MOD_VAR v,ls) = 
	       let
		   val ((v_c,_),_) = splitVar(v, context)
	       in
		   NilContext_use_var(context, v_c);
		   lookup_con_memo (v,ls) (fn()=> xcon' context il_con)
	       end
	     | check_proj(Il.MOD_PROJECT(m,l),ls) = check_proj(m,l::ls)
	     | check_proj _ = xcon' context il_con
	   val result = (case (!do_memoize,il_con) of
			     (true,Il.CON_MODULE_PROJECT(m,l)) => check_proj(m,[l])
			   | _ => xcon' context il_con)
	       handle e => (if (!debug) then (print ("Exception detected in call " ^ 
						    (Int.toString this_call) ^ " to xcon: ");
					      Ppil.pp_con il_con; print "\n") else ();
			    raise e)

	in
	    if (!debug) then print ("Return " ^ (Int.toString this_call) ^ " from xcon\n") else ();
	    result
        end

   and xcon' context (il_con as (Il.CON_VAR var)) : con (* * kind *) = 
       let
	   val var' = rename_var(var, context)
           val _ = NilContext_use_var (context, var')
	   val con = Var_c var'
       in
	   con
       end

     | xcon' context (Il.CON_TYVAR tv) = xcon context (derefTyvar tv)

     | xcon' context (Il.CON_OVAR ov) = xcon context (derefOvar ov)

     | xcon' context (Il.CON_FLEXRECORD fr) = xflexinfo context fr

     | xcon' context (Il.CON_INT Prim.W64) =
       error "64-bit integers not handled during/after phase-split"

     | xcon' context (Il.CON_UINT Prim.W64) =
       error "64-bit integers not handled during/after phase-split"

     | xcon' context (Il.CON_INT intsize) =
       let
           val con = Prim_c (Int_c intsize, [])
       in
	   con
       end

    (* there is no type distinction between signed/unsigned ints from NIL onwards *)
     | xcon' context (Il.CON_UINT intsize) = xcon' context (Il.CON_INT intsize)

     | xcon' context (Il.CON_FLOAT floatsize) = 
       let
	   val con = Prim_c (BoxFloat_c floatsize, [])
       in
	   con
       end

     | xcon' context (Il.CON_ARRAY il_con) = 
       let
	   val con' = xcon context il_con 
	   val con = Prim_c (Array_c, [con'])
       in  con
       end

     | xcon' context (Il.CON_VECTOR il_con) = 
       let
	   val con' = xcon context il_con 
	   val con = Prim_c (Vector_c, [con'])
       in  con
       end

     | xcon' context (Il.CON_ANY) = 
       let
	   val con = Prim_c(Exn_c, [])
       in
	   con
       end

     | xcon' context (Il.CON_REF il_con) = 
       let
	   val con' = xcon context il_con
	   val con = Prim_c (Array_c, [con'])
       in
	   con
       end

     | xcon' context (Il.CON_TAG il_con) = 
       let
	   val con' = xcon context il_con
	   val con = Prim_c (Exntag_c, [con'])
       in  con
       end

     | xcon' context (Il.CON_ARROW (il_cons1, il_con2, closed, arr)) =
       let
           fun translate il_con = (case (closed,il_con) of
				       (true,Il.CON_FLOAT Prim.F64) => Prim_c (Float_c Prim.F64, [])
				     | _ => xcon context il_con)
	   val cons1 = map translate il_cons1
           val con2 = translate il_con2
	   val eff = xeffect (derefOneshot arr)
	   val con = if closed
			 then ExternArrow_c(cons1, con2)
		     else AllArrow_c{openness = Open, effect = eff, isDependent = false,
				     tFormals = [], 
				     eFormals = map (fn c => (NONE,c)) cons1,
				     fFormals = 0w0, body_type = con2}
       in  con
       end

     | xcon' context (il_con as Il.CON_APP (il_con1, il_cons2)) = 
       let
           (* XXX Does not handle the case where il_con1 has a
              dependent kind.  Fortunately, the CON_FUN case of
              xcon' always returns a non-dependent kind. *)
	   val con1 = xcon context il_con1
           val cons2 = map (xcon context) il_cons2
	   val con = App_c(con1, cons2)
       in  con
       end

     | xcon' context (Il.CON_MU(Il.CON_FUN(vars, 
					   Il.CON_TUPLE_INJECT cons))) =
       let
	   val (vars',context') = insert_rename_vars(vars, context)
	   val context'' = update_NILctx_insert_kind_list(context',map (fn v => (v,Type_k)) vars')
	       
	   val cons'= map (xcon context'') cons
	   val freevars = foldl Name.VarSet.union Name.VarSet.empty
	                  (map IlUtil.con_free cons)
	   val is_recur = Listops.orfold (fn v => Name.VarSet.member(freevars,v)) vars
	   val con = Mu_c (is_recur,
			   Sequence.fromList (Listops.zip vars' cons'))

       in
	   con
       end

     | xcon' context (Il.CON_MU(Il.CON_FUN([var], con))) =
       let
	   val (var',context') = insert_rename_var(var, context)
	   val context''= update_NILctx_insert_kind(context', var', Type_k)
	       
	   val con' = xcon context'' con
	   val freevars = IlUtil.con_free con
	   val is_recur = Name.VarSet.member(freevars,var)
	   val con = Mu_c (is_recur,Sequence.fromList [(var', con')])
       in
	   con
       end

     | xcon' context (Il.CON_RECORD rdecs) = 
       let
	   val (lbls, cons) = xrdecs context rdecs
	   val con = Prim_c (Record_c(lbls,NONE), cons)
       in
	   con
       end


     | xcon' context (Il.CON_FUN (vars, il_con1)) = 
       let
	   val (vars', context') = insert_rename_vars(vars, context)
           val args = map (fn v => (v,Type_k)) vars'

	   val context'' = update_NILctx_insert_kind_list(context',args)

	   val con1 = xcon context'' il_con1

	   val fun_name = Name.fresh_var ()
	   val con = NilUtil.makeLetC [Open_cb(fun_name, args, con1)]
			       (Var_c fun_name)	
       in  con
       end

     | xcon' context (Il.CON_SUM {names, carrier, noncarriers, special}) =
       let
	   val known = (case special of
			       NONE => NONE
			     | SOME i => SOME (Word32.fromInt i))
	   val carrier_con = xcon' context carrier
	   val num_carriers = (case NilContext_kind_of(context, carrier_con) of
				   (Record_k seq) => length(Sequence.toList seq)
				 | Type_k => 1
                                 | SingleType_k _ => 1
				 | _ => error "CON_SUM: cannot have non-record and non-word kind")
	   val con = Prim_c (Sum_c {tagcount = Word32.fromInt noncarriers,
				    totalcount = Word32.fromInt(noncarriers + num_carriers),
				    known = known}, [carrier_con])
       in
	   con
       end
     | xcon' context (Il.CON_COERCION (vars,il_from_con,il_to_con)) =
(******************** Replacing this with a Nil coercion type
       let
	 val (vars', context) = insert_rename_vars(vars, context)
	 val tformals = map (fn v => (v,Type_k)) vars'
	 val context = update_NILctx_insert_kind_list(context,tformals)
	 val from_con = xcon context il_from_con
	 val to_con = xcon context il_to_con
	 val arrow = AllArrow_c {openness    = Open,
				 effect      = Total,
				 isDependent = false,
				 tFormals    = tformals,
				 eFormals    = [(NONE,from_con)],
				 fFormals    = 0w0,
				 body_type   = to_con}
       in arrow
       end
**************************************************************)
       let
	 val (vars', context) = insert_rename_vars(vars, context)
	 val tformals = map (fn v => (v,Type_k)) vars'
	 val context = update_NILctx_insert_kind_list(context,tformals)
	 val from_con = xcon context il_from_con
	 val to_con = xcon context il_to_con
       in Coercion_c {vars=vars',from=from_con,to=to_con}
       end	 

     | xcon' context (il_con as (Il.CON_TUPLE_INJECT il_cons)) = 
       let
	   val cons = map (xcon context) il_cons
	   val tuple_length = List.length cons
	   val labels = makeLabels tuple_length
	   val vars = makeVars tuple_length
	   val con = Crecord_c(Listops.zip labels cons)
       in
	   con
       end

     | xcon' context (il_con as (Il.CON_TUPLE_PROJECT (i, il_con1))) = 
       let
	   val con1 = xcon context il_con1
	   val lbl = IlUtil.generate_tuple_label(i+1)
	   val con = Proj_c(con1, lbl)
       in con
       end

     | xcon' context (il_con as (Il.CON_MODULE_PROJECT (modv, lbl))) = 
       let
	   val {cbnd_cat,name_c,(*knd_c,*)context,...} = 
	       xmod context (modv, NONE)
           val cbnd_list = flattenCatlist cbnd_cat

	   val proj_con = Proj_c (name_c, lbl)
	   val con = NilUtil.makeLetC cbnd_list proj_con

       in
	   con
       end
    
   and toFunction context (exp as Il.FIX _) =
       let
	   val Let_e (_, [Fixopen_b fns], Var_e var) = xexp context exp
       in
	   case	(Sequence.lookup (Name.eq_var) fns var) of
	       SOME (Function{tFormals=[],isDependent=false,
			      eFormals=[(v,_,c)],fFormals=[],body,...}) => (v,c,body)
	     | NONE => error "(toFunction): impossible"
       end
     | toFunction _ e = 
       (Ppil.pp_exp e;
	error "(toFunction): not a FIX expression")

   and xvalue context (Prim.int (intsize, w)) = Const_e (Prim.int (intsize, w))

     | xvalue context (Prim.uint (intsize, w)) = Const_e (Prim.uint (intsize, w))

     | xvalue context (Prim.float (floatsize, f)) = 
        Prim_e (NilPrimOp (box_float floatsize),
		[], [Const_e (Prim.float (floatsize, f))])

     | xvalue context (Prim.array (il_con, a)) = 
       let
	   val il_exps = Array.foldr (op ::) nil a
           val con = xcon context il_con
           val exps = map (xexp context) il_exps
       in  Const_e (Prim.array (con, Array.fromList exps))
       end

     | xvalue context (Prim.vector (il_con, v)) = 
       let
	   val il_exps = Array.foldr (op ::) nil v
           val con = xcon context il_con
	   val exps = map (xexp context) il_exps
       in
	   Const_e (Prim.vector (con, Array.fromList exps))
       end

     | xvalue context (Prim.refcell (ref il_exp)) = 
       let
	   val exp = xexp context il_exp
       in
	   (* BUG *)
	   (* SHOULD PRESERVE EQUIVALENCE OF REF VALUES BUT DOESN'T !!! *)
	   Const_e (Prim.refcell (ref exp))
       end

     | xvalue context (Prim.tag(tag, il_con))  =
       let
	   val con = xcon context il_con
       in
	   Const_e (Prim.tag (tag, con))
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
					      print "\nwith exp = \n";
					      Ppil.pp_exp il_exp;
(*
					      print "\nwith context = \n";
					      print_splitting_context context;
*)
					      print "\n")
			    else ();
			    raise e)

	in
	    if (!debug) then print ("Return " ^ (Int.toString this_call) ^ " from xexp\n") else ();
	    result
        end


   and xexp' context (Il.OVEREXP(_, _, exp_oneshot)) = 
       xexp context (derefOneshot exp_oneshot)

     | xexp' context (Il.SCON il_scon) = xvalue context il_scon

     | xexp' context (Il.ETAPRIM (prim, il_cons)) = 
       xexp context (IlUtil.prim_etaexpand(get_hilctxt context,prim,il_cons))

     | xexp' context (Il.ETAILPRIM (ilprim, il_cons)) = 
       xexp context (IlUtil.ilprim_etaexpand(get_hilctxt context,ilprim,il_cons))

     | xexp' context (il_exp as (Il.PRIM (prim, il_cons, il_args))) = 
       let
	   open Prim
	   val cons = map (xcon context) il_cons
	   val args = map (xexp context) il_args
           val (effect,con) = 
	     case strip_arrow (NilPrimUtil.get_type' (get_nilctxt context)  prim cons) of
		 SOME {effect,body_type,...} => (effect,body_type)
		| _ => (perr_c (NilPrimUtil.get_type' (get_nilctxt context) prim cons);
			error "Expected arrow constructor")

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
		     neg_float fs  => float_float fs
		   | (abs_float fs)  => float_float fs
		   | (plus_float fs)  => float_float fs
		   | (minus_float fs)  => float_float fs
		   | (mul_float fs)  => float_float fs
		   | (div_float fs) => float_float fs
		   | (less_float fs)   => float_int fs
		   | (greater_float fs)  => float_int fs
		   | (lesseq_float fs) => float_int fs
		   | (greatereq_float fs)  => float_int fs
		   | (eq_float fs)  => float_int fs
		   | (neq_float fs) => float_int fs
		   | float2int => float_int F64
		   | int2float => int_float F64
		   | _ => (args,id))
       in  wrap(Prim_e (PrimOp prim, cons, args))
	   
       end
     | xexp' context (il_exp as (Il.ILPRIM (ilprim, il_cons, il_args))) = 
       let
	   val cons = map (xcon context) il_cons
	   val args = map (xexp context) il_args
	   val zero = Const_e (Prim.int (Prim.W32, TilWord64.fromInt 0))
	   val one = Const_e (Prim.int (Prim.W32, TilWord64.fromInt 1))
	   val t = (Prim.OtherArray false)
       in  case ilprim of
	     Prim.mk_ref => Prim_e(PrimOp(Prim.create_table t),
				   cons, one::args)
	   | Prim.deref => Prim_e(PrimOp(Prim.sub t),
				  cons, args @ [zero])
	   | Prim.setref => Prim_e(PrimOp(Prim.update t),
				  cons, case args of
				          [a,b] => [a,zero,b]
					| _ => error "bad set_ref")
	   | Prim.eq_ref => Prim_e(PrimOp(Prim.equal_table t),
				   cons, args)
	   | _ => Prim_e (PrimOp (xilprim ilprim), cons, args)
       end

     | xexp' context (Il.VAR var) = 
       let
	   val var' = rename_var(var, context)
       in
	   NilContext_use_var(context,var');
	   Var_e var'
       end

     | xexp' context (il_exp as (Il.EXTERN_APP (il_con1,il_exp1, il_exps2))) =
       let
	   val exp1 = xexp context il_exp1
	   val exps2 = map (xexp context) il_exps2
	   val Il.CON_ARROW(cons2,res_con,_,_) = il_con1
	   fun mapper(e,Il.CON_FLOAT _) = Prim_e (NilPrimOp (unbox_float Prim.F64),[],[e])
	     | mapper(e,_) = e
	   val exps2 = Listops.map2 mapper (exps2,cons2)
	   val app = ExternApp_e (exp1, exps2)
       in  (case res_con of
	     Il.CON_FLOAT _ => Prim_e (NilPrimOp (box_float Prim.F64),[],[app])
	   | _ => app)
       end

           
     | xexp' context (il_exp as (Il.APP (il_exp1, il_exp2))) = 
         (case IlUtil.exp_reduce (get_hilctxt context,il_exp) of
	      NONE => 
		  let
		      val exp1 = xexp context il_exp1
		      val exp2 = xexp context il_exp2
		  in  App_e (Open, exp1, [], [exp2], [])
		  end	   
	    | SOME il_exp => xexp' context il_exp)

     | xexp' context (Il.FIX (is_recur, il_arrow, fbnds)) = 
       let
	   val fbnds'= xfbnds context (is_recur, il_arrow, fbnds)
           val set = Sequence.fromList fbnds'
           val names = map (fn (var,_) => Var_e var) fbnds'
           val num_names = List.length names
           val labels = makeLabels num_names
       in
	   if (num_names = 1) then
               NilUtil.makeLetE Sequential [Fixopen_b set] (hd names)
           else
	       NilUtil.makeLetE Sequential [Fixopen_b set]
		         (Prim_e(NilPrimOp (record labels), [], names))
       end

     | xexp' context (Il.RECORD rbnds) = 
       let
	   val (labels,il_exps) = Listops.unzip rbnds
	   val exps = map (xexp context) il_exps
       in  Prim_e (NilPrimOp (record labels), [], exps)
       end

     | xexp' context (Il.RECORD_PROJECT (il_exp, label, il_record_con)) =
       let
	   val exp_record = xexp context il_exp
       in
	   Prim_e (NilPrimOp (select label), [], [exp_record])
       end

     | xexp' context (Il.SUM_TAIL (i,il_con, il_exp)) =
       let
	   val exp = xexp context il_exp
	   val sumcon = xcon context il_con
       in  Prim_e (NilPrimOp (project (TilWord32.fromInt i)), [sumcon], [exp])
       end
(*
	       handle e => (print "SUM_TAIL error\n";
			    print "tagcount = "; print (w32tos tagcount); print "\n";
			    print "i = "; print (w32tos i); print "\n";
			    print "which = "; print (Int.toString which); print "\n";
			    print "length cons = "; print (Int.toString(length cons)); 
			    print "\ncon = "; Ppnil.pp_con con; print "\n";
			    raise e)
*)

     | xexp' context (Il.HANDLE (il_con, il_exp1, il_exp2)) = 
       let
	   val body = xexp context il_exp1
	   val result_type = xcon context il_con
	   val (bound, _, handler) = toFunction context il_exp2
       in
	   Handle_e {body = body, bound = bound,
		     handler = handler, result_type = result_type}
       end

     | xexp' context (Il.RAISE (il_con, il_exp)) = 
       let
	   val exp = xexp context il_exp
	   val con = xcon context il_con
       in  Raise_e (exp, con)
       end

     | xexp' context (Il.LET (bnds, il_exp)) = 
       let
	   val {cbnd_cat, ebnd_cat, final_context=context'} = xbnds context bnds
           val cbnds = flattenCatlist cbnd_cat
           val ebnds = (map makeConb cbnds) @ (flattenCatlist ebnd_cat)
	   val exp = xexp context' il_exp
       in  NilUtil.makeLetE Sequential ebnds exp
       end

     | xexp' context (Il.NEW_STAMP il_con) = 
       let val con = xcon context il_con
       in  Prim_e(NilPrimOp make_exntag, [con], [])
       end

     | xexp' context (Il.EXN_INJECT (s, il_tag, il_exp)) =
       let
	   val tag = xexp context il_tag
	   val exp = xexp context il_exp
       in
           Prim_e (NilPrimOp (inj_exn s), [], [tag, exp])
       end

     | xexp' context (Il.COERCE(il_coercion,il_cons,il_exp)) = 
       let
	 val coercion = xexp context il_coercion
	 val cons = map (xcon context) il_cons
	 val exp = xexp context il_exp
(*********** Replacing this with a Nil coercion application
       in App_e(Open,coercion,cons,[exp],[])
       end
***********************************************************)
       in Coerce_e(coercion,cons,exp)
       end

     | xexp' context (Il.FOLD (vars, il_expanded_con, il_mu_con)) = 
(*********** Replacing this with a Nil coercion value ******
       let

	   val (vars',context) = insert_rename_vars(vars, context)

	   val tformals = map (fn v => (v,Type_k)) vars'

	   val context = update_NILctx_insert_kind_list(context,tformals)

	   val expanded_con = xcon context il_expanded_con
	   val mu_con = xcon context il_mu_con

	   val fun_name = Name.fresh_named_var "fold"
	   val arg_name = Name.fresh_named_var "fold_arg"
	   val body = Prim_e(NilPrimOp roll, [mu_con], [Var_e arg_name])
	   val lambda = Function {effect = Total,
				  recursive = Leaf,
				  isDependent = false,
				  tFormals = tformals,
				  eFormals = [(arg_name,TraceUnknown,expanded_con)],
				  fFormals = [],
				  body = body,
				  body_type = mu_con}
	   val exp = Let_e (Sequential,[Fixopen_b (Sequence.fromList [(fun_name,lambda)])],Var_e fun_name)
       in exp
       end
***********************************************************)
       let 
	 val (vars',context) = insert_rename_vars(vars, context)
	 val tformals = map (fn v => (v,Type_k)) vars'
	 val context = update_NILctx_insert_kind_list(context,tformals)
	 val expanded_con = xcon context il_expanded_con
	 val mu_con = xcon context il_mu_con
	 val exp = Fold_e(vars',expanded_con,mu_con)
       in exp
       end
	 
  | xexp' context (Il.UNFOLD (vars, il_mu_con, il_expanded_con)) = 
(*********** Replacing this with a Nil coercion value ******
       let

	   val (vars',context) = insert_rename_vars(vars, context)

	   val tformals = map (fn v => (v,Type_k)) vars'

	   val context = update_NILctx_insert_kind_list(context,tformals)

	   val expanded_con = xcon context il_expanded_con
	   val mu_con = xcon context il_mu_con

	   val fun_name = Name.fresh_named_var "unfold"
	   val arg_name = Name.fresh_named_var "unfold_arg"
	   val body = Prim_e(NilPrimOp unroll, [mu_con], [Var_e arg_name])
	   val lambda = Function {effect = Total,
				  recursive = Leaf,
				  isDependent = false,
				  tFormals = tformals,
				  eFormals = [(arg_name,TraceUnknown,mu_con)],
				  fFormals = [],
				  body = body,
				  body_type = expanded_con}
	   val exp = Let_e (Sequential,[Fixopen_b (Sequence.fromList [(fun_name,lambda)])],Var_e fun_name)
       in exp
       end
***********************************************************)
       let 
	 val (vars',context) = insert_rename_vars(vars, context)
	 val tformals = map (fn v => (v,Type_k)) vars'
	 val context = update_NILctx_insert_kind_list(context,tformals)
	 val expanded_con = xcon context il_expanded_con
	 val mu_con = xcon context il_mu_con
	 val exp = Unfold_e(vars',mu_con,expanded_con)
       in exp
       end

     | xexp' context (Il.ROLL (il_con, il_exp)) = 
(*********** Replacing this with a Nil coercion application
       let
	   val con = xcon context il_con
	   val exp = xexp context il_exp
       in
	   Prim_e(NilPrimOp roll, [con], [exp])
       end
***********************************************************)
       let
	   val to_con = xcon context il_con
	   val nilctx = get_nilctxt context
	   val from_con = Normalize.expandMuType (nilctx,xcon context il_con)
	   val exp = xexp context il_exp
       in Coerce_e(Fold_e([],from_con,to_con),[],exp)
       end
	   

     | xexp' context (il_exp as (Il.UNROLL (il_mu_con, il_expanded_con, il_exp1))) = 
(*********** Replacing this with a Nil coercion application
       let
	   val mu_con = xcon context il_mu_con
	   val exp = xexp context il_exp1
       in  Prim_e(NilPrimOp unroll, [mu_con], [exp])
       end
***********************************************************)
       let
	 val from_con = xcon context il_mu_con
	 val to_con = xcon context il_expanded_con
	 val exp = xexp context il_exp1
       in Coerce_e(Unfold_e([],from_con,to_con),[],exp)
       end

     | xexp' context (Il.INJ {sumtype, field, inject = eopt}) =
       let
	   val sumcon = xcon context sumtype
	   val elist = (case eopt of
			    NONE => []
			  | SOME il_exp => [xexp context il_exp])
       in
	   Prim_e(NilPrimOp (inject (TilWord32.fromInt field)),[sumcon],elist)
       end


     | xexp' context (Il.CASE {sumtype, arg=il_arg, arms=il_arms, bound,
			       tipe,default=il_default}) =
       let
	   (* We want to use the result type given to avoid type blowup *)
	   val result_type = xcon context tipe
	   val sumcon = xcon context sumtype
	   val exp = xexp context il_arg
	   val (bound', context') = insert_rename_var(bound, context)
	   fun xarm (n, NONE ) = NONE
	     | xarm (n, SOME ilexp) = SOME(Word32.fromInt n, TraceUnknown, xexp context' ilexp)
	   val arms = List.mapPartial (fn x => x) (mapcount xarm il_arms)
	   val default = Util.mapopt (xexp context') il_default
       
	in Switch_e(Sumsw_e {sumtype = sumcon,
			     bound = bound',
			     arg  = exp, arms = arms, 
			     default = default,
			     result_type = result_type})
       end

     | xexp' context (e as Il.EXN_CASE {arg = il_exp, arms = il_arms, default = il_default, tipe}) =
       let
	   val exp = xexp context il_exp
	   val result_type = xcon context tipe
	   val (bounds, tags, bodies) = 
		Listops.unzip3
		  (map (fn (tag,_,Il.FIX(false,_,[Il.FBND(_,var,_,_,e)])) => (var,tag,e)
			| (_,_,il_arm) => (print "EXN_CASE MATCHn"; Ppil.pp_exp il_arm; raise Match)) il_arms)
	   val (bound :: rest) = bounds
	   val _ = if (List.all (fn v => Name.eq_var(v,bound)) rest)
		       then () else error "exn_case did not get same var in all arms"

	   val (bound', context') = insert_rename_var (bound, context)

	   val arms' = 
	       Listops.map2 (fn (tag,body) => (xexp context' tag, TraceUnknown,
					       xexp context' body))
                  (tags, bodies)

	   val default = Util.mapopt (xexp context) il_default
       in
	   Switch_e(Exncase_e {	bound = bound',
				arg = exp, arms = arms',
				default = default,
				result_type = result_type})
       end

     | xexp' context (Il.MODULE_PROJECT (il_module, label)) =
       let

           val is_it_proj = Name.eq_label(label, IlUtil.it_lab)

	   val mod_opt = 
	       (case il_module of
		    Il.MOD_APP(il_mod_fun, mod_arg) =>
			(case (extractPathLabels il_mod_fun) of
			     (Il.MOD_VAR v, lbls) => 
				 let
				     val ((_,v_r),_) = splitVar (v, context)
				 in
				     if ((!elaborator_specific_optimizations) andalso
					 ((var_is_polyfun(context, v_r)) orelse is_it_proj)) then
					 let
					     val {ebnd_cat, cbnd_cat, name_c, name_r, ...} = 
						 xmod context (mod_arg, NONE)
					     val _ = NilContext_use_var (context, v_r)
					 in 
					     SOME (NilUtil.makeLetE Sequential
						   ((map makeConb (flattenCatlist cbnd_cat)) @
						    (flattenCatlist ebnd_cat))
						   (NilUtil.makeAppE 
						    (selectFromRec (Var_e v_r,lbls)) [name_c] [name_r] []))
					 end
				     else
					 NONE
				 end
			   | _ => NONE)
		  | _ => NONE)

	   val module = 
	       (case mod_opt of 
		    SOME module => module
		  | NONE => 
			let
			    val {ebnd_cat, cbnd_cat, name_r, ...} = xmod context (il_module, NONE)
			    val cbnds = flattenCatlist cbnd_cat
			    val bnds = (map makeConb cbnds) @ (flattenCatlist ebnd_cat)
			in
			    NilUtil.makeLetE Sequential bnds name_r
			end)

       in
	   if ((!elaborator_specific_optimizations) andalso is_it_proj) then
	       module
	   else
               Prim_e (NilPrimOp (select label), [], [module])
       end

     | xexp' context (Il.SEAL (exp,_)) = xexp context exp


   and xfbnds context (is_recur, il_arrow, fbnds) = 
       let
	   val recursive = if is_recur then Arbitrary else NonRecursive
	   val totality = xeffect il_arrow
	   val fun_names = map (fn Il.FBND(v,_,_,_,_) => v) fbnds
	   val (fun_names', context') = insert_rename_vars (fun_names, context)
	   fun mapper (Il.FBND(var1, var2, il_con1, il_con2, body)) = 
	       let
		   val var1' = rename_var(var1, context')
		   val (var2',context'') = insert_rename_var (var2, context')
		   val con1 = xcon context'' il_con1
		   val con2 = xcon context'' il_con2
		   val body' = xexp context'' body
	       in  (var1', Function{recursive = recursive, effect = totality, isDependent = false,
				    tFormals = [], eFormals = [(var2', TraceUnknown, con1)], 
				    fFormals=[], body = body', body_type = con2})
	       end
       in  map mapper fbnds
       end

   handle e => (print "uncaught exception in xfbnds\n";
		raise e)


   and xbnds context bnds =
       let
	   val temporary_labels = makeInternalLabels (length bnds)
	   val sbnds = map Il.SBND (Listops.zip temporary_labels bnds)

	   val {final_context, cbnd_cat, ebnd_cat, record_c_con_items,
		record_c_knd_items, 
		record_r_exp_items} = 
		xsbnds context sbnds

       in
	   {cbnd_cat = cbnd_cat,
	    ebnd_cat = ebnd_cat,
	    final_context = final_context}
       end

   and xsig' context (con0,Il.SIGNAT_VAR v) = 
          xsig' context (con0,case find_sig(context,v) of
			        NONE => error "unbound signature variable"
			      | SOME s => s)

     |  xsig' context (con0,Il.SIGNAT_OF il_path) = 
          let val {cbnd_cat = cbnd_mod_cat, 
		   ebnd_cat = ebnd_mod_cat,
		   name_c   = name_mod_c, 
		   name_r   = name_mod_r,
		   context  = context,
(*		   knd_c = knd, *)
		   ...} = xmod context (IlUtil.path2mod il_path, NONE)

	      val cbnds = flattenCatlist cbnd_mod_cat
	      val cbnds' = map makeConb cbnds
	      val ebnds = flattenCatlist ebnd_mod_cat
	      val con_mod = NilUtil.makeLetC cbnds name_mod_c
	      val e = NilUtil.makeLetE Sequential
		      (cbnds' @ ebnds)
		      name_mod_r
	      val _ = typeof_count()
	  in  (Single_k(con_mod), Typeof_c e)
	  end

     | xsig' context (con0,Il.SIGNAT_FUNCTOR (var, sig_dom, sig_rng, arrow))=
       let

	   val _ = clear_memo var

	   val is_polyfun_sig = 
	       (case sig_rng of
		    Il.SIGNAT_STRUCTURE([Il.SDEC(it_lbl,Il.DEC_EXP _)]) => Name.eq_label(it_lbl,IlUtil.it_lab)
		  | _ => false)

	   val ((var_c, var_r), context) = newSplitVar (var, context)
	   val (knd, con) = xsig context (Var_c var_c, sig_dom)
	   val context = update_NILctx_insert_kind(context, var_c, knd)
	       
	   val (knd', con') = xsig context (App_c(con0, [Var_c var_c]), sig_rng)

           val effect = xeffect arrow
	       
       in
	   (Arrow_k (Open, [(var_c, knd)], knd'),
	    AllArrow_c {openness = Open, effect = effect, isDependent = true,
			tFormals = [(var_c, knd)],
			eFormals = [(SOME var_r, con)], 
			fFormals = 0w0, 
			body_type = con'})
       end

     | xsig' context (con0, Il.SIGNAT_STRUCTURE sdecs) = xsig_struct context (con0,sdecs)
     | xsig' context (con0, Il.SIGNAT_SELF(_, SOME unselfSig, _)) = xsig' context (con0, unselfSig)
     (* the self signature has no self-references; but rather just has no internal variable uses *)
     | xsig' context (con0, Il.SIGNAT_SELF(_, NONE, selfSig)) = xsig' context (con0, selfSig)


   and xsig_struct context (con0,sdecs) = 
       let
	   val {crdecs, erdecs} =
	       xsdecs context (con0, NilSubst.C.empty(), sdecs)
	   val kind = Record_k (Sequence.fromList crdecs)
	   val (erlabs, ervars, ercons) = Listops.unzip3 erdecs
	   val type_r = Prim_c(Record_c (erlabs,SOME ervars), ercons)
	   val type_r = (case (!elaborator_specific_optimizations,sdecs,erdecs) of
			     (true,[Il.SDEC(it_lbl,_)],[(_,_,ercon)]) =>
				 (if (Name.eq_label(it_lbl,IlUtil.it_lab))
				      then ercon
				  else type_r)
			   | _ => type_r)
       in  (kind, type_r)
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
	       handle e => (if (!debug) 
				then (print ("Exception detected in call " ^ 
					    (Int.toString this_call) ^ " to xsig:\n");
				      Ppil.pp_signat il_sig;
				      print "\n")
			    else ();
				raise e)
       in  if (!debug) then print ("Return " ^ (Int.toString this_call) ^ " from xsig\n") else ();
	    result
       end
		    
 (* Returns erdecs: term decs
            crdecs: type decs*)
   and xsdecs context (con,subst,sdecs) =
       let
	   val this_call = ! xsdecs_count
	   val _ = if (! debug) then
	            (xsdecs_count := this_call + 1;
		     print ("Call " ^ (Int.toString this_call) ^ " to xsdecs\n");
		     if (!full_debug) then (Ppil.pp_sdecs sdecs; print "\n";
(*
					    print "\nwith context = \n";
					    NilContext_print context;
*)
					    print "\n\n") else ())
                   else ()

	   val sdecs = rewrite_sdecs sdecs

	   val result = xsdecs' context (con,subst,sdecs)
	       handle e => (if (!debug) then (print ("Exception detected in call " ^ 
						    (Int.toString this_call) ^ " to xsdecs\n");
(*
					      print "\nwith context = \n";
					      print_splitting_context context;
*)
					      print "\n")
			    else ();
				raise e)
		   
       in
	   if (!debug) then print ("Return " ^ (Int.toString this_call) ^ " from xsdecs\n") else ();
	   result
       end

  and rewrite_sdecs sdecs =
       let 
	   fun filter (Il.SDEC(lab,Il.DEC_MOD(var,_,_))) = not (Name.is_dt lab)
             | filter _ = true

	   fun loop [] = []
	     | loop ((sdec as 
		     Il.SDEC(lab,Il.DEC_EXP(top_var,il_con, _, _))) :: rest) = 
	        if (Name.is_cluster lab) then
		   let
(*		       val _ = print "entered mono optimization case\n" *)
		       val clist = (case il_con of
					Il.CON_RECORD lclist => map #2 lclist
				      | Il.CON_ARROW _ => [il_con]
				      | _ => error "can't optimize mono fun")
		       val numFunctions = length clist
		       val (rest, external_labels, external_vars) = 
			   getSdecNames numFunctions rest
		       fun make_sdec (lbl,c) = Il.SDEC(lbl,Il.DEC_EXP(Name.fresh_var(),c,NONE,false))
		       val sdecs' = Listops.map2 make_sdec (external_labels,clist)
		   in  sdecs' @ (loop rest)
		   end
	       else
		   sdec::loop rest
	     | loop ((sdec as 
		     Il.SDEC(lbl,
			     Il.DEC_MOD
			     (top_var, true, s as
			      Il.SIGNAT_FUNCTOR(poly_var, il_arg_signat,
						Il.SIGNAT_STRUCTURE([Il.SDEC(them_lbl,
									    Il.DEC_EXP(_,il_con,_,_))]),
						arrow))))
		     :: rest) = 
	       if ((!do_polyrec)
	           andalso (Name.eq_label (them_lbl, IlUtil.them_lab))) then
                   (* if a polymorphic function has a "them" label rather than 
                      an "it" label, then it is a polymorphic function nest whose
                      code (i.e., this entire component) will be eliminated by the
                      phase-splitter.  Therefore, the corresponding specification
                      also is ignored. 

                      Note that the phase-splitter does some complicated transformations
                      to the projections from such a nest, in order to turn polymorphic
                      recursively-defined-functions into polymorphic recursion; however,
                      the types of the projections are unchanged so we just continue
                      here without doing anything special.
                   *)
		   loop rest
	       else
		   sdec :: (loop rest)
	     | loop (sdec::rest) = sdec::(loop rest)
	   val sdecs = if !elaborator_specific_optimizations
			   then List.filter filter sdecs
		       else sdecs
       in  loop sdecs
       end
   
   and xsdecs' context (con0, _, []) = {crdecs = nil, erdecs = nil}

     | xsdecs' context (con0, subst,  
		    Il.SDEC(lbl, d as Il.DEC_MOD(var,is_poly,signat)) :: rest) =
       let
	   val _ = clear_memo var
	   val ((var_c, var_r), context') = newSplitVar (var, context)
	   val (knd, con) = xsig context' (Proj_c(con0, lbl), signat)
	       
	   val context' = update_NILctx_insert_kind(context', var_c, knd)

	   val {crdecs, erdecs} =
	       xsdecs' context' (con0, addToConSubst subst (var_c, Proj_c(con0, lbl)), rest)

	   val kill_con = is_poly andalso (! elaborator_specific_optimizations)

       in  {crdecs = if kill_con then
                          crdecs
                     else
                          ((lbl, var_c), knd) :: crdecs,
	    erdecs = (lbl,var_r,substConInCon subst con) :: erdecs}
       end

     | xsdecs' context (con0, subst, Il.SDEC(lbl, d as Il.DEC_EXP(var,il_con, _, _)) :: rest) =
       let
	   val con = xcon context il_con
	   val (var', context') = insert_rename_var(var, context)
	   val {crdecs, erdecs} = xsdecs' context' (con0, subst, rest)
       in
	   {crdecs = crdecs,
	    erdecs = (lbl,var',substConInCon subst con) :: erdecs}
       end

     | xsdecs' context (con0, subst, sdecs as Il.SDEC(lbl, d as Il.DEC_CON(var, il_knd, 
									maybecon,_))::rest)=
       let
	   val knd = 
	       (case maybecon of
		    NONE => xkind context il_knd
		  | SOME il_con => Single_k(xcon context il_con))

	   val (var', context') = insert_rename_var(var, context)
	   val context'' = update_NILctx_insert_kind(context', var', knd)
	   val {crdecs, erdecs} = 
	       xsdecs' context'' (con0, addToConSubst subst (var', Proj_c(con0, lbl)),rest)

      in   {crdecs = ((lbl, var'), knd) :: crdecs,
	    erdecs = erdecs}
       end

   and xkind context (Il.KIND) = Type_k
     | xkind context (Il.KIND_TUPLE n) = 
       let val k = makeKindTuple n
       in k
       end
     | xkind context (Il.KIND_ARROW (n,il_kres)) =
       let val args = map0count (fn _ => (Name.fresh_var(), Type_k)) n
	   val kres = xkind context il_kres
           val k = Arrow_k (Open, args, kres)
       in  k
       end


   fun xHILctx HILctx =
       let open Il
	   fun dopc(v,l,pc,(imports,context)) = 
	       (case pc of
		    Il.PHRASE_CLASS_EXP (_,il_type, _, _) => 
			let val nil_type = xcon context il_type
			    val (v',context') = insert_rename_var(v,context)
			in  (ImportValue(l,v',TraceUnknown,nil_type)::imports, context')
			end
		  | Il.PHRASE_CLASS_CON (il_con, il_kind, il_conopt, _) => 
			let
			    val kind = xkind context il_kind
			    val nil_con = 
				(case il_conopt of
				     NONE => NONE
				   | SOME il_con => SOME (xcon context il_con))
			    val (v',context') = insert_rename_var(v,context)
			    val it = ImportType(l,v',(case nil_con of
						 NONE => kind
					       | SOME c => Single_k c))
			    val context'' = 
				(case nil_con of 
				     NONE => update_NILctx_insert_kind(context', v', kind)
				   | SOME c => update_NILctx_insert_kind_equation(context', v', c))
			    val context''' = update_NILctx_insert_label(context'',l,v')
			in  (it::imports, context''')
			end
		  | Il.PHRASE_CLASS_MOD (_,is_polyfun,il_sig) => 
			let
			    val (l_c,l_r) = Name.make_cr_labels l
			    val ((v_c, v_r),context) = newSplitVar (v, context)
			    val il_sig = IlContext.UnselfifySig IlContext.empty_context (PATH(v,[]), il_sig)
			    val (knd, type_r) = xsig context (Var_c v_c, il_sig)
				
			    val context = update_NILctx_insert_kind(context, v_c, knd)
			    val context = update_NILctx_insert_label(context, l_c, v_c)
				
			    val iv = ImportValue(l_r,v_r,TraceUnknown,type_r)
			    val it = ImportType(l_c,v_c,knd)
			in
			    if is_polyfun then
				(iv::imports, update_polyfuns(context, v_r))
			    else
				(iv::it::imports, context)
			end
		  | Il.PHRASE_CLASS_SIG(v,il_sig) => 
			(imports,update_insert_sig(context,v,il_sig)))
	   fun folder (p,acc) =
	       let val SOME(l,pc) = IlContext.Context_Lookup_Path(HILctx,p)
	       in  (case (Name.is_dt l, Name.is_nonexport l, p) of
			(false, false, PATH(v,[])) => dopc(v,l,pc,acc)
		      | _ => acc)
	       end
	   val (rev_imports,context) = foldl folder ([],empty_splitting_context HILctx)
	                                  (IlContext.Context_Ordering HILctx)
       in  (rev rev_imports, context)
       end


    fun phasesplit (HILctx : Il.context, 
		    sbnd_entries : (Il.sbnd option * Il.context_entry) list) : Nil.module = 
	let
	    val _ = reset_memo()

            (* we move all the externs into the context first:
	       this does not always work if the externs depend on
	       things in the sbnd_entries list *)

	    fun folder((SOME sbnd,Il.CONTEXT_SDEC sdec),(ctxt,sbnds)) = 
		(ctxt, (sbnd,sdec)::sbnds)
	      | folder((NONE, ce),(ctxt,sbnds)) = 
		(IlContext.add_context_entries(ctxt,
		      [case ce of
			   Il.CONTEXT_SDEC(Il.SDEC(l,dec)) => 
			       Il.CONTEXT_SDEC(Il.SDEC(l,IlContext.SelfifyDec ctxt dec))
			 | _ => ce]),
		 sbnds)
	    val (HILctx,rev_sbnd_sdecs) = foldl folder (HILctx,[]) sbnd_entries
	    val sbnds_sdecs = rev rev_sbnd_sdecs
	    val sbnds = map #1 sbnds_sdecs
	    val sdecs = map #2 sbnds_sdecs



            (* Compute the initial context and imports *)
	    val _ = 
		if (!full_debug) then
		    (print "\nInitial HIL context varlist:\n";
		     app (fn p => (print "  "; Ppil.pp_path p; print "\n")) 
		     (IlContext.Context_Ordering HILctx);
		     print "\n";
		     print "\nInitial HIL context:\n";
		     Ppil.pp_context HILctx;
		     print "\n")
		else
		    ()
		    
	    val (imports,initial_splitting_context) = 
		Stats.subtimer("Phase-split-ctxt",xHILctx)
		HILctx
		
            val _ = NilContext_reset_used (initial_splitting_context)

	    val _ = 
		if (!full_debug) then
		    (print "\nInitial NIL context:\n";
		     NilContext_print initial_splitting_context;
		     print "\n")
		else
		    ()
	    val _ = msg "  Initial context is phase-split\n"

	    (* Phase-split the bindings *)
	    val {cbnd_cat, ebnd_cat, final_context, ...} =
		xsbnds initial_splitting_context sbnds
	    val cbnds = map makeConb (flattenCatlist cbnd_cat)
	    val ebnds = flattenCatlist ebnd_cat
	    val bnds = cbnds @ ebnds
	    val (nil_initial_context,used) = filter_NILctx initial_splitting_context
	    val (nil_final_context,_) = filter_NILctx final_context
	    val _ = msg "  Bindings are phase-split\n" 

	    fun filtering l = if !debug
				  then print ("filtering import " ^ Name.label2string l ^ "\n")
			      else ()
		
	     (* Filter out the unused imports *)
	    fun filter_imports [] = ([], used)
              | filter_imports ((iv as ImportValue(l,v,_,c)) :: rest) =
		   let
		       val result as (imports, used) = filter_imports rest
		   in
		       if (VarSet.member(used, v) orelse Name.keep_import l) then
			   (iv :: imports, 
			    let val (fvTerm,fvType) = NilUtil.freeExpConVarInCon(true,0,c)
			    in  VarSet.union(used, VarSet.union(fvTerm, fvType))
			    end)
		       else
			   (filtering l; result)
		   end
              | filter_imports ((it as ImportType(l,v,k)) :: rest) =
		   let
		       val result as (imports, used) = filter_imports rest
		   in
		       if (VarSet.member(used, v) orelse Name.keep_import l) then
			   (it :: imports, 
			    VarSet.union(used, NilUtil.freeVarInKind (0,k)))
		       else
			   (filtering l; result)
		   end
	    val imports = if (!killDeadImport) then #1 (filter_imports imports) else imports

	    val _ = msg "  Imports are computed\n" 


	    (* create the exports *)
	    fun folder ((Il.SDEC(l,dec)),exports) = 
		    (case (not (Name.is_nonexport l) andalso
			   not (Name.is_dt l), dec) of
			 (false,_) => exports
		       | (true,Il.DEC_EXP (v,_,_,_)) => 
			     let val v' = rename_var (v, final_context)
			     in  (ExportValue(l,v')::exports)
			     end
		       | (true,Il.DEC_CON (v,_,_,_)) =>
			     let 
				 val v' = rename_var (v, final_context)
				 val k = NilContext.find_kind(nil_final_context, v')
				     handle e => (print "exception while doing DEC_CON\n";
						  raise e)
			     in  (ExportType(l,v')::exports)
			     end
		       | (true,Il.DEC_MOD (v,is_polyfun,s)) => 
			     let val (lc,lr) = Name.make_cr_labels l
				 (* Already bound *)
				 val ((vc,vr),_) = splitVar (v,final_context)
				 val exports = 
				     if is_polyfun then
					 (ExportValue(lr,vr)::exports)
				     else 
					 (ExportValue(lr,vr)::
					  ExportType(lc,vc)::
					  exports)
			     in  exports
			     end)
	    val exports : export_entry list = rev(foldr folder [] sdecs)
	    val _ = msg "  Exports are phase-split\n" 

	    val nilmod = MODULE{bnds = bnds, 
				imports = imports,
				exports = exports}

	    val _ = reset_memo()
	in  nilmod
	end



end

