(* We box all floats and translate floating point operations accordingly.
   Also note that all term-level (but not type-level)
        record fields must be sorted by their labels. *)

(* Issues.

     (1) All the questions marked XXX in the comments below.

     (2) There is some inconsistency in how particular Elaborator
         idioms are recognized.

           There are several methods used to detect modules that
            are really the translation of SML polymorphic instantiation,
            polymorphic types, and polymorphic definitions.  This is
            perhaps inevitable...the translation of
                val x = SOME
            needn't explicitly mention any "it" labels, for example,
            even though we really want to know that the functor that
            the Elaborator generates for x (equivalent to /\a. SOME[a])
            really is a polymorphic definition.

         All such checks can be found by looking for uses of the
         elaborator_specific_optimizations ref.
 *)

structure Tonil :> TONIL =
struct


   (**********************)
   (* Flags and Counters *)
   (**********************)

   val xmod_count = ref 0
   val xsig_count = ref 0
   val xcon_count = ref 0
   val xexp_count = ref 0
   val xsdecs_count = ref 0
   val xsbnds_count = ref 0

   val diag       = Stats.ff("TonilDiag")
   val debug      = Stats.ff("TonilDebug")
   val full_debug = Stats.ff("TonilFullDebug")
   val printFlat = Stats.ff("TonilPrintFlat")
   val chatlev    = ref 0

   (* killDeadImport  :  should the import list be GC'ed to include
                         only variables used by the code being split?

      do_polyrec      :  should we transform mutually-recursive polymorphic
                         functions into polymorphic recursion?

                         It is *not* obvious that this is a good idea,
                         but it does make the resulting code slightly
                         easier to read.

                         XXX: What is the right default?

      keep_hil_numbers:  when mapping variables to var_c and var_r,
                         should the resulting names mention the HIL
                         number of the original variable, or only the
                         name (string) part?  Off by default because
                         it makes variable names much more readable,
                         but can be turned on to help see the
                         correspondence between HIL and MIL code.

      flatten_modules:   Don't change this without changing 
                         FLATTEN_MODULES in Runtime/client.h 
			 appropriately.

                         Flatten all imported and exported structures
                         down to a list of imports of their individual
			 exp, con, and functor components.  This in 
			 principle could make code drastically
			 smaller, since it allows the linker to thin
			 out all of the components that aren't used.
			 In fact, we don't seem to be getting much
			 benefit from this.  I believe that this is
			 due to the fact that the link function almost
			 always mentions most of the labels exported
			 from a unit, and hence prevents them from
			 being thinned.  If we had a mechanism for
			 redirecting the linker from one label to
			 another, this would be greatly alleviated in
			 the common case where we simply re-export an
			 imported label (the sub-structure idiom).
   			                                        -leaf
			 
   *)

   val do_preproject            = Stats.ff("do_preproject")
   val do_memoize               = Stats.ff("do_memoize")
   val do_polyrec               = Stats.ff("do_polyrec")
   val keep_hil_numbers         = Stats.ff("keep_hil_numbers")

   (* Don't change this without setting/unsetting FLATTEN_MODULES in
    * Runtime/client.h appropriately!!
    *)
   val flatten_modules          = Stats.ff("flatten_modules")

   val killDeadImport           = CompilerControl.KillDeadImport
   val ref_is_array             = CompilerControl.RefIsArray
   val do_exports_int           = CompilerControl.DoExportsInt

   (* The elaborator_specific_optimizations ref controls whether
      the phase-splitter should do some simple optimizations
      that rely upon knowing exactly how the elaborator
      generates code.
       (1) Transforms all structures with a single component
           named "it" to the value of that component,
           rather than to a single-element record
           with that component.  Also, does not
           create a binding of a _c variable when
           splitting such structures, since we know it
           will never be used; there are no type components,
           and such a structure will never be used as a
           source-level structure (e.g., passed to a functor).

           Correspondingly, ignores all module projections of
           a component named "it".

       (2) Strips out structures whose label is recognized as a
           datatype label (satisfying Name.is_dt).  Such structures
           are generated by the elaborator (their signatures
           are probably used in the context to determine whether
           identifiers are datatype constructors or not) but
           the code for the constructors is always inlined,
           so the resulting code never actually uses these
           "inner" datatype modules.

       (3) Transforms polymorphic recursive-functions into
           polymorphic recursion, if the do_polyrec flag
           is also turned on.  (Also doesn't define a _c part
           for these functors, which is ok because of #4.)

       (4) Avoids generating a constructor application when
           phase-splitting a functor application that's really
           polymorphic instantiation.

       Unless the elaborator changes significantly, this
       flag should always be kept "true" (which is why
       it doesn't live in the Stats module).  It probably
       shouldn't be in the signature of this structure either.
    *)
   val elaborator_specific_optimizations = ref true


   (***************************)
   (* Opens and other imports *)
   (***************************)

   open Nil Listops

   structure N = Name
   structure NU = NilUtil
   structure NS = NilSubst
   structure VarSet = Name.VarSet

   val perr_c = NilError.perr_c
   val perr_e = NilError.perr_e
   val perr_k = NilError.perr_k
   val perr_c_k = NilError.perr_c_k
   val eq_label = N.eq_label


   (*******************)
   (* Error Reporting *)
   (*******************)

   fun error msg = Util.error "tonil.sml" msg
   fun msg str = if (!diag) then print str else ()

   fun chat lev str = if (!chatlev) >= lev then print str else ()
   val chat0 = chat 0
   val chat1 = chat 1
   val chat2 = chat 2

   (*****************************)
   (* Various utility functions *)
   (*****************************)

   (* Given n, collect the labels and bound variables of the next n
      structure bindings or signature declarations respectively,
      and then return these along with any leftover bindings/declarations
    *)

   fun getSbndNames (n : int) (sbnds : Il.sbnd list) :
                       Il.sbnd list * N.label list * N.var list =
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

   fun getSdecNames (n : int) (sdecs : Il.sdec list) =
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
       in   loop n (sdecs,[],[])
       end

   fun makeInternalLabels n = Listops.map0count (fn n => N.fresh_internal_label "bnd") n

   (*This will work as long as you don't do any composing of substitutions*)
   (* XXX ? *)
   val addToConSubst = NilSubst.C.sim_add

   (* extractProjLabels.  Splits a module "mod.lbls" into
        the "mod" and a list of labels.
    *)
   fun extractProjLabels module =
       let
	   fun loop (Il.MOD_PROJECT(module, lbl), accum) =
	              loop (module, lbl :: accum)
             | loop (module, accum) = (module, accum)

	   fun checkloop (Il.MOD_PROJECT(module, lbl), accum) =
	            if (N.is_dt lbl) then
		      error "Use of datatype label detected"
		    else
		      checkloop (module, lbl :: accum)
             | checkloop (module, accum) = (module, accum)
       in
         (* Avoid checking the ref every time around the loop
          *)
	 if (!elaborator_specific_optimizations) then
	   checkloop (module, nil)
	 else
	   loop (module, nil)
       end

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

   fun setTyvar tyvar con = Tyvar.tyvar_reset (tyvar,con)

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
     | selectFromRec (exp,lbl::lbls) = selectFromRec(Prim_e(NilPrimOp(select lbl), [], [],
							    [exp]), lbls)

   (**********************************)
   (* The Variable-Splitting Mapping *)
   (**********************************)

   (* For the phase-splitting, we need a way of turning each module
      variable var into a new constructor variable var_c and a new
      term variable var_r.  We could play tricks like multiplying
      each HIL variable number by 3 and then adding 1 or 2
      to denote _c and _r, but this is unpleasant.  Thus, we instead
      maintain a mapping from var to the pair (var_c, var_r),
      which is simply a pair of fresh variables.
   *)

   type vmap = (N.var * N.var * N.var) N.VarMap.map


   val empty_vmap : vmap = N.VarMap.empty

   (* Note that many of these functions are redefined below to take a
      full phase-splitting context as the first argument, rather than
      just a vmap *)

   fun lookupVmap (var : N.var, vmap : vmap) : (N.var * N.var * N.var) option =
     N.VarMap.find (vmap, var)

   (* Determines the corresponding _c and _r variables for a
      new bound module variable.
    *)
   fun splitNewVar (var : N.var, vmap : vmap) : N.var * N.var * N.var * vmap =
     let
       (* Sometimes the elaborator generates shadowing for module
          variables --- or (possibly?) the phase splitter may flatten
          some bindings resulting in previously-disjoint scopes
          overlapping, again resulting in shadowing.  In any case, we
          generate fresh _c and _r variables corresponding to the
          inner (shadowing) variable.  (We can't re-use the previous
          _c and _r variables ; see the comments for the renaming map
          below.)
       *)

       (*
	  The only reason to generate warnings about this is if we expect
	  the elaborator to never generate shadowed module variables, in
	  which case we can detect here that this invariant fails.
       val _ = (case (lookupVmap (var,vmap) of
		  NONE => ()
		| SOME _ => (print "Warning: splitNewVar called \
		                    \on already existing variable ";
			     Ppnil.pp_var var; print "\n"))
       *)

       val var_name =
	 if (!keep_hil_numbers) then
	   N.var2string var
	 else
	   N.var2name var
       val var_c = N.fresh_named_var (var_name ^ "_c")
       val var_s = N.fresh_named_var (var_name ^ "_s")
       val var_r = N.fresh_named_var (var_name ^ "_r")
     in
       (var_c, var_s, var_r, N.VarMap.insert (vmap, var, (var_c, var_s, var_r)))
     end

   (* Determines the corresponding _c and _r variables for a
      use of a module variable that should already have been bound.

      XXX  Why is this returning a vmap?  Consistency with splitNewVar?
    *)

   fun splitVar (var : N.var, vmap : vmap) : N.var * N.var * N.var * vmap =
     (case (lookupVmap (var,vmap)) of
	NONE => error ("splitVar called on non-existent variable " ^
		       (N.var2string var))
      | SOME (var_c, var_s, var_r) => (var_c, var_s, var_r, vmap))

   (* Prints a vmap.  Used only for tracing/debugging *)
   fun printVmap (vmap : vmap) : unit =
     N.VarMap.appi (fn (k,(v1,v2,v3)) => (Ppnil.pp_var k;
					  print "=(";
					  Ppnil.pp_var v1;
					  print ",";
					  Ppnil.pp_var v2;
					  print ",";
					  Ppnil.pp_var v3;
					  print ") ")) vmap

   (*********************************)
   (* The Variable-Renaming Mapping *)
   (*********************************)

   (* Every variable other than module variables are renamed.
      This ensures that every bound variable in the resulting program
      is distinct --- not currently guaranteed by the elaborator ---
      and, as a consequence, that there is not shadowing of
      constructor variables (which makes the Nil typechecker very
      unhappy, since this doesn't go well with dependent kinds.)
   *)

   type rmap = N.var N.VarMap.map

   fun do_rename_var (v : N.var) : N.var =
       let
	   val var_name = if (!keep_hil_numbers) then
	              N.var2string v
		   else
		      N.var2name v
	   val v' = N.fresh_named_var var_name
       in
	   v'
       end

   fun insert_rename_var (v : N.var, rmap : rmap) =
       let
	   val v' = do_rename_var v
       in
	   (v', N.VarMap.insert (rmap, v, v'))
       end

   fun insert_rename_vars (vs : N.var list, rmap : rmap) =
       Listops.foldl_acc insert_rename_var rmap vs

   fun insert_given_vars (oldvars : N.var list,
			  newvars : N.var list,
			  rmap : rmap) =
     (case (oldvars,newvars) of
	([],[]) => rmap
      | ((v::vs),(v'::vs')) =>
	  insert_given_vars (vs, vs', N.VarMap.insert(rmap, v, v'))
      | _ => error "insert_given_vars passed lists of unequal length")

   fun rename_var (v, rmap) =
       (case N.VarMap.find(rmap, v) of
	    SOME v' => v'
	  | NONE => (print "Couldn't find IL variable ";
		     Ppnil.pp_var v;
		     print " in rmap\n";
		     error "can't rename_var"))
   fun rename_vars(vs, rmap) = map (fn v => rename_var(v,rmap)) vs


   (***************************)
   (* Phase-Splitting Context *)
   (***************************)

   local
     (* The splitting context contains all the information maintained
        as the phase-splitting process goes along.  This includes
          HILctx : The initial HIL context (the imports). Not
                   kept up-to-date as we traverse the code, as
                   it's only used by some of the IlUtil functions ---
                   apparently to find the bool type and simple things
		   like that.

          NILctx : The Nil context, containing the kinds of the NIL
                   bound *type* variables visible at this point in the
                   translation's output.  This is used to return and
                   report the kinds of the type exports, as well as
                   other minor things like finding the bool type and
                   head-normalizing/unrolling mu types.

                   Bound term variables are not put into the context,
                   because they're never needed.

          sigmap : a mapping from signature variables to IL signatures,
	           accumulated as the imports are processed.

          used   : The set of all (NIL) variables used in the generated
                   NIL code; this is used to figure out which (translations
                   of) imports must be kept and which can be thrown away.

                   The used set is kept as a ref, because it's really
                   more like a state that needs to be threaded through
                   the entire program.  Unlike other compiler phases
                   such as hoisting, we don't bother to maintain a
                   separate phase-splitting context and phase-splitting
                   state.

          vmap   : The module-variable splitting map, as described above

          rmap   : The renaming map, as described above

          polyfuns : The set of _r variables corresponding to IL
                     variables bound to functors that are really
                     the translation of SML polymorphic functions.
                     (Applications of such functors can omit the
                     application of the _c parts, so we want to
                     detect such cases.)
     *)
     datatype splitting_context =
       CONTEXT of {NILctx : NilContext.context,
		   HILctx : Il.context,
		   sigmap : Il.signat Name.VarMap.map,
		   used   : Name.VarSet.set ref,
		   vmap   : (var * var * var) Name.VarMap.map,
		   rmap   : var Name.VarMap.map,
                   polyfuns : Name.VarSet.set}
in (* local *)

    type splitting_context = splitting_context

    fun make_initial_splitting_context HILctx =
          CONTEXT{NILctx = NilContext.empty(),
		  HILctx = HILctx,
		  sigmap = Name.VarMap.empty,
		  used = ref Name.VarSet.empty,
		  vmap = Name.VarMap.empty,
		  rmap = Name.VarMap.empty,
		  polyfuns = Name.VarSet.empty}

   fun print_splitting_context (CONTEXT{NILctx,vmap,rmap,...}) =
       (Name.VarMap.appi
	   (fn (v,(vc,vs,vr)) => (Ppnil.pp_var v; print "  -->  ";
				  Ppnil.pp_var vc; print ", ";
				  Ppnil.pp_var vs; print ", ";
				  Ppnil.pp_var vr; print "\n")) vmap;
	print "\n";
	N.VarMap.appi
	(fn (v,v') => (Ppnil.pp_var v; print "  -->  ";
		       Ppnil.pp_var v'; print "\n")) rmap;
	print "\n";
	NilContext.print_context NILctx;
	print "\n")


     (* Getters for Splitting Context *)
     (*********************************)

     fun get_nilctxt (CONTEXT{NILctx,...}) = NILctx

     fun get_hilctxt (CONTEXT{HILctx,...}) = HILctx

     fun get_used (CONTEXT{used,...}) = !used
     fun reset_used (CONTEXT{used,...}) = used := N.VarSet.empty

     fun var_is_used (CONTEXT{used,...}) var = N.VarSet.member(!used,var)

     fun find_sig(CONTEXT{sigmap,...},v) = N.VarMap.find(sigmap,v)

     fun var_is_polyfun (CONTEXT{polyfuns,...}, v) =
       N.VarSet.member(polyfuns,v)

(*
     fun lookup_module_alias(CONTEXT{alias,memoized_mpath,...},m) =
       case (extractProjLabels m) of
	 (Il.MOD_VAR v,labs) =>
	   let fun follow_alias(v,labs) =
	     (case (N.VarMap.find(alias,v)) of
		NONE => (v,labs)
	      | SOME (v',labs') => follow_alias(v',labs' @ labs))
	       val p = follow_alias(v,labs)
	   in  N.PathMap.find(memoized_mpath,p)
	   end
       | _ =>  error "lookup_module_alias given non-path module"
*)

     (* Setters for Splitting Context *)
     (*********************************)

     fun mark_var_used(CONTEXT{used,...},v) =
       if (N.VarSet.member(!used,v))
	 then ()
       else used := N.VarSet.add(!used,v)

       (* It's amazing how handy ocaml-style functional record update
          would be in the following functions! *)

     fun replace_NILctx(CONTEXT{NILctx,HILctx,sigmap,vmap,rmap,
				used,polyfuns},
			NILctx') =
       let
       in  CONTEXT{NILctx=NILctx', HILctx=HILctx,
		   sigmap=sigmap, vmap=vmap, rmap=rmap, used = used,
		   polyfuns=polyfuns}
       end

     fun update_NILctx_insert_kind(CONTEXT{NILctx,HILctx,sigmap,vmap,rmap,
					   used,polyfuns},
				   v, k) =
       let
	 val NILctx' = NilContext.insert_kind(NILctx,v,k)
       in  CONTEXT{NILctx=NILctx', HILctx=HILctx,
		   sigmap=sigmap, vmap=vmap, rmap=rmap, used = used,
		   polyfuns=polyfuns}
       end

     fun update_NILctx_insert_cbnd(CONTEXT{NILctx,HILctx,sigmap,vmap,rmap,
					   used,polyfuns},
				   cb) =
       let
	 val NILctx' = NilContext.insert_cbnd(NILctx,cb)
       in  CONTEXT{NILctx=NILctx', HILctx=HILctx,
		   sigmap=sigmap, vmap=vmap, rmap=rmap, used = used,
		   polyfuns=polyfuns}
       end

     fun update_NILctx_insert_cbnd_list(CONTEXT{NILctx,HILctx,sigmap,vmap,rmap,
						used,polyfuns},
					cbs) =
       let
	 val NILctx' = NilContext.insert_cbnd_list(NILctx,cbs)
       in  CONTEXT{NILctx=NILctx', HILctx=HILctx,
		   sigmap=sigmap, vmap=vmap, rmap=rmap, used = used,
		   polyfuns=polyfuns}
       end

     fun update_NILctx_insert_kind_equation(CONTEXT{NILctx,HILctx,sigmap,vmap,
						    rmap, used, polyfuns},
					    v, c) =
       let val k = Single_k c
	 val NILctx' = NilContext.insert_kind(NILctx,v,k)
       in  CONTEXT{NILctx=NILctx', HILctx=HILctx,
		   sigmap=sigmap, vmap=vmap, rmap=rmap, used = used,
		   polyfuns=polyfuns}
       end

     fun update_NILctx_insert_kind_list(ctxt,vklist) =
       foldl (fn ((v,k),ctxt) => update_NILctx_insert_kind (ctxt,v,k))
             ctxt vklist

(*
     fun add_modvar_alias(CONTEXT{NILctx,HILctx,sigmap,vmap,rmap,used,
				  memoized_mpath,alias,polyfuns},var,path) =
       let val alias' = N.VarMap.insert(alias,var,path)
       in  CONTEXT{NILctx=NILctx, HILctx=HILctx, sigmap=sigmap, vmap=vmap, rmap=rmap,
		   used=used,alias=alias',  memoized_mpath=memoized_mpath,
		   polyfuns=polyfuns}
       end

     fun add_module_alias(CONTEXT{NILctx,HILctx,sigmap,vmap,rmap,used,alias,
				  memoized_mpath,polyfuns},
			  m, name_c, name_r, con_r ) =
       (case (extractProjLabels m) of
	  (Il.MOD_VAR v, labs) =>
	    let val p = (v,labs)
	      val memoized_mpath' =
		N.PathMap.insert(memoized_mpath, p, (name_c, name_r, con_r))
	    in  CONTEXT{NILctx=NILctx, HILctx=HILctx,
			sigmap=sigmap, vmap=vmap, rmap=rmap,
			used=used,alias=alias,
			memoized_mpath=memoized_mpath',
			polyfuns = polyfuns}
	    end
	| _ => error "add_module_alias given non-path module")
*)
       (* The following have to be "val" rather than "fun" because we're
          using the same name for the wrapped functions as for the
          originals *)

     val insert_rename_var = fn (v, CONTEXT{NILctx,HILctx,sigmap,used,vmap,
					    rmap,polyfuns}) =>
       let
	 val (v',rmap') = insert_rename_var(v,rmap)
       in
	 (v', CONTEXT{NILctx=NILctx, HILctx=HILctx,
		      sigmap=sigmap,
		      used=used, vmap=vmap, rmap=rmap',
		      polyfuns=polyfuns})
       end

     val insert_rename_vars =
       fn (vs, CONTEXT{NILctx,HILctx,sigmap,
		       used,vmap,rmap,polyfuns}) =>
       let
	 val (vs',rmap') = insert_rename_vars(vs,rmap)
       in
	 (vs', CONTEXT{NILctx=NILctx, HILctx=HILctx,
		       sigmap=sigmap,
		       used=used, vmap=vmap, rmap=rmap',
		       polyfuns=polyfuns})
       end

     val insert_given_vars =
       fn (vs, vs', CONTEXT{NILctx,HILctx,sigmap,
			    used,vmap,rmap,polyfuns}) =>
       let
	 val rmap' = insert_given_vars(vs,vs',rmap)
       in
	 CONTEXT{NILctx=NILctx, HILctx=HILctx, sigmap=sigmap,
		 used=used, vmap=vmap, rmap=rmap',
		 polyfuns=polyfuns}
       end


     fun update_insert_sig (CONTEXT{NILctx,HILctx,sigmap,used,vmap,rmap,
				     polyfuns},
			     v, hilsig) =
       CONTEXT{NILctx=NILctx, HILctx=HILctx,
	       sigmap=N.VarMap.insert(sigmap,v,hilsig),
	       used=used, vmap=vmap, rmap=rmap,
	       polyfuns=polyfuns}

     fun update_polyfuns (CONTEXT{NILctx,HILctx,sigmap,used,vmap,rmap,
				  polyfuns},
			  v) =
       CONTEXT{NILctx=NILctx, HILctx=HILctx, sigmap=sigmap,
	       used=used, vmap=vmap, rmap=rmap,
	       polyfuns=N.VarSet.add(polyfuns,v)}

     fun update_polyfuns_list(CONTEXT{NILctx,HILctx,sigmap,used,rmap,vmap,
				      polyfuns},
			      vs) =
       CONTEXT{NILctx=NILctx, HILctx=HILctx, sigmap=sigmap,
	       used=used, vmap=vmap, rmap=rmap,
	       polyfuns=N.VarSet.addList(polyfuns,vs)}


     (* Wrappers for other functions, so that they can be passed a
        full splitting context rather than just a NilContext.context *)
     (****************************************************************)


     fun NilContext_print(CONTEXT{NILctx,...}) =
       NilContext.print_context NILctx

     fun NilContext_kind_of(CONTEXT{NILctx,...},c) =
       NilContext.kind_of(NILctx, c)

     fun NilContext_con_hnf(CONTEXT{NILctx,...},c) =
	 #2(Normalize.reduce_hnf(NILctx, c))

       (* The following have to be "val" rather than "fun" because we're
          using the same name for the wrapped functions as for the
          originals *)

     val lookupVmap = fn (v,CONTEXT{vmap,...}) => lookupVmap (v,vmap)

     val splitVar = fn (var, CONTEXT{NILctx,HILctx,sigmap,used,vmap,rmap,
				     polyfuns}) =>
                  let val (var_c,var_s,var_r,vmap') = splitVar (var, vmap)
		  in  ((var_c,var_s,var_r),
		       CONTEXT{NILctx=NILctx, HILctx=HILctx, sigmap=sigmap,
			       used=used, vmap=vmap', rmap=rmap,
			       polyfuns=polyfuns})
		  end

     val splitNewVar = fn (var, CONTEXT{NILctx,HILctx,sigmap,used,vmap,rmap,
					polyfuns}) =>
       let val (var_c,var_s,var_r,vmap') = splitNewVar (var, vmap)
       in  ((var_c,var_s,var_r),
	    CONTEXT{NILctx=NILctx, HILctx=HILctx, sigmap=sigmap,
		    used=used, vmap=vmap', rmap=rmap,
		    polyfuns=polyfuns})
       end


     val rename_var = fn (v, CONTEXT{rmap,...}) => rename_var(v,rmap)

     val rename_vars = fn (vs, CONTEXT{rmap,...}) => rename_vars(vs, rmap)

     fun update_NILctx_insert_con(CONTEXT{NILctx,HILctx,sigmap,vmap,rmap, used,
					  polyfuns},v,c) =
	 let
	     val NILctx' = NilContext.insert_con(NILctx,v,c)
	 in  CONTEXT{NILctx=NILctx', HILctx=HILctx,
		   sigmap=sigmap, vmap=vmap, rmap=rmap, used = used,
		     polyfuns=polyfuns}
	 end
     fun update_NILctx_insert_con_list(CONTEXT{NILctx,HILctx,sigmap,vmap,rmap, used,
					       polyfuns},vcs) =
	 let
	   val NILctx' = NilContext.insert_con_list(NILctx,vcs)
	 in  CONTEXT{NILctx=NILctx', HILctx=HILctx,
		     sigmap=sigmap, vmap=vmap, rmap=rmap, used = used,
		     polyfuns=polyfuns}
	 end

     val insert_con = update_NILctx_insert_con
     val insert_kind = update_NILctx_insert_kind
     val insert_kind_equation = update_NILctx_insert_kind_equation

     fun strip_arrow_norm (CONTEXT{NILctx,...}) c = Normalize.strip_arrow_norm NILctx c

     fun find_con(CONTEXT{NILctx,...},v) = (NilContext.find_con(NILctx,v)) 
       handle NilContext.Unbound => (error ("Variable " ^ (Name.var2string v) ^ " not found in NIL context"))
				     

     fun type_of (CONTEXT{NILctx,...}) e = NilRename.renameCon (Normalize.type_of (NILctx, e))

end (* local defining splitting context *)


   (***************)
   (* Memoization *)
   (***************)

   (* The phase-splitter memoizes the translations of those
      IL constructors that are module projections.

      There used to be code for memoizing the translations of module
      terms as well, but as of August 2002 it wasn't being used and so
      was removed.
    *)
(*
   local
       (* Memopad for translation of constructor paths
	  Indexed first by the module's variable, and then by the
          full projection path.

          The double indirection is probably so that one can quickly
          clear any past information about all paths starting with
          a particular variable, when this variable is rebound.
        *)
       val con_memo = ref (N.VarMap.empty : con N.PathMap.map N.VarMap.map)
   in
       fun reset_memo() = (con_memo := N.VarMap.empty)

       (* Currently, every time we come across a binding of an IL module var
          we must clear the memo pad of all paths beginning with this
          variable.  This is a global table, so if we find a variable
          in a signature, say, then paths involving this variable
          remain in the memo pad even after we're done with the signature.
          This could get confusing if we come across the same variable
          being used in a disjoint scope, such as the corresponding structure.
          (Especially since the phase-splitter wants to rename the variable
          differently in the separate occurrences.)
        *)
       fun clear_memo v = (con_memo := (#1(N.VarMap.remove(!con_memo,v))
					handle LibBase.NotFound => !con_memo))

       fun lookup_con_memo (path as (v,lbls)) thunk =
	 (case N.VarMap.find(!con_memo,v) of
	    NONE => (* Module variable hasn't been seen before.
                       Insert it with an empty pathmap and recurse (taking
                       the SOME case instead, which will eventually add
		       the current constructor path to this new pathmap)
		     *)
	            (con_memo := N.VarMap.insert(!con_memo,v,N.PathMap.empty);
		     lookup_con_memo path thunk)
	  | SOME pathmap =>
	      (case N.PathMap.find(pathmap,path) of
		 SOME result => (* Found the answer.  We could have recomputed
				   it by calling thunk(), but this is now
				   avoided
				 *)
		                NilRename.renameCon result
	       | NONE => (* We haven't seen this path before, so we have
			    to do the actual translation, which is
			    performed by the thunk.
			  *)
			 let val result = thunk()
			 in
			   con_memo :=
			      N.VarMap.insert(!con_memo, v,
					      N.PathMap.insert(pathmap, path,
							       result));
			   result
			 end))
   end (*local *)
*)

   (**************************)
   (* IL-to-NIL translations *)
   (**************************)

   (* Type definitions for the return types of some of the
      translation functions; contents are described more
      below.
    *)

   type xmod_result = {cbnd_cat : conbnd catlist,
		       sbnd_cat : conbnd catlist,
		       ebnd_cat : bnd catlist,
		       name_c : con,
		       name_s : con,
		       name_r : exp,
		       context : splitting_context}

   type xbnds_result = {cbnd_cat : conbnd catlist,
			sbnd_cat : conbnd catlist,
			ebnd_cat : bnd catlist,
			final_context : splitting_context}

   type xsbnds_result = {cbnd_cat : conbnd catlist,
			 sbnd_cat : conbnd catlist,
			 ebnd_cat : bnd catlist,
			 final_context : splitting_context,
			 record_c_con_items : (N.label * con) list,
			 record_s_con_items : (N.label * con) list,
			 record_r_exp_items : (N.label * exp) list}

   type xsdecs_result = {crdecs : ((N.label * N.var) * kind) list,
			 srdecs : ((N.label * N.var) * kind) list,
			 erdecs : (N.label * N.var * con) list}

   (* One last helper function (which must be defined after the wrapped
      splitNewVar function).

      Given optional desired variables names and a splitting context,
      return either that name or else completely fresh ones
    *)
   fun chooseName (NONE, ctxt) = let val v = N.fresh_var()
				     val ((vc,vs,vr),ctxt) = splitNewVar (v,ctxt)
				 in  (v,vc,vs,vr,ctxt)
				 end
     | chooseName (SOME (var,var_c,var_s,var_r), ctxt) = (var, var_c, var_s, var_r, ctxt)



   (* xeffect.
         Translates the total/partial distinction from HIL to MIL.
    *)
   fun xeffect (Il.TOTAL) = Total
     | xeffect (Il.PARTIAL) = Partial
     | xeffect (Il.APPLICATIVE) = Partial
     | xeffect (Il.GENERATIVE) = Partial

   (* xilprim.  Translates the simplest "IL primitives" (primitives
       which are only segregated in the IL for typing reasons) into
       the corresponding common primitives, which have the same
       run-time behavior at the level of bits.

       Only the primitives that take no arguments are handled here;
       the ref-related primitives are translated in the ILPRIM case
       of xexp'.
    *)
   fun xilprim (Prim.eq_uint intsize)     = Prim.eq_int intsize
     | xilprim (Prim.neq_uint intsize)    = Prim.neq_int intsize
     | xilprim (Prim.not_uint intsize)    = Prim.not_int intsize
     | xilprim (Prim.and_uint intsize)    = Prim.and_int intsize
     | xilprim (Prim.or_uint intsize)     = Prim.or_int intsize
     | xilprim (Prim.xor_uint intsize)    = Prim.xor_int intsize
     | xilprim (Prim.lshift_uint intsize) = Prim.lshift_int intsize


   (* xmod.  Translation of an IL module.
             Arguments are a splitting context, the module to be
             translated, and optionally a specification of names
             to which the two parts of the module must be bound.
	     If so, these names must be "fresh" (not in the context).

      Assume
         xmod in_context (il_mod, required_names)
      returns
         {name_c, name_r, cbnd_cat, ebnd_cat, context} =

      Let cbnds = flatten_catlist cbnd_cat
      and ebnds = flatten_catlist ebnd_cat

      Then

        (1) the compile-time part of mod is LET_C cbnds IN name_c END
        (2) the run-time part of mod is LET_E cbnds, ebnds IN name_r END
        (3) ctx |- il_signat == il_signat' : Sig
	(4) The domains of cbnds and ebnds are disjoint from the
            domain of in_context
        (5) get_nilctxt(context) = get_nilctxt(in_context) ++
	    the (singleton) kinds of the variables bound in cbnds

      If required_names = SOME(var, var_r, var_c) then additionally we have

        (6) name_c = Var_c var_c
        (7) name_r = Var_e var_r
    *)


   fun xmodpathCon (context : splitting_context) (il_mod : Il.mod) : con =
     (case il_mod of
          Il.MOD_VAR v =>
	      let 
		  val ((var_c,_,_),_) = splitVar(v,context)
		  val _ = mark_var_used(context,var_c)
		  val con = Var_c var_c
	      in
		  con
	      end
	| Il.MOD_PROJECT (m,lbl) => 
	      let 
		  val con_m = xmodpathCon context m
		  val con = Proj_c(con_m,lbl)
	      in
		  con
	      end
	| Il.MOD_APP(mfun,marg) => 
	      let
		  val con_fun = xmodpathCon context mfun
		  val con_arg = xmodpathCon context marg
		  val con = App_c(con_fun,[con_arg])
	      in
		  con
	      end
	| _ => (print "xmodpathCon: invoked on module not in extended path form\n";
		Ppil.pp_mod il_mod;
		error "xmodpathCon: invoked on module not in extended path form"))

   fun xmodpathSum (context : splitting_context) (il_mod : Il.mod) : con =
(*
       (case extractProjLabels il_mod of
	    (Il.MOD_VAR v, lbls) =>
		let val ((_,v_s,_),_) = splitVar(v,context)
		    val _ = mark_var_used(context,v_s)
		    val con_s = selectFromCon(Var_c v_s, lbls)
		in
		    con_s
		end
*)
     (case il_mod of
          Il.MOD_VAR v =>
	      let 
		  val ((var_c,var_s,_),_) = splitVar(v,context)
		  val _ = mark_var_used(context,var_s)
		  val con = Var_c var_s
	      in
		  con
	      end
	| Il.MOD_PROJECT (m,lbl) => 
	      let 
		  val con_m = xmodpathSum context m
		  val con = Proj_c(con_m,lbl)
	      in
		  con
	      end
	| Il.MOD_APP(mfun,marg) => 
	      let
		  val con_fun = xmodpathSum context mfun
		  val con_arg_c = xmodpathCon context marg
		  val con_arg_s = xmodpathSum context marg
		  val con = App_c(con_fun,[con_arg_c,con_arg_s])
	      in
		  con
	      end
	| _ => (print "xmodpathSum: invoked on module not in extended path form\n";
		Ppil.pp_mod il_mod;
		error "xmodpathSum: invoked on module not in extended path form"))

   fun xmodpath (context : splitting_context) (il_mod : Il.mod) : (con * con option * exp) option =
       (case extractProjLabels il_mod of
	    (Il.MOD_VAR v, lbls) =>
		let
		    val ((v_c,v_s,v_r),_) = splitVar(v,context)
		    val _ = mark_var_used(context,v_c)
		    val _ = mark_var_used(context,v_r)
		    val not_var_poly = NilContext.bound_con(get_nilctxt context, v_s)
		    val _ = if not_var_poly then mark_var_used(context,v_s) else ()
		    val con_c = selectFromCon(Var_c v_c, lbls)
		    val conopt_s = if not_var_poly then SOME(selectFromCon(Var_c v_s, lbls)) else NONE
		    val exp_r = selectFromRec(Var_e v_r, lbls)
		in
		    SOME (con_c,conopt_s,exp_r)
		end
	  | _ => NONE)

   fun xmod (context : splitting_context)
            (args as (il_mod : Il.mod,
		      required_names : (N.var * N.var * N.var * N.var) option))
	    : xmod_result =
       let
	   val this_call = ! xmod_count  (* remember so that we can
                                            display the same number in
                                            the tracing message on return *)
	   val _ =
	       if (!debug) then
		   (xmod_count := this_call + 1;
		    print "\nCall ";
		    print (Int.toString this_call);
		    print " to xmod\n";
		    if (!full_debug) then (Ppil.pp_mod il_mod 
					   (*; print "\n"; print_splitting_context context*)) else ();
		    print"\n")
	       else ()

           (* Look for the case of a nontrivial IL path (i.e., a
              structure variable with at least one projection).  If it
              already has been translated and its two parts given
              names, we just reuse these names. If the required name
              is specified, we'll define the two new variables with
              required names to be equal to the two variables bound
              previously (in which case the context must be extended).

              If the module is not a nontrivial path, then we pass
              everything to the main worker function xmod'.
           *)
(*
	   fun check_proj (m : Il.mod) : xmod_result =
	     (case (extractProjLabels m) of
		(Il.MOD_VAR v, _::_) =>
		 (* Nontrivial path *)
		 (case (lookup_module_alias(context,il_mod)) of
		    NONE => (if (!debug) then
			       (print "---lookup_module_alias failed to find ";
				Ppil.pp_mod il_mod;
				print "\n")
			     else ();
			     xmod' context args)
		  | SOME (name_c, name_r, con_r) =>
			let val (name_c,name_r,cbnd_cat,ebnd_cat,context) =
			  (case required_names of
			     NONE => (name_c,name_r,LIST[],LIST[],context)
			   | SOME(_, req_c, req_r) =>
			       let val context' =
				   update_NILctx_insert_kind_equation(context,
								      req_c,
								      name_c)
				   val context' = insert_con(context',
							     req_r,
							     NilRename.renameCon con_r)
			       in  (Var_c req_c, Var_e req_r,
				    LIST[Con_cb(req_c,name_c)],
				    LIST[Exp_b(req_r, TraceUnknown, name_r)],
				    context')
			       end)
			in {cbnd_cat=cbnd_cat, ebnd_cat=ebnd_cat,
			    name_c=name_c, name_r=name_r,
			    context = context}

			end)
	      | _ => xmod' context args)
*)
	   (* Compute the result, optionally checking the memoized_mpath
              memopad *)

	   val result = (* if (!do_memoize) then
	                   check_proj il_mod
			 else *)
              (case xmodpath context il_mod of
		   (* If il_mod is a path, then we don't have to let-bind each
		      component projection.  We can just phase-split into 3 paths.
		    *)
		   SOME (con_c,conopt_s,exp_r) =>
		       let
			   val (var,var_c,var_s,var_r,context) = 
			         chooseName(required_names,context)
			   val name_c = Var_c var_c
			   val name_s = Var_c var_s
			   val name_r = Var_e var_r
			   val cbnd_cat = SINGLETON(Con_cb(var_c, con_c))
			   val sbnd_cat = (case conopt_s of SOME con_s => SINGLETON(Con_cb(var_s, con_s))
			                      | NONE => NIL)
			   val ebnd_cat = SINGLETON(Exp_b(var_r, TraceUnknown, exp_r))
			   val context = insert_kind_equation(context,var_c,con_c)
			   val context = (case conopt_s of SOME con_s => insert_kind_equation(context,var_s,con_s)
			                     | NONE => context)
			   val context = insert_con(context,var_r,type_of context exp_r)
		       in
			   {cbnd_cat = cbnd_cat,
			    sbnd_cat = sbnd_cat,
			    ebnd_cat = ebnd_cat,
			    name_c   = name_c,
			    name_s   = name_s,
			    name_r   = name_r,
			    context  = context}
		       end
                   (* Otherwise, we phase-split the usual way. *)
		 | NONE => xmod' context args)

	     handle e => (if (!debug) then
			    (print ("Exception detected in call " ^
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
	    if (!debug) then
	      (print "Return ";
	       print (Int.toString this_call);
	       print " from xmod\n")
	    else ();
	    result
        end

   (* preproject.  Eagerly create, name, and translate all module-projections
         from a module.  The results will be stored in the memoized_mpath
	 so that any future module projection will be fast; furthermore,
         because the projections will be named, the translation of any
         path will be simply be a variable (or rather, two variables, one
         for each part).

	 The preproject function should be called as soon as a new module
	 is created, so that the named projections are in scope wherever
	 the original module was in scope.

         I'm not sure, but I suspect it may not be re-using the result
	 of phase-splitting M.a to phase-split M.a.b --- or vice
	 versa.
    *)
(*
   and preproject (var_arg : N.var (* The IL (unsplit) name of module
				      being projected from *),
		   con_only, il_signat, context) =
	let
	    (* Given a module, an accumulator list, and a signature for
	       that module, find all the valid projections from this module
               that return structures.
             *)
	    fun find_structure_paths (m : Il.mod) (acc : Il.mod list)
		(Il.SIGNAT_STRUCTURE( (Il.SDEC(l,Il.DEC_MOD(_,false,s))) ::
				      rest)) =
 		   if ((!elaborator_specific_optimizations) andalso
		       (N.is_dt l)) then
		       (* Ignore all modules marked as (inner) datatypes;
			  they're never accessed. *)
		       find_structure_paths m acc (Il.SIGNAT_STRUCTURE(rest))
		   else
                       (* Found a structure *)
		       let val acc = (Il.MOD_PROJECT(m,l))::acc
			   (* Recurse to get all the substructures *)
			   val acc = find_structure_paths (Il.MOD_PROJECT(m,l))
			                                  acc s
		       in
			 find_structure_paths m acc (Il.SIGNAT_STRUCTURE(rest))
		       end
	      | find_structure_paths m acc (Il.SIGNAT_STRUCTURE(_::rest)) =
		   find_structure_paths m acc (Il.SIGNAT_STRUCTURE(rest))
	      | find_structure_paths m acc _ = acc

	    val rev_paths : Il.mod list =
	      find_structure_paths (Il.MOD_VAR var_arg) [] il_signat

	    val _ =  if (!debug)
		     then (print "preproject: there are ";
			   print (Int.toString (length rev_paths));
			   print " paths\n")
		     else ()

            (* OK, now loop over all these paths and translate them.
             *)
	    fun folder (mpath : Il.mod, (cbnds,ebnds,context)) =
		let
		  (* Create a name for a fresh IL variable that reflects
		     the IL path to which it is being bound *)
		  fun loop (Il.MOD_VAR v) acc = (N.var2name v) ^ acc
		    | loop (Il.MOD_PROJECT (m,l)) acc =
		           loop m ("_" ^ (N.label2name l) ^ acc)
                    | loop _ _ = error "preproject/folder: non-path module"

		  val var = N.fresh_named_var(loop mpath "")

		  val ((var_c, var_r), context) = splitNewVar (var, context)

		  (* Actually phase-split the path *)
		  val {cbnd_cat, ebnd_cat, name_c, name_r, (*knd_c,*) context}=
		       xmod context (con_only, mpath,SOME(var,var_c,var_r))

                  (* Add the new bindings to the accumulator arguments *)
		  val cbnds = cbnd_cat :: cbnds
		  val ebnds = ebnd_cat :: ebnds

                  (* Remember the results of splitting this path *)
		  val context = add_module_alias(context, mpath,
						 name_c, name_r, type_of context name_r)
		in  (cbnds,ebnds,context)
		end

	    val (rev_cbnds,rev_ebnds,context) =
	      foldr folder ([],[],context) rev_paths

	in
	    {cbnd_cat = APPEND (rev rev_cbnds),
	     ebnd_cat = APPEND (rev rev_ebnds),
	     context = context}
	end
*)

   (* xmod'. The worker function for translating modules, called by xmod.
         Has the same inputs/outputs/preconditions/postconditions as
         xmod.

         Note that all recursive calls should go back to the xmod wrapper,
         instead of directly calling xmod' again.

	 con_only keeps a weaker Nil context that omits some term information,
	 for use when translating signatures.
    *)
   and xmod' context
             (il_mod as (Il.MOD_VAR var_mod), required_names) : xmod_result =
       let
           (* The splitting for this module variable should have already
              been determined at its binding site.
            *)
	   val ((var_mod_c, var_mod_s, var_mod_r), context) = splitVar (var_mod,context)
	   val _ = (* if con_only then () else *) mark_var_used(context,var_mod_r) 
	   val _ = mark_var_used(context,var_mod_c)

           val _ = if (!full_debug)
		       then (print "About to look up :\n";
			     Ppnil.pp_exp (Var_e var_mod_r);
			     print " and ";
			     Ppnil.pp_con (Var_c var_mod_c);
			     print "\n")
		   else ()

           (* If there was no specification as to what variables the
              compile-time parts and run-time parts should be bound to,
              we don't have to create any new bindings and can just return
              the previously determined variables.  Otherwise, we need
              to bind these two variables to new variables with the
              specified names.
            *)
	   val (name_c as Var_c name_c_var, name_s as Var_c name_s_var, name_r as Var_e name_r_var) =
	       (case required_names of
		    NONE => (Var_c var_mod_c, Var_c var_mod_s, Var_e var_mod_r)
		  | SOME (_, name_c, name_s, name_r) => (Var_c name_c, Var_c name_s, Var_e name_r))

	   val (cbnd_cat, sbnd_cat, ebnd_cat, context) =
	       (case required_names of
		    NONE => (LIST [], LIST [], LIST [], context)
		  | SOME (_, name_c, name_s, name_r) =>
		      let
			val context = update_NILctx_insert_kind_equation (context, name_c, Var_c var_mod_c)
			val context = insert_kind_equation (context, name_s, Var_c var_mod_s)
			val name_r_type = find_con(context, var_mod_r)
			val context = insert_con(context, name_r_var, name_r_type)
		      in
			  (LIST [Con_cb(name_c, Var_c var_mod_c)],
			   LIST [Con_cb(name_s, Var_c var_mod_s)],
			   LIST [Exp_b (name_r, TraceUnknown, Var_e var_mod_r)],
			   context)
		      end)

       in
	   {cbnd_cat = cbnd_cat,
	    sbnd_cat = sbnd_cat,
	    ebnd_cat = ebnd_cat,
            name_c   = name_c,
	    name_s   = name_s,
            name_r   = name_r,
	    context  = context}
       end

     | xmod' context (Il.MOD_APP(ilmod_fun, ilmod_arg), required_names) =
       let

	   (* Split the argument and the result
            *)
	   val {cbnd_cat = cbnd_cat_fun,
		sbnd_cat = sbnd_cat_fun,
		ebnd_cat = ebnd_cat_fun,
		name_c = name_fun_c,
		name_s = name_fun_s,
                name_r = name_fun_r,
		context = context
		} = xmod context (ilmod_fun, NONE)

	   val {cbnd_cat = cbnd_cat_arg,
		sbnd_cat = sbnd_cat_arg,
		ebnd_cat = ebnd_cat_arg,
		name_c = name_arg_c,
		name_s = name_arg_s,
		name_r = name_arg_r,
		context = context
		} = xmod context (ilmod_arg, NONE)

	   (* Figure out the names to which the two parts of the
	      application will be bound ... either fresh or as
              specified
            *)
	   val (var, var_c, var_s, var_r, context) =
	     chooseName (required_names, context)
	   val name_c = Var_c var_c
	   val name_s = Var_c var_s
	   val name_r = Var_e var_r

           (* Create the type part of the application *)
           val con_new = App_c(name_fun_c,[name_arg_c])

	   val cbnd_cat_app = LIST[Con_cb(var_c, con_new)]
	   val cbnd_cat = APPEND[cbnd_cat_fun, cbnd_cat_arg, cbnd_cat_app]

	   val sum_new = App_c(name_fun_s,[name_arg_c,name_arg_s])

	   val sbnd_cat_app = LIST[Con_cb(var_s, sum_new)]
	   val sbnd_cat = APPEND[sbnd_cat_fun, sbnd_cat_arg, sbnd_cat_app]

	   (* Update the context *)
	   val context = insert_kind_equation(context, var_c, con_new)

	   val context = insert_kind_equation(context,var_s,sum_new)

	   (* Create the term part of the application *)
	   val app_e = NilUtil.makeAppE name_fun_r [name_arg_c,name_arg_s] [name_arg_r] []
	   val ebnd_cat_app = LIST[Exp_b(var_r, TraceUnknown, app_e)]

	   val ebnd_cat = APPEND[ebnd_cat_fun,ebnd_cat_arg,ebnd_cat_app]

	   val result_type = type_of context app_e
	   val context = insert_con(context, var_r, result_type)

       in
	   {cbnd_cat  = cbnd_cat,
	    sbnd_cat  = sbnd_cat,
	    ebnd_cat  = ebnd_cat,
	    name_c    = name_c,
	    name_s    = name_s,
	    name_r    = name_r,
	    context  = context}
       end

     | xmod' context (Il.MOD_SEAL(il_mod,_), required_names) =
       (* The phase-splitting breaks abstraction since
          the code has already typechecked; just ignore any IL sealing.
        *)
       xmod context (il_mod, required_names)

     | xmod' context (Il.MOD_PROJECT (il_module,lbl), required_names) =
       let
	   (* If we got to this point then il_module.lbl is not a
	      path that can be found in the alias/mpath_memoize memopad
           *)
	   val _ = if ((!elaborator_specific_optimizations) andalso
		       (N.is_dt lbl)) then
                      error "Use of datatype labels detected"
		   else ()

	   val {cbnd_cat = cbnd_mod_cat,
		sbnd_cat = sbnd_mod_cat,
		ebnd_cat = ebnd_mod_cat,
		name_c   = name_mod_c,
		name_s   = name_mod_s,
		name_r   = name_mod_r,
		context  = context,
		...} = xmod context (il_module, NONE)

	   val (var_proj, var_proj_c, var_proj_s, var_proj_r, context) =
	       chooseName (required_names, context)
	   val name_c = Var_c var_proj_c
	   val name_s = Var_c var_proj_s
	   val name_r = Var_e var_proj_r

	   (* Type part *)
	   val con_proj_c = Proj_c(name_mod_c,lbl)
	   val cbnd_cat_proj = LIST [Con_cb(var_proj_c, con_proj_c)]
           val cbnd_cat = APPEND[cbnd_mod_cat,cbnd_cat_proj]

	   val con_proj_s = Proj_c(name_mod_s,lbl)
	   val sbnd_cat_proj = LIST [Con_cb(var_proj_s, con_proj_s)]
	   val sbnd_cat = APPEND[sbnd_mod_cat,sbnd_cat_proj]

	   (* Term part *)
	   val exp_proj_r = NU.makeSelect name_mod_r [lbl]
	   val ebnd_cat_proj = LIST [Exp_b(var_proj_r, TraceUnknown, exp_proj_r)]
	   val ebnd_cat = APPEND[ebnd_mod_cat, ebnd_cat_proj]

           (* Extend context *)
	   val context = insert_kind_equation(context,var_proj_c,con_proj_c)
	   val context = insert_kind_equation(context,var_proj_s,con_proj_s)
	   val context = insert_con(context, var_proj_r, type_of context exp_proj_r)

      in
	   {cbnd_cat = cbnd_cat,
	    sbnd_cat = sbnd_cat,
	    ebnd_cat = ebnd_cat,
            name_c   = name_c,
	    name_s   = name_s,
	    name_r   = name_r,
	    context  = context}
       end

     | xmod' context (Il.MOD_FUNCTOR(arrow, var_arg,
				     il_arg_signat, ilmod_body, ilmod_signat),
		      required_names) =
       let
	   (* Pick the name of the result *)
	   val (var_fun, var_fun_c, var_fun_s, var_fun_r, context) =
	       chooseName (required_names, context)
           val name_fun_c = Var_c var_fun_c
	   val name_fun_s = Var_c var_fun_s
	   val name_fun_r = Var_e var_fun_r

	   (* Split the argument parameter *)
	   val ((var_arg_c, var_arg_s, var_arg_r), context') =
	     splitNewVar (var_arg,context)
(*
	   val _ = clear_memo var_arg
*)
           (* Split the argument signature *)
	   val (knd_arg_c, knd_arg_s, con_arg_r) =
	         xsig context' (Var_c var_arg_c, Var_c var_arg_s, il_arg_signat)

	   (* Temporarily extend the context with the type part of the
	      argument variable
            *)
	   val context' = insert_kind(context', var_arg_c, knd_arg_c)
	   val context' = insert_kind(context', var_arg_s, knd_arg_s)
	   val context' = insert_con(context', var_arg_r, con_arg_r)

	   (* I think this comment is bogus (- Derek) :
              
	      Split the result signature (in the extended context).
              The App_c argument to xsig is really a dummy value.
              It plays no role in computing the type of the term part
              of the functor's result, which is used as the return type
              of the function that is the term part of the entire functor.
            *)
	   val (_,_,con_res) =
	     xsig context' (App_c(name_fun_c, [Var_c var_arg_c]),
			    App_c(name_fun_s, [Var_c var_arg_c, Var_c var_arg_s]),
			    ilmod_signat)

	   (* If we're doing preprojections, start the translation
              of the functor body by projecting out all the module
	      paths from the argument structure.
            *)
(*
           val (cbnd_preproject_cat, ebnd_preproject_cat, context') =
	       if (!do_preproject)
		   then let val {cbnd_cat = cbnd_preproject_cat,
				 ebnd_cat = ebnd_preproject_cat,
				 context = context'} =
		             preproject(var_arg,con_only,il_arg_signat,context')
			in (cbnd_preproject_cat, ebnd_preproject_cat, context')
			end
	       else (NIL, NIL, context')
*)

           (* Split the functor body *)
	   val {cbnd_cat = cbnd_body_cat,
		sbnd_cat = sbnd_body_cat,
		ebnd_cat = ebnd_body_cat,
		name_c = name_body_c,
		name_s = name_body_s,
		name_r = name_body_r,
		context = _
		} = xmod context' (ilmod_body, NONE)

           (* Prepend all the preprojections that the translation
              of the body is depending on.
            *)
(*
	   val cbnd_body_cat = APPEND[cbnd_preproject_cat, cbnd_body_cat]
	   val ebnd_body_cat = APPEND[ebnd_preproject_cat, ebnd_body_cat]
*)
           (* Translate the effect *)
	   val effect = xeffect arrow

           (* Turn the body definitions into lists *)
           val cbnds_body = flattenCatlist cbnd_body_cat
	   val sbnds_body = flattenCatlist sbnd_body_cat
           val ebnds_body = flattenCatlist ebnd_body_cat

           (* Create a binding for the type part of the functor *)
           val con_body_c = NU.makeLetC cbnds_body name_body_c

           val csbnds_body = cbnds_body @ sbnds_body

	   val con_body_s = NU.makeLetC csbnds_body name_body_s

           val cbnd_fun_cat =
	       SINGLETON(Open_cb(var_fun_c, [(var_arg_c, knd_arg_c)],
				 con_body_c))

           val sbnd_fun_cat =
	       SINGLETON(Open_cb(var_fun_s, [(var_arg_c, knd_arg_c),
					     (var_arg_s, knd_arg_s)],
				 con_body_s))

	   val con_fun_r = AllArrow_c{openness = Open, effect = effect,
				      tFormals = [(var_arg_c, knd_arg_c),
						  (var_arg_s, knd_arg_s)],
				      eFormals = [con_arg_r], fFormals = 0w0,
				      body_type = con_res}

	   val con_fun_name = Name.fresh_named_var (Name.var2name var_fun_r ^ "_type")

	   val con_fun_bnd = Con_cb(con_fun_name,con_fun_r)
	   (* Add this binding to the context *)

           (* Create a binding for the term part of the functor *)

	   val ebnd_fun_cat =
	       LIST[
		    Con_b(Runtime,con_fun_bnd),
		    Fixopen_b ([((var_fun_r, Var_c con_fun_name),
				 Function{recursive = NonRecursive,
					  effect = effect,
					  tFormals = [var_arg_c,var_arg_s],
					  eFormals = [(var_arg_r, TraceUnknown)],
					  fFormals = [],
					  body = (NU.makeLetE Sequential
						  ((map NU.makeConb csbnds_body) @ ebnds_body)
						  name_body_r)})])
		    ]

	   val context = insert_kind(context, var_fun_c, 
			   Arrow_k(Open, [(var_arg_c, knd_arg_c)], Single_k(con_body_c)))
	   val context = insert_kind(context, var_fun_s,
		           Arrow_k(Open, [(var_arg_c, knd_arg_c),
					  (var_arg_s, knd_arg_s)], Single_k(con_body_s)))
(*
	     handle e => (if (!debug) then
			    (print ("Exception detected in call " ^
				    " to xmod\n");
			     print "Is it valid?\n";
			      ((NilStatic.kind_valid (get_nilctxt context,Arrow_k(Open, [(var_arg_c, knd_arg_c),
					  (var_arg_s, knd_arg_s)], Single_k(con_body_s))); print "YES\n") handle f => print "NO\n");
			      print "\nwith context = \n";
			      print_splitting_context context;

			     print "\n")
			  else ();
			  raise e)
*)
	   val context = update_NILctx_insert_cbnd(context,con_fun_bnd)
	   val context = insert_con(context, var_fun_r, Var_c con_fun_name)
       in
	   {cbnd_cat = cbnd_fun_cat,
	    sbnd_cat = sbnd_fun_cat,
            ebnd_cat = ebnd_fun_cat,
	    name_c = name_fun_c,
	    name_s = name_fun_s,
	    name_r = name_fun_r,
	    context = context}
       end

     | xmod' context (Il.MOD_STRUCTURE sbnds, required_names) =
       let
           (* XXX is final_context =context ok, or should it be thrown away? *)

           (* We need to throw away most of the context returned by xsbnds.
	      The mpath memoization and HILctx contains variables now out of
	      scope, but we want to keep the nilCTX part because it
	      contains declarations for the cbnd_cat bindings.
            *)

	   val {final_context,
		cbnd_cat = cbnd_cat_bnds,
		sbnd_cat = sbnd_cat_bnds,
		ebnd_cat = ebnd_cat_bnds,
		record_c_con_items,
		record_s_con_items,
		record_r_exp_items} = xsbnds context sbnds

	   val context = replace_NILctx(context, get_nilctxt final_context)

           (* Get the name for the translated structure *)
	   val (var_str, var_str_c, var_str_s, var_str_r, context) =
	       chooseName (required_names, context)
           val name_str_c = Var_c var_str_c
	   val name_str_s = Var_c var_str_s
	   val name_str_r = Var_e var_str_r

           (* Type part *)
	   val con_str_c = Crecord_c record_c_con_items
	   val cbnd_cat_str = LIST [Con_cb(var_str_c, con_str_c)]
           val cbnd_cat = APPEND[cbnd_cat_bnds,cbnd_cat_str]

           (* Sum part *)
	   val con_str_s = Crecord_c record_s_con_items
	   val sbnd_cat_str = LIST [Con_cb(var_str_s, con_str_s)]
	   val sbnd_cat = APPEND[sbnd_cat_bnds,sbnd_cat_str]

           (* Term part *)
           val specialize =
	       (case (!elaborator_specific_optimizations, sbnds) of
		    (true, [Il.SBND(lab, Il.BND_EXP _)]) =>
		                    N.eq_label (lab, IlUtil.it_lab)
		  | _ => false)

	   val context = insert_kind_equation(context, var_str_c, con_str_c)
	   val context = insert_kind_equation(context, var_str_s, con_str_s)


	   val (context, ebnd_cat_str) =
	       if specialize then
		   (insert_con(context, var_str_r, type_of context (#2 (hd record_r_exp_items))),
		    SINGLETON(Exp_b (var_str_r, TraceUnknown, #2(hd record_r_exp_items))))
	       else
		 if null record_r_exp_items then
		   let
		     val r = NilDefs.unit_exp
		     val rtype = NilDefs.unit_con
		     val bnds = SINGLETON(Exp_b (var_str_r, TraceUnknown,r))
		     val context = insert_con(context, var_str_r, rtype)
		   in
		     (context, bnds)
		   end
		 else
		   let
		     val (labels,exps) = unzip record_r_exp_items
		       
		     val tvar = N.fresh_named_var "struct_gctag"
		       
		     val r = Prim_e (NilPrimOp (record labels),[], [], (Var_e tvar)::exps)
		     (* Note that exps will always be of the form (Var_e v)
		      *)
		     val (recbnds,rtype) = NU.makeNamedRecordType ((Name.var2name var_str_r)^"_type1") labels (map (type_of context) exps)
		       
		     val trs = map (fn _ => TraceUnknown) labels
		       
		     val bnds =
		       APPEND[LIST (map (fn cb => (Con_b (Runtime,cb))) recbnds),
			      LIST [Exp_b (tvar,TraceUnknown,
					   Prim_e(NilPrimOp mk_record_gctag, trs,[rtype],[])),
				    Exp_b (var_str_r, TraceUnknown,r)]]
		       
		     val context = update_NILctx_insert_cbnd_list(context,recbnds)
		     val context = insert_con(context,tvar,Prim_c(GCTag_c,[rtype]))
		     val context = insert_con(context, var_str_r, rtype)
		   in
		     (context, bnds)
		   end
		 
	   val ebnd_cat = APPEND[ebnd_cat_bnds,ebnd_cat_str]

       in
	   {cbnd_cat = cbnd_cat,
	    sbnd_cat = sbnd_cat,
	    ebnd_cat = ebnd_cat,
            name_c = name_str_c,
	    name_s = name_str_s,
	    name_r = name_str_r,
	    context = context}
       end

    | xmod' context (Il.MOD_LET (var_loc, il_loc_mod, il_body_mod),
		     required_names) =
       let
	   val ((var_loc_c, var_loc_s, var_loc_r), context) =
	     (* var_loc might be locally shadowing a previous variable
	        of the same name, but we still want new _c and _r
	        variable names
              *)
	     splitNewVar (var_loc, context)
(*
           val _ = clear_memo var_loc
*)
           (* Split the local part *)
	   val {cbnd_cat = cbnd_loc_cat,
		sbnd_cat = sbnd_loc_cat,
		ebnd_cat = ebnd_loc_cat,
		context = context,
		...} = xmod context (il_loc_mod,
				     SOME (var_loc, var_loc_c, var_loc_s, var_loc_r))

	   (* Split the body *)
	   val {cbnd_cat = cbnd_body_cat,
		sbnd_cat = sbnd_body_cat,
		ebnd_cat = ebnd_body_cat,
		name_c = name_let_c,
		name_s = name_let_s,
		name_r = name_let_r,
		context = context} =
	       xmod context (il_body_mod, required_names)

           (* Lets get flattened out, which is safe since everything
              is being named with fresh names so this scope extrusion
              can't cause accidental shadowing.  Everything thus remains
              in scope and all the extensions of the context are
              still useable.
            *)
           val cbnd_let_cat = APPEND[cbnd_loc_cat, cbnd_body_cat]
	   val sbnd_let_cat = APPEND[sbnd_loc_cat, sbnd_body_cat]
           val ebnd_let_cat = APPEND[ebnd_loc_cat, ebnd_body_cat]

       in
	   {cbnd_cat = cbnd_let_cat,
	    sbnd_cat = sbnd_let_cat,
	    ebnd_cat = ebnd_let_cat,
            name_c = name_let_c,
	    name_s = name_let_s,
	    name_r = name_let_r,
	    context = context}
       end
       (* End of xmod' *)

   (* xsbnds.  Translation of a sequence of IL structure bindings.

      Assume
         xsbnds in_context il_sbnds
      returns
         {cbnd_cat : conbnd catlist,
	  ebnd_cat : bnd catlist,
	  final_context : splitting_context,
	  record_c_con_items : (N.label * con) list,
	  record_r_exp_items : (N.label * exp) list}

      Let cbnds = flatten_catlist cbnd_cat
      and ebnds = flatten_catlist ebnd_cat
      and (labels, exps) = unzip record_r_exp_items

      Then

        (1) final_context is the, well, final context; it results from
            threading the context through the translation of all
            the il_sbnds.
        (2) If these sbnds were the contents of a structure then
            the type part of this structure would be
                LET_C cbnds IN  CRECORD_C(record_c_con_items) END
            and the run-time part of the contents would be
                LET_E cbnds @ ebnds IN
                   Prim_C(Record_C labels, tvar::exps)

            where tvar is the traceability value for those expressions.
    *)

    (* What does this comment mean?

      > Further, to determine the traceability of the exps without using
      > Typeof_c it seems necessary to return the types of those expressions.

    *)
   and xsbnds context (il_sbnds : Il.sbnd list) : xsbnds_result =
       let
	   (* Tracing messages on entry *)
	   val this_call = ! xsbnds_count
	   val _ =
	       if (!debug) then
		   (xsbnds_count := this_call + 1;
		    print "Call ";
		    print (Int.toString this_call);
		    print " to xsbnds\n";
		    if (!full_debug) then
		      (Ppil.pp_sbnds il_sbnds;
		       print ("\n"))
		    else ())
	       else ()

           (* Actually do the translation by calling xsbnds_rewrite_1
            *)
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
	    (* Tracing messages on return *)
	    if (!debug) then
               (print "Return ";
		print (Int.toString this_call);
		print " from xsbnds\n")
	    else ();
	    result
        end

   (* xsbnds_rewrite_1.  Phase 1 of rewriting for structure bindings.

      (The rewriting phases detect particular patterns of bindings
      generated by the elaborator, and may treat a binding or sequence
      of bindings specially, or even throw them away.  Thus, we cannot
      implement translation of the sbnds by mapping a function over
      each sbnd individually.)

      If the bindings start with an inner-datatype structure and the
      optimization flag is on, discard this binding and start over
      (from the xsbnds wrapper!) with the remaining bindings.
      Otherwise pass the bindings on the phase 2.
    *)
   and xsbnds_rewrite_1 (context : splitting_context) ([] : Il.sbnd list) =
       {final_context = context,
	cbnd_cat = NIL,
	sbnd_cat = NIL,
	ebnd_cat = NIL,
	record_c_con_items = nil,
	record_s_con_items = nil,
	record_r_exp_items = nil}

     | xsbnds_rewrite_1 context
                       (il_sbnds as (Il.SBND(lab, Il.BND_MOD _)) ::
                                     rest_il_sbnds) =
        if ((!elaborator_specific_optimizations)
	    andalso (N.is_dt lab)) then
	    xsbnds context rest_il_sbnds
        else
	    xsbnds_rewrite_2 context il_sbnds

     | xsbnds_rewrite_1 context
                       (il_sbnds as (Il.SBND(lab, _)) ::
			             rest_il_sbnds) =
        if (!elaborator_specific_optimizations)
	    andalso (N.eq_label(lab,IlUtil.ident_lab))
	then xsbnds context rest_il_sbnds
	else xsbnds_rewrite_2 context il_sbnds

   (* xsbnds_rewrite_2.  Phase 2 of rewriting for structure binidngs.

      The HIL has no primitive notion of bindings for mutually-recursive
      functions.  The elaborator must thus generate a "cluster" (essentially
      the functions as a tuple) immediately followed by a series of
      bindings giving names to all the projections from that cluster.

      If the optimization flag is on, turn these into
      mutually-recursive bindings at the NIL level.  The do_polyrec
      flag must also be on in order to perform this transformation on
      polymorphic mutually-recursive functions

    *)
   and xsbnds_rewrite_2 context
                        (il_sbnds as
			 Il.SBND(lbl,
				 Il.BND_EXP(top_var,
					    il_exp as Il.FIX(il_params as (_, _, fbnds))))
			 ::rest_il_sbnds) =
       (if ((!elaborator_specific_optimizations)
	    andalso (N.is_cluster lbl)) then
	   (* Definition of monomorphic recursive functions *)
	   let
               (* From the cluster we can tell how many projections from
                  this cluster follow its definition. *)
	       val num_functions = length fbnds

	       (* external_labels = Exported labels for these functions.
                  external_vars = Variables to which the functions are being
		                  bound in (skipped) IL bindings
                  rest_il_sbnds' = remaining il_sbnds after this
                                   cluster of functions and the projections
                *)
	       val (rest_il_sbnds', external_labels, external_vars) =
		   getSbndNames num_functions rest_il_sbnds

	       (* internal_renamed_vars =
                      Variables to which the functions are bound
	              in the literal translation of the cluster, and by
                      which their translations refer to each other, and
                      which will be used as the names in the
                      recursive bindings being returned.  (They're
                      fresh variables, as well.)

		  nil_functions = functions in the literal translation
                                  of the clusteris cluster
                *)
	       val (ftbnds, nil_fn_seq) =
		   let
		       val (ftbnds, (Fixopen_b nil_fn_set)::_, _) = xfix context il_params
		   in
		       (ftbnds, nil_fn_set)
		   end

               val (internal_renamed_pairs, nil_functions) =
		 Listops.unzip (Sequence.toList nil_fn_seq)
	       val internal_renamed_vars = map #1 internal_renamed_pairs

               (* Remember for the purposes of translating later
                  sbnds that references to these functions (via
                  variables in external_vars) should use the names
                  appearing in the returned NIL bindings.
                *)
	       val context = insert_given_vars(external_vars,
					       internal_renamed_vars,
					       context)
	       val context = update_NILctx_insert_cbnd_list (context,ftbnds)
	       val context = foldl (fn ((v, c), context) => insert_con(context, v, c)) context internal_renamed_pairs

               (* The translation of the initial cluster binding and all
                  the following projections is this single binding
                *)
	       val ebnd = Fixopen_b nil_fn_seq

               (* Continue on with the remaining bindings, but with the
                  xsbnds wrapper function.
                *)
	       val {final_context, cbnd_cat, sbnd_cat, ebnd_cat, record_c_con_items,
		    record_s_con_items,record_r_exp_items} =
		 xsbnds context rest_il_sbnds'

	   in
	       {final_context = final_context,
		cbnd_cat = cbnd_cat,
		sbnd_cat = sbnd_cat,
		ebnd_cat = APPEND[LIST (map NU.makeConb ftbnds), SINGLETON ebnd, ebnd_cat],
		record_c_con_items = record_c_con_items,
		record_s_con_items = record_s_con_items,
		record_r_exp_items = (* If this is in a module, then
                                        all of the functions within
                                        the nest become components of
                                        the corresponding record
                                      *)
		                     (Listops.zip external_labels
				         (map Var_e internal_renamed_vars))
		                     @ record_r_exp_items}
	   end
	else
	  (* The first binding is a FIX, but either it's not a cluster
             or we've turned off this optimization.  Translate it directly
             by going to phase 3.
           *)
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
					   (_, il_exp as Il.FIX(il_params as (is_recur, _,
								fbnds))))],
				  il_body_signat)))
			 :: rest_il_sbnds) =

       if ((!do_polyrec)
           andalso (!elaborator_specific_optimizations)
	   andalso (N.is_label_internal lbl)
           andalso (not (N.eq_label (lbl, IlUtil.expose_lab)))
	   andalso (N.eq_label (them_lbl, IlUtil.them_lab))
	   andalso (not (N.is_eq lbl))
           ) then

	   (* Same as the previous case, but now we're doing polymorphic
	      function clusters.

	      XXX:  Is this a good idea?  With polymorphic recursion,
                    recursive calls actually are two calls --- one for
                    polymorphic instantiation and one to actually call
                    the resulting function.

                    On the other hand, leaving the code as is can
                    result in an n-fold increase in the number of
                    closures allocated at run-time (where n is the
                    number of mutually-recursive functions) because
                    each instantiation of a single function actually
                    creates closures for all of the functions.
           *)
	     let
(*
               val _ = clear_memo top_var
               val _ = clear_memo poly_var
*)
	       (* external_labels = Exported labels for these functions.
                  external_vars = Variables to which the functions are being
		                  bound in (skipped) IL bindings
                  rest_il_sbnds' = remaining il_sbnds after this
                                   cluster of functions and the projections
                *)
	       val num_functions = length fbnds
	       val (rest_il_sbnds', external_labels, external_vars) =
		   getSbndNames num_functions rest_il_sbnds

               (* Split the signature of the polymorphic argument structure.
                  Remember that because of equality polymorphism, there
                  may be value specs as well as type specs in this
                  signature
                *)

               (* The poly_var_s variable here is a dummy, it should never be
                  used (i.e. referred to) in con_arg. - Derek *)
	       val ((poly_var_c, poly_var_s, poly_var_r), context') =
		                       splitNewVar (poly_var, context)
	       val (knd_arg, _, con_arg) =
		      xsig context' (Var_c poly_var_c, Var_c poly_var_s, il_arg_signat)

	       (* Translate the cluster of functions.
                *)
	       val context' = update_NILctx_insert_kind(context', poly_var_c,
							knd_arg)
	       val context' = insert_con(context', poly_var_r, con_arg)

	       (*val Let_e (_, (Fixopen_b set)::_, _) = xexp context' il_exp*)
	       val (ftbnds, (Fixopen_b set)::_, _) = xfix context' il_params

	       (* internal_vars = Variables to which the functions are bound
                                  and by which they refer to each other
	                          in the direct translation of the HIL cluster.
		  functions = Bodies of the functions in this
		                 mutually-recursive group *)
               val (internal_pairs, functions) =
		   Listops.unzip set
	       val (internal_vars, ebnd_types) =
		   unzip internal_pairs

               (* Each function definition will become a curried function,
                     first taking the polymorphic parameters and then
                     taking the function arguments.
                 inner_vars = the names of these "inner" functions, the
                     ones returned when the outer function is instantiated.
                *)
               val inner_vars =
		 map (fn v => N.fresh_named_var((N.var2name v) ^ "_inner"))
		     external_vars

               (* external_var_rs = The _r parts of the external_var variables,
                                    which will be used by later parts of the
                                    code to refer to these functions.

                  These will be the variable names of the "outer" functions
                     and will also be used by each function to refer
                     to the other functions (which is the idea of
                     polymorphic recursion).
                *)

               val (external_var_rs, context) =
		   let
		       fun folder (v,context) =
			   let
			       val ((_,_,v_r),context) = splitNewVar (v,context)
			   in
			       (v_r, context)
			   end
		   in
		       Listops.foldl_acc folder context external_vars
		   end

	       (* Wrap the function body e with bindings that
                  define each of the names it is currently using to
                  refer to the other functions
                  to be the result of polymorphically-instantiatiating
                  the corresponding outer function.
                *)
	       fun wrap(current_internal_var, inner_var, e) =
		   let
		       fun mapper(internal_var,external_var_r) =
			 if (N.eq_var(internal_var,current_internal_var)) then
			   (* Since the original code couldn't have been
			      polymorphic recursive, a direct recursive
			      call of a function to itself can still go
			      directly to the inner (non-polymorphic) name,
			      rather than re-instantiating itself.
			    *)
			   Exp_b(internal_var, TraceUnknown, Var_e inner_var)
			 else
			   Exp_b(internal_var, TraceUnknown,
				 NU.makeAppE
				    (Var_e external_var_r)
				    [Var_c poly_var_c]
				    [Var_e poly_var_r]
				    [])
		       val bnds = Listops.map2 mapper
			           (internal_vars, external_var_rs)
		   in
		     NU.makeLetE Sequential bnds e
		   end

	       val ftbnds_subst = 
		 let fun folder (cb,s) = let val (v,c) = NU.extractCbnd cb
					 in NS.C.addr (s,v,c)
					 end
		 in foldl folder (NS.C.empty()) ftbnds
		 end

	       val ftbnds_context = update_NILctx_insert_cbnd_list(context,ftbnds)


               (* Rewrite each function into this curried outer/inner
                * function pair.
                *)
               fun reviseFunction ((internal_var,
				   external_var_r, inner_var, inner_type,
				   Function{effect,recursive,
					    tFormals = [],
					    eFormals = [(arg_var,arg_tr)],
					    fFormals = [],
					    body}), context) =
		   let
		     val {body_type = inner_body_type,
			  eFormals = [arg_con],
			  tFormals = [], fFormals = 0w0, ...} = strip_arrow_norm ftbnds_context inner_type

		     val body' = wrap(internal_var, inner_var, body)
		       
		     (* I could build a let here.  But these are the bnds for the whole cluster,
		      * most of which are extraneous for this type.  The only time that they are likely
		      * to all be relevant, is when there is a single function (an important special case
		      * because of functors).
		      *)

		     val closed_inner_body_type = (if num_functions = 1 then NU.makeLetC ftbnds else NS.substConInCon ftbnds_subst) inner_body_type
		     val closed_arg_con = NS.substConInCon ftbnds_subst arg_con
		       
		     val closed_outer_body_type = AllArrow_c{openness = Open, effect = effect,
							     tFormals = [],
							     eFormals = [closed_arg_con],
							     fFormals = 0w0,
							     body_type = closed_inner_body_type}

		     val outer_type = AllArrow_c{openness = Open, effect = Total,
						 tFormals = [(poly_var_c, knd_arg)], 
						 eFormals = [con_arg],
						 fFormals = 0w0, 
						 body_type = closed_outer_body_type}

		       
		     val outer_ftype = Name.fresh_named_var (Name.var2name external_var_r ^ "_type")

		     val context = insert_kind_equation(context, outer_ftype, outer_type)
		     val context = insert_con(context,external_var_r,Var_c outer_ftype)
		     val bnd = Con_b(Compiletime, Con_cb(outer_ftype, outer_type))
		     val vc = (external_var_r, Var_c outer_ftype)
		     val f = Function{effect = Total,
				      recursive = Leaf,
				      tFormals = [poly_var_c],
				      eFormals = [(poly_var_r, TraceUnknown)],
				      fFormals = [],
				      body = 
				      Let_e (Sequential,
					     (map NU.makeConb ftbnds)@
					     [Fixopen_b
					      [((inner_var, inner_type),
						Function{effect=effect,recursive=recursive,
							 tFormals = [],
							 eFormals = [(arg_var,arg_tr)],
							 fFormals = [],
							 body = body'})
					       ]
					      ],
					     Var_e inner_var
					     )
				      }
		     val vcf = (vc,f)
		   in  ((bnd,vcf),context)
		   end

               val (ebnd_entries, context) = foldl_acc reviseFunction context
				   (Listops.zip5 internal_vars external_var_rs inner_vars ebnd_types functions)

	       val (outer_bnds, ebnd_entries) = Listops.unzip ebnd_entries

	       val ebnds = outer_bnds @ [Fixopen_b ebnd_entries]

               val context = update_polyfuns_list(context, external_var_rs)

	       (* Translate the remaining bindings
		*)
	       val {final_context, cbnd_cat, sbnd_cat, ebnd_cat, record_c_con_items,
		    record_s_con_items,record_r_exp_items} = xsbnds context rest_il_sbnds'

	   in
	       {final_context = final_context,
		cbnd_cat = cbnd_cat,
		sbnd_cat = sbnd_cat,
		ebnd_cat = APPEND [LIST ebnds, ebnd_cat],
		record_c_con_items = record_c_con_items,
		record_s_con_items = record_s_con_items,
		record_r_exp_items = (Listops.zip external_labels
				                  (map Var_e external_var_rs))
		                     @ record_r_exp_items}
	   end
       else
	   xsbnds_rewrite_3 context il_sbnds

     | xsbnds_rewrite_2 context il_sbnds = xsbnds_rewrite_3 context il_sbnds

   (* xsbnds_rewrite_3.  Phase 3 of translation for structure bindings.

      None of the special cases matched, so translate the first binding
      and recursively (via the xsbnds wrapper) translate the rest.
    *)

   and xsbnds_rewrite_3 context
                        (Il.SBND(lbl, Il.BND_EXP(var, il_exp)) ::
			 rest_il_sbnds) =
       (* Translation of a binding for an expression. *)
       let
	   val exp = xexp context il_exp

	   val (var', context') = insert_rename_var (var, context)

	   val tp = type_of context exp

	   val context' = update_NILctx_insert_con (context', var', tp)

	   val {final_context, cbnd_cat, sbnd_cat, ebnd_cat, record_c_con_items,
		record_s_con_items, record_r_exp_items} = xsbnds context' rest_il_sbnds
       in
	   {final_context = final_context,
	    cbnd_cat = cbnd_cat,
	    sbnd_cat = sbnd_cat,
	    ebnd_cat = CONS(Exp_b(var',TraceUnknown, exp), ebnd_cat),
	    record_c_con_items = record_c_con_items,
	    record_s_con_items = record_s_con_items,
	    record_r_exp_items = (lbl, Var_e var') :: record_r_exp_items}
       end

     | xsbnds_rewrite_3 context
                        (Il.SBND(lbl, Il.BND_CON(var, il_con)) ::
			 rest_il_sbnds) =
       (* Translation of a binding for a type *)
       let
	   val _ = if !debug then
	       (print "Bind constructor ";
		Ppil.pp_var var;
		print " to ";
		Ppil.pp_con il_con;
		print "\n")
		   else ()

	   val con = xcon context il_con

	   val (var',context') = insert_rename_var (var, context)

	   val reset = case il_con
			 of Il.CON_TYVAR tv =>
			   let
			     val il_con = derefTyvar tv
			     val _ = setTyvar tv (Il.CON_VAR var) ;
			   in fn () => setTyvar tv il_con
			   end
			  | _ => fn () => ()

           val context'' = update_NILctx_insert_kind_equation(context',
							      var', con)

	   val {final_context, cbnd_cat, sbnd_cat, ebnd_cat, record_c_con_items,
		record_s_con_items, record_r_exp_items} = xsbnds context'' rest_il_sbnds

	   val _ = reset()

           (* This is where we decide which types get put in the "c" part and which
              ones get put in the "s" part. -Derek *)
	   val (cbnd_cat, record_c_con_items, sbnd_cat, record_s_con_items) =
	       if N.is_sum lbl
	       then (cbnd_cat, record_c_con_items,
		     CONS(Con_cb(var',con), sbnd_cat), (lbl, Var_c var')::record_s_con_items)
	       else (CONS(Con_cb(var',con),cbnd_cat), (lbl, Var_c var')::record_c_con_items,
		     sbnd_cat, record_s_con_items)

       in
	   {final_context = final_context,
	    cbnd_cat = cbnd_cat,
	    sbnd_cat = sbnd_cat,
	    ebnd_cat = ebnd_cat,
	    record_c_con_items = record_c_con_items,
	    record_s_con_items = record_s_con_items,
	    record_r_exp_items = record_r_exp_items}
       end

     | xsbnds_rewrite_3 context
                       (Il.SBND(lbl, Il.BND_MOD(var, false, il_module))
			::rest_il_sbnds) =
       (* Translation of a binding that is not a functor resulting
          in the translation of a polymorphic function. *)
       let
(*
           val _ = clear_memo var
*)

           (* The elaborator is not supposed to duplicate variables,
              but occasionally this happens (and the phase-splitter's
              scope extrusion may put variables that used to
              have disjoint scopes into overlapping scopes).

              Thus, apparently because we're keeping the *unsplit*
              variable name associated with path definitions in the
              alias part of the splitting context, and we don't want
              unrelated paths to be associated with the same variable
              name, we rename the variable in the IL code before
              processing the binding.
            *)

         (*
             If the elaborator never produces variable bindings that shadow
             earlier ones, I don't see how the SOME case here could ever arise.
             So I've turned that case into an error.
              - Derek
          *)
(*
	   val (var,rest_il_sbnds) =
	       (case lookupVmap (var,context) of
		    NONE => (var,rest_il_sbnds)
		  | SOME _ =>
			let val _ = (print ("WARNING (xsbnds/BND_MOD):  " ^
					"Compensating for duplicate variable");
				     Ppnil.pp_var var;
				     print "\n";
				     error "Error in xsbnds_rewrite_3")
			    val v = N.derived_var var
			    val subst = IlUtil.subst_add_modvar(IlUtil.empty_subst, var, Il.MOD_VAR v)
			    val Il.MOD_STRUCTURE rest' =
				IlUtil.mod_subst(Il.MOD_STRUCTURE rest_il_sbnds,subst)
			in (v,rest')
			end)
*)
	   val ((var_c, var_s, var_r), context) = splitNewVar (var,context)

           (* Split the right-hand side of the binding *)
	   val {cbnd_cat = cbnd_mod_cat,
		sbnd_cat = sbnd_mod_cat,
		ebnd_cat = ebnd_mod_cat,
		context = context,
		name_c, name_s, name_r,
		...} = xmod context (il_module, SOME(var, var_c, var_s, var_r))

	   (* If the module being bound is a path, then remember that
	      the IL variable and IL path were defined to be aliases.
	    *)
(*
	   val context =
	     (case (extractProjLabels il_module) of
		(Il.MOD_VAR tovar,labs) =>
		    add_modvar_alias(context,var,(tovar,labs))
	      | _ => context)
*)

	   (* Do the rest of the bindings *)
	   val {final_context, cbnd_cat, sbnd_cat, ebnd_cat, record_c_con_items,
		record_s_con_items, record_r_exp_items} = xsbnds context rest_il_sbnds
       in
	   {final_context = final_context,
	    cbnd_cat = APPEND[cbnd_mod_cat, cbnd_cat],
	    sbnd_cat = APPEND[sbnd_mod_cat, sbnd_cat],
	    ebnd_cat = APPEND[ebnd_mod_cat, ebnd_cat],
	    record_c_con_items = (lbl, name_c) :: record_c_con_items,
	    record_s_con_items = (lbl, name_s) :: record_s_con_items,
	    record_r_exp_items = (lbl, name_r) :: record_r_exp_items}
       end

     | xsbnds_rewrite_3 context (Il.SBND(lbl,Il.BND_MOD(var, true, il_polymod))
				 :: rest_il_sbnds) =
       (* Translation of functor resulting from the translation of a
          polymorphic function.  This is not redundent, because code like
                 val f = SOME
          creates a polymorphic definition that isn't a cluster caught
          in xsbnds_rewrite_2.
        *)
       let
           (* Unfortunately, the HIL may duplicate variables, and the flattening
              of modules may put duplicates that used to have disjoint scopes
              into overlapping scopes. *)
           (* See my comment for the previous case. - Derek *)

(*
	   val (var,rest_il_sbnds) =
	       (case lookupVmap(var,context) of
		    NONE => (var,rest_il_sbnds)
		  | SOME _ =>
			let val _ = (print ("WARNING (xsbnds/BND_MOD):  " ^
					    "Duplicate variable found:");
				     Ppnil.pp_var var;
				     print "\n";
				     error "Error in xsbnds_rewrite_3")
			    val v = Name.derived_var var
			    val subst = IlUtil.subst_add_modvar(IlUtil.empty_subst, var, Il.MOD_VAR v)
			    val Il.MOD_STRUCTURE rest' =
				IlUtil.mod_subst(Il.MOD_STRUCTURE rest_il_sbnds,subst)
			in (v,rest')
			end)
*)

           val ((_, _, var_r), context) = splitNewVar(var, context)

           val (context, bnd) = xpolymod context (var_r, il_polymod)

           val context = update_polyfuns (context, var_r)

	   val {final_context, cbnd_cat, sbnd_cat, ebnd_cat, record_c_con_items,
		record_s_con_items, record_r_exp_items} = xsbnds context rest_il_sbnds

	   val _ =
	       if !debug then
		   (print "Poly label is ";
		    Ppil.pp_label lbl;
		    print "\n")
	       else ()
       in
	   {final_context = final_context,
	    cbnd_cat = cbnd_cat,
	    sbnd_cat = sbnd_cat,
	    ebnd_cat = CONS(bnd, ebnd_cat),
	    record_c_con_items = record_c_con_items,
	    record_s_con_items = record_s_con_items,
	    record_r_exp_items = (lbl, Var_e var_r) :: record_r_exp_items}
       end

   (* Translate functors used in encoding polymorphism other than
      those which return clusters of functions
    *)
   and xpolymod context
                (v_r,  Il.MOD_FUNCTOR(arrow, poly_var, il_arg_signat, il_body,
				      il_result_sig as Il.SIGNAT_STRUCTURE _))=
         let
             (* This case catches code that elaboration has essentially been
                wrapped with type abstraction/application, e.g., where
                   datatype ('a, 'b) foo = FOO
                   val 'a x : ('a,'a) foo = FOO
                becomes
                   x : = /\a. FOO[a,a]
                represented as a functor.

                Essentially it's just like a translation of a normal
                functor except that we know the result has no compile-time
                part that we ever care about.
              *)
(*
	     val _ = clear_memo poly_var
*)
(*	     val _ = (print "xpolymod binding for "; Ppil.pp_var v_r; print "\n") *)

             (* poly_var_s is a dummy variable, which should not be referred to
                in arg_type. - Derek *)
	     val ((poly_var_c, poly_var_s, poly_var_r), context') =
	       splitNewVar (poly_var, context)

	     val (knd_arg, _, arg_type) =
		 xsig context' (Var_c poly_var_c, Var_c poly_var_s, il_arg_signat)

             val effect = xeffect arrow

	     val context' = update_NILctx_insert_kind(context', poly_var_c, knd_arg)
	     val context' = insert_con(context', poly_var_r, arg_type)

             val il_body_structure =
		 (case il_body of
		      Il.MOD_STRUCTURE _ => il_body
		    | _ => (* e.g., if the body of the SML source function
                                    was a polymorphic instantiation *)
                           Il.MOD_STRUCTURE
			     [Il.SBND(IlUtil.it_lab,
				      Il.BND_EXP(N.fresh_var(),
						 Il.MODULE_PROJECT
						   (il_body, IlUtil.it_lab)))])

	     val {ebnd_cat = ebnd_cat, name_r = name_r,...} =
		 xmod context' (il_body_structure, NONE)

             val exp = NU.makeLetE Sequential (flattenCatlist ebnd_cat) name_r

	     val dummy_con = Var_c (N.fresh_var())
             val (_,_,con) = xsig context' (dummy_con, dummy_con, il_result_sig)

	     val v_t = AllArrow_c {effect=effect, openness=Open,
				   tFormals=[(poly_var_c, knd_arg)], eFormals=[arg_type], fFormals=0w0,
				   body_type=con}

	     val context' = insert_con(context', v_r, v_t)
	 in
	     (context', Fixopen_b (Sequence.fromList
			[((v_r, v_t),
			  Function{effect=effect, recursive=Leaf,
				   tFormals = [poly_var_c],
				   eFormals = [(poly_var_r, TraceUnknown)],
				   fFormals = [],
				   body = exp})]))
	 end

     | xpolymod context (v_r, il_mod) =
	 (* Catches the case in which there is no wrapping, just one
             module variable representing a polymorphic function is
             being defined by another variable or path representing
             a polymorphic function, e.g., in source code like
	       val x = SOME
          *)
         (case extractProjLabels il_mod of
	      (Il.MOD_VAR v', lbls) =>
		  let
		      val ((_,_,v'_r),_) = splitVar(v',context)
		      val _ = mark_var_used(context,v'_r)
		      val e = selectFromRec(Var_e v'_r, lbls)
		      val context = insert_con(context, v_r, type_of context e)
		  in
		      (context, Exp_b(v_r, TraceUnknown,e))
		  end
            | _ => (print "xpolymod: bad module argument\n";
                    Ppil.pp_mod il_mod;
                    error "xpolymod: bad module argument"))


   (* xflexinfo.  Translator for CON_FLEXRECORD
    *)
   and xflexinfo context (ref (Il.INDIRECT_FLEXINFO f)) =
          xflexinfo context f
     | xflexinfo context (ref (Il.FLEXINFO(_,true, recs))) =
          let
	    val (lbls, cons) = xrdecs context recs
	    val con = Prim_c(Record_c lbls, cons) (* already sorted *)
	  in
	    con
	  end
     | xflexinfo _ _ = error "xflexinfo found FLEXINFO that wasn't finalized"

   (* xrdecs.   Translation of a series of label/type pairs as
                would be found within a record type.

                Returns its result as a list of labels and a list
                of (translated) types.
    *)
   and xrdecs context [] = ([], [])
     | xrdecs context ((lab, il_con) :: rest) =
       let
	   val (labs, cons) = xrdecs context rest
	   val con = xcon context il_con
       in
	   (lab :: labs, con :: cons)
       end

   (* xcon.  Wrapper function for the translation of IL types *)
   and xcon context il_con : con  =
       let
	   (* Tracing messages on entry
            *)
	   val this_call = ! xcon_count
	   val _ =
	       if (!debug) then
		   (xcon_count := this_call + 1;
		    print ("Call " ^ (Int.toString this_call) ^ " to xcon\n");
		    if (!full_debug) then (Ppil.pp_con il_con; print"\n") else ())
	       else ()

           (* Try to use the memoizing of IL constructor paths.
	      If not a path, or if do_memoize is off, then pass
              the type to the work function xcon'.
            *)
(*
	   fun check_proj(Il.MOD_VAR v,ls) =
	       let
		   val ((v_c,_),_) = splitVar (v,context)
	       in
		   mark_var_used(context, v_c);
		   lookup_con_memo (v,ls) (fn()=> xcon' context il_con)
	       end
	     | check_proj(Il.MOD_PROJECT(m,l),ls) = check_proj(m,l::ls)
	     | check_proj _ = xcon' context il_con
*)
	   val result = (* (case (!do_memoize,il_con) of
			     (true, Il.CON_MODULE_PROJECT(m,l)) =>
			       check_proj(m,[l])
			   | _ => *)
	       xcon' context il_con
	       handle e => (if (!debug) then
			      (print ("Exception detected in call " ^
				      (Int.toString this_call) ^ " to xcon: ");
			       Ppil.pp_con il_con;
			       print "\n")
			    else ();
			    raise e)

	in
	   (* Tracing messages on return
            *)
	    if (!debug) then
	      print ("Return " ^ (Int.toString this_call) ^
		     " from xcon\n")
	    else ();
	    result
        end

   (* xcon'.  The worker function for actually translating IL types *)
   and xcon' context (il_con as (Il.CON_VAR var)) : con =
       let
	   (* The IL variable var had a renaming chosen whereever it
              was bound; apply this renaming *)
	   val var' = rename_var(var, context)

           val _    = mark_var_used (context, var')
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

       (* There is no type distinction between signed/unsigned ints
          from NIL onwards.
       *)
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
     | xcon' context (Il.CON_INTARRAY sz) = Prim_c (IntArray_c sz, [])
     | xcon' context (Il.CON_INTVECTOR sz) = Prim_c (IntVector_c sz, [])
     | xcon' context (Il.CON_FLOATARRAY sz) = Prim_c (FloatArray_c sz, [])
     | xcon' context (Il.CON_FLOATVECTOR sz) = Prim_c (FloatVector_c sz, [])
     | xcon' context (Il.CON_ANY) =
       let
	   val con = Prim_c(Exn_c, [])
       in
	   con
       end

     | xcon' context (Il.CON_REF il_con) =
       let
	 val con' = xcon context il_con
       in 
	 if !ref_is_array then
	   Prim_c (Array_c, [con'])
	 else
	   Prim_c (Ref_c, [con'])
       end

     | xcon' context (Il.CON_TAG il_con) =
       let
	   val con' = xcon context il_con
	   val con = Prim_c (Exntag_c, [con'])
       in  con
       end

     | xcon' context (Il.CON_ARROW (il_cons1, il_con2, closed, arr)) =
       let
           fun translate il_con =
	     (case (closed,il_con) of
                (* Normally xcon translates F64 to BoxedFloat, but
                   we don't want to do this when describing the types
		   of external functions.
                 *)
		(true,Il.CON_FLOAT Prim.F64) => Prim_c (Float_c Prim.F64, [])
	      | _ => xcon context il_con)

	   val cons1 = map translate il_cons1

           val con2 = translate il_con2

	   val eff = xeffect (derefOneshot arr)
	   val con = if closed
			 then ExternArrow_c(cons1, con2)
		     else AllArrow_c{openness = Open, effect = eff,
				     tFormals = [],
				     eFormals = cons1,
				     fFormals = 0w0, body_type = con2}
       in
	   con
       end

     | xcon' context (il_con as Il.CON_APP (il_con1, il_cons2)) =
       let
	   val con1 = xcon context il_con1
           val cons2 = map (xcon context) il_cons2
	   val con = App_c(con1, cons2)
       in  con
       end

     | xcon' context (Il.CON_MU(Il.CON_FUN(vars,
					   Il.CON_TUPLE_INJECT cons))) =
       let
	   val (vars',context') = insert_rename_vars(vars, context)

	   val context'' = context'
(*
	     update_NILctx_insert_kind_list(context',
					    map (fn v => (v,Type_k)) vars')
*)
	   val cons'= map (xcon context'') cons

	   val freevars = foldl N.VarSet.union N.VarSet.empty
	                  (map IlUtil.con_free cons)
	   val is_recur' = Listops.orfold (fn v => N.VarSet.member(freevars,v))
	                                 vars
	   val is_recur = List.exists (var_is_used context) vars'

	   val _ = if is_recur <> is_recur then error "is_recur disagrees!" else ()

	   val con = Mu_c (is_recur,
			   Sequence.fromList (Listops.zip vars' cons'))

       in
	   con
       end

     | xcon' context (Il.CON_MU(Il.CON_FUN([var], con))) =
       let
	   val (var',context') = insert_rename_var(var, context)
	   val context''= context' (* update_NILctx_insert_kind(context', var', Type_k) *)

	   val con' = xcon context'' con
	   val freevars = IlUtil.con_free con
	   val is_recur' = N.VarSet.member(freevars,var)

	   val is_recur = var_is_used context var'

	   val _ = if is_recur <> is_recur then error "is_recur disagrees2!" else ()

	   val con = Mu_c (is_recur,Sequence.fromList [(var', con')])
       in
	   con
       end

     | xcon' context (Il.CON_RECORD rdecs) =
       let
	   val (lbls, cons) = xrdecs context rdecs
	   val con = Prim_c (Record_c lbls, cons)
       in
	   con
       end

     | xcon' context (Il.CON_FUN (vars, il_con1)) =
       let
	   val (vars', context') = insert_rename_vars(vars, context)
           val args = map (fn v => (v,Type_k)) vars'

	   val context'' = context' (* update_NILctx_insert_kind_list(context',args) *)

	   val con1 = xcon context'' il_con1

	   val fun_name = N.fresh_var ()
	   val con = NU.makeLetC [Open_cb(fun_name, args, con1)]
			       (Var_c fun_name)
       in  con
       end

     | xcon' context (Il.CON_SUM {names, carrier, noncarriers, special}) =
       let
	   val known = (case special of
			       NONE => NONE
			     | SOME i => SOME (Word32.fromInt i))
	   val carrier_con = xcon' context carrier

           (* I changed the implementation here so that xcon will work even if
              the given constructor is not well-formed in the input context.
              This is because the rather slick implementation of xsig/xsdecs
	      wants to avoid adding things to the context.  -Derek *)

	   val num_carriers = length(names) - noncarriers

(*
     This would be another way of calculating it...
	   val num_carriers = (case carrier_con of
				   Crecord_c lblcons => length lblcons
				 | _ => 1)
*)
(*
     This was the original way of calculating it...
	   val num_carriers = (case NilContext_kind_of(context, carrier_con) of
				   (Record_k seq) => length(Sequence.toList seq)
				 | Type_k => 1
                                 | SingleType_k _ => 1
				 | _ => error "CON_SUM: cannot have \
                                              \non-record and non-word kind")
*)
	   val con = Prim_c (Sum_c {tagcount = Word32.fromInt noncarriers,
				    totalcount = Word32.fromInt(noncarriers +
								num_carriers),
				    known = known},
			     [carrier_con])
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
(* 	 val context = update_NILctx_insert_kind_list(context,tformals) *)
	 val from_con = xcon context il_from_con
	 val to_con = xcon context il_to_con
       in Coercion_c {vars=vars',from=from_con,to=to_con}
       end

     | xcon' context (il_con as (Il.CON_TUPLE_INJECT il_cons)) =
       let
	 val cons = map (xcon context) il_cons
       in
	 NilDefs.con_tuple cons
       end

     | xcon' context (il_con as (Il.CON_TUPLE_PROJECT (i, il_con1))) =
       let
	   val con1 = xcon context il_con1
	   val lbl = IlUtil.generate_tuple_label(i+1)
	   val con = Proj_c(con1, lbl)
       in con
       end

     | xcon' context (il_con as (Il.CON_MODULE_PROJECT (module, lbl))) =
       (* If we get here then either modv is a structure value, which
          is formally possible but probably never occurs in elaborator
          output, or else il_con is a path that hasn't been memoized
          yet.
        *)
       let
(*
	   val {cbnd_cat,name_c,context,...} =
	       xmod context (module, NONE)
           val cbnd_list = flattenCatlist cbnd_cat

	   val proj_con = Proj_c (name_c, lbl)
	   val con = NU.makeLetC cbnd_list proj_con
*)
	   val con_mod = if N.is_sum lbl 
			     then xmodpathSum context module
			 else xmodpathCon context module
	   val con = Proj_c (con_mod, lbl)
       in
	   con
       end

   (* toFunction.  Helper function for translating exception handlers.

      We know from the way the elaborator works that the exception
      handler is represented as an Il.FIX defining one function.
      We know from the way Il.FIX is translated below that the
      translation of this will be Let_e with a function type binding
      followed by a single binding,
      containing just the definition of the (non-dependent) handler function,
      and that the body of the let will just return that function.

      The toFunction function translates the fix, pulls it apart, and
      returns the handler's parameter and the code for the handler body.
    *)
   and toFunction context (exp as Il.FIX param) : N.var * con * exp =
       let
	   val _ = if !debug then
	       (print "Doing''";
		Ppil.pp_exp exp)
		   else
		       ()
	   (*val Let_e (_, [_, Fixopen_b fns], Var_e var) = xexp context exp*)
	   val (ftbnds, [Fixopen_b fns], Var_e var) = xfix context param
       in
	   case List.find (fn ((v, _), _) => Name.eq_var(var,v)) fns of
	       SOME ((_, con), Function{tFormals=[],
			      eFormals=[(v,_)],fFormals=[],body,...}) =>
		     let
			 val {eFormals=[c],...} = strip_arrow_norm context (NU.makeLetC ftbnds con)
		     in
			 (v,c,body)
		     end
	     | NONE => error "(toFunction): impossible"
       end
     | toFunction _ e =
       (Ppil.pp_exp e;
	error "(toFunction): not a FIX expression")

   (* xvalue.  Translation function for term values
    *)
   and xvalue context (Prim.int (intsize, w): Il.value) : Nil.exp =
         Const_e (Prim.int (intsize, w))

     | xvalue context (Prim.uint (intsize, w)) =
	 Const_e (Prim.uint (intsize, w))

     | xvalue context (Prim.float (floatsize, f)) =
         (* All floating-point values are boxed by the phase-splitter.
            Possibly a later optimization can unbox them.
          *)
         Prim_e (NilPrimOp (box_float floatsize), [],
		 [], [Const_e (Prim.float (floatsize, f))])

     | xvalue context (Prim.array (il_con, a)) =
	 error "xvalue:  Can't translate array constants \
                \because sharing is lost"
(*       let
	   val il_exps = Array.foldr (op ::) nil a
           val con = xcon context il_con
           val exps = map (xexp context) il_exps
       in  Const_e (Prim.array (con, Array.fromList exps))
       end
*)

     | xvalue context (Prim.vector (il_con, v)) =
       let
	   val il_exps = Array.foldr (op ::) nil v
           val con = xcon context il_con
	   val exps = map (xexp context) il_exps
       in
	   Const_e (Prim.vector (con, Array.fromList exps))
       end

     | xvalue context (Prim.intvector (sz, v)) =
       let
	   val il_exps = Array.foldr (op ::) nil v
	   val exps = map (xexp context) il_exps
       in
	   Const_e (Prim.intvector (sz, Array.fromList exps))
       end
     | xvalue context (Prim.floatvector (sz, v)) =
       let
	   val il_exps = Array.foldr (op ::) nil v
	   val exps = map (xexp context) il_exps
       in
	   Const_e (Prim.floatvector (sz, Array.fromList exps))
       end

     | xvalue context (Prim.refcell (ref il_exp)) =
         error "xvalue:  Can't translate ref cell constants \
                \because sharing is lost"
(*       let
	   val exp = xexp context il_exp
       in
	   Const_e (Prim.refcell (ref exp))
       end
*)

     | xvalue context (Prim.tag(tag, il_con))  =
       let
	   val con = xcon context il_con
       in
	   Const_e (Prim.tag (tag, con))
       end

     and xfix context (is_recur, il_arrow, fbnds) =
       let
	   val (ftbnds, fbnds') = xfbnds context (is_recur, il_arrow, fbnds)

	   val _ = if !debug then
	       print "\nxfbnds done!!\n"
		   else ()
           val set = Sequence.fromList fbnds'

	   val (vars, types) = unzip (map #1 fbnds')
	   val names = map Var_e vars
	   val _ = if !debug then
	       (app (fn x => (print "\nVar : "; Ppnil.pp_exp x)) names;
		print "\n")
		   else ()
           val num_names = List.length names
           val labels = IlUtil.generate_tuple_labels num_names
       in
	   if (num_names = 1) then
	       (* If there's only one function, it should translate to
                  a value of a function type, rather than a 1-tuple
               *)
	       (ftbnds, [Fixopen_b set], hd names)
	    else
	     let
	       val (cbnds,rtype) = NU.makeNamedRecordType "gctag_arg" labels types
	       val evar = N.fresh_named_var "gctag"
	       val trs = map (fn _ => TraceUnknown) labels
	       val tag = Prim_e(NilPrimOp mk_record_gctag,trs,[rtype],[])
	       val ebnd = Exp_b (evar,TraceUnknown,tag)
	       val fields = (Var_e evar)::names
	       val ftbnds = ftbnds@cbnds
	     in
		 (ftbnds, [Fixopen_b set,ebnd], Prim_e(NilPrimOp (record labels), [],[], fields))
	     end
       end

   (* xexp.  Wrapper function for translating expressions *)
   and xexp context (il_exp : Il.exp) : Nil.exp =
       let
	   (* Tracing messages on entry
            *)
	   val this_call = ! xexp_count
	   val _ =
	       if (!debug) then
		   (xexp_count := this_call + 1;
		   print ("Call " ^ (Int.toString this_call) ^ " to xexp\n");
		   if (!full_debug) then
		     (Ppil.pp_exp il_exp; print"\n")
		   else ())
	       else ()

	   val result = (xexp' context il_exp)
	       handle e => (if (!debug) then
			      (print ("Exception detected in call " ^
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
	   (* Tracing messages on return
            *)
	    if (!debug) then
	      print ("Return " ^ (Int.toString this_call) ^ " from xexp\n")
	    else ();
	    result
        end

   (* xexp'.   Worker function for translating expressions.
               Should only be called by xexp, and not recursively.
    *)
   and xexp' context (Il.OVEREXP(_, _, exp_oneshot) : Il.exp) : Nil.exp =
       xexp context (derefOneshot exp_oneshot)

     | xexp' context (Il.SCON il_scon) = xvalue context il_scon

     | xexp' context (Il.ETAPRIM (prim, il_cons)) =
          xexp context (IlUtil.prim_etaexpand(get_hilctxt context, prim,
					      il_cons))

     | xexp' context (Il.ETAILPRIM (ilprim, il_cons)) =
          xexp context (IlUtil.ilprim_etaexpand(get_hilctxt context, ilprim,
						il_cons))

     | xexp' context (il_exp as (Il.PRIM (prim, il_cons, il_args))) =
       let
	   open Prim
	   (* translate the constructor and term arguments *)
	   val cons = map (xcon context) il_cons
	   val args = map (xexp context) il_args

           val (effect,con) =
	     case NU.strip_arrow
	            (NilPrimUtil.get_type' (get_nilctxt context) prim cons) of
		 SOME {effect,body_type,...} => (effect, body_type)
		| _ => (perr_c (NilPrimUtil.get_type'
				(get_nilctxt context) prim cons);
			error "xexp'/PRIM: Expected arrow constructor")

           (* The IL may think the primitive returns an unboxed float,
              but the phase-splitting adds code to box the result, which
              changes the return type.
            *)
	   val con : con = (case con of
				Prim_c(Float_c fs,[]) =>
				  Prim_c(BoxFloat_c fs,[])
			      | _ => con)

	   fun id (e : exp) = e
	   fun box fs e       = Prim_e(NilPrimOp(box_float fs),[], [], [e])
	   fun unbox fs e     = Prim_e(NilPrimOp(unbox_float fs), [],[], [e])
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
	   val (prim,args) = 
	     if !ref_is_array then
	       let
		 (* Refs are represented as arrays in the rest of the compiler when 
		  * compiling to alpha or sparc.  *)
		 val zero = Const_e (Prim.int (Prim.W32, TilWord64.fromInt 0))
		 val one = Const_e (Prim.int (Prim.W32, TilWord64.fromInt 1))
		 val t = (Prim.OtherArray false)
	       in
		 case prim 
		   of mk_ref => (create_table t,one::args)
		    | deref => (sub t, args @[zero])
		    | setref => (update t,(case args 
					     of [a,b] => [a,zero,b]
					      | _ => error "bad set_ref"))
		    | eq_ref => (equal_table t,args)
		    | _ => (prim,args)
	       end
	     else (prim,args)
       in
	 wrap(Prim_e (PrimOp prim, [], cons, args))
       end
     
     | xexp' context (il_exp as (Il.ILPRIM (ilprim, il_cons, il_args))) =
       let
	   val cons = map (xcon context) il_cons
	   val args = map (xexp context) il_args
       in
	 Prim_e (PrimOp (xilprim ilprim), [], cons, args)
       end

     | xexp' context (Il.VAR var) =
       let
	   (* The IL variable var had a renaming chosen whereever it
              was bound; apply this renaming *)
	   val var' = rename_var(var, context)
       in
	   mark_var_used(context,var');
	   Var_e var'
       end

     | xexp' context (il_exp as (Il.EXTERN_APP (il_con1, il_exp1, il_exps2))) =
       let
	 val exp1 = xexp context il_exp1
	 val exps2 = map (xexp context) il_exps2
	 val Il.CON_ARROW(cons2,res_con,_,_) = il_con1

	 (* An external function is not expecting boxed floats, so
            unbox any float arguments.

            XXX Depends on the elaborator not using type abbreviations
            that expand out into float in the type annotation of
            the EXTERN_APP function!  i.e, the code is looking
            literally for CON_FLOAT, rather than doing any reductions.
          *)
	 fun mapper(e,Il.CON_FLOAT _) =
	        Prim_e (NilPrimOp (unbox_float Prim.F64),[],[],[e])
	   | mapper(e,_) = e
	 val exps2 = Listops.map2 mapper (exps2,cons2)

	 (* Create the application
          *)
	 val app = ExternApp_e (exp1, exps2)
       in
	 (case res_con of
	     Il.CON_FLOAT _ =>
	       (* If the external function returned an unboxed float,
                  it has to be boxed.
                *)
	       Prim_e (NilPrimOp (box_float Prim.F64),[],[],[app])
	   | _ => app)
       end

     | xexp' context (il_exp as (Il.APP (il_exp1, il_exp2))) =
         (* An easy optimization:  reduce trivial redices (e.g.,
              an application of a lambda to a variable, or an
              eta-expanded primop to an argument).  This does
              *not* do general beta-reductions.

              XXX: The HIL context being supplied to exp_reduce
                   only contains the imports, and so il_exp
                   may not be well-formed with respect to this
                   context.   Does it matter?  (It may not,
                   if only things like "bool" are being accessed.)
          *)
         (case IlUtil.exp_reduce (get_hilctxt context, il_exp) of
	    NONE =>
	      let
		val exp1 = xexp context il_exp1
		val exp2 = xexp context il_exp2
	      in  App_e (Open, exp1, [], [exp2], [])
	      end
	  | SOME il_exp => xexp' context il_exp)

     | xexp' context (Il.FIX (is_recur, il_arrow, fbnds)) =
       let
	   val (ftbnds, fbnds, body) = xfix context (is_recur, il_arrow, fbnds)
       in
	   NU.makeLetE Sequential ((map NU.makeConb ftbnds) @ fbnds) body
       end

     (* The empty record does not take gctag, so treat it specially
      *)
     | xexp' context (Il.RECORD [])    =
          (Prim_e (NilPrimOp (record []), [], [], []))

     | xexp' context (Il.RECORD rbnds) =
       let
	   val (labels,il_exps) = Listops.unzip rbnds
	   val exps = map (xexp context) il_exps

	   fun mapper (l,e) = let val v = N.fresh_named_var (Name.label2name l)
			      in (Exp_b (v,TraceUnknown,e),
				  type_of context e,
				  TraceUnknown,
				  Var_e v)
			      end
	   val (bnds,types,trs,exps) = Listops.unzip4 (Listops.map2 mapper (labels,exps))

	   val (recbnds,rtype) = NU.makeNamedRecordType "record_gctag_arg" labels types

	   val evar = N.fresh_named_var "gctag"
	   val tag = Prim_e(NilPrimOp mk_record_gctag, trs,[rtype],[])
	   val tbnd = Exp_b (evar,TraceUnknown,tag)

	   val fields = (Var_e evar)::exps
       in
	 NU.makeLetE Sequential (bnds @ (map NU.makeConb recbnds) @ [tbnd] )
	 (Prim_e (NilPrimOp (record labels), [],[], fields))
       end

     | xexp' context (Il.RECORD_PROJECT (il_exp, label, il_record_con)) =
       let
	   val exp_record = xexp context il_exp
       in
	   Prim_e (NilPrimOp (select label), [],[], [exp_record])
       end

     | xexp' context (Il.SUM_TAIL (i,il_con, il_exp)) =
       let
	   val exp = xexp context il_exp
	   val sumcon = xcon context il_con
	   val sumcon_reduced = NilContext_con_hnf (context, sumcon)

	   (* The HIL has a subtyping on known/unknown sums, so either
	    * type is valid here.  So we must promote to the supertype for
	    * the MIL
	    *)
	   val sumcon = 
	     (case NilUtil.strip_sum sumcon_reduced
		of SOME (_,_,NONE,_) => sumcon
		 | SOME (tagcount,totalcount,_,carriers) =>
		  Prim_c(Sum_c {tagcount=tagcount, totalcount=totalcount,
				known = NONE}, [carriers])
		 | NONE => error "SUM_TAIL argument not a sumcon")


       in  Prim_e (NilPrimOp (project (TilWord32.fromInt i)),
		   [], [sumcon], [exp])
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
       in
	 Raise_e (exp, con)
       end

     | xexp' context (Il.LET (bnds, il_exp)) =
       let
	 val {cbnd_cat, sbnd_cat, ebnd_cat, final_context=context'} =
	   xbnds context bnds
	 val cbnds = flattenCatlist cbnd_cat
	 val sbnds = flattenCatlist sbnd_cat
	 val ebnds = (map NU.makeConb (cbnds @ sbnds)) @ (flattenCatlist ebnd_cat)
	 val exp = xexp context' il_exp
       in
	 NU.makeLetE Sequential ebnds exp
       end

     | xexp' context (Il.NEW_STAMP il_con) =
       let
	 val con = xcon context il_con
       in
	 Prim_e(NilPrimOp make_exntag, [],[con], [])
       end

     | xexp' context (Il.EXN_INJECT (s, il_tag, il_exp)) =
       let
	 val tag = xexp context il_tag
	 val exp = xexp context il_exp
       in
	 Prim_e (NilPrimOp (inj_exn s), [],[], [tag, exp])
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

	   val fun_name = N.fresh_named_var "fold"
	   val arg_name = N.fresh_named_var "fold_arg"
	   val body = Prim_e(NilPrimOp roll, [mu_con], [Var_e arg_name])
	   val lambda = Function {effect = Total,
				  recursive = Leaf,
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
       in
	 exp
       end

  | xexp' context (Il.UNFOLD (vars, il_mu_con, il_expanded_con)) =
(*********** Replacing this with a Nil coercion value ******
       let

	   val (vars',context) = insert_rename_vars(vars, context)

	   val tformals = map (fn v => (v,Type_k)) vars'

	   val context = update_NILctx_insert_kind_list(context,tformals)

	   val expanded_con = xcon context il_expanded_con
	   val mu_con = xcon context il_mu_con

	   val fun_name = N.fresh_named_var "unfold"
	   val arg_name = N.fresh_named_var "unfold_arg"
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
	 val field  =  TilWord32.fromInt field
	 val elist  = (case eopt of
			 NONE => []
		       | SOME il_exp => [xexp context il_exp])

	 val svar = Name.fresh_named_var "inj_sumcon"
	 val inj_var = Name.fresh_named_var "inj_var"

	 val cbnd = Con_b(Runtime,Con_cb (svar,sumcon))

	 val bnd = Exp_b (inj_var,TraceUnknown,Prim_e(NilPrimOp (inject field),[],[Var_c svar],elist))
	 val coercion = ForgetKnown_e (Var_c svar,field)
	 val body = Coerce_e(coercion,[],Var_e inj_var)
       in Let_e (Sequential,[cbnd,bnd],body)
       end

     | xexp' context (Il.CASE {sumtype, arg=il_arg, arms=il_arms, bound,
			       tipe, default=il_default}) =
       let
	   (* We want to use the result type given, rather than
              reconstructing the type, to avoid type blowup *)
	   val result_type = xcon context tipe
	   val sumcon = xcon context sumtype
	   val sumcon_reduced = NilContext_con_hnf (context, sumcon)
	   val exp = xexp context il_arg
	   val (bound', context') = insert_rename_var(bound, context)

	   fun xarm (n, NONE ) = NONE
	     | xarm (n, SOME ilexp) =
	       let
		   val context' = insert_con(context', bound',
					     NilUtil.convert_sum_to_special (sumcon_reduced, Word32.fromInt n))
	       in
		   SOME(Word32.fromInt n,
			TraceUnknown,
			xexp context' ilexp)
	       end

	   val arms = List.mapPartial (fn x => x) (mapcount xarm il_arms)

	   val default = Util.mapopt (xexp context') il_default

       in
	 Switch_e(Sumsw_e {sumtype = sumcon,
			   bound = bound',
			   arg  = exp, arms = arms,
			   default = default,
			   result_type = result_type})
       end

     | xexp' context (e as Il.EXN_CASE {arg = il_exp, arms = il_arms,
					default = il_default, tipe}) =
       let
	   val exp = xexp context il_exp
	   val result_type = xcon context tipe

           (* Check that the arms are all single-component FIXes
            *)
	   val (bounds, cons, tags, bodies) =
		Listops.unzip4
		  (map (fn (tag,c,Il.FIX(false,_,[Il.FBND(_,var,_,_,e)])) =>
			     (var,c,tag,e)
		         | (_,_,il_arm) =>
			     (print "EXN_CASE MATCH";
			      Ppil.pp_exp il_arm;
			      raise Match))
		       il_arms)

	   val (bound :: rest) = bounds

           (* Check that the elaborator uses the same variable
              for each arm of the case, which it currently does.
            *)
	   val _ = if (List.all (fn v => N.eq_var(v,bound)) rest) then
	              ()
		   else error "xexp': exn_case didn't get same var in all arms"

	   val (bound', context') = insert_rename_var (bound, context)

	   val arms =
	       Listops.map3 (fn (tag,il_con,body) =>
			     let
				 val con = xcon context il_con
				 val context' = insert_con(context', bound', con)
			     in
			          (xexp context' tag,
				   TraceUnknown,
				   xexp context' body)
			     end)
                            (tags, cons, bodies)

	   val default = Util.mapopt (xexp context) il_default
       in
	   Switch_e(Exncase_e {bound = bound',
			       arg = exp,
			       arms = arms,
			       default = default,
			       result_type = result_type})
       end

     | xexp' context (Il.MODULE_PROJECT (il_module, label)) =
       let
           (* This code is slightly confusing.  It's not clear
              how much, if any, of this work is redundant.
            *)


           (* Are we projecting the "it" label?
            *)
           val is_it_proj = N.eq_label(label, IlUtil.it_lab)

           (* Try to optimize the translation of this projection,
              by recognizing it as part of a polymorphic instantiation
            *)
	   val mod_opt =
	       (case il_module of
		  Il.MOD_APP(il_mod_fun, mod_arg) =>
		    (case (extractProjLabels il_mod_fun) of
		       (Il.MOD_VAR v, lbls) =>
			 (* We have a path applied to an argument *)
			 let
			   val ((_,_,v_r),_) = splitVar (v,context)
			 in
			   if ((!elaborator_specific_optimizations) andalso
			       ((var_is_polyfun(context, v_r)) orelse
				is_it_proj)) then
			     (* It appears to be polymorphic instantiation
                                and we're optimizing these.
                              *)

                             (* XXX:  Is it really possible to have a
                                 projection from a polymorphic instantiation
                                 that doesn't end in "it", or another
                                 discardable label?  If so, fun_part_r
                                 might need to project "lbls @ [lbl]"
                                 instead of just "lbls" in such cases.
                              *)
			     let
                               (* N.B. This code explicitly ignores the sumtype components
                                  of the argument, in order to maintain the invariant that
				  polymorphic functions are only passed a c and an r component.
                                    -Derek *)
			       val {ebnd_cat, cbnd_cat, name_c, name_r, ...} =
				    xmod context (mod_arg, NONE)
			       val _ = mark_var_used (context, v_r)
			       val fun_part_r =
				 NU.makeSelect (Var_e v_r) lbls

			     in
			       SOME (NU.makeLetE Sequential
				     ((map NU.makeConb (flattenCatlist cbnd_cat)) @
				      (flattenCatlist ebnd_cat))
				     (NU.makeAppE
				      fun_part_r [name_c] [name_r] []))
			     end
			   else
			     NONE
			 end
		     | _ => NONE)
		| _ => NONE)

           (* If we managed to optimize the translation, return this
              optimized translation.  Otherwise, translate the entire
              module.
            *)
	   val module =
	       (case mod_opt of
		    SOME module => module
		  | NONE =>
	            case xmodpath context il_module of
			SOME (_,_,exp) => exp
		      | NONE => 
			let
			    val {cbnd_cat, sbnd_cat, ebnd_cat, name_r, ...} =
			      xmod context (il_module, NONE)
			    val cbnds = flattenCatlist cbnd_cat
			    val sbnds = flattenCatlist sbnd_cat
			    val ebnds = flattenCatlist ebnd_cat
			    val bnds = (map NU.makeConb (cbnds @ sbnds)) @ ebnds
			in
			    NU.makeLetE Sequential bnds name_r
			end)
       in
	 (* Even if it's a projection of the it label, we know that
            the resulting run-time part of the module is not a singleton
            record, but the value we want.  Otherwise, project out
            the component.

            When does this happen?
          *)
	 if ((!elaborator_specific_optimizations) andalso is_it_proj) then
	   module
	 else
	   Prim_e (NilPrimOp (select label), [],[], [module])
       end

     | xexp' context (Il.SEAL (exp,_)) =
         (* We can freely break abstraction, so we do.
          *)
         xexp context exp

   (* xfbnds.  Translation of the core of the Il.FIX construct.
    * Return a list of cbnds, and the new function bound
    *)
   and xfbnds context (is_recur, il_arrow, fbnds) =
       let
	 val recursive = if is_recur then Arbitrary else NonRecursive
	 val totality = xeffect il_arrow
	 val fun_names = map (fn Il.FBND(v,_,_,_,_) => v) fbnds
	 val (fun_names, context) = insert_rename_vars (fun_names, context)
	   
	   
	   
	 (* Pre compute the new function name, the argument type, and 
	  * the function type for each function
	  *)
	 fun premapper (Il.FBND(var1, var2, il_con1, il_con2, body)) =
	   let
	     val var1' = rename_var(var1, context)
	     val con1 = xcon context il_con1
	     val con2 = xcon context il_con2
	     val aname = Name.var2name var2 ^ "_type"
	     val (cb1,con1) = NU.nameType aname con1
	     val name = Name.var2name var1' ^ "_type"
	     val (ftbnds,ftype) = NU.makeNamedArrowType name totality con1 con2
	   in
	     (cb1@ftbnds, (var1', ftype), con1)
	   end
	 
	 val (ftbnds,vcs,argtypes) = unzip3 (map premapper fbnds)
	 val ftbnds = Listops.flatten ftbnds

	 val context = update_NILctx_insert_cbnd_list(context,ftbnds)
	 val context = update_NILctx_insert_con_list(context,vcs)
	   
	 fun mapper ((var1', arrow_con), argcon, Il.FBND(var1, var2, il_con1, il_con2, body)) =
	   let
	     val (var2',context) = insert_rename_var (var2, context)
	       
	     val context = update_NILctx_insert_con(context, var2', argcon)
	     val body' = xexp context body
	   in   
	     ((var1', arrow_con),
	      Function{recursive = recursive, effect = totality,
		       tFormals = [], eFormals = [(var2', TraceUnknown)],
		       fFormals=[], body = body'})
	   end
	 
	 val vcflist = map3 mapper (vcs,argtypes,fbnds)
	   
       in (ftbnds,vcflist)
       end
     handle e => (print "uncaught exception in xfbnds\n";
		      raise e)

   (* xbnds.  Translation of a sequence of bindings.

      Implemented by adding a bunch of unnecessary labels, so that
      we get a sequence of sbnds, translating the sbnds, and then
      throwing away all the label-related results.
    *)
   and xbnds context bnds =
       let
	   val temporary_labels = makeInternalLabels (length bnds)
	   val sbnds = map Il.SBND (Listops.zip temporary_labels bnds)

	   val {final_context, cbnd_cat, sbnd_cat, ebnd_cat, ...} =
		xsbnds context sbnds
       in
	   {cbnd_cat = cbnd_cat,
	    sbnd_cat = sbnd_cat,
	    ebnd_cat = ebnd_cat,
	    final_context = final_context}
       end

   (* xsig.  The wrapper function for translating signatures.

              Because the type of the term part can refer to the
              components of the type part, we can only translate
              signatures given the name of the variables to be used for
	      the type part, con_c, *and* the sumtype part, con_s.
              These do not actually have to be variables, and on recursive
	      calls they are instantiated with non-variables to avoid having
	      to perform too many substitutions later on.
   *)

   and xsig context (con_c, con_s, il_sig) =
       let
	   (* Tracing message on entry *)
	   val this_call = ! xsig_count
	   val _ =
	       if (!debug) then
		   (xsig_count := this_call + 1;
		    print ("\nCall " ^ (Int.toString this_call) ^
			   " to xsig\n");
		    if (!full_debug) then
		      (Ppil.pp_signat il_sig; print "\n")
		    else ())
	       else ()

           (* Do the translation *)
	   val result =
	     xsig' context (con_c, con_s, il_sig)
	        handle e => (if (!debug)
			       then (print ("Exception detected in call " ^
					    (Int.toString this_call) ^
					    " to xsig:\n");
				     Ppil.pp_signat il_sig;
				     print "\n")
			     else ();
			     raise e)
       in
	 (* Tracing message on exit *)
	 if (!debug) then
	   print ("Return " ^ (Int.toString this_call) ^ " from xsig\n")
	 else ();

	 result
       end

   (* xsig'.  Translation of signatures.
              Should only be called by the wrapper function xsig,
              and not even recursively.

              Same arguments/results as xsig.
    *)

   and xsig' context (con_c, con_s, Il.SIGNAT_VAR v) =
          xsig' context (con_c, con_s,
			 case find_sig(context,v) of
			   NONE => error "unbound signature variable"
			 | SOME s => s)

     | xsig' context (con_c, con_s, Il.SIGNAT_STRUCTURE sdecs) =
           xsig_struct context (con_c,con_s,sdecs)

     | xsig' context (con_c, con_s, Il.SIGNAT_FUNCTOR (var, sig_dom, sig_rng, arrow))=
       let
(*
	   val _ = clear_memo var
*)
	   val ((var_c, var_s, var_r), context) = splitNewVar (var, context)
	   val (knd_c, knd_s, con_r) = xsig context (Var_c var_c, Var_c var_s, sig_dom)

	   val context = insert_kind(context, var_c, knd_c)
	   val context = insert_kind(context, var_s, knd_s)
	   val context = insert_con(context, var_r, con_r)

	   val (knd_c', knd_s', con_r') = xsig context (App_c(con_c, [Var_c var_c]),
					    App_c(con_s, [Var_c var_c, Var_c var_s]),
					    sig_rng)

           val effect = xeffect arrow

	   val tFormals = [(var_c,knd_c),(var_s,knd_s)]

       in
	   (Arrow_k (Open, [(var_c, knd_c)], knd_c'),
	    Arrow_k (Open, [(var_c, knd_c),(var_s, knd_s)], knd_s'),
	    AllArrow_c {openness = Open, effect = effect,
			tFormals = tFormals,
			eFormals = [con_r],
			fFormals = 0w0,
			body_type = con_r'})
       end

     | xsig' context (con_c, con_s, Il.SIGNAT_RDS (var, sdecs)) =
       let
	   val ((var_c, var_s, var_r), context) = splitNewVar (var, context)
	   val (knd_c, knd_s, type_r) = 
	       xsig_struct context (Var_c var_c, con_s, sdecs)
	   val subst = addToConSubst (NS.C.empty()) (var_c, con_c)
	   val knd_s = NS.substConInKind subst knd_s
	   val type_r = NS.substConInCon subst type_r
       in
	   (knd_c,knd_s,type_r)
       end

   and xpolysig context (Il.SIGNAT_FUNCTOR(var, sig_dom, sig_rng, arrow)) : Nil.con =
       let
	   val ((var_c, var_s, var_r), context) = splitNewVar (var, context)
	   val (knd_c, _, con_r) = xsig context (Var_c var_c, Var_c var_s, sig_dom)

	   val context = insert_kind(context, var_c, knd_c)
	   val context = insert_con(context, var_r, con_r)

           (* var_c and var_s here are dummy names, they should not appear in sig_rng. -Derek *)
	   val (_, _, con_r') = 
	       xsig context (Var_c var_c, Var_c var_s, sig_rng)

	   val effect = xeffect arrow

	   val tFormals = [(var_c,knd_c)]

       in
	   AllArrow_c {openness = Open, effect = effect,
		       tFormals = tFormals,
		       eFormals = [con_r],
		       fFormals = 0w0,
		       body_type = con_r'}
       end

     | xpolysig _ _ = error "Unimplemented case in xpolysig\n"


   (* xsig_struct.  Helper function used by xsig' to translate
                    structure signatures.
    *)
   and xsig_struct context (con_c : con, con_s : con, sdecs : Il.sdec list) =
       let
	   (* Translate the sdecs
            *)
	   val empty_subst = NilSubst.C.empty()
	   val {crdecs, srdecs, erdecs} =
	       xsdecs context (con_c, con_s, empty_subst, empty_subst, sdecs)

           (* Create the kind part *)
	   val kind_c = Record_k (Sequence.fromList crdecs)
	   val kind_s = Record_k (Sequence.fromList srdecs)

	   val (erlabs, ervars, ercons) = Listops.unzip3 erdecs

           (* Create the type part *)
	   val type_r = 
	     let val (rbnds,rtype) = NU.makeNamedRecordType "sig_term_type" erlabs ercons
	     in NU.makeLetC rbnds rtype
	     end

           (* If this is the signature of a single-element module
              with the "it" label (and hence an artifact of encoding
              polymorphism in the HIL), then "unbox" the result by
              taking it out of the module.
            *)
	   val type_r = (case (!elaborator_specific_optimizations,
			       sdecs, erdecs) of
			     (true, [Il.SDEC(it_lbl,_)], [(_,_,ercon)]) =>
				 (if (N.eq_label(it_lbl,IlUtil.it_lab))
				      then ercon
				  else type_r)
			   | _ => type_r)
       in  (kind_c, kind_s, type_r)
       end

   (* xsdecs.  Wrapper function for translating sdecs.

      Assume
         {erdecs, srdecs, crdecs} = xsdecs in_context (con,subst,sdecs).
      where
         con_c is the type part, and con_s the sumtype part,
	    of the structure whose signature
            contains these sdecs. (See the arguments of xsig).
    *)
   and xsdecs context (con_c,con_s,subst_c,subst_s,sdecs) : xsdecs_result =
       let
	   (* Tracing message on entry *)
	   val this_call = ! xsdecs_count
	   val _ = if (! debug) then
	            (xsdecs_count := this_call + 1;
		     print ("Call " ^ (Int.toString this_call) ^
			    " to xsdecs\n");
		     if (!full_debug) then
		       (Ppil.pp_sdecs sdecs; print "\n";
			(*
			 print "\nwith context = \n";
			 NilContext_print context;
			 *)
			print "\n\n")
		     else ())
                   else ()

           (* Rewrite the sdecs to get rid of datatype structures
              and inefficient recursive-function definitions
            *)
	   val sdecs = rewrite_sdecs sdecs

           (* Translate the resulting sdecs
            *)
	   val result =
	     xsdecs' context (con_c,con_s,subst_c,subst_s,sdecs)
	        handle e => (if (!debug) then
			       (print ("Exception detected in call " ^
				       (Int.toString this_call) ^
				       " to xsdecs\n");
				(*
				 print "\nwith context = \n";
				 print_splitting_context context;
				 *)
				print "\n")
			    else ();
			    raise e)

       in
	 (* Tracing message on return *)
	 if (!debug) then
	   print ("Return " ^ (Int.toString this_call) ^ " from xsdecs\n")
	 else ();

	 result
       end

  (* rewrite_sdecs.  Transforms a sequence of IL declarations in
         a fashion exactly parallel to the translations that
         the rewrite_xxx functions do to sbnds.
   *)
   and rewrite_sdecs (sdecs : Il.sdec list) : Il.sdec list =
     let
       fun filter (Il.SDEC(lab,Il.DEC_MOD(var,_,_))) = not (N.is_dt lab)
	 | filter (Il.SDEC(lab,_)) = not (N.eq_label(lab,IlUtil.ident_lab))

       fun loop [] = []
	 | loop ((sdec as
		  Il.SDEC(lab,Il.DEC_EXP(top_var,il_con, _, _))) :: rest) =
	     if (N.is_cluster lab) then
	       let
                 (* XXX
                      Why do we rewrite the declarations of the
                      projections from this nest, instead of just
                      translating them as-is as we do in the
                      polymorphic case below?
                 *)

		 (* val _ = print "entered mono optimization case\n" *)
		 val clist = (case il_con of
				Il.CON_RECORD lclist => map #2 lclist
			      | Il.CON_ARROW _ => [il_con]
			      | _ => error "can't optimize mono fun")
		 val numFunctions = length clist
		 val (rest, external_labels, external_vars) =
		   getSdecNames numFunctions rest
		 fun make_sdec (lbl,c) =
		   Il.SDEC(lbl,Il.DEC_EXP(N.fresh_var(),c,NONE,false))
		 val sdecs' = Listops.map2 make_sdec (external_labels,clist)
	       in  sdecs' @ (loop rest)
	       end
	     else
	       sdec::loop rest
	 | loop ((sdec as
		  Il.SDEC(lbl,
			  Il.DEC_MOD
			  (top_var, true, s as
			   Il.SIGNAT_FUNCTOR
			     (poly_var, il_arg_signat,
			      Il.SIGNAT_STRUCTURE([Il.SDEC(them_lbl,
							   Il.DEC_EXP(_,il_con,
								      _,_))]),
			      arrow))))
		 :: rest) =
	     if ((!do_polyrec)
		 andalso (!elaborator_specific_optimizations)
		 andalso (N.eq_label (them_lbl, IlUtil.them_lab))) then

	       (* if a polymorphic function has a "them" label rather
		  than an "it" label, then it is a polymorphic
		  function nest whose code (i.e., this entire
		  component) will be eliminated by the phase-splitter.
		  Therefore, the corresponding specification also is
		  ignored.

                  Note that the phase-splitter does some complicated
                  transformations to the projections from such a nest,
                  in order to turn polymorphic
                  recursively-defined-functions into polymorphic
                  recursion; however, the types of the projections,
                  which we know immediately follow, are unchanged so
                  we just continue on and translate them without doing
                  anything special.
		*)
		   loop rest
	       else
		   sdec :: (loop rest)
	     | loop (sdec::rest) = sdec::(loop rest)

           (* Get rid of the inner-datatype structures
            *)
	   val sdecs = if !elaborator_specific_optimizations
			   then List.filter filter sdecs
		       else sdecs
       in
	 (* Compensate for the transformation of recursive-function
	    definitions
          *)
	 loop sdecs
       end

   (* xsdecs'.  The worker function for translating sequences of sdecs.
                Should only be called by xsdecs (not even recursively).

                Same arguments/results as xsdecs'.

                The substitution argument is used because the types
                of the run-time part cannot refer directly to the
                internal variable names of the compile-time parts.
                Such references are replaced by projections from
                the compile-time part, and the correspondence between
                variables and projections is maintained in the substitution.
    *)
   and xsdecs' context (con_c, con_s, _, _, []) = {crdecs = nil, srdecs = nil, erdecs = nil}

     | xsdecs' context (con_c, con_s, subst_c, subst_s,
			Il.SDEC(lbl, d as Il.DEC_MOD(var,false,signat))
			  :: rest) =
       let
(*
	   val _ = clear_memo var
*)
	   val ((var_c, var_s, var_r), context) = splitNewVar (var, context)

	   (* Split the signature *)
	   val (knd_c, knd_s, con_r) = 
	         xsig context (Proj_c(con_c, lbl), Proj_c(con_s, lbl), signat)

	   val knd_s = NS.substConInKind subst_c knd_s
	   val con_r = NS.substConInCon subst_c (NS.substConInCon subst_s con_r)
(*
	   val context = insert_kind(context, var_c, knd_c)
	   val context = insert_kind(context, var_s, knd_s)
	   val context = insert_con(context, var_r, con_r)  
*)
	   val subst_c = addToConSubst subst_c (var_c,Proj_c(con_c,lbl))
	   val subst_s = addToConSubst subst_s (var_s,Proj_c(con_s,lbl))

	   val {crdecs, srdecs, erdecs} =
	       xsdecs' context (con_c, con_s, subst_c, subst_s, rest)

       in  {crdecs = ((lbl, var_c), knd_c) :: crdecs,
            srdecs = ((lbl, var_s), knd_s) :: srdecs,
	    erdecs = (lbl, var_r, con_r) :: erdecs}
       end

     | xsdecs' context (con_c, con_s, subst_c, subst_s,
			Il.SDEC(lbl, d as Il.DEC_MOD(var,true,signat))
			  :: rest) =
       let
(*
	   val _ = clear_memo var
*)
	   val ((_, _, var_r), context) = splitNewVar (var, context)

	   (* Split the signature *)
	   val con_r = xpolysig context signat

	   val con_r = NS.substConInCon subst_c (NS.substConInCon subst_s con_r)

	   val {crdecs, srdecs, erdecs} =
	       xsdecs' context (con_c, con_s, subst_c, subst_s, rest)

       in  {crdecs = crdecs,
	    srdecs = srdecs,
	    erdecs = (lbl, var_r, con_r) :: erdecs}
       end

     | xsdecs' context (con_c, con_s, subst_c, subst_s,
			Il.SDEC(lbl, d as Il.DEC_EXP(var,il_con, _, _))
			  :: rest) =
       let
	 val con = xcon context il_con
	 val con = NS.substConInCon subst_c (NS.substConInCon subst_s con)
	 val (var', context) = insert_rename_var(var, context)
	 val {crdecs, srdecs, erdecs} = xsdecs' context (con_c, con_s, subst_c, subst_s, rest)
       in
	 {crdecs = crdecs,
	  srdecs = srdecs,
	  erdecs = (lbl,var',con) :: erdecs}
       end

     | xsdecs' context (con_c, con_s, subst_c, subst_s,
			sdecs as Il.SDEC(lbl, d as Il.DEC_CON(var, il_knd,
							      maybecon,_))
			  :: rest)=
       let
	 val knd =
	   (case maybecon of
	      NONE => xkind il_knd
	    | SOME il_con => Single_k(xcon context il_con))

	 val knd = if N.is_sum lbl then NS.substConInKind subst_c knd else knd

	 val (var', context) = insert_rename_var(var, context)
(*
	 val context = update_NILctx_insert_kind(context, var', knd)
*)
         val (subst_c, subst_s) = 
	     if N.is_sum lbl
             then (subst_c, addToConSubst subst_s (var', Proj_c(con_s,lbl)))
	     else (addToConSubst subst_c (var', Proj_c(con_c,lbl)), subst_s)

	 val {crdecs, srdecs, erdecs} =
	       xsdecs' context (con_c,con_s,subst_c,subst_s,rest)

	 val (crdecs, srdecs) = 
	     if N.is_sum lbl
             then (crdecs, ((lbl, var'), knd) :: srdecs)
	     else (((lbl, var'), knd) :: crdecs, srdecs)

       in
	 {crdecs = crdecs,
	  srdecs = srdecs,
	  erdecs = erdecs}
       end

   (* xkind.   Translation of kinds.
               No wrapper function is created because HIL kinds are
               so simple.
    *)
   and xkind (Il.KIND : Il.kind) : kind = Type_k

     | xkind (Il.KIND_TUPLE n) = NilDefs.kind_type_tuple n

     | xkind (Il.KIND_ARROW (n,il_kres)) =
       let val args = map0count (fn _ => (N.fresh_var(), Type_k)) n
	   val kres = xkind il_kres
       in
           Arrow_k (Open, args, kres)
       end




   (******* BEGIN  MAIN  SPLITTING  CODE  HERE **************************)


   (* Given a set of used variables filter the import_bnds 
    *) 
   fun filter_import_bnds used import_bnds = 
     let
       fun loop [] = ([],used)
	 | loop (b::bnds) = 
	 let
	   val result as (bnds, used) = loop bnds
	   val vars = NU.varsBoundByBnds[b]
	   val isused = List.exists (fn v => VarSet.member(used, v)) vars
	 in
	   if isused then
	     (b::bnds,
	      let val (fvTerm,fvType) = NU.freeExpConVarInBnd(true,0,b)
	      in  VarSet.union(VarSet.union(used, fvType),fvTerm)
	      end)
	   else
	     result
	 end
     in loop import_bnds
     end


   (* Filter out the unused imports
    *)
   fun filter_imports used (imports,import_bnds) = 
     let
       val (import_bnds,used) = filter_import_bnds used import_bnds
       fun filter_chat isused keep l = 
	 if isused then 
	   chat2 ("Keeping label " ^ N.label2string l ^ " : used\n")
	 else if keep then 
	   chat2 ("Keeping label   " ^ N.label2string l ^ " : keep\n")
	 else
	   chat2 ("Filtering label " ^ N.label2string l ^ "\n")

       fun loop [] = ([], used)
	 | loop (imp::rest) =
	 (case imp
	    of (iv as ImportValue(l,v,_,c)) =>
	      let
		val result as (imports, used) = loop rest
		val isused = VarSet.member(used, v)
		val keep = N.keep_import l
		val _ = filter_chat isused keep l
	      in
		if (isused orelse keep) then
		  (iv :: imports,
		   let val fvType = NU.freeConVarInCon(true,0,c)
		   in  VarSet.union(used, fvType)
		   end)
		else
		  result
	      end
	     | (it as ImportType(l,v,k)) =>
	      let
		val result as (imports, used) = loop rest
		val isused = VarSet.member(used, v)
		val keep = N.keep_import l
		val _ = filter_chat isused keep l
	      in
		if (isused orelse keep) then
		  (it :: imports,
		   VarSet.union(used, NU.freeVarInKind (0,k)))
		else
		  result
	      end
	 | (ib as ImportBnd (phase,cb)) =>
	      let
		val result as (imports, used) = loop rest
		val v = (case cb 
			   of Con_cb (v,_)    => v
			    | Open_cb (v,_,_) => v
			    | Code_cb (v,_,_) => v)
	      in
		if VarSet.member(used, v) then
		  (ib :: imports,
		   let val fvType = NU.freeConVarInCbnd(true,0,cb)
		   in  VarSet.union(used, fvType)
		   end)
		else
		  result
	      end)

       val (imports,_) = loop imports
     in (imports,import_bnds)
     end


   datatype fimport = 
     ISDEC of Il.sdec  
     | ISBND of Il.sbnd
     | ISGNT of (Il.label * Il.var * Il.signat)
     | IXTRN of (Il.label * Il.var * Il.label * Il.con)
     
   fun print_fimport fimport = 
     (case fimport 
	of ISDEC sdec => Ppil.pp_sdec sdec
	 | ISBND sbnd => Ppil.pp_sbnd sbnd
	 | ISGNT arg  => Ppil.pp_context_entry(Il.CONTEXT_SIGNAT arg)
     	 | IXTRN arg  => Ppil.pp_context_entry(Il.CONTEXT_EXTERN arg))

   fun print_fimports fimports = app (fn fimp => (print_fimport fimp;print "\n")) fimports

(*
   fun flatten_sdecs (context,lbls,sdecs,subst,rimports) = 
     let
       fun loop (context,sdecs,subst,rsbnds,rimports) = 
	 (case sdecs
	    of [] => (rev rsbnds,rimports)
	     | sdec::sdecs => 
	      let
		val (subst,sbnd,rimports) = flatten_sdec(context,lbls,sdec,subst,rimports)
		val context = IlContext.add_context_sdecs(context,[sdec])
	      in loop (context,sdecs,subst,sbnd::rsbnds,rimports)
	      end)
     in loop (context,sdecs,subst,[],rimports)
     end
   
   (*
    * Give back: 
    * 1) list of sdecs (sub-components)
    * 2) an sbnd
    * 
    *)
   and flatten_sdec (context,lbls,Il.SDEC(l,dec),subst,rimports) = 
     let
       fun rpath2label lbls = Name.join_labels (rev lbls)
       fun do_dec_mod(v,is_polyfun,signat) = 
	 (case IlStatic.reduce_signat context signat
	    of Il.SIGNAT_STRUCTURE sdecs => 
	      let 
		val sdecs = rewrite_sdecs sdecs  (* Do elaborator specific optimizations *)
		  
		val (sbnds,rimports) = flatten_sdecs(context,l::lbls,sdecs,subst,rimports)
		val modl = Il.MOD_STRUCTURE sbnds		  
		val newl = rpath2label (l::lbls)
		val newv = Name.derived_var v
		  
		val subst = IlUtil.subst_add_modvar (subst,v,Il.MOD_VAR newv)
		val isbnd = Il.SBND(newl,Il.BND_MOD(newv,is_polyfun,modl))
		val sbnd  = Il.SBND(l,Il.BND_MOD(v,is_polyfun,Il.MOD_VAR newv))
	      in (subst,sbnd,(ISBND isbnd)::rimports)
	      end
	     | Il.SIGNAT_FUNCTOR _ => 
	      let
		val newl = rpath2label (l::lbls)
		val newv = Name.derived_var v
		val signat = IlUtil.sig_subst(signat,subst)
		val isdec = Il.SDEC(newl,Il.DEC_MOD(newv,is_polyfun,signat))
		val sbnd = Il.SBND(l,Il.BND_MOD(v,is_polyfun,Il.MOD_VAR newv))
	      in (subst,sbnd,(ISDEC isdec)::rimports)
	      end
	     | _ => error "Failed to reduce signature")
	    
       fun do_dec_exp(v,il_con,eopt,inline) =
	 let 
	   val newl = rpath2label (l::lbls)
	   val newv = Name.derived_var v
	   val il_con = IlUtil.con_subst(il_con,subst)
	   val eopt = (case eopt of SOME e => SOME (IlUtil.exp_subst(e,subst)) | _ => NONE)
	   val e = 
	     (case (inline,eopt) 
		of (true,SOME e) => e
		 | _ => Il.VAR newv)
		
	   val sbnd = Il.SBND (l,Il.BND_EXP (v,e))
	   val sdec = Il.SDEC (newl,Il.DEC_EXP(newv,il_con,eopt,inline))
	     
	   val subst = IlUtil.subst_add_expvar (subst,v,e)
	     
	 in (subst,sbnd,(ISDEC sdec)::rimports)
	 end
       fun do_dec_con(v,il_knd,copt,inline) =
	 let 		    
	   val newl = rpath2label (l::lbls)
	   val newv = Name.derived_var v
	   val copt = (case copt 
			 of SOME c => SOME (IlUtil.con_subst(c,subst))
			  | NONE => NONE)    
	   val c = 
	     (case (inline,copt) 
		of (true,SOME c) => c
		 | _ => Il.CON_VAR newv)
		
	   val dec = Il.DEC_CON(newv,il_knd,copt,inline)
	   val sdec = Il.SDEC(newl,dec)
	   val sbnd = Il.SBND (l,Il.BND_CON (v,c))
	     
	   val subst = IlUtil.subst_add_convar (subst,v,c)
	 in (subst,sbnd,(ISDEC sdec)::rimports)
	 end
     in
       case dec
	 of Il.DEC_MOD arg => do_dec_mod arg
	   
	  | Il.DEC_EXP arg => do_dec_exp arg
	  | Il.DEC_CON arg => do_dec_con arg
     end
   

   (* The top level module will have an unnecessary renaming.  This
    * will get rid of it, relying on knowledge of how flatten_sdec is 
    * implemented 
    *)
   fun fixup_rimports (sbnd,rimports) = 
     let
       fun default () = (print "Warning: unexpected result from flatten_sdec: punting!\n";
			 (ISBND sbnd)::rimports)
       val rimports = 
	 (case (sbnd,rimports)
	    of (Il.SBND(l,Il.BND_MOD(v,_,Il.MOD_VAR newv)),(ISBND (Il.SBND(_,Il.BND_MOD(newv',is_polyfun,m))))::rimports) => 
	      if Name.eq_var(newv,newv') then
		(ISBND (Il.SBND(l,Il.BND_MOD(v,is_polyfun,m))))::rimports
	      else default()
	     | _ => default())
     in rimports
     end

   val flatten_sdec = fn (context,sdec) => 
     let 
       val (_,sbnd,rimports) = flatten_sdec(context,[],sdec,IlUtil.empty_subst,[])
       val context = IlContext.add_context_sdecs(context,[sdec])
       val rimports = fixup_rimports(sbnd,rimports)   
     in (context,rimports)
     end


   (*
    * Optimization: proactively replace projections from modules
    * with the variables declaring or defining their flattened sub-components.
    * This greatly improves the code coming out of the phase-splitter, though
    * it has no effect on the eventual result.
    *)
   fun add_paths_from_rimports (subst,rimports,Il.SDEC (l,Il.DEC_MOD(rootv,_,_))) = 
     let
       fun label2path l = 
	 let
	   val (_::lbls) = Name.split_label l
	 in Il.PATH(rootv,lbls)
	 end

       fun add2subst (dosubst,mk) (subst,l,v) = 
	 if Name.is_flat l then
	   dosubst(subst,label2path l, mk v)
	 else subst
       val addmod2subst = add2subst (IlUtil.subst_add_modpath,Il.MOD_VAR)
       val addexp2subst = add2subst (IlUtil.subst_add_exppath,Il.VAR)
       val addcon2subst = add2subst (IlUtil.subst_add_conpath,Il.CON_VAR)

       fun imp_subst (ISDEC sdec,subst) = let val [sdec] = IlUtil.sdecs_subst([sdec],subst) 
					  in ISDEC sdec
					  end
	 | imp_subst (ISBND sbnd,subst) = let val [sbnd] = IlUtil.sbnds_subst([sbnd],subst) 
					  in ISBND sbnd
					  end
       fun loop [] = (subst,[])
	 | loop (imp::rimports) = 
	 let
	   val (subst,rimports) = loop rimports
	   val imp = imp_subst(imp,subst)
	   val rimports = imp::rimports
	   val subst = 
	     case imp
	       of ISDEC (Il.SDEC (l,Il.DEC_EXP (v,_,_,_))) => addexp2subst(subst,l,v)
		| ISDEC (Il.SDEC (l,Il.DEC_CON (v,_,_,_))) => addcon2subst(subst,l,v)
		| ISDEC (Il.SDEC (l,Il.DEC_MOD (v,_,_)))   => addmod2subst(subst,l,v)
		| ISBND (Il.SBND (l,Il.BND_MOD (v,_,_)))   => addmod2subst(subst,l,v)
	 in (subst,rimports)
	 end
     in loop rimports
     end
		

   fun flatten_export_sdec (context,sdec as (Il.SDEC (l, Il.DEC_MOD (rootv,_,_)))) = 
     let
       fun mk_export (Il.SDEC(l,dec)) =
	 let
	   val (root::labels) = Name.split_label l
	   val (bnd,dec) = 
	     case dec
	       of Il.DEC_EXP (v,c,eopt,inline) => 
		 let 
		   val bnd = Il.BND_EXP(v,IlUtil.path2exp (Il.PATH(rootv,labels)))
		   val dec = Il.DEC_EXP(v,c,eopt,inline)
		 in (bnd,dec)
		 end
		| Il.DEC_CON (v,k,copt,inline) => 
		 let 
		   val bnd = Il.BND_CON(v,IlUtil.path2con (Il.PATH(rootv,labels)))
		   val dec = Il.DEC_CON(v,k,copt,inline)
		 in (bnd,dec)
		 end
		| Il.DEC_MOD (v,is_polyfun,f) =>  
		 let 
		   val bnd = Il.BND_MOD(v,is_polyfun,IlUtil.path2mod (Il.PATH(rootv,labels)))
		   val dec = Il.DEC_MOD(v,is_polyfun,f)
		 in (bnd,dec)
		 end
	 in (Il.SBND(Name.fresh_internal_label (Name.label2name l),bnd),Il.SDEC(l,dec))
	 end
       
       (* The first bnd is just an eta expansion of the 
	* top level sdec.  Drop it to avoid shadowing and
	* to produce cleaner code from the start.
	*)
       val (_,_::rimports) = flatten_sdec(context,sdec)
	 
       fun loop ([],sbnds,exports) = (sbnds,exports)
	 | loop (imp::imports,sbnds,exports) = 
	 (case imp
	    of ISDEC sdec => 
	      let
		val (sbnd,sdec) = mk_export sdec
	      in loop(imports,sbnd::sbnds,sdec::exports)
	      end
	     | ISBND sbnd => loop(imports,sbnd::sbnds,exports)
	     | _ => error "Shouldn't be other context entries here")
     in loop(rimports,[],[])
     end
     | flatten_export_sdec _ = error "Non mod export from elaborator!"
     
   fun FlattenHILMod (HILctx,sbnd,sdec) : (fimport list * Il.sbnd list * Il.sdec list) = 
     let
       
       fun folder (entry,(context,subst,rimports)) =
	 (case entry 
	    of Il.CONTEXT_SDEC (sdec as (Il.SDEC (l,dec))) =>
	      if (!elaborator_specific_optimizations) andalso (N.is_dt l)
		then (context,subst,rimports)
	      else 
		let 
		  val (context,new_rimports) = flatten_sdec(context,sdec)
		  val (subst,new_rimports) = add_paths_from_rimports(subst,new_rimports,sdec)
		in (context,subst,new_rimports@rimports)
		end
	     | Il.CONTEXT_SIGNAT (arg as (l,v,s)) => 
		let
		  val context = IlContext.add_context_sig(context,l,v,s)  (* Old world sig *)
		  val s = IlUtil.sig_subst(s,subst)
		in (context,subst,ISGNT (l,v,s)::rimports)
		end
	     | Il.CONTEXT_EXTERN (l1,v,l2,c) => (context,subst,(IXTRN (l1,v,l2,IlUtil.con_subst(c,subst)))::rimports)
	     | Il.CONTEXT_FIXITY arg => (context,subst,rimports)
	     | Il.CONTEXT_OVEREXP arg => (context,subst,rimports))
	    
       val (context,subst,rimports) =
	 foldl folder  (IlContext.empty_context,IlUtil.empty_subst,[]) (IlContext.list_entries HILctx)
	 
       val _ = 
	 if !full_debug then 
	   (print "\nFinished flattening context\n")
	 else ()
	   

       val (newsbnds,sdecs) = flatten_export_sdec(context,sdec)
	 
       val sbnds = sbnd::newsbnds

       val sbnds = IlUtil.sbnds_subst(sbnds,subst)
       val sdecs = IlUtil.sdecs_subst(sdecs,subst)
	 
     in (rev rimports,sbnds,sdecs)
     end
*)

   (*  In the case that we choose not to flatten, map to the same type as a flattened module
    *)
   fun dontFlattenHILMod (HILctx,sbnd,sdec) : (fimport list * Il.sbnd list * Il.sdec list) = 
     let
       
       fun folder (entry,imports) =
	 (case entry 
	    of Il.CONTEXT_SDEC sdec => (ISDEC sdec)::imports
	     | Il.CONTEXT_SIGNAT arg => (ISGNT arg)::imports
	     | Il.CONTEXT_EXTERN arg => (IXTRN arg)::imports
	     | Il.CONTEXT_FIXITY arg => imports
	     | Il.CONTEXT_OVEREXP arg => imports)
	    
       val imports =
	 foldr folder  [] (IlContext.list_entries HILctx)
	 

       val sbnds = [sbnd]
       val sdecs = [sdec]

     in (imports,sbnds,sdecs)
     end

(*
   fun FlattenHILInt (HILctx,sdec) : (fimport list * Il.sdec list) = error "Unimplemented"
*)

   (*  In the case that we choose not to flatten, map to the same type as a flattened module
    *)
   fun dontFlattenHILInt (HILctx,sdec) : (fimport list * Il.sdec list) = 
     let
       
       fun folder (entry,imports) =
	 (case entry 
	    of Il.CONTEXT_SDEC sdec => (ISDEC sdec)::imports
	     | Il.CONTEXT_SIGNAT arg => (ISGNT arg)::imports
	     | Il.CONTEXT_EXTERN arg => (IXTRN arg)::imports
	     | Il.CONTEXT_FIXITY arg => imports
	     | Il.CONTEXT_OVEREXP arg => imports)
	    
       val imports =
	 foldr folder  [] (IlContext.list_entries HILctx)
	 

       val sdecs = [sdec]

     in (imports,sdecs)
     end


   fun flatten_type_to_bnds (context,name,con) = 
     let
       (* Horrible icky hack to avoid modifying xsig and xcon to return cbnds *)
       val (cbnds,c) = 
	 (case NU.flattenCLet con
	    of Let_c (_,tbnds,body) => (tbnds,body)
	     | Var_c v => ([],Var_c v)
	     | _ => 
	      let
		val type_var = Name.fresh_named_var name
	      in ([Con_cb(type_var,con)],Var_c type_var)
	      end)
	    
       val context = update_NILctx_insert_cbnd_list(context,cbnds)
	 
       val ibnds = rev (map (fn cb => ImportBnd(Runtime,cb)) cbnds)
     in (context,ibnds,c)
     end
   
   fun ximports (fimports,context) =
     let

       fun dodec(l,dec,(imports, bnds,context : splitting_context)) = 
	 (case dec of
	    Il.DEC_MOD (v, is_polyfun, il_sig) =>
	      let
		val (l_c,l_s,l_r) = N.make_csr_labels l

		val ((v_c, v_s, v_r),context) = splitNewVar (v,context)
		  
		val (knd_c, knd_s, type_r) = xsig context (Var_c v_c, Var_c v_s, il_sig)
		  
		val context = insert_kind(context,v_c, knd_c)
		val context = insert_kind(context,v_s, knd_s)

		val (context,ivtbs,type_r) = flatten_type_to_bnds(context,(Name.var2name v)^"_t",type_r)
		  
		val context = insert_con(context, v_r, type_r)
		  
		val ivc = ImportType(l_c,v_c,knd_c)
		val ivs = ImportType(l_s,v_s,knd_s)
		val ivr = ImportValue(l_r,v_r,TraceUnknown,type_r)
	      in
		(* If we're importing a polymorphic function,
			     then we don't ever need the type part.
			       *)
		if is_polyfun then
		  (* This case should only come up in the presence of flattening, which is currently
                     not (fully) implemented. -Derek *)
		  (* (ivr::ivtbs@imports, bnds,update_polyfuns(context, v_r)) *)
 		  error "tonil: ximports error"
		else
                  (* The imports are backwards because we're building the list in reverse. *)
		  (ivr::ivtbs@(ivs::ivc::imports), bnds,context)
	      end
          (* This case should only come up in the presence of flattening, which is currently
             not (fully) implemented. -Derek *)
	  | _ => error "tonil: ximports error"
        )
(*
	  | Il.DEC_EXP (var, il_con, _,_) =>
	      let
		val l = if Name.is_flat l then #2 (Name.make_cr_labels l) else l

		val con = xcon context il_con
		val (context,ibnds,con) = flatten_type_to_bnds(context,(Name.var2name var)^"_type",con)
		val (var, context) = insert_rename_var(var, context)
		val context = update_NILctx_insert_con(context, var, con)
	      in
		((ImportValue(l,var,TraceUnknown,con))::ibnds@imports,bnds,context)
	      end
	  | Il.DEC_CON(var, il_knd, maybecon,_) =>
	      let
		val l = if Name.is_flat l then #1 (Name.make_cr_labels l) else l
		val knd =
		  (case maybecon of
		     NONE => xkind context il_knd
		   | SOME il_con => Single_k(xcon context il_con))
		     
		val (var, context) = insert_rename_var(var, context)
		val context = update_NILctx_insert_kind(context, var, knd)
	      in
		((ImportType(l,var,knd))::imports,bnds,context)
	      end
*)

       fun dobnd (sbnd,(imports,bnds,context)) = 
	 let
	    val {cbnd_cat, ebnd_cat, final_context, ...} =
	      xsbnds context [sbnd]

	    val ibnds = map (fn cb => ImportBnd(Runtime,cb)) (flattenCatlist cbnd_cat)
	    val bnds = APPEND [bnds,ebnd_cat]
	 in (List.revAppend (ibnds,imports),bnds,final_context)
	 end

       fun dosig(v,il_sig,(imports, bnds,context : splitting_context)) =
	 (* Just store any signature definition we come across.
	  *)
	 (imports,bnds,update_insert_sig(context,v,il_sig))
	 
       fun doext(v,l,il_type,(imports, bnds, context : splitting_context)) =
	 let 
	   val nil_type = xcon context il_type
	   val (v',context') = insert_rename_var(v,context)
	   val context' = insert_con(context', v', nil_type)
	 in  (ImportValue(l,v',TraceUnknown,nil_type)::imports,bnds,
	      context')
	 end
       
       (* Process each HIL context entry *)
       fun folder (ent,acc) =
	 (case ent of
	    ISDEC (Il.SDEC (l,dec)) =>
	      if ((!elaborator_specific_optimizations) andalso (N.is_dt l))
		then acc
	      else dodec(l,dec,acc)
	  | ISBND sbnd => dobnd(sbnd,acc)
	  | ISGNT (_,v,il_sig) => dosig(v,il_sig,acc)
	  | IXTRN (_,v,l,c) => doext(v,l,c,acc))
	    
       val (rev_imports,bnds,context) =
	 foldl folder ([],NIL, context) fimports
       val imports = rev rev_imports
       val bnds = flattenCatlist bnds
     in  (imports, bnds,context)
     end

   fun sdecs2exports final_context sdecs =
     let
       fun loop ([],acc) = rev acc
	 | loop ((Il.SDEC (l,dec))::sdecs,acc) = 
	 (case dec 
	    of Il.DEC_MOD (v,is_polyfun,_) =>
	      let
		val (lc,ls,lr) = N.make_csr_labels l
		(* v should already be bound in the context*)
		val ((vc,vs,vr),_) = splitVar (v,final_context)
		val acc = 
		  if is_polyfun then 
		    (ExportValue(lr,vr)):: acc
		  else
		    (ExportValue(lr,vr)) :: (ExportType(ls,vs)) :: (ExportType(lc,vc)) :: acc
	      in
		loop(sdecs,acc)
	      end
          (* This case should only come up in the presence of flattening, which is currently
             not (fully) implemented. -Derek *)
	  | _ => error "tonil: sdecs2exports error")
(*
	     | Il.DEC_EXP (v,_,_,_) => 
	      let
		val l = if Name.is_flat l then #2 (Name.make_cr_labels l) else l
		val v = rename_var(v, final_context)
	      in
		mark_var_used(final_context,v);
		loop(sdecs,(ExportValue(l,v)) :: acc)
	      end
	     | Il.DEC_CON (v,_,_,_) => 
	      let
		val l = if Name.is_flat l then #1 (Name.make_cr_labels l) else l
		val v = rename_var(v, final_context)
	      in
		mark_var_used(final_context,v);
		loop(sdecs,ExportType(l,v) :: acc)
	      end
*)
     in loop (sdecs,[])
     end

   fun sdecs2exports_int context sdecs =
     let
       fun loop ([],acc) = rev acc
	 | loop ((Il.SDEC (l,dec))::sdecs,acc) = 
	 (case dec of
	    Il.DEC_MOD (v, is_polyfun, il_sig) =>
	      let
		val (l_c,l_s,l_r) = N.make_csr_labels l
		  
		val ((v_c, v_s, v_r),context) = splitNewVar (v,context)
		  
		val (knd_c, knd_s, type_r) = xsig context (Var_c v_c, Var_c v_s, il_sig)
		  
		val context = insert_kind(context,v_c,knd_c)
		val context = insert_kind(context,v_s,knd_s)
		  
		val (context,ivtbs,type_r) = flatten_type_to_bnds(context,(Name.var2name v)^"_t",type_r)
		  
		val context = insert_con(context, v_r, type_r)
		  
		  
		val ivc = ImportType(l_c,v_c,knd_c)
		val ivs = ImportType(l_s,v_s,knd_s)
		val ivr = ImportValue(l_r,v_r,TraceUnknown,type_r)
		(* If we're importing a polymorphic function,
			     then we don't ever need the type part.
			       *)
		val acc =
		  if is_polyfun then
                  (* This case should only come up in the presence of flattening, which is currently
                     not (fully) implemented. -Derek *)
	             error "tonil: sdecs2exports_int error"
		    (* ivr::ivtbs@acc *)
		  else
		    (ivr::ivtbs@(ivs::ivc::acc))
	      in loop(sdecs,acc)
	      end
          (* This case should only come up in the presence of flattening, which is currently
             not (fully) implemented. -Derek *)
	  | _ => error "tonil: sdecs2exports_int error")
(*
	  | Il.DEC_EXP (var, il_con, _,_) =>
	      let
		val l = if Name.is_flat l then #2 (Name.make_cr_labels l) else l
		  
		val con = xcon context il_con
		val (context,ibnds,con) = flatten_type_to_bnds(context,(Name.var2name var)^"_type",con)
		val (var, context) = insert_rename_var(var, context)
		val context = update_NILctx_insert_con(context, var, con)
		val acc = 
		  ((ImportValue(l,var,TraceUnknown,con))::ibnds@acc)
	      in loop(sdecs,acc)
	      end
	  | Il.DEC_CON(var, il_knd, maybecon,_) =>
	      let
		val l = if Name.is_flat l then #1 (Name.make_cr_labels l) else l
		val knd =
		  (case maybecon of
		     NONE => xkind context il_knd
		   | SOME il_con => Single_k(xcon context il_con))
		     
		val (var, context) = insert_rename_var(var, context)
		val context = update_NILctx_insert_kind(context, var, knd)
		val acc =
		  ((ImportType(l,var,knd))::acc)
	      in loop(sdecs,acc)
	      end
*)
     in loop (sdecs,[])
     end   
   (* The top-level function for the phase-splitter.
    *)
   fun phasesplit (ilmodule : Il.module) : Nil.module =
     let
(*
       val _ = reset_memo()
*)
       val (HILctx,sbnd,sdec) = ilmodule
	 
       (* GC the HIL context *)
       val roots = VarSet.union (IlUtil.sdec_free sdec,
				 IlUtil.sbnd_free sbnd)
       val HILctx = IlContext.gc_context (HILctx, roots)
	 
       val _ =
	 if (!full_debug) then
	   (print "\nInitial HIL context:\n";
	    Ppil.pp_context HILctx;
	    print "\n")
	 else
	   ()
	   
       val (fimports,sbnds,sdecs) = if (!flatten_modules) 
				      then error "FlattenHILMod unimplemented"
(*					  FlattenHILMod (HILctx,sbnd,sdec) *)
				    else dontFlattenHILMod (HILctx,sbnd,sdec)
	 
       (* Debugging printing *)
       val _ =
	 if ((!full_debug) orelse (!printFlat)) andalso (!flatten_modules) then
	   (print "\nFlattened HIL module:\n";
	    print "\tFlattened imports:\n";
	    print_fimports fimports;
	    print "\n";
	    print "\tFlattened sbnds:\n";
	    Ppil.pp_sbnds sbnds;
	    print "\n";
	    print "\tFlattened exports:\n";
	    Ppil.pp_sdecs sdecs;
	    print "\n")
	 else
	   ()
	   
       (* Compute the initial context and imports.  
	*)
       val (imports,import_bnds,initial_splitting_context) =
	 Stats.subtimer("Phase-split-ctxt",ximports) (fimports,make_initial_splitting_context HILctx)
	 
       (* Debugging printing *)
       val _ =
	 if (!full_debug) then
	   (print "\nInitial NIL context:\n";
	    NilContext_print initial_splitting_context;
	    print "\n")
	 else
	   ()
	   
       val _ = msg "  Initial context is phase-split\n"
	 
       (* Since we actually compute FV sets for imports during filtering,
	* we only care about the status of variables from the body
	* of the module.  Keeping the used information from above
	* could in principle interfere with cascaded elimination.
	*)
       val _ = reset_used initial_splitting_context
	 
       (* Phase-split the bindings
	*)
       val {cbnd_cat, sbnd_cat, ebnd_cat, final_context, ...} =
	 xsbnds initial_splitting_context sbnds
       val cbnds = map NU.makeConb (flattenCatlist cbnd_cat)
       val sumbnds = map NU.makeConb (flattenCatlist sbnd_cat)
       val ebnds = flattenCatlist ebnd_cat
       val bnds = cbnds @ sumbnds @ ebnds
       val _ = if !debug then (print "PRINTING BNDS\n\n\n"; Ppnil.pp_bnds bnds; print "\n\n\n") else ()
       val nil_initial_context = get_nilctxt initial_splitting_context
       val nil_final_context = get_nilctxt final_context
	 
       val _ = msg "  Bindings are phase-split\n"
	 
       (* Create the exports.
	*)
       val exports : export_entry list = sdecs2exports final_context sdecs
       val exports_int = if !do_exports_int then SOME(sdecs2exports_int final_context sdecs)
			 else NONE
       
       val _ = msg "  Exports are phase-split\n"

       (* Since "used" is maintained as a ref in the context, we
	could access it via either the nil_initial_context or
	nil_final_context
	*)
       val used = get_used initial_splitting_context
	 
       val (imports,import_bnds) = 
	 if (!killDeadImport) then
	   filter_imports used (imports,import_bnds)
	 else
	   (imports,import_bnds)
	   
       val bnds = import_bnds @ bnds

       val _ = msg "  Imports are computed\n"
	 	 
	 
       (* We finally have the phase-split compilation unit.
	*)
       val nilmod = MODULE{bnds = bnds,
			   imports = imports,
			   exports = exports,
			   exports_int = exports_int}
	 
       (* Release the memory for the memo pad immediately, rather
	than waiting for the next time the phase-splitter runs.
	*)
(*
       val _ = reset_memo()
*)
     in
       nilmod
     end


   (* The top-level function for the phase-splitter.
    *)
   fun phasesplit_interface (ilinterface : Il.sc_module) : Nil.interface =
     let
(*
       val _ = reset_memo()
*)
       val (HILctx,sdec) = ilinterface
	 
       (* GC the HIL context *)
       val roots = IlUtil.sdec_free sdec

       val HILctx = IlContext.gc_context (HILctx, roots)
	 
       val _ =
	 if (!full_debug) then
	   (print "\nInitial HIL context:\n";
	    Ppil.pp_context HILctx;
	    print "\n")
	 else
	   ()
	   
       val (fimports,sdecs) = if (!flatten_modules) 
				then error "FlattenHILInt unimplemented"
(*				    FlattenHILInt (HILctx,sdec) *)
			      else dontFlattenHILInt (HILctx,sdec)
	 
       (* Debugging printing *)
       val _ =
	 if ((!full_debug) orelse (!printFlat)) andalso (!flatten_modules) then
	   (print "\nFlattened HIL module:\n";
	    print "\tFlattened imports:\n";
	    print_fimports fimports;
	    print "\n";
	    print "\tFlattened exports:\n";
	    Ppil.pp_sdecs sdecs;
	    print "\n")
	 else
	   ()
	   
	   
       (* Compute the initial context and imports.  
	*)
       val (imports,import_bnds,initial_splitting_context) =
	 Stats.subtimer("Phase-split-ctxt",ximports) (fimports,make_initial_splitting_context HILctx)
	 
       (* Debugging printing *)
       val _ =
	 if (!full_debug) then
	   (print "\nInitial NIL context:\n";
	    NilContext_print initial_splitting_context;
	    print "\n")
	 else
	   ()
	   
	   
       val _ = msg "  Initial context is phase-split\n"
	 
       (* Since we actually compute FV sets for imports during filtering,
	* we only care about the status of variables from the body
	* of the module.  Keeping the used information from above
	* could in principle interfere with cascaded elimination.
	*)
       val _ = reset_used initial_splitting_context
	 	 
       (* Create the exports.
	*)

       val context = initial_splitting_context 

       val exports : import_entry list = sdecs2exports_int context sdecs 
       
       val _ = msg "  Exports are phase-split\n"
	 
       val used = get_used initial_splitting_context
	 
	 
       val (imports,import_bnds) = 
	 if (!killDeadImport) then
	   filter_imports used (imports,import_bnds)
	 else
	   (imports,import_bnds)
	   
       val ibnds = map (fn b => case b of Con_b (p,cb) => ImportBnd(p,cb) | _ => error "No Bnds in interfaces") import_bnds


       val _ = msg "  Imports are computed\n"

       (* We finally have the phase-split compilation unit.
	*)
       val nilint = INTERFACE{imports = imports @ ibnds,
			      exports = exports}
	 
       (* Release the memory for the memo pad immediately, rather
	than waiting for the next time the phase-splitter runs.
	*)
(*
       val _ = reset_memo()
*)
     in
       nilint
     end
end
