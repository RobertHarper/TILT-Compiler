(*$import Il Nil PPIL ILUTIL ILCONTEXT ILSTATIC NILUTIL NILERROR NILCONTEXT PPNIL NILSUBST PRIMUTIL Stats LibBase TONIL NORMALIZE Option *)

(* We box all floats and translate floating point operations accordingly.
   Thus, kind type is replaced by work types.
   Also note that all term-level (but not type-level) 
        record fields must be sorted by their labels. *)

functor Tonil(structure Ilutil : ILUTIL
              structure Nilutil : NILUTIL
	      structure NilError : NILERROR
              structure Ilcontext : ILCONTEXT
              structure IlStatic : ILSTATIC
              structure Nilcontext : NILCONTEXT
	      structure Normalize : NORMALIZE
              structure Nilprimutil : PRIMUTIL
	         where type exp = Nil.exp
                   and type con = Nil.con
              structure Ppnil : PPNIL
              structure Ppil : PPIL
	      structure Subst : NILSUBST
	       sharing type Nilcontext.context = Normalize.context
		   and type Subst.subst = Nilcontext.subst)
           (*   :> TONIL *) =
struct


   structure VarSet = Name.VarSet

   open Nil Listops
   val debug      = ref false
   val diag       = ref true
   val full_debug = ref false
   val trace      = ref false
   val do_memoize = ref true
   val do_kill_cpart_of_functor = ref true
   val killDeadImport = Stats.bool("killDeadImport")
   val _ = killDeadImport := true
   val do_preproject = Stats.bool("do_preproject")
   val _ = do_preproject := true

   val elaborator_specific_optimizations = ref true
   val omit_datatype_bindings = ref true
   fun error msg = Util.error "tonil.sml" msg


   fun msg str = if (!diag) then print str else ()


   val strip_arrow = Nilutil.strip_arrow

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

       fun newSplit (var, vmap) = 
	   let
	       val var_name = Name.var2string var
	       val var_c = Name.fresh_named_var (var_name ^ "_c")
	       val var_r = Name.fresh_named_var (var_name ^ "_r")
	   in
	       (var_c, var_r, addToVmap(vmap, var, var_c, var_r))
	   end
       

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
	   	   
       fun splitVar (var, vmap) =
	   (case (lookupVmap (var, vmap)) of
		NONE => newSplit (var, vmap)
	      | SOME (var_c, var_r) => (var_c, var_r, vmap))

   end

   fun makeConb cbnd = Con_b (Runtime,cbnd)

   fun getSbndNames n sbnds =
       let 
	   fun loop 0 (rest, labs, vars) = (rest, rev labs, rev vars)
	     | loop n (Il.SBND(lab,bnd)::rest, labs, vars) = 
	       let val var = (case bnd of
				  Il.BND_MOD (v,_) => v
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
				  Il.DEC_MOD (v,_) => v
				| Il.DEC_EXP (v,_) => v
				| Il.DEC_CON (v,_,_) => v)
	       in  loop (n-1) (rest, lab::labs, var::vars)
	       end
	     | loop _ _ = error "getSdecName: ran out of bnds"
       in   loop n (sbnds,[],[])
       end

   (* makeLabels.  Returns a list of labels for a tuple of length n. *)
   fun makeLabels n = Listops.map0count (fn n => Ilutil.generate_tuple_label(n+1)) n

   (* makeVars.  Returns a list of fresh variables of length n. *)
   fun makeVars n = Listops.map0count (fn _ => Name.fresh_var ()) n

   (* makeKindTuple.  
         Creates the kind for a "tuple" of types of length n. 
         1-tuples yield kind Word, rather than a record kind.
    *)
   fun makeKindTuple 1 = Type_k
     | makeKindTuple n =
       let
	   fun makeField i =
	       ((Ilutil.generate_tuple_label (i+1), Name.fresh_var()), 
		Type_k)
       in  
	   Record_k (Sequence.fromList (Listops.map0count makeField n))
       end



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

   fun rename_sdecs sdecs = 
       let open Il Ilutil
	   fun folder(SDEC(l,dec),s : (var * exp) list * (var * con) list * (var * mod) list) = 
	       case dec of
		   DEC_EXP(v,c) => let val c = con_subst_expconmodvar(c,#1 s, #2 s, #3 s)
				   in  (SDEC(l,DEC_EXP(v,c)),s)
				   end
		 | DEC_CON(v,k,c) => let val v' = Name.derived_var v
					 val k = kind_subst_expconmodvar(k,#1 s, #2 s, #3 s)
					 val c = (case c of
						      NONE => NONE
						    | SOME c => SOME(con_subst_expconmodvar(c,#1 s, #2 s, #3 s)))
				     in  (SDEC(l,DEC_CON(v',k,c)),
					  (#1 s, (v,CON_VAR v'):: (#2 s), #3 s))
				     end
		 | DEC_MOD(v,signat) => let val v' = Name.derived_var v
					    val signat = sig_subst_expconmodvar(signat,
										#1 s, #2 s, #3 s)
					in  (SDEC(l,DEC_MOD(v,signat)),s)
					end
       in  #1(foldl_acc folder ([],[],[]) sdecs)
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
       (case (Tyvar.tyvar_deref tyvar) of
	    NONE => error "(derefTyvar)  tyvar unset"
          | SOME x => x)

   fun derefOvar ovar = derefTyvar (Tyvar.ocon_deref ovar)

   (* selectFromKind.  Given a singleton-less record kind,
         project out the using the reversed list of labels
    *)
   fun selectFromKindStrip ctxt (k, lbls) = 
       let fun loop ctxt k [] = k
	     | loop ctxt k (lbl::lbls) = 
	       let val Record_k lvk_seq = k
		   val lvk_list = Sequence.toList lvk_seq
		   fun find ctxt [] = (print "selectfromkind could not find label = ";
				       Ppnil.pp_label lbl;
				       print " in kind = \n";
				       Ppnil.pp_kind k; print "\n";
				       error "selectFromKind could not find field")
		     | find ctxt (((l,v),k)::rest) = 
		       if (Name.eq_label(l,lbl))
			   then (ctxt,k)
		       else find ctxt rest
		   val (ctxt,k) = find ctxt lvk_list
	       in  loop ctxt k lbls
	       end
       in  loop ctxt k (rev lbls)
       end


   (* stripKind : Remove trivial arrow kinds that appear in a record kind 
      stripArrowKind: Remove trivial arrow kinds in a given kind;
                      returns bool indicating whether result is trivial arrow kind *)
   fun stripKind k : kind =
       if (!do_kill_cpart_of_functor)
	   then (case k of
		Singleton_k _ => k
	      | Type_k => k
	      | Record_k seq => let val lvk = Sequence.toList seq
				    val lvk = Listops.map_second stripArrowKind lvk
				    val lvk = List.mapPartial (fn (lv,(false,k)) => SOME(lv,k)
				  | _ => NONE) lvk
				in  Record_k (Sequence.fromList lvk)
				end
	      | Arrow_k _ => let val (trivial,k) = stripArrowKind k
			     in  k
			     end)
       else k

   and stripArrowKind k : bool * kind = 
       (case k of
	    Arrow_k(openness,vklist,k) =>
		let val vklist = map_second stripKind vklist
		    val k = stripKind k
		    val trivial = (case k of
				       Record_k seq => (Sequence.length seq) = 0
				     | _ => false)
		in  (trivial, Arrow_k(openness,vklist,k))
		end
	  | _ => (false, stripKind k))

   (* Helper functions to remove trivial constructors - no singletons permitted here *)
   fun killRecordKind' context k = 
       (!do_kill_cpart_of_functor andalso
       case k of
	   Record_k seq => let fun loop context [] = true
				 | loop context (((l,v),k)::rest) = 
				   (killArrowKind'' context k) andalso
				   let (* val context = Nilcontext.insert_kind(context,v,k) *)
				   in  loop context rest
				   end
			   in loop context (Sequence.toList seq)
			   end
	 | Singleton_k _ =>  error "killRecordKind' got singleton kind" 
(*			   killRecordKind' context (Normalize.make_shape context k)  *)
	 | _ => false)

   and killArrowKind' context k = 
       if (!do_kill_cpart_of_functor)
	   then (case k of
		     Arrow_k(openness,vklist,k) => 
			 let (* fun folder ((v,k),context) = Nilcontext.insert_kind(context,v,k)
			     val context = foldl folder context vklist *)
			 in  if (killRecordKind' context k)
				 then SOME(openness,vklist)
			     else NONE
			 end
		   | Singleton_k _ =>  error "killArrowKind' got singleton kind" 
(* killArrowKind' context (Normalize.make_shape context k)  *)
		   | _ => NONE)
       else NONE

   and killArrowKind'' context k = Option.isSome(killArrowKind' context k)


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

      {name_c, name_r, cbnd_cat, ebnd_cat, knd_c} =
         xmod ctx (il_mod, vmap_0, SOME var)

      Preconditions:  

        (1) var not free in ctx.

      Postconditions: 

        Let cbnds = flatten_catlist cbnd_cat, ebnds = flatten_catlist ebnd_cat

        (1) the compile-time part of mod is LET_C cbnds IN name_c END
        (2) the compile-time part has kind knd_c
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
       CONTEXT of {NILctx : Nilcontext.context,
		   sigmap : Il.signat Name.VarMap.map,
		   used   : VarSet.set ref,
		   vmap   : (var * var) Name.VarMap.map,
		   alias  : (var * label list) Name.VarMap.map,
		   memoized_mpath : (con * exp * kind) Name.PathMap.map}
       

in
    type splitting_context = splitting_context
    fun get_nilctxt (CONTEXT{NILctx,...}) = NILctx
    fun empty_splitting_context() = CONTEXT{NILctx = Nilcontext.empty(),
					    sigmap = Name.VarMap.empty,
					    used = ref Name.VarSet.empty,
					    vmap = Name.VarMap.empty,
					    alias = Name.VarMap.empty,
					    memoized_mpath = Name.PathMap.empty}

   fun print_splitting_context (CONTEXT{NILctx,sigmap,used,vmap,
					alias,memoized_mpath}) = 
       (Name.VarMap.appi (fn (v,(vc,vr)) => (Ppnil.pp_var v; print "  -->  "; 
					     Ppnil.pp_var vc; print ", ";
					     Ppnil.pp_var vr; print "\n")) vmap;
	print "\n";
	Nilcontext.print_context NILctx;
	print "\n")

   fun filter_NILctx (CONTEXT{NILctx,used,...}) = (NILctx, !used)

   fun find_sig(CONTEXT{sigmap,...},v) = Name.VarMap.find(sigmap,v)

   fun nilcontext_use_var(CONTEXT{NILctx,used,...},v) = 
       if (Name.VarSet.member(!used,v))
	   then ()
       else used := Name.VarSet.add(!used,v)
		
   fun nilcontext_find_kind(ctxt as CONTEXT{NILctx,used,...},v) = 
       let val _ = nilcontext_use_var(ctxt,v)
       in (SOME (Nilcontext.find_kind(NILctx,v))
	   handle Nilcontext.Unbound => NONE)
       end
			    
		
   fun nilcontext_find_shape(ctxt as CONTEXT{NILctx,used,...},v) = 
       let val _ = nilcontext_use_var(ctxt,v)
       in (SOME (Nilcontext.find_shape(NILctx,v))
	   handle Nilcontext.Unbound => NONE)
       end

   fun reduce_to_sum(CONTEXT{NILctx,...},c) = Normalize.reduceToSumtype(NILctx,c)
   fun nilcontext_con_hnf(CONTEXT{NILctx,...},c) = #2(Normalize.reduce_hnf(NILctx,c))
   fun nilcontext_print(CONTEXT{NILctx,...}) = Nilcontext.print_context NILctx

   val splitVar = fn (var,CONTEXT{NILctx,sigmap,
			     used,vmap,alias,memoized_mpath}) =>
                  let val (var_c,var_r,vmap') = splitVar(var,vmap)
		  in  ((var_c,var_r),
		       CONTEXT{NILctx=NILctx, sigmap=sigmap,
			       used=used, vmap=vmap',
			       memoized_mpath=memoized_mpath,
			       alias=alias})
		  end

   val chooseName = (fn (NONE, ctxt) => let val v = Name.fresh_var()
					    val ((vc,vr),ctxt) = splitVar(v,ctxt)
					in  (v,vc,vr,ctxt)
					end
		      | (SOME (var,var_c,var_r), ctxt) => (var, var_c, var_r, ctxt))

   val lookupVmap = fn (var, CONTEXT{vmap,...}) => lookupVmap (var,vmap)
   fun update_insert_sig  (CONTEXT{NILctx,sigmap,
			     used,vmap,alias,memoized_mpath}, v, hilsig) = 
       CONTEXT{NILctx=NILctx, sigmap=Name.VarMap.insert(sigmap,v,hilsig),
	       used=used, vmap=vmap,
	       memoized_mpath=memoized_mpath,alias=alias}


   fun update_NILctx_insert_kind(CONTEXT{NILctx,sigmap,vmap,used,
					 memoized_mpath,alias},v,k,k_shape_opt) = 
       let 
	   val NILctx' = Nilcontext.insert_kind(NILctx,v,k)
(*
val NILctx' = (case k_shape_opt of
			      NONE => Nilcontext.insert_kind(NILctx,v,k)
			    | SOME shape => Nilcontext.insert_kind_shape(NILctx,v,k,shape))
*)
       in  CONTEXT{NILctx=NILctx', sigmap=sigmap, vmap=vmap, used = used, 
		   memoized_mpath=memoized_mpath, alias = alias}
       end
   fun update_NILctx_insert_kind_equation(CONTEXT{NILctx,sigmap,vmap,used,
						  memoized_mpath,alias},
					  v,c,k_shape_opt) = 
       let val k = Singleton_k c
	   val NILctx' = Nilcontext.insert_kind(NILctx,v,k)
(*
	   val NILctx' = 
	       (case k_shape_opt of
		    NONE => Nilcontext.insert_kind_equation(NILctx,v,c,k)
		  | SOME shape => Nilcontext.insert_kind_shape_equation(NILctx,v,c,k,shape))
*)
       in  CONTEXT{NILctx=NILctx', sigmap=sigmap, vmap=vmap, used = used, 
		   memoized_mpath=memoized_mpath, alias = alias}
       end



   fun update_NILctx_insert_kind_list(ctxt,vklist) = 
       foldl (fn ((v,k),ctxt) => update_NILctx_insert_kind (ctxt,v,k,NONE)) ctxt vklist




    fun add_modvar_alias(CONTEXT{NILctx,sigmap,vmap,used,
				 memoized_mpath,alias},var,path) =
	let val alias' = Name.VarMap.insert(alias,var,path)
	in  CONTEXT{NILctx=NILctx, sigmap=sigmap, vmap=vmap, 
		    used=used,alias=alias',  memoized_mpath=memoized_mpath}
	end
    fun add_module_alias(CONTEXT{NILctx,sigmap,vmap,used,alias,
				 memoized_mpath},m,name_c,name_r,k1) = 
	case (extractPathLabels m) of
		  (Il.MOD_VAR v, labs) => 
		      let val p = (v,labs)
			  val memoized_mpath' = Name.PathMap.insert(memoized_mpath,p,(name_c,name_r,k1))
		      in  CONTEXT{NILctx=NILctx, sigmap=sigmap, vmap=vmap, 
				  used=used,alias=alias,  memoized_mpath=memoized_mpath'}
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




   fun killArrowKind str context k = killArrowKind'' (get_nilctxt context) k
       handle e => (print "exception in killArrowKind from ";
		    print str; print "\n";
		    raise e)

   fun generate_con_from_kind str context c k : bool * con =
       (case (killArrowKind' (get_nilctxt context) k 
       handle e => (print "exception in killArrowKind from generate_con_from_kind ";
		    print str; print "\n";
		    raise e)) of
	    SOME(openness,vklist) =>
		let val v = Name.fresh_named_var "generated_con"
		    val conbnd = Open_cb(v,vklist,Crecord_c[], Record_k(Sequence.fromList[]))
		in  (true,Let_c(Sequential,[conbnd],Var_c v))
		end
	  | NONE => (false, c))


       type con_result = con * kind
       type mod_result = {cbnd_cat : conbnd catlist,
			  ebnd_cat : bnd catlist,
			  name_c : con,
			  name_r : exp,
			  knd_c_context : kind,
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
				in  con_memo := Name.VarMap.insert(!con_memo,v,
								   Name.PathMap.insert(pathmap,path,res));
				    res
				end))

       fun lookup_mod_memo (context,preferred_name) (path as (v,lbls)) thunk =
	     (case Name.VarMap.find(!mod_memo,v) of
		NONE => (mod_memo := Name.VarMap.insert(!mod_memo,v,Name.PathMap.empty);
			 lookup_mod_memo (context,preferred_name) (v,lbls) thunk)
	      | SOME pathmap =>
		   (case Name.PathMap.find(pathmap,path) of
			SOME result => 
			    let val {name_c,name_r,knd_c_context,
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
						LIST[Exp_b(pr,Var_e vr)],
						update_NILctx_insert_kind_equation(context,pc,
						Var_c vc,SOME knd_c_context)))
					| _ => (name_c, name_r, LIST[], LIST[], context))

				val _ = (print "\nlookup_mod_memo of "; Ppnil.pp_var v;
				Ppnil.pp_list Ppnil.pp_label' lbls ("",".","",false);
				print " returning memoized result\n  knd_c_context = \n";
				Ppnil.pp_kind knd_c_context; print "\n\n")
			    in  {cbnd_cat = cbnd_cat, ebnd_cat = ebnd_cat,
				 name_c = name_c, name_r = name_r,
				 knd_c_context = knd_c_context,
				 
				 (* these fields are not cached *)
				 context = context} : mod_result
			    end
		      | NONE => let val res = thunk()
				    val _ = mod_memo := Name.VarMap.insert
					(!mod_memo,v,Name.PathMap.insert(pathmap,path,res))
				val {knd_c_context,...} = res
				val _ = (print "\nlookup_mod_memo of "; Ppnil.pp_var v;
				Ppnil.pp_list Ppnil.pp_label' lbls ("",".","",false);
				print " returning first-time result\n  knd_c_context = \n";
				Ppnil.pp_kind knd_c_context; print "\n\n")
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
		      | SOME(name_c,name_r,knd_c_context) =>
			let val (name_c,name_r,cbnd_cat,ebnd_cat,context) = 
				(case preferred_name of 
				  NONE => (name_c,name_r,LIST[],LIST[],context)
				| SOME(_,pc,pr) => 
					let val context = update_NILctx_insert_kind_equation(context,pc,
										    name_c, SOME knd_c_context)
					in  (Var_c pc, Var_e pr, 
						LIST[Con_cb(pc,name_c)], LIST[Exp_b(pr,name_r)],
						context)
					end)
			in {cbnd_cat=cbnd_cat, ebnd_cat=ebnd_cat, name_c=name_c, name_r=name_r,
				context = context, knd_c_context=knd_c_context}

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
		(Il.SIGNAT_STRUCTURE(popt,(Il.SDEC(l,Il.DEC_MOD (_,s)))::rest)) = 
		let val skip_project = 
		    ((Ilutil.is_datatype_lab l) orelse 
		     (case s of 
			  Il.SIGNAT_FUNCTOR(_,_,Il.SIGNAT_STRUCTURE(_,[Il.SDEC(l,_)]),_) => 
			      Name.eq_label(l,Ilutil.it_lab)
			| _ => false))
			  
		in  if skip_project
			then find_structure_paths m acc (Il.SIGNAT_STRUCTURE(popt,rest))
		    else	let val acc = (Il.MOD_PROJECT(m,l))::acc
				    val acc = find_structure_paths (Il.MOD_PROJECT(m,l)) acc s
				in  find_structure_paths m acc (Il.SIGNAT_STRUCTURE(popt,rest))
				end
		end
	      | find_structure_paths m acc (Il.SIGNAT_STRUCTURE(popt,_::rest)) =
			    find_structure_paths m acc (Il.SIGNAT_STRUCTURE(popt,rest))
	      | find_structure_paths m acc _ = acc
	    val rev_paths : Il.mod list = find_structure_paths (Il.MOD_VAR var_arg) [] il_signat
	val _ =  if (!debug)
		     then (print "preproject: there are "; 
			   print (Int.toString (length rev_paths)); print " paths\n")
		 else ()
	    fun folder (mpath,(cbnds,ebnds,context)) = 
		let val _ = (print "Working on "; Ppil.pp_mod mpath; print "\n")
		    fun loop (Il.MOD_VAR v) acc = (Name.var2name v) ^ acc
		      | loop (Il.MOD_PROJECT (m,l)) acc = loop m ("_" ^ (Name.label2name l) ^ acc)
		    val var = Name.fresh_named_var(loop mpath "")
		    val ((var_c, var_r), context) = splitVar (var, context)
		    val {cbnd_cat : conbnd catlist,
			 ebnd_cat,name_c,name_r,
			 knd_c_context,context} =
			  xmod context (mpath,SOME(var,var_c,var_r))
		    val cbnds = cbnd_cat::cbnds
		    val ebnds = ebnd_cat::ebnds
		    val context = add_module_alias(context,mpath,name_c,name_r,knd_c_context)
		in  (cbnds,ebnds,context)
		end
	    val (rev_cbnds,rev_ebnds,context) = foldr folder ([],[],context) rev_paths
	in  {cbnd_cat = APPEND (rev rev_cbnds), ebnd_cat = APPEND (rev rev_ebnds), context = context}
	end

   and xmod' context (il_mod as (Il.MOD_VAR var_mod), preferred_name) : mod_result = 
       let
	   val ((var_mod_c, var_mod_r), context) = splitVar (var_mod, context)
	   val _ = nilcontext_use_var(context,var_mod_r)

           val _ = if (!full_debug)
		       then (print "About to look up :\n";
			     Ppnil.pp_exp (Var_e var_mod_r);
			     print " and ";
			     Ppnil.pp_con (Var_c var_mod_c);
			     print "\n")
		   else ()

	   (* these are singleton free *)
           val knd_c_context =
	     case nilcontext_find_shape (context, var_mod_c) of
		 SOME knd => knd
		| _ => (print "Variable: "; Ppnil.pp_var var_mod_c;
			error "Constructor variable not found in context")

	   val (generated,gencon) = 
	        generate_con_from_kind "11" context (Var_c var_mod_c) knd_c_context

	   val (var_mod_c,cbnds,context) = 
	       if not generated 
		    then (var_mod_c,[],context)
	       else let val var_mod_c' = Name.derived_var var_mod_c
		    in  (var_mod_c',
			 [Con_cb(var_mod_c', gencon)],
			 update_NILctx_insert_kind_equation(context,var_mod_c',
							    Var_c var_mod_c,
							    SOME knd_c_context))
		    end

	   val (name_c, name_r) = 
	       (case preferred_name of
		    NONE => (Var_c var_mod_c, Var_e var_mod_r)
		  | SOME (_, name_c, name_r) => (Var_c name_c, Var_e name_r))

	   val (cbnd_cat, ebnd_cat, context) =
	       (case preferred_name of
		    NONE => (LIST cbnds, LIST nil, context)
		  | SOME (_, name_c, name_r) => 
			(APPEND[LIST cbnds, LIST[Con_cb(name_c, Var_c var_mod_c)]],
			 LIST [Exp_b (name_r, Var_e var_mod_r)],
			 update_NILctx_insert_kind_equation
			  (context,
			   name_c, Var_c var_mod_c, SOME knd_c_context)))

       in
	   {cbnd_cat = cbnd_cat,
	    ebnd_cat = ebnd_cat,
            name_c   = name_c,
            name_r   = name_r,
	    knd_c_context = knd_c_context,
	    context  = context}
       end

     | xmod' context (Il.MOD_APP(ilmod_fun, ilmod_arg), preferred_name) =
       let

	   val {cbnd_cat = cbnd_cat_fun,
		ebnd_cat = ebnd_cat_fun,
		name_c = name_fun_c,
                name_r = name_fun_r,
		knd_c_context = knd_fun_c_context,
		context = context
		} = xmod context (ilmod_fun, NONE)

	   val {cbnd_cat = cbnd_cat_arg,
		ebnd_cat = ebnd_cat_arg,
		name_c = name_arg_c,
		name_r = name_arg_r,
		knd_c_context = knd_arg_c_context,
		context = context
		} = xmod context (ilmod_arg, NONE)
	       
	   val (var, var_c, var_r, context) = chooseName (preferred_name, context)
	   val name_c = Var_c var_c
	   val name_r = Var_e var_r
	       
	   val Arrow_k(_, [_], con_body_kind_context) = knd_fun_c_context 
	   val knd_c_context = Subst.varConKindSubst var_c name_arg_c con_body_kind_context
		   
	   val cbnd_cat_new =
	       if (killArrowKind "1" context knd_fun_c_context)
		   then (LIST[Con_cb(var_c, Crecord_c[])])
	       else LIST[Con_cb(var_c, App_c(name_fun_c,[name_arg_c]))]
	   val cbnd_cat = APPEND[cbnd_cat_fun, cbnd_cat_arg, cbnd_cat_new]
	   val context = update_NILctx_insert_kind(context,var_c,
						   knd_c_context, SOME knd_c_context)

       
	   val ebnd_cat_new = LIST[Exp_b(var_r, 
					 Nilutil.makeAppE
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
	    knd_c_context = knd_c_context,
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

	   val _ = if (!omit_datatype_bindings)
		       then app (fn l => if (Ilutil.is_datatype_lab l)
					     then error "use of datatype labels detected"
					 else ()) lbls
		   else ()
	   val {cbnd_cat = cbnd_mod_cat, 
		ebnd_cat = ebnd_mod_cat,
		name_c   = name_mod_c, 
		name_r   = name_mod_r,
		knd_c_context = knd_mod_c_context,
		context  = context,
		...} = xmod context (il_module, NONE)

	   val (var_proj, var_proj_c, var_proj_r, context) = 
	       chooseName (preferred_name, context)


	   val name_proj_c = Var_c var_proj_c
	   val name_proj_r = Var_e var_proj_r

	   val con_proj_c = selectFromCon(name_mod_c, lbls)
	   val knd_proj_c = selectFromKindStrip (get_nilctxt context) (knd_mod_c_context,lbls)
	       
	   val (_,con_proj_c) = generate_con_from_kind "12" context con_proj_c knd_proj_c

	   val context = update_NILctx_insert_kind_equation(context,var_proj_c, 
							    con_proj_c, SOME knd_proj_c)
	   val cbnd_cat_new = LIST [Con_cb(var_proj_c, con_proj_c)]
           val cbnd_proj_cat = APPEND[cbnd_mod_cat,cbnd_cat_new]

	   val ebnd_cat_new = LIST [Exp_b(var_proj_r, 
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
	    knd_c_context = knd_proj_c,
	    context  = context}
       end

     | xmod' context (Il.MOD_FUNCTOR(var_arg, il_arg_signat, ilmod_body, ilmod_signat), 
		    preferred_name) =
       let
	   (* Pick the name of the result *)
	   val (var_fun, var_fun_c, var_fun_r, context) = 
	       chooseName (preferred_name, context)
           val name_fun_c = Var_c var_fun_c
	   val name_fun_r = Var_e var_fun_r


	   (* Split the argument parameter *)
	   val ((var_arg_c, var_arg_r), context') = splitVar (var_arg, context)
	   val (var_arg_c, var_arg_r, context',var_arg,ilmod_body) = 
		(case nilcontext_find_kind(context',var_arg_c) of
			NONE => (var_arg_c, var_arg_r, context',var_arg,ilmod_body)
		      | SOME _ =>
			let val _ = print "Duplicate functor var_arg in HIL\n"
			    val var_arg' = Name.derived_var var_arg
			    val ilmod_body = Ilutil.mod_subst_convar(ilmod_body,[(var_arg,Il.CON_VAR var_arg')])
		     	    val ((var_arg_c, var_arg_r), context') = splitVar (var_arg', context')
			in  (var_arg_c, var_arg_r, context',var_arg',ilmod_body)
			end)

	   val _ = clear_memo var_arg

	   val (knd_arg_context, knd_arg_shape, knd_arg_use, con_arg) = 
	         xsig context' (Var_c var_arg_c, il_arg_signat)

	   val context' = update_NILctx_insert_kind(context', var_arg_c, 
						    knd_arg_context, SOME knd_arg_shape)

	   val (_, _, _, con_res) = 
	         xsig context' (App_c(name_fun_c, [Var_c var_arg_c]), 
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
		knd_c_context = knd_body_c_context,
		context = _
		} = xmod context' (ilmod_body, NONE)


	   val cbnd_body_cat = APPEND[cbnd_preproject_cat, cbnd_body_cat]
	   val ebnd_body_cat = APPEND[ebnd_preproject_cat, ebnd_body_cat]

	   val (arrow, effect) = (Il.PARTIAL, Partial)

           val cbnds_body = flattenCatlist cbnd_body_cat
           val ebnds_body = flattenCatlist ebnd_body_cat

           val local_name_fun_c = Name.fresh_var ()

	   val cbnds_body_subst = 
	       let fun folder (cbnd,s) = let val (v,c) = Nilutil.extractCbnd cbnd
					 in  Subst.add s (v, Subst.substConInCon s c)
					 end
	       in  foldl folder (Subst.empty()) cbnds_body
	       end

	   val con_res' = Nilutil.makeLetC cbnds_body con_res

	   val knd_fun_c_context = Arrow_k(Open, [(var_arg_c, knd_arg_context)], knd_body_c_context)

           val cbnd_fun_cat = 
	       LIST[Open_cb(var_fun_c, [(var_arg_c, knd_arg_use)],
			    Nilutil.makeLetC cbnds_body name_body_c,
			    stripKind knd_body_c_context)]

	   val ebnd_fun_cat =  
	       LIST[Fixopen_b (Sequence.fromList
			      [(var_fun_r,
			       Function(effect, Leaf,
					 [(var_arg_c, knd_arg_use)],
					 true,
					 [(var_arg_r, con_arg)],
					 [],
					 Nilutil.makeLetE
					  ((map makeConb cbnds_body) @ ebnds_body)
					  name_body_r,
					 con_res'))])]


	   val context = update_NILctx_insert_kind(context, var_fun_c, 
						   knd_fun_c_context, SOME knd_fun_c_context)

       in
	   {cbnd_cat = cbnd_fun_cat,
            ebnd_cat = ebnd_fun_cat,
	    name_c = name_fun_c,
	    name_r = name_fun_r,
	    knd_c_context = knd_fun_c_context,
	    context = context}
       end
   
     | xmod' context (Il.MOD_STRUCTURE sbnds, preferred_name) =
       let
	   val {final_context = _, 
		cbnd_cat, ebnd_cat, record_c_con_items,
		record_c_knd_items_context, 
		record_r_exp_items : (label * exp) list} = 
		(xsbnds context sbnds)

	   val (var_str, var_str_c, var_str_r, context) = 
	       chooseName (preferred_name, context)

           val name_str_c = Var_c var_str_c
	   val name_str_r = Var_e var_str_r

	   val knd_str_c_context = Record_k (Sequence.fromList record_c_knd_items_context)

	   val con_str_c = Crecord_c record_c_con_items
	   val cbnd_cat_new = LIST [Con_cb(var_str_c, con_str_c)]
           val cbnd_str_cat = APPEND[cbnd_cat,cbnd_cat_new]

           val specialize =
	       (case (!elaborator_specific_optimizations, sbnds) of
		    (true, [Il.SBND(lab, Il.BND_EXP _)]) => Name.eq_label (lab, Ilutil.it_lab)
		  | _ => false)

	   val ebnd_cat_new = 
	       if specialize 
		   then LIST[Exp_b (var_str_r, #2(hd record_r_exp_items))]
	       else     LIST[Exp_b (var_str_r, 
				    Prim_e (NilPrimOp (record (map #1 record_r_exp_items)),
					    [], map #2 record_r_exp_items))]
	   val ebnd_str_cat = APPEND[ebnd_cat,ebnd_cat_new]


	   val context = update_NILctx_insert_kind(context, var_str_c, 
						   knd_str_c_context, NONE)

       in
	   {cbnd_cat = cbnd_str_cat,
	    ebnd_cat = ebnd_str_cat,
            name_c = name_str_c,
	    name_r = name_str_r,
	    knd_c_context = knd_str_c_context,
	    context = context}
       end

    | xmod' context (Il.MOD_LET (var_loc, il_loc_mod, il_body_mod),
		     preferred_name) =
       let

	   val _ = clear_memo var_loc

	   val ((var_loc_c, var_loc_r), context) = splitVar (var_loc, context)

	   val {cbnd_cat = cbnd_loc_cat,
		ebnd_cat = ebnd_loc_cat,
		knd_c_context = knd_loc_c_context,
		context = context,
		...} = xmod context (il_loc_mod, SOME (var_loc, var_loc_c, var_loc_r))

	   val {cbnd_cat = cbnd_body_cat,
		ebnd_cat = ebnd_body_cat,
		name_c = name_let_c,
		name_r = name_let_r,
		knd_c_context = knd_let_c_context,
		context = context} =  
	       xmod context (il_body_mod, preferred_name)

           val cbnd_let_cat = APPEND[cbnd_loc_cat, cbnd_body_cat]
           val ebnd_let_cat = APPEND[ebnd_loc_cat, ebnd_body_cat]

       in
	   {cbnd_cat = cbnd_let_cat,
	    ebnd_cat = ebnd_let_cat,
            name_c = name_let_c,
	    name_r = name_let_r,
	    knd_c_context = knd_let_c_context,
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
	record_c_knd_items_context = nil,
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
	       val num_functions = length fbnds
	       val (rest_il_sbnds', external_labels, external_vars) = 
		   getSbndNames num_functions rest_il_sbnds

       (* internal_vars = Variables to which the functions are bound
                        in the HIL fix-construct.
        il_functions = Bodies of the functions in this mutually-recursive group *)
               val (internal_vars, nil_functions) = 
		   let
		       val Let_e (_,[Fixopen_b nil_fn_set],_) = xexp context il_exp
		   in
		       Listops.unzip (Sequence.toList nil_fn_set)
		   end

	       (* check that internal_vars = external_vars *)
	       val _ = if (Listops.eq_list(Name.eq_var,external_vars,internal_vars))
			   then ()
		       else error "internal_vars != external_vars in FIX"


               val ebnd_entries = Listops.zip external_vars nil_functions
               val ebnd_types = map (Nilutil.function_type Open) nil_functions

	       val ebnds = [Fixopen_b (Sequence.fromList ebnd_entries)]


	       val {final_context, cbnd_cat, ebnd_cat, record_c_con_items,
		    record_c_knd_items_context,
		    record_r_exp_items} = 
		   (xsbnds context rest_il_sbnds') 

	   in
	       {final_context = final_context,
		cbnd_cat = cbnd_cat,
		ebnd_cat = APPEND [LIST ebnds, ebnd_cat],
		record_c_con_items   = record_c_con_items,
		record_c_knd_items_context = record_c_knd_items_context,
		record_r_exp_items = (Listops.zip external_labels 
				      (map Var_e external_vars)) @ record_r_exp_items}
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
					   (_, il_exp as Il.FIX(is_recur, arrow, fbnds)))],
				  il_body_signat)))
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
	       val num_functions = length fbnds
	       val (rest_il_sbnds', external_labels, external_vars) = 
		   getSbndNames num_functions rest_il_sbnds

       (* internal_vars = Variables to which the functions are bound
                        in the HIL fix-construct.
        il_functions = Bodies of the functions in this mutually-recursive group *)

	       val (external_vars,context) = foldl_acc splitVar context external_vars
	       val external_vars_c = map #1 external_vars
	       val external_vars_r = map #2 external_vars

	       val ((poly_var_c, poly_var_r), context') = splitVar (poly_var, context)
	       val (knd_arg_context, knd_arg_shape, knd_arg_use, con_arg) = 
		                  xsig context' (Var_c poly_var_c, il_arg_signat)

	       val context' = update_NILctx_insert_kind(context', poly_var_c, 
							knd_arg_context, SOME knd_arg_shape)

	       val Let_e (_, [Fixopen_b set], _) = xexp context' il_exp
               val (internal_vars, functions) = Listops.unzip (Sequence.toList set)


               val inner_vars = map 
		   (fn v => Name.fresh_named_var((Name.var2name v) ^ "_inner")) external_vars_r

	       fun wrap(current_internal_var, inner_var,e) = 
		   let 
		       fun mapper(internal_var,external_var_r) = 
			   if (Name.eq_var(internal_var,current_internal_var))
			       then Exp_b(internal_var, Var_e inner_var)
			   else Exp_b(internal_var, Nilutil.makeAppE 
				      (Var_e external_var_r)
				      [Var_c poly_var_c]
				      [Var_e poly_var_r]
				      [])
		       val bnds = Listops.map2 mapper (internal_vars, external_vars_r)
		   in  Nilutil.makeLetE bnds e
		   end

               fun reviseFunction (internal_var,
				   external_var_r, inner_var,
				   Function(effect,recursive,[],
					   _,[(arg_var,arg_con)],[],body,body_con)) =
		   let val body' = wrap(internal_var, inner_var, body)
		   in  (external_var_r,
		       Function(Total, Leaf, 
				[(poly_var_c, knd_arg_use)],
				false,
				[(poly_var_r, con_arg)],
				[],
				Let_e (Sequential,
				       [Fixopen_b
					(Sequence.fromList 
					 [(inner_var, Function(effect,recursive,[],
							  false,[(arg_var,arg_con)],[],
							  body',body_con))])],
				       Var_e inner_var),
				AllArrow_c(Open, effect, [], NONE, [arg_con], 0w0, body_con)))
		   end

               val nullfunction_c = 
		   let
		       val var'' = Name.fresh_var ()
		   in
		       Nilutil.makeLetC [Open_cb(var'', 
					 [(poly_var_c, knd_arg_use)], 
					 Crecord_c [],
					 Record_k (Sequence.fromList []))]
		       (Var_c var'')
		   end

               val nullfunction_k =
		   Arrow_k(Open, [(poly_var_c, knd_arg_context)], Record_k (Sequence.fromList []))

               val cbnds = (map (fn n_c => Con_cb(n_c, nullfunction_c))
			    external_vars_c)
		       

               val cbnd_knds = map (fn _ => nullfunction_k) external_vars_c

               val ebnd_entries = (Listops.map4 reviseFunction 
				   (internal_vars, external_vars_r, inner_vars, functions))
               val ebnd_types = map (Nilutil.function_type Open) functions


	       val ebnds = [Fixopen_b (Sequence.fromList ebnd_entries)]

               val context'' = 
		   foldl (fn ((v,k),context) => update_NILctx_insert_kind(context, v, k, SOME k))
		         context  (Listops.zip external_vars_c cbnd_knds)


               val dummy_vars = map (fn _ => Name.fresh_var()) external_vars_c
		    
	       val {final_context, cbnd_cat, ebnd_cat, record_c_con_items,
		    record_c_knd_items_context, 
		    record_r_exp_items} = xsbnds context'' rest_il_sbnds'
		    
	   in
	       {final_context = final_context,
		cbnd_cat = if !do_kill_cpart_of_functor
			       then cbnd_cat
			   else
			       APPEND [LIST cbnds, cbnd_cat],
		ebnd_cat = APPEND [LIST ebnds, ebnd_cat],
		record_c_con_items = if !do_kill_cpart_of_functor
					 then record_c_con_items
				     else (Listops.zip external_labels (map Var_c external_vars_c))
					 @ record_c_con_items,
  	        record_c_knd_items_context = (Listops.zip (Listops.zip external_labels dummy_vars) cbnd_knds)
		                             @ record_c_knd_items_context,
		record_r_exp_items = (Listops.zip external_labels (map Var_e external_vars_r))
		                     @ record_r_exp_items}
	   end
       else
	   xsbnds_rewrite_3 context il_sbnds

     | xsbnds_rewrite_2 context il_sbnds = xsbnds_rewrite_3 context il_sbnds

   and xsbnds_rewrite_3 context (Il.SBND(lbl, Il.BND_EXP(var, il_exp)) :: rest_il_sbnds) =
       let
	   val exp = xexp context il_exp


	   val {final_context, cbnd_cat, ebnd_cat, record_c_con_items,
		record_c_knd_items_context, 
		record_r_exp_items} = xsbnds context rest_il_sbnds
       in
	   {final_context = final_context,
	    cbnd_cat = cbnd_cat,
	    ebnd_cat = APPEND[LIST [Exp_b(var,exp)], ebnd_cat],
	    record_c_con_items = record_c_con_items,
	    record_c_knd_items_context = record_c_knd_items_context,
	    record_r_exp_items = (lbl, Var_e var) :: record_r_exp_items}
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

	   val (con, knd_context) = xcon context il_con
	   val kill_con = killArrowKind "2" context knd_context

           val context' = update_NILctx_insert_kind(context, var, 
						    knd_context, SOME knd_context) 

	   val {final_context, cbnd_cat, ebnd_cat, record_c_con_items,
		record_c_knd_items_context,
		record_r_exp_items} = xsbnds context' rest_il_sbnds
       in
	   {final_context = final_context,
	    cbnd_cat = CONS (Con_cb(var, con), cbnd_cat),
	    ebnd_cat = ebnd_cat,
	    record_c_con_items = if (kill_con)
				     then record_c_con_items
				 else (lbl, Var_c var) :: record_c_con_items,
	    record_c_knd_items_context = ((lbl, var), knd_context) :: record_c_knd_items_context, 
	    record_r_exp_items = record_r_exp_items}
       end

     | xsbnds_rewrite_3 context (Il.SBND(lbl, Il.BND_MOD(var, il_module))::rest_il_sbnds) =
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
			    val table = [(var, Il.MOD_VAR v)]
			    val Il.MOD_STRUCTURE rest' = 
				Ilutil.mod_subst_modvar(Il.MOD_STRUCTURE rest_il_sbnds,table)
			in (v,rest')
			end)


	   val ((var_c, var_r), context) = splitVar (var, context)
	       
	   val {cbnd_cat = cbnd_mod_cat, 
		ebnd_cat = ebnd_mod_cat,
		knd_c_context = knd_mod_c_context,
		context = context,
		name_c, name_r,
		...} = xmod context (il_module, SOME(var, var_c, var_r))


	   val context = (case (extractPathLabels il_module) of
			      (Il.MOD_VAR tovar,labs) => add_modvar_alias(context,var,(tovar,labs))
			    | _ => context)


	       
	   val {final_context, cbnd_cat, ebnd_cat, record_c_con_items,
		record_c_knd_items_context, 
		record_r_exp_items} = xsbnds context rest_il_sbnds

	   val kill_con = killArrowKind "3" context knd_mod_c_context

       in
	   {final_context = final_context,
	    cbnd_cat = APPEND[cbnd_mod_cat, cbnd_cat],
	    ebnd_cat = APPEND[ebnd_mod_cat, ebnd_cat],
	    record_c_con_items = if (kill_con)
				     then record_c_con_items
				 else (lbl, name_c) :: record_c_con_items,
	    record_c_knd_items_context = ((lbl, var_c), knd_mod_c_context) :: record_c_knd_items_context, 
	    record_r_exp_items = (lbl,name_r) :: record_r_exp_items}
       end

   and xflexinfo context (ref (Il.INDIRECT_FLEXINFO f)) = 
       xflexinfo context f
     | xflexinfo context (ref (Il.FLEXINFO(_,true, recs))) = 
       let
	   val (lbls, cons) = xrdecs context recs
	   val con = Prim_c(Record_c(lbls,NONE), cons) (* already sorted *)
	   val knd = Type_k
       in
	   (con, knd)
       end

   and xrdecs context [] = ([], [])
     | xrdecs context ((lab, il_con) :: rest) = 
       let
	   val (labs, cons) = xrdecs context rest
	   val (con, _) = xcon context il_con
       in
	   (lab :: labs, con :: cons)
       end

   (* Returns the translated NIL con and a singleton-less (thick) kind *)
   and xcon context il_con : con * kind =
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

   and xcon' context (il_con as (Il.CON_VAR var)) : con * kind = 
       let
	   val con = Var_c var
	   val kind_context = 
	       (case nilcontext_find_shape (context, var) of
		    SOME kinds => kinds
		  | NONE => (print "Could not find constructor variable ";
			     Ppil.pp_var var;
			     if (!debug)
				 then (print "in context:\n"; nilcontext_print context)
			     else ();
				 error "xcon: CON_VAR\n"))
	   val (_,con) = generate_con_from_kind "13" context con kind_context

       in
	   (con, kind_context)
       end

     | xcon' context (Il.CON_TYVAR tv) = xcon context (derefTyvar tv)

     | xcon' context (Il.CON_OVAR ov) = xcon context (derefOvar ov)

     | xcon' context (Il.CON_FLEXRECORD fr) = xflexinfo context fr

     | xcon' context (Il.CON_INT Prim.W64) =
       raise Util.BUG "64-bit integers not handled during/after phase-split"

     | xcon' context (Il.CON_UINT Prim.W64) =
       raise Util.BUG "64-bit integers not handled during/after phase-split"

     | xcon' context (Il.CON_INT intsize) =
       let
           val con = Prim_c (Int_c intsize, [])
           val knd = Type_k
       in
	   (con, knd)
       end

    (* there is no type distinction between signed/unsigned ints from NIL onwards *)
     | xcon' context (Il.CON_UINT intsize) = xcon' context (Il.CON_INT intsize)

     | xcon' context (Il.CON_FLOAT floatsize) = 
       let
	   val con = Prim_c (BoxFloat_c floatsize, [])
	   val knd = Type_k
       in
	   (con, knd)
       end

     | xcon' context (Il.CON_ARRAY il_con) = 
       let
	   val (con', _) = xcon context il_con 
	   val con = Prim_c (Array_c, [con'])
	   val knd = Type_k
       in  (con, knd)
       end

     | xcon' context (Il.CON_VECTOR il_con) = 
       let
	   val (con', _) = xcon context il_con 
	   val con = Prim_c (Vector_c, [con'])
	   val knd = Type_k
       in  (con, knd)
       end

     | xcon' context (Il.CON_ANY) = 
       let
	   val con = Prim_c(Exn_c, [])
	   val knd = Type_k
       in
	   (con, knd)
       end

     | xcon' context (Il.CON_REF il_con) = 
       let
	   val (con', _) = xcon context il_con
	   val con = Prim_c (Ref_c, [con'])
	   val knd = Type_k
       in
	   (con, knd)
       end

     | xcon' context (Il.CON_TAG il_con) = 
       let
	   val (con', _) = xcon context il_con
	   val con = Prim_c (Exntag_c, [con'])
	   val knd = Type_k
       in  (con, knd)
       end

     | xcon' context (Il.CON_ARROW (il_cons1, il_con2, closed, arr)) =
       let
	   val cons1 = map (#1 o (xcon context)) il_cons1
           val con2 = #1(xcon context il_con2)
           val con2 = (case (closed,il_con2) of
			   (true,Il.CON_FLOAT Prim.F64) => Prim_c (Float_c Prim.F64, [])
			 | _ => #1(xcon context il_con2))
	   val eff = xeffect (derefOneshot arr)
	   val con = if closed
			 then ExternArrow_c(cons1, con2)
		     else AllArrow_c(Open, eff, [], NONE, cons1,0w0,con2)
	   val knd_context = Type_k
       in  (con, knd_context)
       end

     | xcon' context (il_con as Il.CON_APP (il_con1, il_con2)) = 
       let
	   val (con1, knd1_context) = xcon context il_con1
           val (Arrow_k(_,[(v_arg,_)],body_context)) = knd1_context
           val (con2, _) = xcon context il_con2
	   val con = App_c(con1, [con2])
       in  (con, body_context)
       end


     | xcon' context (Il.CON_MU(Il.CON_FUN(vars, 
					   Il.CON_TUPLE_INJECT cons))) =
       let

	   fun is_bound v = (case nilcontext_find_shape (context, v) of
				 NONE => false | SOME _ => true)
	   val Il.CON_FUN(vars, Il.CON_TUPLE_INJECT cons) = 
	       Ilutil.rename_confun(is_bound,vars,Il.CON_TUPLE_INJECT cons)

	   val context' = update_NILctx_insert_kind_list(context,map (fn v => (v,Type_k)) vars)
	       
	   val cons'= map (#1 o (xcon context')) cons
	   val freevars = Listops.flatten (map Ilutil.con_free_convar cons)
	   val is_recur = Listops.orfold (fn v => Listops.member_eq(Name.eq_var,v,freevars)) vars

	   val con = Mu_c (is_recur,
			   Sequence.fromList (Listops.zip vars cons'))

	   val knd = Nilutil.kind_tuple(map (fn _ => Type_k) vars)
       in
	   (con, knd)
       end

     | xcon' context (Il.CON_MU(Il.CON_FUN([var], con))) =
       let
	   fun is_bound v = (case nilcontext_find_shape (context, v) of
				 NONE => false | SOME _ => true)
	   val Il.CON_FUN([var],con) = Ilutil.rename_confun(is_bound,[var],con)

	   val context' = update_NILctx_insert_kind(context, var, Type_k, SOME (Type_k))
	       
	   val (con',_) = xcon context' con
	   val freevars = Ilutil.con_free_convar con
	   val is_recur = Listops.member_eq(Name.eq_var,var,freevars)
	   val con = Mu_c (is_recur,Sequence.fromList [(var, con')])
	   val knd = Type_k
       in
	   (con, knd)
       end

     | xcon' context (Il.CON_RECORD rdecs) = 
       let
	   val (lbls, cons) = xrdecs context rdecs
	   val con = Prim_c (Record_c(lbls,NONE), cons)
	   val knd = Type_k
       in
	   (con, knd)
       end

     | xcon' context (Il.CON_FUN (vars, il_con1)) = 
       let
	   val context' = update_NILctx_insert_kind_list(context, map (fn v => (v,Type_k)) vars)

	   val (con1, knd1_context) = xcon context' il_con1
	   val (arg, con1, knd1_context) =
	       case vars of
		   [v] => ((v, Type_k), con1, knd1_context)
		 | _ => let fun mapper (n,_) = ((Nilutil.generate_tuple_label (n+1),
						 Name.fresh_var()),Type_k)
			    val arg_var = Name.fresh_var()
			    val arg_kind = Record_k(Sequence.toList
						    (Listops.mapcount mapper vars))
			    fun mapper (n,v) = 
				(v,Proj_c(Var_c arg_var, 
					  Nilutil.generate_tuple_label (n+1)))
			    val substlist = Listops.mapcount mapper vars
				
			    val con1' = Nilutil.makeLetC (map (fn (v,c) => Con_cb(v,c))
						  substlist) con1
			in  ((arg_var, arg_kind), con1', knd1_context)
			end
	   val knd1_use = stripKind knd1_context
	   val fun_name = Name.fresh_var ()
	   val con = Nilutil.makeLetC ([Open_cb(fun_name, [arg], con1, knd1_use)])
	       (Var_c fun_name)
	   val knd_context = Arrow_k(Open, [arg], knd1_context)
       in  (con, knd_context)
       end


     | xcon' context (Il.CON_SUM {carrier, noncarriers, special}) =
       let
	   val known = (case special of
			       NONE => NONE
			     | SOME i => SOME (Word32.fromInt i))
	   val (carrier_con,knd_context) = xcon' context carrier
	   val num_carriers = (case knd_context of
				   (Record_k seq) => length(Sequence.toList seq)
				 | Type_k => 1
				 | _ => error "CON_SUM: cannot have non-record and non-word kind")
	   val con = Prim_c (Sum_c {tagcount = Word32.fromInt noncarriers,
				    totalcount = Word32.fromInt(noncarriers + num_carriers),
				    known = known}, [carrier_con])
	   val knd = Type_k
       in
	   (con, knd)
       end

     | xcon' context (il_con as (Il.CON_TUPLE_INJECT il_cons)) = 
       let
	   val (cons, knds_context) = Listops.unzip (map (xcon context) il_cons)
	   val tuple_length = List.length cons
	   val labels = makeLabels tuple_length
	   val vars = makeVars tuple_length
	   val con = Crecord_c(Listops.zip labels cons)
	   val knd_context = Record_k (Sequence.fromList 
			       (Listops.zip (Listops.zip labels vars) knds_context))
       in
	   (con, knd_context)
       end

     | xcon' context (il_con as (Il.CON_TUPLE_PROJECT (i, il_con1))) = 
       let
	   val (con1, knd_context) = xcon context il_con1
	
	   val Record_k seq_context = knd_context

	   val lbl = Ilutil.generate_tuple_label(i+1)
	   val con = Proj_c(con1, lbl)
	   fun equal((l,_),(l',_)) = Name.eq_label (l,l')
	   val SOME knd_context = Sequence.lookup equal seq_context (lbl,Name.fresh_var())
	   val (_,con) = generate_con_from_kind "14" context con knd_context
		
       in (con, knd_context)
       end

     | xcon' context (il_con as (Il.CON_MODULE_PROJECT (modv, lbl))) = 
       let
	   val {cbnd_cat,name_c,knd_c_context,context,...} = 
	       xmod context (modv, NONE)
           val cbnd_list = flattenCatlist cbnd_cat

	   val proj_con = Proj_c (name_c, lbl)
	   val con = Nilutil.makeLetC cbnd_list proj_con

	   val knd_context = selectFromKindStrip (get_nilctxt context) (knd_c_context,[lbl])

       in
	   (con, knd_context)
       end
    
(*
     | xcon' _ c = (print "Error:  Unrecognized constructor:\n";
		    Ppil.pp_con c;
		    print "\n";
		    error "(xcon):  Unrecognized constructor")
*)

   
   and toFunction context (exp as Il.FIX _) =
       let
	   val Let_e (_, [Fixopen_b fns], Var_e var) = xexp context exp
       in
	   case	(Sequence.lookup (Name.eq_var) fns var) of
	       SOME (Function(_,_,[],false,[(v,c)],[],body,_)) => (v,c,body)
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
           val (con,_) = xcon context il_con
           val exps = map (xexp context) il_exps
       in  Const_e (Prim.array (con, Array.fromList exps))
       end

     | xvalue context (Prim.vector (il_con, v)) = 
       let
	   val il_exps = Array.foldr (op ::) nil v
           val (con, _) = xcon context il_con
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
	   val (con, _) = xcon context il_con
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
       xexp context (Ilutil.prim_etaexpand(prim,il_cons))

     | xexp' context (Il.ETAILPRIM (ilprim, il_cons)) = 
       xexp context (Ilutil.ilprim_etaexpand(ilprim,il_cons))

(* XXX need to handle floats *)
     | xexp' context (il_exp as (Il.PRIM (prim, il_cons, il_args))) = 
       let
	   open Prim
	   val cons = map (#1 o (xcon context)) il_cons
	   val args = map (xexp context) il_args
           val (effect,con) = 
	     case strip_arrow (Nilprimutil.get_type' prim cons)
	       of SOME (_,effect,_,_,_,_,con) => (effect,con)
		| _ => (perr_c (Nilprimutil.get_type' prim cons);
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
	   val args = map (xexp context) il_args
           val (effect,con) = 
	     case strip_arrow (Nilprimutil.get_iltype' ilprim cons)
	       of SOME (_,effect,_,_,_,_,con) => (effect,con)
		| _ => (perr_c (Nilprimutil.get_iltype' ilprim cons);
			error "Expected arrow constructor")
       in
	   Prim_e (PrimOp (xilprim ilprim), cons, args)
       end

     | xexp' context (Il.VAR var) = (nilcontext_use_var(context,var);
				     Var_e var)

     | xexp' context (il_exp as (Il.EXTERN_APP (il_con1,il_exp1, il_exps2))) =
       let
	   val exp1 = xexp context il_exp1
	   val exps2 = map (xexp context) il_exps2
	   val Il.CON_ARROW(cons2,_,_,_) = il_con1
	   fun mapper(e,Il.CON_FLOAT _) = Prim_e (NilPrimOp (unbox_float Prim.F64),[],[e])
	     | mapper(e,_) = e
	   val exps2 = Listops.map2 mapper (exps2,cons2)
       in  ExternApp_e (exp1, exps2)
       end

     | xexp' context (Il.APP (il_exp1, il_exp2)) = 
       let
	   val exp1 = xexp context il_exp1
	   val exp2 = xexp context il_exp2
       in  App_e (Open, exp1, [], [exp2], [])
       end	   

     | xexp' context (Il.FIX (is_recur, il_arrow, fbnds)) = 
       let
	   val fbnds'= xfbnds context (is_recur, fbnds)
           val set = Sequence.fromList fbnds'
           val names = map (fn (var,_) => Var_e var) fbnds'
           val num_names = List.length names
           val labels = makeLabels num_names
       in
	   if (num_names = 1) then
               Nilutil.makeLetE [Fixopen_b set] (hd names)
           else
	       Nilutil.makeLetE [Fixopen_b set]
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
	   val (sumcon,_) = xcon context il_con
       in  Prim_e (NilPrimOp (project_sum (TilWord32.fromInt i)), [sumcon], [exp])
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

     | xexp' context (Il.HANDLE (il_exp1, il_exp2)) = 
       let
	   val exp1 = xexp context il_exp1
	   val (bound, _, handler) = toFunction context il_exp2
       in
	   Handle_e (exp1, bound, handler)
       end

     | xexp' context (Il.RAISE (il_con, il_exp)) = 
       let
	   val exp = xexp context il_exp
	   val (con, _) = xcon context il_con
       in  Raise_e (exp, con)
       end

     | xexp' context (Il.LET (bnds, il_exp)) = 
       let
	   val {cbnd_cat, ebnd_cat, final_context=context'} = xbnds context bnds
           val cbnds = flattenCatlist cbnd_cat
           val ebnds = (map makeConb cbnds) @ (flattenCatlist ebnd_cat)
	   val exp = xexp context' il_exp
       in   Nilutil.makeLetE ebnds exp
       end

     | xexp' context (Il.NEW_STAMP il_con) = 
       let val (con, _) = xcon context il_con
       in  Prim_e(NilPrimOp make_exntag, [con], [])
       end

     | xexp' context (Il.EXN_INJECT (s, il_tag, il_exp)) =
       let
	   val tag = xexp context il_tag
	   val exp = xexp context il_exp
       in
           Prim_e (NilPrimOp (inj_exn s), [], [tag, exp])
       end

     | xexp' context (Il.ROLL (il_con, il_exp)) = 
       let
	   val (con, _) = xcon context il_con
	   val exp = xexp context il_exp
       in
	   Prim_e(NilPrimOp roll, [con], [exp])
       end

     | xexp' context (il_exp as (Il.UNROLL (il_mu_con, il_expanded_con, il_exp1))) = 
       let
	   val (mu_con, _) = xcon context il_mu_con
	   val exp = xexp context il_exp1
       in  Prim_e(NilPrimOp unroll, [mu_con], [exp])
       end

     | xexp' context (Il.INJ {sumtype, field, inject = eopt}) =
       let
	   val (sumcon,_) = xcon context sumtype
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
	   val (sumcon,_) = xcon context sumtype
	   val exp = xexp context il_arg
	   fun xarm (n, NONE ) = NONE
	     | xarm (n, SOME ilexp) = SOME(Word32.fromInt n, xexp context ilexp)
	   val arms = List.mapPartial (fn x => x) (mapcount xarm il_arms)
	   val default = Util.mapopt (xexp context) il_default
       
	in Switch_e(Sumsw_e {sumtype = sumcon,
			     bound = bound,
			     arg  = exp, arms = arms, 
			     default = default})
       end

     | xexp' context (e as Il.EXN_CASE {arg = il_exp, arms = il_arms, default = il_default, tipe}) =
       let
	   val exp = xexp context il_exp
           fun xarm (il_tag_exp, _, exp) =
	       let val tag = xexp context il_tag_exp
		   val (bound, _, handler) = toFunction context exp
	       in  (bound,(tag,handler))
	       end

	   val (vars,arms) = Listops.unzip (map xarm il_arms)
	   val bound = hd vars
	   val _ = if (List.all (fn v => Name.eq_var(v,bound)) vars)
		       then () else error "exn_case did not get same var in all arms"
	   val default = Util.mapopt (xexp context) il_default
       in
	   Switch_e(Exncase_e {	bound = bound,
				arg = exp, arms = arms,
				default = default})
       end

     | xexp' context (Il.MODULE_PROJECT (module, label)) =
       let

	   
	 val {cbnd_cat, ebnd_cat, name_r, ...} = xmod context (module, NONE)
	   
	 val specialize = (Name.eq_label(label, Ilutil.it_lab)) andalso 
	     ! elaborator_specific_optimizations
	     
	 val cbnds = flattenCatlist cbnd_cat
	 val bnds = (map makeConb cbnds) @ 
	     (flattenCatlist ebnd_cat)
	     
	 val let_body = 
	     if specialize then name_r
	     else Prim_e (NilPrimOp (select label), [], [name_r])

       in Nilutil.makeLetE bnds let_body
       end

     | xexp' context (Il.SEAL (exp,_)) = xexp context exp


   and xfbnds context (is_recur, fbnds) = 
       let
	   val recursive = if is_recur then Arbitrary else Leaf
	   fun mapper (Il.FBND(var, var', il_con1, il_con2, body)) = 
	       let
		   val (con1,_) = xcon context il_con1
		   val (con2,_) = xcon context il_con2
		   val body' = xexp context body
	       in  (var, Function(Partial, recursive, [],
				  false, [(var', con1)], [], body', con2))
	       end
       in  map mapper fbnds
       end

   handle e => (print "uncaught exception in xfbnds\n";
		raise e)


   and xbnds context bnds =
       let
	   val temporary_labels = makeLabels (length bnds)
	   val sbnds = map Il.SBND (Listops.zip temporary_labels bnds)

	   val {final_context, cbnd_cat, ebnd_cat, record_c_con_items,
		record_c_knd_items_context, 
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

     |  xsig' context (con0,Il.SIGNAT_OF il_module) = 
          let val {cbnd_cat = cbnd_mod_cat, 
		   ebnd_cat = ebnd_mod_cat,
		   name_c   = name_mod_c, 
		   name_r   = name_mod_r,
		   context  = context,
		   knd_c_context = knd_shape,
		   ...} = xmod context (il_module, NONE)

	      val cbnds = flattenCatlist cbnd_mod_cat
	      val cbnds' = map makeConb cbnds
	      val ebnds = flattenCatlist ebnd_mod_cat
	      val con_mod = Nilutil.makeLetC cbnds name_mod_c
	      val knd_context = Singleton_k con_mod
	      val knd_use = Singleton_k con_mod
	      val e = Nilutil.makeLetE
		      (cbnds' @ ebnds)
		      name_mod_r

	  in  (knd_context, knd_shape, knd_use, Typeof_c e)
	  end

     | xsig' context (con0,Il.SIGNAT_FUNCTOR (var, sig_dom, sig_rng, arrow))=
       let

	   val _ = clear_memo var

	   val is_polyfun_sig = 
	       (case sig_rng of
		    Il.SIGNAT_STRUCTURE(_,[Il.SDEC(it_lbl,Il.DEC_EXP _)]) => Name.eq_label(it_lbl,Ilutil.it_lab)
		  | _ => false)

	   val ((var_c, var_r), context) = splitVar (var, context)
	   val (knd_context, knd_shape, knd_use, con) = xsig context (Var_c var_c, sig_dom)
	   val context = update_NILctx_insert_kind(context, var_c, knd_context, NONE)
	       
	   val (knd'_context, knd'_shape, knd'_use, con') = xsig context (App_c(con0, [Var_c var_c]), sig_rng)
	       
       in
		 (Arrow_k (Open, [(var_c, knd_context)], knd'_context),
		  Arrow_k (Open, [(var_c, knd_shape)], knd'_shape),
		  Arrow_k (Open, [(var_c, knd_use)], knd'_use),
		  AllArrow_c (Open, xeffect arrow, [(var_c, knd_use)],
			      SOME[var_r],[con], 0w0, con'))

       end

     | xsig' context (con0, Il.SIGNAT_STRUCTURE (NONE,sdecs)) = xsig_struct context (con0,sdecs)
     | xsig' context (con0, Il.SIGNAT_STRUCTURE (SOME p,sdecs)) = 
       let val s = Il.SIGNAT_STRUCTURE(SOME p,sdecs)
	   val sig' = IlStatic.UnselfifySig (Ilcontext.empty_context) (p,s)
       in  xsig' context (con0, sig')
       end

     | xsig' context (con0, Il.SIGNAT_INLINE_STRUCTURE {code,...}) =
       let val s = IlStatic.GetModSig(Ilcontext.empty_context, Il.MOD_STRUCTURE code)
       in  xsig' context (con0, s)
       end


   and xsig_struct context (con0,sdecs) = 
       let
	   val {crdecs_context, crdecs_shape, crdecs_use, erdecs} =
	       xsdecs context (con0, Subst.empty(), sdecs)
	   val kind_context = Record_k (Sequence.fromList crdecs_context)
	   val kind_shape = Record_k (Sequence.fromList crdecs_shape)
	   val kind_use = Record_k (Sequence.fromList crdecs_use)
	   val (erlabs, ervars, ercons) = Listops.unzip3 erdecs
	   val type_r = Prim_c(Record_c (erlabs,SOME ervars), ercons)
	   val type_r = (case (!elaborator_specific_optimizations,sdecs,erdecs) of
			     (true,[Il.SDEC(it_lbl,_)],[(_,_,ercon)]) =>
				 (if (Name.eq_label(it_lbl,Ilutil.it_lab))
				      then ercon
				  else type_r)
			   | _ => type_r)
       in  (kind_context,kind_shape,kind_use, type_r)
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
            crdecs_context: type decs with singleton (thick) kinds,
	    crdecs_shape: type decs with singleton-less (thick) kinds 
	    crdecs_use: type decs with singleton (thin) kinds *)
   and xsdecs context (con,subst,sdecs) =
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

	   val sdecs = rename_sdecs sdecs
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
	   fun filter (Il.SDEC(lab,_)) =
	       not ((Ilutil.is_datatype_lab lab) andalso (! omit_datatype_bindings)) 
	   fun loop [] = []
	     | loop ((sdec as 
		     Il.SDEC(lab,Il.DEC_EXP(top_var,il_con))) :: rest) = 
	        if (Util.substring("polyfun",Name.label2string lab)) then
		   let
(*		       val _ = print "entered mono optimization case\n" *)
		       val clist = (case il_con of
					Il.CON_RECORD lclist => map #2 lclist
				      | Il.CON_ARROW _ => [il_con]
				      | _ => error "can't optimize mono fun")
		       val numFunctions = length clist
		       val (rest, external_labels, external_vars) = 
			   getSdecNames numFunctions rest
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
		       val (rest, external_labels, external_vars) = 
			   getSdecNames numFunctions rest
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
	   val sdecs = if !elaborator_specific_optimizations
			   then List.filter filter sdecs
		       else sdecs
       in  if !elaborator_specific_optimizations
	       then loop sdecs
	   else sdecs
       end
   
   and xsdecs' context (con0, _, []) = {crdecs_context = nil, crdecs_shape = nil, 
					crdecs_use = nil, erdecs = nil}

     | xsdecs' context (con0, subst,  
		    Il.SDEC(lbl, d as Il.DEC_MOD(var,signat)) :: rest) =
       let
	   val _ = clear_memo var
	   val ((var_c, var_r), context') = splitVar (var, context)
	   val (knd_context, knd_shape, knd_use, con) = xsig context' (Proj_c(con0, lbl), signat)
	       
	   val context' = update_NILctx_insert_kind(context', var_c, knd_context, NONE)

	   val {crdecs_context, crdecs_shape, crdecs_use, erdecs} =
	       xsdecs' context' (con0, Subst.add subst (var_c, Proj_c(con0, lbl)), rest)

	   val kill_con = killArrowKind "4" context knd_shape

       in  {crdecs_context = ((lbl, var_c), knd_context) :: crdecs_context,
	    crdecs_shape = ((lbl, var_c), knd_shape) :: crdecs_shape,
	    crdecs_use = if kill_con then crdecs_use
			 else ((lbl, var_c), knd_use) :: crdecs_use,
	    erdecs = (lbl,var_r,Subst.substConInCon subst con) :: erdecs}
       end

     | xsdecs' context(con0, subst, Il.SDEC(lbl, d as Il.DEC_EXP(var,il_con)) :: rest) =
       let
	   val (con,_) = xcon context il_con
	   val {crdecs_context, crdecs_shape, crdecs_use, erdecs} = xsdecs' context (con0, subst, rest)
       in
	   {crdecs_context = crdecs_context,
	    crdecs_shape = crdecs_shape,
	    crdecs_use = crdecs_use,
	    erdecs = (lbl,var,Subst.substConInCon subst con) :: erdecs}
       end

     | xsdecs' context (con0, subst, sdecs as Il.SDEC(lbl, d as Il.DEC_CON(var, knd, 
									maybecon))::rest)=
       let
	   val (knd_context,knd_shape,knd_use) = 
	       (case maybecon of
		    NONE => xkind context knd
		  | SOME il_con => 
			(let val (c,k_context) = xcon context il_con
			 in  (Singleton_k c, k_context, Singleton_k c)
			 end))

	   val context' = update_NILctx_insert_kind(context, var, knd_context, 
						    SOME knd_shape)
	   val {crdecs_context, crdecs_shape, crdecs_use, erdecs} = 
	       xsdecs' context' (con0, Subst.add subst (var, Proj_c(con0, lbl)),rest)

 	   val kill_con = killArrowKind "5" context knd_shape

      in   {crdecs_context = ((lbl, var), knd_context) :: crdecs_context,
	    crdecs_shape = ((lbl, var), knd_shape) :: crdecs_shape,
	    crdecs_use = if kill_con then crdecs_use else ((lbl, var), knd_use) :: crdecs_use,
	    erdecs = erdecs}
       end

   (* Returns a singleton-full (thick) kind and a singleton-less (thick) kind *)
   and xkind context (Il.KIND_TUPLE n) = 
       let val k = makeKindTuple n
       in (k,k,k)
       end
     | xkind context (Il.KIND_ARROW (n,m)) =
       let val k = Arrow_k (Open, 
			    [(Name.fresh_var(), 
			      if n = 1 then Type_k else makeKindTuple n)], 
			    makeKindTuple m)
       in  (k, k, k)
       end
     | xkind context (Il.KIND_INLINE (k,c)) = 
	 let val (con,knd_shape) = xcon context c
	 in  (Singleton_k con, knd_shape, Singleton_k con)
	 end

  fun make_cr_labels l = (Name.internal_label((Name.label2string l) ^ "_c"),
			  Name.internal_label((Name.label2string l) ^ "_r"))

   fun xHILctx HILctx =
       let open Il
	   fun dopc(v,l,pc,(imports,context)) = 
	       (case pc of
		    Il.PHRASE_CLASS_EXP (_,il_type) => 
			let val (nil_type,_) = xcon context il_type
			in  (ImportValue(l,v,nil_type)::imports, context)
			end
		  | Il.PHRASE_CLASS_CON (il_con, il_kind) => 
			let
			    val (kind_context, kind_shape, kind_use) = xkind context il_kind
			    val nil_con = 
				(case il_con of
				     Il.CON_VAR v' => 
					 if (Name.eq_var(v,v'))
					     then NONE
					 else ((SOME (#1 (xcon context il_con))) handle _ => NONE)
				   | _ => ((SOME (#1 (xcon context il_con))) handle _ => NONE))
			    val kill_con = killArrowKind "6" context kind_shape
				handle e => (print "error while importtype with kind_shape = ";
					     Ppnil.pp_kind kind_shape; print "\n";
					     raise e)
			    val it = ImportType(l,v,(case nil_con of
						 NONE => kind_use
					       | SOME c => Singleton_k c))
			in  (if kill_con then imports else it::imports,
			     case nil_con of 
				 NONE => update_NILctx_insert_kind(context, v, kind_context, NONE)
			       | SOME c => 
				     update_NILctx_insert_kind_equation(context, v, c, NONE))
			end
		  | Il.PHRASE_CLASS_MOD (_,il_sig) => 
			let
			    val (l_c,l_r) = make_cr_labels l
			    val ((v_c, v_r),context) = splitVar (v, context)
			    val (knd_context, knd_shape, knd_use, type_r) = xsig context (Var_c v_c, il_sig)
				
			    val context = update_NILctx_insert_kind(context, v_c, knd_context, NONE)
			    val kill_con = killArrowKind "6" context knd_shape
			    val iv = ImportValue(l_r,v_r,type_r)
			    val it = ImportType(l_c,v_c,knd_use)
			in  (if kill_con
				 then iv::imports
			     else iv::it::imports,
			     context)
			end
		  | Il.PHRASE_CLASS_SIG(v,il_sig) => 
			(imports,update_insert_sig(context,v,il_sig))
		  | _ => (imports,context))
	   fun folder (v,acc) =
	       let val SOME(l,pc) = Ilcontext.Context_Lookup'(HILctx,v)
	       in  if (Ilutil.is_datatype_lab l andalso (!omit_datatype_bindings)
		       orelse (Ilutil.is_nonexport_lab l))
		       then acc
		   else dopc(v,l,pc,acc)
	       end
	   val (rev_imports,context) = foldl folder ([],empty_splitting_context()) 
	                                  (Ilcontext.Context_Varlist HILctx)
       in  (rev rev_imports, context)
       end


    fun phasesplit (HILctx : Il.context, 
		    sbnd_entries : (Il.sbnd option * Il.context_entry) list) : Nil.module = 
	let
	    val _ = reset_memo()
	    open Nil Ilcontext Name IlStatic Ilutil

            (* we move all the externs into the context first:
	       this does not always work if the externs depend on
	       things in the sbnd_entries list *)

	    fun folder((SOME sbnd,Il.CONTEXT_SDEC sdec),(ctxt,sbnds)) = 
		(ctxt, (sbnd,sdec)::sbnds)
	      | folder((NONE, ce),(ctxt,sbnds)) = 
		(add_context_entries(ctxt,
		      [case ce of
			   Il.CONTEXT_SDEC(Il.SDEC(l,dec)) => 
			       Il.CONTEXT_SDEC(Il.SDEC(l,SelfifyDec ctxt dec))
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
		     app (fn v => (print "  "; Ppnil.pp_var v; print "\n")) 
		     (Ilcontext.Context_Varlist HILctx);
		     print "\n";
		     print "\nInitial HIL context:\n";
		     Ppil.pp_context HILctx;
		     print "\n")
		else
		    ()
		    
	    val (imports,initial_splitting_context) = 
		Stats.subtimer("Phase-spltting-context",xHILctx)
		HILctx
		
	    val _ = 
		if (!full_debug) then
		    (print "\nInitial NIL context:\n";
		     nilcontext_print initial_splitting_context;
		     print "\n")
		else
		    ()
	    val _ = msg "tonil.sml: initial splitting context computed\n"

	    (* Phase-split the bindings *)
	    val {cbnd_cat, ebnd_cat, final_context, ...} =
		xsbnds initial_splitting_context sbnds
	    val cbnds = map makeConb (flattenCatlist cbnd_cat)
	    val ebnds = flattenCatlist ebnd_cat
	    val bnds = cbnds @ ebnds
	    val (nil_initial_context,used) = filter_NILctx initial_splitting_context
	    val (nil_final_context,_) = filter_NILctx final_context
	    val _ = msg "---bindings phase-split\n" 

	     (* Filter out the unused imports *)
	    fun filter (ImportValue(l,v,c)) = VarSet.member(used,v)
	      | filter (ImportType(l,v,k)) = VarSet.member(used,v)
	    val imports = if (!killDeadImport) then List.filter filter imports else imports

	    (* create the exports *)
	    fun folder ((Il.SDEC(l,dec)),exports) = 
		    (case (not (is_nonexport_lab l), dec) of
			 (false,_) => exports
		       | (true,Il.DEC_EXP (v,_)) => (ExportValue(l,Var_e v)::exports)
		       | (true,Il.DEC_CON (v,_,_)) =>
			     let val c = Var_c v
				 val k = Nilcontext.find_kind(nil_final_context, v)
				     handle e => (print "exception while doing DEC_CON\n";
						  raise e)
			     in  if killArrowKind "7" final_context k
				     then exports
				 else (ExportType(l,c)::exports)
			     end
		       | (true,Il.DEC_MOD (v,s)) => 
			     let val (lc,lr) = make_cr_labels l
				 val ((vc,vr),_) = splitVar (v,final_context)
				 val exports = 
				     let 
					 val er = Var_e vr
					 val cc = Var_c vc
					 val kc = Nilcontext.find_shape(nil_final_context, vc)
				     handle e => (print "exception while doing DEC_MOD\n";
						  raise e)
				     in if killArrowKind "8" final_context kc
					    then 
						(ExportValue(lr,er)::exports)
					else (ExportValue(lr,er)::
					      ExportType(lc,cc)::
					      exports)
				     end
			     in  exports
			     end)
	    val exports : export_entry list = rev(foldl folder [] sdecs)
	    val _ = msg "---done phase-splitting\n" 

	    val nilmod = MODULE{bnds = bnds, 
				imports = imports,
				exports = exports}

	    val _ = reset_memo()
	in  nilmod
	end



end

