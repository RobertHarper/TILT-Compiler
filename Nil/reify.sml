(* Determines which constructors are used as data, creates proper traces, and possibly adds extra bindings for reified types *)

structure Reify :> REIFY =
struct
    open Util Nil

    val debug = Stats.ff("ReifyDebug")
    val rdebug = Stats.ff("ReifyMinDebug")
    fun error s = Util.error "reify.sml" s

    val float64 = NilDefs.ftype64
    val strip_arrow_norm = Normalize.strip_arrow_norm

    val foldl_acc = Listops.foldl_acc


    (* Generalize the idea of expressions inside bindings *)
    datatype letbody = BODY_EXP of Nil.exp
                     | BODY_EXPORTS of Nil.export_entry list

    (* psets are sets of variables that must be available at runtime *)
    val empty_pset = Name.VarSet.empty

    val print_pset =
	Name.VarSet.app
           (fn v => (print (Name.var2string v); print " "))

    val pset_add_list = Name.VarSet.addList
    val pset_member = Name.VarSet.member
    val pset_add_pset = Name.VarSet.union

    (* Some expressions don't carry enough typing information to be processed here (i.e., they're "analysis terms") *)
    (* This function tries to extract a type from an option, and raises an error to indicate insufficient information if none is there *)
    fun extract NONE = error "Insufficient information to reify constructor"
      | extract (SOME x) = x

    fun type_of (D, e) = Normalize.type_of (D, e)

    (* reify_con_rt
         post: returned pset extends input pset, ensuring that
               all the free variables of c will be available at run-time
     *)
    fun reify_con_rt (c, pset) =
      let
         val free_cvars = NilUtil.freeConVarInCon(false, 0, c)
      in
         pset_add_pset (pset, free_cvars)
      end

    and reify_cons_rt ([], pset) = pset
      | reify_cons_rt (c::cs, pset) =
          reify_cons_rt (cs, reify_con_rt (c, pset))

    fun reify_con Compiletime  (_, pset) = pset
      | reify_con Runtime (c, pset) = reify_con_rt (c, pset)

    fun decide_con_b_phase ctxt (Con_cb (v,c), pset) =
          if pset_member (pset, v) then
             (Runtime, reify_con_rt (c, pset))
          else
             (Compiletime, pset)
      | decide_con_b_phase ctxt (Open_cb (v,_,c), pset) =
          if pset_member (pset, v) then
             (Runtime, reify_con_rt (c, pset))
          else
             (Compiletime, pset)
      | decide_con_b_phase ctxt (Code_cb (v,_,c), pset) =
          if pset_member (pset, v) then
             (Runtime, reify_con_rt (c, pset))
          else
             (Compiletime, pset)

    (* Update the trace information for a constructor, which may generate extra bindings and add to the pset *)
    fun do_reify (ctxt, con, nt, pset) =
      let
	fun doit con =
	  case TraceOps.get_trace (ctxt, con) of
	    SOME tinfo =>
	      (TraceKnown tinfo, [],
	       pset_add_pset (pset, TraceOps.get_free_vars' tinfo))
	  | NONE =>
	      let
		val v' = Name.fresh_named_var "reify"
		val con = NilRename.renameCon con
		val pset' = reify_con_rt(con,pset)
	      in
		(TraceCompute v',
		 [Con_b(Runtime,Con_cb (v', con))], pset')
	      end

	(*Even if the trace is valid, specialize or inline may have exposed
	 * new definitions that mean we can do better (in fact, tortl expects
	 * us to do better.)  So even if the trace is valid, we call get_trace
	 * on the current trace info.  get_trace will at worst return the same thing,
	 * but may be able to improve on it.
	 *)

      in case (TraceOps.valid_trace (ctxt,nt),nt)
	   of (false,_) => doit con
	    | (_,TraceCompute v) => doit (Var_c v)
	    | (_,TraceKnown (TraceInfo.Compute p)) => doit (NilDefs.path2con p)
	    | (_,ti) => (ti,[],pset)
      end

    fun reify_exp ctxt (e as Var_e v, pset) = (e, pset)
      | reify_exp ctxt (e as Const_e _, pset) = (e, pset)
      | reify_exp ctxt (Prim_e (p, trs, cons, exps), pset) =
	  let

	    fun reify1 (con,tr,(bnds,pset)) =
	      let val (tr,bnd,pset) = do_reify(ctxt,con,tr,pset)
	      in (con,tr,(bnd @ bnds,pset))
	      end

	    fun reify_record_gctag_con (trs,c,pset) =
	      (case #2 (Normalize.reduce_hnf (ctxt,c)) of
		 Prim_c (Record_c _,cons) =>
		   let val (_,trs,(bnds,pset)) = Listops.foldl_acc2 reify1 ([],pset) (cons,trs)
		   in (bnds,trs,pset)
		   end
	       | _ => error "record tag created with non record type")

             val (bnds,trs,pset') =
	       case p of
		 NilPrimOp mk_record_gctag     => reify_record_gctag_con(trs,hd cons,pset)
	       | NilPrimOp mk_sum_known_gctag  =>
		   let
		     val carrier = NilUtil.sum_project_carrier_type (#2 (Normalize.reduce_hnf (ctxt,hd cons)))
		     val (tr,bnds,pset) = do_reify(ctxt,carrier,hd trs,pset)
		   in (bnds,[tr],pset)
		   end
		 | _ =>
                   ([],trs,
		    if (NilDefs.allprim_uses_carg p) then (* Only reify params for primops that use con args as data *)
		      reify_cons_rt (cons, pset)
		    else
		      pset)

             val (exps', pset'') = reify_exps ctxt (exps, pset')
	  in
             (NilUtil.makeLetE Sequential bnds (Prim_e (p, trs,cons, exps'))
	      , pset'')
	  end

      | reify_exp ctxt (App_e (openness, f, cons, exps0, exps1), pset) =
	  let
             val (f', pset) = reify_exp ctxt (f, pset)
             val pset = reify_cons_rt (cons, pset)
             val (exps0', pset) = reify_exps ctxt (exps0, pset)
             val (exps1', pset) = reify_exps ctxt (exps1, pset)
          in
             (App_e (openness, f', cons, exps0', exps1'), pset)
	  end

      | reify_exp ctxt (ExternApp_e (e, es), pset) =
          let
             val (e', pset) = reify_exp ctxt (e, pset)
             val (es', pset) = reify_exps ctxt (es, pset)
          in
             (ExternApp_e (e',es'), pset)
          end

      | reify_exp ctxt (Raise_e (e, c), pset) =
          let
              val (e', pset) = reify_exp ctxt (e, pset)
          in
              (Raise_e (e', c), pset)
          end

      | reify_exp ctxt (Handle_e {body,bound,handler,result_type}, pset) =
          let
              val (body', pset) = reify_exp ctxt (body, pset)
              val ctxt = NilContext.insert_con(ctxt, bound, Prim_c(Exn_c,[]))
              val (handler', pset) = reify_exp ctxt (handler, pset)
          in
	      (Handle_e {body = body', bound = bound,
			handler = handler', result_type = result_type},
	       pset)
          end

      | reify_exp ctxt (Let_e (Sequential, bs, e), pset) =
          let
	      val (bs', BODY_EXP e', pset) =
                reify_seq_bnds ctxt (bs, BODY_EXP e, pset)
	  in
              (Let_e (Sequential, bs', e'), pset)
	  end

      | reify_exp ctxt (Let_e (Parallel, bnds, exp), pset) =
          let
              (* XXX:  Correct, but turning parallel bindings into
                       sequential bindings is probably sub-optimal! *)
	      val (bnds', BODY_EXP exp', pset) =
		  reify_seq_bnds ctxt (bnds, BODY_EXP exp, pset)
	  in
              (Let_e (Sequential, bnds', exp'), pset)
	  end

      | reify_exp ctxt (Switch_e(Intsw_e {arg,size,arms,default,result_type}),
                       pset) =
          let
              val (arg',pset) = reify_exp ctxt (arg, pset)
              val (arms',pset) = reify_int_arms ctxt (arms, pset)
              val (default',pset) = reify_exp_option ctxt (default,pset)
	      (* result_type is here for typechecking only, so we don't
	         need to reify it *)
          in
              (Switch_e(Intsw_e{arg = arg', size = size,
                                arms = arms', default = default',
				result_type = result_type}),
               pset)
          end

      | reify_exp ctxt (Switch_e (Sumsw_e {arg, sumtype, bound,
                                          arms,default, result_type}), pset) =
          let
              val (arg',pset) = reify_exp ctxt (arg, pset)
              val (true,sumtype') = Normalize.reduce_hnf (ctxt, sumtype)
              val (bnds, arms', pset) =
                    reify_sum_arms ctxt (arms, bound, sumtype', pset)
              val (default',pset) = reify_exp_option ctxt (default, pset)
          in
              (NilUtil.makeLetE Sequential bnds
	       (Switch_e (Sumsw_e{arg = arg', sumtype = sumtype,
				  bound = bound, arms = arms',
				  default = default',
				  result_type = result_type})),
               pset)
          end

      | reify_exp ctxt (Switch_e (Exncase_e {arg, bound, arms,
                                            default, result_type}), pset) =
          let
             val (arg', pset) = reify_exp ctxt (arg, pset)
             val (bnds, arms', pset) = reify_exn_arms ctxt (arms, bound, pset)
             val (default',pset) = reify_exp_option ctxt (default, pset)
          in
	     (NilUtil.makeLetE Sequential bnds
	      (Switch_e (Exncase_e {arg = arg', bound = bound,
				    arms = arms', default = default',
				    result_type = result_type})),
              pset)
          end

      | reify_exp ctxt (Switch_e (Typecase_e {arg, arms, default, result_type}), pset) =

          let val pset = reify_con_rt(arg,pset)
	      val (arms', pset) = reify_typecase_arms ctxt (arms, pset)
	      val (default',pset) = reify_exp ctxt (default, pset)
	  in  (Switch_e (Typecase_e {arg = arg, arms = arms',
				    default = default', result_type = result_type}),
	      pset)
	  end

      | reify_exp ctxt (Switch_e (Ifthenelse_e _), pset) = error "Ifthenelse_e not implemented yet"

      | reify_exp ctxt (e_and_pset as (ForgetKnown_e _,_))         = e_and_pset
      | reify_exp ctxt (e_and_pset as (Fold_e (vars,from,to),_))   = e_and_pset
      | reify_exp ctxt (e_and_pset as (Unfold_e (vars,from,to),_)) = e_and_pset
      | reify_exp ctxt (Coerce_e (coercion,cargs,exp),pset) =
	let
	    (* Since we plan to erase the coercion application entirely, *)
	    (* the constructor arguments need not be reified.            *)
	    val (coercion,pset) = reify_exp ctxt (coercion,pset)
	    val (exp,pset) = reify_exp ctxt (exp,pset)
	in
	    (Coerce_e (coercion,cargs,exp),pset)
	end

    and reify_typecase_arms ctxt ([], pset) = ([],pset)
      | reify_typecase_arms ctxt ((pc,vklist,e)::arms, pset) =
	let val (arms',pset) = reify_typecase_arms ctxt (arms,pset)
	    val ctxt = NilContext.insert_kind_list(ctxt,vklist)
	    val (e',pset) = reify_exp ctxt (e,pset)
	in  ((pc,vklist,e')::arms',pset)
	end

    and reify_seq_bnds ctxt ([], BODY_EXP e, pset) =
           let
              val (e', pset) = reify_exp ctxt (e, pset)
           in
              ([], BODY_EXP e', pset)
           end

      | reify_seq_bnds ctxt ([], BODY_EXPORTS es, pset) =
           let
              val pset = reify_exports ctxt (es, pset)
           in
              ([], BODY_EXPORTS es, pset)
           end

      | reify_seq_bnds ctxt ((bnd as Exp_b (v,nt,e))::bs, body, pset) =
           let
              val t1 = type_of (ctxt, e)
              val ctxt' = NilContext.insert_con(ctxt, v, t1)
              val (bs', body', pset) = reify_seq_bnds ctxt' (bs, body, pset)
              val (e', pset') = reify_exp ctxt (e, pset)

	      val (nt', new_bnds, pset'') = do_reify(ctxt, t1, nt, pset')

	      val bnds = new_bnds @ (Exp_b (v, nt',e') :: bs')
           in
              (bnds, body', pset'')
           end

      | reify_seq_bnds ctxt ((Con_b (_,cb))::bs, body, pset) =
           let
              val (v,c) = NilUtil.extractCbnd cb
              val ctxt = NilContext.insert_kind(ctxt, v, Single_k c)
              val (bs', body', pset) = reify_seq_bnds ctxt (bs, body, pset)
              val (phase, pset) = decide_con_b_phase ctxt (cb, pset)
           in
              ((Con_b (phase,cb)) :: bs', body', pset)
           end

      | reify_seq_bnds ctxt ((Fixopen_b vfseq)::bs, body, pset) =
           let
              val (vfseq', bnds', pset, ctxt) = reify_vfseq ctxt (vfseq, Open, pset)
              val (bs', body', pset) = reify_seq_bnds ctxt (bs, body, pset)
           in
              (bnds' @ ((Fixopen_b vfseq'):: bs'), body', pset)
           end

      | reify_seq_bnds ctxt ((Fixcode_b vfseq)::bs, body, pset) =
           let
              val (vfseq', bnds', pset, ctxt) = reify_vfseq ctxt (vfseq, Code, pset)
              val (bs', body', pset) = reify_seq_bnds ctxt (bs, body, pset)
           in
              (bnds' @ ((Fixcode_b vfseq')::bs'), body', pset)
           end

      | reify_seq_bnds ctxt ((Fixclosure_b (recur,vcseq))::bs, body, pset) =
           let
              val (vcseq', pset, ctxt) = reify_vclseq ctxt (vcseq, pset)
              val (bs', body', pset) = reify_seq_bnds ctxt (bs, body, pset)
           in
              ((Fixclosure_b (recur,vcseq'))::bs', body', pset)
           end

    and reify_exp_option ctxt (NONE,pset) = (NONE, pset)
      | reify_exp_option ctxt (SOME e, pset) =
          let
             val (e', pset) = reify_exp ctxt (e,pset)
          in
             (SOME e', pset)
          end

    and reify_one_exps ctxt (e, pset) =
          let
             val (e', pset) = reify_exp ctxt (e,pset)
          in
             ([e'], pset)
          end

    and reify_exps ctxt ([], pset) = ([], pset)
      | reify_exps ctxt (e::es, pset) =
          let
             val (e', pset) = reify_exp ctxt (e,pset)
             val (es', pset) = reify_exps ctxt (es, pset)
          in
             (e'::es', pset)
          end

    and reify_int_arms ctxt ([], pset) = ([], pset)
      | reify_int_arms ctxt ((w,e)::arms, pset) =
          let
             val (arms', pset) = reify_int_arms ctxt (arms, pset)
             val (e', pset) = reify_exp ctxt (e, pset)
          in
             ((w,e')::arms', pset)
          end

    (* pre: c is a sum type in whnf *)
    and reify_sum_arms ctxt ([], _, _, pset) = ([], [], pset)
      | reify_sum_arms ctxt ((w,nt,e)::arms, v, c, pset) =
          let
             val (bnds, arms', pset) = reify_sum_arms ctxt (arms, v, c, pset)
             val t = NilUtil.convert_sum_to_special(c, w)
             val ctxt = NilContext.insert_con(ctxt, v, t)
	     val (nt', new_bnds, pset) = do_reify (ctxt, t, nt, pset)
             val (e', pset) = reify_exp ctxt (e, pset)
          in
             (new_bnds @ bnds, (w,nt',e')::arms', pset)
          end

    and reify_exn_arms ctxt ([], _, pset) = ([], [], pset)
      | reify_exn_arms ctxt ((e1,nt,e2)::arms, v, pset) =
          let
             val (bnds, arms', pset) = reify_exn_arms ctxt (arms, v, pset)
             val (e1', pset) = reify_exp ctxt (e1, pset)
             val tagcon = type_of(ctxt,e1)
	     val (_,Prim_c(Exntag_c, [con])) = Normalize.reduce_hnf(ctxt,tagcon)
	     val (nt', new_bnds, pset) = do_reify (ctxt, con, nt, pset)
	     val ctxt = NilContext.insert_con(ctxt, v, con)
             val (e2', pset) = reify_exp ctxt (e2, pset)
          in
             (new_bnds @ bnds, (e1',nt',e2')::arms', pset)
          end

    and reify_vfseq ctxt (vfseq, openness, pset) =
          let
             val vflist = Sequence.toList vfseq
             val ctxt = NilContext.insert_con_list
                         (ctxt, map #1 vflist)


             fun folder (((f, c),
			Function{effect=eff,recursive=recur,
				 tFormals=vksF,eFormals=vtcsF,
				 fFormals=fsF,body=e,...}), (bnds, pset))=
                   let
		      val arr = strip_arrow_norm ctxt c
		      val {tFormals=vksA,eFormals=vtcsA,fFormals=fsA,body_type,...} =
			  NilUtil.rename_arrow (arr, vksF)

                      val ctxt = NilContext.insert_kind_list (ctxt, vksA)

                      val vks_length = List.length vksA

                      val error_message =
		       "reify_vflist: Cannot hoist from Lambda-lambda function"

                      fun folder' ((v,nt,c), (bnds, pset)) =
	                    let
			       val (nt', new_bnds, pset') =
				   do_reify(ctxt, c, nt, pset)

			       val _ =
				   if (vks_length > 0) then
				       (case new_bnds of
					    [] => ()
					  | _ =>  error error_message)
				   else
				       ()

			       val bnds' = new_bnds @ bnds
                            in
				((v,nt',c), (bnds', pset'))
                            end

                      val (vtcs', (bnds', pset)) = foldl_acc folder' ([], pset) (ListPair.map (fn (c, (v, tr)) => (v, tr, c)) (vtcsA, vtcsF))
                      val ctxt = NilContext.insert_con_list (ctxt, map (fn (v, _, c) =>
									(v, c)) vtcs')
		      val ctxt = foldl (fn (v, ctxt) => NilContext.insert_con(ctxt, v, float64)) ctxt fsF

                      val (e', pset) = reify_exp ctxt (e, pset)
                   in

                      (((f, c), Function{effect=eff,recursive=recur,
				    tFormals=vksF,eFormals = map (fn (v,nt,_) => (v,nt)) vtcs',fFormals=fsF,
				    body=e'}),
                       (bnds' @ bnds, pset))
                   end

             val (vflist', (bnds', pset)) = foldl_acc folder (nil, pset) vflist
          in
             (Sequence.fromList vflist', bnds', pset, ctxt)
          end

   and reify_vclseq ctxt (vclseq, pset) =
          let
             val vcllist = Sequence.toList vclseq
             val ctxt = NilContext.insert_con_list (ctxt,
                          (map (fn ((v,tipe),{...}) => (v, tipe)) vcllist))

             fun folder (((v,c),{code, cenv, venv}), pset) =
                   let
                      val (venv', pset) = reify_exp ctxt (venv, pset)
                   in
                      (((v,c),{code = code, cenv = cenv, venv = venv'}), pset)
                   end

             val (vcllist', pset) = foldl_acc folder pset vcllist
          in
             (Sequence.fromList vcllist', pset, ctxt)
          end

    and reify_exports ctxt ([], pset) = pset
      | reify_exports ctxt ((ExportType (l, v))::exports, pset) =
	let val pset = pset_add_list(pset,[v])
	in  reify_exports ctxt (exports, pset)
	end
      | reify_exports ctxt ((ExportValue _)::exports, pset) =
	reify_exports ctxt (exports, pset)

    fun reify_mod' ([], ctxt, MODULE {bnds, exports, ...}) =
	let
	    val (bnds', BODY_EXPORTS exports', pset) =
                  reify_seq_bnds ctxt (bnds, BODY_EXPORTS exports, empty_pset)
	in
	    (bnds', exports', pset, ctxt, [])
	end
      | reify_mod' ((imp as ImportType (l, v, k)) :: is,
		       ctxt, module) =
	let
	    val ctxt = NilContext.insert_kind(ctxt, v, k)
	    val ctxt = NilContext.insert_label(ctxt, l, v)

	    val (bnds', exports', pset, ctxt, is') =
		reify_mod' (is, ctxt, module)
	in
	    (bnds', exports', pset, ctxt, imp :: is')
	end
      | reify_mod' (ImportBnd (_, cb) :: is, ctxt, module) =
	let
	    val (v, k) = NilStatic.kind_of_cbnd (ctxt, cb)
	    val ctxt = NilContext.insert_kind(ctxt, v, k)

	    val (bnds', exports', pset, ctxt, is') =
		reify_mod' (is, ctxt, module)

	    val (phase, pset) = decide_con_b_phase ctxt (cb, pset)
	in
	    (bnds', exports', pset, ctxt, ImportBnd (phase, cb) :: is')
	end
      | reify_mod' (ImportValue (l, v, nt, c) :: is, ctxt, module) =
	let
	    val nt' =
		if (TraceOps.valid_trace (ctxt, nt)) then
		    nt
		else
		    (case TraceOps.get_trace(ctxt, c) of
			 SOME tinfo => TraceKnown tinfo
		       | NONE => TraceUnknown)

	    val imp' = ImportValue(l, v, nt', c)
	    val ctxt = NilContext.insert_con(ctxt, v, c)
	    val ctxt = NilContext.insert_label(ctxt, l, v)

	    val (bnds', exports', pset, ctxt, is') =
		reify_mod' (is, ctxt, module)
	in
	    (bnds', exports', pset, ctxt, imp' :: is')
	end

    fun reify_mod (nilmod as MODULE {imports, ...}) =
        let val (bnds', exports', pset, ctxt, imports') =
	       reify_mod' (imports, NilContext.empty (), nilmod)

	in  if (!debug) then print_pset pset else ();
            MODULE {bnds = bnds', imports = imports',
		    exports = exports'}

	end

end
