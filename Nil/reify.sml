(*$import TopLevel Nil NilContext NilUtil Util Sequence Name TraceInfo TraceOps REIFY *)

structure Reify :> REIFY =
struct
    open Util Nil

    val debug = Stats.ff("ReifyDebug")
    fun error s = Util.error "reify.sml" s 



    datatype letbody = BODY_EXP of Nil.exp
                     | BODY_EXPORTS of Nil.export_entry list

    val empty_pset = Name.VarSet.empty

    val print_pset =
	Name.VarSet.app 
           (fn v => (print (Name.var2string v); print " "))

    val pset_add_list = Name.VarSet.addList

    val pset_member = Name.VarSet.member

    (* reify_con_rt 
         post: returned pset extends input pset, ensuring that 
               all the free variables of c will be available at run-time
     *)
    fun reify_con_rt (c, pset) = 
      let
         val free_cvars = NilUtil.freeConVarInCon(false, c)
      in
         pset_add_list (pset, free_cvars)
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
      | decide_con_b_phase ctxt (Open_cb (v,_,c,_), pset) =
          if pset_member (pset, v) then
             (Runtime, reify_con_rt (c, pset))
          else
             (Compiletime, pset)
      | decide_con_b_phase ctxt (Code_cb (v,_,c,_), pset) =
          if pset_member (pset, v) then
             (Runtime, reify_con_rt (c, pset))
          else
             (Compiletime, pset)


    fun reify_exp ctxt (e as Var_e v, pset) = (e, pset)
      | reify_exp ctxt (e as Const_e _, pset) = (e, pset)
      | reify_exp ctxt (Prim_e (p, cons, exps), pset) =
	  let 
             val pset' = 
                   if (Normalize.allprim_uses_carg p) then
                      reify_cons_rt (cons, pset)
                   else
                      pset

             val (exps', pset'') = reify_exps ctxt (exps, pset')
	  in  
             (Prim_e (p, cons, exps'), pset'')
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

      | reify_exp ctxt (Handle_e (e1, v, e2), pset) =
          let
              val (e1', pset') = reify_exp ctxt (e1, pset)
              val ctxt = NilContext.insert_con(ctxt, v, Prim_c(Exn_c,[]))
              val (e2', pset) = reify_exp ctxt (e2, pset)
          in
	      (Handle_e (e1', v, e2'), pset)
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
              val (arms', pset) = 
                    reify_sum_arms ctxt (arms, bound, sumtype', pset)
              val (default',pset) = reify_exp_option ctxt (default, pset)
          in
              (Switch_e (Sumsw_e{arg = arg', sumtype = sumtype,
                                bound = bound, arms = arms', 
                                default = default',
				 result_type = result_type}),
               pset)
          end

      | reify_exp ctxt (Switch_e (Exncase_e {arg, bound, arms,
                                            default, result_type}), pset) =
          let
             val (arg', pset) = reify_exp ctxt (arg, pset)
             val (arms', pset) = reify_exn_arms ctxt (arms, bound, pset)
             val (default',pset) = reify_exp_option ctxt (default, pset)
          in
	     (Switch_e (Exncase_e {arg = arg', bound = bound,
                                   arms = arms', default = default',
				   result_type = result_type}),
              pset)
          end

      | reify_exp ctxt (Switch_e (Typecase_e _), pset) =
          error "reify_exp:  Typecase_e unimplemented"  

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

      | reify_seq_bnds ctxt (Exp_b (v,_,e)::bs, body, pset) =
           let
              val t1 = Normalize.type_of (ctxt, e)
              val ctxt' = NilContext.insert_con(ctxt, v, t1)
              val (bs', body', pset) = reify_seq_bnds ctxt' (bs, body, pset)
              val (e', pset') = reify_exp ctxt (e, pset)

              val (bnds, pset'') = 
                 (case TraceOps.get_trace (ctxt, t1) of
                    SOME tinfo => 
                       (Exp_b (v, TraceKnown tinfo, e') :: bs',
                        pset_add_list (pset', TraceOps.get_free_vars' tinfo))
                  | NONE =>
                       let 
                          val v' = Name.fresh_named_var "reify"
			  val pset'' = reify_con_rt(t1,pset')
                       in
                          (Con_b(Runtime,Con_cb (v', t1))::
                              Exp_b (v, TraceCompute v',e') :: bs',
                           pset'')
                       end)

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
    and reify_sum_arms ctxt ([], _, _, pset) = ([], pset)
      | reify_sum_arms ctxt ((w,e)::arms, v, c, pset) = 
          let
             val (arms', pset) = reify_sum_arms ctxt (arms, v, c, pset)
             val t = NilUtil.convert_sum_to_special(c, w)
             val ctxt = NilContext.insert_con(ctxt, v, t)
             val (e', pset) = reify_exp ctxt (e, pset)
          in
             ((w,e')::arms', pset)
          end

    and reify_exn_arms ctxt ([], _, pset) = ([], pset)
      | reify_exn_arms ctxt ((e1,e2)::arms, v, pset) = 
          let
             val (arms', pset) = reify_exn_arms ctxt (arms, v, pset)
             val (e1', pset) = reify_exp ctxt (e1, pset)
             val tagcon = Normalize.type_of(ctxt,e1)
	     val (_,Prim_c(Exntag_c, [con])) = Normalize.reduce_hnf(ctxt,tagcon)
	     val ctxt = NilContext.insert_con(ctxt, v, con)
             val (e2', pset) = reify_exp ctxt (e2, pset)
          in
             ((e1',e2')::arms', pset)
          end

    (* really should probably be written using a fold over sequence *)
    and reify_vfseq ctxt (vfseq, openness, pset) =
          let
             (* should probably be written with Sequence.foldl_acc *)

             val vflist = Sequence.toList vfseq
             val getftype = NilUtil.function_type openness
             val ctxt = NilContext.insert_con_list 
                         (ctxt, (map (fn (v,f) => (v, getftype f)) vflist))

             fun loop ([], pset) = ([], [], pset)
               | loop ((f, Function{effect=eff,recursive=recur,isDependent=dep,
				    tFormals=vks,eFormals=vtcs,
				    fFormals=fs,body=e,body_type})::fns, pset)=
                   let
                      val (fns', bnds, pset) = loop (fns, pset)
                      val ctxt = NilContext.insert_kind_list (ctxt,vks)
                      val vks_length = List.length vks
                      fun loop' _ [] pset = (bnds, [], pset)
                        | loop' ctxt ((v,tr,c)::vtcs) pset =
	                    let
                               val ctxt' = 
				   if dep then
				       NilContext.insert_con (ctxt, v, c)
				   else
				       ctxt
                               val (bnds, vtcs', pset) = loop' ctxt' vtcs pset
                            in 
			       case TraceOps.get_trace (ctxt, c) of
				   SOME tinfo => 
				       (bnds, (v,TraceKnown tinfo,c)::vtcs',pset)
				 | NONE => 
				       if (vks_length = 0) then
					   let 
					       val v' = Name.fresh_named_var "reify"
					       val pset' = reify_con_rt(c,pset)
					   in
					       (Con_b(Runtime,Con_cb (v', c))::bnds,
						(v,TraceCompute v',Var_c v')::vtcs',
						pset')
					   end
				       else
					   error ("reify_vflist: Cannot hoist from" ^
						  " Lambda-lambda function")
                            end
                      val (bnds', vtcs', pset) = loop' ctxt vtcs pset
                      val ctxt = NilContext.insert_con_list (ctxt,
							     (map (fn (v,t,c) => (v,c)) vtcs))
                      val (e', pset) = reify_exp ctxt (e, pset)
                   in
                      ((f, Function{effect=eff,recursive=recur,isDependent=dep,
				    tFormals=vks,eFormals=vtcs',fFormals=fs,
				    body=e',body_type=body_type})::fns',
                       bnds', pset)
                   end

             val (vflist', bnds', pset) = loop (vflist, pset)
          in
             (Sequence.fromList vflist', bnds', pset, ctxt)
          end

   and reify_vclseq ctxt (vclseq, pset) =
          let
             val vcllist = Sequence.toList vclseq
             val ctxt = NilContext.insert_con_list (ctxt,
                          (map (fn (v,{tipe,...}) => (v, tipe)) vcllist))

             fun loop ([], pset) = ([], pset)
               | loop ((v,{code, cenv, venv, tipe})::cls, pset) = 
                   let
                      val (cls', pset) = loop (cls, pset)
                      val (venv', pset) = reify_exp ctxt (venv, pset)
                   in
                      ((v,{code = code, cenv = cenv, venv = venv', 
                        tipe = tipe})::cls', pset)
                   end

             val (vcllist', pset) = loop (vcllist, pset)
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


    fun reify_mod (MODULE {bnds, imports, exports}) =
        let fun create_ctxt ([], ctxt) = ctxt
              | create_ctxt (ImportType (_, v, k) :: is, ctxt) = 
                   create_ctxt (is, NilContext.insert_kind(ctxt, v, k))
		   (* XXX Chris look at the trace on ImportValue *)
              | create_ctxt (ImportValue (_, v, tr, c) :: is, ctxt) = 
                   create_ctxt (is, NilContext.insert_con(ctxt, v, c))
	    val ctxt = create_ctxt (imports, NilContext.empty ())
	    val (bnds', BODY_EXPORTS exports', pset) = 
                  reify_seq_bnds ctxt (bnds, BODY_EXPORTS exports, empty_pset)
	in  if (!debug) then print_pset pset else ();
            MODULE {bnds = bnds', imports = imports,
		    exports = exports'}
            
	end

end
