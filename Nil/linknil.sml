signature LINKNIL = 
sig
    structure Nil : NIL
    structure NilUtil : NILUTIL
    structure NilContext : NILCONTEXT
    structure NilStatic : NILSTATIC
    structure PpNil : PPNIL

    val compile_prelude : bool * string -> Nil.module
    val compile : string -> Nil.module
    val test : string -> Nil.module

end

structure Linknil (* : LINKNIL *) =
struct

    val error = fn s => Util.error "linknil.sml" s

    structure Nil = Nil(structure ArgAnnotation = Annotation
			structure ArgPrim = LinkIl.Prim)      
	
    structure PpNil = Ppnil(structure Nil = Nil
			    structure Ppprim = LinkIl.Ppprim)

    structure Alpha = Alpha(structure ArgNil = Nil)

    structure NilPrimUtilParam = NilPrimUtilParam(structure Nil= Nil)
	
    structure NilPrimUtil = PrimUtil(structure Prim = LinkIl.Prim
				     structure Ppprim = LinkIl.Ppprim
				     structure PrimUtilParam = NilPrimUtilParam)

    structure NilUtil = NilUtilFn(structure ArgNil = Nil
				  structure IlUtil = LinkIl.IlUtil
				  structure Prim = LinkIl.Prim
				  structure PrimUtil = NilPrimUtil
				  structure Alpha = Alpha)

    structure NilContext = NilContextFn(structure ArgNil = Nil
					structure PpNil = PpNil
					structure Cont = Cont)

    structure NilStatic = NilStaticFn(structure Annotation = Annotation
				      structure Prim = LinkIl.Prim
				      structure ArgNil = Nil
				      structure PrimUtil = NilPrimUtil
				      structure NilUtil = NilUtil
				      structure NilContext = NilContext
				      structure PpNil = PpNil
				      structure Alpha = Alpha)

    fun nilstatic_exp_valid (nilctxt : NilContext.context ,nilexp : Nil.exp) : Nil.exp * Nil.con = 
	let val (e,c,k) = NilStatic.exp_valid(nilctxt,nilexp) in (e,c) end
(*
	let open Nil
(*	    val _ = (print "nilstatic_exp_valid called on:\n"; PpNil.pp_exp nilexp; print "\n") *)
	    val res = 
		     (case nilexp of
		 Var_e v => (case (NilContext.find_con(nilctxt,v)) of
				 SOME c => (nilexp,c)
			       | _ => (print "temp typechecker could not find var";
				       PpNil.pp_var v; print "\n";
				       error "temp typechecker could not find var"))
	       | Prim_e(NilPrimOp(select l), _, [e]) => 
		     let val (_,c) = nilstatic_exp_valid(nilctxt,e)
			 fun lerror() = (print "temporary typechecker failed typechecking projection\n";
					print "expected record type with field ";
					PpNil.pp_label l; print "; instead found:";
					PpNil.pp_con c; print "\n";
					error "temporary typechecker failed typechecking projection")
		     in  case c of
			 Prim_c(Record_c labels,fields) => 
			     (case (Listops.assoc_eq(Name.eq_label,l,Listops.zip labels fields)) of
				   SOME fieldc => (nilexp,fieldc)
				 | _ => lerror())
		       | _ => lerror()
		     end
	       | _ => (print "sorry, this temporary typecheker does not handler this:\n";
		       PpNil.pp_exp nilexp; print "\n";
		       error "sorry, this temporary typecheker does not handler this"))
(*
	    val _ = (print "nilstatic_exp_valid called on:\n"; PpNil.pp_exp nilexp; print "\n";
		     print "and returning type:\n"; PpNil.pp_con (#2 res); print "\n\n")
*)
	in res
	end
*)
    structure Tonil = Tonil(structure Il = LinkIl.Il
			    structure Nilstatic = NilStatic
			    structure Nilprimutil = NilPrimUtil
			    structure Ilutil = LinkIl.IlUtil
                            structure Ilcontext = LinkIl.IlContext
                            structure IlStatic = LinkIl.IlStatic
			    structure Nilcontext = NilContext
			    structure Nilutil = NilUtil
			    structure Ppnil = PpNil
			    structure Ppil = LinkIl.Ppil)

    structure Linearize = Linearize(structure Nil = Nil
				    structure NilUtil = NilUtil
				    structure Ppnil = PpNil)

    structure BetaReduce = BetaReduce(structure Nil = Nil
				      structure NilUtil = NilUtil
				      structure Ppnil = PpNil
				      structure IlUtil = LinkIl.IlUtil)

    structure Cleanup = Cleanup(structure Nil = Nil
				structure NilUtil = NilUtil
				structure Ppnil = PpNil
				structure IlUtil = LinkIl.IlUtil)

    structure ToClosure = ToClosure(structure Nil = Nil
				    structure Ppnil = PpNil
				    structure NilUtil = NilUtil)

	
    structure NilEval = NilEvaluate(structure Nil = Nil
				    structure NilUtil = NilUtil
				    structure Ppnil = PpNil
				    structure PrimUtil = NilPrimUtil)

    fun phasesplit debug (ctxt,sbnds,sdecs) : Nil.module = 
	let
	    open Nil LinkIl.Il LinkIl.IlContext Name
	    fun make_cr_labels l = (internal_label((label2string l) ^ "_c"),
				    internal_label((label2string l) ^ "_r"))

            (* obtain all the imports with classifiers from the context *)
	    datatype import_type = ImpExp | ImpType | ImpMod
		
	    fun mapper v =
		let val SOME(l,pc) = LinkIl.IlContext.Context_Lookup'(ctxt,v)
		in
		    (case pc of
			 PHRASE_CLASS_EXP _ => SOME(ImpExp,v,l)
		       | PHRASE_CLASS_CON _ => SOME(ImpType,v,l)
		       | PHRASE_CLASS_MOD _ => SOME(ImpMod,v,l)
		       | PHRASE_CLASS_SIG _ => NONE
		       | PHRASE_CLASS_OVEREXP _ => NONE)
		end
	    val varlist = LinkIl.IlContext.Context_Varlist ctxt
	    val import_temp = List.mapPartial mapper varlist

            (* create a module-variable to pair of variables map from import_modmap *)
	    fun folder ((ImpMod,v,l),map) = let val vc = fresh_named_var (var2string v ^ "myvc")
						val vr = fresh_named_var (var2string v ^ "myvr")
					    in  VarMap.insert(map,v,(vc,vr))
					    end
	      | folder (_,map) = map
	    val import_varmap = foldl folder VarMap.empty import_temp


            (* call the phase splitter *)
	    val {nil_initial_context : NilContext.context , nil_final_context : NilContext.context, 
		 cu_bnds = bnds, vmap = total_varmap} = 
		Tonil.xcompunit ctxt import_varmap sbnds

            (* create the imports with classifiers by using the NIL context 
	      returned by the phase splitter *)
	    fun folder ((ImpExp,v,l),imps) = 
		let val c' = (case NilContext.find_con(nil_initial_context,v) of
				  SOME c' => c'
				| NONE => error "exp var not in NIL context")
		in  (ImportValue(l,v,c'))::imps
		end
	      | folder ((ImpType,v,l),imps) = 
		let 
		    fun strip_var_from_singleton (var,kind) = 
			let open NilUtil
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
		    val (c,k) = NilStatic.con_valid(nil_initial_context,Var_c v) 
		    val k = strip_var_from_singleton(v,Singleton_k(Runtime,k,c))
		in  (ImportType(l,v,k))::imps
		end
	      | folder ((ImpMod,v,l),imps) = 
		if (LinkIl.IlUtil.is_exportable_lab l) (* a label is exportable iff it is importable *)
		    then
			let val SOME(cvar,rvar) = VarMap.find(import_varmap,v)
			    val (cl,rl) = make_cr_labels l
			in  folder((ImpExp,rvar,rl),folder((ImpType,cvar,cl),imps))
			end
		else imps
	    val imports : import_entry list = rev (foldl folder [] import_temp)

	    (* create the export map by looking at the original sbnds;
	      labels that are "exportable" must be exported
             open labels must be recursively unpackaged 
             for module bindings, use the varmap returned by the phase-splitter *)
(*
	    fun folder cr_pathopts ((SBND(l,bnd)),exports) = 
		let open LinkIl.IlUtil
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
		    (case (is_exportable_lab l, Name.is_label_open l, bnd) of
			 (false,false,_) => exports
		       | (true,_,BND_EXP (v,_)) => 
			     let val e = path2exp (make_rpath v)
				 val (_,c) = nilstatic_exp_valid(nil_final_context,e)
			     in  (ExportValue(l,e,c)::exports)
			     end
		       | (true,_,BND_CON (v,_)) =>
			     let val c = path2con (make_cpath v)
				 val (_,k) = NilStatic.con_valid(nil_final_context,c)
			     in  (ExportType(l,c,k)::exports)
			     end
		       | (false,true,BND_EXP _) => error "BND_EXP with open label"
		       | (false,true,BND_CON _) => error "BND_CON with open label"
		       | (is_export,is_open,BND_MOD (v,m)) => 
			     let val (lc,lr) = make_cr_labels l
				 val (vc,vr) = (case VarMap.find(total_varmap,v) of
						    SOME vrc => vrc
						  | NONE => error "total_varmap missing bindings")
				 val rpath = make_rpath vr
				 val cpath = make_rpath vc
				 val exports = 
				     if is_export
					 then
					     let val _ = print "exporting module\n"
						 val er = path2exp rpath
						 val (_,cr) = nilstatic_exp_valid(nil_final_context,er)
						 val cc = path2con cpath
						 val (_,kc) = NilStatic.con_valid(nil_final_context,cc)
						 val _ = print "done exporting module\n"
					     in (ExportValue(lr,er,cr)::
						 ExportType(lc,cc,kc)::
						 exports)
					     end
				     else exports
				 val exports = 
				     (case (is_open,m) of
					  (true,MOD_STRUCTURE sbnds) =>
					      let val _ = print "exporting open module\n"
						  val res = foldl (folder (SOME (cpath,rpath))) exports sbnds
						  val _ = print "done exporting open module\n"
					      in res
					      end
					| (true, _) => 
					      (print "BND_MOD (non-structure) with open label:\n";
					       LinkIl.Ppil.pp_mod m; print "\n";
					       error "BND_MOD (non-structure) with open label")
					| _ => exports)
			     in  exports
			     end)
		end
*)
	    fun folder cr_pathopts ((SDEC(l,dec)),exports) = 
		let open LinkIl.IlUtil
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
		    (case (is_exportable_lab l, false andalso Name.is_label_open l, dec) of
			 (false,false,_) => exports
		       | (true,_,DEC_EXP (v,_)) => 
			     let val e = path2exp (make_rpath v)
				 val (_,c) = nilstatic_exp_valid(nil_final_context,e)
			     in  (ExportValue(l,e,c)::exports)
			     end
		       | (true,_,DEC_CON (v,_,_)) =>
			     let val c = path2con (make_cpath v)
				 val (_,k) = NilStatic.con_valid(nil_final_context,c)
			     in  (ExportType(l,c,k)::exports)
			     end
		       | (false,true,DEC_EXP _) => error "DEC_EXP with open label"
		       | (false,true,DEC_CON _) => error "DEC_CON with open label"
		       | (is_export,is_open,DEC_MOD (v,s)) => 
			     let val (lc,lr) = make_cr_labels l
				 val (vc,vr) = (case VarMap.find(total_varmap,v) of
						    SOME vrc => vrc
						  | NONE => error "total_varmap missing bindings")
				 val rpath = make_rpath vr
				 val cpath = make_rpath vc
				 val exports = 
				     if is_export
					 then
					     let 
(*						 val _ = print "exporting module\n" *)
						 val er = path2exp rpath
						 val (_,cr) = nilstatic_exp_valid(nil_final_context,er)
						 val cc = path2con cpath
						 val (_,kc) = NilStatic.con_valid(nil_final_context,cc)
(*						 val _ = print "done exporting module\n" *)
					     in (ExportValue(lr,er,cr)::
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
					       LinkIl.Ppil.pp_signat s; print "\n";
					       error "DEC_MOD (non-structure) with open label")
					| _ => exports)
			     in  exports
			     end)
		end
	   val exports : export_entry list = rev(foldl (folder NONE) [] sdecs)

	    val nilmod = MODULE{bnds = bnds, 
				imports = imports,
				exports = exports}
	    val _ = if debug
			then (print "\n\n=======================================\n\n";
			      print "phase-split results:\n";
			      PpNil.pp_module nilmod;
			      print "\n")
		    else print "Phase-splitting complete\n"
	in  nilmod
	end
    val phasesplit = Stats.timer("Phase-splitting",phasesplit)


    fun compile' debug (ctxt,sbnds,sdecs) = 
	let
	    open Nil LinkIl.Il LinkIl.IlContext Name
	    val nilmod = phasesplit debug (ctxt,sbnds,sdecs)
	    val nilmod = (Stats.timer("Cleanup",Cleanup.cleanModule)) nilmod
	    val _ = if debug 
			then (print "\n\n=======================================\n\n";
			      print "cleanup results:\n";
			      PpNil.pp_module nilmod;
			      print "\n")
		    else print "cleanup complete\n"
	    val nilmod = (Stats.timer("Linearization",Linearize.linearize_mod)) nilmod
	    val _ = if debug 
			then (print "\n\n=======================================\n\n";
			      print "renaming results:\n";
			      PpNil.pp_module nilmod;
			      print "\n")
		    else print "Renaming complete\n"
	    val D = NilContext.empty()
	    val nilmod = (Stats.timer("Nil typechecking",NilStatic.module_valid)) (D,nilmod)
	    val _ = if debug 
			then (print "\n\n=======================================\n\n";
			      print "nil typechecking results:\n";
			      PpNil.pp_module nilmod;
			      print "\n")
		    else print "Nil typechecking complete\n"
	    val nilmod = (Stats.timer("Beta-reduction",BetaReduce.reduceModule)) nilmod
	    val _ = if debug 
			then (print "\n\n=======================================\n\n";
			      print "beta-reduction results:\n";
			      PpNil.pp_module nilmod;
			      print "\n")
		    else print "Beta-reduction complete\n"
	    val nilmod = (Stats.timer("Closure-conversion",ToClosure.close_mod)) nilmod
	    val _ = if debug
			then (print "\n\n=======================================\n\n";
			      print "closure-conversion results:\n";
			      PpNil.pp_module nilmod;
			      print "\n")
		    else print "Closure-conversion complete\n"
	in  nilmod
	end

    fun test filename = 
	let val SOME(ctxt,sbnds,sdecs) = LinkIl.test filename
	in  compile' true (ctxt,sbnds,sdecs)
	end
    fun compile filename = 
	let val SOME(ctxt,sbnds,sdecs) = LinkIl.compile filename
	in  compile' false (ctxt,sbnds,sdecs)
	end
    val cached_prelude = ref (NONE : Nil.module option)
    fun compile_prelude (use_cache,filename) = 
	case (use_cache, !cached_prelude) of
		(true, SOME m) => m
	      | _ => let val (ctxt,sbnds,sdecs) = 
				LinkIl.compile_prelude(use_cache,filename)
			 val m = compile' false (ctxt,sbnds,sdecs)
			 val _ = cached_prelude := SOME m
		     in  m
		     end
end

