signature LINKIL = 
  sig
      structure Prim : PRIM
      structure Il : IL
      structure Ppprim : PPPRIM 
      structure Ppil : PPIL
      structure IlUtil : ILUTIL
      structure IlPrimUtil : PRIMUTIL
      structure IlContext : ILCONTEXT
      structure IlStatic : ILSTATIC 

(*      type module = (Il.context * Il.sbnd list * Il.sdec list)  *)
      type module = (Il.context * (Il.sbnd option * Il.context_entry) list)
      val compile_prelude : bool * string -> module
      val compile : string -> module option
      val compiles : string -> (module list) option
      val test : string ->  module option
      val setdepth : int -> unit (* printing depth *)
      val elaborate : Il.context * string -> (Il.sbnds * Il.context) option
  end

structure LinkIl (* : LINKIL *) = 
    struct
	structure Tyvar = Tyvar();
	structure Prim = Prim();
	structure IlLeak = Il(structure Prim = Prim
			      structure Tyvar = Tyvar);
	structure Il : IL = IlLeak
	structure IlContext = IlContext(structure Il = IlLeak);
	structure Formatter = Formatter;
	structure AstHelp = AstHelp;
	structure Ppprim = Ppprim(structure ArgPrim = Prim);
	structure Ppil = Ppil(structure AstHelp = AstHelp
			      structure Ppprim = Ppprim
			      structure IlContext = IlContext
			      structure Il = Il); 
	structure IlPrimUtilParam = IlPrimUtilParam(structure Il = Il);
	structure IlPrimUtil = PrimUtil(structure Prim = Prim
					structure Ppprim = Ppprim
					structure PrimUtilParam = IlPrimUtilParam);
	structure IlUtil = IlUtil(structure Il = Il
				  structure IlContext = IlContext
				  structure PrimUtil = IlPrimUtil
				  structure AstHelp = AstHelp
				  structure Ppil = Ppil);
	structure IlStatic = IlStatic(structure Il = Il
				      structure IlContext = IlContext
				      structure PrimUtil = IlPrimUtil
				      structure IlUtil = IlUtil
				      structure Ppil = Ppil);
	structure Datatype = Datatype(structure Il = Il
				      structure IlContext = IlContext
				      structure AstHelp = AstHelp
				      structure IlStatic = IlStatic
				      structure IlUtil = IlUtil
				      structure Ppil = Ppil);
	structure Signature = Signature(structure Il = Il
				      structure IlContext = IlContext
				      structure AstHelp = AstHelp
				      structure IlStatic = IlStatic
				      structure IlUtil = IlUtil
				      structure Ppil = Ppil);
	structure Equal = Equal(structure Il = Il
				structure IlContext = IlContext
				structure IlStatic = IlStatic
				structure IlUtil = IlUtil
				structure Ppil = Ppil);
	    
	structure InfixParse = InfixParse(structure Il = Il
					  structure Ppil = Ppil
					  structure AstHelp = AstHelp);
	structure Pat = Pat(structure Il = Il
			    structure IlContext = IlContext
			    structure IlStatic = IlStatic
			    structure IlUtil = IlUtil
			    structure AstHelp = AstHelp
			    structure Datatype = Datatype
			    structure Ppil = Ppil)
	structure Toil = Toil(structure Il = Il
			      structure IlContext = IlContext
			      structure AstHelp = AstHelp
			      structure IlStatic = IlStatic
			      structure IlUtil = IlUtil
			      structure Ppil = Ppil
			      structure Pat = Pat
			      structure Datatype = Datatype
			      structure Signature = Signature
			      structure Equal = Equal
			      structure Error = Error
			      structure InfixParse = InfixParse);
	structure Basis = Basis(structure Il = Il		
				structure IlContext = IlContext
				structure IlStatic = IlStatic
				structure Ppil = Ppil
				structure Toil = Toil
				structure Datatype = Datatype      
				structure IlUtil = IlUtil);
	structure IlEval = IlEval(structure Il = Il
				  structure IlContext = IlContext
				  structure PrimUtil = IlPrimUtil
				  structure IlStatic = IlStatic
				  structure IlUtil = IlUtil
				  structure Ppil = Ppil);
	    
	open Il IlUtil Ppil IlStatic Formatter
	    
	structure Il = Il
        type module = (Il.context * (Il.sbnd option * Il.context_entry) list)

	val _ = Compiler.Control.Print.printDepth := 15;
	val _ = Pagewidth := 80;
	val error = fn s => Util.error "linkil.sml" s

	fun setdepth d = Compiler.Control.Print.printDepth := d
(*
	fun parse s = 
	    let val (is,dec) = LinkParse.parse_one s
	    in (Source.filepos is, LinkParse.named_form_dec(LinkParse.tvscope_dec dec))
	    end
*)	    
	val _ = Ppil.convar_display := Ppil.VALUE_ONLY

	fun SelfifySdec(SDEC(l,dec)) = SDEC(l,SelfifyDec dec)
	fun local_add_context_entries(ctxt,entries) = 
	    let fun help (CONTEXT_SDEC sdec) = CONTEXT_SDEC(SelfifySdec sdec)
		  | help ce = ce
		val entries' = map help entries
	    in IlContext.add_context_entries(ctxt,entries')
	    end

	fun local_add_context_sdecs(ctxt,sdecs) = 
	    local_add_context_entries(ctxt,map CONTEXT_SDEC sdecs)

	val empty_context = IlContext.empty_context


	(* Given a context GAMMA and a filename, returns then translation of
	   the file using GAMMA as the translation context.  The augmented
	   context is also returned. *)
	fun entries2sdecs entries = 
	    let fun get_sdec (CONTEXT_SDEC sdec) = SOME sdec
		  | get_sdec _ = NONE
	    in List.mapPartial get_sdec entries	    
	    end

        (* returns its results backwards *)
        fun export_sdecs subst path sdecs : (sbnd * sdec) list = 
		let fun label2obj path2obj l = path2obj(join_path_labels(path,[l]))
		    fun do_sdec (SDEC(l,dec),(acc,(etab,ctab,mtab))) = 
			let val fv = Name.fresh_var()
			in  (case dec of
				 DEC_EXP(v,c) => 
				     let val c = con_subst_expconmodvar(c,etab,ctab,mtab)
					 val subst = ((v,VAR fv)::etab,ctab,mtab)
				     in  ((SBND(l,BND_EXP(fv,label2obj path2exp l)),
					   SDEC(l,DEC_EXP(fv,c)))::acc, subst)
				     end
			       | DEC_CON(v,k,copt) => 
				     let val subst = (etab,(v,CON_VAR fv)::ctab,mtab)
					 val copt = (case copt of
							 NONE => NONE
						       | SOME c => SOME(con_subst_expconmodvar(c,etab,ctab,mtab)))
				     in  ((SBND(l,BND_CON(fv,label2obj path2con l)),
					   SDEC(l,DEC_CON(fv,k,copt)))::acc, subst)
				     end
			       | DEC_MOD(v,s) => 
				     let val subst = (etab,ctab,(v,MOD_VAR fv)::mtab)
					 val s = sig_subst_expconmodvar(s,etab,ctab,mtab)
					 val first = (SBND(l,BND_MOD(fv,label2obj path2mod l)),
						      SDEC(l,DEC_MOD(fv,s)))
					 val rest = 
					     (case (Name.is_label_open l, s) of
						  (true,SIGNAT_STRUCTURE(_,inner_sdecs)) =>
						      export_sdecs subst (join_path_labels(path,[l])) inner_sdecs
						| _ => [])
				     in  if (Name.is_label_open l)
					     then (rest @ acc, subst)
					 else (first :: acc, subst)
				     end)
			end
		    val (rev_sdecs,_) = foldl do_sdec ([],subst) sdecs
		in  rev_sdecs
		end

	fun kill_datatype (pair as (SOME(SBND(l,_)),entry)) =
	    if IlUtil.is_datatype_lab l
		then NONE
	    else SOME pair
	  | kill_datatype pair = SOME pair


	val xdec = Stats.timer("Elaboration",Toil.xdec)
	val xspec = Stats.timer("Elaboration",Toil.xspec)

	fun elaborate (context,filename) : ((sbnd option * context_entry) list) option = 
	    let val (filepos,imports,astdec) = LinkParse.parse_impl filename
		val _ = print ("Parsing complete: " ^ filename ^ "\n")
		val res = 
		    (case (xdec(context,filepos,astdec)) of
		     SOME sbnd_ctxt_list =>
			 let 
			     val _ = print ("Elaboration complete: " ^ filename ^ "\n")
(*			     val sbnd_ctxt_list = List.mapPartial kill_datatype sbnd_ctxt_list *)
(*			     val sbnds = List.mapPartial #1 sbnd_ctxt_list *)
(*			     val entries = map #2 sbnd_ctxt_list *)
			 in  SOME(sbnd_ctxt_list)
			 end
		   | _ => (print ("Elaboration failed: " ^ filename ^ "\n");
			   NONE))
	    in  res
	    end




        (* elaborate *)
	val prelude_module = ref (NONE : module option)
	val inlineprelude_quad = ref (NONE : (context * (sbnd option * context_entry) list * context) option)
	val inline_size = 0
	local
	    (* elaborate_prepend takes a context with the inlined entries, 
	         the entries to be inlined, and a context without the inlined entries
	       It returns the entry without the inlined entries,
	         the new entries that typecheck in the context without the inlined entry,
	         the new entries that typecheck in the context with the inlined entry *)
	    fun elaborate_prepend' (context_inline,sb_entries_init,context_noninline) filename = 
		(case elaborate(context_inline,filename) of
		     NONE => NONE
		   | SOME sb_entries => 
			 SOME(context_noninline,sb_entries_init @ sb_entries, sb_entries))
	    fun elaborate_prepend arg filename = 
		(case elaborate_prepend' arg filename of
		     NONE => NONE
		   | SOME (ctxt,sb_entries,_) => SOME (ctxt,sb_entries))

	    fun hide_sbnd_sdec (sbnd,sdec) = 
		let 
		    fun loop acc_b acc_d [] [] = (rev acc_b,rev acc_d)
		      | loop _ _ [] _ = error "hide_quad: number sbnd != number sdec"
		      | loop _ _ _ [] = error "hide_quad: number sbnd != number sdec"
		      | loop acc_b acc_d ((SBND(l,b))::brest) ((SDEC(_,d))::drest) = 
			let val l' = Name.fresh_internal_label(Name.label2string l)
			    val sb = SBND(l',b)
			    val sd = SDEC(l',d)
			in  loop (sb::acc_b) (sd::acc_d) brest drest
			end
		    val (sbnd,sdec) = loop [] [] sbnd sdec
		in  (sbnd,sdec)
		end
	    (* we want to keep the sbnds that are reasonably small *)
	    fun filter sbnd_entries =
		let fun loop acc [] = rev acc
		      | loop acc ((sb as SOME(Il.SBND(l,b)),ce)::rest) =
		            if ((IlUtil.bnd_size b < inline_size) andalso 
				(IlUtil.is_inline_bnd b))
				then loop ((sb,ce)::acc) rest
			    else loop acc rest
		      | loop acc (_::rest) = loop acc rest
		in  loop [] sbnd_entries
		end
 	    val (ctxt_inline,_,_,ctxt_noninline) = Basis.initial_context()  

(*
	    val ctxt_inline = Basis.empty_context
	    val ctxt_noninline = Basis.empty_context
*)
(*
	    val ctxt_inline = IlContext.add_context_inline(Basis.empty_context,
			Name.symbol_label(Symbol.tycSymbol "float"),
			Name.fresh_named_var "float",
			Il.INLINE_CONKIND(Il.CON_FLOAT Il.Prim.F64, 
					Il.KIND_TUPLE 1))
	    val ctxt_noninline = ctxt_inline
*)
       	    fun reparse filename : module = 
		(case elaborate(ctxt_inline,filename) of 
		       NONE => error "prelude failed to elaborate"
		     | SOME (sbnds_entries : (sbnd option * context_entry) list) =>
		     let 
(*                       val sdecs = entries2sdecs entries *)

			 (* compute prelude_module *)
			 val m = (ctxt_noninline,sbnds_entries)
			 val _ = prelude_module := SOME m


			 (* compute inlineprelude_quad *)
			 val entries = map #2 sbnds_entries
			 val ctxt_inline = local_add_context_entries(ctxt_inline,entries)
			 val ctxt_noninline = local_add_context_entries(ctxt_noninline,entries)
(*
			 val _ = (print "sdecs are:\n"; Ppil.pp_sdecs sdecs; print "\n\n";
				  print "ctxt_noninline is:\n"; Ppil.pp_context ctxt_noninline; print "\n\n")
*)
			 val sbnd_entry_filt =  filter(sbnds_entries)
(*			 val (sb_filt,sd_filt) = hide_sbnd_sdec(sb_filt,sd_filt) *)
			 val _ = inlineprelude_quad := SOME (ctxt_inline,sbnd_entry_filt,ctxt_noninline)
		     in  m
		     end)
	in  
	    fun compile_prelude (use_cache,filename) =
		(case (!prelude_module, use_cache) of
		     (SOME m,true) => m
		   | _ => reparse filename)
	    fun compile filename = 
		(case (!inlineprelude_quad) of
		     NONE => error "prelude not elaborated yet"
		   | SOME q => elaborate_prepend q filename)
	    fun compiles filenames = 
		let fun loop q [] = SOME []
		      | loop q (filename::rest) = 
			(case (elaborate_prepend' q filename) of
			     NONE => NONE
			   | SOME (ctxt,sb_ent_inline,sb_ent_noinline) =>
				 (let val m = (ctxt,sb_ent_inline)
				      val (ct1,sb_ent,ct2) = q
				      val entries = map #2 sb_ent_inline
				      val ct1' = local_add_context_entries(ct1,entries)
				      val ct2' = local_add_context_entries(ct2,entries)
				      val q' = (ct1',sb_ent,ct2')
				  in  (case (loop q' rest) of
					   NONE => NONE
					 | SOME ms => SOME(m :: ms))
				  end))
		    val q = (case (!inlineprelude_quad) of
				 NONE => error "prelude not elaborated yet"
			       | SOME q => q)
		in  loop q filenames
		end
	end

(*
	fun evaluate target_sbnds =
	    let
		val allsbnds = initial_sbnds @ target_sbnds
		val module = Il.MOD_STRUCTURE allsbnds
		val allresmodule = IlEval.eval_mod module
		val ressbnds =
		    (case allresmodule of
			 Il.MOD_STRUCTURE allressbnds => allressbnds
		       | _ => error "a structure evaluated to a non-structure")
	    in ressbnds
	    end
*)


	fun has_unset m = 
	    let val unset = ref false
		fun ehandle (OVEREXP (_,_,oe)) = ((case Util.oneshot_deref oe of
						       NONE => unset := true
						     | _ => ()); NONE)
		  | ehandle _ = NONE
		fun chandle (CON_TYVAR tv) = ((case Tyvar.tyvar_deref tv of
						   NONE => unset := true
						 | _ => ()); NONE)
		  | chandle _ = NONE
		val _ = mod_all_handle(m,ehandle,chandle,fn _ => NONE, fn _ => NONE)
	    in  !unset
	    end


	fun check' filename {doprint,docheck} =
	    let
		val (context,sbnd_entries) =
			(case compile filename of
			       SOME res => res
			     | NONE => error "Elaboration failed")
		val _ = print "XXXXXXXXXXXXXXXXX linkil.sml:check not done\n"

		val _ = if doprint 
			    then (print "\ntest: sbnds are: \n";
				  Ppil.pp_sbnds (List.mapPartial #1 sbnd_entries);
				  print "\ntest: entries are: \n";
				  app (fn (_,CONTEXT_SDEC sdec) => (Ppil.pp_sdec sdec; print "\n")
				        | _ => ()) sbnd_entries)
			else ()
(*
		val m = MOD_STRUCTURE sbnds
		val given_s = SIGNAT_STRUCTURE (NONE,sdecs)
		val _ =  if doprint
			     then (print "\ngiven_s is:\n";
				   Ppil.pp_signat given_s)
			 else ()
		val msize = IlUtil.mod_size m
		val ssize = IlUtil.sig_size given_s
		val _ = (Stats.int "Module Size") := msize
		val _ = (Stats.int "Signature Size") := ssize
		val _ = if (has_unset m)
			    then error "module is not fully resolved"
			else ()
(*		val _ =
		    if docheck
			then 
			    let 
(* XXX				val context = break_abstract context *)
				val precise_s = (Stats.timer("TYPECHECKING",IlStatic.GetModSig))(context,m)
				fun sanity_check (ctxt,m,precise_s,given_s) = 
				    if (not (IlStatic.Sig_IsSub(ctxt,precise_s,given_s)))
					then SOME "precise_s is not a subsig of given_s"
				    else if (not (mod_resolved m))
					     then SOME "module is unresolved(tyvars or overexps)"
					 else if (not (sig_resolved precise_s))
						  then SOME "signature is unresolved(tyvars or overexps)"
					      else NONE
				val _ =  if doprint
					     then (print "\nprecise_s is:\n";
						   Ppil.pp_signat precise_s;
						   print "\n";
						   print "\ncontext is\n";
						   Ppil.pp_context context;
						   print "\n")
					 else ()
			    in  ()
			    (* (case ((Stats.timer("SANITY_CHECK",sanity_check))
					(context,m,precise_s,given_s)) of
				      NONE => ()
				    | SOME str => (print "\n\n****** SANITY_CHECK FAILED: ";
						   print str;
						   print " ******\n\n";
						   error "sanity-check"))
				   *)
			    end
		    else ()*)

		val _ = if doprint
			    then (print "\n\n\nELABORATION SUCESSFULLY COMPLETED\n\n\n"; 
				  Ppil.pp_sbnds sbnds;
				  print "\nSize of IL = ";
				  print (Int.toString (IlUtil.mod_size 
						       (Il.MOD_STRUCTURE sbnds)));
				  print "\n\n========================================\n";
				  print "\n\ninitial_context = ctxt = \n";
				  Ppil.pp_context context;
				  print "\n")
			else ()
*)
	    in  (context,sbnd_entries)
	    end


	fun test_res s = check' s {docheck = false, doprint = false}
	fun ptest_res s = check' s {docheck = false, doprint = true}

	fun test_res' s = check' s {docheck = true, doprint = false}
	fun ptest_res' s = check' s {docheck = true, doprint = true}

	fun test s = (test_res s; ())
	fun ptest s = (ptest_res s; ())

	fun test' s = (test_res' s; ())
	fun ptest' s = (ptest_res' s; ())

	structure P = Compiler.Profile
	fun profile thunk arg = 
	    let val _ = P.reset()
		val res = thunk arg
		val _ = P.report TextIO.stdOut
	    in res
	    end

	val test = (fn filename => SOME(ptest_res' filename) handle _ => NONE)

(*
	val elaborate = fn (ctxt,s) =>
	    case elaborate (ctxt,s)
	      of SOME (sbnd_entries) => 
		  SOME(sbnds, IlContext.add_context_entries(IlContext.empty_context,entries))
	       | NONE => NONE       (* maybe we should selfify entries *)
*)

	val plus_context = IlContext.plus_context(IlUtil.con_subst_expconmodvar,
						  IlUtil.kind_subst_expconmodvar,
						  IlUtil.sig_subst_expconmodvar)

	structure IlContextEq = IlContextEq(structure IlContext = IlContext
					    structure IlUtil = IlUtil
					    structure Ppil = Ppil)
	val eq_context = IlContextEq.eq_context
	val init_context = empty_context

	type filepos = SourceMap.charpos -> string * int * int
	fun elab_specs (ctxt, fp, specs) = 
	    case xspec(ctxt, fp, specs) of
		SOME sdecs => let val sdecs' = map SelfifySdec sdecs
			      in  SOME(IlContext.add_context_sdecs(empty_context, sdecs'))
			      end
	      | NONE => NONE

	fun elab_dec (ctxt, fp, dec) = 
	    case xdec(ctxt,fp,dec) 
		of SOME sbnd_ctxt_list => 
		    let val sbnds = List.mapPartial #1 sbnd_ctxt_list
			val ctxts = map #2 sbnd_ctxt_list
			val ctxt = local_add_context_entries(empty_context,ctxts) 
		    in 
			SOME(sbnd_ctxt_list,ctxt)
		    end
	      | NONE => NONE

	fun elab_dec_constrained (ctxt1, fp, dec, fp2, specs : Ast.spec list) = 
	    let open Ast
		fun seqDec [d] = d
		  | seqDec decs = SeqDec decs
		fun valspec2dec (sym,ty) = ValDec([Vb{pat = ConstraintPat{pattern = VarPat [sym], 
									  constraint = ty},
						      exp = VarExp [sym]}],
						  ref [])
		fun strspec2dec (sym,SOME sigexp,path_opt) = StrDec[Strb{name = sym,
								     def = VarStr [sym],
								     constraint = Opaque sigexp}]
		  | strspec2dec _ = error "strspec2dec no constraining signature"
		fun spec2dec (MarkSpec(spec,r)) = MarkDec(spec2dec spec, r)
		  | spec2dec (StrSpec ls) = seqDec(map strspec2dec ls)
		  | spec2dec (TycSpec (ls,_)) = error "elab_dec_constrained: type spec not implemented"
		  | spec2dec (ValSpec ls) = seqDec(map valspec2dec ls)
		  | spec2dec _ = error "elab_dec_constrained: unhandled spec"
		val fp' = fp
		val dec' = seqDec(dec::(map spec2dec specs))
	    in  elab_dec(ctxt1,fp',dec')
	    end
    end (* struct *)

