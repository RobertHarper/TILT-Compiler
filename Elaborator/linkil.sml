(*$import LINKIL Prim Il Ppprim Ppil IlUtil PrimUtil IlContext IlStatic Toil Pat Datatype Signature Basis Tyvar IlPrimUtilParam IlContextEq Error Equal InfixParse Formatter LinkParse Specific TVClose *)

structure LinkIl :> LINKIL  = 
    struct

	structure Ppprim = Ppprim
	structure Ppil = Ppil
	structure IlContext = IlContext
	structure IlPrimUtilParam = IlPrimUtilParam
	structure IlUtil = IlUtil
	structure IlStatic = IlStatic
	structure Error = Error
        structure Datatype = Datatype
	structure Signature = Signature
	structure Equal = Equal
	structure InfixParse = InfixParse
	structure Pat = Pat
	structure Toil = Toil
	structure Basis = Basis


	open Il IlUtil Ppil IlStatic Formatter
	val error = fn s => Util.error "linkil.sml" s

        type module = (Il.context * (Il.sbnd option * Il.context_entry) list)
	val _ = Ppil.convar_display := Ppil.VALUE_ONLY

	fun SelfifySdec ctxt (SDEC(l,dec)) = SDEC(l,SelfifyDec ctxt dec)
	fun local_add_context_entries self_ctxt (acc_ctxt,entries) = 
	    let fun folder (CONTEXT_SDEC sdec,(self_ctxt,acc_ctxt)) = 
		    let val sdec = SelfifySdec self_ctxt sdec
		    in  (IlContext.add_context_sdec(self_ctxt,sdec),
			 IlContext.add_context_sdec(acc_ctxt,sdec))
		    end
		  | folder (ce,(self_ctxt,acc_ctxt)) = 
		    let val self_ctxt = IlContext.add_context_entries(self_ctxt,[ce])
			val acc_ctxt = IlContext.add_context_entries(acc_ctxt,[ce])
		    in  (self_ctxt,acc_ctxt)
		    end
	    in  #2(foldl folder (self_ctxt,acc_ctxt) entries)
	    end

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
			       | DEC_MOD(v,b,s) => 
				     let val subst = (etab,ctab,(v,MOD_VAR fv)::mtab)
					 val s = sig_subst_expconmodvar(s,etab,ctab,mtab)
					 val first = (SBND(l,BND_MOD(fv,b,label2obj path2mod l)),
						      SDEC(l,DEC_MOD(fv,b,s)))
					 val rest = 
					     (case (IlUtil.is_open l, s) of
						  (true,SIGNAT_STRUCTURE(_,inner_sdecs)) =>
						      export_sdecs subst (join_path_labels(path,[l])) inner_sdecs
						| _ => [])
				     in  if (IlUtil.is_open l)
					     then (rest @ acc, subst)
					 else (first :: acc, subst)
				     end)
			end
		    val (rev_sdecs,_) = foldl do_sdec ([],subst) sdecs
		in  rev_sdecs
		end


	val xdec = Stats.timer("Elaboration",Toil.xdec)
	val xspec = Stats.timer("Elaboration",Toil.xspec)


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


	fun check' (context,sbnd_entries) {doprint,docheck} =
	    let
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


	val plus_context = IlContext.plus_context(IlUtil.con_subst_expconmodvar,
						  IlUtil.kind_subst_expconmodvar,
						  IlUtil.sig_subst_expconmodvar)

	structure IlContextEq = IlContextEq

	val eq_context = IlContextEq.eq_context
	val init_context = empty_context

	type filepos = SourceMap.charpos -> string * int * int
	fun elab_specs (base_ctxt, fp, specs) = 
	    case xspec(base_ctxt, fp, specs) of
		SOME sdecs => 
		    let val ctxts = map CONTEXT_SDEC sdecs
			val ctxt = local_add_context_entries base_ctxt (empty_context,ctxts) 
		    in  SOME ctxt
		    end
	      | NONE => NONE

	fun elab_dec (base_ctxt, fp, dec) = 
	    case xdec(base_ctxt,fp,dec) 
		of SOME sbnd_ctxtent_list => 
		    let val sbnds = List.mapPartial #1 sbnd_ctxtent_list
			val ctxtents = map #2 sbnd_ctxtent_list
			val ctxt = local_add_context_entries base_ctxt (empty_context,ctxtents) 
		    in 
			SOME(ctxt,sbnd_ctxtent_list)
		    end
	      | NONE => NONE


	local
	    open Ast
	    fun seqDec [d] = d
	      | seqDec decs = SeqDec decs
	    fun valspec2dec (sym,ty) = 
		let val pat = ConstraintPat{pattern = VarPat [sym], 
					    constraint = ty}
		    val dec = ValDec([Vb{pat = pat,
						exp = VarExp [sym]}],
					    ref [])
		    val _ = TVClose.closeDec dec
		in  dec
		end
	    fun strspec2dec coerce (sym,SOME sigexp,path_opt) = 
		let val copysym = Symbol.strSymbol("copy_" ^ (Symbol.name sym))
		in  if coerce
		    then StrDec[Strb{name = copysym,
				     def = VarStr [sym],
				     constraint = Opaque sigexp}]
		    else StrDec[Strb{name = sym,
				     def = VarStr [copysym],
				     constraint = NoSig}]
		end
	      | strspec2dec _ _ = error "strspec2dec no constraining signature"
	in  val seqDec = seqDec
	    fun spec2dec coerce (MarkSpec(spec,r)) = spec2dec coerce spec
	      | spec2dec coerce (StrSpec ls) = SOME(seqDec(map (strspec2dec coerce) ls))
	      | spec2dec true (ValSpec ls) = SOME(seqDec(map valspec2dec ls))
	      | spec2dec false (ValSpec ls) = NONE
	      | spec2dec _ (TycSpec (ls,_)) = error "elab_dec_constrained: type spec not implemented"
	      | spec2dec _ _ = error "elab_dec_constrained: unhandled spec"
	end



	fun elab_dec_constrained (ctxt1, fp, dec, fp2, specs : Ast.spec list) = 
	    let val coerce_dec = List.mapPartial (spec2dec true) specs
		val export_dec = List.mapPartial (spec2dec false) specs
		val new_dec = seqDec([dec] @ coerce_dec @ export_dec)
	    in  elab_dec(ctxt1,fp,new_dec)
	    end

    end (* struct *)

