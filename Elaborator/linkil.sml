(*$import LINKIL Prim Il Ppprim Ppil IlUtil PrimUtil IlContext IlStatic Toil Pat Datatype Signature Basis Tyvar IlPrimUtilParam IlContextEq Error Equal InfixParse Formatter LinkParse Specific TVClose *)

structure LinkIl :> LINKIL  = 
    struct

	val _ = IlContext.installHelpers {eq_con = IlStatic.eq_con}
	val _ = Pat.installHelpers {typecompile = Toil.typecompile,
				    expcompile = Toil.expcompile,
				    polyinst = Toil.polyinst}
	val _ = Signature.installHelpers {polyinst = Toil.polyinst}
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

	val show_hil = Stats.ff("showHIL")
	val show_hilcontext = Stats.ff("showHILContext")

	open Il IlUtil Ppil IlStatic Formatter
	val error = fn s => Util.error "linkil.sml" s

	val _ = Ppil.convar_display := Ppil.VALUE_ONLY

	fun local_add_context_entries (acc_ctxt,entries) = 
	    let fun folder (CONTEXT_SDEC sdec,acc_ctxt) = 
		    let val sdec = IlContext.SelfifySdec acc_ctxt sdec
		    in  IlContext.add_context_sdec(acc_ctxt,sdec)
		    end
		  | folder (ce,acc_ctxt) = 
		        IlContext.add_context_entries(acc_ctxt,[ce])
	    in  foldl folder acc_ctxt entries
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


	val xdec = Stats.timer("Elaboration",Toil.xdec)
	val xspec = Stats.timer("Elaboration",Toil.xspec)



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
		val _ = if (IlUtil.mod_resolved m)
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


	val plus_context = IlContext.plus_context

	structure IlContextEq = IlContextEq

	val cached_initial_context = ref (NONE : context option)
	fun initial_context() = 
	  (case (!cached_initial_context) of
	       SOME ctxt => ctxt
	     | _ => let val initial_context = Basis.initial_context()
			val _ = cached_initial_context := (SOME initial_context)
		    in  initial_context
		    end)

	type filepos = SourceMap.charpos -> string * int * int
	fun elab_specs (base_ctxt, fp, specs) = 
	    case xspec(base_ctxt, fp, specs) of
		SOME sdecs => 
		    let val ctxts = map CONTEXT_SDEC sdecs
			val new_ctxt = local_add_context_entries (base_ctxt,ctxts) 
			val partial_ctxt = IlContext.sub_context(new_ctxt,base_ctxt)
		    in  SOME partial_ctxt
		    end
	      | NONE => NONE

	fun elab_dec (base_ctxt, fp, dec) = 
	  let val _ = if (!show_hilcontext)
			  then (print "\nCONTEXT:\n"; Ppil.pp_context base_ctxt) 
		      else ()
	  in  case xdec(base_ctxt,fp,dec) of
	      SOME sbnd_ctxtent_list => 
		    let val sbnds = List.mapPartial #1 sbnd_ctxtent_list
			val ctxtents = map #2 sbnd_ctxtent_list
			val new_ctxt = local_add_context_entries (base_ctxt,ctxtents) 
			val _ = if (!show_hil)
				    then  (print "SBNDS:\n"; Ppil.pp_sbnds sbnds;
					   print "\nENTRIES:\n"; 
					   (app (fn e => (Ppil.pp_context_entry e; 
							  print "\n")) ctxtents);
					   print "\n")
				else ()
			val partial_ctxt = IlContext.sub_context(new_ctxt, base_ctxt)
		    in  SOME(base_ctxt,partial_ctxt,sbnd_ctxtent_list)
		    end
	      | NONE => NONE
	  end


        (* Interface files are tricky to compile because the signature expressions
	   in the interface files are not compiled in the same context as in the context.
	   (1) The source file is compiled in codeContext yielding codeSbnds and codeSdecs.
	   (2) The interface file is compiled one spec at a time, maintaining
	       an interfaceContext and list of sbnds/sdecs, as follows:

	       structure X : SIG

	       SIG is compiled in interfaceContext, yielding the HIL signature sigTarget.
	       Then, "X" is compiled and  opaquely coerced to sigTarget in codeContext, generating
	       the HIL structure modX and actual HIL signature sigActual (which leaks type information).
	       The interfaceContext is then enriched with "X : sigActual" (not sigTarget).
	       Note that we may access structures being bound by the interface.  For this reason,
	       we must keep a "leaky" interfaceContext.

	   (3) As we process the interface specs, We keep a list of interSbnds and interSdecs 
	       which is constructed with sigTarget (not sigActual).  The final result is obtained
	       by shadowing all top-level labels of codeSbnds/codeSdecs and appending it
	       with interSbnds and interSdecs
	*)

	fun elab_dec_constrained (base_ctxt, fp, dec, fp2, specs : Ast.spec list) = 
	  let val _ = if (!show_hilcontext)
			  then (print "\nCONTEXT:\n"; Ppil.pp_context base_ctxt) 
		      else ()
	  in  case Toil.xdecspec(base_ctxt,fp,dec,fp2,specs) of
	      SOME sbnd_ctxtent_list => 
		    let val sbnds = List.mapPartial #1 sbnd_ctxtent_list
			val ctxtents = map #2 sbnd_ctxtent_list
			val new_ctxt = local_add_context_entries (base_ctxt,ctxtents) 
			val _ = if (!show_hil)
				    then  (print "SBNDS:\n"; Ppil.pp_sbnds sbnds;
					   print "\nENTRIES:\n"; 
					   (app (fn e => (Ppil.pp_context_entry e; 
							  print "\n")) ctxtents);
					   print "\n")
				else ()
			val partial_ctxt = IlContext.sub_context(new_ctxt, base_ctxt)
		    in  SOME(base_ctxt,partial_ctxt,sbnd_ctxtent_list)
		    end
	      | NONE => NONE
	  end

    end (* struct *)

