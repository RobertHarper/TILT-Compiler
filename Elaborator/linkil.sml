(*
signature LINKIL = 
  sig
      structure Tyvar : TYVAR
      structure Prim : PRIM
      structure Il : IL
      structure Ppprim : PPPRIM
      structure Ppil : PPIL
      structure IlUtil : ILUTIL
      structure IlPrimUtil : PRIMUTIL
      structure IlLookup : ILLOOKUP
      structure IlStatic : ILSTATIC 
      structure Datatype : DATATYPE
      structure Basis : BASIS
      structure InfixParse : INFIXPARSE
      structure Pat : PAT
      structure Toil : TOIL
      structure IlEval : ILEVAL

      val initial_sbnds : Il.sbnd list
      val initial_context : Il.context

      val elaborate       : string -> Il.sbnd list * Il.context (* uses the inital_context *)
      val elaborate_diff  : string -> Il.sbnd list * Il.context_entry list (* returns only the differential *)
      val elaborate'      : Il.context * string -> Il.sbnd list * Il.context
      val elaborate_diff' : Il.context * string -> Il.sbnd list * Il.context_entry list
      val evaluate  : Il.sbnd list -> Il.sbnd list
      val parse : string -> Ast.dec
      val setdepth : int -> unit
  end
*)
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
	structure Ppprim = Ppprim(structure Prim = Prim);
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
	structure InfixParse = InfixParse(structure Il = Il
					  structure Ppil = Ppil
					  structure AstHelp = AstHelp);
	structure Pat = Pat(structure Il = Il
			    structure IlContext = IlContext
			    structure IlStatic = IlStatic
			    structure IlUtil = IlUtil
			    structure AstHelp = AstHelp
			    structure Datatype = Datatype
			    structure Ppil = Ppil
			    structure InfixParse = InfixParse);
	structure Toil = Toil(structure Il = Il
			      structure IlContext = IlContext
			      structure AstHelp = AstHelp
			      structure IlStatic = IlStatic
			      structure IlUtil = IlUtil
			      structure Ppil = Ppil
			      structure Pat = Pat
			      structure Datatype = Datatype
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

	val _ = Compiler.Control.Print.printDepth := 15;
	val _ = Pagewidth := 80;
	val error = fn s => Util.error "linkil.sml" s

	fun setdepth d = Compiler.Control.Print.printDepth := d
	fun parse s = 
	    let val (is,dec) = LinkParse.parse_one s
	    in (Source.filepos is, LinkParse.named_form_dec(LinkParse.tvscope_dec dec))
	    end
	    
	val _ = Ppil.convar_display := Ppil.VALUE_ONLY

	val empty_context = IlContext.empty_context
	val (initial_context, initial_sbnds) = Basis.initial_context()
	val initial_sbnds_len = length initial_sbnds


	fun local_add_context_entries(ctxt,entries) = 
	    let fun help (CONTEXT_SDEC(SDEC(l,dec))) = CONTEXT_SDEC(SDEC(l,SelfifyDec dec))
		  | help ce = ce
		val entries' = map help entries
	    in IlContext.add_context_entries(ctxt,entries')
	    end

	fun elaborate_diff'(context,(filepos,astdec)) = 
	    (case (Toil.xdec(context,filepos,astdec)) of
		SOME sbnd_ctxt_list =>
		    let val sbnds = List.mapPartial #1 sbnd_ctxt_list
			val ctxts = map #2 sbnd_ctxt_list
		    in	SOME(sbnds,ctxts)
		    end
	      | _ => NONE)

	fun elaborate'(context,(filepos,astdec)) = 
	    (case (elaborate_diff'(context,(filepos,astdec))) of
		 SOME(sbnds,context_diff) =>
		     SOME(sbnds,local_add_context_entries(context,context_diff))
	       | _ => NONE)

	fun elaborate s = elaborate'(initial_context,parse s)
	fun elaborate_diff s = elaborate_diff'(initial_context,parse s)
	fun evaluate target_sbnds =
	    let
		val allsbnds = initial_sbnds @ target_sbnds
		val module = Il.MOD_STRUCTURE allsbnds
		val allresmodule = IlEval.eval_mod module
		val ressbnds =
		    (case allresmodule of
			 Il.MOD_STRUCTURE allressbnds => List.drop(allressbnds,initial_sbnds_len)
		       | _ => error "a structure evaluated to a non-structure")
	    in ressbnds
	    end


	
	fun check' filename {doprint,docheck} =
	    let
		val _ = Stats.reset_stats()
  	        val (is,astdec) = (Stats.timer("PARSING",LinkParse.parse_one)) filename
		val fp = Source.filepos is
  	        val astdec = (Stats.timer("PARSE_TVSCOPE",LinkParse.tvscope_dec)) astdec
  	        val astdec = (Stats.timer("PARSE_NAMEDFORM",LinkParse.named_form_dec)) astdec
		val (sbnds,cdiff) = 
		    (case (Stats.timer("ELABORATION",elaborate_diff')) (initial_context,(fp,astdec)) of
			 SOME res => res
		       | NONE => error "Elaboration failed")
		val _ = if doprint 
			    then (print "test: sbnds are: \n";
				  Ppil.pp_sbnds sbnds)
			else ()
		val m = MOD_STRUCTURE sbnds
		val sdecs = List.mapPartial (fn (CONTEXT_SDEC sdec) => SOME sdec | _ => NONE) cdiff
		val given_s = SIGNAT_STRUCTURE (NONE,sdecs)
		val _ =  if doprint
			     then (print "\ngiven_s is:\n";
				   Ppil.pp_signat given_s)
			 else ()
		val msize = IlUtil.mod_size m
		val ssize = IlUtil.sig_size given_s
		val _ = (Stats.int "Module Size") := msize
		val _ = (Stats.int "Signature Size") := ssize
		val ctxt = initial_context 
		val _ =
		    if docheck
			then 
			    let val precise_s = (Stats.timer("TYPECHECKING",IlStatic.GetModSig))(ctxt,m)
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
						   Ppil.pp_context (local_add_context_entries(empty_context,
											      cdiff));
						   print "\n")
					 else ()
			    in (case ((Stats.timer("SANITY_CHECK",sanity_check))
				      (ctxt,m,precise_s,given_s)) of
				    NONE => ()
				  | SOME str => (print "\n\n****** SANITY_CHECK FAILED: ";
						 print str;
						 print " ******\n\n";
						 error "sanity-check"))
			    end
			else ()
		val _ = Stats.print_stats()
	    in  (m,given_s)
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
    end (* struct *)

