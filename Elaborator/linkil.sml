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

      type module = (Il.context * Il.sbnd list * Il.sdec list) 
      val compile_prelude : bool * string -> module
      val compile : string -> module option
      val test : string ->  module option
      val setdepth : int -> unit (* printing depth *)
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
        type module = (Il.context * Il.sbnd list * Il.sdec list) 

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

	fun local_add_context_entries(ctxt,entries) = 
	    let fun help (CONTEXT_SDEC(SDEC(l,dec))) = CONTEXT_SDEC(SDEC(l,SelfifyDec dec))
		  | help ce = ce
		val entries' = map help entries
	    in IlContext.add_context_entries(ctxt,entries')
	    end

	val empty_context = IlContext.empty_context

(*
	val initial_context' = 
	local_add_context_entries(empty_context, 
			 map CONTEXT_SDEC
			 (IlStatic.GetSbndsSdecs(empty_context,initial_sbnds)))
*)

	fun elaborate_help (context,(filepos,astdec)) = 
	    (case (Toil.xdec(context,filepos,astdec)) of
		 SOME sbnd_ctxt_list =>
		     let val sbnds = List.mapPartial #1 sbnd_ctxt_list
			 val ctxts = map #2 sbnd_ctxt_list
		     in	SOME(sbnds,ctxts)
		     end
	       | _ => NONE)
	val elaborate_help = Stats.timer("Elaboration",elaborate_help)

	fun entries2sdecs entries = 
		let fun get_sdec (CONTEXT_SDEC sdec) = SOME sdec
		      | get_sdec _ = NONE
		in List.mapPartial get_sdec entries	    
		end

	fun elaborate_quad (context_before,sbnds,sdecs,context_after) filename = 
	    (case (elaborate_help(context_after,
				  LinkParse.parse_all filename)) of
		 SOME (sbnds',entries') =>
		     let val sdecs' = entries2sdecs entries'
			 val sbnds = sbnds @ sbnds'
			 val sdecs = sdecs @ sdecs'
		   	 val context' = local_add_context_entries(context_after,entries')
			 val _ = print "Elaboration complete\n"
		     in  SOME(context_before, sbnds, sdecs, context')
		     end
	       | NONE => NONE)

	val initial_quad = Basis.initial_context()
	val prelude_quad = ref (NONE : (context * sbnds * sdecs * context) option)
	local
       	    fun reparse filename = 
			let val q = elaborate_quad initial_quad filename
			    val _ = prelude_quad := q
			in  case q of 
				SOME (ctxt,sd,sb,_) => (ctxt,sd,sb)
				| NONE => error "prelude failed to elaborate"
			end
	in  fun elaborate_prelude (use_cache,filename) =
		(case (!prelude_quad, use_cache) of
			(SOME (ctxt,sd,sb,_), true) => (ctxt,sd,sb)
		      | _ => reparse filename)
	end

	fun elaborate filename = 
		(case (!prelude_quad) of
			NONE => error "prelude not elaborated yet"
		      | SOME q => 
			(case elaborate_quad q filename of
			  SOME (ctxt,sd,sb,_) => SOME(ctxt,sd,sb)
			| NONE => NONE))
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

	
	fun check' filename {doprint,docheck} =
	    let
		val (context,sbnds,sdecs) = 
			(case elaborate filename of
			       SOME res => res
			     | NONE => error "Elaboration failed")
		val _ = if doprint 
			    then (print "test: sbnds are: \n";
				  Ppil.pp_sbnds sbnds)
			else ()
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
		val _ =
		    if docheck
			then 
			    let val precise_s = (Stats.timer("TYPECHECKING",IlStatic.GetModSig))(context,m)
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
			    in (case ((Stats.timer("SANITY_CHECK",sanity_check))
				      (context,m,precise_s,given_s)) of
				    NONE => ()
				  | SOME str => (print "\n\n****** SANITY_CHECK FAILED: ";
						 print str;
						 print " ******\n\n";
						 error "sanity-check"))
			    end
		    else ()
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
	    in  (context,sbnds,sdecs)
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





	val compile_prelude = elaborate_prelude
	val compile = elaborate
	val test = (fn filename => SOME(ptest_res filename) handle _ => NONE)

    end (* struct *)

