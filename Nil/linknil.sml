(* Reorder *)

structure Linknil :> LINKNIL  =
  struct

    val LinkNilDiag = Stats.ff("LinkNilDiag")
    val do_cse = Stats.tt("doCSE")         (* do CSE during optimzie2 *)
    val do_uncurry = Stats.ff("doUncurry") (* do uncurry during optimize2 *)
    val UptoPhasesplit = Stats.ff("UptoPhasesplit")
    val show_size = Stats.ff("showSize")   (* show size after each pass *)
    val show_html = Stats.ff("showHTML")   (* when showing pass results, generate HTML *)
    val typecheck = Stats.tt "Typecheck"   (* typecheck after each pass *)
    val measure   = Stats.ff "Measure"     (* measure after each pass *)
    val show      = Stats.ff "ShowNil"     (* show after each pass *)

    (*val typeof_elim = fn nilmod => Typeof_Elim.mod_elim (NilContext.empty()) nilmod*)

    fun makeEntry (enable, str) = ((if enable then Stats.tt else Stats.ff) ("do" ^ str),
				   Stats.ff("show" ^ str),
				   Stats.ff("check" ^ str),
				   Stats.ff("wcheck" ^ str),
				   Stats.ff("measure" ^ str),
				   str)

    val phasesplit  = makeEntry (true, "Phasesplit")
    val cc          = makeEntry (true, "ClosureConv")
    val optimize1   = makeEntry (true, "Optimize1")
    val optimize2   = makeEntry (true, "Optimize2")
    val optimize3   = makeEntry (true, "Optimize3")
    val optimize4   = makeEntry (true, "Optimize4")
    val optimize5   = makeEntry (true, "Optimize5")
    val reify1      = makeEntry (true, "Reify1")
    val reify2      = makeEntry (true, "Reify2")
    val vararg      = makeEntry (true, "Vararg")
    val specialize1 = makeEntry (true, "Specialize1")
    val specialize2 = makeEntry (true, "Specialize2")
    val rename      = makeEntry (true, "Rename")
    val hoist1      = makeEntry (true, "Hoist1")
    val hoist2      = makeEntry (true, "Hoist2")
    val inlineOnce1 = makeEntry (true, "InlineOnce1")
    val inlineOnce2 = makeEntry (true, "InlineOnce2")
    val inline1     = makeEntry (true, "Inline1")
    val inline2     = makeEntry (true, "Inline2")
    val inline3     = makeEntry (true, "Inline3")
    val coerce_elim = makeEntry (false, "CoerceElim")
    val sing_elim   = makeEntry (false, "SingElim")
    val rename2      = makeEntry (false, "Rename2")

    val walk         = makeEntry (true, "Walk")
    val context_walk = makeEntry (true, "ContextWalk")
    val bind_walk    = makeEntry (true, "BindWalk")
    (*val worst_walk   = makeEntry (true, "WorstWalk")*)

(*  val reorder     = makeEntry (false, "Reorder") *)

    val error = fn s => Util.error "linknil.sml" s

(*
    val debug = Stats.ff "nil_debug"
    val profile = Stats.ff "nil_profile"
    val closure_print_free = Stats.ff "closure_print_free"
*)

    structure Ppnil = Ppnil
    structure Alpha = Alpha
    structure NilSubst = NilSubst
    structure NilUtil = NilUtil
    structure NilError = NilError
    structure NilContext = NilContext
    structure Normalize = Normalize

    structure NilStatic = NilStatic


    structure Vararg = Vararg
    structure Tonil = Tonil
    structure Optimize = Optimize
    structure Specialize = Specialize
    structure Linearize = Linearize
    structure ToClosure = ToClosure



    fun printBold str =
	if (!LinkNilDiag) then
	    (print "===== "; print str; print " =====\n")
	else ()

    fun pass filename (ref false, _, _, _, _, _) (transformer,obj) =
	error "pass called with a false flag"
      | pass filename (ref true, showphase, checkphase, wcheckphase, measurephase, phasename) (transformer,obj) =
	let val str = "Starting " ^ phasename ^ ": " ^ filename
	    val _ = Timestamp.timestamp()
	    val _ = printBold str
	    val nilmod = Stats.timer(phasename,transformer) obj
	    val _ = if !show_size
		      then
			  (print "  size = ";
			   print (Int.toString (NilUtil.module_size nilmod));
			   print "\n")
		    else ()
	    val _ = if !showphase orelse !show then
		      ((if !show_html then
			   PpnilHtml.pp_module
		       else
			   Ppnil.pp_module)
			   {module = nilmod,
			    header = phasename,
			    name = filename,
			    pass = phasename};
		      print "\n")
		    else ()
	    val _ = if !checkphase orelse !typecheck then
		      Stats.timer(phasename^"_Typecheck",NilStatic.module_valid) (NilContext.empty (), nilmod)
		    else ()
(*	    val _ = 
	      let
		val max = Stats.int "MaxVarsBound"
		val phasemax = Stats.int (phasename^"MaxVarsBound")
		val bound_count = Stats.int "BoundVariables"
		val unused_count = Stats.int "UnusedVariables"
		val phasebound = Stats.int (phasename^"::BoundVariables")
		val phaseunused = Stats.int (phasename^"::UnusedVariables")
	      in
		phasemax := Int.max (!max,!phasemax);
		phasebound := Int.max (!bound_count,!phasebound);
		phaseunused := Int.max (!unused_count,!phaseunused);
		max := 0;
		bound_count := 0;
		unused_count := 0
	      end*)

	    (* Note that these get redirected in the self-compile,
	     * since TIL can't handle the Wizard.  (Datatype bug)
	     *)
	    (*val _ = if !wcheckphase orelse !wtypecheck then
		      WNilStatic.module_valid (WNilContext.empty (), NilToWizard.nil_to_wizard nilmod)
		    else ()*)
	val _ =
	  if !measurephase orelse !measure then
	    let
	      val (imps,bnds,exps) = Measure.mod_size' {cstring=Measure.cstring,count=[],count_in=[]} nilmod
	      val impsr = Stats.int (phasename^"::importsize")
	      val totcsr = Stats.int (phasename^"::totalconsize")
	      val totsr = Stats.int (phasename^"::totalsize")
	    in
		impsr := !impsr + (#total imps);
		totcsr := !totcsr + (#cons imps) + (#cons bnds) + (#cons exps);
		totsr := !totsr + (#total imps) + (#total bnds) + (#total exps)
	    end
	  else ()
	in  nilmod
	end

    fun transform filename (ref false,_,_,_,_,phasename) (_,nilmod) =
	let val str = "Skipping " ^ phasename ^ " : " ^ filename
	in  (* printBold str;  *)
	    nilmod
	end
      | transform filename (tr as (ref true,_,_,_,_,_)) arg =
	pass filename tr arg


    exception Stop of Nil.module

    (* (1) Rename must precede everything.
       (2) Vararg must precede Reify because vararg changes traceability
       (3) Reify must occur before uncurrying.  This is
           because we do not allow type applications in traceability annotations.
	   Therefore, we allow reify to insert any needed type applications
	   between the type lambda and the term lambda before we try to uncurry,
	   so that we block uncurrying when it would force us to place applications
	   in the trace annotations.
       (4) Reify2 is needed because some optimizations create TraceUnknowns.
    *)
    fun compile' (filename,ilmodule : Il.module) =
	let
	    val pass = pass filename
	    val transform = transform filename

	    open Nil Name
	    val D = NilContext.empty()

	    val nilmod = pass phasesplit (Tonil.phasesplit, ilmodule)

	    val _ = if !UptoPhasesplit
			then raise (Stop nilmod)
		    else ()

	    val nilmod = transform coerce_elim (CoerceElim.transform, nilmod)

	    val nilmod = transform rename (Linearize.linearize_mod, nilmod)




	    val nilmod = transform sing_elim (SingletonElim.R_module, nilmod)

	    val nilmod = transform rename2 (Linearize.linearize_mod, nilmod)

	    val nilmod = transform hoist1 (Hoist.optimize, nilmod)

	    val nilmod = transform optimize1
				   (Optimize.optimize {doDead = true,
						       doProjection = SOME 50,
						       doCse = true,
						       doUncurry = false},
				    nilmod)

	    val nilmod = transform inlineOnce1 (Inline.inline_once  true, nilmod)

	    (* It is very important that specialize be run before any speculative
	     * inlining (that is, before inlining anything except functions called
	     * only once).  Otherwise, the inliner may generate lots of copies of
	     * the same function applied at the same type, which is a complete loss.
	     * We would much rather specialize them to get a single monomorphic copy,
	     * and then inline that single copy as necessary.
	     *)
	    val nilmod = transform specialize1 (Specialize.optimize, nilmod)

	    val nilmod = transform optimize2
				   (Optimize.optimize {doDead = true,
						       doProjection = SOME 50,
						       doCse = true,
						       doUncurry = false},
				    nilmod)

	    val nilmod = transform inlineOnce2 (Inline.inline_once  true, nilmod)

	    val nilmod = transform vararg (Vararg.optimize, nilmod)


	    (* It might be a good idea to iterate inline (and possibly optimize)
	     * with the thresholds set to zero until the code reaches a fixpoint.
	     * The idea would be to inline all functions called only once right
	     * off the bat.  This is more inline with Tarditi's thesis.  As it stands,
	     * some functions only called once will not get inlined, or will not get
	     * inlined until the last inline stage, because they are deeply curried.
	     * Another alternative might be to have a special "inlineOnce" pass, since
	     * the analysis could be much simpler.
	     * -leaf
	     *)
	    val nilmod = transform inline1
		                   (Inline.inline {iterate = false,
						   tinyThreshold = 10,
						   sizeThreshold = 50,
						   occurThreshold = 5},
				    nilmod)

            val nilmod = transform reify1 (Reify.reify_mod, nilmod)


	    val nilmod = transform hoist2 (Hoist.optimize, nilmod)

	    val nilmod = transform optimize3 (Optimize.optimize {doDead = true,
								 doProjection = SOME 50,
								 doCse = !do_cse,
								 doUncurry = !do_uncurry},
					      nilmod)
	    val nilmod = transform inline2
		                   (Inline.inline {iterate = false,
						   tinyThreshold = 10,
						   sizeThreshold = 50,
						   occurThreshold = 5},
				    nilmod)

	    val nilmod = transform optimize4
				   (Optimize.optimize {doDead = true,
						       doProjection = SOME 50,
						       doCse = !do_cse,
						       doUncurry = !do_uncurry},
				   nilmod)

	    val nilmod = transform specialize2 (Specialize.optimize, nilmod)

	    val nilmod = transform inline3
		                   (Inline.inline {iterate = true,
						   tinyThreshold = 10,
						   sizeThreshold = 50,
						   occurThreshold = 5},
				    nilmod)

	    (* Optimizing after every inlining is a good thing.
	     * We could notice when the inliner failed to do anything
	     * and not do this pass in that case.  This would be
	     * especially true if the optimizer were actually idempotent
	     * (which it tries to be).
	     * -leaf
	     *)
	    val nilmod = transform optimize5
				   (Optimize.optimize {doDead = true,
						       doProjection = SOME 50,
						       doCse = !do_cse,
						       doUncurry = !do_uncurry},
				   nilmod)

            val nilmod = transform reify2 (Reify.reify_mod, nilmod)

	    val nilmod = transform cc (ToClosure.close_mod, nilmod)

	    (* We should consider experimenting with some optimizations
	     * after closure conversion.  CSE and hoisting could definitely
	     * be done, and dead-code elimination?  Other optimizations discussed
	     * in Greg and Bob's paper?
	     * -leaf
	     *)

(*	    val nilmod = transform reorder (Reorder.optimize, nilmod) *)

	in  nilmod
	end
    handle Stop nilmod => nilmod

    val il_to_nil = compile'

end
