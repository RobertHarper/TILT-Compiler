(* Reorder *)

structure Linknil :> LINKNIL  =
  struct

    val LinkNilDiag = Stats.ff("LinkNilDiag")
    val do_uncurry = Stats.tt("doUncurry") (* do uncurry *)

    val do_poly_uncurry = CompilerControl.UncurryPolymorphism
    val liltarget  = CompilerControl.NilTargetsLil  (* Compiling to LIL *)

    val UptoPhasesplit = Stats.ff("UptoPhasesplit")
    val show_size  = Stats.ff("showSize")   (* show size after each pass *)
    val show_html  = Stats.ff("showHTML")   (* when showing pass results, generate HTML *)
    val typecheck  = Stats.tt "Typecheck"   (* typecheck after each pass *)
    val measure    = Stats.ff "Measure"     (* measure after each pass *)
    val show       = Stats.ff "ShowNil"     (* show after each pass *)

    (*val typeof_elim = fn nilmod => Typeof_Elim.mod_elim (NilContext.empty()) nilmod*)

    fun makeEntry (enable, check, str) =
	((if enable then Stats.tt else Stats.ff) ("do" ^ str),
	 Stats.ff("show" ^ str),
	 (if check then Stats.tt else Stats.ff) ("check" ^ str),
	 Stats.ff("measure" ^ str),
	 str)
    fun makeEntry' (doflag, check, str) =
	(doflag,
	 Stats.ff("show" ^ str),
	 (if check then Stats.tt else Stats.ff) ("check" ^ str),
	 Stats.ff("measure" ^ str),
	 str)

    val phasesplit  = makeEntry (true, true, "Phasesplit")
    val cc          = makeEntry' (CompilerControl.ClosureConvertNil, true, "ClosureConv")
    val optimize1   = makeEntry (true, false, "Optimize1")
    val optimize2   = makeEntry (true, false, "Optimize2")
    val optimize3   = makeEntry (true, false, "Optimize3")
    val optimize4   = makeEntry (true, false, "Optimize4")
    val optimize5   = makeEntry (true, false, "Optimize5")
    val optimize6   = makeEntry (true, false, "Optimize6")
    val reify1      = makeEntry (true, false, "Reify1")
    val reify2      = makeEntry (true, false, "Reify2")
    val vararg      = makeEntry (true, false, "Vararg")
    val specialize1 = makeEntry (true, false, "Specialize1")
    val specialize2 = makeEntry (true, false, "Specialize2")
    val rename      = makeEntry (true, false, "Rename")
    val hoist1      = makeEntry (true, false, "Hoist1")
    val hoist2      = makeEntry (true, false, "Hoist2")
    val inlineOnce1 = makeEntry (true, false, "InlineOnce1")
    val inlineOnce2 = makeEntry (true, false, "InlineOnce2")
    val inline1     = makeEntry (true, false, "Inline1")
    val inline2     = makeEntry (true, false, "Inline2")
    val inline3     = makeEntry (true, false, "Inline3")
    val coerce_elim = makeEntry (false, false, "CoerceElim")
    val sing_elim   = makeEntry' (CompilerControl.EliminateSingletons, false, "SingElim")
    val rename2     = makeEntry' (CompilerControl.EliminateSingletons, false, "Rename2")

(*    val walk         = makeEntry (true, false, "Walk")
    val context_walk = makeEntry (true, false, "ContextWalk")
    val bind_walk    = makeEntry (true, false, "BindWalk")
    val worst_walk   = makeEntry (true, false, "WorstWalk")*)

(*  val reorder     = makeEntry (false, false, Reorder") *)

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

    fun pass filename (ref false, _, _, _, _) (transformer,obj) =
	error "pass called with a false flag"
      | pass filename (ref true, showphase, checkphase, measurephase, phasename) (transformer,obj) =
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
		val max = Stats.counter "MaxVarsBound"
		val phasemax = Stats.counter'(phasename^"MaxVarsBound")
		val bound_count = Stats.counter "BoundVariables"
		val unused_count = Stats.counter "UnusedVariables"
		val phasebound = Stats.counter'(phasename^"::BoundVariables")
		val phaseunused = Stats.counter'(phasename^"::UnusedVariables")
	      in
		Stats.counter_max(phasemax,Stats.counter_fetch max);
		Stats.counter_max(phasebound,Stats.counter_fetch bound_count);
		Stats.counter_max(phaseunused,Stats.counter_fetch unused_count);
		Stats.counter_clear max;
		Stats.counter_clear bound_count;
		Stats.counter_clear unused_count
	      end*)

	val _ =
	  if !measurephase orelse !measure then
	    let
	      val (imps,bnds,exps) = Measure.mod_size' {cstring=Measure.cstring,count=[],count_in=[]} nilmod
	      val impsr = Stats.counter (phasename^"::importsize")
	      val totcsr = Stats.counter (phasename^"::totalconsize")
	      val totsr = Stats.counter (phasename^"::totalsize")
	    in
		Stats.counter_add(impsr,(#total imps));
		Stats.counter_add(totcsr,(#cons imps) + (#cons bnds) + (#cons exps));
		Stats.counter_add(totsr,(#total imps) + (#total bnds) + (#total exps))
	    end
	  else ()
	in  nilmod
	end

    fun transform filename (ref false,_,_,_,phasename) (_,nilmod) =
	let val str = "Skipping " ^ phasename ^ " : " ^ filename
	in  (* printBold str;  *)
	    nilmod
	end
      | transform filename (tr as (ref true,_,_,_,_)) arg =
	pass filename tr arg


    exception Stop of Nil.module

    (* (1) Rename must precede everything.
       (2) Vararg must precede Reify because vararg changes traceability
       (3) Reify must occur before general uncurrying.  This is
           because we do not allow type applications in traceability annotations,
	   so in the case of a Lambda-lambda, we would have no place to write
	   down the application.  So the only time it is safe to do general uncurrying
	   is immediately after reification.  Therefore, we allow reify to insert any 
	   needed type applications between the type lambda and the term lambda before 
	   we try to uncurry,  so that we block uncurrying when it would force us to place 
	   applications in the trace annotations.   So the only time it is safe to do 
	   general uncurrying is immediately after reification.  We can do non-polymorphic 
	   uncurrying at any time.
	   We can always uncurry when targetting the Lil.
       (4) Reify2 is needed because some optimizations create TraceUnknowns.
       (5) Running hoist before specialize helps quite a bit.
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

	    val nilmod = transform optimize1
				   (Optimize.optimize {doDead = true,
						       doProjection = SOME 50,
						       doCse = true,
						       doPolyUncurry = false,
						       doUncurry = false},
				    nilmod)

	    (* It is very important that specialize be run before any speculative
	     * inlining (that is, before inlining anything except functions called
	     * only once).  Otherwise, the inliner may generate lots of copies of
	     * the same function applied at the same type, which is a complete loss.
	     * We would much rather specialize them to get a single monomorphic copy,
	     * and then inline that single copy as necessary.
	     *
	     * It's also important to run this before uncurrying, since uncurried
	     * functions won't get specialized.  
	     *)
	    val nilmod = transform specialize1 (Specialize.optimize, nilmod)

	    val nilmod = transform inlineOnce1 (Inline.inline_once  (true,true), nilmod)

	    (* It's ok to uncurry here when targetting the LIL, since
	     * we don't use traces in the LIL 
	     *)
	    val nilmod = transform optimize2
				   (Optimize.optimize {doDead = true,
						       doProjection = SOME 50,
						       doCse = true,
						       doPolyUncurry = !liltarget,
						       doUncurry = !do_uncurry},
				    nilmod)


	    val nilmod = transform sing_elim (SingletonElim.R_module, nilmod)

	    (* sing_elim doesn't produce a-normal types *)
	    val nilmod = transform rename2 (Linearize.linearize_mod, nilmod)

	    val nilmod = transform hoist1 (Hoist.optimize, nilmod)


	    val nilmod = transform optimize3
				   (Optimize.optimize {doDead = true,
						       doProjection = SOME 50,
						       doCse = true,
						       doPolyUncurry = false,
						       doUncurry = false},
				    nilmod)

	    val nilmod = transform inline1
		                   (Inline.inline {iterate = true,
						   inlinecons = true,
						   tinyThreshold = 20,
						   sizeThreshold = 50,
						   occurThreshold = 5},
				    nilmod)

	    val nilmod = transform vararg (Vararg.optimize, nilmod)

	    val nilmod = transform optimize4
				   (Optimize.optimize {doDead = true,
						       doProjection = SOME 50,
						       doCse = true,
						       doPolyUncurry = !do_poly_uncurry,
						       doUncurry = !do_uncurry},
				    nilmod)

            val nilmod = transform reify1 (Reify.reify_mod, nilmod)

	    val nilmod = transform inline2
		                   (Inline.inline {iterate = false,
						   inlinecons = false,
						   tinyThreshold = 20,
						   sizeThreshold = 50,
						   occurThreshold = 5},
				    nilmod)


	    val nilmod = transform hoist2 (Hoist.optimize, nilmod)
	      
	    val nilmod = transform optimize5
				   (Optimize.optimize {doDead = true,
						       doProjection = SOME 50,
						       doCse = true,
						       doPolyUncurry = false,
						       doUncurry = false},
				   nilmod)

	    val nilmod = transform specialize2 (Specialize.optimize, nilmod)

	    val nilmod = transform inline3
		                   (Inline.inline {iterate = true,
						   inlinecons = true,
						   tinyThreshold = 20,
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
	    val nilmod = transform optimize6
				   (Optimize.optimize {doDead = true,
						       doProjection = SOME 50,
						       doCse = true,
						       doPolyUncurry = false,
						       doUncurry = false},
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



    local
      fun pass filename (ref false, _, _, _, _) (transformer,obj) =
	error "pass called with a false flag"
	| pass filename (ref true, showphase, checkphase, measurephase, phasename) (transformer,obj) =
	let val str = "Starting " ^ phasename ^ ": " ^ filename
	    val _ = Timestamp.timestamp()
	    val _ = printBold str
	    val nilint = Stats.timer(phasename,transformer) obj

	    val _ = if !showphase orelse !show then
		      ((if !show_html then
			   PpnilHtml.pp_interface
		       else
			   Ppnil.pp_interface)
			   {interface = nilint,
			    header = phasename,
			    name = filename,
			    pass = phasename};
		      print "\n")
		    else ()
	    val _ = if !checkphase orelse !typecheck then
		      Stats.timer(phasename^"_Typecheck",NilStatic.interface_valid) (NilContext.empty (), nilint)
		    else ()
	in  nilint
	end
      
      fun transform filename (ref false,_,_,_,phasename) (_,nilint) =
	let val str = "Skipping " ^ phasename ^ " : " ^ filename
	in  (* printBold str;  *)
	    nilint
	end
	| transform filename (tr as (ref true,_,_,_,_)) arg =
	pass filename tr arg
	
      exception Stop of Nil.interface
    in
      fun compile_interface' (filename,ilinterface : Il.sc_module) : Nil.interface =
	let
	    val pass = pass filename
	    val transform = transform filename

	    open Nil Name
	    val D = NilContext.empty()

	    val nilint = pass phasesplit (Tonil.phasesplit_interface, ilinterface)

	    val _ = if !UptoPhasesplit
			then raise (Stop nilint)
		    else ()

	    val nilint = transform rename (Linearize.linearize_int, nilint)

	    val nilint = transform optimize1
				   (Optimize.optimize_int {doDead = true,
							   doProjection = SOME 50,
							   doCse = true,
							   doPolyUncurry = false,
							   doUncurry = false},
				    nilint)


	    val nilint = transform sing_elim (SingletonElim.R_interface, nilint)

	    (* sing_elim doesn't produce a-normal types *)
	    val nilint = transform rename2 (Linearize.linearize_int, nilint)

	    val nilint = transform hoist1 (Hoist.optimize_int, nilint)

	    val nilint = transform optimize2
				   (Optimize.optimize_int {doDead = true,
							   doProjection = SOME 50,
							   doCse = true,
							   doPolyUncurry = false,
							   doUncurry = false},
				    nilint)

	    (* Could consider some inlining to make code smaller *)
(*	    val nilint = transform inline1
		                   (Inline.inline_int {iterate = true,
						       inlinecons = true,
						       tinyThreshold = 20,
						       sizeThreshold = 50,
						       occurThreshold = 5},
				    nilint)
*)
	    val nilint = transform vararg (Vararg.optimize_int, nilint)

	    val nilint = transform optimize3
				   (Optimize.optimize_int {doDead = true,
							   doProjection = SOME 50,
							   doCse = true,
							   doPolyUncurry = false,
							   doUncurry = false},
				    nilint)
(*
	    val nilint = transform inline2
		                   (Inline.inline {iterate = false,
						   inlinecons = false,
						   tinyThreshold = 20,
						   sizeThreshold = 50,
						   occurThreshold = 5},
				    nilint)
*)

	    val nilint = transform hoist2 (Hoist.optimize_int, nilint)

	    val nilint = transform optimize4
	      (Optimize.optimize_int {doDead = true,
				      doProjection = SOME 50,
				      doCse = true,
				      doPolyUncurry = false,
				      doUncurry = false},
	       nilint)

	in  nilint
	end
      handle Stop nilint => nilint
    end

    val il_to_nil = compile'
    val ilint_to_nilint = compile_interface'
end
