(*$import Prelude TopLevel Util Int Il Name LinkIl Annotation Nil NilUtil NilContext Ppnil ToNil Optimize Specialize Normalize Linearize ToClosure  LINKNIL Stats Alpha NilSubst NilError PrimUtil Hoist Reify NilStatic Inline PpnilHtml Measure Vararg Real Timestamp *)

(* Reorder *)

structure Linknil :> LINKNIL  =
  struct

    val do_cse = Stats.tt("doCSE")         (* do CSE during optimzie2 *)
    val do_uncurry = Stats.ff("doUncurry") (* do uncurry during optimize2 *)
	
    val show_size = Stats.ff("showSize")   (* show size after each pass *)
    val show_html = Stats.ff("showHTML")   (* when showing pass results, generate HTML *)
    val typecheck = Stats.ff "Typecheck"   (* typecheck after each pass *)
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
    val typeofelim1 = makeEntry (true, "TypeofElim1")
    val typeofelim2 = makeEntry (true, "TypeofElim2")
    val cc          = makeEntry (true, "ClosureConv")
    val optimize1   = makeEntry (true, "Optimize1")
    val optimize2   = makeEntry (true, "Optimize2")
    val optimize3   = makeEntry (true, "Optimize3")
    val optimize4   = makeEntry (true, "Optimize4")
    val reify1      = makeEntry (true, "Reify1")
    val reify2      = makeEntry (true, "Reify2")
    val vararg      = makeEntry (true, "Vararg")
    val specialize  = makeEntry (true, "Specialize")
    val rename      = makeEntry (true, "Rename")
    val hoist       = makeEntry (true, "Hoist")
    val inline1     = makeEntry (true, "Inline1")
    val inline2     = makeEntry (true, "Inline2")
    val inline3     = makeEntry (true, "Inline3")
(*  val reduce      = makeEntry (false, "Reduce") *)
(*  val flatten     = makeEntry (false, "Flatten") *)
    val coerce_elim = makeEntry (false, "CoerceElim")

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

(*    structure Flatten = Flatten(structure PrimUtil = NilPrimUtil) 
    structure Reduce = Reduce 
*)


    fun printBold str = (print "===== "; print str;
			 print (Util.spaces (50 - (size str)));
			 print " =====\n")

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
		      NilStatic.module_valid (NilContext.empty (), nilmod)
		    else ()
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
       (3) Reify must occur before uncurrying which is in optimize2.
       (4) Reify2 is needed because some optimizations create TraceUnknowns.
    *)
    fun compile' (filename,(ctxt,_,sbnd_entries) : Il.module) =
	let
	    val pass = pass filename
	    val transform = transform filename

	    open Nil LinkIl.IlContext Name
	    val D = NilContext.empty()

	    val nilmod = pass phasesplit (Tonil.phasesplit, (ctxt,sbnd_entries))

	    val _ = if !(Stats.bool("UptoPhasesplit"))
			then raise (Stop nilmod)
		    else ()
	    val nilmod = transform rename (Linearize.linearize_mod, nilmod)

	    val nilmod = transform optimize1 
				   (Optimize.optimize {doDead = true, 
						       doProjection = SOME 50,
						       doCse = false, 
						       doUncurry = false},
				    nilmod)

	    val nilmod = transform vararg (Vararg.optimize, nilmod)

	    val nilmod = transform inline1
		                   (Inline.inline {sizeThreshold = 50, 
						   occurThreshold = 5},
				    nilmod)

            val nilmod = transform reify1 (Reify.reify_mod, nilmod)

	    val nilmod = transform specialize (Specialize.optimize, nilmod)

	    (*val _ = transform walk (Walker.walk, nilmod)
	    val _ = transform context_walk (Walker.context_walk, nilmod)
	    val _ = transform bind_walk (Walker.bind_walk, nilmod)
	    val _ = transform worst_walk (Walker.worst_walk, nilmod)*)

	    val nilmod = transform hoist (Hoist.optimize, nilmod)

	    val nilmod = transform optimize2
				   (Optimize.optimize {doDead = true, 
						       doProjection = SOME 50,
						       doCse = !do_cse, 
						       doUncurry = !do_uncurry},
				   nilmod) 
	    val nilmod = transform inline2 
		                   (Inline.inline {sizeThreshold = 50, 
						   occurThreshold = 5},
				    nilmod)
	    val nilmod = transform optimize3
				   (Optimize.optimize {doDead = true, 
						       doProjection = SOME 50,
						       doCse = !do_cse, 
						       doUncurry = !do_uncurry},
				   nilmod) 
	    val nilmod = transform inline3 
		                   (Inline.inline {sizeThreshold = 50, 
						   occurThreshold = 5},
				    nilmod)

	    (* Optimizing after every inlining is a good thing.  
	     * We could notice when the inliner failed to do anything
	     * and not do this pass in that case.  This would be
	     * especially true if the optimizer was actually idempotent
	     * (which it tries to be).
	     *)
	    val nilmod = transform optimize4
				   (Optimize.optimize {doDead = true, 
						       doProjection = SOME 50,
						       doCse = !do_cse, 
						       doUncurry = !do_uncurry},
				   nilmod) 
            val nilmod = transform reify2 (Reify.reify_mod, nilmod)

	    val nilmod = transform cc (ToClosure.close_mod, nilmod)

	in  nilmod
	end
    handle Stop nilmod => nilmod

    val il_to_nil = compile'

end
