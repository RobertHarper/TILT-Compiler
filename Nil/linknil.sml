(*$import Prelude TopLevel Util Int Il Name LinkIl Annotation Nil NilUtil NilContext Ppnil ToNil Optimize Specialize Normalize Linearize ToClosure  LINKNIL Stats Alpha NilSubst NilError PrimUtil Hoist Reify NilStatic Inline PpnilHtml Measure Vararg Dummy Typeof_Elim Real *)

(* Reorder *)

structure Linknil :> LINKNIL  =
  struct

    val do_cse = Stats.tt("doCSE")         (* do CSE during optimzie2 *)
    val do_uncurry = Stats.ff("doUncurry") (* do uncurry during optimize2 *)
    val show_size = Stats.ff("showSize")
    val show_html = Stats.ff("showHTML")

    fun makeEntry (enable, str) = ((if enable then Stats.tt else Stats.ff) ("do" ^ str),
				   Stats.ff("show" ^ str), str)
    val phasesplit  = makeEntry (true, "Phasesplit")
    val cc          = makeEntry (true, "ClosureConv")
    val optimize1   = makeEntry (true, "Optimize1")
    val optimize2   = makeEntry (true, "Optimize2")
    val optimize3   = makeEntry (true, "Optimize3")
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
    val coerce_elim = makeEntry (true, "CoerceElim")

    val measure_after_phaseplit = makeEntry (false, "MeasureAfterPhasesplit")
(*    val measure_after_opts      = makeEntry (false, "MeasureAfterOpts")*)
    val measure_after_cc        = makeEntry (false, "MeasureAfterCC")
(*  val reorder     = makeEntry (false, "Reorder") *)

    val typecheck_after_phasesplit = makeEntry(false,"TypecheckAfterPhasesplit")
    val typecheck_between_opts     = makeEntry(false,"TypecheckBetweenOpts")
    val typecheck_after_opt        = makeEntry(false,"TypecheckAfterOpt")
    val typecheck_after_opt2       = makeEntry(false,"TypecheckAfterOpt2")
    val typecheck_after_cc         = makeEntry(false,"TypecheckAfterCC")

    val wtypecheck_after_phasesplit = makeEntry(false,"WTypecheckAfterPhasesplit")
    val wtypecheck_after_opt        = makeEntry(false,"WTypecheckAfterOpt")
    val wtypecheck_after_opt2       = makeEntry(false,"WTypecheckAfterOpt2")
    val wtypecheck_after_cc         = makeEntry(false,"WTypecheckAfterCC")

    val error = fn s => Util.error "linknil.sml" s

    val debug = Stats.ff "nil_debug"
    val profile = Stats.ff "nil_profile"
    val closure_print_free = Stats.ff "closure_print_free"

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

    fun pass filename (ref false, showphase,phasename) (transformer,obj) =
	error "pass called with a false flag"
      | pass filename (ref true, showphase,phasename) (transformer,obj) =
	let val str = "Starting " ^ phasename ^ ": " ^ filename
	    val _ = printBold str
	    val nilmod = Stats.timer(phasename,transformer) obj
	    val _ = if !show_size
		      then
			  (print "  size = ";
			   print (Int.toString (NilUtil.module_size nilmod));
			   print "\n")
		    else ()
	    val _ = if !showphase then
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
	in  nilmod
	end

    fun transform filename (ref false,_,phasename) (_,nilmod) = 
	let val str = "Skipping " ^ phasename ^ " : " ^ filename
	in  (* printBold str;  *)
	    nilmod
	end
      | transform filename (tr as (ref true,showphase,phasename)) arg =
	pass filename tr arg


    fun typecheck nilmod = (NilStatic.module_valid (NilContext.empty (), nilmod); nilmod)

    (*Note that these get redirected in the self-compile, 
     * since TIL can't handle the Wizard.  (Datatype bug)
     *)
    fun wtypecheck nilmod = (WNilStatic.module_valid (WNilContext.empty (), NilToWizard.nil_to_wizard nilmod); nilmod)

    val typeof_count1 = ("PS",Stats.int "PostPS_Size", Stats.int "PostPS_Diff",Stats.int "PS_Max%_Increase")
    val typeof_count2 = ("CC",Stats.int "PostCC_Size", Stats.int "PostCC_Diff",Stats.int "CC_Max%_Increase")


    fun do_measure (pass,size,diff,max) filename nilmod = 
      let

	val meas   = Measure.mod_size {cstring = Measure.cstring,count = ["Typeof_c"], count_in = []} 
	val pre    = meas nilmod
	val nilmod = Typeof_Elim.mod_elim (NilContext.empty ()) nilmod
	val post   = meas nilmod
	val incr   = if pre > 0 then Real.round(100.0 * (Real.fromInt (post - pre) / Real.fromInt pre)) else 0
      in 
	size := !size + pre;
	diff := !diff + (post - pre);
	if incr > !max then 
	  (print ("\nNew "^pass^" Max: "^(Int.toString incr)^"% increase in file "^filename^"\n");
	   max := incr)
	else ();
	nilmod
      end
					    
    exception Stop of Nil.module

    (* (1) Rename must precede everything.
       (2) Vararg must precede Reify
       (3) Reify must occur before uncurrying which is in optimize2.
       (4) Reify2 is needed because some optimizations create TraceUnknowns.
    *)
    fun compile' debug (filename,(ctxt,_,sbnd_entries) : Il.module) =
	let
	    val pass = pass filename
	    val transform = transform filename

	    open Nil LinkIl.IlContext Name
	    val D = NilContext.empty()

	    val nilmod = pass phasesplit (Tonil.phasesplit, (ctxt,sbnd_entries))

	    val nilmod = transform measure_after_phaseplit (do_measure typeof_count1 filename, nilmod)

	    val nilmod = transform typecheck_after_phasesplit  (typecheck, nilmod)
	    val nilmod = transform wtypecheck_after_phasesplit (wtypecheck, nilmod)

	    val nilmod = transform coerce_elim (CoerceElim.transform, nilmod)


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

 	    val nilmod = transform typecheck_between_opts (typecheck,nilmod)

	    val nilmod = transform vararg (Vararg.optimize, nilmod)

(*	    val nilmod = transform reduce1 (Reduce.doModule, nilmod)  *)
(*	    val nilmod = transform flatten (Flatten.doModule, nilmod) *)

 	    val nilmod = transform typecheck_between_opts (typecheck,nilmod)

	    val nilmod = transform inline1
		                   (Inline.inline {sizeThreshold = 50, 
						   occurThreshold = 5},
				    nilmod)

 	    val nilmod = transform typecheck_between_opts (typecheck,nilmod)

            val nilmod = transform reify1 (Reify.reify_mod, nilmod)

 	    val nilmod = transform typecheck_after_opt (typecheck,nilmod)
 	    val nilmod = transform wtypecheck_after_opt (wtypecheck,nilmod)

	    val nilmod = transform specialize (Specialize.optimize, nilmod)
	    val nilmod = transform hoist (Hoist.optimize, nilmod)
 	    val nilmod = transform typecheck_after_opt (typecheck,nilmod)
	    val nilmod = transform optimize2
				   (Optimize.optimize {doDead = true, 
						       doProjection = SOME 50,
						       doCse = !do_cse, 
						       doUncurry = !do_uncurry},
				   nilmod) 
 	    val nilmod = transform typecheck_after_opt (typecheck,nilmod)
	    val nilmod = transform inline2 
		                   (Inline.inline {sizeThreshold = 50, 
						   occurThreshold = 5},
				    nilmod)
 	    val nilmod = transform typecheck_after_opt (typecheck,nilmod)
	    val nilmod = transform optimize3
				   (Optimize.optimize {doDead = true, 
						       doProjection = SOME 50,
						       doCse = !do_cse, 
						       doUncurry = !do_uncurry},
				   nilmod) 
 	    val nilmod = transform typecheck_after_opt (typecheck,nilmod)
	    val nilmod = transform inline3 
		                   (Inline.inline {sizeThreshold = 50, 
						   occurThreshold = 5},
				    nilmod)
 	    val nilmod = transform typecheck_after_opt (typecheck,nilmod)
            val nilmod = transform reify2 (Reify.reify_mod, nilmod)

 	    val nilmod = transform typecheck_after_opt2 (typecheck, nilmod)
 	    val nilmod = transform wtypecheck_after_opt2 (wtypecheck, nilmod)

	    val nilmod = transform cc (ToClosure.close_mod, nilmod)

	    val nilmod = transform measure_after_cc (do_measure typeof_count2 filename,nilmod)

(*	    val nilmod = transform reorder (Reorder.optimize, nilmod) *)
 	    val nilmod = transform typecheck_after_cc (typecheck, nilmod)
 	    val nilmod = transform wtypecheck_after_cc (wtypecheck, nilmod)

	in  nilmod
	end
    handle Stop nilmod => nilmod

    val il_to_nil = compile' false

end
