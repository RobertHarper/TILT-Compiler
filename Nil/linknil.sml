(*$import LinkIl Annotation Nil NilUtil NilContext Ppnil ToNil Optimize Specialize Normalize Linearize ToClosure  LINKNIL Stats Alpha NilSubst NilError PrimUtil Hoist Reify NilStatic Inline Flatten Reduce PpnilHtml Measure *)


structure Linknil :> LINKNIL  =
  struct

    val do_cleanup = ref false
    val do_opt = ref false
    val do_one_optimize = Stats.tt("doOptimize1")
    val do_vararg = Stats.ff("do_vararg")
    val do_specialize = Stats.tt("doSpecialize")
    val do_two_optimize = Stats.tt("doOptimize2")
    val do_rename = Stats.tt("doRename")
    val do_hoist = Stats.tt("doHoist")
    val do_reify = Stats.tt("doReify")
    val do_cse = Stats.tt("doCSE")
    val do_uncurry = Stats.ff("doUncurry")
    val do_reduce = Stats.tt("doReduce")
    val do_flatten = Stats.tt("doFlatten")
    val do_inline = Stats.tt ("doInline")

    val show_size = ref false
    val show_phasesplit = Stats.ff("showPhaseSplit")
    val show_renamed = Stats.ff("showRenaming1")
    val show_opt = ref false
    val show_one_optimize = Stats.ff("showOptimize1")
    val show_vararg = ref false
    val show_two_optimize = Stats.ff("showOptimize2")
    val show_hoist = Stats.ff("showHoist")
    val show_reify = Stats.ff("showReify")
    val show_reify2 = Stats.ff("showReify2")
    val show_cse = Stats.ff("showCSE")
    val show_specialize = Stats.ff("showSpecialize")
    val show_cc = Stats.ff("showCC")
    val show_before_rtl = Stats.ff("showBeforeRtl")
    val show_reduce = Stats.ff("showReduce")
    val show_reduce2 = Stats.ff("showReduce2")
    val show_flatten = Stats.ff("showFlatten")
    val show_inline = Stats.ff("showInline")
    val show_inline2 = Stats.ff("showInline2")
    val show_html = Stats.tt("showHTML")

    val do_typecheck_after_phasesplit = Stats.ff("TypecheckAfterPhasesplit")
    val do_typecheck_before_opt = Stats.ff("TypecheckBeforeOpt")
    val do_typecheck_after_opt = Stats.ff("TypecheckAfterOpt")
    val do_typecheck_after_opt2 = Stats.ff("TypecheckAfterOpt2")
    val do_typecheck_after_cc = Stats.ff("TypecheckAfterCC")
    val do_measure = Stats.ff("NilMeasure")
    val typecheck_after_phasesplit = (do_typecheck_after_phasesplit, ref false, "Nil_typecheck_post_phasesplit")
    val typecheck_before_opt = (do_typecheck_before_opt, ref false, "Nil_typecheck_pre-opt")
    val typecheck_after_opt = (do_typecheck_after_opt, ref false, "Nil_typecheck_post-opt")
    val typecheck_after_opt2 = (do_typecheck_after_opt2, ref false,"Nil_typecheck_post-opt2")
    val typecheck_after_cc = (do_typecheck_after_cc, ref false, "Nil_typecheck_postcc")
    val measure = (do_measure,ref false,"Nil_measure")
    val phasesplit = (show_phasesplit, "Phase-split")
    val rename1 = (do_rename, show_renamed, "Renaming1")
    val reduce1 = (do_reduce, show_reduce, "Reduce")
    val flatten = (do_flatten, show_flatten, "Flatten")
    val inline = (do_inline, show_inline, "Inline")
    val reify1 = (do_reify, show_reify, "Reification1")
    val optimize1 = (do_one_optimize, show_one_optimize, "Optimize1")
    val specialize = (do_specialize, show_specialize, "Specialize")
    val hoist = (do_hoist, show_hoist, "Hoist")
    val optimize2 = (do_two_optimize, show_two_optimize, "Optimize2")
    val reduce2 = (do_reduce, show_reduce2, "Reduce2")
    val inline2 = (do_inline, show_inline2, "Inline2")
    val reify2 = (do_reify, show_reify2, "Reification2")
    val cc = (ref true, show_cc, "Closure-conversion")

    val number_flatten = 6
    val _ = Stats.int("number_flatten") := number_flatten
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

(*
    structure Vararg = Vararg(val number_flatten = number_flatten
			      structure Subst = NilSubst			
			      structure NilUtil = NilUtil
			      structure NilContext = NilContext
			      structure Normalize = Normalize
			      structure Ppnil = Ppnil)

*)

    structure Tonil = Tonil
    structure Optimize = Optimize
    structure Specialize = Specialize
    structure Linearize = Linearize
    structure ToClosure = ToClosure
    structure Flatten = Flatten(structure PrimUtil = NilPrimUtil)
    structure Reduce = Reduce 


    fun pass filename (showphase,phasename) (transformer,obj) =
	let val _ = print "\n\n============================================\n"
	    val _ = (print "Starting "; print phasename; print ": "; print filename; print "\n")
	    val nilmod = Stats.timer(phasename,transformer) obj
	    val _ = (print "Completed "; print phasename; print ": "; print filename; print "\n")
	    val _ = if !show_size
		      then
			  (print "  size = ";
			   print (Int.toString (NilUtil.module_size nilmod));
			   print "\n")
		    else ()
	    val _ = if !showphase then
		      (if !show_html then
			   PpnilHtml.pp_module 
		       else
			   Ppnil.pp_module)   
			   {module = nilmod,
			    header = phasename,
			    name = filename,
			    pass = phasename}
		    else ()
	in  nilmod
	end

    fun transform filename (ref false,_,phasename) (_,nilmod) = 
	(print "\n=======================================\n";
	 print "Skipping "; print phasename; print " on "; print filename;
	 nilmod)
      | transform filename (ref true,showphase,phasename) arg =
	pass filename (showphase,phasename) arg


    fun inline_domod m = 
	let val (count, result) = Inline.optimize m
	in print "  Small functions inlined: ";
	    print (Int.toString count);
	    print "\n";
	    result
	end
    fun typecheck nilmod = (NilStatic.module_valid (NilContext.empty (), nilmod); nilmod)

    exception Stop of Nil.module

    (* (1) Rename must precede everything.
       (2) Reify must occur before uncurrying which is in optimize2.
       (3) Flatten should occur before uncurrying which is in optimize2
              since it works on single-argument function.
       (4) Reduce2 follows optimize2 to kill dead code generated by optimize2.
       (5) Reify2 is needed because some optimizations create TraceUnknowns.
    *)
    fun compile' debug (filename,(ctxt,_,sbnd_entries) : Il.module) =
	let
	    val pass = pass filename
	    val transform = transform filename

	    open Nil LinkIl.IlContext Name
	    val D = NilContext.empty()

	    val nilmod = pass phasesplit (Tonil.phasesplit, (ctxt,sbnd_entries))

(*	    val nilmod = NilRename.renameMod nilmod *)
	    val nilmod = transform typecheck_after_phasesplit (typecheck, nilmod)
	    val _ = if !(Stats.bool("UptoPhasesplit"))
			then raise (Stop nilmod)
		    else ()

 	    val nilmod = transform measure (Measure.measureMod, nilmod)

	    val nilmod = transform rename1 (Linearize.linearize_mod, nilmod)

 	    val nilmod = transform measure (Measure.measureMod, nilmod)

 	    val nilmod = transform typecheck_before_opt (typecheck, nilmod)

	    val nilmod = transform optimize1 
				   (Optimize.optimize {lift_array = true,
						       dead = true, projection = true, 
						       cse = false, uncurry = false},
				    nilmod)
	    val nilmod = transform reduce1 (Reduce.doModule, nilmod)
	    val nilmod = transform flatten (Flatten.doModule, nilmod)
	    val nilmod = transform inline (inline_domod, nilmod)
            val nilmod = transform reify1 (Reify.reify_mod, nilmod)


 	    val nilmod = transform typecheck_after_opt (typecheck,nilmod)

	    val nilmod = transform specialize (Specialize.optimize, nilmod)
	    val nilmod = transform hoist (Hoist.optimize, nilmod)
(* 	    val nilmod = transform (ref true,ref false,"Before Optimize2") (typecheck, nilmod)*)
	    val nilmod = transform optimize2
				   (Optimize.optimize {lift_array = false,
						      dead = true, projection = true, 
						      cse = !do_cse, uncurry = !do_uncurry},
				   nilmod) 

(* 	    val nilmod = transform (ref true,ref false,"Before Reduce2") (typecheck, nilmod)*)
	    val nilmod = transform reduce2 (Reduce.doModule, nilmod)

(* 	    val nilmod = transform (ref true,ref false,"Before Inline") (typecheck, nilmod)*)
	    val nilmod = transform inline2 (inline_domod, nilmod)

(* 	    val nilmod = transform (ref true,ref false,"Before Reify") (typecheck, nilmod)*)
            val nilmod = transform reify2 (Reify.reify_mod, nilmod)

 	    val nilmod = transform typecheck_after_opt2 (typecheck, nilmod)
	    val nilmod = transform cc (ToClosure.close_mod, nilmod)

 	    val nilmod = transform measure (Measure.measureMod, nilmod)

 	    val nilmod = transform typecheck_after_cc (typecheck, nilmod)


	in  nilmod
	end
    handle Stop nilmod => nilmod

    val il_to_nil = compile' false

end
