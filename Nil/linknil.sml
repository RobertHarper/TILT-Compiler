(*$import LinkIl Annotation Nil NilUtil NilContext Ppnil ToNil Optimize Specialize Normalize Linearize ToClosure  LINKNIL Stats Alpha NilPrimUtilParam NilSubst NilError PrimUtil Hoist Reify NilStatic Inline Flatten Reduce PpnilHtml *)


structure Linknil (* :> LINKNIL  *) =
  struct
    val typecheck_before_opt = ref false
    val typecheck_after_opt = ref false
    val typecheck_after_cc = ref false


    val do_cleanup = ref false
    val do_opt = ref false
    val do_one_optimize = ref true
    val do_vararg = Stats.ff("do_vararg")
    val do_specialize = ref true
    val do_two_optimize = ref true
    val do_hoist = Stats.tt("doHoist")
    val do_reify = Stats.tt("doReify")
    val do_cse = Stats.tt("doCSE")
    val do_uncurry = Stats.ff("doUncurry")
    val do_reduce = Stats.tt("doReduce")
    val do_flatten = Stats.tt("doFlatten")
    val do_inline = Stats.tt ("doInline")

    val show_size = ref false
    val show_hil = ref false
    val show_phasesplit = Stats.ff("showPhaseSplit")
    val show_renamed = Stats.ff("showRenaming1")
    val show_opt = ref false
    val show_one_optimize = ref false
    val show_vararg = ref false
    val show_two_optimize = Stats.ff("showOptimize2")
    val show_hoist = Stats.ff("showHoist")
    val show_reify = Stats.ff("showReify")
    val show_cse = Stats.ff("showCSE")
    val show_specialize = Stats.ff("showSpecialize")
    val show_cc = ref false
    val show_before_rtl = ref false
    val show_typecheck = ref false
    val show_reduce = Stats.ff("showReduce")
    val show_flatten = Stats.ff("showFlatten")
    val show_inline = Stats.ff("showInline")
    val show_html = Stats.tt("showHTML")

(*    val type_check_before_opt = Stats.ff("TypecheckBeforeOpt")
    val type_check_after_opt = Stats.ff("TypecheckAfterOpt")
    val type_check_after_cc = Stats.ff("TypecheckAfterCC")
*)
    val number_flatten = 6
    val _ = Stats.int("number_flatten") := number_flatten
    val error = fn s => Util.error "linknil.sml" s

    val debug = Stats.ff "nil_debug"
    val profile = Stats.ff "nil_profile"
    val short_circuit = Stats.tt "subst_short_circuit"
    val hash = Stats.ff "subst_use_hash"
    val bnds_made_precise = Stats.tt "bnds_made_precise"
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
    structure NilPrimUtilParam = NilPrimUtilParam
    structure NilPrimUtil = PrimUtil(structure PrimUtilParam = NilPrimUtilParam)
    structure Flatten = Flatten(structure PrimUtil = NilPrimUtil)
    structure Reduce = Reduce 

(*    structure Flatten = Flatten *)

(*
    structure NilEval = NilEvaluate(structure Nil = Nil
				    structure NilUtil = NilUtil
				    structure Ppnil = Ppnil
				    structure PrimUtil = NilPrimUtil
				    structure Subst = NilSubst)


    structure DoOpts = DoOpts (structure Nil = Nil
			       structure NilPrimUtil = NilPrimUtil 
			       structure Ppnil = Ppnil
			       structure Linearize = Linearize
			       structure NilContext = NilContext
			       structure NilEval = NilEval
			       structure NilStatic = NilStatic
			       structure NilSubst = NilSubst
			       structure NilUtil = NilUtil
                          structure Linearize = Linearize)

*)

     val phasesplit = Stats.timer("Phase-splitting",Tonil.phasesplit)


    fun pass (showphase,phasename,phase,filename,nilmod) = 
	let val _ = print "\n\n=======================================\n"
	    val nilmod = Stats.timer(phasename,phase) nilmod
	    val _ = (print phasename; print " complete: "; print filename; print "\n")
	    val _ = if !show_size
		      then
			  (print "  size = ";
			   print (Int.toString (NilUtil.module_size nilmod));
			   print "\n")
		    else ()
	    val _ = if !showphase
			then
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

    fun transform (ref false,showphase,phasename,phase,filename,nilmod) = 
	(print "\n===== SKIPPING "; print phasename; print "  =======\n"; nilmod)
      | transform (ref true,showphase,phasename,phase,filename,nilmod) = 
	pass (showphase,phasename,phase,filename,nilmod)

    fun check (doit,showphase,phasename,phase,filename,nilmod) = 
      transform (doit,showphase,phasename,(fn nilmod => (phase nilmod;nilmod)),filename,nilmod)

    fun pcompile' debug (filename,(ctxt,sbnd_entries)) =
	let
	    open Nil Il LinkIl.IlContext Name
	    val D = NilContext.empty

	in  pass(show_phasesplit,
		 "Phase-split", Tonil.phasesplit,
		 filename, (ctxt,sbnd_entries))
	end

    fun compile' debug (filename,(ctxt,sbnd_entries)) =
	let
	    open Nil LinkIl.IlContext Name
	    val D = NilContext.empty()

	    val _ = if (!show_hil)
			then 
			    let val sbnds = List.mapPartial #1 sbnd_entries
			    in  LinkIl.Ppil.pp_sbnds sbnds
			    end
		    else ()

	    val nilmod = pass(show_phasesplit,
			       "Phase-split", Tonil.phasesplit,
			       filename, (ctxt,sbnd_entries))


	    val nilmod = transform(ref true, show_renamed,
				 "Renaming1",Linearize.linearize_mod,
				 filename, nilmod)

 	    val nilmod = check (typecheck_before_opt,show_typecheck,
				    "Nil_typecheck_pre-opt",
				    Util.curry2 NilStatic.module_valid (NilContext.empty ()),
				    filename, nilmod)

	    val nilmod = transform(do_reduce, show_reduce,
				   "Reduce", Reduce.doModule, 
				   filename, nilmod)

	    val nilmod = transform(do_flatten, show_flatten,
				   "Flatten", Flatten.doModule, 
				   filename, nilmod)

	    fun inline_domod m = 
                   let val (count, result) = Inline.optimize m
                   in print "Small functions inlined: ";
                      print (Int.toString count);
                      print "\n";
                      result
                   end
	    val nilmod = transform(do_inline, show_inline,
				   "Inline", 
				   inline_domod, filename, nilmod)

	    val nilmod = transform(do_one_optimize, show_one_optimize,
				 "Optimize1", 
				 Optimize.optimize
				   {lift_array = true,
				    dead = true, projection = true, 
				    cse = false, uncurry = false},
				 filename, nilmod)

	    val _ = if (!do_vararg) then error "no vararg" else ()
(*
	    val nilmod = if (!do_vararg)
			     then (Stats.timer("Vararg",Vararg.optimize)) nilmod
			 else nilmod
	    val _ = if (!do_vararg)
			then transform (!show_vararg,!show_size) 
			    "Vararg" (filename, nilmod)
		    else ()
*)

	    val nilmod = transform(do_specialize, show_specialize,
				 "Specialize",
				 Specialize.optimize,
				 filename, nilmod)

	    val nilmod = transform(do_hoist, show_hoist,
				 "Hoist", 
				 Hoist.optimize,
				 filename, nilmod)

            val nilmod = transform(do_reify, show_reify,
                                   "Reification",
                                   Reify.reify_mod,
                                   filename, nilmod)


	    val nilmod = transform(do_two_optimize, show_two_optimize,
				 "Optimize2", 
				 Optimize.optimize
				   {lift_array = false,
				    dead = true, projection = true, 
				    cse = !do_cse, uncurry = !do_uncurry},
				 filename, nilmod) 



 	    val nilmod = check (typecheck_after_opt,show_typecheck,
				    "Nil typechecking - post opt",
				    Util.curry2 NilStatic.module_valid (NilContext.empty ()),
				    filename, nilmod)
	
(*
	    val _ = print "starting beta-reduction\n"	  
	    val nilmod = (Stats.timer("Beta-reduction",BetaReduce.reduceModule)) nilmod
	    val _ = transform debug  "Beta-reduction" nilmod
*)

(*
	    val nilmod = transform(ref true, show_before_rtl,
				 "Renaming2",Linearize.linearize_mod,
				 filename, nilmod)
*)
(*
            val nilmod = transform(do_reify, show_reify,
                                   "Reification2",
                                   Reify.reify_mod,
                                   filename, nilmod)
*)

	    val nilmod = transform(ref true, show_cc,
				 "Closure-conversion", 
				 ToClosure.close_mod,
				 filename, nilmod)


 	    val nilmod = check (typecheck_after_cc,show_typecheck,
				    "Nil typechecking - post cc",
				    Util.curry2 NilStatic.module_valid (NilContext.empty ()),
				    filename, nilmod)

	in  nilmod
	end

    fun linkil_tests [] = NONE
      | linkil_tests [one] = SOME [valOf (LinkIl.test one)]
      | linkil_tests _ = error "linkil_tests only defined for one file"

    fun meta_compiles debug filenames = 
	let val mods = valOf ((if debug then linkil_tests else LinkIl.compiles) filenames)
	in  map (compile' debug) (Listops.zip filenames mods)
	end

    fun meta_pcompiles debug filenames = 
	let val mods = valOf ((if debug then linkil_tests else LinkIl.compiles) filenames)
	in  map (pcompile' (debug,!show_size)) (Listops.zip filenames mods)
	end

    fun pcompile filename = hd(meta_pcompiles false [filename])
    fun pcompiles filenames = hd(meta_pcompiles false filenames)
    fun ptest filename = hd(meta_pcompiles true [filename])

    fun compiles filenames = meta_compiles false filenames
    fun compile filename = hd(meta_compiles false [filename])
    fun tests filenames = meta_compiles true filenames
    fun test filename = hd(meta_compiles true [filename])
    fun il_to_nil ilmod = compile' false ilmod

    val cached_prelude = ref (NONE : Nil.module option)
    fun compile_prelude (use_cache,filename) = 
	case (use_cache, !cached_prelude) of
		(true, SOME m) => m
	      | _ => let val (ctxt,sbnd_entries) =
				LinkIl.compile_prelude(use_cache,filename)
			 val m = compile' false (filename,(ctxt,sbnd_entries))
			 val _ = cached_prelude := SOME m
		     in  m
		     end
end













