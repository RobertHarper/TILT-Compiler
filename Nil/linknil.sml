(*$import LinkIl Annotation Nil NilUtil NilContext PpNil NilStatic ToNil Optimize Specialize Normalize Linearize ToClosure  LINKNIL Stats Alpha NilPrimUtilParam NilSubst NilError PrimUtil *)

structure Linknil :> LINKNIL  =
  struct
    val typecheck_before_opt = ref true
    val typecheck_after_opt = ref true
    val typecheck_after_cc = ref true


    val do_cleanup = ref false
    val do_opt = ref false
    val do_one_optimize = ref true
    val do_specialize = ref true
    val do_two_optimize = ref true
    val show_size = ref false
    val show_hil = ref false
    val show_phasesplit = ref false
    val show_renamed = ref false
    val show_opt = ref false
    val show_one_optimize = ref false
    val show_two_optimize = ref false
    val show_specialize = ref false
    val show_cc = ref false
    val show_before_rtl = ref false


    val error = fn s => Util.error "linknil.sml" s

    val select_carries_types = Stats.bool "select_carries_types"
    val profile = Stats.bool "nil_profile"
    val short_circuit = Stats.bool "subst_short_circuit"
    val hash = Stats.bool "subst_use_hash"
    val bnds_made_precise = Stats.bool "bnds_made_precise"
    val closure_print_free = Stats.bool "closure_print_free"
    val _ = (select_carries_types:=false;
	     profile := false;
	     short_circuit := true;
	     hash := false;
	     bnds_made_precise := true;
	     closure_print_free := false)

    structure Il = LinkIl.Il
    structure Stats = Stats
    structure Name = Name
    structure LinkIl = LinkIl
    structure Nil = Nil(structure ArgAnnotation = Annotation
			structure ArgPrim = LinkIl.Il.Prim)      

    structure PpNil = Ppnil(structure ArgNil = Nil
			    structure Prim = LinkIl.Il.Prim
			    structure Ppprim = LinkIl.Ppprim)

    structure Alpha = Alpha(structure ArgNil = Nil)

    structure NilPrimUtilParam = NilPrimUtilParam(structure Nil= Nil)
	
    structure NilPrimUtil = PrimUtil(structure Prim = LinkIl.Il.Prim
				     structure Ppprim = LinkIl.Ppprim
				     structure PrimUtilParam = NilPrimUtilParam)

    structure NilSubst = NilSubstFn(structure Nil = Nil
				    structure PpNil = PpNil)

    structure NilUtil = NilUtilFn(structure ArgNil = Nil
				  structure IlUtil = LinkIl.IlUtil
				  structure ArgPrim = LinkIl.Il.Prim
				  structure PrimUtil = NilPrimUtil
				  structure Alpha = Alpha
				  structure Subst = NilSubst
				  structure PpNil = PpNil)

    structure NilError = NilErrorFn(structure ArgNil = Nil
				    structure PpNil = PpNil)

    structure NilContext = NilContextFn(structure NilUtil = NilUtil
					structure ArgNil = Nil
					structure PpNil = PpNil
					structure Subst = NilSubst)

    structure Normalize = NormalizeFn(structure Nil = Nil
				      structure NilUtil = NilUtil
				      structure NilContext = NilContext
				      structure PpNil = PpNil
				      structure Subst = NilSubst)


    structure NilStatic = NilStaticFn(structure Annotation = Annotation
				      structure Prim = LinkIl.Il.Prim
				      structure ArgNil = Nil
				      structure PrimUtil = NilPrimUtil
				      structure NilUtil = NilUtil
				      structure NilContext = NilContext
				      structure PpNil = PpNil
				      structure Alpha = Alpha
				      structure NilError = NilError
				      structure Subst = NilSubst
				      structure Normalize = Normalize)

    val nilstatic_exp_valid = NilStatic.exp_valid

    structure Tonil = Tonil(structure Il = LinkIl.Il
			    structure Nilstatic = NilStatic
			    structure NilError = NilError
			    structure Nilprimutil = NilPrimUtil
			    structure Ilutil = LinkIl.IlUtil
                            structure Ilcontext = LinkIl.IlContext
                            structure IlStatic = LinkIl.IlStatic
			    structure Nilcontext = NilContext
			    structure Nilutil = NilUtil
			    structure Ppnil = PpNil
			    structure Ppil = LinkIl.Ppil
			    structure Nil = Nil
			    structure Subst = NilSubst)

    structure Optimize = Optimize(structure Nil = Nil
				  structure Subst = NilSubst			
				  structure NilUtil = NilUtil
				  structure NilContext = NilContext
				  structure NilStatic = NilStatic
				  structure Ppnil = PpNil)

    structure Specialize = Specialize(structure Nil = Nil
				    structure NilUtil = NilUtil
				    structure Ppnil = PpNil)

    structure Linearize = Linearize(structure Nil = Nil
				    structure NilUtil = NilUtil
				    structure Ppnil = PpNil)

(*
    structure BetaReduce = BetaReduce(structure Prim = LinkIl.Il.Prim
				      structure Nil = Nil
				      structure NilUtil = NilUtil
				      structure Ppnil = PpNil
				      structure IlUtil = LinkIl.IlUtil
				      structure Subst = NilSubst)

    structure Cleanup = Cleanup(structure Prim = LinkIl.Il.Prim
				structure Nil = Nil
				structure NilUtil = NilUtil
				structure Ppnil = PpNil
				structure IlUtil = LinkIl.IlUtil
				structure Subst = NilSubst)
*)

    structure ToClosure = ToClosure(structure Nil = Nil
				    structure NilContext = NilContext
				    structure NilStatic = NilStatic
				    structure Ppnil = PpNil
				    structure NilUtil = NilUtil
				    structure Subst = NilSubst)

	
(*
    structure NilEval = NilEvaluate(structure Nil = Nil
				    structure NilUtil = NilUtil
				    structure Ppnil = PpNil
				    structure PrimUtil = NilPrimUtil
				    structure Subst = NilSubst)


    structure DoOpts = DoOpts (structure Nil = Nil
			       structure NilPrimUtil = NilPrimUtil 
			       structure PpNil = PpNil
			       structure Linearize = Linearize
			       structure NilContext = NilContext
			       structure NilEval = NilEval
			       structure NilStatic = NilStatic
			       structure NilSubst = NilSubst
			       structure NilUtil = NilUtil
                          structure Linearize = Linearize)

*)

     val phasesplit = Stats.timer("Phase-splitting",Tonil.phasesplit)



    fun showmod (showmod,showsize) str (filename,nilmod) = 
	(print "\n\n=======================================\n";
	 print str; print " complete: "; print filename; print "\n";
	 if showsize
	     then (print "  size = ";
		   print (Int.toString (NilUtil.module_size nilmod));
		   print "\n")
	 else ();
	 if showmod
	    then (PpNil.pp_module nilmod;
		  print "\n")
	 else ())

	    
    fun pcompile' debug (filename,(ctxt,sbnd_entries)) =
	let
	    open Nil LinkIl.Il LinkIl.IlContext Name
	    val D = NilContext.empty

	    val nilmod = (Stats.timer("Phase-splitting",Tonil.phasesplit)) (ctxt,sbnd_entries)
	    val _ = showmod debug "Phase-split" (filename,nilmod)
	in
	    nilmod
	end

    fun compile' debug (filename,(ctxt,sbnd_entries)) =
	let
	    open Nil LinkIl.Il LinkIl.IlContext Name
	    val D = NilContext.empty

	    val _ = if (!show_hil)
			then 
			    let val sbnds = List.mapPartial #1 sbnd_entries
			    in  LinkIl.Ppil.pp_sbnds sbnds
			    end
		    else ()

	    val nilmod = (Stats.timer("Phase-splitting",Tonil.phasesplit)) (ctxt,sbnd_entries)
	    val _ = showmod (!show_phasesplit orelse debug,!show_size) "Phase-split" (filename, nilmod)

(*
	    val nilmod = if (!do_cleanup)
			     then (Stats.timer("Cleanup",Cleanup.cleanModule)) nilmod
			 else nilmod
	    val _ = if (!do_cleanup)
			then showmod (debug,!show_size) "Cleanup" (filename, nilmod)
		    else ()
*)

	    val nilmod = (Stats.timer("Linearization",Linearize.linearize_mod)) nilmod
	    val _ = showmod (!show_renamed orelse debug,!show_size) "Renaming" (filename, nilmod)


	    val nilmod = if (!do_one_optimize)
			     then (Stats.timer("OneOptimize",Optimize.optimize)) nilmod
			 else nilmod
	    val _ = if (!do_one_optimize)
			then showmod (!show_one_optimize orelse debug,!show_size) 
			        "OneOptimize" (filename, nilmod)
		    else ()

	    val nilmod = (Stats.timer("Linearization__temp",Linearize.linearize_mod)) nilmod


	    val nilmod = if (!do_specialize)
			     then (Stats.timer("Specialize",Specialize.optimize)) nilmod
			 else nilmod
	    val _ = if (!do_specialize)
			then showmod (!show_specialize orelse debug,!show_size) 
			        "Specialize" (filename, nilmod)
		    else ()


	    val nilmod = if (!do_two_optimize)
			     then (Stats.timer("TwoOptimize",Optimize.optimize)) nilmod
			 else nilmod
	    val _ = if (!do_two_optimize)
			then showmod (!show_two_optimize orelse debug,!show_size) 
			        "TwoOptimize" (filename, nilmod)
		    else ()


 	    val nilmod' = 
	      if (!typecheck_before_opt) then
		(Stats.timer("Nil typechecking - pre opt",NilStatic.module_valid)) (D,nilmod)
	      else
		nilmod
	    val _ = 
	      if (!typecheck_before_opt) then 
		  showmod (debug,!show_size) "Pre-opt typecheck" (filename, nilmod')
	      else ()

(*
	    val nilmod = if (!do_opt) 
			    then (Stats.timer("Nil Optimization", 
					      DoOpts.do_opts debug)) nilmod else nilmod
	    val _ = if (!do_opt)
			then showmod (!show_opt orelse debug,!show_size) 
			    "Optimization" (filename,nilmod)
		    else ()
*)

 	    val nilmod' = 
	      if (!typecheck_after_opt andalso !do_opt) then
		(Stats.timer("Nil typechecking",NilStatic.module_valid)) (D,nilmod)
	      else
		nilmod
	    val _ = 
	      if (!typecheck_after_opt andalso !do_opt) then 
		  showmod (debug,!show_size) "Post-opt typecheck" (filename, nilmod')
	      else ()
	
(*
	    val _ = print "starting beta-reduction\n"	  
	    val nilmod = (Stats.timer("Beta-reduction",BetaReduce.reduceModule)) nilmod
	    val _ = showmod debug  "Beta-reduction" nilmod
*)

	    val nilmod = (Stats.timer("Closure-conversion",ToClosure.close_mod)) nilmod
	    val _ = showmod (debug orelse !show_cc,!show_size) "Closure-conversion" (filename, nilmod)

	    val nilmod = (Stats.timer("Linearization2",Linearize.linearize_mod)) nilmod
	    val _ = showmod (debug orelse !show_before_rtl,!show_size) "Renaming2" (filename, nilmod)

 	    val nilmod' = 
	      if (!typecheck_after_cc) then
		(Stats.timer("Nil typechecking (post cc)",NilStatic.module_valid)) (D,nilmod)
	      else
		nilmod
	    val _ = 
	      if (!typecheck_after_cc) then 
		  showmod (debug,!show_size) "Post-cc Typecheck" (filename, nilmod')
	      else ()
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

fun doit() = Linknil.compile_prelude(false, "test");
