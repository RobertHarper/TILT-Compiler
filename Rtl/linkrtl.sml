(*$import LINKRTL Linknil TortlBase TortlSum Tortl Util TilWord32 Rtl RtlTags Pprtl TortlVararg *)

structure Linkrtl :> LINKRTL =
struct

    val show_rtl = ref false
    val error = fn x => Util.error "Linkrtl." x

    structure Rtltags = Rtltags()

    structure Pprtl = Pprtl(structure Rtltags = Rtltags)
    
    structure TortlBase = TortlBase(
			    structure NilContext = Linknil.NilContext
			    structure Normalize = Linknil.Normalize
			    structure Pprtl = Pprtl
			    structure Rtltags = Rtltags
			    structure NilUtil = Linknil.NilUtil
			    structure Ppnil = Linknil.PpNil)

    structure TortlSum = TortlSum(
			    structure NilContext = Linknil.NilContext
			    structure Pprtl = Pprtl
			    structure TortlBase = TortlBase
			    structure Rtltags = Rtltags
			    structure NilUtil = Linknil.NilUtil
			    structure Ppnil = Linknil.PpNil)

    structure TortlVararg = TortlVararg(val number_flatten = 6
					structure NilContext = Linknil.NilContext
					structure Pprtl = Pprtl
					structure TortlBase = TortlBase
					structure Rtltags = Rtltags
					structure NilUtil = Linknil.NilUtil
					structure Ppnil = Linknil.PpNil)

    structure Tortl = Tortl(
			    structure NilContext = Linknil.NilContext
			    structure Pprtl = Pprtl
			    structure TortlBase = TortlBase
			    structure TortlSum = TortlSum
			    structure TortlVararg = TortlVararg
			    structure Rtltags = Rtltags
			    structure NilUtil = Linknil.NilUtil
			    structure Ppnil = Linknil.PpNil)

(*
    structure Rtlopt = MakeRtlopt(structure Pprtl = Pprtl)

    structure Registerset = MakeRegisterSet(structure Pprtl = Pprtl)

    structure Heap = MakeHeap(structure Rtl = Rtl
			      structure Registerset = Registerset
			      val hs = 10000)

    structure Operations = MakeOperations(structure Rtl = Rtl)

    structure Rtlinterp = Rtlinterp(structure Rtl = Rtl
				    structure Pprtl = Pprtl
				    structure Rtltags = Rtltags
				    structure Heap = Heap
				    structure Registerset = Registerset
				    structure Operations = Operations)
*)
    fun compile' (debug,unitname,nilmodule) = 
	let val translate_params = {HeapProfile = NONE, do_write_list = true,
				    codeAlign = Rtl.QUAD, FullConditionalBranch = false,
				    elim_tail_call = true, recognize_constants = true}
	    val rtlmod = Tortl.translate unitname translate_params nilmodule
(*	    val rtlmod = Rtlopt.opt translate_params rtlmod *)

	    val _ = print "Not doing Rtlopt.GCmerge\n"
(*
	    val _ = print "Starting Rtlopt.GCmerge\n"
	    val rtlmod = Rtlopt.GCmerge rtlmod
	    val _ = print "Finished Rtlopt.GCmerge\n"
*)
	    val _ = if debug orelse !show_rtl
			then (print "============================================\n\n";
			      print "RTL code:\n";
			      Pprtl.pp_Module rtlmod;
			      print "\n\n";
(*			      Rtlinterp.RTL_interp([("main",rtlmod)],([],[]),false); *)
			      ())
		    else print "Translation to RTL complete\n"
	in  rtlmod
	end

    val compile' = Stats.timer("Translation to RTL",compile')

    fun metacompiles debug filenames = 
	let val nilmodules : Nil.module list = 
	    if debug then Linknil.tests filenames else Linknil.compiles filenames 
	    val filenames_with_nilmodules = Listops.zip filenames nilmodules
		handle _ => error "metacompiles"
	in  map (fn (name,nmod) => compile'(debug,name,nmod)) filenames_with_nilmodules
	end

    fun compiles filenames = metacompiles false filenames
    fun compile filename = hd(metacompiles false [filename])
    fun tests filenames = metacompiles true filenames
    fun test filename = hd(metacompiles true [filename])
    fun nil_to_rtl (nilmod : Nil.module, unitname: string) : Rtl.module = compile'(false,unitname,nilmod)

    val cached_prelude = ref (NONE : Rtl.module option)
    fun compile_prelude (use_cache,filename) = 
	case (use_cache, !cached_prelude) of
		(true, SOME m) => m
	      | _ => let val nilmod = Linknil.compile_prelude(use_cache,filename)
			 val m = compile' (false,filename,nilmod)
			 val _ = cached_prelude := SOME m
		     in  m
		     end

end
