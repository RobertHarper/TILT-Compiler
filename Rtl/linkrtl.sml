(*$import LINKRTL Linknil TortlBase TortlSum Tortl Util TilWord32 Rtl Rtltags Pprtl TortlVararg *)

structure Linkrtl :> LINKRTL =
struct

    val show_rtl = ref false
    val error = fn x => Util.error "Linkrtl." x

    structure Rtltags = Rtltags
    structure Pprtl = Pprtl
    structure TortlBase = TortlBase
    structure TortlSum = TortlSum
    structure TortlVararg = TortlVararg
    structure Tortl = Tortl


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
    fun nil_to_rtl (unitname : string, nilmod : Nil.module) : Rtl.module = compile'(false,unitname,nilmod)

end
