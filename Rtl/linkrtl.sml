signature LINKRTL = 
sig
    structure Tortl : TORTL
    structure Rtl : RTL
    structure Pprtl : PPRTL

    val compile_prelude : bool * string -> Rtl.module
    val compile : string -> Rtl.module
    val test : string -> Rtl.module
end

structure Linkrtl : LINKRTL =
struct
   
    fun in_imm_range x =  x>=0 andalso x<256 
    fun in_ea_disp_range x = x >= ~32768 andalso x<32768

    structure Rtl = Rtl(val in_imm_range = in_imm_range
			val in_ea_disp_range = in_ea_disp_range)

    structure Rtltags = Rtltags(structure Rtl = Rtl)

    structure Pprtl = Pprtl(structure Rtl = Rtl
			    structure Rtltags = Rtltags)
    
    structure Tortl = Tortl(structure Nil = Linknil.Nil
			    structure NilContext = Linknil.NilContext
			    structure NilStatic = Linknil.NilStatic
			    structure Rtl = Rtl
			    structure Pprtl = Pprtl
			    structure Rtltags = Rtltags
			    structure NilUtil = Linknil.NilUtil
			    structure Ppnil = Linknil.PpNil)

    structure Rtlopt = MakeRtlopt(structure Rtl = Rtl
				  structure Pprtl = Pprtl)

    structure Registerset = MakeRegisterSet(structure Rtl = Rtl
					    structure Pprtl = Pprtl)

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
    fun compile' debug nilmodule = 
	let val translate_params = {HeapProfile = NONE, do_write_list = true,
				    codeAlign = Rtl.QUAD, FullConditionalBranch = false,
				    elim_tail_call = true, recognize_constants = true}
	    val rtlmod = Tortl.translate translate_params nilmodule
(*	    val rtlmod = Rtlopt.opt translate_params rtlmod *)
	    val rtlmod = Rtlopt.GCmerge rtlmod
	    val _ = if debug
			then (print "============================================\n\n";
			      print "RTL code:\n";
			      Pprtl.pp_Module rtlmod;
			      print "\n\n";
			      Rtlinterp.RTL_interp([("main",rtlmod)],([],[]),false);
			      ())
		    else print "Translation to RTL complete\n"
	in  rtlmod
	end

    fun compile filename = 
	let val nilmodule : Linknil.Nil.module = Linknil.compile filename
	in  compile' false nilmodule
	end
    fun test filename = 
	let val nilmodule : Linknil.Nil.module = Linknil.test filename
	in  compile' true nilmodule
	end

    val cached_prelude = ref (NONE : Rtl.module option)
    fun compile_prelude (use_cache,filename) = 
	case (use_cache, !cached_prelude) of
		(true, SOME m) => m
	      | _ => let val nilmod = Linknil.compile_prelude(use_cache,filename)
			 val m = compile' false nilmod
			 val _ = cached_prelude := SOME m
		     in  m
		     end

end
