signature LINKRTL = 
sig
    structure Rtl : RTL
    structure Pprtl : PPRTL
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
    fun compile' debug s = 
	let val nilmodule : Linknil.Nil.module = (if debug then Linknil.test else Linknil.compile) s
	    val translate_params = {HeapProfile = NONE, do_write_list = true,
				    codeAlign = Rtl.QUAD, FullConditionalBranch = false,
				    elim_tail_call = true, recognize_constants = true}
	    val rtlmod = Tortl.translate translate_params nilmodule
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

    val test = compile' true
    val compile = compile' false

end
