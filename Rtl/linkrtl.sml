structure Linkrtl =
struct
   
    fun in_imm_range x =  x>=0 andalso x<256 
    fun in_ea_disp_range x = x >= ~32768 andalso x<32768

    structure Rtl = Rtl(val in_imm_range = in_imm_range
			val in_ea_disp_range = in_ea_disp_range)

    structure Rtltags = Rtltags(structure Rtl = Rtl)

    structure Pprtl = Pprtl(structure Rtl = Rtl
			    structure Rtltags = Rtltags)
    
    structure Tortl = Tortl(structure Nil = Linknil.Nil
			    structure Rtl = Rtl
			    structure Pprtl = Pprtl
			    structure Rtltags = Rtltags
			    structure NilUtil = Linknil.NilUtil
			    structure Ppnil = Linknil.Ppnil)

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
    fun test s = 
	let val nilmodule : Linknil.Nil.module = Linknil.test s
	    val translate_params = {HeapProfile = NONE, do_write_list = true,
				    codeAlign = Rtl.QUAD, FullConditionalBranch = false,
				    elim_tail_call = true, recognize_constants = true}
	    val rtlmod = Tortl.translate translate_params nilmodule
	    val _ = (print "============================================\n\n";
		     print "RTL code:\n";
		     Pprtl.pp_Module rtlmod;
		     print "\n\n")
	    val _ =  Rtlinterp.RTL_interp([("main",rtlmod)],([],[]),false)
	in  rtlmod
	end

end
