(*$import Prelude TopLevel Stats Nil LINKRTL Linknil TortlBase TortlSum Tortl Util TilWord32 Rtl Rtltags Pprtl *)

structure Linkrtl :> LINKRTL =
struct

    val show_rtl = Stats.ff("showRtl")
    val error = fn x => Util.error "Linkrtl." x

    structure Rtltags = Rtltags
    structure Pprtl = Pprtl
    structure TortlBase = TortlBase
    structure TortlSum = TortlSum
    structure Tortl = Tortl

    val ptrWriteBarrier = Rtltags.ptrWriteBarrier
    val fullWriteBarrier = Rtltags.fullWriteBarrier
    val mirrorGlobal = Rtltags.mirrorGlobal
    val mirrorPtrArray = Rtltags.mirrorPtrArray

(*
    structure TortlVararg = TortlVararg
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
	let val _ = print "===== Translating to RTL             =====\n"
	    val rtlmod = Tortl.translate(unitname, nilmodule)
	    val _ = if debug orelse !show_rtl
			then (print "RTL code:\n";
			      Pprtl.pp_Module rtlmod;
			      print "\n\n")
		    else ()
	in  rtlmod
	end

    val compile' = Stats.timer("Translation to RTL",compile')
    fun nil_to_rtl (unitname : string, nilmod : Nil.module) : Rtl.module = compile'(false,unitname,nilmod)

end
