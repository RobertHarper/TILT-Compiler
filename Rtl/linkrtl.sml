(*$import Prelude TopLevel Stats Nil LINKRTL Linknil TortlBase TortlSum Tortl Util TilWord32 Rtl Rtltags Pprtl *)

structure Linkrtl :> LINKRTL =
struct

  val ptrWriteBarrier = TortlArray.ptrWriteBarrier
  val fullWriteBarrier = TortlArray.fullWriteBarrier
  val mirrorPtrArray = TortlArray.mirrorPtrArray
  val mirrorGlobal = TortlBase.mirrorGlobal
    
  val show_rtl = Stats.ff("showRtl")
  val error = fn x => Util.error "Linkrtl." x
    
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
