(*$import Stats Nil LINKRTL Linknil TortlBase TortlSum Tortl Util TilWord32 Rtl Rtltags Pprtl TortlArray *)

structure Linkrtl :> LINKRTL =
struct

  val ptrWriteBarrier = TortlArray.ptrWriteBarrier
  val fullWriteBarrier = TortlArray.fullWriteBarrier
  val mirrorPtrArray = TortlArray.mirrorPtrArray
  val mirrorGlobal = TortlBase.mirrorGlobal
    
  val show_rtl = Stats.ff("showRtl")
  val error = fn x => Util.error "Linkrtl." x

  type unitmap = Tortl.unitmap
  val empty = Tortl.empty
  val extend = Tortl.extend
  val restrict = Tortl.restrict
      
  fun compile' (debug,unitname,unitmap,nilmodule) = 
    let val _ = print "===== Translating to RTL             =====\n"
      val rtlmod = Tortl.translate(unitname, unitmap, nilmodule)
      val _ = if debug orelse !show_rtl
		then (print "RTL code:\n";
		      Pprtl.pp_Module rtlmod;
		      print "\n\n")
	      else ()
    in  rtlmod
    end
  
  val compile' = Stats.timer("Translation to RTL",compile')
  fun nil_to_rtl (unitname : string, unitmap : unitmap, nilmod : Nil.module) : Rtl.module =
      compile'(false,unitname,unitmap,nilmod)

end
