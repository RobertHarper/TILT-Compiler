structure Linkrtl :> LINKRTL =
struct

  val ptrWriteBarrier = TortlArray.ptrWriteBarrier
  val fullWriteBarrier = TortlArray.fullWriteBarrier
  val mirrorPtrArray = TortlArray.mirrorPtrArray
  val mirrorGlobal = TortlBase.mirrorGlobal

  val LinkRtlDiag = Stats.ff ("LinkRtlDiag")
  fun msg str = if (!LinkRtlDiag) then print str else ()

  val ShowRtl = Stats.ff("ShowRtl")
  val error = fn x => Util.error "Linkrtl." x

  fun compile' (debug,unitname,nilmodule) =
    let val _ = msg ("===== Translating to RTL: " ^ unitname ^ " =====\n")
      val rtlmod = Tortl.translate(unitname, nilmodule)
      val _ = if debug orelse !ShowRtl
		then (print "RTL code:\n";
		      Pprtl.pp_Module rtlmod;
		      print "\n\n")
	      else ()
    in  rtlmod
    end

  val compile' = Stats.timer("Translation to RTL",compile')
  fun nil_to_rtl (unitname : string, nilmod : Nil.module) : Rtl.module =
      compile'(false,unitname,nilmod)

end
