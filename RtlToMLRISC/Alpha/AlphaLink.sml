(*$import LINK TopLevel Linkrtl TextIO AsmStream AlphaEmitRtlMLRISC *)

(* =========================================================================
 * AlphaLink.sml
 * ========================================================================= *)

structure AlphaLink :> LINKASM
	    = struct

  (* -- translation functions (adapted from linkalpha.sml) ----------------- *)

  fun base2s base = base ^ ".alpha.s"
  fun base2o base = base ^ ".alpha.o"
  fun base2uo base = base ^ ".alpha.uo"

  fun comp(basefile, rtlmod) = 
	let
	    val asmfile = base2s basefile
	    val stream = TextIO.openOut asmfile
	in
	  AsmStream.asmOutStream := stream;
	  AlphaEmitRtlMLRISC.emitModule rtlmod;
	  TextIO.closeOut stream;
	  print "Generation of MLRISC-Alpha assembly files complete\n";
	  asmfile
	end

  fun wrapper string command = Stats.timer(string,command)
  val comp = wrapper "toasm" comp

  fun rtl_to_asm (base_file, rtlmod) : string * Rtl.label =
      let val Rtl.MODULE{main,...} = rtlmod
      in (comp(base_file,rtlmod), main)
      end

  fun link (base_file,labels) = 
    let val rtlmod = Tortl.entryTables labels
    in  #1 (rtl_to_asm(base_file,rtlmod))
    end

end

