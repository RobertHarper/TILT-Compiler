(*$import LINK TopLevel Linkrtl TextIO AsmStream SparcEmitRtlMLRISC *)

(* =========================================================================
 * SparcLink.sml
 * ========================================================================= *)

structure SparcLink :> LINKASM
	    = struct

  (* -- translation functions (adapted from linksparc.sml) ----------------- *)

  val asm_suffix = ".sparc.s"

  fun comp(asmfile, rtlmod) = 
	let
	  val stream = TextIO.openOut asmfile
	in
	  AsmStream.asmOutStream := stream;
	  SparcEmitRtlMLRISC.emitModule rtlmod;
	  TextIO.closeOut stream;
	  print "Generation of MLRISC-Sparc assembly files complete\n";
	  asmfile
	end

  fun wrapper string command = Stats.timer(string,command)
  val comp = wrapper "toasm" comp

  fun rtl_to_asm (asm_file, rtlmod) : string * Rtl.label =
      let val Rtl.MODULE{main,...} = rtlmod
      in (comp(asm_file,rtlmod), main)
      end

  fun link (asm_file,labels) = 
    let val rtlmod = Tortl.entryTables labels
	val _ = rtl_to_asm(asm_file,rtlmod)
    in  ()
    end

end

