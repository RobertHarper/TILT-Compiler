(*$import LINK TopLevel Linkrtl TextIO AsmStream SparcEmitRtlMLRISC *)

(* =========================================================================
 * SparcLink.sml
 * ========================================================================= *)

structure SparcLink :> LINK
	    = struct

  (* -- translation functions (adapted from linkalpha.sml) ----------------- *)

  val asm_suffix = ".alpha.s"

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

  fun link(srcfile, labels) =
	let
	  val asmfile = srcfile^asm_suffix
	  val stream  = TextIO.openAppend asmfile
	in
	  AsmStream.asmOutStream := stream;
	  SparcEmitRtlMLRISC.emitEntryTable labels;
	  TextIO.closeOut stream;
	  ()
	end

  fun mk_link_file(asmfile, labels) = 
	let
	  val stream = TextIO.openOut asmfile
	in
	  AsmStream.asmOutStream := stream;
	  SparcEmitRtlMLRISC.emitEntryTable labels;
	  TextIO.closeOut stream;
	  ()
	end

  fun compiles filenames = 
      let val rtlmods = Linkrtl.compiles filenames
	  fun doit (filename,rtlmod) = let val Rtl.MODULE{main,...} = rtlmod
				       in  (comp(filename ^ asm_suffix,rtlmod),main)
				       end
      in  Listops.map2 doit (filenames,rtlmods)
      end
  fun compile filename = hd(compiles [filename])

  fun rtl_to_entrance (filename, rtlmod) : string * Rtl.label =
      let val Rtl.MODULE{main,...} = rtlmod
      in (comp(filename ^ ".s",rtlmod), main)
      end

  fun test filename = 
      let val rtlmod = Linkrtl.test filename
	  val Rtl.MODULE{main,...} = rtlmod
      in  (comp(filename ^ asm_suffix,rtlmod),main)
      end

  val cached_prelude = ref (NONE : (string * Rtl.label) option)
  fun compile_prelude (use_cache,filename) = 
      case (use_cache, !cached_prelude) of
	  (true, SOME mlabel) => mlabel
	| _ => let val rtlmod = Linkrtl.compile_prelude(use_cache,filename)
		   val Rtl.MODULE{main=label,...} = rtlmod
		   val mlabel = (comp(filename ^ asm_suffix,rtlmod),label)
		   val _ = cached_prelude := SOME mlabel
	       in  mlabel
	       end

end

