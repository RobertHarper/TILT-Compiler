(*$import LINKASM Linkrtl Sparc Labelgraph SparcUtils IfGraph SparcCallConv Bblock Tracetable DivMult ToSparc PrintUtils VarGraph SparcTrackStorage Color Chaitin RtlToAsm Recursion *)


structure Linksparc :> LINKASM =
struct
  val error = fn s => Util.error "linksparc.sml" s
  open Linkrtl


  structure Sparc = Sparc
  structure Machine = Sparc.Machine
  structure Sparcutils = Sparcutils
  structure Callconv = SparcCallconv
  structure Ifgraph = Ifgraph(structure Machine = Machine)
  structure Tracetable = Tracetable(val little_endian = true
				    structure ArgMachine = Sparc.Machine)

  structure Bblock = Bblock(structure Machine = Machine
			    structure Machineutils = Sparcutils
			    structure Tracetable = Tracetable)


  structure Tosparc = Tosparc(structure Machineutils = Sparcutils
			      structure ArgTracetable = Tracetable
			      structure Bblock = Bblock)
			      

  structure Printutils = Printutils(val commentHeader = " !"
				    structure Bblock = Bblock
				    structure Machineutils = Sparcutils
				    structure Tracetable = Tracetable)

  structure Recursion = Recursion(structure Printutils = Printutils)

  structure Trackstorage = SparcTrackstorage(structure Printutils = Printutils	
					     structure Machineutils = Sparcutils)


  structure Color1 = Color1(structure Machine = Machine
			    structure Ifgraph = Ifgraph
			    structure Trackstorage = Trackstorage
			    structure MU = Sparcutils
			    structure Printutils = Printutils)

  structure Chaitin = Chaitin(val commentHeader = " !"
			      structure Machineutils = Sparcutils
			      structure Callconv = Callconv
			      structure Bblock = Bblock
			      structure Trackstorage = Trackstorage			      
			      structure Printutils = Printutils
			      structure Ifgraph = Ifgraph
			      structure Color = Color1
			      structure Tracetable = Tracetable)


  structure Rtltosparc = Rtltoasm(val commentHeader = " !"
				  structure Machineutils = Sparcutils
				  structure Callconv = Callconv
				  structure Printutils = Printutils
				  structure Procalloc = Chaitin
				  structure Recursion = Recursion
				  structure Toasm = Tosparc)

  val prelude_modules : ((Rtl.label list * string list) option) ref = ref NONE
  val prelude_modules_hprof : ((Rtl.label list * string list) option) ref = ref NONE

  fun asm_suffix() = ".sparc.s"

  fun comp (asm_file,rtlmod) = 
    let 
	val _ = print "\n================================================\n"
	val _ = print "Starting translation to TIL-Sparc assembly\n"
	val _ = Printutils.openOutput asm_file
	val _ = Rtltosparc.allocateModule rtlmod
	val _ = Printutils.closeOutput()
	val _ = print "Generation of TIL-Sparc assembly files complete\n"
    in asm_file
    end

  fun wrapper string command = Stats.timer(string,command)
  val comp = wrapper "toasm" comp

  fun rtl_to_asm (asm_file, rtlmod) : string * Rtl.label =
      let val Rtl.MODULE{main,...} = rtlmod
      in (comp(asm_file, rtlmod), main)
      end

  fun link (asm_file,labels) = 
    let val rtlmod = Tortl.entryTables labels
	val _ = rtl_to_asm(asm_file,rtlmod)
    in  ()
    end

end
