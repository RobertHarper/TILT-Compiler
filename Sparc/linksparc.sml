(*$import Util Rtl Stats LINKASM Linkrtl Sparc Labelgraph SparcUtils IfGraph SparcCallConv Bblock Tracetable DivMult ToSparc PrintUtils VarGraph SparcTrackStorage Color Chaitin RtlToAsm Recursion Tortl *)


structure Linksparc :> LINKASM =
struct
  val error = fn s => Util.error "linksparc.sml" s
  open Linkrtl


  structure Sparc = Sparc
  structure Machine = Sparc.Machine
  structure Sparcutils = Sparcutils
  structure Callconv = SparcCallconv
  structure Ifgraph = Ifgraph(structure Machine = Machine)
  structure Tracetable = Tracetable(val little_endian = false)

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

  fun comp (asm_file,rtlmod) = 
    let val _ = print "===== Translating to TIL-Sparc assembly      =====\n"
	val _ = Printutils.openOutput asm_file
	val _ = Rtltosparc.allocateModule rtlmod
	val _ = Printutils.closeOutput()
    in	()
    end

  val rtl_to_asm = Stats.timer("To Sparc ASM",comp)

  fun link {asmFile, units} = 
    let val rtlmod = Tortl.entryTables units
    in  rtl_to_asm(asmFile,rtlmod)
    end

end
