(*$import Util Rtl Stats LINKASM Linkrtl DecAlpha Labelgraph DecAlphaUtils IfGraph CallConv Bblock Tracetable DivMult ToAlpha PrintUtils VarGraph TrackStorage Color Chaitin RtlToAsm Recursion Tortl *)


structure Linkalpha :> LINKASM =
struct
  val error = fn s => Util.error "linkalpha.sml" s
  open Linkrtl


  structure Decalpha = Decalpha
  structure Machine = Decalpha.Machine
  structure Decalphautils = Decalphautils
  structure Callconv = DecalphaCallconv
  structure Ifgraph = Ifgraph(structure Machine = Machine)
  structure Tracetable = Tracetable(val little_endian = true)

  structure Bblock = Bblock(structure Machine = Machine
			    structure Machineutils = Decalphautils
			    structure Tracetable = Tracetable)


  structure Divmult = Divmult

  structure Toalpha = Toalpha(structure Machineutils = Decalphautils
			      structure ArgTracetable = Tracetable
			      structure Bblock = Bblock
			      structure DM = Divmult)
			      

  structure Printutils = Printutils(val commentHeader = " #"
				    structure Bblock = Bblock
				    structure Machineutils = Decalphautils
				    structure Tracetable = Tracetable)

  structure Recursion = Recursion(structure Printutils = Printutils)

  structure Trackstorage = AlphaTrackstorage(structure Printutils = Printutils	
					     structure Machineutils = Decalphautils)


  structure Color1 = Color1(structure Machine = Machine
			    structure Ifgraph = Ifgraph
			    structure Trackstorage = Trackstorage
			    structure MU = Decalphautils
			    structure Printutils = Printutils)

  structure Chaitin = Chaitin(val commentHeader = " #"
			      structure Machineutils = Decalphautils
			      structure Callconv = Callconv
			      structure Bblock = Bblock
			      structure Trackstorage = Trackstorage			      
			      structure Printutils = Printutils
			      structure Ifgraph = Ifgraph
			      structure Color = Color1
			      structure Tracetable = Tracetable)


  structure Rtltoalpha = Rtltoasm(val commentHeader = " #"
				  structure Machineutils = Decalphautils
				  structure Callconv = Callconv
				  structure Printutils = Printutils
				  structure Procalloc = Chaitin
				  structure Recursion = Recursion
				  structure Toasm = Toalpha)

  val prelude_modules : ((Rtl.label list * string list) option) ref = ref NONE
  val prelude_modules_hprof : ((Rtl.label list * string list) option) ref = ref NONE

  fun comp (asm_file,rtlmod) = 
    let val _ = print "===== Translating to TIL-Alpha assembly      =====\n"
	val _ = Printutils.openOutput asm_file
	val _ = Rtltoalpha.allocateModule rtlmod
	val _ = Printutils.closeOutput()
    in	()
    end

  val rtl_to_asm = Stats.timer("To Alpha ASM",comp)

  fun link {asmFile,units} = 
    let val rtlmod = Tortl.entryTables units
    in  rtl_to_asm(asmFile,rtlmod)
    end

end
