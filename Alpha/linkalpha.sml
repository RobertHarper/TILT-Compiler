(*$import LINKASM Linkrtl DecAlpha Labelgraph DecAlphaUtils IfGraph CallConv Bblock Tracetable DivMult ToAlpha PrintUtils VarGraph TrackStorage Color Chaitin RtlToAsm Recursion *)


structure Linkalpha :> LINKASM =
struct
  val error = fn s => Util.error "linkalpha.sml" s
  open Linkrtl


  structure Decalpha = Decalpha
  structure Machine = Decalpha.Machine
  structure Decalphautils = Decalphautils
  structure Callconv = DecalphaCallconv
  structure Ifgraph = Ifgraph(structure Machine = Machine)
  structure Tracetable = Tracetable(val little_endian = true
				    structure ArgMachine = Decalpha.Machine)

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

  fun base2ui base = base ^ ".alpha.ui"
  fun base2s base = base ^ ".alpha.s"
  fun base2o base = base ^ ".alpha.o"
  fun base2uo base = base ^ ".alpha.uo"

  fun comp (base_file,rtlmod) = 
    let val asm_file = base2s base_file
	val _ = print "\n================================================\n"
	val _ = print "Starting translation to TIL-Alpha assembly\n"
	val _ = Printutils.openOutput asm_file
	val _ = Rtltoalpha.allocateModule rtlmod
	val _ = Printutils.closeOutput()
	val _ = print "Generation of TIL-Alpha assembly files complete\n"
    in asm_file
    end

  fun wrapper string command = Stats.timer(string,command)
  val comp = wrapper "toasm" comp

  fun rtl_to_asm (base_file, rtlmod) : string * Rtl.label =
      let val Rtl.MODULE{main,...} = rtlmod
      in (comp(base_file, rtlmod), main)
      end

  fun link (base_file,labels) = 
    let val rtlmod = Tortl.entryTables labels
    in  #1(rtl_to_asm(base_file,rtlmod))
    end

end
