(*$import LINK Linkrtl DecAlpha Regset Regmap Labelgraph Labelmap DecAlphaUtils IfGraph CallConv Bblock Tracetable DivMult ToAlpha PrintUtils VarGraph TrackStorage Color Chaitin RtlToAsm ToAlpha Recursion *)


structure Linkalpha :> LINKASM =
struct
  val error = fn s => Util.error "linkalpha.sml" s
  open Linkrtl

  val do_tailcalls = ref true

  structure Decalpha = Decalpha(val exclude_intregs = [] : int list
				structure Rtl = Rtl)
  structure Machine = Decalpha.Machine

(*
  structure Regset     = Regset    (structure Machine = Machine)
  structure Regmap     = Regmap    (structure Machine = Machine)
  structure Labelgraph = Labelgraph(structure Machine = Machine)
  structure Labelmap   = Labelmap  (structure Machine = Machine)
*)

  structure Decalphautils = Decalphautils(structure Decalpha = Decalpha
(*
					  structure Labelmap = Labelmap
					  structure Regmap = Regmap
					  structure Regset = Regset
*)
					      )

  structure Callconv   = DecalphaCallconv (structure Decalpha = Decalpha
					   structure Machineutils = Decalphautils)


  structure Ifgraph   = Ifgraph    (structure Machine = Machine)


  structure Tracetable = Tracetable(val little_endian = true
				    structure MU = Decalphautils)

  structure Bblock = Bblock(structure Machineutils = Decalphautils
			    structure Tracetable = Tracetable)


  structure Divmult = Divmult(structure Decalpha = Decalpha
			      structure MU = Decalphautils)

  structure Toalpha = Toalpha(val do_tailcalls = do_tailcalls
			      structure Decalpha = Decalpha
			      structure Pprtl = Pprtl
			      structure Machineutils = Decalphautils
			      structure ArgTracetable = Tracetable
			      structure Bblock = Bblock
			      structure DM = Divmult)
			      

  structure Printutils = Printutils(val commentHeader = " #"
				    structure Bblock = Bblock
				    structure Machineutils = Decalphautils
				    structure Tracetable = Tracetable)



  structure Graph = Labelgraph()

  structure Recursion = Recursion(structure Pprtl = Pprtl
				  structure Graph = Graph
				  structure Printutils = Printutils)

 

  structure Trackstorage = AlphaTrackstorage(structure Decalpha = Decalpha
					     structure Printutils = Printutils					     
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

  fun asm_suffix() = ".alpha.s"

(*
  fun asm_suffix() = if (!Linkrtl.Tortl.HeapProfile)
		       then ".hprof.alpha.s"
		     else if (not (!Linkrtl.Tortl.do_writelist))
		       then ".semi.alpha.s"
		     else ".alpha.s"

  val default_prelude = "Preludes/LEND_Prelude.sml"
  val default_inlineprelude = "Preludes/LEND_InlinePrelude.sml"
  fun reparse_prelude (NONE) = reparse_prelude(SOME(default_prelude, default_inlineprelude))
    | reparse_prelude (SOME(pre,ipre)) = 
	      let 
		  val _ = Linkrtl.reparse_prelude (pre,ipre)
		  fun doit(rtl_preludes) =
		    let val names = map (fn (_,Rtl.MODULE{main,...}) => main) rtl_preludes
			fun loop [] count = []
			  | loop (a::rest) count = 
			  let val fname = 
			    "prelude"^(makestring count)^".sml"^asm_suffix()
			  in
			    (print "\n\n";
			   Printutils.openOutput ("Runtime/"^fname);
			     (case a of 
				(_,x) => Rtltoalpha.allocateModule x);
				Printutils.closeOutput();
				fname::(loop rest (count+1)))
			  end
			val fnames = loop rtl_preludes 0
		    in SOME(names,fnames)
		    end
		  val prelude_bs = 
		      (case !Linkrtl.prelude_modules of
			   NONE => error "missing RTL prelude modules."
			 | (SOME bs) => bs)
		  val prelude_bs_hprof = 
		      (case !Linkrtl.prelude_modules_hprof of
			   NONE => error "missing RTL hprof prelude modules."
			 | (SOME bs) => bs)
	      in
		(Linkrtl.Tortl.HeapProfile := false;
		 prelude_modules := doit(prelude_bs);
		 Linkrtl.Tortl.HeapProfile := true;
		 prelude_modules_hprof := doit(prelude_bs_hprof);
		 Linkrtl.Tortl.HeapProfile := false)
	      end

   open Compstream

  fun comp_stream s = 
  let fun loop (EOS k) = EOS (fn source => loop(k source))
	| loop (CONS((_,rtl as Rtl.MODULE{main,...}),k)) = 
	  let val a = Comptime.time Comptime.register_alloc
	                            Rtltoalpha.allocateModule rtl
	  in
	      CONS(main,loop o k)
	  end
  in
      loop s
  end
*)

  fun comp (asm_file,rtlmod) = 
    let val _ = Printutils.openOutput asm_file
	val _ = Rtltoalpha.allocateModule rtlmod
	val _ = Printutils.closeOutput()
	val _ = print "Generation of TIL-Alpha assembly files complete\n"
    in asm_file
    end

  fun wrapper string command = Stats.timer(string,command)
  val comp = wrapper "toasm" comp

  fun link (srcfile,labels) = 
    let 
      val asm_file = srcfile ^ (asm_suffix())
      val _ = Printutils.openAppend asm_file
      val _ = Rtltoalpha.dumpEntryTables labels 
      val _ = Printutils.closeOutput()
    in ()
    end

  fun mk_link_file (asm_file,labels) = 
    let 
      val _ = Printutils.openOutput asm_file
      val _ = Rtltoalpha.dumpEntryTables labels 
      val _ = Printutils.closeOutput()
    in ()
    end

  fun rtl_to_asm (filename, rtlmod) : string * Rtl.label =
      let val Rtl.MODULE{main,...} = rtlmod
      in (comp(filename ^ ".s",rtlmod), main)
      end

end
