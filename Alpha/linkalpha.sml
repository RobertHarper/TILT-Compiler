signature LINKALPHA =
sig
    val compile : string -> string list
    val test : string -> string list
end

structure Linkalpha : LINKALPHA =
struct
    val error = fn s => Util.error "linkalpha.sml" s

local
  open Linkrtl
in

  val do_tailcalls = ref true

  structure Decalpha = Decalpha(val exclude_intregs = [] : int list
				structure Rtl = Rtl)

  structure Regset     = Regset    (structure Machine = Decalpha)
  structure Regmap     = Regmap    (structure Machine = Decalpha)
  structure Labelgraph = Labelgraph(structure Machine = Decalpha)
  structure Labelmap   = Labelmap  (structure Machine = Decalpha)
  structure Ifgraph   = Ifgraph    (structure Machine = Decalpha)

  structure Decalphautils = Decalphautils(structure Decalpha = Decalpha
					  structure Labelmap = Labelmap
					  structure Regmap = Regmap
					  structure Regset = Regset)
 
  structure Callconv   = DecalphaCallconv (structure Machineutils = Decalphautils
					   structure Decalpha = Decalpha)

  structure Bblock = Bblock(structure Machineutils = Decalphautils)

  structure Tracetable = Tracetable(val little_endian = true
				    structure MU = Decalphautils
				    structure R = Rtl)

  structure Divmult = Divmult(structure MU = Decalphautils
			      structure DA = Decalpha)

  structure Toalpha = Toalpha(structure Bblock = Bblock
			      structure Decalpha = Decalpha
			      structure DM = Divmult
			      structure Machineutils = Decalphautils
			      structure Rtl = Rtl
			      structure Pprtl = Pprtl
			      structure Tracetable = Tracetable
			      structure DM = Divmult
			      val do_tailcalls = do_tailcalls)

  structure Printutils = Printutils(val commentHeader = " #"
				    structure Bblock = Bblock
				    structure Machineutils = Decalphautils
				    structure Tracetable = Tracetable)

  structure Graph = Vargraph()

  structure Recursion = Recursion(structure Pprtl = Pprtl
				  structure Printutils = Printutils
				  structure Graph = Graph)
 
  structure Trackstorage = AlphaTrackstorage(val commentHeader = " #"
					     structure Regmap = Regmap
					     structure Regset = Regset
					     structure Decalpha = Decalpha
					     structure Printutils = Printutils)

  structure Color1 = Color1(structure Ifgraph = Ifgraph
			    structure Trackstorage = Trackstorage
			    structure MU = Decalphautils
			    structure Printutils = Printutils)

  structure Chaitin = Chaitin(val commentHeader = " #"
			      structure Bblock = Bblock
			      structure Callconv = Callconv
			      structure Color = Color1
			      structure Decalpha = Decalpha
			      structure Printutils = Printutils
			      structure Tracetable = Tracetable
			      structure Trackstorage = Trackstorage
			      structure Ifgraph = Ifgraph)

  structure Rtltoalpha = Rtltoasm(val commentHeader = " #"
				  structure Callconv = Callconv
				  structure Procalloc = Chaitin
				  structure Printutils = Printutils
				  structure Recursion = Recursion
				  structure Toasm = Toalpha)

  val prelude_modules : ((Rtl.local_label list * string list) option) ref = ref NONE
  val prelude_modules_hprof : ((Rtl.local_label list * string list) option) ref = ref NONE

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

  fun comp (x,rtlmod) = 
    let 
	val files = []
(*
      val (prelude_nl,files) = 
	(case (if (!Linkrtl.Tortl.HeapProfile) 
		 then !prelude_modules_hprof else !prelude_modules) of
	   NONE => 
	     (error ("missing Alpha prelude modules: " ^ 
		     "try Linkalpha.reparse_prelude(\"\",\"\")"))
	 | (SOME (prelude_nl,files)) => (prelude_nl,files))
*)
      val xs = x^(asm_suffix())
      val _ = Printutils.openOutput xs
      val _ = Rtltoalpha.allocateModule rtlmod
      val (Rtl.MODULE{main=nl,...}) = rtlmod
      val nls = [nl]
      val _ = Rtltoalpha.dumpEntryTables nls
      val _ = Printutils.closeOutput()
      val res = files @ [xs]
      val _ = print "Generation of assembly files complete\n"
    in res
    end

  fun compile' debug  str = ((* Linkrtl.Tortl.hasFullConditionalBranch := false; *)
			     comp (str,(if debug then Linkrtl.test else Linkrtl.compile) str))

  val compile = compile' false
  val test = compile' false
 end (* open *)
end;


