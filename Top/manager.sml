functor Manager (structure Parser: LINK_PARSE
		 structure Elaborator: ELABORATOR
		 structure Compiler: COMPILER
		 structure Linker: LINKER
		 structure Makedep: MAKEDEP
		 sharing type Elaborator.sbnds = Compiler.sbnds
		 sharing type Elaborator.context = Compiler.context
		) : MANAGER = 
struct

  structure Basis = Elaborator.Basis
  structure UIBlast = mkBlast(type t = Elaborator.context)

  val error = fn x => Util.error "Manager" x

  val chat_ref = ref true
  fun chat s = if !chat_ref then print s
	       else ()

  fun help() = print "This is TILT - no help available.\n"

  fun getContext imports = 
      let val (_,_,_,ctxt_noninline) = Basis.initial_context()
	  val ctxts = List.map (fn file => UIBlast.blastIn (file^".ui")) imports
      in Elaborator.plus_context (ctxt_noninline :: ctxts)
      end

  fun emitter oFile outs = 
      let val ins = BinIO.openIn oFile
      in BinIO.output(outs, BinIO.input(ins));
         BinIO.closeIn ins
      end

  fun elab_constrained(ctxt,sourcefile,fp,dec,uiFile) =
      let val ctxt' = UIBlast.blastIn(uiFile) 
	              handle _ => error ("File "^uiFile^" not found.")
      in case Elaborator.elab_dec_constrained(ctxt, fp, dec, ctxt')
	   of SOME sbnds => (sbnds, ctxt')
            | NONE => error("File " ^ sourcefile ^ " failed to elaborate.")
      end

  fun elab_nonconstrained(ctxt,sourcefile,fp,dec,uiFile) =
      case Elaborator.elab_dec(ctxt, fp, dec)
	of SOME(sbnds, ctxt') => 
	    let	val _ = (if Elaborator.eq_context(ctxt', UIBlast.blastIn uiFile) then ()
			 else UIBlast.blastOut(uiFile, ctxt'))
		        handle IO.Io _ => UIBlast.blastOut(uiFile, ctxt')
	    in (sbnds, ctxt')
	    end
         | NONE => error("File " ^ sourcefile ^ " failed to elaborate.")

  fun compileSML sourcefile = 
      let val _ = chat ("  [Parsing " ^ sourcefile ^ "...")
	  val (fp, imports, dec) = Parser.parse_impl sourcefile
	  val _ = chat "]\n"
	  val _ = chat "  [Creating context from imports..."
	  val ctxt = getContext imports
	  val _ = chat "]\n"
	  val imports = List.map (fn x => (x, Linker.Crc.crc_of_file (x^".ui"))) imports
	  val unitName = OS.Path.base(OS.Path.file sourcefile)
	  val uiFile = unitName^".ui"
	  val intFile = unitName^".int"
	  val oFile = unitName^".o"
	  val (sbnds, ctxt') = 
	      if OS.FileSys.access(intFile, []) then 
		  let val _ = chat "  [Elaborating with constraint..."  
		  in elab_constrained(ctxt,sourcefile,fp,dec,uiFile)
		  end
	      else let val _ = chat "  [Elaborating non-constrained..."
		   in elab_nonconstrained(ctxt,sourcefile,fp,dec,uiFile)
		   end
	  val _ = chat "]\n"
	  val _ = chat ("  [Compiling into " ^ unitName ^ ".o ...")
	  val _ = Compiler.compile(ctxt, unitName, sbnds, ctxt')  (* generates oFile *)
	  val _ = chat "]\n"
	  val crc = Linker.Crc.crc_of_file uiFile
	  val exports = [(unitName, crc)]
	  val _ = chat ("  [Creating " ^ unitName ^ ".uo ...")
	  val res = Linker.mk_uo {imports = imports,
				  exports = exports,
				  uo_result = (unitName^".uo"),
				  emitter = emitter oFile}
	  val _ = chat "]\n"
      in res
      end

  fun compileINT sourcefile = 
      let val (fp, includes, specs) = Parser.parse_inter sourcefile
	  val ctxt = getContext includes
	  val unitName = OS.Path.base(OS.Path.file sourcefile)
      in case Elaborator.elab_specs(ctxt, fp, specs)  
	   of SOME ctxt' => UIBlast.blastOut(unitName^".ui", ctxt')
	    | NONE => error("File " ^ sourcefile ^ " failed to elaborate.")
      end

  fun compileFile sourcefile = 
      case OS.Path.ext sourcefile
	of SOME "sml" => compileSML sourcefile
         | SOME "int" => compileINT sourcefile
	 | SOME "uo" => ()                      (* Ignores .uo and .ui files *)
	 | SOME "ui" => () 
	 | _ => error("Missing or unsupported file extension in file " ^ sourcefile)

  val flags = (ref false, ref false, ref false) (* c, r ,o *)

  fun resetFlags () = (#1(flags) := false,
		       #2(flags) := false,
		       #3(flags) := false)

  fun getArgs args = let
    fun getArgsH [] = (false, NONE, NONE, [])
      | getArgsH ("-c"::rest) = let
	  val (cs, rs, os, srcs) = getArgsH rest
	  val (seen, _, _) = flags
	in
	  if !seen then error ("Two -c switches not allowed.") else
	    (seen := true; (true, rs, os, srcs))
	end
      | getArgsH ("-o"::rest) = if List.length(rest) = 0 then
	error "No output file specified for -o switch." else let 
	  val file = hd rest
	  val (cs, rs, os, srcs) = getArgsH (tl rest)
	  val (_, _, seen) = flags
	in
	  if (!seen) then error ("Two -o switches not allowed.") else
	    (seen := true; (cs, rs, SOME file, srcs))
	end
      | getArgsH ("-r"::rest) = if List.length(rest) = 0 then
	error "No output file specified for -r switch." else let
	  val file = hd rest
	  val (cs, rs, os, srcs) = getArgsH (tl rest)
	  val (_, seen, _) = flags
	in
	  if (!seen) then error ("Two -r switches not allowed.") else
	    (seen := true; (cs, SOME file, rs, srcs))
	end
      | getArgsH (f::rest) = let
	  val (cs, rs, os, srcs) = getArgsH rest
	in
	  (cs, rs, os, f::srcs)
	end
  in
    (resetFlags(); getArgsH args)
  end

  fun srcToUO file = let
    val ext = OS.Path.ext(file)
  in
    case ext of
      SOME "uo" => SOME file
    | SOME "sml" => SOME (OS.Path.base(file)^".uo")
    | _ => NONE
  end

  fun compileThem(link, uo, exec, out, srcs) = let
    val _ = List.app compileFile srcs
    val tmp_uo = OS.FileSys.tmpName() ^ ".uo"
    val uo_args = List.mapPartial srcToUO srcs
  in
    case uo of
	 SOME f => (if link then Linker.link {uo_args = uo_args, uo_result = f}
		    else ();
		    if exec then Linker.mk_exe {uo_arg = f, exe_result = out}
		    else ())
       | NONE => (if link then Linker.link{uo_args = uo_args, uo_result = tmp_uo}
		  else ();
		  if exec then Linker.mk_exe {uo_arg = tmp_uo, exe_result = out}
		  else ();
		  OS.FileSys.remove tmp_uo)
  end
       
       
  fun tilc(args, env:string list) = 
    case args of 
      [] => error ("No arguments specified.")
    | (("-h"::args)|("-?"::args)) => if args = [] then help() else
	error ("Invalid arguments: -h or -? must occur by itself.")
    | (["-mkdep", makefile]) =>	Makedep.mkDep(makefile)
    | ("-mkdep"::args) => error ("Incorrect number of files to -mkdep")
    | args => let 
	val (cs, rs, os, srcs) = getArgs args
      in
	if srcs = [] then error ("No source files specified.") else 
	  case (cs, rs, os) of
	    (false, NONE, NONE) =>   compileThem(true, NONE, true, "a.out", srcs)
	  | (false, SOME f, NONE) => compileThem(true, SOME f, false, "", srcs)
	  | (false, NONE, SOME f) => compileThem(true, NONE, true, f, srcs)
	  | (false, SOME f, SOME g)=> compileThem(true, SOME f, true, g, srcs)
	  | (true, NONE, NONE) =>    compileThem(false, NONE, false, "", srcs)
	  | (true, SOME f, NONE) =>  compileThem(true, SOME f, false, "", srcs)
	  | (true, NONE, SOME f) =>  compileThem(true, NONE, true, f, srcs)
	  | (true, SOME f, SOME g) =>compileThem(true, SOME f, true, g, srcs)
      end
    

end


structure TM = Manager(structure Parser = LinkParse
		       structure Elaborator = LinkIl
		       structure Compiler = Compiler
		       structure Linker = Linker
		       structure Makedep = Makedep)