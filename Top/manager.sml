(* should add caching of import scans and contexts read in *)

functor Manager (structure Parser: LINK_PARSE
		 structure Elaborator: ELABORATOR
		 structure Compiler: COMPILER
		 structure Linker: LINKER
		 structure Makedep: MAKEDEP
		 sharing type Elaborator.sbnd = Compiler.sbnd
		 sharing type Elaborator.context_entry = Compiler.context_entry
		 sharing type Elaborator.context = Compiler.context
		) : MANAGER = 
struct

  val up_to_phasesplit = ref false

  structure Basis = Elaborator.Basis
(*  structure UIBlast = mkBlast(type t = Elaborator.context) *)

  val error = fn x => Util.error "Manager" x

  val chat_ref = ref true
  fun chat s = if !chat_ref then (print s; TextIO.flushOut TextIO.stdOut)
	       else ()

  fun help() = print "This is TILT - no help available.\n"
(*
  fun addPath fileWithPath fileWithoutPath =
      let val path = OS.Path.dir fileWithPath
      in  (case path of
	       "" => fileWithoutPath
	     | _ => path ^ "/" ^ fileWithoutPath)
      end
*)
  fun readContext file = let val is = TextIO.openIn file
			     val res = Elaborator.IlContext.blastInContext is
			     val _ = TextIO.closeIn is
			 in  res
			 end
  fun writeContext (file,ctxt) = let val os = TextIO.openOut file
				     val _ = Elaborator.IlContext.blastOutContext os ctxt
				     val _ = TextIO.closeOut os
				 in  ()
				 end

  fun name2base (mapping : (string * string) list) (name : string) = 
      (case Listops.assoc(name,mapping) of
	   SOME s => s
	 | NONE => error (name ^ " is not a known unit name"))
  fun base2ui (f : string) = f ^ ".ui"
  fun base2uo (f : string) = f ^ ".uo"
  fun base2sml (f : string) = f ^ ".sml"
  fun base2int (f : string) = f ^ ".int"


  fun getContext imports = 
      let val (ctxt_inline,_,_,ctxt_noninline) = Basis.initial_context()
(*	  val ctxts = List.map (fn file => UIBlast.blastIn (base2ui file)) imports *)
	  val ctxts = List.map (fn file => readContext (base2ui file)) imports
	  val context_basis = Elaborator.plus_context (ctxt_inline :: ctxts)
	  val context = Elaborator.plus_context ctxts
      in (context_basis, context)
      end

    fun bincopy (is,os) = 
	let fun loop() = (BinIO_Util.copy(is,os); if (BinIO.endOfStream is) then () else loop())
	in  loop()
	end
    fun emitter in_file os = let (* val _ = (print "mk_emitter on file "; print in_file; print "\n") *)
				 val is = BinIO.openIn in_file
			     in bincopy(is,os); 
				 BinIO.closeIn is
			     end

  fun elab_constrained(ctxt,sourcefile,fp,dec,uiFile) =
      let val _ = (print "blasting in "; print uiFile)
(*	  val ctxt' = UIBlast.blastIn(uiFile) 
	              handle _ => error ("File "^uiFile^" not found.") *)
	  val ctxt' = readContext uiFile
      in case Elaborator.elab_dec_constrained(ctxt, fp, dec, ctxt')
	   of SOME sbnds => (sbnds, ctxt')
            | NONE => error("File " ^ sourcefile ^ " failed to elaborate.")
      end

  fun elab_nonconstrained(pre_ctxt,sourcefile,fp,dec,uiFile) =
      case Elaborator.elab_dec(pre_ctxt, fp, dec)
	of SOME(sbnd_entries, new_ctxt) => 
	    let	val same = 
		(OS.FileSys.access(uiFile, [OS.FileSys.A_READ]) andalso
		 Elaborator.eq_context(new_ctxt, readContext uiFile))
		val _ = if same then ()
			else (chat ("[writing " ^ uiFile);
			      writeContext(uiFile, new_ctxt);
			      chat "\n")
	    in (sbnd_entries, new_ctxt)
	    end
         | NONE => error("File " ^ sourcefile ^ " failed to elaborate.")

  (* Takes a string(line) and splits into white-space separted fields.
     Inclusively drops all fields after the first field(non-empty string) that passes dropper. *)
  fun split_line dropper line = let val fields = String.fields Char.isSpace line
				    fun filter [] = []
				      | filter (x::y) = if size x = 0 
							    then filter y
							else if dropper x
								 then []
							     else x::(filter y)
				in  filter fields
				end


  fun parse_impl_import smlfile = 
      let val ins = TextIO.openIn smlfile
	  val line = TextIO.inputLine ins
	  val sz = size line
	  val _ = TextIO.closeIn ins
	  val import_str = "(*$import"
	  val import_str_sz = size import_str
      in  if (sz >= import_str_sz andalso String.substring(line,0,import_str_sz) = import_str)
	      then    
		  let fun dropper s = ("(*"; s = "*)")
		  in  split_line dropper (String.substring(line,import_str_sz,sz-import_str_sz))
                  end
	  else (print ("Warning: first line of " ^ smlfile ^ " is not import.\n");
  	        print "Calling parser to process.\n";
		#2(Parser.parse_impl smlfile))
      end

  (* getImport:
   given an implementation file, find all the imports it depends on with leaves listed first *)
  fun getImport mapping smlfile : (string * string) list = 
      let val imports = parse_impl_import smlfile
	  fun folder(import,acc) = 
	      let val base = name2base mapping import
		  val depends = getImport mapping (base2sml base)
		  fun adder(p as (k,_), ac) = 
		      (case (Listops.assoc(k,ac)) of
			   NONE => p::ac
			 | SOME _ => ac)
	      in  adder((import,base),foldl adder acc depends)
	      end
      in  rev(foldl folder [] imports)
      end

  fun compileSML mapping sourcebase : unit = 
      let val smlfile = base2sml sourcebase
	  val uofile = base2uo sourcebase
	  val _ = chat ("  [Scanning " ^ smlfile ^ " for imports]\n")
	  val import_pairs = getImport mapping smlfile
	  val _ = (char "  [" ^ smlfile ^ "has imports: ";
		   app (fn (i,_) => (chat i; chat "  ")) import_pairs;
		   chat "]\n")
	  val imports = map #1 import_pairs
	  val import_bases = map #2 import_pairs

          (* work on imports *)
	  val _ = app (compile false mapping) imports
	  val imports_ui = map base2ui import_bases
	  val importdates = (map OS.FileSys.modTime imports_ui)
	      handle OS.SysErr _ => error "import .uo files not present"

	  val smldate = OS.FileSys.modTime smlfile
	  val uodate = OS.FileSys.modTime uofile
	      handle OS.SysErr _ => Time.zeroTime
(*
	  val _ = (print "smldate = "; print (Time.toString smldate); print "\n";
		   print "uodate = "; print (Time.toString uodate); print "\n";
		   app (fn t => (print "importdate = "; print (Time.toString t); print "\n"))
		   importdates; print "\n\n")
*)
      in  if (Listops.andfold (fn t => Time.<(t,uodate)) (smldate :: importdates))
	      then chat ("  [" ^ uofile ^ " is up-to-date]\n")
	  else compileSML' mapping (sourcebase, imports)
      end
  
  and compile make_uo mapping unit =
      let val sourcebase = name2base mapping unit
	  val source_sml = base2sml sourcebase
	  val source_int = base2int sourcebase
      in  case (make_uo,
		OS.FileSys.access(source_int, [OS.FileSys.A_READ]),
		OS.FileSys.access(source_sml, [OS.FileSys.A_READ])) of
	  (true, true, true) => compileSML mapping sourcebase
	| (false, true, true) => compileINT mapping sourcebase
	| (_,true, _) => compileINT mapping sourcebase
	| (_, _, true) => 
	      let val source_ui = base2ui sourcebase
		  val fresh = 
		      ((not make_uo)
		       andalso OS.FileSys.access(source_ui, [OS.FileSys.A_READ])
		       andalso Time.<(OS.FileSys.modTime(source_sml),
				      OS.FileSys.modTime(source_ui)))
	      in  if fresh
		      then ()
		  else compileSML mapping sourcebase
	      end
	| _ => error ("Missing " ^ source_sml ^ " and " ^ source_int ^ ": cannot generate .ui")
      end

  and compileSML' mapping (srcBase, imports) : unit = 
      let val smlfile = base2sml srcBase
	  val _ = chat ("  [Parsing " ^ smlfile)
	  val (fp, _, dec) = Parser.parse_impl smlfile
	  val _ = chat "]\n"
	  val _ = (chat "  [Creating context from imports: ";
		   app (fn s => (chat s; print " ")) imports)
	  val import_bases = map (name2base mapping) imports
	  val (ctxt_for_elab,ctxt) = getContext import_bases
	  val _ = chat "]\n"
	  val import_uis = List.map (fn x => (x, Linker.Crc.crc_of_file (x^".ui"))) import_bases
	  val uiFile = srcBase ^ ".ui"
	  val intFile = srcBase ^ ".int"
	  val oFile = srcBase ^ ".o"
	  val uoFile = srcBase ^ ".uo"
	  val (sbnds, ctxt') = 
	      if OS.FileSys.access(intFile, []) then 
		  let val _ = chat "  [Elaborating with constraint..."  
		  in elab_constrained(ctxt_for_elab,smlfile,fp,dec,uiFile)
		  end
	      else let val _ = chat "  [Elaborating non-constrained..."
		   in elab_nonconstrained(ctxt_for_elab,smlfile,fp,dec,uiFile)
		   end
	  val _ = chat "]\n"
	  val _ = if (!up_to_phasesplit)
		      then
			  (chat ("  [Just phase-splitting " ^ srcBase ^ " ...");
			   Compiler.pcompile(ctxt, srcBase, sbnds, ctxt');  (* generates oFile *)
			   chat "]\n")
		  else 
		      let val _ = (chat ("  [Compiling into " ^ oFile ^ " ...");
				   Compiler.compile(ctxt, srcBase, sbnds, ctxt');  (* generates oFile *)
				   chat "]\n")
			  val crc = Linker.Crc.crc_of_file uiFile
			  val exports = [(srcBase, crc)]
			  val _ = chat ("  [Creating " ^ uoFile ^ " ...")
		      in   Linker.mk_uo {imports = import_uis,
					 exports = exports,
					 uo_result = uoFile,
					 emitter = emitter oFile}
		      end
	  val _ = chat "]\n"
      in  ()
      end

  (* XXXXXX change to be recursive like compileSML *)
  and compileINT mapping sourcebase = 
      let val sourcefile = base2int sourcebase
	  val (fp, includes, specs) = Parser.parse_inter sourcefile
	  val includes = map (name2base mapping) includes
	  val (ctxt_for_elab,ctxt) = getContext includes
	  val unitName = OS.Path.base(OS.Path.file sourcefile)
	  val uiFile = (OS.Path.base sourcefile) ^ ".ui"
      in case Elaborator.elab_specs(ctxt_for_elab, fp, specs) of
	  SOME ctxt' => writeContext(uiFile, ctxt')
	| NONE => error("File " ^ sourcefile ^ " failed to elaborate.")
      end

(*
  fun compileFile mapping sourcefile = 
      case OS.Path.ext sourcefile of
	   SOME "sml" => compileSML mapping (OS.Path.base sourcefile)
         | SOME "int" => compileINT mapping (OS.Path.base sourcefile)
	 | SOME "uo" => ()                      (* Ignores .uo files *)
	 | SOME "ui" => error "Cannot compile or link .ui file"
	 | _ => error("Missing or unsupported file extension in file " ^ sourcefile)
*)

  val flags = (ref false, ref false, ref false) (* c, r ,o *)

  fun resetFlags () = (#1(flags) := false,
		       #2(flags) := false,
		       #3(flags) := false)


  (* getArgs:
     Takes a list of string arguments and returns a 4-tuple.
     The first indicates whether the -c flag is present.
     The second carries the name of the -r filename, if present.
     The third carries the name of the -o filename (final executable), if present.
     The last component is a list of the source files to process. *)

  fun getArgs (args : string list) : bool * string option * string option * string list = 
      let
	  fun getArgsH [] = (false, NONE, NONE, [])
	    | getArgsH ("-c"::rest) = 
	      let
		  val (cs, rs, os, srcs) = getArgsH rest
		  val (seen, _, _) = flags
	      in
		  if !seen then error ("Two -c switches not allowed.") else
	 	      (seen := true; (true, rs, os, srcs))
	      end
	    | getArgsH ("-o"::rest) = if List.length(rest) = 0 then
	      error "No output file specified for -o switch." else 
	      let 
		  val file = hd rest
		  val (cs, rs, os, srcs) = getArgsH (tl rest)
		  val (_, _, seen) = flags
	      in
		  if (!seen) then error ("Two -o switches not allowed.") else
		      (seen := true; (cs, rs, SOME file, srcs))
	      end
	    | getArgsH ("-r"::rest) = if List.length(rest) = 0 then
	      error "No output file specified for -r switch." else 
	      let
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


  (* linkopt - if present, names the file containing the concatenation of the generated .uo files
     exeopt - if present, names the final executable 
     srcs    - .int, .uo, or .sml filenames *)
  fun compileThem(mapping, linkopt, exeopt, units) = 
      let val bases = map (name2base mapping) units
	  val _ = List.app (compile true mapping) units
	  val tmp_uo = OS.FileSys.tmpName() ^ ".uo"
	  fun srcToUO file = let
				 val ext = OS.Path.ext(file)
			     in
				 case ext of
				     SOME "uo" => SOME file
				   | SOME "sml" => SOME ((OS.Path.base file)^".uo")
				   | _ => NONE
			     end
	  val uo_args = List.mapPartial srcToUO bases
      in
	  (case (linkopt,exeopt) of
	       (NONE,NONE) => ()
	     | (NONE, SOME out) => (Linker.link {uo_args = uo_args, uo_result = tmp_uo};
				    Linker.mk_exe {uo_arg = tmp_uo, exe_result = out};
				    OS.FileSys.remove tmp_uo)
	     | (SOME f, NONE) => Linker.link {uo_args = uo_args, uo_result = f}
	     | (SOME f, SOME out) => (Linker.link {uo_args = uo_args, uo_result = f};
				      Linker.mk_exe {uo_arg = f, exe_result = out}))
      end
       
  fun getMapping mapFile = 
      let val _ = if (OS.FileSys.access(mapFile, [OS.FileSys.A_READ]))
		      then ()
		  else error "Cannot read map file"
	  val is = TextIO.openIn mapFile
	  fun fetch_line() = let fun dropper s = String.sub(s,0) = #"#"
				 val line = TextIO.inputLine is
			     in  case (split_line dropper line) of
				 [unitname, filebase] => (unitname, filebase)
			       | _ => error ("ill-formed map line: " ^ line)
			     end
	  fun loop() = if (TextIO.endOfStream is)
			   then (TextIO.closeIn is; [])
		       else fetch_line()::loop()
      in  loop()
      end

  fun tilc(mapfile : string, args : string list) =
    case args of 
      [] => error ("No arguments specified.")
    | (("-h"::args)|("-?"::args)) => if args = [] then help() else
	error ("Invalid arguments: -h or -? must occur by itself.")
    | (["-mkdep", makefile]) =>	Makedep.mkDep(makefile)
    | ("-mkdep"::args) => error ("Incorrect number of files to -mkdep")
    | args => 
	let 
	    val (cs, rs, os, srcs) = getArgs args
	    val mapping = getMapping mapfile
	in
	    if srcs = [] then error ("No source files specified.") else 
		case (cs, rs, os) of
		    (false, NONE, NONE) =>    compileThem(mapping, NONE, SOME "a.out", srcs)
		  | (false, SOME f, NONE) =>  compileThem(mapping, SOME f, NONE, srcs)
		  | (false, NONE, SOME f) =>  compileThem(mapping, NONE, SOME f, srcs)
		  | (false, SOME f, SOME g)=> compileThem(mapping, SOME f, SOME g, srcs)
		  | (true, NONE, NONE) =>     compileThem(mapping, NONE, NONE, srcs)
		  | (true, _, _) =>           error "Cannot specify -c and -o/-r"
	end
    

end


structure TM = Manager(structure Parser = LinkParse
		       structure Elaborator = LinkIl
		       structure Compiler = Til
		       structure Linker = Linker
		       structure Makedep = Makedep)