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
  val up_to_elaborate = ref false
  val stat_each_file = ref false

  structure Basis = Elaborator.Basis
(*  structure UIBlast = mkBlast(type t = Elaborator.context) *)

  val error = fn x => Util.error "Manager" x
  fun access arg = OS.FileSys.access arg handle _ => false

  val chat_ref = ref true
  val diag_ref = ref false
  fun chat s = if !chat_ref then (print s; TextIO.flushOut TextIO.stdOut)
	       else ()
  val chat_ref = ref true
  fun diag s = if !diag_ref then (print s; TextIO.flushOut TextIO.stdOut)
	       else ()

  fun chat_imports skip imports =
      let fun f(i,n) = (chat i; chat "  ";
			if (n > 0 andalso n mod 8 = 0) then chat "\n     " else (); n+1)
      in  foldl f skip imports
      end

  fun help() = print "This is TILT - no help available.\n"
  fun readContext file = let val is = BinIO.openIn file
			     val res = Elaborator.IlContext.blastInContext is
			     val _ = BinIO.closeIn is
			 in  res
			 end
  fun writeContext (file,ctxt) = let val os = BinIO.openOut file
				     val _ = Elaborator.IlContext.blastOutContext os ctxt
				     val _ = BinIO.closeOut os
				 in  ()
				 end
  val readContext = Stats.timer("ReadingContext",readContext)
  val writeContext = Stats.timer("WritingContext",writeContext)
  val addContext = Stats.timer("AddingContext",Elaborator.plus_context)

  type unitname = string
  type filebase = string
  local
      datatype mapping = Map of ((unitname * (filebase * unitname list option ref * 
					      unitname list option ref * bool ref)) list) ref
      fun find_unit (Map (ref entries)) unitname = Listops.assoc(unitname,entries)
  in  type mapping = mapping
      fun make_mapping() = Map(ref [])
      fun add_unit (m as Map (r as (ref entries))) (unitname,filebase) = 
	  case (find_unit m  unitname) of
	      NONE => r := (unitname,(filebase,ref NONE, ref NONE, ref false))::entries
	    | SOME _ => error ("unit " ^ unitname ^ " already present")
      fun lookup_unit m unitname =
	  (case (find_unit m  unitname) of
	       NONE => error ("unit " ^ unitname ^ " missing")
	     | SOME entry => entry)
      fun list_units (Map (ref data)) = rev(map #1 data)
  end

  fun name2base (mapping : mapping) (name : string) = 
      #1(lookup_unit mapping name)

  fun base2ui (f : string) = f ^ ".ui"
  fun base2uo (f : string) = f ^ ".uo"
  fun base2sml (f : string) = f ^ ".sml"
  fun base2int (f : string) = f ^ ".int"


  fun getContext imports = 
      let val (ctxt_inline,_,_,ctxt_noninline) = Basis.initial_context()
(*	  val ctxts = List.map (fn file => UIBlast.blastIn (base2ui file)) imports *)
	  val ctxts = List.map (fn file => readContext (base2ui file)) imports
	  val _ = chat ("  [Adding contexts now]\n")
	  val context_basis = addContext (ctxt_inline :: ctxts)
	  val context = addContext ctxts
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

  fun elab_constrained(ctxt,sourcefile,fp,dec,fp2,specs,least_new_time) =
      let 
      in case Elaborator.elab_dec_constrained(ctxt, fp, dec, fp2,specs) of
	      SOME sbnds_entries_ctxt => sbnds_entries_ctxt 
            | NONE => error("File " ^ sourcefile ^ " failed to elaborate.")
      end

  fun elab_nonconstrained(pre_ctxt,sourcefile,fp,dec,uiFile,least_new_time) =
      case Elaborator.elab_dec(pre_ctxt, fp, dec)
	of SOME(sbnd_entries, new_ctxt) => 
	    let	val same = 
		(access(uiFile, [OS.FileSys.A_READ]) andalso
		 Elaborator.eq_context(new_ctxt, readContext uiFile))
		val _ = if same 
			    then (if Time.<(OS.FileSys.modTime uiFile, least_new_time)
				      then 
					  (print "OS.FileSys.setTime does not seem to work: conservatively using current time for now!\n";
					   OS.FileSys.setTime(uiFile, NONE))
(* OS.FileSys.setTime(uiFile, SOME least_new_time) *)
				  else ())
			else (chat ("[writing " ^ uiFile);
			      writeContext(uiFile, new_ctxt);
			      chat "]\n")
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


  fun parse_depend depend_str failure file =
      let val ins = TextIO.openIn file
	  val line = TextIO.inputLine ins
	  val sz = size line
	  val _ = TextIO.closeIn ins
	  val depend_str_sz = size depend_str
      in  if (sz >= depend_str_sz andalso String.substring(line,0,depend_str_sz) = depend_str)
	      then    
		  let fun dropper s = ("(*"; s = "*)")
		  in  split_line dropper (String.substring(line,depend_str_sz,sz-depend_str_sz))
                  end
	  else (print ("Warning: first line of " ^ file ^ " is not import/include.\n");
  	        print "Calling parser to process.\n";
		failure file)
      end

   fun parse_impl_import file = parse_depend "(*$import" (#2 o Parser.parse_impl) file
   fun parse_inter_include file = parse_depend "(*$include" (#2 o Parser.parse_inter) file

  (* getImport:
   given a unit, find all the imports it depends on with leaves listed first;
   if the interface file is present, search the includes
   if the interface file is absent, search the imports *)
  fun getImport use_imp mapping unitname seenunits : unitname list =
      (case (use_imp,lookup_unit mapping unitname) of
	  (true,(_,ref (SOME result),_,_)) => result  (* cached implementation imports *)
	| (false,(_,_,ref (SOME result),_)) => result (* cached interface includes *)
	| (_,(sourcebase,r_imp,r_int,_)) =>
	      let val smlfile = base2sml sourcebase
		  val intfile = base2int sourcebase
		  fun folder(import,acc) = 
		      let val depends = getImport false mapping import (import::seenunits)
			  fun adder(u, ac) = 
			      if Listops.member(u,ac)
				  then ac else u::ac
		      in  adder(import,foldl adder acc depends)
		      end
		  fun check_loop imports = 
		      app (fn imp => if (Listops.member(imp,seenunits))
					 then error ("Loop detected in: " ^
						     foldr (fn (a,b) => (a ^ " " ^ b)) "" seenunits)
				     else ()) imports
	      in  if (not use_imp andalso access(intfile, [OS.FileSys.A_READ]))
		      then let val _ = diag ("  [Scanning " ^ intfile ^ " for includes]\n")
			       val imports = parse_inter_include intfile
			       val _ = check_loop imports
			       val result = rev(foldl folder [] imports)
			       val _ = r_int := SOME result
			   in  result
			   end
		  else 
		      let val _ = diag ("  [Scanning " ^ smlfile ^ " for imports]\n")
			  val imports = parse_impl_import smlfile
			  val _ = check_loop imports
			  val result = rev(foldl folder [] imports)
			  val _ = r_imp := SOME result
		      in  result
		      end
	      end)

  fun is_fresh files (curdate,curfile) = 
      let fun folder f = 
	      let val t = OS.FileSys.modTime f
		  handle OS.SysErr _ => error (f ^ "not present")
	      in  Time.<=(t,curdate)
		  orelse
		  (chat ("  [" ^ f ^ " is newer than " ^ curfile ^ "]\n"); false)
	      end
      in  Listops.andfold folder files
      end
	  
  fun  compileSML make_uo mapping unitname : unit = 
       if !(#4(lookup_unit mapping unitname))
	   then ()
       else compileSML' make_uo mapping unitname 

  and compileSML' make_uo mapping unitname : unit = 
      let val sourcebase = name2base mapping unitname
	  val smlfile = base2sml sourcebase
	  val uofile = base2uo sourcebase
	  val uifile = base2ui sourcebase
	  val intfile = base2int sourcebase

	  val imports = getImport true mapping unitname []
	  val import_bases = map (name2base mapping) imports

          (* work on imports *)
	  val _ = app (compile false mapping) imports
	  val imports_ui = map base2ui import_bases

	  val smldate = OS.FileSys.modTime smlfile
	  val least_new_time = foldl (fn (f,t) => let val t' = OS.FileSys.modTime f
						  in  if (Time.<(t,t')) then t' else t
						  end) smldate imports_ui
	  fun is_fresh (curdate,curfile) = 
	      let val files = smlfile :: imports_ui
		  fun folder f = 
		      let val t = OS.FileSys.modTime f
			  handle OS.SysErr _ => error (f ^ "not present")
		      in  Time.<=(t,curdate)
			  orelse
			  (chat ("  [" ^ f ^ " is newer than " ^ curfile ^ "]\n"); false)
		      end
	      in  Listops.andfold folder files
	      end
	  val fresh = (access(uofile, [OS.FileSys.A_READ]) andalso
		       is_fresh(OS.FileSys.modTime uofile, uofile) andalso
		       (access(intfile, [OS.FileSys.A_READ]) orelse
			(access(uifile, [OS.FileSys.A_READ]) andalso
			 is_fresh(OS.FileSys.modTime uifile, uifile))))
	  val _ = if fresh
		      then diag ("  [" ^ uofile ^ " is up-to-date]\n")
		  else (chat ("  [" ^ smlfile ^ " has imports: ");
			chat_imports 4 imports;
			chat "]\n";
			compileSML'' mapping (unitname, imports, least_new_time))
      in  (#4(lookup_unit mapping unitname)) := true
      end
  
  and compile make_uo mapping unit =
      let val sourcebase = name2base mapping unit
	  val source_sml = base2sml sourcebase
	  val source_int = base2int sourcebase
      in  case (make_uo,
		access(source_int, [OS.FileSys.A_READ]),
		access(source_sml, [OS.FileSys.A_READ])) of
	  (true, true, true) => (compileINT mapping unit;
				 compileSML make_uo mapping unit)
	| (false, true, true) => compileINT mapping unit
	| (_,true, _) => compileINT mapping unit
	| (_, _, true) => compileSML make_uo mapping unit
	| _ => error ("Missing " ^ source_sml ^ " and " ^ source_int ^ ": cannot generate .ui")
      end

  and compileSML'' mapping (unit, imports, least_new_time) : unit = 
      let val _ = if (!stat_each_file)
		      then Stats.clear_stats()
		  else ()
	  val srcBase = name2base mapping unit
	  val smlfile = base2sml srcBase
	  val _ = chat ("  [Parsing " ^ smlfile ^ "]\n")
	  val (fp, _, dec) = Parser.parse_impl smlfile
	  val _ = (chat "  [Creating context from imports: ";
		   chat_imports 4 imports;
		   chat "]\n")
	  val import_bases = map (name2base mapping) imports
	  val (ctxt_for_elab,ctxt) = getContext import_bases
	  val import_uis = List.map (fn x => (x, Linker.Crc.crc_of_file (x^".ui"))) import_bases
	  val uiFile = srcBase ^ ".ui"
	  val intFile = srcBase ^ ".int"
	  val oFile = srcBase ^ ".o"
	  val uoFile = srcBase ^ ".uo"
	  val (sbnds, ctxt') = 
	      if access(intFile, []) then 
		  let val _ = compileINT mapping unit 
		      val (fp2, _, specs) = Parser.parse_inter intFile
		      val _ = chat ("  [Elaborating " ^ smlfile ^ " with constraint]\n"  )
		  in elab_constrained(ctxt_for_elab,smlfile,fp,dec,fp2,specs,least_new_time)
		  end
	      else let val _ = chat ("  [Elaborating " ^ smlfile ^ " non-constrained]\n")
		   in elab_nonconstrained(ctxt_for_elab,smlfile,fp,dec,uiFile,least_new_time)
		   end
	  val _ = if (!up_to_elaborate)
		      then ()
		  else if (!up_to_phasesplit)
		      then
			  (chat ("  [Just phase-splitting " ^ srcBase ^ " and making dummy .uo]\n");
			   Compiler.pcompile(ctxt, srcBase, sbnds, ctxt'))  (* does not generate oFile *)
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
	  val _ = if (!up_to_elaborate orelse !up_to_phasesplit)
		      then (* create dummy .uo file or update time if it exists *)
			  let val os = TextIO.openOut uoFile
			      val _ = TextIO.output(os,"Dummy .uo file\n")
			  in  TextIO.closeOut os
			  end
		  else ()
	  val _ = chat "]\n"
	  val _ = if (!stat_each_file)
		      then Stats.print_stats()
		  else ()
      in  ()
      end

  (* XXXXXX change to be recursive like compileSML *)
  and compileINT mapping unit =
      let val sourcebase = name2base mapping unit
	  val sourcefile = base2int sourcebase
	  val (fp, _, specs) = Parser.parse_inter sourcefile
	  val includes = getImport false mapping unit [] 
	  val _ = app (compile false mapping) includes
	  val includes_base = map (name2base mapping) includes
	  val includes_uo = map base2uo includes_base
	  val unitName = OS.Path.base(OS.Path.file sourcefile)
	  val uiFile = (OS.Path.base sourcefile) ^ ".ui"
      in if (access(uiFile, [OS.FileSys.A_READ]) andalso
	     is_fresh includes_uo (OS.FileSys.modTime uiFile, uiFile))
	     then ()
	 else let val (ctxt_for_elab,ctxt) = getContext includes_base
		  val _ = (chat "[Compiling specification file: ";
			   chat sourcefile; chat "\n")
	      in  (case Elaborator.elab_specs(ctxt_for_elab, fp, specs) of
		       SOME ctxt' => writeContext(uiFile, ctxt')
		     | NONE => error("File " ^ sourcefile ^ " failed to elaborate."))
	      end
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

  val flags = (ref false, ref false, ref false, ref false) (* c, r ,o, all *)

  fun resetFlags () = (#1(flags) := false;
		       #2(flags) := false;
		       #3(flags) := false;
		       #4(flags) := false)


  (* getArgs:
     Takes a list of string arguments and returns a 4-tuple.
     The 1st is the mapfile.
     The 2nd indicates whether the -c flag is present.
     The 3rd carries the name of the -r filename, if present.
     The 4th carries the name of the -o filename (final executable), if present.
     The 5th component is a list of the source files to process. *)

  fun getArgs (args : string list) :  bool * string option * string option * bool * string list = 
      let
	  fun getArgsH [] = (false, NONE, NONE, false, [])
	    | getArgsH ("-c"::rest) = 
	      let
		  val (cs, rs, os, all, srcs) = getArgsH rest
		  val (seen, _, _, _) = flags
	      in
		  if !seen then error ("Two -c switches not allowed.") else
	 	      (seen := true; (true, rs, os, all, srcs))
	      end
	    | getArgsH ("-all"::rest) = 
	      let
		  val (cs, rs, os, all, srcs) = getArgsH rest
		  val (_, _, _, seen) = flags
	      in
		  if !seen then error ("Two -all switches not allowed.") else
	 	      (seen := true; (true, rs, os, true, srcs))
	      end
	    | getArgsH ("-o"::rest) = if List.length(rest) = 0 then
	      error "No output file specified for -o switch." else 
	      let 
		  val file = hd rest
		  val (cs, rs, os, all, srcs) = getArgsH (tl rest)
		  val (_, _, seen, _) = flags
	      in
		  if (!seen) then error ("Two -o switches not allowed.") else
		      (seen := true; (cs, rs, SOME file, all, srcs))
	      end
	    | getArgsH ("-r"::rest) = if List.length(rest) = 0 then
	      error "No output file specified for -r switch." else 
	      let
		  val file = hd rest
		  val (cs, rs, os, all, srcs) = getArgsH (tl rest)
		  val (_, seen, _, _) = flags
	      in
		  if (!seen) then error ("Two -r switches not allowed.") else
		      (seen := true; (cs, SOME file, rs, all, srcs))
	      end
	    | getArgsH (f::rest) = let
				       val (cs, rs, os, all, srcs) = getArgsH rest
				   in
				       (cs, rs, os, all, f::srcs)
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
	  val uo_args = map (fn u => base2uo(name2base mapping u)) units
      in
	  (case (linkopt,exeopt) of
	       (NONE,NONE) => ()
	     | (NONE, SOME out) => (print "manager calling linker with: ";
				    app (fn s => (print s; print " ")) uo_args;
				    print "\nand with uo_result = ";
				    print tmp_uo; print "\n";
				    Linker.link {uo_args = uo_args, uo_result = tmp_uo};
				    Linker.mk_exe {uo_arg = tmp_uo, exe_result = out};
				    OS.FileSys.remove tmp_uo)
	     | (SOME f, NONE) => Linker.link {uo_args = uo_args, uo_result = f}
	     | (SOME f, SOME out) => (Linker.link {uo_args = uo_args, uo_result = f};
				      Linker.mk_exe {uo_arg = f, exe_result = out}))
      end
       
  fun getMapping mapFile : mapping = 
      let val _ = if (access(mapFile, [OS.FileSys.A_READ]))
		      then ()
		  else error "Cannot read map file"
	  val is = TextIO.openIn mapFile
	  val m = make_mapping()
	  fun fetch_line() = let fun dropper s = String.sub(s,0) = #"#"
				 val line = TextIO.inputLine is
			     in  case (split_line dropper line) of
				 [unitname, filebase] => add_unit m (unitname, filebase)
			       | [] => ()
			       | _ => error ("ill-formed map line: " ^ line)
			     end
	  fun loop () = if (TextIO.endOfStream is)
			    then TextIO.closeIn is
			else (fetch_line();loop())
      in  loop (); m
      end

  fun tilc(mapfile : string, cs : bool, rs : string option, 
	   os : string option, srcs : string list) =
	let val _ = Stats.clear_stats()
	    val mapping = getMapping mapfile
	    val srcs = if srcs = [] 
			   then list_units mapping
		       else srcs
	in  (case (cs, rs, os) of
		    (false, NONE, NONE) =>    compileThem(mapping, NONE, SOME "a.out", srcs)
		  | (false, SOME f, NONE) =>  compileThem(mapping, SOME f, NONE, srcs)
		  | (false, NONE, SOME f) =>  compileThem(mapping, NONE, SOME f, srcs)
		  | (false, SOME f, SOME g)=> compileThem(mapping, SOME f, SOME g, srcs)
		  | (true, NONE, NONE) =>     compileThem(mapping, NONE, NONE, srcs)
		  | (true, _, _) =>           error "Cannot specify -c and -o/-r");
	    Stats.print_stats()
	end

  fun command(env : string, args : string list) : int =
    case args of 
      [] => (print ("No arguments specified.\n"); 1)
    | ["-h"] => (help(); 0)
    | ["-?"] => (help(); 0)
    | ("-h"::_) =>
	  (print  ("Invalid arguments: -h or -? must occur by itself.\n"); 1)
    | ("-?"::_) =>
	  (print  ("Invalid arguments: -h or -? must occur by itself.\n"); 1)
    | (["-mkdep", makefile]) =>	(Makedep.mkDep(makefile); 0)
    | ("-mkdep"::args) => (print ("Incorrect number of files to -mkdep.\n"); 1)
    | args => 
	let val mapfile = "mapfile"
	    val (cs, rs, os, all, srcs) = getArgs args
	    val srcs = if all then [] else srcs
	in (tilc(mapfile, cs, rs, os, srcs); 0)
	end

end


structure TM = Manager(structure Parser = LinkParse
		       structure Elaborator = LinkIl
		       structure Compiler = Til
		       structure Linker = Linker
		       structure Makedep = Makedep)