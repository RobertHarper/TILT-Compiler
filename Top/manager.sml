(*$import MANAGER LINKPARSE LINKIL COMPILER LINKER MAKEDEP OS LinkParse LinkIl Linker MakeDep Compiler List SplayMapFn *)
(* it is touching too many files so it's slow *)
(* should add caching of import scans *)


functor Manager (structure Parser: LINK_PARSE
		 structure Elaborator: LINKIL
		 structure Compiler: COMPILER
		 structure Linker: LINKER
		 structure Makedep: MAKEDEP
		 sharing type Elaborator.sbnd = Compiler.sbnd
		 sharing type Elaborator.context_entry = Compiler.context_entry
		 sharing type Elaborator.context = Compiler.context)
		:> MANAGER = 
struct


  val up_to_phasesplit = ref false
  val up_to_elaborate = ref false
  val stat_each_file = ref false
  val cache_context = ref false

  structure Basis = Elaborator.Basis
(*  structure UIBlast = mkBlast(type t = Elaborator.context) *)

  val error = fn x => Util.error "Manager" x

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


  (* ---- we want to do lookup on strings ----- *)
  local
      structure StringKey = 
	  struct
	      type ord_key = string
	      val compare = String.compare
	  end
  in  structure StringMap = SplayMapFn(StringKey)
  end

  (* ---- memoize the result of getting file attributes ---- *)
  local
      datatype stat = ABSENT | PRESENT of Time.time
      val stats = ref (StringMap.empty : stat StringMap.map)
      fun fetch_stat file =
	  let val exists = (OS.FileSys.access(file, [OS.FileSys.A_READ])
	                    handle _ => (print "Warning: OS.FileSys.access\n"; false))
	  in  if exists
		  then (PRESENT(OS.FileSys.modTime file)
			handle _ => (print "Warning: OS.FileSys.modTime raised exception\n"; ABSENT))
	      else ABSENT
	  end
      fun fetch file = 
	  (case StringMap.find(!stats,file) of
	       NONE => let val stat = fetch_stat file
		       in  (stats := (StringMap.insert(!stats,file,stat)); stat)
		       end
	     | SOME stat => stat)
  in  fun reset_stats() = stats := StringMap.empty
      fun exists file = (case fetch file of
			     ABSENT => false
			   | PRESENT _ => true)
      fun modTime file = (case fetch file of
			     ABSENT => error ("Getting modTime on non-existent file " ^ file)
			   | PRESENT t => t)

  end

  (* ------------ reading the import/include list of a file ------------------------------- *)
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


  fun readContextRaw file = let val is = BinIO.openIn file
			     val res = Elaborator.IlContextEq.blastInContext is
			     val _ = BinIO.closeIn is
			 in  res
			 end
  fun writeContextRaw (file,ctxt) = let val os = BinIO.openOut file
				     val _ = Elaborator.IlContextEq.blastOutContext os ctxt
				     val _ = BinIO.closeOut os
				 in  ()
				 end
  val readContextRaw = Stats.timer("ReadingContext",readContextRaw)
  val writeContextRaw = Stats.timer("WritingContext",writeContextRaw)
  val addContext = Stats.timer("AddingContext",Elaborator.plus_context)

  type unitname = string
  type filebase = string
  fun base2ui (f : string) = f ^ ".ui"
  fun base2uo (f : string) = f ^ ".uo"
  fun base2sml (f : string) = f ^ ".sml"
  fun base2int (f : string) = f ^ ".int"
  datatype fresh = STALE | FRESH_INTER | FRESH_IMPL
  local
      val unitlist = ref ([] : unitname list)
      val mapping = ref (StringMap.empty : 
			 {filebase : filebase,
			  imports_base  : unitname list option ref,
			  includes_base : unitname list option ref,
			  imports  : unitname list option ref,
			  includes : unitname list option ref,
			  fresh : fresh ref,
			  context : Elaborator.context option ref} StringMap.map)

      fun find_unit unitname = StringMap.find(!mapping,unitname)
      fun lookup unitname =
	  (case (find_unit unitname) of
	       NONE => error ("unit " ^ unitname ^ " missing")
	     | SOME entry => entry)
  in  fun reset_mapping() = (unitlist := [];
			     mapping := StringMap.empty)
      fun list_units () = rev(!unitlist)
      fun add_unit (unitname,filebase) = 
	  case (find_unit unitname) of
	      NONE => let val newentry = {filebase = filebase,
					  imports_base = ref NONE,
					  includes_base = ref NONE,
					  imports = ref NONE,
					  includes = ref NONE,
					  fresh = ref STALE,
					  context = ref NONE}
			  val _ = mapping :=  StringMap.insert(!mapping,unitname,newentry)
			  val _ = unitlist := unitname::(!unitlist)
		      in  ()
		      end
	    | SOME _ => error ("unit " ^ unitname ^ " already present")
      fun get_base unit = #filebase(lookup unit)
      val name2base = get_base
      fun get_fresh unit = #fresh(lookup unit)
      fun get_import_direct unit = 
	  let val {imports_base=r,filebase,...} = lookup unit
	  in  (case !r of
	        SOME result => result
	      | NONE => let val result = parse_impl_import(base2sml filebase)
		        in   (r := SOME result; result)
                        end)
          end
      fun get_include_direct unit = 
	  let val {includes_base=r,filebase,...} = lookup unit
	  in  (case !r of
	        SOME result => result
	      | NONE => let val result = parse_inter_include(base2int filebase)
		        in   (r := SOME result; result)
                        end)
          end
      fun get_import unit = #imports(lookup unit)
      fun get_include unit = #includes(lookup unit)
      fun get_context unit = #context(lookup unit)
  end


  fun readContext unit = 
      let val r = get_context unit
      in  (case !r of
	       SOME ctxt => ctxt
	     | NONE => let val uifile = base2ui (name2base unit)
			   val ctxt = readContextRaw uifile
			   val _ = if (!cache_context) then r := SOME ctxt else ()
		       in  ctxt
		       end)
      end
  fun writeContext (unit,ctxt) = 
       let val r = get_context unit
	   val _ = if (!cache_context) then r := SOME ctxt else ()
	   val uifile = base2ui (name2base unit)
       in  writeContextRaw(uifile,ctxt)
       end



  fun getContext imports = 
      let val (ctxt_inline,_,_,ctxt_noninline) = Basis.initial_context()
	  val _ = (chat "  [Creating context from imports: ";
	           chat_imports 4 imports;
	           chat "]\n")
          val ctxts = List.map readContext imports
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
	      SOME ctxt_sbnds_entries => ctxt_sbnds_entries
            | NONE => error("File " ^ sourcefile ^ " failed to elaborate.")
      end

  fun elab_nonconstrained(unit,pre_ctxt,sourcefile,fp,dec,uiFile,least_new_time) =
      case Elaborator.elab_dec(pre_ctxt, fp, dec)
	of SOME(new_ctxt, sbnd_entries) => 
	    let	val same = 
		(exists uiFile andalso
		 Elaborator.IlContextEq.eq_context(new_ctxt, readContext unit))
		val _ = if same 
			    then (if Time.<(modTime uiFile, least_new_time)
				      then 
					  (print "OS.FileSys.setTime does not seem to work: conservatively using current time for now!\n";
					   OS.FileSys.setTime(uiFile, NONE))
(* OS.FileSys.setTime(uiFile, SOME least_new_time) *)
				  else ())
			else (chat ("[writing " ^ uiFile);
			      writeContext (unit, new_ctxt);
			      chat "]\n")
	    in (new_ctxt,sbnd_entries)
	    end
         | NONE => error("File " ^ sourcefile ^ " failed to elaborate.")


  (* getImportTransitive:
   given a unit, find all the imports it depends on with leaves listed first;
   if the interface file is present, search the includes
   if the interface file is absent, search the imports *)
  fun getImportTr use_imp unitname seenunits : unitname list =
     let val filebase = get_base unitname
	 val impref = get_import unitname
	 val incref = get_include unitname
	 val smlfile = base2sml filebase
	 val intfile = base2int filebase
	 val int_exists = exists intfile
     in  (case (use_imp orelse (not int_exists), !impref, !incref) of
	  (true, SOME i,_) => i (* cached implementation imports *)
	| (false,_, SOME i) => i (* cached interface includes *)
	| _ =>
	      let fun folder(import,acc) = 
		      let val depends = getImportTr false import (import::seenunits)
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
	      in  if (not use_imp andalso exists intfile)
		      then let val _ = diag ("  [Scanning " ^ intfile ^ " for includes]\n")
			       val base_includes = get_include_direct unitname
			       val _ = check_loop base_includes
			       val result = rev(foldl folder [] base_includes)
			       val _ = incref := SOME result
			   in  result
			   end
		  else 
		      let val _ = diag ("  [Scanning " ^ smlfile ^ " for imports]\n")
			  val base_imports = get_import_direct unitname
			  val _ = check_loop base_imports
			  val result = rev(foldl folder [] base_imports)
			  val _ = impref := SOME result
		      in  result
		      end
	      end)
     end

  (* ----- get_latest ----- *)
  fun get_latest files =
      let fun folder (f,(missing,curtime)) = 
	  if (exists f)
	      then let val mt = modTime f
		   in  (missing, if (Time.<=(curtime,mt)) then mt else curtime)
		   end
	  else (SOME f, curtime)
      in foldl folder (NONE,Time.zeroTime) files
      end


  val depth = ref 0
  fun space 0 = ()
    | space n = (print " "; space (n-1))
  fun push_tab() = (print (Int.toString (!depth));
		    space (2 * (!depth)); 
		    depth := !depth + 1)
  fun pop() = (depth := !depth - 1)

  fun  compileSML make_uo unitname : unit = 
      let val _ = if (!diag_ref) then (push_tab(); print "compileSML: "; print unitname; print "\n") else ()
	  val fresh = get_fresh unitname
	  val _ = (case !fresh of
		       STALE       => (compileSML' make_uo unitname; fresh := FRESH_IMPL)
		     | FRESH_INTER => (compileSML' make_uo unitname; fresh := FRESH_IMPL)
		     | FRESH_IMPL => ())
	  val _ = if (!diag_ref) then pop() else ()
      in  ()
      end

  and compileSML' make_uo unitname : unit = 
      let val sourcebase = name2base unitname
	  val smlfile = base2sml sourcebase
	  val uofile = base2uo sourcebase
	  val uifile = base2ui sourcebase
	  val intfile = base2int sourcebase

	  val imports_direct = get_import_direct unitname
	  val imports = getImportTr true unitname []
	  val import_bases = map name2base imports

          (* work on imports *)
	  val _ = app (compile false) imports_direct
	  val imports_ui = map base2ui import_bases

	  val smldate = modTime smlfile

	  val (missing,latest_time) = get_latest (smlfile :: imports_ui)

	  val fresh = (case missing of
			   NONE => (exists uofile andalso
				    Time.>=(modTime uofile, latest_time) andalso
				    (Time.<=(modTime smlfile, modTime uofile) orelse
				     exists intfile orelse
				     (exists uifile andalso
				      Time.>=(modTime uifile, latest_time))))
			 | SOME f => (chat ("  [" ^ f ^ " is missing: recompiling\n"); false))
	  val _ = if fresh
		      then diag ("  [" ^ uofile ^ " is up-to-date]\n")
		  else (chat ("  [" ^ smlfile ^ " has imports: ");
			chat_imports 4 imports;
			chat "]\n";
			compileSML'' (unitname, imports, latest_time))
      in  ()
      end
  
  and compile make_uo unit =
      let val sourcebase = name2base  unit
	  val source_sml = base2sml sourcebase
	  val source_int = base2int sourcebase
      in  case (make_uo, exists source_int, exists source_sml) of
	  (true, true, true) => (compileINT unit;
				 compileSML make_uo unit)
	| (false, true, true) => compileINT unit
	| (_,true, _) => compileINT unit
	| (_, _, true) => compileSML make_uo unit
	| _ => error ("Missing " ^ source_sml ^ " and " ^ source_int ^ ": cannot generate .ui")
      end

  and compileSML'' (unit, imports, least_new_time) : unit = 
      let val _ = if (!stat_each_file)
		      then Stats.clear_stats()
		  else ()
	  val srcBase = name2base unit
	  val smlfile = base2sml srcBase
	  val _ = chat ("  [Parsing " ^ smlfile ^ "]\n")
	  val (fp, _, dec) = Parser.parse_impl smlfile
	  val (ctxt_for_elab,ctxt) = getContext imports
	  val import_bases = map name2base imports
	  val import_uis = List.map (fn x => (x, Linker.Crc.crc_of_file (x^".ui"))) import_bases
	  val uiFile = srcBase ^ ".ui"
	  val intFile = srcBase ^ ".int"
	  val oFile = srcBase ^ ".o"
	  val uoFile = srcBase ^ ".uo"
	  val (ctxt',sbnds) = 
	      if exists intFile then 
		  let val _ = compileINT unit 
		      val (fp2, _, specs) = Parser.parse_inter intFile
		      val _ = chat ("  [Elaborating " ^ smlfile ^ " with constraint]\n"  )
		  in elab_constrained(ctxt_for_elab,smlfile,fp,dec,fp2,specs,least_new_time)
		  end
	      else let val _ = chat ("  [Elaborating " ^ smlfile ^ " non-constrained]\n")
		   in elab_nonconstrained(unit,ctxt_for_elab,smlfile,fp,dec,uiFile,least_new_time)
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
					 base_result = srcBase}
		      end
	  val _ = if (!up_to_elaborate orelse !up_to_phasesplit)
		      then (* create dummy .uo file or update time if it exists *)
			  let val os = TextIO.openOut uoFile
			      val _ = TextIO.output(os,"Dummy .uo file\n")
			  in  TextIO.closeOut os
			  end
		  else ()
	  val _ = chat "]\n"
	  val _ = OS.Process.system ("size " ^ oFile)
	  val _ = if (!stat_each_file)
		      then Stats.print_stats()
		  else ()
      in  ()
      end


  and compileINT unitname =
      let val _ = if (!diag_ref) then (push_tab(); print "compileINT: "; print unitname; print "\n") else ()
	  val fresh = get_fresh unitname
	  val _ = (case !fresh of
		       STALE => (compileINT' unitname; fresh := FRESH_INTER)
		     | FRESH_INTER => ()
		     | FRESH_IMPL => ())
	  val _ = if (!diag_ref) then pop() else ()
      in  ()
      end

  and compileINT' unit =
      let val sourcebase = name2base unit
	  val sourcefile = base2int sourcebase
	  val includes = getImportTr false unit [] 
	  val _ = app (compile false) includes
	  val includes_base = map name2base includes
	  val includes_uo = map base2uo includes_base
	  val unitName = OS.Path.base(OS.Path.file sourcefile)
	  val uiFile = (OS.Path.base sourcefile) ^ ".ui"
	  val (missing,latest_time) = get_latest includes_uo
      in if (exists uiFile andalso
	     (case missing of
		  NONE => Time.>=(modTime uiFile, latest_time)
		| SOME _ => false))
	     then ()
	 else let val (ctxt_for_elab,ctxt) = getContext includes
		  val (fp, _, specs) = Parser.parse_inter sourcefile
		  val _ = (chat "[Compiling specification file: ";
			   chat sourcefile; chat "\n")
	      in  (case Elaborator.elab_specs(ctxt_for_elab, fp, specs) of
		       SOME ctxt' => writeContext (unit, ctxt')
		     | NONE => error("File " ^ sourcefile ^ " failed to elaborate."))
	      end
      end

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
  fun compileThem(linkopt, exeopt, units) = 
      let val bases = map name2base  units
	  val _ = app (compile true) units
	  val tmp = OS.FileSys.tmpName()
	  val tmp_uo = tmp ^ ".uo"
	  val base_args = map name2base units
      in
	  (case (linkopt,exeopt) of
	       (NONE,NONE) => ()
	     | (NONE, SOME out) => (print "manager calling linker with: ";
				    app (fn s => (print s; print " ")) base_args;
				    print "\nand with uo_result = ";
				    print tmp_uo; print "\n";
				    Linker.link {base_args = base_args, base_result = tmp};
				    Linker.mk_exe {base_arg = tmp, exe_result = out};
				    OS.FileSys.remove tmp_uo)
	     | (SOME f, NONE) => Linker.link {base_args = base_args, base_result = f}
	     | (SOME f, SOME out) => (Linker.link {base_args = base_args, base_result = f};
				      Linker.mk_exe {base_arg = f, exe_result = out}))
      end
       
  fun setMapping mapFile =
      let val _ = if (exists mapFile)
		      then ()
		  else error "Cannot read map file"
	  val is = TextIO.openIn mapFile
	  val _ = reset_mapping()
	  fun fetch_line() = let fun dropper s = String.sub(s,0) = #"#"
				 val line = TextIO.inputLine is
			     in  case (split_line dropper line) of
				 [unitname, filebase] => add_unit (unitname, filebase)
			       | [] => ()
			       | _ => error ("ill-formed map line: " ^ line)
			     end
	  fun loop () = if (TextIO.endOfStream is)
			    then TextIO.closeIn is
			else (fetch_line();loop())
      in  loop ()
      end

  fun tilc(mapfile : string, cs : bool, rs : string option, 
	   os : string option, srcs : string list) =
	let val _ = Stats.clear_stats()
	    val _ = reset_stats()
	    val _ = setMapping mapfile
	    val srcs = if srcs = [] 
			   then list_units()
		       else srcs
	in  (case (cs, rs, os) of
		    (false, NONE, NONE) =>    compileThem(NONE, SOME "a.out", srcs)
		  | (false, SOME f, NONE) =>  compileThem(SOME f, NONE, srcs)
		  | (false, NONE, SOME f) =>  compileThem(NONE, SOME f, srcs)
		  | (false, SOME f, SOME g)=> compileThem(SOME f, SOME g, srcs)
		  | (true, NONE, NONE) =>     compileThem(NONE, NONE, srcs)
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