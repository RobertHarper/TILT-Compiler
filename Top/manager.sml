(*$import MANAGER LinkParse LinkIl Compiler Linker MakeDep OS List SplayMapFn SplaySetFn *)

structure Manager :> MANAGER = 

  struct

    structure Elaborator = LinkIl
    structure Compiler = Til
    structure Parser = LinkParse

    val error = fn x => Util.error "Manager" x
    val chat_ref = Stats.tt("ManagerChat")
    val diag_ref = Stats.ff("ManagerDiag")
    fun chat s = if !chat_ref then (print s; TextIO.flushOut TextIO.stdOut)
		 else ()
    fun diag s = if !diag_ref then (print s; TextIO.flushOut TextIO.stdOut)
		 else ()

    val stat_each_file = Stats.ff("TimeEachFile")
    val cache_context = ref 5
    val stop_early_compiling_sml_to_ui = ref false
    val eager = ref true

    fun chat_imports skip imports =
	let fun f(str,acc) = let val acc = if acc > 80
						then (chat "\n        "; 8)
					    else acc + (size str) + 2
			     in  chat str; chat "  "; acc
			     end
	in  if (!chat_ref) then foldl f skip imports else 0
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
	structure StringSet = SplaySetFn(StringKey)
    end

    (* ---- memoize the result of getting file attributes ---- *)
    local
	datatype stat = ABSENT 
	              | PRESENT of Time.time
	    
	val stats = ref (StringMap.empty : stat StringMap.map)
	    
	fun fetch_stat file =
	    let val exists = 
		((OS.FileSys.access(file, []) andalso
		  OS.FileSys.access(file, [OS.FileSys.A_READ]))
		 handle _ => (print ("Warning: OS.FileSys.access " ^ 
				     file ^ "\n"); false))
	    in  if exists
		    then (PRESENT(OS.FileSys.modTime file)
			  handle _ => (print "Warning: OS.FileSys.modTime raised exception\n"; ABSENT))
		else ABSENT
	    end
	
	fun fetch file = 
	    (case StringMap.find(!stats,file) of
		 NONE => let val stat = fetch_stat file
		       in  (stats := (StringMap.insert(!stats,file,stat));
                            stat)
			 end
	       | SOME stat => stat)
    in
	fun reset_stats() = (stats := StringMap.empty)
	    
	fun forget_stat file = 
            (stats := #1 (StringMap.remove(!stats, file))
	     handle _ => ())
	    
	fun exists file = (case fetch file of
			       ABSENT => ((*print (file ^ " does not exist\n");*)
					  false)
			     | PRESENT _ => ((*print (file ^ " exists\n");*)
					     true))
	    
	fun modTime file = (case fetch file of
				ABSENT => error ("Getting modTime on non-existent file " ^ file)
			      | PRESENT t => t)
	    
    end

  (* ------------ reading the import/include list of a file -------------*)
  (* Takes a string(line) and splits into white-space separted fields.
     Inclusively drops all fields after the first field(non-empty string) 
       that passes dropper. *)

  fun split_line dropper line = 
       let val fields = String.fields Char.isSpace line
	   fun filter [] = []
	     | filter (x::y) = if size x = 0 then 
                                  filter y
                               else if dropper x then
                                  []
                               else
                                  x :: (filter y)
       in  
           filter fields
       end


  fun parse_depend depend_str failure file =
      let val ins = TextIO.openIn file
	  val line = TextIO.inputLine ins
	  val sz = size line
	  val _ = TextIO.closeIn ins
	  val depend_str_sz = size depend_str
      in  if (sz >= depend_str_sz andalso 
              String.substring(line,0,depend_str_sz) = depend_str) then
	     let 
                 fun dropper s = ("(*"; s = "*)")
	     in  
                split_line dropper 
                      (String.substring(line,depend_str_sz,sz-depend_str_sz))
             end
	  else 
            (print ("Warning: first line of " ^ file ^ 
                    " is not import/include.\n");
  	     print "Calling parser to process.\n";
             failure file)
      end

   fun parse_impl_import file = 
         parse_depend "(*$import" (#3 o Parser.parse_impl) file

   fun parse_inter_include file = 
         parse_depend "(*$include" (#3 o Parser.parse_inter) file


  fun readContextRaw file = 
         let val is = BinIO.openIn file
	     val res = Elaborator.IlContextEq.blastInContext is
	     val _ = BinIO.closeIn is
	 in  
            res
         end
  val readContextRaw = Stats.timer("ReadingContext",readContextRaw)

  fun writeContextRaw (file,ctxt) = 
         let val os = BinIO.openOut file
             val _ = Elaborator.IlContextEq.blastOutContext os ctxt
             val _ = BinIO.closeOut os
         in  
            ()
         end
  val writeContextRaw = Stats.timer("WritingContext",writeContextRaw)


  val addContext = Stats.timer("AddingContext",Elaborator.plus_context)

  type unitname = string
  type filebase = string
  fun base2sml (f : string) = f ^ ".sml"
  fun base2int (f : string) = f ^ ".int"
  fun base2ui (f : string) = f ^ ".ui"
  val base2o = Compiler.base2o
  val base2uo = Compiler.base2uo

  datatype fresh = STALE | FRESH_INTER | FRESH_IMPL
  local
      val unitlist = ref ([] : unitname list)

      datatype unitinfo = 
         UNIT of {position : int,
		  filebase : filebase,
		  imports_base  : unitname list option ref,
		  includes_base : unitname list option ref,
		  imports  : unitname list option ref,
		  imports_link  : unitname list option ref,
		  includes : unitname list option ref,
		  fresh : fresh ref,
		  context : (int * Elaborator.context) option ref}

      val mapping = ref (StringMap.empty : unitinfo StringMap.map)

      fun find_unit unitname = StringMap.find(!mapping,unitname)
      fun lookup unitname =
	  (case (find_unit unitname) of
	       NONE => error ("unit " ^ unitname ^ " missing")
	     | SOME entry => entry)

  in  
      fun tick_cache() = let fun apper (UNIT{context=r as (ref(SOME(i,ctxt))),...}) = 
	                         if (i<=1) then r := NONE else r := SOME(i-1,ctxt)
			       | apper _ = ()
  	                 in  StringMap.app apper (!mapping)
                         end
      fun flush_cache() = let fun apper (UNIT{context=r as (ref(SOME(i,ctxt))),...}) = 
	                         r := NONE
			       | apper _ = ()
  	                 in  StringMap.app apper (!mapping)
                         end
      fun reset_mapping() = (unitlist := [];
			     mapping := StringMap.empty)

      fun list_units () = rev(!unitlist)

      fun add_unit (pos,unitname,filebase) = 
	  case (find_unit unitname) of
	      NONE => let val newentry = UNIT{position = pos,
					  filebase = filebase,
					  imports_base = ref NONE,
					  includes_base = ref NONE,
					  imports = ref NONE,
                                          imports_link = ref NONE,
					  includes = ref NONE,
					  fresh = ref STALE,
					  context = ref NONE}
			  val _ = mapping :=  StringMap.insert(!mapping,unitname,newentry)
			  val _ = unitlist := unitname::(!unitlist)
		      in  ()
		      end
	    | SOME _ => error ("unit " ^ unitname ^ " already present")

      fun get_base unit = 
            let val UNIT{filebase,...} = lookup unit
            in filebase end

      fun get_fresh unit = 
            let val UNIT{fresh,...} = lookup unit
            in fresh end

      fun get_import_direct unit = 
	  let val UNIT{imports_base=r,filebase,...} = lookup unit
	  in  (case !r of
	        SOME result => result
	      | NONE => let val result = parse_impl_import(base2sml filebase)
		        in   (r := SOME result; result)
                        end)
          end

      fun get_include_direct unit = 
	  let val UNIT{includes_base=r,filebase,...} = lookup unit
	  in  (case !r of
	        SOME result => result
	      | NONE => let val result = parse_inter_include(base2int filebase)
		        in   (r := SOME result; result)
                        end)
          end

      fun get_import unit = 
            let val UNIT{imports,...} = lookup unit
            in imports end

      fun get_imports_link unit = 
            let val UNIT{imports_link,...} = lookup unit
            in imports_link end

      fun get_include unit =
            let val UNIT{includes,...} = lookup unit
            in includes end

      fun get_context unit = 
            let val UNIT{context,...} = lookup unit
            in context end

      fun get_position unit = 
            let val UNIT{position,...} = lookup unit
            in position end
  end


  fun readContext unit = 
      let val r = get_context unit
      in  (case !r of
	       SOME (i,ctxt) => (r := SOME(Int.min(i+2,!cache_context),ctxt); 
		                 (true,ctxt))
	     | NONE => let val uifile = base2ui (get_base unit)
			   val ctxt = readContextRaw uifile
			   val _ = if (!cache_context>0) 
                                   then r:=SOME(2,ctxt) else ()
		       in  (false,ctxt)
		       end)
      end

  fun writeContext (unit,ctxt) = 
       let val r = get_context unit
(*	   val _ = if (!cache_context>0) then r := SOME(2,ctxt) else () *)
	   val uifile = base2ui (get_base unit)
       in  forget_stat uifile;
           writeContextRaw(uifile,ctxt)
       end


  fun getContext imports = 
      let 
	  val _ = Name.reset_varmap()
	  val _ = tick_cache()
          val cached_ctxts = map readContext imports
	  val ctxts = map #2 cached_ctxts
	  val uncached = List.mapPartial (fn (imp,(false,_)) => SOME imp
	                                   | _ => NONE) (Listops.zip imports cached_ctxts)
	  val _ = (chat "  [These imports were not cached: ";
		   chat_imports 30 uncached;
		   chat "]\n")

	  val initial_ctxt = Elaborator.initial_context()
	  val context = addContext (initial_ctxt :: ctxts)
	  val _ = chat ("  [Added contexts.]\n")
      in  context
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
	    let val _ =      (chat ("  [writing " ^ uiFile);
			      writeContext (unit, new_ctxt);
			      chat "]\n")
	    in (new_ctxt,sbnd_entries)
	    end
         | NONE => error("File " ^ sourcefile ^ " failed to elaborate.")


  local

  (* getImportTransitive:
   given a unit, find all the imports it depends on with leaves listed first;
   if the interface file is present, search the includes
   if the interface file is absent, search the imports *)
  fun getImportTr' use_imp linking unitname seenunits : unitname list =
     let val filebase = get_base unitname
	 val impref = if linking then (get_imports_link unitname) else (get_import unitname)
	 val incref = get_include unitname
	 val smlfile = base2sml filebase
	 val intfile = base2int filebase
	 val int_exists = exists intfile
     in  (case (use_imp orelse (not int_exists), !impref, !incref) of
	  (true, SOME i,_) => i (* cached implementation imports *)
	| (false,_, SOME i) => i (* cached interface includes *)
	| _ =>
	      let fun folder(import,acc) = 
		      let val depends = getImportTr' linking
                                          linking import 
                                          (import::seenunits)
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
  in

     fun getImportTr use_imp unitname = 
           getImportTr' use_imp false unitname [unitname]

     fun getImportTr_link unitname =
           getImportTr' true true unitname [unitname]

  end

  (* ----- get_latest ----- *)
  fun get_latest [] = (NONE, Time.zeroTime)
    | get_latest (f::fs) = 
      let
         val recur_result as (_,fstime) = get_latest fs 
         val ftime = modTime f
      in
         if (Time.>=(ftime, fstime)) then (SOME f, ftime) else recur_result
      end


  val depth = ref 0
  fun space 0 = ()
    | space n = (print " "; space (n-1))
  fun push_tab() = (print (Int.toString (!depth));
		    space (2 * (!depth)); 
		    depth := !depth + 1)
  fun pop() = (depth := !depth - 1)

  (* INVARIANT:  If there is a .int file, the .ui file is already fresh *)
  (* If make_uo is false, force the .ui file to be fresh
     otherwise, force the .ui and .uo files to be fresh *)
  fun compileSML make_uo unitname : unit = 
      let val _ = if (!diag_ref) then 
                     (push_tab(); 
                      print "compileSML: "; 
                      print unitname; 
                      print "\n") 
                  else ()

	  val fresh = get_fresh unitname

	  val _ = (case !fresh of
		       STALE       => (compileSML' make_uo unitname; 
                                       fresh := FRESH_IMPL)
		     | FRESH_INTER => (compileSML' make_uo unitname; 
                                       fresh := FRESH_IMPL)
		     | FRESH_IMPL => ())

	  val _ = if (!diag_ref) then pop() else ()
      in  ()
      end

  (* INVARIANT:  If there is a .int file, the .ui file is already fresh *)
  and compileSML' make_uo unitname = 
      let val sourcebase = get_base unitname
	  val intfile = base2int sourcebase
	  val smlfile = base2sml sourcebase
	  val uofile = base2uo sourcebase
	  val ofile = base2o sourcebase
	  val uifile = base2ui sourcebase


	  val direct_imports = get_import_direct unitname
	  val direct_imports_base = map get_base direct_imports
          val direct_imports_ui = map base2ui direct_imports_base

	  (* Call getImportTr to obtain all imports, possibly detecting cycles *)
	  val all_imports = getImportTr true unitname
	  val all_imports_base = map get_base all_imports
	  val all_imports_ui = map base2ui all_imports_base

          (* make sure all imports are fresh *)
	  val _ = app (compile false) direct_imports

	  val smldate = modTime smlfile

	  val (latest_import_file, latest_import_time) = 
                get_latest direct_imports_ui

          val nonconstrained = not (exists intfile)
          val dest_ui_exists = exists uifile
          val dest_uo_exists = exists uofile
          val dest_o_exists = exists ofile
 
          val sml_changed = 
              if make_uo then
                (dest_ui_exists andalso
                 dest_uo_exists andalso 
                 dest_o_exists andalso
                 (Time.<(modTime uofile, smldate) orelse
                  Time.<(modTime ofile, smldate) orelse
                  (nonconstrained
                   andalso Time.<(modTime uifile, smldate))))
              else
                (nonconstrained andalso
                 dest_ui_exists andalso
                 Time.<(modTime uifile, smldate))

          val constraint_changed = 
              (make_uo andalso
               dest_uo_exists andalso
               dest_o_exists andalso
               not (nonconstrained) andalso
               (Time.<=(modTime uofile, modTime uifile) orelse
                Time.<=(modTime ofile, modTime uifile) orelse
                Time.<=(modTime uofile, modTime intfile) orelse
                Time.<=(modTime ofile, modTime intfile)))

          val import_changed = 
              if make_uo then
                (dest_uo_exists andalso
                 dest_o_exists andalso
                 (Time.<(modTime ofile, latest_import_time) orelse
                  Time.<(modTime uofile, latest_import_time)))
              else
                (dest_ui_exists andalso
                 Time.<(modTime uifile, latest_import_time))

          val fresh =           
             if (not dest_ui_exists) then
                (chat ("  [Compiling "^ smlfile ^ " because " ^ 
                      uifile ^ " is missing.]\n");
                 false)
             else if (make_uo andalso (not dest_uo_exists)) then
                (chat ("  [Compiling "^ smlfile ^ " because " ^ 
                      uofile ^ " is missing.]\n");
                 false)
             else if (make_uo andalso (not dest_o_exists)) then
                (chat ("  [Compiling "^ smlfile ^ " because " ^ 
                      ofile ^ " is missing.]\n");
                 false)
             else if sml_changed then
                (chat ("  [Compiling "^ smlfile ^ " because " ^
                      smlfile ^ " newer than objects or interface.]\n");
                 false)
             else if constraint_changed then
                (chat ("  [Compiling "^ smlfile ^ " because " ^
                      uifile ^ " newer than object files.]\n");
                 false)
             else if import_changed then
                (chat ("  [Compiling "^ smlfile ^ " because " ^
                      (valOf latest_import_file) ^ " changed.]\n");
                 false)
             else 
                true

	  val _ = if fresh
		      then diag ("  [" ^ smlfile ^ " is up-to-date]\n")
		  else (chat ("  [" ^ smlfile ^ " has imports: ");
			chat_imports 30 all_imports;
			chat "]\n";
			compileSML'' (unitname, all_imports, make_uo);
                        diag "returning from compileSML'\n")
      in  ()
      end

  (* generates a .ui file, and optionally a .uo file *)
  and compile make_uo unitname =
      let val sourcebase = get_base unitname
	  val source_sml = base2sml sourcebase
	  val source_int = base2int sourcebase
          val full_compile = !eager orelse make_uo
      in  case (full_compile, exists source_int, exists source_sml) of
	  (true, true, true)   => (compileINT unitname; 
                                   compileSML true unitname)
	| (false, true, true)  => compileINT unitname
	| (false, true, false) => compileINT unitname
	| (_, false, true)     => compileSML full_compile unitname
        | (true, _, false) => error ("Missing " ^ source_sml ^
                                     ": cannot generate .uo")
	| _ => error ("Missing " ^ source_sml ^ " and " ^ source_int ^ 
                       ": cannot generate .ui")
      end

  and compileSML'' (unit, imports, make_uo) : unit = 
      let val _ = if (!stat_each_file)
		      then Stats.clear_stats()
		  else ()
	  val srcBase = get_base unit
	  val smlfile = base2sml srcBase

	  val ctxt = getContext imports

	  val _ = chat ("\n  [Parsing " ^ smlfile ^ "]\n")
	  val (lines,fp, _, dec) = Parser.parse_impl smlfile
	  val _ = if (lines > 1000) then flush_cache() else ()

	  val import_bases = map get_base imports
	  val import_uis = List.map (fn x => (x, Linker.Crc.crc_of_file (x^".ui"))) import_bases
	  val uiFile = srcBase ^ ".ui"
	  val intFile = srcBase ^ ".int"

	  val (ctxt',sbnds) = 
	      if exists intFile then 
		  let val _ = compileINT unit 
		      val (_,fp2, _, specs) = Parser.parse_inter intFile
		      val _ = chat ("  [Elaborating " ^ smlfile ^ " with constraint]\n"  )
		  in elab_constrained(ctxt,smlfile,fp,dec,fp2,specs,Time.zeroTime)
		  end
	      else let val _ = chat ("  [Elaborating " ^ smlfile ^ " non-constrained]\n")
		   in elab_nonconstrained(unit,ctxt,smlfile,fp,dec,uiFile,Time.zeroTime)
		   end

	  val _ = chat ("  [Compiling to object file ...")
	  val oFile = Compiler.compile(ctxt, srcBase, sbnds, ctxt')  (* generates oFile *)
	  val _ =  chat "]\n"

	  val crc = Linker.Crc.crc_of_file uiFile
	  val exports = [(srcBase, crc)]
	  val _ = chat ("  [Creating .uo file ...")
	  val uoFile = Linker.mk_uo {imports = import_uis,
				     exports = exports,
				     base_result = srcBase}

          val _ = (forget_stat uoFile; forget_stat uiFile)
	  val _ = chat "]\n"
	  val _ = OS.Process.system ("size " ^ oFile)
	  val _ = if (!stat_each_file)
		      then Stats.print_timers()
		  else ()
      in  ()
      end


  and compileINT unitname =
      let val _ = 
             if (!diag_ref) then 
                (push_tab(); 
                 print "compileINT: "; 
                 print unitname; 
                 print "\n") 
             else ()

	  val fresh = get_fresh unitname

	  val _ = (case !fresh of
		       STALE => (compileINT' unitname; fresh := FRESH_INTER)
		     | FRESH_INTER => ()
		     | FRESH_IMPL => ())

	  val _ = if (!diag_ref) then pop() else ()

      in 
        () 
      end

  and compileINT' unitname =
      let val sourcebase = get_base unitname
	  val sourcefile = base2int sourcebase
          val uifile     = base2ui sourcebase
          val direct_includes = get_include_direct unitname
          val _ = app (compile false) direct_includes

	  val includes_base = map get_base direct_includes
          val includes_ui = map base2ui includes_base
          val (latest_include_file, latest_include_time) = 
                  get_latest includes_ui

          val dest_ui_exists = exists uifile

      in
          if (dest_ui_exists andalso
              Time.>=(modTime uifile, modTime sourcefile) andalso
              Time.>=(modTime uifile, latest_include_time)) then
            (* uifile is up-to-date.  Do not touch this file. *)
            ()
          else
            let
                val _ = (chat ("  [Compiling " ^ sourcefile ^ " because ");
                         (case (dest_ui_exists, latest_include_file)  of
                            (false,_) => chat (uifile ^ " is missing.]\n")
                          | (_,SOME f) => chat (f ^ " has changed.]\n")
                          | (_,NONE) => chat (sourcefile ^" has changed.]\n")))

                val all_includes = getImportTr false unitname

                val ctxt = getContext all_includes

	        val (_,fp, _, specs) = Parser.parse_inter sourcefile

	      in  (case Elaborator.elab_specs(ctxt, fp, specs) of
		       SOME ctxt' => (chat ("  [Writing " ^ uifile);
                                      writeContext (unitname, ctxt');
                                      chat "]\n")
		     | NONE => error("File " ^ sourcefile ^ 
                                     " failed to elaborate."))
	      end
      end

  val flags = (ref false, ref false, ref false, ref false) (* c, r ,o, all *)

  fun resetFlags () = (#1(flags) := false;
		       #2(flags) := false;
		       #3(flags) := false;
		       #4(flags) := false)




  (* exeopt  - if present, names the final executable 
     srcs    - unit names *)

  fun compileThem(exeopt, units) = 
      let val _ = app (compile true) units
          val unit_set = 
               List.foldl 
                 (fn (next, set) => 
                     let val import_tr = getImportTr_link next
			 val next_pos = get_position next
			 fun check import = if (get_position import < next_pos) then ()
			                    else 
						error ("Mapfile file ordering is inconsistent because " ^
							next ^ " imports " ^ import ^ " but precedes it.")
			 val _ = app check import_tr
			 val _ = if (!diag_ref)
				     then (print "Imports for ";
					   print next;
					   print " are:\n   ";
					   app (fn s => (print s; print " ")) import_tr;
					   print "\n")
				 else ()
		     in StringSet.add(StringSet.addList (set, import_tr), next)
                     end)
                 (StringSet.empty)
                 units

	  val units = List.filter (fn unit => (StringSet.member(unit_set,unit))) units

      in  (case exeopt of
	       NONE => ()
	     | SOME out => 
		   let fun mapper unit = 
		       let val base = get_base unit
		       in  {unit=unit, base=base,
			    uiFile=base2ui base, uoFile=base2uo base, oFile=base2o base}
		       end
		       val packages = map mapper units
		   in  (print "Manager calling linker with: ";
			app (fn s => (print s; print " ")) units;
			print "\n";
			Linker.mk_exe {units = packages, exe_result = out})
		   end)
      end
       
  fun setMapping mapFile =
      let val _ = if (exists mapFile)
		      then ()
		  else error "Cannot read map file"
	  val is = TextIO.openIn mapFile
	  val _ = reset_mapping()
	  fun fetch_line n = let fun dropper s = String.sub(s,0) = #"#"
				 val line = TextIO.inputLine is
			     in  case (split_line dropper line) of
				 [unitname, filebase] => add_unit (n,unitname, filebase)
			       | [] => ()
			       | _ => error ("ill-formed map line: " ^ line)
			     end
	  fun loop n = if (TextIO.endOfStream is)
			    then TextIO.closeIn is
			else (fetch_line n;loop(n+1))
      in  loop 0
      end


  fun tilc(mapfile : string, cs : bool,
	   os : string option, srcs : string list) =
	let val _ = if !(Stats.tt "Reset stats between calls") then Stats.clear_stats() else ()
	    val _ = reset_stats()
	    val _ = (depth := 0)
	    val _ = setMapping mapfile
	    val srcs = if srcs = [] 
			   then list_units()
		       else srcs
	    val default_exe = (List.last srcs) ^ ".exe"
	in  (case (cs, os) of
		    (false, NONE)   => compileThem(SOME default_exe, srcs)
		  | (false, SOME f) => compileThem(SOME f, srcs)
		  | (true,  NONE)   => compileThem(NONE, srcs)
		  | (true,  SOME _) => error "Cannot specify -c and -o");
	    Stats.print_stats()
	end


  fun purge(mapfile : string) =
      let val _ = setMapping mapfile
	  val units = list_units()
	  fun remove unit = 
	      let val base = get_base unit
		  val ui = base2ui base
		  val uo = base2uo base
		  val obj = base2o base
		  val command = "rm -f " ^ ui ^ "; rm -f " ^ ui ^ "; rm -f " ^ obj
	      in  (Util.system command; ())

	      end
      in  app remove units
      end

  fun buildRuntime rebuild = 
      let val command = if rebuild then "cd Runtime; gmake purge; gmake runtime"
			else "cd Runtime; gmake runtime"
      in  if Util.system command then () else error "Error in building runtime"
      end

  (* getArgs:
     Takes a list of string arguments and returns a 4-tuple.
     The 1st is the mapfile.
     The 2nd indicates whether the -c flag is present.
     The 3rd carries the name of the -o filename (final executable), if present.
     The 4th component is a list of the source files to process. *)

  fun getArgs (args : string list) :  string option * bool * string option * bool * string list = 
      let
	  fun loop args (acc as (mapFile, hasC, oFile, hasAll, srcs)) = 
	      case args of
		  [] => acc
		| ("-c"::rest) => if hasC 
				      then  error ("Two -c switches not allowed.")
				  else loop rest (mapFile, true, oFile, hasAll, srcs)
		| ("-all"::rest) => if hasAll
				      then error ("Two -all switches not allowed.")
				    else if (null srcs)
					     then loop rest (mapFile, hasC, oFile, true, srcs)
					 else error "-all switch given but also units"
		| ["-o"] => error "No output file specified for -o switch." 
		| ("-o" :: file :: rest) => 
				      (case oFile of
					   NONE => loop rest (mapFile, hasC, SOME file, hasAll, srcs)
					 | SOME _ => error "Output file already specified")
		| ["-m"] => error "No output file specified for -m switch." 
		| ("-m" :: file :: rest) => 
				      (case mapFile of
					   NONE => loop rest (SOME file, hasC, oFile, hasAll, srcs)
					 | SOME _ => error "Output file already specified")
		| (src::rest) => if hasAll
				     then error "-all switch given and also unts"
				 else loop rest (mapFile, hasC, oFile, hasAll, src::srcs)
      in  (resetFlags(); loop args (NONE, false, NONE, false, []))
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
	let val (mapFile, hasC, oFile, hasAll, srcs) = getArgs args
	    val mapFile = (case mapFile of
			       NONE => "mapfile"
			     | SOME mfile => mfile)
	    val srcs = if hasAll then [] else srcs
	in (tilc(mapFile, hasC, oFile, srcs); 0)
	end

end

