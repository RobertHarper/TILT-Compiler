(*$import LINKER Compiler Util Crc Listops OS Name Linkalpha Linksparc Dirs Popen *)
structure Linker :> LINKER =
  struct

    val doConsistent = Stats.tt("doConsistent")
    val debug_asm = Stats.bool("debug_asm")
    val keep_link_asm = Stats.ff("keep_link_asm")
    val error = fn x => Util.error "Linker" x

    type config = {assembler : string list,
		   linker : string list,
		   ldpre : string list,
		   ldpost : string list}

    (* runtimeFile : string -> string *)
    fun runtimeFile path = Dirs.relative (Dirs.getRuntimeDir (Dirs.getDirs()), path)
	
    val gccPath : string Delay.value =
	Delay.delay (fn () =>
		     let val gcc = "gcc"
			 val path = ["/usr/local/bin", "/usr/bin"]
			 val which = Dirs.accessPath (path, [OS.FileSys.A_EXEC])
		     in
			 case which gcc
			   of NONE => gcc
			    | SOME gcc' => gcc'
		     end)

    (* chop : string -> string *)
    fun chop "" = ""
      | chop s = String.extract (s, 0, SOME (size s - 1))

    (* gccFile : string -> string *)
    fun gccFile file = chop (Popen.outputOf (Delay.force gccPath ^ " --print-file-name=" ^ file))

    val sparcConfig : config Delay.value =
	Delay.delay (fn () =>
		     let
			 val _ = Til.checkNative()	(* gccFile only works native *)
		     in
			 {assembler = ["/usr/ccs/bin/as"],
			  linker    = ["/usr/ccs/bin/ld"], (* -L/usr/local/lib ? *)
			  ldpre     = [runtimeFile "obj_solaris/firstdata.o", gccFile "crt1.o", gccFile "crti.o",
				       "/usr/ccs/lib/values-Xa.o", gccFile "crtbegin.o"],
			  ldpost    = ["-L/afs/cs/project/fox/member/pscheng/ml96/SparcPerfMon/lib", 
				       runtimeFile "runtime.solaris.a", "-lperfmon", "-lpthread","-lposix4", "-lgen", "-lm", "-lc",
				       gccFile "libgcc.a", gccFile "crtend.o", gccFile "crtn.o"]}
		     end)
    val alphaConfig : config Delay.value =
	Delay.delay (fn() =>
		     let
			 val debug = if (!debug_asm) then ["-g"] else nil
		     in
			 {assembler = ["/usr/bin/as"],
			  linker    = ["/usr/bin/ld", "-call_shared", "-D", "a000000", "-T", "8000000"] @ debug,
			  ldpre     = ["/usr/lib/cmplrs/cc/crt0.o"],
			  ldpost    = [runtimeFile "runtime.alpha_osf.a", "-lpthread", "-lmach", "-lexc", "-lm", "-lc", "-lrt"]}
		     end)

    (* targetConfig : unit -> config *)
    fun targetConfig () =
	Delay.force (case Til.getTargetPlatform()
		       of Til.MLRISC_ALPHA => alphaConfig
			| Til.TIL_ALPHA => alphaConfig
			| Til.TIL_SPARC => sparcConfig
			| Til.MLRISC_SPARC => sparcConfig)
    
    structure Crc = Crc

    (* -------------------------------------------------------
     * Unit Environments:  A unit environment UE is a mapping
     * from unit names to crc's. Unit objects holds unit
     * environments for imports and exports. When unit objects
     * are linked it is checked to see if unit environments
     * match up. If this is not the case, linking is aborted.
     * ------------------------------------------------------- *)
      
    structure UE =  (* We assume entries in unit environments are lex-sorted *)
      struct
	type UE = (string * Crc.crc) list

	(* confine(UE1,UE2)=UE3 : UE3 holds those components of UE1,
	 * that does not occur in UE2. Components that do also occur
	 * in UE2, must match up; otherwise confinement fails. *)

	fun confine (unitname,ue1 : UE, ue2 : UE) : UE =
	    let fun find name = Listops.assoc_eq((op =) : string * string -> bool, name, ue2)
		fun folder ((name,crc),acc) = 
		    (case find name of
			 NONE => (print "Could not find "; print name; print " in ";
				  app (fn (str,_) => (print str; print "\n")) ue2;
				  print "\n\n";
				  (name,crc)::acc)
		       | SOME crc2 => 
			     let val msg = "The unit object " ^ unitname ^ " builds\n" ^
				           "on a version of " ^ name ^ " which is inconsistent\n" ^
					   "with which it is linked."
			     in  if (crc = crc2)
				     then acc
				 else if (!doConsistent)
					  then error ("Link Error: " ^ msg)
				      else (print ("Link Error overridden by doConsistent set to false: " ^ msg);
					    acc)
			     end)
		val rev_ue = foldl folder [] ue1
	  in rev rev_ue
	  end

	fun plus_overlap(unitname,ue1,ue2) : UE =      (* used on import unit environments *)
	  let fun plus ([],[],a) = rev a
		| plus ([],e::ue2,a) = plus([],ue2,e::a)
		| plus (e::ue1,[],a) = plus(ue1,[],e::a)
		| plus (ue1 as ((un1,crc1)::ue1'),ue2 as ((un2,crc2)::ue2'),a) =
	        if String.<(un1,un2) then plus(ue1',ue2,(un1,crc1)::a)
		else if String.<(un2,un1) then plus(ue1,ue2',(un2,crc2)::a)
		else (* un1=un2 *)
		  if crc1=crc2 then plus(ue1',ue2',(un1,crc1)::a)
		  else error ("Link Error: The unit object " ^ unitname ^ " builds\n" ^
			      "on a version of " ^ un1 ^ " which is inconsistent\n" ^
			      "with versions of " ^ un1 ^ " imported elsewhere.") 
	  in plus(ue1,ue2,[])
	  end
	fun plus_no_overlap(unitname,ue1,ue2) : UE =      (* used on export unit environments *)
	  let val ue = ue1 @ ue2  (* maintain the order! *)
              fun plus ([],[],a) = rev a
		| plus ([],e::ue2,a) = plus([],ue2,e::a)
		| plus (e::ue1,[],a) = plus(ue1,[],e::a)
		| plus (ue1 as ((un1,crc1)::ue1'),ue2 as ((un2,crc2)::ue2'),a) =
	        if String.<(un1,un2) then plus(ue1',ue2,(un1,crc1)::a)
		else if String.<(un2,un1) then plus(ue1,ue2',(un2,crc2)::a)
		else (* un1=un2 *)
		  error ("Link Error: You are trying to link in the unit " ^ un1 ^ " more\n" ^
			 "than once. This is not allowed.") 
	  in plus(ue1,ue2,[]); ue
	  end
      end


    fun bincopy (is,os) = 
	let fun loop() = (BinIO_Util.copy(is,os); if (BinIO.endOfStream is) then () else loop())
	in  loop()
	end
    fun mk_emitter in_file os = let (* val _ = (print "mk_emitter on file "; print in_file; print "\n")*)
				    val is = BinIO.openIn in_file
				in bincopy(is,os); 
				    BinIO.closeIn is
				end

    local open BinIO_Util
    in
      fun mk_uo {imports : (string * Crc.crc) list,
		 exports : (string * Crc.crc) list,
		 base_result : string} : string =
	let
	    val encode = Dirs.encode (Dirs.getDirs())
	    val uo_result = Til.base2uo base_result
	    val os = BinIO.openOut uo_result
	    val out_pairs = app (fn (name,crc) =>
				 (output_string (os, encode name ^ ":");
				  Crc.output_crc (os, crc);
				  output_string (os, "\n")))
	in  output_string (os, "$imports:\n");
	    out_pairs imports;
	    output_string (os, "$exports:\n");
	    out_pairs exports;
	    BinIO.closeOut os;
	    uo_result
	end

      fun read_string (is, s) : unit =
	case input_string(is,size s)
	  of SOME s' => if s = s' then () 
			else error ("read_string: expecting to read \"" ^ s ^ 
				    "\", but found \"" ^ s' ^ "\"")
	   | NONE => error ("read_string: could not read the string \"" ^ s ^ "\"")


      fun read_unitname_and_colon is =
	let fun loop a = case input_char is
			   of SOME #":" => implode (rev a)
			    | SOME c => loop (c::a)
			    | NONE => error ("read_unitname_and_colon")
	in loop []
	end
 
      fun input_pairs (is : BinIO.instream) : (string * Crc.crc) list =
	let val decode = Dirs.decode (Dirs.getDirs())
	    fun loop a = case lookahead is of
			      NONE => rev a
			    | SOME #"$" => rev a
			    | SOME _ => let val unitname = decode (read_unitname_and_colon is)
					    val crc = Crc.input_crc is
					    val _ = read_string(is,"\n")
					in loop((unitname,crc)::a)
					end
	in loop []
	end

      fun read_imports (is) =
	let val _ = read_string(is, "$imports:\n")
	in input_pairs is
	end

      fun read_exports (is) =
	let val _ = read_string(is, "$exports:\n")
	in input_pairs is
	end


      (* read imports and exports from uo-file *)
      fun read_header_and_extract_code {uo_arg : string} : 
	{imports : (string * Crc.crc) list,
	 exports : (string * Crc.crc) list} =
	let
	    val is = BinIO.openIn uo_arg
	    val imports = read_imports is
	    val exports = read_exports is
	    val _ = BinIO.closeIn is
	in {imports=imports, exports=exports}
	end
        handle e => (print "exception while read_header of ";
		     print uo_arg; print "\n"; raise e)
    end

    (* link: Link a sequence of uo-files into a new uo-file 
     * and perform consistency check. *)

    type package = {unit : string, base : string, 
		    uiFile : string, uoFile : string, oFile : string}

    fun check (units : package list) = (* Current directory, or absolute path. *)
      let 
	  val linkinfo =
	    map (fn {unit, base, uiFile, uoFile, oFile} =>
		 let
		     val {imports,exports} = read_header_and_extract_code {uo_arg = uoFile}
(*
		     val _ = (print "IMPORTS: "; app print (map #1 imports); print "\n\n";
			      print "EXPORTS: "; app print (map #1 exports); print "\n\n")
*)
		 in {unitname=unit, imports=imports, exports=exports, ofile=oFile}
		 end) units
	  fun li (iue0,eue0,[]) = (iue0,eue0)
	    | li (iue0,eue0,{unitname,imports=iue,exports=eue,ofile}::rest) =
	    let val iue' = UE.confine(unitname,iue,eue0)
	        val iue0' = UE.confine(unitname,iue0,eue)
	        val iue_next = UE.plus_overlap(unitname,iue0',iue') 
		val eue_next = UE.plus_no_overlap(unitname,eue0,eue)
(*
		val _ = print "------------------------------------------------\n";
		val _ = (print "iue0: "; app print (map #1 iue0); print "\n\n")
		val _ = (print "eue0: "; app print (map #1 eue0); print "\n\n")
		val _ = (print "iue: "; app print (map #1 iue); print "\n\n")
		val _ = (print "eue: "; app print (map #1 eue); print "\n\n")
		val _ = (print "iue': "; app print (map #1 iue'); print "\n\n")
		val _ = (print "iue0': "; app print (map #1 iue0'); print "\n\n")
		val _ = (print "iue_next: "; app print (map #1 iue_next); print "\n\n")
		val _ = (print "eue_next: "; app print (map #1 eue_next); print "\n\n")
*)
	    in li (iue_next,eue_next,rest)
	    end
	  val (imports, exports) = li ([],[],linkinfo)
	  val _ = (print "imports: "; app print (map #1 imports); print "\n\n")
	  val o_files = map #ofile linkinfo
      in  (imports, exports, o_files)
      end

    (* run' : string list -> unit *)
    fun run' nil = ()
      | run' (cmd :: args) =
	let
	    val command = List.foldr (fn (a,b) => a ^ " " ^ b) "" (cmd::args)
	    val _ = (print "Running: "; print command; print "\n")
	in
	    if Util.system command then ()
	    else error (cmd ^ " failed")
	end
    (* run : string list list -> unit *)
    val run = run' o List.concat
    
    (* mk_exe: Make an executable from a uo-file and check 
     * that the sequence of imports is empty. *)
    fun mk_exe {units : package list,
		exe_result : string} : unit =
      let val (imports, exports, o_files) = check units
      in
	  case imports
	    of nil => (* everything has been resolved *)
		let
		    val {assembler, linker, ldpre, ldpost} = targetConfig()
		    val link = "link_" ^ exe_result
		    val unitnames = map #unit units
		    val local_labels = map (fn un => Rtl.ML_EXTERN_LABEL
					    (un ^ "_unit")) unitnames
		    val link_s = (case (Til.getTargetPlatform()) of
				      Til.TIL_ALPHA => Linkalpha.link
				    |	Til.TIL_SPARC => Linksparc.link
				  (*| Til.MLRISC_ALPHA => AlphaLink.link 
			            | Til.MLRISC_SPARC => SparcLink.link*)) (link, local_labels)
		    val link_o = (String.substring(link_s,0,size link_s - 2)) ^ ".o"
			
		    val _ = run [assembler, ["-o", link_o, link_s]]
		    val _ = run [linker, ["-o", exe_result], ldpre, o_files, [link_o], ldpost]
		    val _ = if not (!keep_link_asm)
				then List.app OS.FileSys.remove [link_s, link_o]
			    else ()
		in
		    ()
		end
	     | _ => let fun pr_units [] = error "pr_units"
			  | pr_units [a] = a
			  | pr_units (a::rest) = (a ^ ", " ^ pr_units rest)
		    in print ("\nError! The units : [" ^ pr_units (map #1 imports) ^ 
			      "] have not been resolved.\nExports were : [" ^
			      pr_units (map #1 exports) ^ "].\n I cannot generate an executable for you.\n"); error "mk_exe"
		    end
      end
  end

