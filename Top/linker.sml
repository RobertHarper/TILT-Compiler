(*$import LINKER Compiler Util Crc Listops OS Name Linkalpha Linksparc *)
structure Linker :> LINKER =
  struct

    val debug_asm = Stats.bool("debug_asm")

    val as_path = "as"
    fun preld() = 
	let val alpha = "ld -r " 
	    val solaris = "ld -r "
	in   case !Til.platform of
	       Til.MLRISC_ALPHA => alpha
	     | Til.TIL_ALPHA => alpha
	     | Til.TIL_SPARC => solaris
	     | Til.MLRISC_SPARC => solaris
	end
    fun ld() = 
	let val alpha = 
	    if (!debug_asm) then
		"ld -D a000000 -T 8000000 -g" 
	    else
		"ld -D a000000 -T 8000000"
	    val solaris = "ld"
	in   case !Til.platform of
	       Til.MLRISC_ALPHA => alpha
	     | Til.TIL_ALPHA => alpha
	     | Til.TIL_SPARC => solaris
	     | Til.MLRISC_SPARC => solaris
	end
    fun crt() = 
	let val alpha = "/usr/lib/cmplrs/cc/crt0.o "
	    val solaris = "Runtime/obj_solaris/firstdata.o /usr/local/lib/gcc-lib/sparc-sun-solaris2.4/2.7.2/crt1.o /usr/local/lib/gcc-lib/sparc-sun-solaris2.4/2.7.2/crti.o /usr/ccs/lib/values-Xa.o /usr/local/lib/gcc-lib/sparc-sun-solaris2.4/2.7.2/crtbegin.o  -L/usr/local/lib/gcc-lib/sparc-sun-solaris2.4/2.7.2 -L/usr/ccs/bin -L/usr/ccs/lib -L/usr/local/lib"
	in   case !Til.platform of
	       Til.MLRISC_ALPHA => alpha
	     | Til.TIL_ALPHA => alpha
	     | Til.TIL_SPARC => solaris
	     | Til.MLRISC_SPARC => solaris
	end
    fun ld_libs() = 
	let val alpha = "Runtime/runtime.alpha_osf.a -call_shared -lpthread -lmach -lexc -lm -lc"
	    val solaris = "Runtime/runtime.solaris.a -lpthread -lm -lc -lgcc /usr/local/lib/gcc-lib/sparc-sun-solaris2.4/2.7.2/crtend.o /usr/local/lib/gcc-lib/sparc-sun-solaris2.4/2.7.2/crtn.o"
	in  case !Til.platform of
	       Til.MLRISC_ALPHA => alpha
	     | Til.TIL_ALPHA => alpha
	     | Til.TIL_SPARC => solaris
	     | Til.MLRISC_SPARC => solaris
	end
    val error = fn x => Util.error "Linker" x

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
(*   
	fun confine (unitname,ue1,ue2) : UE =
	  let fun conf ([],ue2,a) = rev a
		| conf (e::ue1,[],a) = conf(ue1,[],e::a)
		| conf (ue1 as ((un1,crc1)::ue1'),ue2 as ((un2,crc2)::ue2'),a) =
	        if un1 < un2 then conf(ue1',ue2,a)
		else if un2 < un1 then conf(ue1,ue2',a)
		else (* un1=un2 *)
		    if crc1=crc2 then conf(ue1',ue2',(un1,crc1)::a) 
		  else error ("Link Error: The unit object " ^ unitname ^ " builds\n" ^
			      "on a version of " ^ un1 ^ " which is inconcistent\n" ^
			      "with which it is linked.") 
	  in conf(ue1,ue2,[])
	  end
*)

	fun confine (unitname,ue1 : UE, ue2 : UE) : UE =
	    let fun find name = Listops.assoc_eq((op =) : string * string -> bool, name, ue2)
		fun folder ((name,crc),acc) = 
		    (case find name of
			 NONE => (name,crc)::acc
		       | SOME crc2 => 
			     if (crc = crc2)
				 then acc
			     else error ("Link Error: The unit object " ^ unitname ^ " builds\n" ^
					 "on a version of " ^ name ^ " which is inconsistent\n" ^
					 "with which it is linked."))
		val rev_ue = foldl folder [] ue1
	  in rev rev_ue
	  end

	fun plus_overlap(unitname,ue1,ue2) : UE =      (* used on import unit environments *)
	  let fun plus ([],[],a) = rev a
		| plus ([],e::ue2,a) = plus([],ue2,e::a)
		| plus (e::ue1,[],a) = plus(ue1,[],e::a)
		| plus (ue1 as ((un1,crc1)::ue1'),ue2 as ((un2,crc2)::ue2'),a) =
	        if un1 < un2 then plus(ue1',ue2,(un1,crc1)::a)
		else if un2 < un1 then plus(ue1,ue2',(un2,crc2)::a)
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
	        if un1 < un2 then plus(ue1',ue2,(un1,crc1)::a)
		else if un2 < un1 then plus(ue1,ue2',(un2,crc2)::a)
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
	    val uo_result = Til.base2uo base_result
	    val os = BinIO.openOut uo_result
	    val out_pairs = app (fn (name,crc) =>
				 (output_string (os, name ^ ":");
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
	let fun loop a = case lookahead is of
			      NONE => rev a
			    | SOME #"$" => rev a
			    | SOME _ => let val unitname = read_unitname_and_colon is
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
		 in {unitname=unit, imports=imports, exports=exports, ofile=oFile}
		 end) units
	  fun li (iue0,eue0,[]) = (iue0,eue0)
	    | li (iue0,eue0,{unitname,imports=iue,exports=eue,ofile}::rest) =
	    let val iue' = UE.confine(unitname,iue,eue0)
	        val iue0' = UE.confine(unitname,iue0,eue)
	        val iue_next = UE.plus_overlap(unitname,iue0',iue') 
		val eue_next = UE.plus_no_overlap(unitname,eue0,eue)
	    in li (iue_next,eue_next,rest)
	    end
	  val (imports, exports) = li ([],[],linkinfo)
	  val o_files = map #ofile linkinfo
      in  (imports, o_files)
      end

    (* mk_exe: Make an executable from a uo-file and check 
     * that the sequence of imports is empty. *)
    fun mk_exe {units : package list,
		exe_result : string} : unit =
      let val (imports, o_files) = check units
      in case imports of
	  nil => (* everything has been resolved *)
	      let val link = "link_" ^ exe_result
		  val bases = map #base units
		  val local_labels = map (fn un => Rtl.ML_EXTERN_LABEL
					  ("main_" ^ un ^ "_doit")) bases
		  val link_s = (case !Til.platform of
				Til.TIL_ALPHA => Linkalpha.link
			      |	Til.TIL_SPARC => Linksparc.link
    (*			      | Til.MLRISC_ALPHA => AlphaLink.link 
			      | Til.MLRISC_SPARC => SparcLink.link*))
		      (link, local_labels)
		  val link_o = (String.substring(link_s,0,size link_s - 2)) ^ ".o"
		  val success = Util.system (as_path ^ " -o " ^ link_o ^ " " ^ link_s)
		   val _ = if success then ()
			   else error "mk_exe - as failed"

		   val o_files_str = foldl (fn (a,b) => a ^ " " ^ b) "" o_files
		   val command = (ld() ^ " -o " ^
				  exe_result ^ " " ^ (crt()) ^ " " ^ 
				     o_files_str ^ " " ^ link_o ^ " " ^ ld_libs())
		   val _ = (print "Running: "; print command; print "\n")
		   val success = Util.system command
		   val _ = if success then ()
			   else (print "load failed: "; print command; print "\n";
				 error "mk_exe - ld failed")
		   val rmcommand = "rm " ^ link_s ^ "; rm " ^ link_o ^ "\n"
		   val _ = Util.system rmcommand
	       in ()
	       end
	    | _ => let val units = map #1 imports
	               fun pr_units [] = error "pr_units"
			 | pr_units [a] = a
			 | pr_units (a::rest) = (a ^ ", " ^ pr_units rest)
		   in print ("\nError! The units : [" ^ pr_units units ^ 
			     "] have not been resolved. I cannot generate\n" ^
			     "an executable for you.\n"); error "mk_exe"
		   end
      end

  end

