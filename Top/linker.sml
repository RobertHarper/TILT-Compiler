(*$import LINKER Compiler Util Crc Listops OS Name Linkalpha *)
structure Linker :> LINKER =
  struct

    val as_path = "as"
    fun preld() = 
	let val alpha = "ld -r " 
	    val solaris = "ld"
	in   case !Til.platform of
	       Til.MLRISC_ALPHA => alpha
	     | Til.TIL_ALPHA => alpha
	     | Til.MLRISC_SPARC => solaris
	end
    fun ld() = 
	let val alpha = "ld -D a000000 -T 8000000 " 
	    val solaris = "ld"
	in   case !Til.platform of
	       Til.MLRISC_ALPHA => alpha
	     | Til.TIL_ALPHA => alpha
	     | Til.MLRISC_SPARC => solaris
	end
    fun crt() = 
	let val alpha = "/usr/lib/cmplrs/cc/crt0.o "
	    val solaris = "Runtime/obj_solaris/firstdata.o /usr/local/lib/gcc-lib/sparc-sun-solaris2.4/2.7.2/crt1.o /usr/local/lib/gcc-lib/sparc-sun-solaris2.4/2.7.2/crti.o /usr/ccs/lib/values-Xa.o /usr/local/lib/gcc-lib/sparc-sun-solaris2.4/2.7.2/crtbegin.o  -L/usr/local/lib/gcc-lib/sparc-sun-solaris2.4/2.7.2 -L/usr/ccs/bin -L/usr/ccs/lib -L/usr/local/lib"
	in   case !Til.platform of
	       Til.MLRISC_ALPHA => alpha
	     | Til.TIL_ALPHA => alpha
	     | Til.MLRISC_SPARC => solaris
	end
    fun ld_libs() = 
	let val alpha = "Runtime/runtime.alpha_osf.a -call_shared -lpthread -lmach -lexc -lm -lsys5 -lc"
	    val solaris = "Runtime/runtime.solaris.a -lpthread -lm -lc -lgcc /usr/local/lib/gcc-lib/sparc-sun-solaris2.4/2.7.2/crtend.o /usr/local/lib/gcc-lib/sparc-sun-solaris2.4/2.7.2/crtn.o"
	in  case !Til.platform of
	       Til.MLRISC_ALPHA => alpha
	     | Til.TIL_ALPHA => alpha
	     | Til.MLRISC_SPARC => solaris
	end
    val error = fn x => Util.error "Linker" x

    fun base2uo s = s ^ ".uo"
    fun base2o s = s ^ ".o"


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
		 base_result : string} : unit = 
	let 
	    val uo_result = base2uo base_result
	    val os = BinIO.openOut uo_result
	  val out_pairs = app (fn (name,crc) =>
			       (output_string (os, name ^ ":");
				Crc.output_crc (os, crc);
				output_string (os, "\n")))
	in output_string (os, "$imports:\n");
	  out_pairs imports;
	  output_string (os, "$exports:\n");
	  out_pairs exports;
	  BinIO.closeOut os
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

    fun link {base_args : string list,       (* Current directory, or absolute path. *)
	      base_result : string} : unit = (* Strings should not contain extension. *) 
      let 
	  val linkinfo =
	    map (fn base =>
		 let
		     val o_file = base2o base
		     val uo_file = base2uo base
		     val {imports,exports} = read_header_and_extract_code 
		       {uo_arg = uo_file}
		 in {unitname=base, imports=imports, exports=exports,ofile=o_file}
		 end) base_args
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
	  val o_file = base2o base_result
	  fun pr_list [] = ""
	    | pr_list [a] = a
	    | pr_list (a::xs) = a ^ " " ^ pr_list xs
	  val command = (preld() ^ " -o " ^ o_file ^ 
			 " " ^ pr_list o_files)
	  val _ = (print "Running: "; print command; print "\n")
	  val success = Util.system command
	  val _ = if success
		      then mk_uo {imports=imports,exports=exports,base_result=base_result}
		  else (print "link failed: "; print command; print "\n";
			error "link. System command ld failed")
      in  ()
      end

    (* mk_exe: Make an executable from a uo-file and check 
     * that the sequence of imports is empty. *)
    fun mk_exe {base_arg : string,
		exe_result : string} : unit =
      let val o_temp = base2o base_arg
	  val uo_arg  = base2uo base_arg
	  val {imports,exports} = read_header_and_extract_code {uo_arg = uo_arg}

      in case imports
	   of nil => (* everything has been resolved *)
	       let val link_s = "link_" ^ exe_result ^ ".s"
		   val link_o = "link_" ^ exe_result ^ ".o"
		   val unitnames = map #1 exports
		   val local_labels = map (fn un => Rtl.ML_EXTERN_LABEL
					   ("main_" ^ un ^ "_doit")) unitnames
		   val _ = (case !Til.platform of
				Til.TIL_ALPHA => Linkalpha.link
(*			      | Til.MLRISC_ALPHA => AlphaLink.link 
			      | Til.MLRISC_SPARC => SparcLink.link*))

		       (link_s, local_labels)
		   val success = Util.system (as_path ^ " -o " ^ link_o ^ " " ^ link_s)
		   val _ = if success then ()
			   else error "mk_exe - as failed"
		   val command = (ld() ^ " -o " ^
				  exe_result ^ " " ^ (crt()) ^ " " ^ o_temp ^ " " ^ link_o ^ " " ^ ld_libs())
		   val _ = (print "Running: "; print command; print "\n")
		   val success = Util.system command
		   val _ = if success then ()
			   else (print "load failed: "; print command; print "\n";
				 error "mk_exe - ld failed")
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
(*
    structure Test =
      struct
	val uo_file = "/tmp/test.uo"
	val o_file = "/tmp/test.o"
	val A_ui = "/tmp/A.ui"
	val B_ui = "/tmp/B.ui"
	val C_ui = "/tmp/C.ui"
	val crc_A = Crc.crc_of_file A_ui
	val crc_B = Crc.crc_of_file B_ui
	val crc_C = Crc.crc_of_file C_ui
	val object = "/tmp/first.o"
	val _ =
	  mk_uo {imports= [("A",crc_A),("B",crc_B)],
		 exports= [("C",crc_C)],
		 uo_result= uo_file,
		 emitter= emitter object}
	val {imports,exports} = read_header_and_extract_code {uo_arg = uo_file,
							      o_file = o_file}
	val _ = if Crc.crc_of_file object = Crc.crc_of_file o_file then ()
		else error "TestError"
	val [("A",crc_A'),("B",crc_B')] = imports
	val [("C",crc_C')] = exports
	val _ = if [crc_A,crc_B,crc_C] = [crc_A',crc_B',crc_C'] then ()
		else error "TestError - imports, exports"
      end

    structure Test2 =
      struct
	val o_file = "/home/mael/tmp/hello.o"
	val uo_file =  "/home/mael/tmp/helloworld.uo" 
	val exe_result = "/home/mael/tmp/run"
	val _ = mk_uo {imports= [],
		       exports= [],
		       uo_result= uo_file,
		       emitter= emitter o_file}
	val _ = mk_exe {uo_arg = uo_file,
			exe_result = exe_result}

	val A_ui = "/tmp/A.ui"
	val crc_A = Crc.crc_of_file A_ui
	val _ = mk_uo {imports= [("A", crc_A)],
		       exports= [],
		       uo_result= uo_file,
		       emitter= emitter o_file}
	val _ = (mk_exe {uo_arg = uo_file,
			 exe_result = exe_result};
		 error "Test2 - should not get here... \n**** ERROR ****\n") handle _ => ()

      end
*)


  end

