(*$import COMPILER LinkIl Linknil Linkrtl Linkalpha Linksparc OS Stats Platform *)
structure Til :> COMPILER =
  struct

    val error = fn x => Util.error "compiler.sml" x
    val littleEndian = Stats.tt("littleEndian")

    datatype platform = TIL_ALPHA | TIL_SPARC | MLRISC_ALPHA | MLRISC_SPARC

    (* defaultPlatform () -> platform *)
    fun defaultPlatform () =
	(case Platform.platform()
	   of Platform.SOLARIS => (print "Sun detected.  Using Til-Sparc\n";
				   TIL_SPARC)
	    | Platform.DUNIX => (print "Alpha detected.  Using Til-Alpha\n";
				 TIL_ALPHA)
	    | _ => (print "Unsupported platform detected.  Using Til-Alpha.\n";
		    TIL_ALPHA))
	      
    val targetPlatform = ref (defaultPlatform ())
    fun getTargetPlatform() = !targetPlatform
    fun setTargetPlatform p = 
	let val little = (case p of
			      TIL_ALPHA => true
			    | TIL_SPARC => false
			    | MLRISC_ALPHA => true
			    | MLRISC_SPARC => false)
	in  targetPlatform := p;
	    littleEndian := little
	end
    fun native() = 
	(case (getTargetPlatform(), Platform.platform()) of
	     (TIL_ALPHA, Platform.DUNIX) => true
	   | (TIL_ALPHA, _) => false
	   | (TIL_SPARC, Platform.SOLARIS) => true
	   | (TIL_SPARC, _) => false
	   | _ => error "MLRISC not supported")

    type sbnd = Il.sbnd
    type context_entry = Il.context_entry
    type context = Il.context
	
    val as_path = "as"
	
    val debug_asm = Stats.ff("debug_asm")
    val keep_asm = Stats.tt("keep_asm")
    val compress_asm = Stats.tt("compress_asm")
    fun as_flag() = 
	(case (getTargetPlatform()) of
	     TIL_ALPHA => if (!debug_asm) then " -g " else ""
	   | TIL_SPARC => if (!debug_asm) then " -xarch=v8plus" else "-xarch=v8plus"
	   | MLRISC_ALPHA => if (!debug_asm) then " -g " else ""
	   | MLRISC_SPARC => if (!debug_asm) then " -xarch=v8plus" else "-xarch=v8plus")
    fun base2ui base = base ^ ".ui"
(*
	(case (getTargetPlatform()) of
	     TIL_ALPHA => Linkalpha.base2ui
	   | TIL_SPARC => Linksparc.base2ui
	   | _ => error "No MLRISC"
(*	   | MLRISC_ALPHA => AlphaLink.base2ui
	   | MLRISC_SPARC => SparcLink.base2ui *) ) base
*)
    fun base2s base = 
	(case (getTargetPlatform()) of
	     TIL_ALPHA => Linkalpha.base2s
	   | TIL_SPARC => Linksparc.base2s
	   | _ => error "No MLRISC"
(*	   | MLRISC_ALPHA => AlphaLink.base2s
	   | MLRISC_SPARC => SparcLink.base2s *) ) base
    fun base2o base = 
	(case (getTargetPlatform()) of
	     TIL_ALPHA => Linkalpha.base2o
	   | TIL_SPARC => Linksparc.base2o
	   | _ => error "No MLRISC"
(*	   | MLRISC_ALPHA => AlphaLink.base2o
	   | MLRISC_SPARC => SparcLink.base2o *) ) base
    fun base2uo base = 
	(case (getTargetPlatform()) of
	     TIL_ALPHA => Linkalpha.base2uo
	   | TIL_SPARC => Linksparc.base2uo
	   | _ => error "No MLRISC"
(*	   | MLRISC_ALPHA => AlphaLink.base2uo
	   | MLRISC_SPARC => SparcLink.base2uo *) ) base



    val uptoElaborate = Stats.ff("UptoElaborate")
    val uptoPhasesplit = Stats.ff("UptoPhasesplit")
    val uptoClosureConvert = Stats.ff("UptoClosureConvert")
    val uptoRtl = Stats.ff("UptoRtl")
    val uptoAsm = Stats.ff("UptoAsm")

    fun assemble_help background base =
	let val s_file = base2s base
	    val o_file = base2o base
	in  if (!uptoAsm orelse !uptoPhasesplit orelse !uptoClosureConvert orelse !uptoRtl) 
		then let val os = TextIO.openOut o_file
			 val _ = TextIO.output(os,"Dummy .o file\n")
		     in  TextIO.closeOut os; o_file
		     end
	    else
		let val as_command = as_path ^ " " ^ (as_flag()) ^ " -o " ^ o_file 
		                       ^ " " ^ s_file
		    val rm_command = "rm " ^ s_file
		    val compress_command = "gzip -q -f " ^ s_file
		    val command = 
			(case (!keep_asm, !compress_asm) of
			     (false, _) => as_command ^ "; " ^ rm_command
			   | (true, false) => as_command
			   | (true, true) => as_command ^ "; " ^ compress_command)
		    val command = if background
				      then "(" ^ command ^ ") &"
				  else command
		    val success = (Stats.timer("Assemble",Util.system) command)
		    val success = success andalso
			(background orelse
			 ((OS.FileSys.fileSize o_file > 0) handle _ => false))
		in if success
		       then o_file
		   else error ("System command as failed:\n" ^ command ^ "\n")
		end
	end
    val assemble = assemble_help false
    val assemble_start = assemble_help true
    fun assemble_done base = OS.FileSys.access(base2o base, [OS.FileSys.A_READ])

    (* compile(ctxt, unitName, sbnds, ctxt') compiles sbnds into an
     * object file `unitName.o'. ctxt is the context in which the sbnds
     * were produced, and ctxt' contains the new bindings. unitName is
     * the name of the unit being compiled and can be used for
     * generating unique identifiers. Also, `unitName.o' must contain a
     * label for `initialization' with name `unitName_doit'. 
     *)
    exception Stop
    fun il_to_asm (unitName : string,
		   fileBase: string, 
		   il_module) : string = 
	let val _ = if (!uptoElaborate) then raise Stop else ()
	    val nilmod = Linknil.il_to_nil(unitName, il_module)
	    val _ = if (!uptoPhasesplit orelse !uptoClosureConvert)
			then raise Stop else ()
	    val rtlmod = Linkrtl.nil_to_rtl (unitName,nilmod)
	    val _ = if (!uptoRtl) then raise Stop else ()
		
		
	    (* rtl_to_asm creates fileBase.s file with main label * `fileName_doit' *)
	    val rtl_to_asm = 
		case (getTargetPlatform()) of
		    TIL_ALPHA => Linkalpha.rtl_to_asm
		  | TIL_SPARC => Linksparc.rtl_to_asm
		  | _ => error "No MLRISC"
	    (*		       | MLRISC_ALPHA => AlphaLink.rtl_to_asm *)
	    (*		       | MLRISC_SPARC => SparcLink.rtl_to_asm*)
	    val (sFile,_) = rtl_to_asm(fileBase, rtlmod)    
	in  sFile
	end
    handle Stop => (let val sFile = base2s fileBase
			val os = TextIO.openOut sFile
			val _ = TextIO.output(os,"Dummy .s file\n")
		    in  TextIO.closeOut os; sFile
		    end)
	
    end
