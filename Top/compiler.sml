(*$import COMPILER LinkIl Linknil Linkrtl Linkalpha AlphaLink OS Stats *)
structure Til : COMPILER =
  struct

    val littleEndian = Stats.tt("littleEndian")

    datatype platform = TIL_ALPHA | TIL_SPARC | MLRISC_ALPHA | MLRISC_SPARC
    val platform = ref TIL_ALPHA

    type sbnd = Il.sbnd
    type context_entry = Il.context_entry
    type context = Il.context
	
    val error = fn x => Util.error "Compiler" x
    val as_path = "as"
	
    val debug_asm = ref false
    val keep_asm = Stats.tt("keep_asm")
    val compress_asm = Stats.tt("compress_asm")
    fun as_flag() = 
	(case !platform of
	     TIL_ALPHA => if (!debug_asm) then " -g " else ""
	   | TIL_SPARC => if (!debug_asm) then " -g " else ""
	   | MLRISC_ALPHA => if (!debug_asm) then " -g " else ""
	   | MLRISC_SPARC => "-xarch=v8plus")
    fun assemble(s_file,o_file) =
	let val as_command = as_path ^ " " ^ (as_flag()) ^ " -o " ^ o_file ^ " " ^ s_file
	    val rm_command = "rm " ^ s_file
	    val compress_command = "gzip -f " ^ s_file ^ " &"
	    val success = (Stats.timer("Assemble",Util.system) as_command)
	    val success = success andalso
		((OS.FileSys.fileSize o_file > 0) handle _ => false)
	in if success
	       then (if (!keep_asm)
			 then (if (!compress_asm)
				   then (Util.system compress_command; ())
			       else ())
		     else (Util.system rm_command; ()))
	   else error "assemble. System command as failed"
	end
    
    (* compile(ctxt, unitName, sbnds, ctxt') compiles sbnds into an
     * object file `unitName.o'. ctxt is the context in which the sbnds
     * were produced, and ctxt' contains the new bindings. unitName is
     * the name of the unit being compiled and can be used for
     * generating unique identifiers. Also, `unitName.o' must contain a
     * label for `initialization' with name `unitName_doit'. 
     *)
	exception Stop
	val uptoElaborate = Stats.ff("UptoElaborate")
	val uptoPhasesplit = Stats.ff("UptoPhasesplit")
	val uptoClosureConvert = Stats.ff("UptoClosureConvert")
	val uptoRtl = Stats.ff("UptoRtl")
	val uptoAsm = Stats.ff("UptoAsm")
	fun compile (ctxt: context, unitName: string, 
		     sbnd_entries: (sbnd option * context_entry) list , ctxt': context) : unit =
	    let val _ = if (!uptoElaborate) then raise Stop else ()
		val nilmod = Linknil.il_to_nil(unitName, (ctxt, sbnd_entries))
		val _ = if (!uptoPhasesplit orelse !uptoClosureConvert)
			    then raise Stop else ()
		val rtlmod = Linkrtl.nil_to_rtl (unitName,nilmod)
		val _ = if (!uptoRtl) then raise Stop else ()


		(* rtl_to_asm creates unitName.s file with main label * `unitName_doit' *)
		val rtl_to_asm = 
		    case !platform of
			 TIL_ALPHA => Linkalpha.rtl_to_asm
		       | TIL_SPARC => Linksparc.rtl_to_asm
(*		       | MLRISC_ALPHA => AlphaLink.rtl_to_asm 
		       | MLRISC_SPARC => SparcLink.rtl_to_asm *)
		val _ = rtl_to_asm(unitName ^ ".s", rtlmod)    
		val _ = if (!uptoAsm) then raise Stop else ()

		val sFile = unitName ^ ".s"
		val oFile = unitName ^ ".o"
		val _ = assemble(sFile, oFile)
	    in ()
	    end
	handle Stop => (let val os = TextIO.openOut (unitName ^ ".o")
                              val _ = TextIO.output(os,"Dummy .o file\n")
			in  TextIO.closeOut os
			end)


 
    end
