(*$import COMPILER LinkIl Linknil Linkrtl Linkalpha AlphaLink OS Stats *)
structure Til : COMPILER =
    struct

	val littleEndian = Stats.tt("littleEndian")

	val use_mlrisc = ref false

	type sbnd = Il.sbnd
	type context_entry = Il.context_entry
	type context = Il.context

	val error = fn x => Util.error "Compiler" x
	val as_path = "as"

	val debug_asm = ref false
	val keep_asm = ref false
	fun get_debug_flag() = if (!debug_asm) then " -g " else ""
	fun assemble(s_file,o_file) =
	    let val as_command = as_path ^ (get_debug_flag()) ^ " -o " ^ o_file ^ " " ^ s_file
		val rm_command = "rm " ^ s_file
		val compress_command = "gzip -f " ^ s_file ^ " &"
		val success = (Stats.timer("Assemble",Util.system) as_command)
		val success = success andalso
		              ((OS.FileSys.fileSize o_file > 0) handle _ => false)
	    in if success
		   then (if (!keep_asm)
			     then (Util.system compress_command; ())
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
	val uptoNil = Stats.ff("UptoNil")
	val uptoRtl = Stats.ff("UptoRtl")
	val uptoAlpha = Stats.ff("UptoAlpha")
	val uptoAsm = Stats.ff("UptoAsm")
	fun compile (ctxt: context, unitName: string, 
		     sbnd_entries: (sbnd option * context_entry) list , ctxt': context) : unit =
	    let val sFile = unitName ^ ".s"
		val oFile = unitName ^ ".o"
		val _ = if (!uptoElaborate) then raise Stop else ()
		val sdecs = LinkIl.IlContext.context_to_sdecs ctxt'
		val nilmod = (if !uptoPhasesplit 
				 then Linknil.phasesplit
			     else Linknil.il_to_nil) (unitName, (ctxt, sbnd_entries))
		val _ = if (!uptoPhasesplit orelse !uptoNil) then raise Stop else ()
		val rtlmod = Linkrtl.nil_to_rtl (unitName,nilmod)
		val _ = if (!uptoRtl) then raise Stop else ()
		val rtl_to_entrance = if (!use_mlrisc) 
					  then (print "*** CALLING MLRISC\n";
						AlphaLink.rtl_to_asm)
				      else (print "*** CALLING TIL\n";
					    Linkalpha.rtl_to_asm)
		val _ = rtl_to_entrance(unitName, rtlmod)    (* creates unitName.s file with main label
								     * `unitName_doit' *)
		val _ = if (!uptoAsm) then raise Stop else ()
		val _ = assemble(sFile, oFile)
	    in ()
	    end
	handle Stop => (let val os = TextIO.openOut (unitName ^ ".o")
                              val _ = TextIO.output(os,"Dummy .o file\n")
			in  TextIO.closeOut os
			end)


 
    end
