(*$import COMPILER LinkIl Linknil Linkrtl Linkalpha OS Stats *)
structure Til : COMPILER =
    struct

	val use_mlrisc = ref false

	type sbnd = Il.sbnd
	type context_entry = Il.context_entry
	type context = Il.context

	val error = fn x => Util.error "Compiler" x
	val as_path = "as"
	val has_sys = OS.Process.system "sys" = OS.Process.success

	val debug_asm = ref false
	val keep_asm = ref false
	fun get_debug_flag() = if (!debug_asm) then " -g " else ""
	fun assemble(s_file,o_file) =
	    let val as_command = as_path ^ (get_debug_flag()) ^ " -c -o " ^ o_file ^ " " ^ s_file
		val rm_command = "rm " ^ s_file
		val compress_command = "gzip -f " ^ s_file ^ " &"
	    in  if (not has_sys)
	        then 
                  let val os = TextIO.openOut "worklist"
	              val _ = TextIO.output(os,as_command)
	              val _ = TextIO.closeOut os
		      fun sleep() = ()
		      fun loop() = if OS.FileSys.access("worklist",[])
					then (sleep(); loop()) else ()
                  in  loop()
                  end
	        else 
	          (if ((Stats.timer("Assemble",OS.Process.system)as_command) =  OS.Process.success 
		       andalso ((OS.FileSys.fileSize o_file > 0)
				handle _ => false))
		       then (if (!keep_asm)
				 then (OS.Process.system compress_command; ())
			     else (OS.Process.system rm_command; ()))
		  else error "assemble. System command as failed")
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
	fun compile (ctxt: context, unitName: string, 
		     sbnd_entries: (sbnd option * context_entry) list , ctxt': context) : unit =
	    let val sFile = unitName ^ ".s"
		val oFile = unitName ^ ".o"
		val _ = if (!uptoElaborate) then raise Stop else ()
		val sdecs = LinkIl.IlContext.context_to_sdecs ctxt'
		val nilmod = if !uptoPhasesplit 
				 then Linknil.phasesplit(ctxt, sbnd_entries)
			     else Linknil.il_to_nil (unitName, (ctxt, sbnd_entries))
		val _ = if (!uptoPhasesplit orelse !uptoNil) then raise Stop else ()
		val rtlmod = Linkrtl.nil_to_rtl (nilmod,unitName)
		val _ = if (!uptoRtl) then raise Stop else ()
		val rtl_to_alpha = Linkalpha.rtl_to_alpha
(*
		val rtl_to_alpha = if (!use_mlrisc) 
				       then AlphaLink.rtl_to_alpha else Linkalpha.rtl_to_alpha
*)
		val _ = rtl_to_alpha(unitName, rtlmod)    (* creates unitName.s file with main label
								     * `unitName_doit' *)
		val _ = assemble(sFile, oFile)
	    in ()
	    end
	handle Stop => (let val os = TextIO.openOut (unitName ^ ".o")
                              val _ = TextIO.output(os,"Dummy .o file\n")
			in  TextIO.closeOut os
			end)


 
    end
