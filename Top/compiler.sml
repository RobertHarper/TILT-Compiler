
structure Compiler : COMPILER =
    struct
	type sbnds = Linknil.Il.sbnds
	and context = Linknil.Il.context

	val error = fn x => Util.error "Compiler" x
	val as_path = "/usr/ucb/cc"

	fun assemble(s_file,o_file) =
	    if OS.Process.system (as_path ^ " -c -o " ^ o_file ^ " " ^ s_file) 
		= OS.Process.success then ()
	    else error "assemble. System command as failed"

	fun compile (ctxt: context, unitName: string, sbnds: sbnds, ctxt': context) : unit =
	    let val sdecs = LinkIl.IlContext.context_to_sdecs ctxt'
		val nilmod = Linknil.il_to_nil (ctxt, sbnds, sdecs)
		val rtlmod = Linkrtl.nil_to_rtl (nilmod,unitName)
		val _ = Linkalpha.rtl_to_alpha(unitName, rtlmod)    (* creates unitName.s file with main label
								     * `unitName_doit' *)
		val _ = assemble(unitName ^ ".s", unitName ^ ".o")
	    in ()
	    end

    (* compile(ctxt, unitName, sbnds, ctxt') compiles sbnds into an
     * object file `unitName.o'. ctxt is the context in which the sbnds
     * were produced, and ctxt' contains the new bindings. unitName is
     * the name of the unit being compiled and can be used for
     * generating unique identifiers. Also, `unitName.o' must contain a
     * label for `initialization' with name `unitName_doit'. 
     *)
 
    end
