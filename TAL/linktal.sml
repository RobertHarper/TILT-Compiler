structure LinkTAL = 
  struct
    val diag = Stats.ff("LinkTALDiag")
    val show_linkmod = Stats.ff("ShowLilLinkMod")
    fun msg str = if (!diag) then print str else ()
    
    fun comp (asm_ifile:string , asm_file:string, asm_efile:string , unitname : string, imports : string list, exports : string list, lilmod : Lil.module) =
      let 
	val _ = msg "===== Translating to TAL assembly =====\n"
	val () = LilToTal.allocateModule asm_file (SOME (asm_ifile,asm_efile)) unitname imports exports lilmod
      in ()
      end

    fun comp_int (asm_file : string, lilint : Lil.interface) =
      let 
	val _ = msg "===== Translating to TAL interface =====\n"
	val () = LilToTal.allocateInterface asm_file lilint
      in ()
      end
    
    val lil_to_asm = Stats.timer("To TAL ASM",comp)

    val to_tal_interface = fn int => fn filename => Stats.timer ("To TAL interface",comp_int) (filename,int)

    fun link {asmFile : string , importfiles : string list, units : {imports:string list, name:string} list} = 
      let
	val _ = msg "===== Generating TAL link module =====\n"
	val linkmod = LilLinkUnit.linkunit units
	val () = if !show_linkmod then 
	  PpLil.pp_pass  {module = linkmod,
			  name = "LINK",
			  pass = "linkmod",
			  header = "LINK"}
		 else ()
	val () = LilTypecheck.M.check linkmod
	val linkmod = LilOptimize.optimize {doCse = true} linkmod
	val () = if !show_linkmod then 
	  PpLil.pp_pass  {module = linkmod,
			  name = "LINK",
			  pass = "linkmod optimized",
			  header = "LINK"}
		 else ()
	val () = LilTypecheck.M.check linkmod

	val () = LilToTal.allocateModule asmFile NONE "TiltMain" importfiles ["tilt_main.tali"] linkmod
      in ()
      end

  end