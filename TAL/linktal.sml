structure LinkTAL = 
  struct
    val diag = Stats.ff("LinkTALDiag")
      
    fun msg str = if (!diag) then print str else ()
    
    fun comp (asm_file : string, unitname : string, imports : string list, exports : string list, lilmod : Lil.module) =
      let 
	val _ = msg "===== Translating to TAL assembly =====\n"
	val () = LilToTal.allocateModule asm_file unitname imports exports lilmod
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

  end