structure LinkTAL = 
  struct
    val diag = Stats.ff("LinkTALDiag")
      
    fun msg str = if (!diag) then print str else ()
    
    fun comp (asm_file : string ,lilmod : Lil.module) =
      let 
	val _ = msg "===== Translating to (empty) TAL assembly =====\n"
	val s = TextIO.openOut asm_file
	val _ = TextIO.closeOut s
      in	()
      end
    
    val lil_to_asm = Stats.timer("To TAL ASM",comp)

  end