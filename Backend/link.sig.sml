signature LINKASM =
sig

    val diag : bool ref
    val rtl_to_asm : string * Rtl.module -> unit (* asm filename *)

    (* create an asm file of table info for the given units *)
    val link : {asmFile : string, units : string list} -> unit

end

