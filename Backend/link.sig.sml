(*$import Rtl *)


signature LINKASM =
sig

    val rtl_to_asm : string * Rtl.module -> unit (* asm filename *)

    (* create an asm file of table info for the given units *)
    val link : {asmFile : string, units : string list} -> unit

end

