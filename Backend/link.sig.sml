(*$import Prelude Rtl *)


signature LINKASM =
sig

    (* rtl_to_asm(asm filename, RTL module) -> label for the entrance to the module *)
    val rtl_to_asm : string * Rtl.module -> Rtl.label

    (* create an asm file of table info for the given units *)
    val link : {asmFile : string, units : string list} -> unit

end

