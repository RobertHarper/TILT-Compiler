(*$import Rtl *)

signature LINKASM =
sig

    (* rtl_to_asm(source filename, RTL module) -> 
                   (asm filename, label for the entrance to the module)
     *)
    val rtl_to_asm : string * Rtl.module -> string * Rtl.label

    (* create an asm file of table info using given labels *)
    val link : string * (Rtl.label list) -> unit 


end

