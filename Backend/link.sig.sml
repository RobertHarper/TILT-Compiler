(*$import Rtl *)

signature LINKASM =
sig

    (* rtl_to_asm(source filename, RTL module) -> 
                   (asm filename, label for the entrance to the module)
     *)
    val rtl_to_asm : string * Rtl.module -> string * Rtl.label

    (* add table info into corresponding asm file *)
    val link : string * (Rtl.label list) -> unit 

    (* the string is the name of the asm-file to create *)
    val mk_link_file : string * (Rtl.label list) -> unit

end

