(*$import Rtl *)


signature LINKASM =
sig

    (* rtl_to_asm(source filename, RTL module) -> 
                   (asm filename, label for the entrance to the module)
     *)
    val rtl_to_asm : string * Rtl.module -> string * Rtl.label

    (* create an asm file given extension-less filename of table info using given labels *)
    val link : string * (Rtl.label list) -> string

   val base2s : string -> string  (* Given a base, create name of .o file for current platform *)
   val base2o : string -> string  (* Given a base, create name of .o file for current platform *)
   val base2uo : string -> string  (* Given a base, create name of .o file for current platform *)

end

