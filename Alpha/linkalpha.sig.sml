(*$import Rtl *)

signature LINKALPHA =
sig


    val compile_prelude : bool * string -> string * Rtl.label
    (* add table info into corresponding asm file *)
    val link : string * (Rtl.label list) -> unit 
    val mk_link_file : string * (Rtl.label list) -> unit
	(* the string is the name of the asm-file to create *)
    val compile : string -> string * Rtl.label
    val compiles : string list -> (string * Rtl.label) list
    val test : string -> string * Rtl.label
    val rtl_to_alpha : string * Rtl.module -> string * Rtl.label
	(* rtl_to_alpha(s,m) returns a pair of the name of the
	 * asm-file and a label for the entrance to the module. 
	 * s is the source filename.
	 *)
end

