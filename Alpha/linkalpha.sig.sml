(*$import Linkrtl *)
signature LINKALPHA =
sig
    structure Rtl : RTL
    val compile_prelude : bool * string -> string * Rtl.local_label
    (* add table info into corresponding asm file *)
    val link : string * (Rtl.local_label list) -> unit 
    val mk_link_file : string * (Rtl.local_label list) -> unit
	(* the string is the name of the asm-file to create *)
    val compile : string -> string * Rtl.local_label
    val compiles : string list -> (string * Rtl.local_label) list
    val test : string -> string * Rtl.local_label
    val rtl_to_alpha : string * Rtl.module -> string * Rtl.local_label
	(* rtl_to_alpha(s,m) returns a pair of the name of the
	 * asm-file and a label for the entrance to the module. 
	 * s is the source filename.
	 *)
end
where Rtl = Linkrtl.Rtl
