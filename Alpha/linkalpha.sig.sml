(*$import Rtl *)

signature LINKALPHA =
sig

    val compile_prelude : bool * string -> string * Rtl.label

    val compile : string -> string * Rtl.label
    val compiles : string list -> (string * Rtl.label) list
    val test : string -> string * Rtl.label

    (* rtl_to_alpha(s,m) returns a pair of the name of the
     * asm-file and a label for the entrance to the module. 
     * s is the source filename.
     *)
    val rtl_to_alpha : string * Rtl.module -> string * Rtl.label

    (* create an assembly file with table information using given labels *)
    val link : string * (Rtl.label list) -> unit 
end

