(*$import NIL TORTL PPRTL RTL Linknil *)

signature LINKRTL = 
sig
    structure Tortl : TORTL
    structure Pprtl : PPRTL

    val show_rtl : bool ref
    val compile_prelude : bool * string -> Rtl.module
    val compile : string -> Rtl.module
    val compiles : string list -> Rtl.module list
    val test : string -> Rtl.module
    val tests : string list -> Rtl.module list
    val nil_to_rtl : Nil.module * string -> Rtl.module

end


