(*$import NIL TORTL PPRTL RTL Nil *)

signature LINKRTL = 
sig
    structure Tortl : TORTL
    structure Pprtl : PPRTL

    val show_rtl : bool ref
    val nil_to_rtl : string * Nil.module -> Rtl.module

end


