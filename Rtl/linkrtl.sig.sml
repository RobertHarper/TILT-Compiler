(*$import Prelude TORTL PPRTL Rtl Nil *)

signature LINKRTL = 
sig
	
    val show_rtl : bool ref
    val ptrWriteBarrier : bool ref
    val fullWriteBarrier : bool ref
    val mirrorGlobal : bool ref
    val mirrorPtrArray : bool ref

    val nil_to_rtl : string * Nil.module -> Rtl.module

end


