(*$import Prelude TORTL PPRTL Rtl Nil *)

signature LINKRTL = 
sig
	
    val show_rtl : bool ref
    val ptrWriteBarrier : bool ref
    val fullWriteBarrier : bool ref
    val mirrorGlobal : bool ref
    val mirrorPtrArray : bool ref

    (* The translation to RTL performs some name mangling.  Translate
     looks up each imported label's compilation unit in a unitmap. *)

    type unitmap
    val empty : unitmap
    val extend : unitmap * Nil.label * string -> unitmap (* unit name *)
    val restrict : unitmap * Nil.label list -> unitmap
	
    val nil_to_rtl : string * unitmap * Nil.module -> Rtl.module (* unit name *)

end


