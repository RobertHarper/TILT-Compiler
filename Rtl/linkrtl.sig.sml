signature LINKRTL =
sig

    val LinkRtlDiag : bool ref
    val ShowRtl : bool ref
    val ptrWriteBarrier : bool ref
    val fullWriteBarrier : bool ref
    val mirrorGlobal : bool ref
    val mirrorPtrArray : bool ref

    val nil_to_rtl : string * Nil.module -> Rtl.module (* unit name *)

end
