(*$import OS *)

val mkExn = TiltExn.SysErr

val test = TiltExn.SysErr ("msg", SOME 0xBAD)
