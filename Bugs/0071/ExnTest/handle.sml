(*$import OS *)

fun test f =
    f () handle OS.SysErr _ => ()
