(*$import *)
exception NoExceptionRaised

val exn_bad = (1 div 0; NoExceptionRaised) handle e => e
    
val _ = print (exnName exn_bad)
