(*$import *)
exception NoExceptionRaised

val exn = (1 div 0; NoExceptionRaised) handle e => e
    
val _ = print (exnName exn)

(* exnMessage works fine *)
