exception Div
exception Overflow
structure TiltExn =
struct
    exception SysErr of string * int option
    exception LibFail of string
end

(* The runtime needs to know the stamps for these exceptions.  
 * See General/tal_extern.sml
 *)
val _ = Ccall(registerDivExnRuntime, Div)
val _ = Ccall(registerOvflExnRuntime, Overflow)
val _ = Ccall(registerSysErrExnRuntime, TiltExn.SysErr ("",NONE))
val _ = Ccall(registerLibFailExnRuntime, TiltExn.LibFail "")
