exception Div
exception Overflow
structure TiltExn =
struct
	exception SysErr of string * int option
	exception LibFail of string
end

(*
	The runtime needs to know the stamps for these exceptions.  We
	register them with the runtime by calling these C functions.
	Note that all that matters is the stamp: The values passed
	with SysErr and LibFail are ignored.
*)
val () = Ccall(registerSubExnRuntime, Subscript)
val () = Ccall(registerDivExnRuntime, Div)
val () = Ccall(registerOvflExnRuntime, Overflow)
val () = Ccall(registerSysErrExnRuntime, TiltExn.SysErr ("",NONE))
val () = Ccall(registerLibFailExnRuntime, TiltExn.LibFail "")
