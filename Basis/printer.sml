structure TiltPrinter =
struct

	val printer : (string -> unit) ref =
		ref (ccall1 printer_print)

	fun print (s:string) : unit =
		(!printer) s

	fun set_printer (f:string -> unit) : unit =
		printer := f

end
