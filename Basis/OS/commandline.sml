structure CommandLine :> COMMAND_LINE =
struct

	val name : unit -> string = fn () => Ccall(commandline_name,())
	val args_size : unit -> int = fn () => Ccall(commandline_arguments_size,())
	val args_nth : int -> string = fn n => Ccall(commandline_arguments_nth,n)
	fun arguments () =
		let	val n = args_size()
		in	List.tabulate(n,args_nth)
		end

end
