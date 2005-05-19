structure Main :> MAIN =
struct

	fun main (_ : string, args : string list) : OS.Process.status =
		let	val _ = UtilError.Interactive := false
			val () = Manager.make args
		in	OS.Process.success
		end handle e => UtilError.print_and_exit e

end
