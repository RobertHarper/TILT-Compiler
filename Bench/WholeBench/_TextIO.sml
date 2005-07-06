(*
	Dummy TextIO for tyan benchmark.
*)
structure TextIO = 
struct
	val stdOut = 0
	val stdIn = 1
	fun flushOut x = 0
	exception Unimp
	fun inputLine _ = raise Unimp
	fun inputN _ = raise Unimp
	fun openIn s = raise Unimp
	fun closeIn s = raise Unimp
	fun endOfStream s = raise Unimp
end

