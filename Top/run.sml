(*$import Prelude Posix Word8 OS RUN TopLevel CommandLine *)

structure Run : RUN =
struct

    (* exit : int -> 'a *)
    fun exit n = Posix.Process.exit (Word8.fromInt n)
	
    (* run : (string * string list -> OS.Process.status) -> unit *)
    fun run main =
	let
	    val cmd = CommandLine.name()
	    val args = CommandLine.arguments()
	    val status = main (cmd, args) handle _ => OS.Process.failure
	in
	    if status = OS.Process.success
		then ()
	    else exit 1
	end
end
