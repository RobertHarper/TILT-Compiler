structure ExnHandler :> EXN_HANDLER =
struct
    val Interactive = Stats.tt "Interactive"
    fun errorMsg (e : exn) : string =
	(case e
	   of UtilError.BUG {file,msg} => "internal error: " ^ file ^ ": " ^ msg
	    | Compiler.Reject msg => msg
	    | Group.Error => "group file error"
	    | e => "uncaught exception: " ^ exnMessage e)

    val bug : Word8.word = 0w1
    val reject : Word8.word = 0w10	(* See ../Test/runtest.sml *)

    fun exitStatus (e : exn) : Word8.word =
	(case e
	   of UtilError.BUG _ => bug
	    | _ => reject)

    fun eprint (s : string) : unit = TextIO.output(TextIO.stdErr, s)

    fun print (e : exn) : unit =
	(eprint "tilt: "; eprint (errorMsg e); eprint "\n")

    fun exit (e : exn) : 'a =
	Posix.Process.exit (exitStatus e)

    fun printAndExit (e : exn) : 'a =
	(print e; exit e)
end
